testCasRecentrage <- function(df) {
  
  dfOUT <- data.frame('classe'=numeric(0), 'cas'=character(0))
  
  for (ligne in df$'classe') {
    
    data <- dplyr::filter(df, classe==ligne)

    # Cas A
    testA <- (data$Vs_moyen<ligne & is.element(ligne+1, df$classe)) |
      (data$Vs_moyen>ligne & is.element(ligne-1, df$classe))
    if (testA) { cas <- 'A' }
    
    # Cas B
    testB <- data$Vs_moyen<ligne & 
      !(is.element(ligne+1, df$classe)) & 
      is.element(ligne-1,df$classe)
    if (testB) { cas <- 'B' }
    
    # Cas C
    testC <- data$Vs_moyen>ligne & 
      !(is.element(ligne-1, df$classe)) &
      is.element(ligne+1, df$classe)
    if (testC) { cas <- 'C' }
    
    # Cas D
    testD <- !(is.element(ligne-1, df$classe)) & !(is.element(ligne+1, df$classe))
    if (testD) { cas <- 'D' }
    
    ### Vérifier qu'un seul cas a été détecté
    if (sum(testA, testB, testC, testD) != 1) {
      stop(
        paste('Erreur dans testCasRecentrage : zero ou plusieurs cas détectés, doit être unique',
              '(classe', ligne, ')'))
    }
    
    dfOUT <- rbind(dfOUT, data.frame(classe=ligne,cas))  
  }
  return(dfOUT)
}

#' Interpolations et extrapolations aux valeurs de vitesses de vent entières
#' 
#' Applique la norme NF S 31-114\cr
#' Les cas sont nommés de A à E d'après le projet de norme NFS 31114 v9 annexe B.\cr
#' La fonction n'a pas connaissance du nombre de descripteurs du niveau sonore
#' par classe de vitesse de vent. Le cas E (moins de 10 descripteurs) doit être déterminé en amont.\cr
#' Pour ce cas, la data.frame en entrée ne contiendra pas de ligne pour la classe considérée.
#' A titre transitoire, la valeur de Vs_moyen ou de L_median pourra être à NA.
#'
#' @param dfIN data.frame en entrée contient au moins 3 colonnes :
#'  \itemize{
#'    \item classe    : numeric classe de vent
#'    \item Vs_moyen  : numeric moyenne des vitesses de vent
#'    \item L_median  : numeric médiane des descripteurs du niveau sonore
#'    }
#' Les colonnes doivent être nommées.\cr
#' Par défaut, la fonction attend les colonnes 'classe', Vs_moyen' et 'L_median'.
#'
#' @param classe character Optionnel
#' @param Vs_moyen character Optionnel
#' @param L_median character Optionnel
#' 
#' Les arguments classe, Vs_moyen et L_median permettent de spécifier le nom des colonnes
#' si nécessaire.

#'
#' @return data.frame en sortie contient 2 colonnes :
#'  \itemize{
#'   \item classe : numeric
#'   \item indicateur : numeric
#' }
#'
#' @export
recentrage <- function(dfIN, classe='classe', Vs_moyen='Vs_moyen', 
                       L_median='L_median') {
  
  ### Vérification du format de dfIN
  codeErreur <- NA
  # classe data.frame
  if (!(class(dfIN)=='data.frame')) { codeErreur <- '1' }
  # noms des colonnes
  if (!(classe %in% names(dfIN))) { codeErreur <- '2.1' }
  if (!(Vs_moyen %in% names(dfIN))) { codeErreur <- '2.2' }
  if (!(L_median %in% names(dfIN))) { codeErreur <- '2.3' }
  # type des données
  if (!(is.numeric(dfIN$classe))) { codeErreur <- '3.1' }
  if (!(is.numeric(dfIN$Vs_moyen))) { codeErreur <- '3.2' }
  if (!(is.numeric(dfIN$L_median))) { codeErreur <- '3.3' }
  # 1 ligne par classe de vitesse de vent (ou NA)
  if (!(length(na.omit(unique(round(dfIN$Vs_moyen)))) + 
          sum(is.na(dfIN$Vs_moyen)) == nrow(dfIN))) { codeErreur <- '4' }
  
  if (!(is.na(codeErreur))) {
    warning(paste('Problème open31114::recentrage, dfIN erreur', codeErreur))
  }
  
  
  
  ### Renommage des colonnes selon arguments
  dfIN <- dplyr::select_(dfIN, 'classe'=classe, 'Vs_moyen'=Vs_moyen, 
                         'L_median'=L_median)
  
  ### Préparation de la data.frame de sortie
  dfOUT <- data.frame('classe'=numeric(0),'indicateur'=numeric(0))
  
  ### Cas E (valeur NA pour Vs_moyen ou L_median)
  # Vecteur des classes de vent concernées
  casE <- dplyr::filter(dfIN, is.na(Vs_moyen) | is.na(L_median))$'classe'
  
  ### Analyse des cas à traiter hormis cas E
  dfCas <- testCasRecentrage(dplyr::select(dfIN, classe, Vs_moyen) %>% 
                               dplyr::filter(!is.element(classe, casE)))
  
  for (ligne in dfIN$'classe') {
    
    if (ligne %in% casE) { 
      cas <- 'E'
    } else { cas <- dplyr::filter(dfCas, classe==ligne)$cas }
    
    # Cas A
    if (cas == 'A') {
      # Vs_moyen de la classe
      Vs <- dplyr::filter(dfIN, classe==ligne)$Vs_moyen
      # Distinguer interpolation par la gauche ou la droite
      if (Vs < ligne) {
        linearModel <- lm(L_median ~ Vs_moyen, data=dfIN[which(dfIN$classe %in% c(ligne,ligne+1)),])
      }
      else if (Vs > ligne) {
        linearModel <- lm(L_median ~ Vs_moyen, data=dfIN[which(dfIN$classe %in% c(ligne-1,ligne)),])
      }
      recentre <- predict.lm(linearModel, newdata=data.frame('Vs_moyen'=ligne))
    }
    
    # Cas B
    if (cas == 'B') {
      linearModel <- lm(L_median ~ Vs_moyen, data=dfIN[which(dfIN$classe %in% c(ligne-1,ligne)),])
      recentre <- predict.lm(linearModel, newdata=data.frame('Vs_moyen'=ligne))
    }
    
    # Cas C
    if (cas == 'C') {
      linearModel <- lm(L_median ~ Vs_moyen, data=dfIN[which(dfIN$classe %in% c(ligne,ligne+1)),])
      recentre <- predict.lm(linearModel, newdata=data.frame('Vs_moyen'=ligne))
    }
    
    # Cas D
    if (cas == 'D') {
      recentre <- dplyr::filter(dfIN, classe==ligne)$L_median
    }
    
    # Cas E
    if (cas == 'E') {
      recentre <- NA
    }
    
    dfOUT <- rbind(dfOUT, data.frame(classe=ligne, recentre))
  }
  
  return(dfOUT)
}


