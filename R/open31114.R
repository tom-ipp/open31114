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
#' Applique la norme NF S 31-114
#' Les cas sont nommés d'après le projet de norme NFS 31114 v9 annexe B
#'
#' Le cas E (moins de 10 descripteurs) ne peut être attribué par la fonction.\cr
#' Il appartient à l'utilisateur de ne pas transmettre à cette fonction 
#' de données numériques pour une classe contenant moins de 10 descripteurs du niveau sonore.
#' 
#' @param dfIN data.frame en entrée contient 3 colonnes :
#'  \itemize{
#'    \item classe    : numeric
#'    \item Vs_moyen  : numeric
#'    \item L_median  : numeric
#'    }
#' Les colonnes doivent être nommées.
#' La colonne classe est obligatoirement nommée ainsi.
#' Les arguments Vs_moyen et L_median permettent de passer le nom des colonnes spécifiées.
#'
#' @param Vs_moyen character Optionnel
#' @param L_median character Optionnel
#' 
#'
#'
#' @return dfOUT data.frame de sortie contient 2 colonnes :
#'  \itemize{
#'   \item classe : numeric
#'   \item indicateur : numeric
#' }
#'
#' @export
recentrage <- function(dfIN, Vs_moyen='Vs_moyen', L_median='L_median') {
  
  ### Renommage des colonnes selon arguments
  dfIN <- dplyr::select_(dfIN, 'classe', 'Vs_moyen'=Vs_moyen, 'L_median'=L_median)
  
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


