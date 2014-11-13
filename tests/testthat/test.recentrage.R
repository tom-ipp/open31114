##########################################
##### Test de la fonction recentrage #####
##### Norme 31-114
##### 7.3.2 Interpolations et extrapolations aux valeurs de vitesses de vent entières
##### Données tests issues de la 31-114 v9 page 22 Annexe B
##########################################

dataTestRecentrage <- data.frame(
  'classe' = seq(3,10),
  'Vs_moyen' = c(2.8,4.2,NA,5.8,NA,8.2,9.2,9.8),
  'L_median' = seq(33,40)
  )

library('dplyr')

context("recentrage")

test_that("format de la sortie", {
  OUT <- recentrage(dataTestRecentrage)
  # classe data.frame
  expect_that(OUT, is_a('data.frame'))
  # colonne nommée 'classe'
  expect_that('classe' %in% names(OUT), is_true())
  # colonne nommée 'recentre'
  expect_that('recentre' %in% names(OUT), is_true())
  # classe integer pour colonne 'classe'
  expect_that(OUT$'classe', is_a('integer'))
  # classe numeric pour colonne 'recentre'
  expect_that(OUT$'recentre', is_a('numeric'))
})

test_that("conforme aux données entrées", {
  IN <- dataTestRecentrage
  OUT <- recentrage(IN)
  # même nombre de lignes
  expect_that(nrow(IN) == nrow(OUT), is_true())
  # classes de vent bien répliquées
  expect_that(sum(IN$'classe' == OUT$'classe') == nrow(IN), is_true())  
})

test_that("résultat conforme aux attentes", {
  
  IN <- dataTestRecentrage
  OUT <- recentrage(IN)
  
  # Cas A avec Vs_moyen < classe : classe test 3
  classeTest <- 3
  x1 <- filter(IN, classe==classeTest)$'Vs_moyen'
  x2 <- filter(IN, classe==classeTest+1)$'Vs_moyen'
  y1 <- filter(IN, classe==classeTest)$'L_median'
  y2 <- filter(IN, classe==classeTest+1)$'L_median'
  a <- (y2-y1)/(x2-x1)
  b <- y1-a*x1
  expect_that(filter(OUT, classe == classeTest)$'recentre',  equals(a*classeTest+b))
  
  # Cas A avec Vs_moyen > classe : classes tests 4 et 9
  classesTests <- c(4,9)
  for (classeTest in classesTests) {
    x1 <- filter(IN, classe==classeTest-1)$'Vs_moyen'
    x2 <- filter(IN, classe==classeTest)$'Vs_moyen'
    y1 <- filter(IN, classe==classeTest-1)$'L_median'
    y2 <- filter(IN, classe==classeTest)$'L_median'
    a <- (y2-y1)/(x2-x1)
    b <- y1-a*x1
    expect_that(filter(OUT, classe == classeTest)$'recentre',  equals(a*classeTest+b))
  }
  
  # Cas B : classes tests 10
  classesTests <- c(10)
  for (classeTest in classesTests) {
    x1 <- filter(IN, classe==classeTest-1)$'Vs_moyen'
    x2 <- filter(IN, classe==classeTest)$'Vs_moyen'
    y1 <- filter(IN, classe==classeTest-1)$'L_median'
    y2 <- filter(IN, classe==classeTest)$'L_median'
    a <- (y2-y1)/(x2-x1)
    b <- y1-a*x1
    expect_that(filter(OUT, classe == classeTest)$'recentre',  equals(a*classeTest+b))
  }
  
  # Cas C : classes tests 8
  classesTests <- c(8)
  for (classeTest in classesTests) {
    x1 <- filter(IN, classe==classeTest)$'Vs_moyen'
    x2 <- filter(IN, classe==classeTest+1)$'Vs_moyen'
    y1 <- filter(IN, classe==classeTest)$'L_median'
    y2 <- filter(IN, classe==classeTest+1)$'L_median'
    a <- (y2-y1)/(x2-x1)
    b <- y1-a*x1
    expect_that(filter(OUT, classe == classeTest)$'recentre',  equals(a*classeTest+b))
  }
  
  # Cas D : classes tests 6
  classesTests <- c(6)
  for (classeTest in classesTests) {
    expect_that(filter(OUT, classe == classeTest)$'recentre',  equals(filter(IN, classe==classeTest)$'L_median'))
  }
  
  # Cas E : classes tests 5 et 7
  classesTests <- c(5, 7)
  for (classeTest in classesTests) {
    expect_that(is.na(filter(OUT, classe == classeTest)$'recentre'),  is_true())
  }
  
})
