# Fonction de saisie
saisirNombre <- function(message) {
  as.numeric(readline(prompt = message))
}

test_Bartlett <- function() {
  
  # Nombre de groupes
  k <- saisirNombre("Entrez le nombre de groupes : ")
  
  groupes <- list()
  
  # Saisie des groupes
  for (i in 1:k) {
    
    cat(" Groupe", i, "\n")
    
    n <- saisirNombre("Taille du groupe : ")
    
    valeurs <- c()
    
    for (j in 1:n) {
      x <- saisirNombre(paste("Valeur", j, ": "))
      valeurs <- c(valeurs, x)
    }
    
    groupes[[i]] <- valeurs
  }
  
  #  Test de Bartlett 
  test <- bartlett.test(groupes)
  
  # valeur de test
  stat <- test$statistic
  
  # ddl
  ddl <- test$parameter
  
  
  alpha <- 0.05
  
  
  valeur_critique <- qchisq(1 - alpha, df = ddl)
  
  
  cat("Résultats du test de Bartlett")
  cat("Valeur de test :", stat, "\n")
  cat("Degrés de liberté :", ddl, "\n")
  cat("Valeur critique  :", valeur_critique, "\n")
  
  
  cat("Prise de Décision \n")
  
  if (stat > valeur_critique) {
    cat("On rejette H0 , variances différentes \n")
  } else {
    cat("On accepte H1 ,  variances homogènes \n")
  }
}