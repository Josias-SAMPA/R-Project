cat("Donnees de l'echantillon\n")

# Fonction de saisie utilisateur
saisieUser <- function(message) {
  val <- readline(prompt = message)
  return(as.numeric(val))
}

# Fonction principale du test de Wilcoxon
test_wilcoxon <- function() {
  
  cat("Debut du Test de Wilcoxon pour cet echantillon\n")
  
  # Taille de l'échantillon
  n <- saisieUser("Entrer la taille de votre echantillon : ")
  cat("n =", n, "\n")
  
  # Création des vecteurs
  avant <- numeric(n)
  apres <- numeric(n)
  
  cat("Vecteurs crees avec succes, en attente des valeurs\n")
  
  # Remplissage des vecteurs
  for (i in 1:n) {
    avant[i] <- saisieUser(paste("Avant[", i, "] : ", sep = ""))
    apres[i] <- saisieUser(paste("Apres[", i, "] : ", sep = ""))
  }
  
  # Affichage des données
  cat("Avant :", avant, "\n")
  cat("Apres :", apres, "\n")
  
  # Seuil de signification
  alpha <- saisieUser("Donner le seuil de signification (%) : ") / 100
  cat("alpha =", alpha, "\n")
  
  # Calcul des différences
  d <- avant - apres
  cat("Differences :", d, "\n")
  
  # Suppression des différences nulles
  d <- d[d != 0]
  
  # Vérification
  if (length(d) == 0) {
    cat("Toutes les differences sont nulles. Test impossible.\n")
    return()
  }
  
  # Valeurs absolues
  d_abs <- abs(d)
  
  # Rangs
  rangs <- rank(d_abs)
  
  # Tableau récapitulatif
  cat("\nTableau des calculs :\n")
  print(data.frame(difference = d, valeur_absolue = d_abs, rang = rangs))
  
  # Sommes des rangs
  Wplus <- sum(rangs[d > 0])
  Wmoins <- sum(rangs[d < 0])
  
  cat("\nW+ =", Wplus, "\n")
  cat("W- =", Wmoins, "\n")
  
  # Statistique de test
  W <- min(Wplus, Wmoins)
  cat("W observe =", W, "\n")
  
  # Nombre de valeurs non nulles
  m <- length(d)
  cat("m =", m, "\n")
  
  # Valeur critique (test bilatéral)
  Wcrit <- qsignrank(alpha / 2, m)
  cat("W critique =", Wcrit, "\n")
  
  # Décision
  if (W <= Wcrit) {
    cat("➡️ Conclusion : On rejette H0 (difference significative)\n")
  } else {
    cat("➡️ Conclusion : On ne rejette pas H0 (pas de difference significative)\n")
  }
}

# Lancer le test
test_wilcoxon()