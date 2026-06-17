test_spearman <- function() {

  
  n <- as.integer(readline("Entrer le nombre d'observations n : "))
  if (is.na(n) || n <= 0) stop("Erreur : n doit etre un entier strictement positif.")

  X <- numeric(n)
  Y <- numeric(n)

  cat("Entrer les valeurs de X :\n")
  for (i in 1:n) X[i] <- as.numeric(readline(paste0("X[", i, "] = ")))

  cat("Entrer les valeurs de Y :\n")
  for (i in 1:n) Y[i] <- as.numeric(readline(paste0("Y[", i, "] = ")))

  if (any(is.na(X)) || any(is.na(Y)))
    stop("Erreur : des valeurs manquantes ont ete detectees dans X ou Y.")

  alpha <- suppressWarnings(
    as.numeric(readline("Entrer le seuil de signification alpha (ex: 0.05) : "))
  )
  if (is.na(alpha) || alpha <= 0 || alpha >= 1)
    stop("Erreur : alpha doit etre compris entre 0 et 1 (ex: 0.05).")

  
  RX <- rank(X, ties.method = "average")
  RY <- rank(Y, ties.method = "average")

  has_ties <- any(RX != round(RX)) || any(RY != round(RY))
  if (has_ties) {
    cat("\nDes ex-aequo ont ete detectes.\n")
  }
  
  cat("\n")
  cat("TABLEAU DES RANGS\n")
  cat("\n")
  tableau <- data.frame(
    X = X,
    Y = Y,
    R_X = RX,
    R_Y = RY,
    di = RX - RY,
    di2 = (RX - RY)^2
  )
  print(tableau)

  cat("\n")
  
  somme_d2 <- sum((RX - RY)^2)
  rs       <- 1 - (6 * somme_d2) / (n * (n^2 - 1))

  cat("\n")
  cat("\nCOEFFICIENT DE SPEARMAN\n")
  cat("Somme des di^2 :", somme_d2, "\n")
  cat(" valeur de rs   :", round(rs, 4), "\n")

  cat("Correlation de Reference (population)\n")
  reponse <- readline("La correlation r de la population est-elle connue et non nulle ? (O/N) : ")
  if (toupper(reponse) == "O") {
    r <- suppressWarnings(as.numeric(readline("Entrer la valeur de r : ")))
    if (is.na(r) || abs(r) >= 1) stop("Erreur : r doit etre compris strictement entre -1 et 1.")
  } else {
    r <- 0
    cat("Dans ce cas  la valeur r = ", r)
  }

  cat("\n")
  cat("\n")
  
  cat(" (1) Formulation de l'Hypothese\n")
  cat("\n")
  cat("Type de Test\n")
  cat("1 - Bilateral         (H1 : rs !=", r, ")\n")
  cat("2 - Unilateral droit  (H1 : rs > ", r, ")\n")
  cat("3 - Unilateral gauche (H1 : rs < ", r, ")\n\n")

  type_test <- suppressWarnings(as.integer(readline("Votre choix (1/2/3) : ")))
  if (is.na(type_test) || !type_test %in% 1:3)
    stop("Erreur : choix invalide. Entrer 1, 2 ou 3.")
  cat("\n")
  cat("H0 : rs =", r, "\n")
  if (type_test == 1) cat("H1 : rs !=", r, "\n")
  if (type_test == 2) cat("H1 : rs >",  r, "\n")
  if (type_test == 3) cat("H1 : rs <",  r, "\n")

 
  cat("\n")
  cat("\n")
  cat("(2) Choix du Test \n")
  cat("Test :Coefficient de  Correlation de Spearman\n")
  cat(" t = (rs - r) * sqrt(n-2) / sqrt(1 - rs^2)  qui tend vers la loi de student T(ddl) avec ddl = n-2 \n")
 

  
  cat("\n")
  cat("\n")
  cat(" (3) : Valeur de Test\n")
  cat("\n")

  t_obs <- ((rs - r) * sqrt(n - 2)) / sqrt(1 - rs^2)
  ddl   <- n - 2
  cat(sprintf("t_obs = %.4f\n", t_obs))
  cat("Degré de liberte : ddl =", ddl, "\n")

  cat("\n")
  cat("\n")
  cat("(4) Valeur critique\n")
  cat("\n")

  if (type_test == 1) {
    t_critique <- qt(1 - alpha / 2, df = ddl)
    p_val      <- 2 * pt(-abs(t_obs), df = ddl)
    cat(sprintf("t_critique = t(%.3f ; %d) = %.4f\n", alpha / 2, ddl, t_critique))
    cat(sprintf("p-value (bilaterale) = %.4f\n", p_val))
    cat(sprintf("|t_obs| = %.4f\n", abs(t_obs)))

  } else if (type_test == 2) {
    t_critique <- qt(1 - alpha, df = ddl)
    p_val      <- pt(t_obs, df = ddl, lower.tail = FALSE)
    cat(sprintf("t_critique = t(%.3f ; %d) = %.4f\n", alpha, ddl, t_critique))
    cat(sprintf("Probabilite d'observer t_obs sous H0 (unilaterale droite) = %.4f\n", p_val))
    cat("Regle : Rejeter H0 si t_obs > t_critique\n")
    cat(sprintf("t_obs = %.4f\n", t_obs))

  } else {
    t_critique <- qt(alpha, df = ddl)
    p_val      <- pt(t_obs, df = ddl, lower.tail = TRUE)
    cat(sprintf("t_critique = t(%.3f ; %d) = %.4f\n", alpha, ddl, t_critique))
    cat(sprintf("Probabilite d'observer t_obs sous H0 (unilaterale gauche) = %.4f\n", p_val))
    cat("Regle : Rejeter H0 si t_obs < t_critique\n")
    cat(sprintf("t_obs = %.4f\n", t_obs))
  }

  cat("\n")
  cat("\n")
  cat(" (5) : Prise de decision et conclusion\n")
  cat("\n")

  if (type_test == 1) {
    if (abs(t_obs) > t_critique) {
      cat(sprintf("Decision : |t_obs| = %.4f > t_critique = %.4f\n", abs(t_obs), t_critique))
      cat("=> On Rejette H0\n")
      cat("=> Il existe une liaison significative entre X et Y\n")
      cat("=> X et Y ne sont PAS independantes\n")
    } else {
      cat(sprintf("Decision : |t_obs| = %.4f <= t_critique = %.4f\n", abs(t_obs), t_critique))
      cat("=> On NE REJETTE PAS H0\n")
      cat("=> Il n'existe pas de liaison significative entre X et Y\n")
      cat("=> X et Y sont independantes\n")
    }

  } else if (type_test == 2) {
    if (t_obs > t_critique) {
      cat(sprintf("Decision : t_obs = %.4f > t_critique = %.4f\n", t_obs, t_critique))
      cat("=> On Rejette H0\n")
      cat("=> Il existe une liaison significative positive entre X et Y\n")
      cat("=> X et Y ne sont PAS independantes\n")
    } else {
      cat(sprintf("Decision : t_obs = %.4f <= t_critique = %.4f\n", t_obs, t_critique))
      cat("=> On NE REJETTE PAS H0\n")
      cat("=> Il n'existe pas de liaison significative positive entre X et Y\n")
      cat("=> X et Y sont independantes\n")
    }

  } else {
    if (t_obs < t_critique) {
      cat(sprintf("Decision : t_obs = %.4f < t_critique = %.4f\n", t_obs, t_critique))
      cat("=> On Rejette H0\n")
      cat("=> Il existe une liaison significative negative entre X et Y\n")
      cat("=> X et Y ne sont PAS independantes\n")
    } else {
      cat(sprintf("Decision : t_obs = %.4f >= t_critique = %.4f\n", t_obs, t_critique))
      cat("=> On NE REJETTE PAS H0\n")
      cat("=> Il n'existe pas de liaison significative negative entre X et Y\n")
      cat("=> X et Y sont independantes\n")
    }
  }

}
# Lancer le test
test_spearman()
