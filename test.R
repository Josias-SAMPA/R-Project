# =====================================================
# TEST DE CORRELATION DE SPEARMAN (MANUELLEMENT)
# Sans utilisation de cor() ou cor.test() pour Spearman
# =====================================================

test_spearman <- function() {

  cat("=== TEST DE SPEARMAN (Calcul détaillé) ===\n\n")

  # ----------------- SAISIE DES DONNEES -----------------
  n <- as.integer(readline("Entrez le nombre d'observations (n) : "))
  if (is.na(n) || n < 3) stop("n doit être un entier >= 3.")

  cat("\nSaisie des valeurs de la variable X :\n")
  X <- numeric(n)
  for (i in 1:n) {
    X[i] <- as.numeric(readline(paste0("X[", i, "] = ")))
  }

  cat("\nSaisie des valeurs de la variable Y :\n")
  Y <- numeric(n)
  for (i in 1:n) {
    Y[i] <- as.numeric(readline(paste0("Y[", i, "] = ")))
  }

  if (any(is.na(X)) || any(is.na(Y))) stop("Des valeurs manquantes ont été détectées.")

  # ----------------- AFFICHAGE GENERAL DES DONNEES -----------------
  cat("\n=== DONNEES DE L'EXERCICE ===\n")
  donnees <- data.frame(Observation = 1:n, X = X, Y = Y)
  print(donnees)

  # ----------------- ETAPE 1 : FORMULATION DES HYPOTHESES -----------------
  cat("\n=== ETAPE 1 : FORMULATION DES HYPOTHESES ===\n")
  cat("H0 : Il n'existe pas de corrélation monotone entre X et Y (rho = 0)\n")
  cat("H1 : Il existe une corrélation monotone entre X et Y (rho != 0)\n")
  alpha <- 0.05
  cat(sprintf("Niveau de signification alpha = %.2f\n", alpha))

  # ----------------- ETAPE 2 : CALCUL DES RANGS -----------------
  cat("\n=== ETAPE 2 : CALCUL DES RANGS ===\n")

  rank_X <- rank(X, ties.method = "average")
  rank_Y <- rank(Y, ties.method = "average")

  cat("Rangs de X :", rank_X, "\n")
  cat("Rangs de Y :", rank_Y, "\n")

  # Détection des ex-æquo
  has_ties <- any(rank_X != round(rank_X)) || any(rank_Y != round(rank_Y))
  if (has_ties) {
    cat("\n[AVERTISSEMENT] Des ex-aequo ont été détectés.\n")
    cat("La formule simplifiee 1 - 6*sum(d²)/n(n²-1) est une approximation.\n")
    cat("Le résultat peut légèrement différer du coefficient exact.\n")
  }

  # ----------------- ETAPE 3 : DIFFERENCES DE RANGS -----------------
  cat("\n=== ETAPE 3 : DIFFERENCES DE RANGS (d_i) ===\n")
  d  <- rank_X - rank_Y
  d2 <- d^2

  df_diff <- data.frame(Obs = 1:n, rank_X = rank_X, rank_Y = rank_Y, d = d, d2 = d2)
  print(df_diff)

  sum_d2 <- sum(d2)
  cat(sprintf("\nSomme des d_i^2 = %g\n", sum_d2))

  # ----------------- ETAPE 4 : CALCUL DU COEFFICIENT DE SPEARMAN -----------------
  cat("\n=== ETAPE 4 : CALCUL DU COEFFICIENT DE SPEARMAN ===\n")
  rho <- 1 - (6 * sum_d2) / (n * (n^2 - 1))
  cat(sprintf("rho (coefficient de Spearman) = %.4f\n", rho))

  # Force et direction
  if (abs(rho) < 0.3) {
    force <- "faible"
  } else if (abs(rho) < 0.7) {
    force <- "modérée"
  } else {
    force <- "forte"
  }
  direction <- ifelse(rho > 0, "positive", "négative")
  cat(sprintf("Force de la corrélation : %s | Direction : %s\n", force, direction))

  # ----------------- ETAPE 5 : TEST STATISTIQUE -----------------
  cat("\n=== ETAPE 5 : CALCUL DE LA VALEUR DU TEST ===\n")
  cat("Approximation par la loi de Student : t = rho * sqrt((n-2) / (1 - rho^2))\n")

  ddl <- n - 2
  cat(sprintf("Degrés de liberté (ddl) = %d\n", ddl))

  # Cas limite : corrélation parfaite
  if (abs(rho) == 1) {
    cat("\n[INFO] rho = ±1 : corrélation parfaite. La statistique t tend vers l'infini.\n")
    cat("Décision : REJETER H0 (corrélation parfaite).\n")
    cat(sprintf("\n=== RESUME FINAL ===\nrho = %.4f | Corrélation parfaite | REJETER H0\n", rho))
    return(invisible(list(rho = rho, t = Inf, p_value = 0, decision = "REJETER H0")))
  }

  t_calc <- rho * sqrt((n - 2) / (1 - rho^2))
  p_val  <- 2 * pt(-abs(t_calc), df = ddl)

  cat(sprintf("Valeur de t calculée = %.4f\n", t_calc))
  cat(sprintf("P-value (bilatérale) = %.4f\n", p_val))

  # ----------------- ETAPE 6 : VALEUR CRITIQUE -----------------
  cat("\n=== ETAPE 6 : VALEUR CRITIQUE ===\n")
  t_crit <- qt(1 - alpha / 2, df = ddl)
  cat(sprintf("Valeur critique t (bilatéral, alpha/2 = %.3f) ≈ %.4f\n", alpha / 2, t_crit))

  # ----------------- ETAPE 7 : PRISE DE DECISION -----------------
  cat("\n=== ETAPE 7 : PRISE DE DECISION ===\n")
  if (abs(t_calc) > t_crit) {
    decision   <- "REJETER H0"
    conclusion <- sprintf(
      "Il existe une corrélation monotone significative entre X et Y au seuil de %.0f%%.",
      alpha * 100
    )
  } else {
    decision   <- "NE PAS REJETER H0"
    conclusion <- sprintf(
      "Il n'y a pas de corrélation monotone significative entre X et Y au seuil de %.0f%%.",
      alpha * 100
    )
  }
  cat("Décision :", decision, "\n")

  # ----------------- ETAPE 8 : CONCLUSION -----------------
  cat("\n=== ETAPE 8 : CONCLUSION ===\n")
  cat(conclusion, "\n")
  cat(sprintf("Coefficient de Spearman rho = %.4f\n", rho))

  # ----------------- RESUME FINAL -----------------
  cat("\n=== RESUME FINAL ===\n")
  cat(sprintf(
    "rho = %.4f | t = %.4f | t_crit ≈ %.4f | p-value = %.4f | %s\n",
    rho, t_calc, t_crit, p_val, decision
  ))

  invisible(list(rho = rho, t = t_calc, t_crit = t_crit, p_value = p_val, decision = decision))
}

# Lancer le test
test_spearman()
