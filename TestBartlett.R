# Fonction de saisie
saisirNombre <- function(message) {
  as.numeric(readline(prompt = message))
}

test_Bartlett <- function() {
  
  # Nombre de groupes
  K <- saisirNombre("Entrez le nombre de groupes : ")
  
  groupes <- list()
  
  ni<-c()
  s2_i<-c()
  
  # Saisie des groupes
  for (i in 1:K) {
    
    cat(" Groupe", i, "\n")
    
    n <- saisirNombre("Taille du groupe : ")
    
    valeurs <- c()
    
    for (j in 1:n) {
      x <- saisirNombre(paste("Valeur", j, ": "))
      valeurs <- c(valeurs, x)
    }
    
    groupes[[i]] <- valeurs
    
    ni[i]<-length(valeurs)
    s2_i[i]<-var(valeurs)
    
  }
    cat("la variance des groupes est  = ",s2_i)
    
    N<-sum(ni)
    
    cat("Effectif total = ",N,"\n")
    
    sp2<- sum((ni-1)*s2_i)/(N-K)
    
    express_numerateur<-(N-K)*log(sp2)-sum((ni-1)*log(s2_i))
    
    express_denominateur<- 1 + (1 / (3 * (k - 1))) * (sum(1 / (ni - 1)) - 1 / (N - k))
  
    
    B_test<-express_numerateur/express_denominateur
    
    cat("la valeur de test  = ",B_test,"\n")
    
    #calcul de la valeur critique
    alpha<-saisirNombre("Entrez votre de signification(0.05) : \n")
    
    ddl<-K-1
    
    Bk_alpha<-qchisq(1-alpha,ddl )
    
    B_crit<-(1/N)*sum(ni*Bk_alpha)
    
    cat("La valeur critique est ",B_crit,"\n")
    
    #Prise de decision
    if(B_test > B_crit){
      cat("on rejette L'hypothese nulle H0")
    }else{
      cat("on accepte L'hypothese alternative H1")
    }
    
  
}

test_Bartlett()