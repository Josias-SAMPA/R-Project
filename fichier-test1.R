
n<-36

moy_theorique<-10.5

x_bar<-8.5

s<-1.5

seuil_signifi<-0.05

#formulation d'hypothese
cat("Donnees \n")
cat("H0:mu1 == mu0 \n")
cat("H1:mu1 > mu0 \n")

cat("\n")
#loi de probabilite a utiliser
if(n>=30){
  cat(paste("comme n>=30 Alors on vas utiliser la loi normale, Test Unilateral\n"))
  cat("\n")
  s_xbar = s/sqrt(n)
  cat(paste(s_xbar))
  
  cat("\n")
  cat("\n")
  z_obs<-round((x_bar-moy_theorique)/s_xbar,4)
  cat(paste("z_obs=",z_obs,"\n"))
  #valeur critique
  
  seuil_signifi<-0.01
  
  resultat<-1-(seuil_signifi)/2
  
  z_<-round(qnorm(resultat),2)
  
  cat("\n")
  cat(z_)
  cat("\n")
  borne_inf<- -z_
  borne_sup<-  z_
  
  cat("\n")
  cat(paste("Zone de non rejet de H0 est :[-Inf ;",borne_sup,"] \n"))
  
  #prise de decision et conclusion
  
  #prise de decision
  
  cat("\n")
  if(z_obs>=borne_inf & z_obs<=borne_sup){
    cat(paste(z_obs,"appartient inclus a [ -Inf ;",borne_sup,"] \n"))
    cat("\n")
    cat(paste("Ho est accepte \n"))
  }else{
    cat(paste(z_obs,"appartient non inclus dans [-Inf ;",borne_sup,"] \n"))
    cat("\n")
    cat(paste("Donc H0 est rejette et H1 est accepte \n"))
    cat("\n")
    cat(paste("Conclusion \n"))
    cat("\n")
    cat(paste("Au risque de se tromper  de ",seuil_signifi*100,"% la note moyenne des etudiants est superieur a  10,5\n"))
    
    x_val<-seq(-4,4,length=1000)
    y_val<-dnorm(x_val)
    
    plot(x_val,y_val,type="l",lwd=2,col="blue",main="Loi Normale - Test unilateral",xlab="Z",ylab="Densite")
    
    x_left<-seq(-4,borne_inf,length=200)
    polygon(c(-4, x_left,borne_inf),c(0,dnorm(x_left),0),col="red",border = NA)
    
    x_right<-seq(borne_sup,4,length=500)
    polygon(c(borne_sup,x_right,4),c(0,dnorm(x_right),0),col="lightgreen",border = NA)
    
    
    
    abline(v=z_obs,col="purple",lwd=2,lty=2)
    
    abline(v=borne_inf,col="darkred",lwd=2,lty=2)
    
    
  }
  
}else{
  print("Alors on vas utiliser la loi de Student")
}



