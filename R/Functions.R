
preparation_base <- function(base_ehcvm) {
  colnames(base_ehcvm)[4:14] <- c("AutresCereales","Qtty_cons",
                                "Unite_cons","Taille_cons",
                                "AutoCons","AutresProv",
                                "DernierAchat","Qtty_achat",
                                "Unite_achat","Taille_achat",
                                "Value_achat")
  ##Gestion des NA

  ##Suppression des ménages ne consommant pas de céréales

  ##Création d'une variable temporaire
  base_ehcvm$t <- ifelse(is.na(base_ehcvm$Qtty_cons)==1,1,0)

  ##Suppression des ménages n'ayant pas déclaré les quantités consommées

  base_ehcvm <- base_ehcvm[base_ehcvm$t==0,]
  dim(base_ehcvm)

  ##Suppression de la variable temporaire
  base_ehcvm$t <- NULL
  return(base_ehcvm)

}

qtty_conso <- function(ehcvm){
  #Importation de la base table de conversion
  library(readr)
  table_de_conversion <<- read_delim(file.choose(), ";")
  #------------------Calcul de la quantité consommée en kg----------------------------------

  ehcvm$real_id_cons <- paste(ehcvm[[3]], ehcvm$Unite_cons, ehcvm$Taille_cons)
  table_de_conversion$real_id_cons <- paste(table_de_conversion$produitID, table_de_conversion$uniteID, table_de_conversion$tailleID)


  ###Nous pouvons maintenant fusionner la base viandes et la base table_de_conversion par la variable real_id
  ehcvm1 <- merge(ehcvm, table_de_conversion, by= "real_id_cons", all.x = TRUE)

  ###Nous pouvons à présent créer la variable qui va calculer la consommation totale
  ehcvm1$Qtty_cons<- as.numeric(ehcvm1$Qtty_cons)
  ehcvm1$poids<- as.numeric(ehcvm1$poids)
  ehcvm1$qtotale_conso <-ehcvm1$Qtty_cons*ehcvm1$poids/1000
  return(ehcvm1)
}



