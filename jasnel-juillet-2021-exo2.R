#Manipulation de champs "dates"

#Création d'objets et affectations de valeurs
Datdeb=c("12/01/90", "02/01/90", "22/04/95", "05/06/91")
Datfin=c("25/02/1990", "15/06/1991", "20/05/1996", "14/04/1992")

#Convertir les objets en date
Datdeb = as.Date(Datdeb,"%d/%m/%y")
Datfin = as.Date(Datfin,"%d/%m/%y")

#Difference entre les date
Duree = difftime(Datdeb, Datfin, units="auto")

#Créer un objet appelé Duree qui contiendra la durée entre Datdeb	et Datfin et l'afficher	sur la console
An = as.character(Datdeb, "%y")
Mois = as.character(Datdeb, "%m")
Jour = as.character(Datdeb, "%d")

# Extraction de données

Poids = seq(20, 45, by=0.5)

#Extraire la 16° valeur	de l'objet Poids
Poids[16]

#Extraire les 1°,	16° et 31° valeurs de l'objet	Poids
Poids[c(1, 16, 31)]

#Extraire les poids dont la valeur est supérieure à 38
Poids[Poids > 38]

#Extraire les poids dont les valeurs sont à la fois supérieures	à 25 et inférieures	à 37
Poids[Poids > 25 & Poids < 37]

#La valeur 1, si le poids est	inférieur à	25, la valeur 2 si le poids est compris entre 25 et 30 et 3 si le poids est supérieur à 30
Classpoids = ifelse(Poids < 25,1, ifelse(Poids > 30, 3, 2))

#l'effectif	de chacun des groupes
summary(as.factor(Classpoids))