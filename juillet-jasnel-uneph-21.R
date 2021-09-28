#Ex1 effacer tous les objets la console de R
rm(list=ls())
#verifier si les objets ont ete effacer
ls()

#Ex2	- Manipulation	de	chaînes	de	caractère

#Objet prenom
Prenom="Jasnel"

#objet nom
Nom="Juillet"

#Creer un objet qui donera votre nom et prenom separe par une virgule
Identite=paste(Nom,Prenom,sep=",")

#Afficher le contenu
Identite

#Ex3	- Manipulation	de	données	numériques

#La population féminine d'une	région d'une année.
Femme=257836

#La population masculine d'une	région d'une année.
Homme=247643

Population=sum(Femme, Homme)

#Afficher le total de la Population
Population

#rapport Homme/Femme
Sexratio=Homme/Femme

#Afficher le rapport Homme/Femme
Sexratio

decFemme=236
decHomme=328
totalDes = sum(decFemme,decHomme)

#le taux de mortalité pour 1000 personnes pour chacun des 2 sexes et pour l'ensemble de la population.

mortaliteFemme=(decFemme/Femme)*1000
mortaliteHomme=(decHomme/Homme)*1000
mortaliteTotale=(totalDes/Population)*1000

#Affichage des objets

decFemme
decHomme
totalDes
mortaliteFemme
mortaliteHomme
mortaliteTotale

#Ex4 - Afficher dans la console la liste des objets créés dans
ls()