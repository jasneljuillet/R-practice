#Final

#6 Description d’une variable quantitative en fonction d’une variable qualitative

#6.1 Déterminer la moyenne des salaires annuelles suivant les catégories socioprofessionnelles
by(monFichier$sal,monFichier$soc,mean,na.rm=T)

#6.2Afficher l’âge moyen selon que la personne décide de s’abonner ou non
by(monFichier$age,monFichier$abo,mean,na.rm=T)

#6.3 Quel est le salaire moyen par zone.
by(monFichier$sal,monFichier$zau,mean,na.rm=T)

#7 Description d’une variable quantitative en fonction d’une variable qualitative.

#7.1. Quel est le pourcentage d’employés y-a-t-il dans la base. Dresser un tableau de contingence
complet.

#Fonctions descriptives.
premierfonction = table(monFichier$sex,monFichier$soc)
secondfonction  = round(prop.table(premierfonction ,1)*100,digit=1)
totalfonc = margin.table(premierfonction ,2)

#construction de la table
count = rbind(premierfonction [1,],secondfonction  [1,],premierfonction [2,],secondfonction  [2,])
dimnames(count) = list(c("Femme","%","Homme","%"),c("Cadre","Employe","Ouvrier"))
count = rbind(count ,totalfonc)
totalfonc = margin.table(count ,1)
count = cbind(count ,totalfonc)

totalfonct = margin.table(premierfonction ,1)
counte = cbind(premierfonction [,1],secondfonction  [,1],premierfonction [,2],secondfonction  [,2],premierfonction [,3],secondfonction  [,3])
dimnames(counte) = list(c("Femme", "Homme"),c("Cadre", "%", "Employe", "%", "Ouvrier", "%"))
counte = cbind(counte ,totalfonc = totalfonct )
totalfonct = margin.table(counte ,2)
totalfonct[2] = round((totalfonct [1]/totalfonct[7])*100,1)
totalfonct[4] = round((totalfonct[3]/totalfonct[7])*100,1)
totalfonct[6] = round((totalfonct[5]/totalfonct[7])*100,1)
counte = rbind(counte ,totalfonc = totalfonct)

count
counte 

#Quel est le pourcentage de personnes qui vivent en zone urbaine (utiliser votre variable « zau » crée au niveau de la question 3.5) . Dresser un tableau de contingence complet.
monFichier$zau2<-ifelse(monFichier$zau<=3,1,0)
monFichier

#Fonctions descriptives.
var = table(monFichier$sex, monFichier$zau2)
var2 = round(prop.table(var ,1)*100,digit=1)
result = margin.table(var ,2)

#construction de la table
count1 = rbind(var [1,],var2[1,],var [2,],var2[2,])
dimnames(count1) = list(c("Femme","%","Homme","%"),c(1,0))
count1 = rbind(count1 ,result 

result2 = margin.table(var ,1)
count2 = cbind(var [,1],var2 [,1],var [,2],var2 [,2])
dimnames(count2 ) = list(c("Femme","Homme"),c(1,"%",0, "%"))
count2 = cbind(count2 ,result = result2)
result2 = margin.table(count2 ,2)
result2[2] = round((result2[1]/result2[5])*100,1)
result2[4]<-round((result2[3]/result2[5])*100,1)
count2 = rbind(count2,result = result2)

count1 
count2 

#8 Analyses bivariées
#Fonctions descriptives.
var4 = table(monFichier$abo,monFichier$soc)
var5 = round(prop.table(var4,1)*100,digit=1)
result3 = margin.table(var4,2)

#construction de la table
#b)Réaliser l’analyse choisie et interpréter les résultats.
count4<-rbind(var4[1,],var5[1,],var4[2,],var5[2,])
dimnames(count4)<-list(c(1,"%",0,"%"),c("Cadre","Employe","Ouvrier"))
count4 = rbind(count4,result3)
result3 = margin.table(count4 ,1)
count4 = cbind(count4 ,result3)

count4 

#c)Réaliser le test approprié et conclure ?
var6 = margin.table(var4 ,1)
count5 = cbind(var4[,1],var5[,1],var4[,2],var5[,2],var4[,3],var5[,3])
dimnames(count5) = list(c(1,0),c("Cadre","%","Employe","%","Ouvrier","%"))
count5 = cbind(count5 ,result3 = var6)
var6 = margin.table(count5,2)
var6[2] = round((var6[1]/var6[7])*100,1)
var6[4] = round((var6[3]/var6[7])*100,1)
var6[6] = round((var6[5]/var6[7])*100,1)
count5 = rbind(count5,result3 = var6)
count5 

#d)preciser l'intensite de la relation
chisq.test(table(monFichier$abo,monFichier$soc),correct=TRUE)
intensite = sqrt(19.236)
intensite 

#9 Les graphiques
#9.1 Proposer un diagramme circulaire pour la variable « Sexe »
Cont = table(monFichier$sex)
Cont
pie(Cont,col='pink',density=c(NA,50,30),main='Diagramme \n circulaire de la variable sexe',labels=c(paste(monFichier$sex[1],';',Cont[1]),paste(monFichier$sex[2],';',Cont[2])))

#9.2 Réaliser un histogramme pour la variable « zau »
hist(monFichier$zau,breaks=c(1,3,7,11),col='green',density=c(NA,50,30),xlab='Zone',ylab='Effectifs',main="Historicite de la variable Zau")