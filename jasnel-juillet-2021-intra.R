#1.Importation et mise en forme

#Importation du fichier 
monFichier=read.table(file.choose(), header=FALSE,sep=",")

# Renommer les variables du fichier importer 
names(monFichier)[c(1,2,3,4,5,6,7)]=c("sex","age","abo","sitfam","soc","zau","sal")

#Nombre de variables et d’observations
str(monFichier) #7 variables et 200 obs

#le dictionnaire des variables 
str(monFichier)

#les 6 premières lignes de chaque variable
head(monFichier)

View(monFichier)

#2 Traitement des valeurs manquantes

#des valeurs manques dans le jeu de données.
which(is.na(monFichier),arr.ind=TRUE)

#valeurs aberrantes dans le jeu de données
sapply(monFichier,function(x) sum(is.na(x)))

#3 Création de variables

#Une variable prenant la valeur 1 lorsque l’individu ne désire pas s’abonner
UtilisateurAbo<- monFichier$UtilisateurAbo<-ifelse(monFichier$abo==0,1,0)
UtilisateurAbo

#des variables binaire associées à la variable caractérisant le sexe
binsex<-monFichier$binsex<-ifelse(monFichier$sex=="Homme",1,0)
binsex

#Recodifier la variable caractérisant la situation familiale : 1- Marié, 2-Celibataire 3-Divorcé
recositfam<-monFichier$recositfam<-ifelse(monFichier$sitfam ==1, "Marié", ifelse(monFichier$sitfam==2, "Celibataire", "Divorcé"))
recositfam

#Recodifier à la variable caractérisant la CSP : Ouvrier-> 1, Employé-> 2, Cadre-> 3 
recosoc<-monFichier$recosoc<-ifelse(monFichier$soc =="Ouvrier",1, ifelse(monFichier$soc=="Employe",2,3))
recosoc

# Une variable prenant la valeur 1 lorsque l’individu vie en zone urbaine
UtilisateurUrb<-monFichier$UtilisateurUrb<-ifelse(monFichier$zau> 0 & monFichier$zau<4,1,0)
UtilisateurUrb

#Une variable égale au salaire divisé par 1000
Salairediv1000<-monFichier$Salairediv1000<-(monFichier$sal/1000)
Salairediv1000

#Une variable égale au salaire au carré divisé par 100000
salairediv100000<-monFichier$salairediv100000<-((monFichier$sal * monFichier$sal)/100000)
salairediv100000

#Une variable égale au carré de l’âge
carreage<-monFichier$carreage<-(monFichier$age * monFichier$age)
carreage

#Une variable égale au logarithme de l’âge
logage<-monFichier$logage<-(log10(monFichier$age))
logage

#Transformer la variable salaire en tranche de salaire en utilisant votre propre estimation de classes
salTranch<-cut(monFichier$sal,breaks=c(1200,20503,27601, 50000))
monFichier<-cbind(monFichier,salTranch)
View(salTranch)

#Analyse descriptive

# Calculer la moyenne, médiane l’écart-type, minimum, maximum et les quartiles pourl’ensemble des variables continues du fichier de la base

#Moyenne
moyage<-mean(monFichier$age)
moysal<-mean(monFichier$sal)
moysaldiv1000<-mean(monFichier$salairediv1000)
moysaldiv100000<-mean(monFichier$salairediv100000)
moycarreage<-mean(monFichier$carreage)

moyage
moysal
moysaldiv1000
moysaldiv100000
moycarreage

#Médiane
medage<-median(monFichier$age)
medsal<-median(monFichier$sal)
medsaldiv1000<-median(monFichier$salairediv1000)
medsaldiv100000<-median(monFichier$salairediv100000)
medcarreage<-median(monFichier$carreage)

medage
medsal
medsaldiv1000
medsaldiv100000
medcarreage

#Maximum
maxage<-max(monFichier$age)
maxsal<-max(monFichier$sal)
maxsaldiv1000<-max(monFichier$salairediv1000)
maxsaldiv100000<-max(monFichier$salairediv100000)
maxcarreage<-max(monFichier$carreage)

maxage
maxsal
maxsaldiv1000
maxsaldiv100000
maxcarreage

#Minimum
minage<-min(monFichier$age)
minsal<-min(monFichier$sal)
minsaldiv1000<-min(monFichier$salairediv1000)
minsaldiv100000<-min(monFichier$salairediv100000)
mincarreage<-min(monFichier$carreage)

minage
minsal
minsaldiv1000
minsaldiv100000
mincarreage

#Ecart type
ecage<-sd(monFichier$age)
ecsal<-sd(monFichier$sal)
acsaldiv1000<-sd(monFichier$salairediv1000)
ecsaldiv100000<-sd(monFichier$salairediv100000)
eccarreage<-sd(monFichier$carreage)

ecage
ecsal
acsaldiv1000
ecsaldiv100000
eccarreage

#Quartile
qage<-quantile(monFichier$age)
qsal<-quantile(monFichier$sal)
qsaldiv1000<-quantile(monFichier$salairediv1000)
qsaldiv100000<-quantile(monFichier$salairediv100000)
qcarreage<-quantile(monFichier$carreage)

qage
qsal
qsaldiv1000
qsaldiv100000
qcarreage

#Présenter un tableau d’effectif et de fréquence des variables « sitfam », « soc » qui sont recofidifié et « abo »

#tableau d’effectif
efsitfam<-table(monFichier$recositfam)
efrecosoc<-table(monFichier$recosoc)
efabo<-table(monFichier$UtilisateurAbo)

efsitfam
efrecosoc
efabo

#tableau fréquence 
frsitfam<-prop.table(efsitfam)
frrecosoc<-prop.table(efrecosoc)
frabo<-prop.table(efabo)

frsitfam
frrecosoc
frabo

#Extraction

#Afficher tous les ages qui sont supérieur à la moyenne de cette même variable.
monFichier$age[monFichier$age > moyage]

#Afficher tous les salaires compris entre le premier et le troisième quartile
salaireComPreEtTroQua<-monFichier$sal[monFichier$sal > qsal[c(2)] & monFichier$sal < qsal[c(4)] ]
View(salaireComPreEtTroQua)

#Afiicher la liste des cadres qui habite en zone urbaine 
listCadre<-monFichier[monFichier$soc=="Cadre" & monFichier$UtilisateurUrb ==1,]
View(listCadre)

#Afiicher la liste des employés qui sont mariés
lisemppasmarie<-monFichier[monFichier$soc=="Employe" & monFichier$recositfam == "Marié",]
View(lisemppasmarie)

#Extraire la liste des abonnées qui sont des hommes
listAboHom<-monFichier[monFichier$abo==1 & monFichier$sex=="Homme",]
View(listAboHom)

#Extraire la liste des ouvriers qui sont des femmes et qui touche un salaire inferieure à la médiane de cette dernière variable
listOuvFemmSalInfMed<-monFichier[monFichier$soc=="Ouvrier" & monFichier$sex=="Femme" & monFichier$sal < medsal,]
View(listOuvFemmSalInfMed)

# la liste des abonnés célibataires dont l’age ne dépasse pas la moyenne d’age.
listAboCelAgeInfMoyAge<-monFichier[monFichier$abo==1 & monFichier$recositfam == "Celibataire" & monFichier$age < moyage,]
View(listAboCelAgeInfMoyAge)