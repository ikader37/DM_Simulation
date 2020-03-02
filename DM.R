#Exercice 1
curve(dpois(x,1), col="red" ,-5,20)
#Exercice 2
#X->B(5,0.6)
#1)P(X<=4)
X_inf_4<-pbinom(4,5,0.6)
print(X_inf_4)
#2)graphe de la fonction de repartion de X
F<-stepfun(-1:6,c(0,pbinom(-1:6,5,0.6)))
plot(F,vertical=FALSE ,col="red", ,main = "Fonction de repartition de la loi B(5,0.6)")
#3)determiner k tel que P(X<=k)>=0.25
X_inf_k<-qbinom(0.25,5,0.6)
print(X_inf_k)
#4)simuler 20 realisations de X
sim_X<-rbinom(20,5,0.6)
table(sim_X)

#Exercice 3
#la densite de la loi de chi-deux (3) sur [0,10]
curve(dchisq(x,3),0,10,col="red",main="Graphe de densite de la loi de chi-deux de df=3 sur [0.10]")


#Exercice 4
#divisons l'ecran graphique en 3(1 ligne , 3 colonnes)
par(mfrow=c(1,3))
#Dans la première fenêtre représenter le graphe de la densité de la loi normale N (4, 1), puis
#ajouter, dans la même fenêtre, avec une autre couleur, le graphe de la densité de la loi
#normale N (5, 1).
curve(dnorm(x,4,1),-2,10,col="red",add = T)
curve(dnorm(x,5,1),-2,10,col="blue",add = T)














