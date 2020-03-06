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
curve(dnorm(x,4,1),-2,10,col="red")
curve(dnorm(x,5,1),-2,10,col="blue",add = T)
legend("topleft",legend = c("N(4,1)","N(5.1)"),col = c("red","blue"),lty = 1:2,cex = 0.8,title = "Courbe densite",text.font = 4,bg="lightblue")

#Dans la deuxième fenêtre représenter le graphe de la densité de la loi normale N (4, 1), puis
#ajouter, dans la même fenêtre, avec une autre couleur, le graphe de la densité de la loi
#normale N (4, 4).

curve(dnorm(x,4,1),-2,10,col="yellow")
curve(dnorm(x,4,4),-2,10,col="orange",add = T)
legend("topleft",legend = c("N(4,1)","N(4,4)"),col = c("yellow","orange"),lty = 1:2,cex = 0.8,title = "Courbe densite",text.font = 4,bg="lightblue")

#Dans la troisième fenêtre représenter le graphe de la densité de la loi normale N (4, 1), puis
#ajouter, dans la même fenêtre, avec une autre couleur, le graphe de la densité de la loi
#normale N (5, 4).
curve(dnorm(x,4,1),-2,10,col="pink")
curve(dnorm(x,5,4),-2,10,col="black",add = T)
legend("topleft",legend = c("N(4,1)","N(5,4)"),col = c("pink","black"),lty = 1:2,cex = 0.8,title = "Courbe densite",text.font = 4,bg="lightblue")

#Exercice 5
#Z->N(0,1)
#P(Z<=-0.5)
pnorm(-0.5,0,1)

1-pnorm(1.5,0,1)
1-pnorm(-1,0,1)

#P(|Z|<=1.96)=P(Z<=1.96)-P(Z<=-1.96)
pnorm(1.96,0,1)-pnorm(-1.96,0,1)


pnorm(1.96,0,1)-pnorm(-1.96,0,1)

#P(|Z| ≤ 2.58)=F(2.58)-F(2.58)
pnorm(2.58,0,1)-pnorm(-2.58,0,1)
#P(|Z|>=3)
1+pnorm(-3,0,1)-pnorm(3,0,1)



#Exercice 6
#X->N(15,9)
#1)
#P(16<=X<=20)
p_1<-sum(dnorm(16:20,15,9))
p_2<-1-pnorm(18,15,9)

p_3<-pnorm(5,15,9)
#P(|X-15|>5,88)=P(-5,88<=X-15<=5,88)=P(-5,88+15<=X<=5,88+15)=F(20,88)-F(10,88)
p_4<- pnorm(20.88,15,9)-pnorm(10.88,15,9)

print(p_4)

Fn_R<-stepfun(6:24,c(0,pnorm(6:24,15,9)))
plot(Fn_R,vertical=FALSE ,col="red", ,main = "Fonction de repartition de la loi N(15,9) sur [6,24]")

#Exercice 7
Fn_B<-stepfun(-1:51,c(0,pbinom(-1:51,50,0.4)))
plot(Fn_B,vertical=FALSE ,col="red", ,main = "Fonction de repartition de la loi B(50,0.4) sur [-1,51]")

curve(pnorm(x,20,12),col="blue",add = T)
legend("bottomright",legend = c("B(50,0.4)","N(20,12)"),col = c("red","blue"),lty = 1:2,cex = 0.8,title = "Courbe fonction repartition",text.font = 4,bg="lightblue")

#Exercice 10

#divisons l'ecran graphique en 2 ligne 2 colonnes
par(mfrow=c(2,2))
curve(dexp(x,1),xlim =c(0,3) ,col="Red",main = "graphe de la densite de la loi Exp(1) sur [0;3]")

curve(dexp(x,2),xlim=c(0,3),col="blue",main = "graphe de la densite de la loi Exp(2) sur [0,3]")

curve(dexp(x,0.5),xlim=c(0,20),col="black",main = "graphe de la densite de la loi Exp(0.5) sur [0;20]")

curve(dexp(x,0.1),xlim=c(0,60),col="pink",main = "graphe de la densite de la loi Exp(0.1) sur [0;60]")

#Exercice 8
#1)
z_1<-qnorm(0.00135,0,1)
z_2<-qnorm(0.025,0,1)
z_3<-qnorm(0.95,0,1)
z_4<-qnorm(0.999,0,1)
z_5<-qnorm(0.995,0,1)
z_6<-qnorm(0.99865,0,1)

print(z_1)
print(z_2)
print(z_3)
print(z_4)
print(z_5)
print(z_6)


#2)

x_1<-qnorm(0.975,19,3)
x_2<-qnorm(0.025,19,3)
print(x_1)
print(x_2)

x_3<-19+sqrt(3)*qnorm(0.975,0,1)
print(x_3)

