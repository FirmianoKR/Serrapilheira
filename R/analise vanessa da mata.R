rm (list=ls(all=TRUE))

eb2<-read.table("eb2.txt", h=T)
eb2
attach(eb2)
summary(eb2)
plot(eb2)
biomassa=glm(biomassa~folhas*altura)
anova(biomassa,test="Chisq")
# n?mero de folhas e altura determinam a biomassa


m.biomassa=glm(biomassa~folhas+altura)
anova(m.biomassa,test="Chisq")
summary(m.biomassa)
plot (biomassa~folhas)
# fazer os graficos de biomassa em fun??o do n?mero de folhas e altura

biomassa3=glm(biomassa~folhas)
anova(biomassa3,test="Chisq")
summary(biomassa3)

plot(biomassa~folhas,pch=16,bty="l",ylim=c(0,120),xlab="N?mero de folhas",ylab="Biomassa de serapilheira (g)")
curve(0.34949+0.76385*x,add=T,col="red")


biomassa4=glm(biomassa~altura)
anova(biomassa4,test="Chisq")
summary(biomassa4)
plot(biomassa~altura,pch=16,bty="l",ylim=c(0,120),xlab="Altura (cm) ",ylab="Biomassa de serapilheira (g)")
curve(-21.34546+0.59252*x,add=T,col="red")


#analise de riqueza


eb2<-read.table("eb2.txt", h=T)
eb2
attach(eb2)
eb2=glm(riqueza~biomassa*altura*folhas*copa*mancha,family=poisson)
anova(eb2,test="Chisq")

eb2=glm(riqueza~biomassa+altura+mancha,family=quasipoisson)
anova(eb2,test="Chisq")
summary(eb2)


eb3=glm(riqueza~biomassa,family=quasipoisson)
anova(eb3,test="Chisq")
summary(eb3)
plot(riqueza~biomassa,pch=16,bty="l",ylim=c(0,25),xlab="Biomassa de serapilheira (g)",ylab="Riqueza de taxa")
curve(exp(2.147663+0.007803*x),add=T,col="red")


