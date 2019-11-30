#------------------#
#--- uloha c.6 ----#
#-- Miroslav Kubu -#
#------------------#


#install.packages("rlang")
#install.packages("easyGgplot2")
library(rlang)
library(caret)
library(lattice)
library(ggplot2)
library(corrplot)
library(ggplot)

getwd()
setwd('C:/Users/User/Desktop/ZLIM')




#------------------#
#-DATA preparation-#
#------------------#

data <- read.table("lowbwt.dat")
#View(data)

#rename datasets
colnames(data)
names(data)[names(data) == "V1"] <- "ID"
names(data)[names(data) == "V2"] <- "BIRTH"
names(data)[names(data) == "V3"] <- "SMOKE"
names(data)[names(data) == "V4"] <- "RACE"
names(data)[names(data) == "V5"] <- "AGE"
names(data)[names(data) == "V6"] <- "LWT"
names(data)[names(data) == "V7"] <- "BWT"
names(data)[names(data) == "V8"] <- "LOW"


attach(data)

ID <- factor(ID)
BIRTH <- factor(BIRTH)
SMOKE <- factor(SMOKE)
RACE <- factor(RACE)
LOW <- factor(LOW)


#------------------#
#-DATA exploration-#
#------------------#

#sum NaN
sum(is.na(data))


#summary
summary(data)

#basic histograms

hist(AGE, main = "", breaks = 40, col=rgb(1,0,0,0.5))
box()

hist(BWT, main = "", breaks = 20, col=rgb(0,1,0,0.5))
box()

hist(LWT, main = "", breaks = 20, col=rgb(0,0,1,0.5))
box()

#histograms with LOW indication

length(data[LOW==1,]$AGE)

hist(data[LOW==0,]$AGE, col=rgb(1,0,0,0.5), freq = FALSE,main = "", xlim=c(5,50), ylim=c(0,0.1), xlab="AGE", breaks = 40)
hist(data[LOW==1,]$AGE, col=rgb(0,0,1,0.5), add=T, freq = FALSE , xlim=c(5,50), xlab="AGE", breaks = 20) # or hist(x,plot=FALSE) to avoid the plot of the histogram
legend("topright", c("LOW=0", "LOW=1"), fill=c("red", "blue"),bty = "n")
box()

hist(data[LOW==0,]$LWT, col=rgb(1,0,0,0.5),freq = FALSE ,main = "", xlim=c(50,max(LWT)+20), ylim=c(0,0.02), xlab="LWT", breaks = 10)
hist(data[LOW==1,]$LWT, col=rgb(0,0,1,0.5), add=T, freq = FALSE, xlab="LWT", , breaks = 10)
legend("topright", c("LOW=0", "LOW=1"), fill=c("red", "blue"),bty = "n")
box()


#boxplots

boxplot(AGE~LOW, xlab="LOW", ylab="AGE", 
        col="red", border="brown")

boxplot(LWT~ LOW, xlab="LOW", ylab="LWT", 
        col="red", border="brown")


boxplot(BWT~ SMOKE, xlab="SMOKE", ylab="BWT", names=c("No", "Yes"),
        col="blue", border="gray")

boxplot(BWT~ RACE, xlab="RACE", ylab="BWT", 
        col="blue", border="gray",names=c("White", "Black", "Other"))

boxplot(BWT~ BIRTH, xlab="BIRTH", ylab="BWT", 
        col="blue", border="gray")


#scatter plots

Race <- as.factor(RACE)
qplot(LWT, BWT, colour = Race, 
      data = data)
legend("topright", c("LOW=0", "LOW=1"), fill=c("red", "blue"),bty = "n")

Smoke <- as.factor(SMOKE)
qplot(LWT, BWT, colour = Smoke, 
      data = data)
legend("topright", c("LOW=0", "LOW=1"), fill=c("red", "blue"),bty = "n")

Birth <- as.factor(BIRTH)
qplot(LWT, BWT, colour = Birth, 
      data = data)
legend("topright", c("LOW=0", "LOW=1"), fill=c("red", "blue"),bty = "n")


Race <- as.factor(RACE)
qplot(AGE, BWT, colour = Race, 
      data = data)
legend("topright", c("LOW=0", "LOW=1"), fill=c("red", "blue"),bty = "n")

Smoke <- as.factor(SMOKE)
qplot(AGE, BWT, colour = Smoke, 
      data = data)
legend("topright", c("LOW=0", "LOW=1"), fill=c("red", "blue"),bty = "n")

Birth <- as.factor(BIRTH)
qplot(AGE, BWT, colour = Birth, 
      data = data)
legend("topright", c("LOW=0", "LOW=1"), fill=c("red", "blue"),bty = "n")

#cont. tables

(table(SMOKE,LOW))
(table(LOW,RACE))
(table(LOW,BIRTH))

#corr matrix

correlations <- cor(data[,2:8])
corrplot(correlations, method = "number")


#pie charts


##Non smoker
slices <- c(114,81)
lbls <- c("LOW = 0", "LOW = 1")
pie(slices, labels = lbls, main="", col = c("white", "gray"))

##Non smoker
slices <- c(223,70)
lbls <- c("LOW = 0", "LOW = 1")
pie(slices, labels = lbls, main="", col = c("white", "gray"))


##White
slices <- c(114,100)
lbls <- c("LOW = 0", "LOW = 1")
pie(slices, labels = lbls, main="", col = c("cornsilk", "red"))

##Black
slices <- c(57,15)
lbls <- c("LOW = 0", "LOW = 1")
pie(slices, labels = lbls, main="", col = c("cornsilk", "red"))

##Other
slices <- c(136,36)
lbls <- c("LOW = 0", "LOW = 1")
pie(slices, labels = lbls, main="", col = c("cornsilk", "red"))


##Birth1
slices <- c(135,53)
lbls <- c("LOW = 0", "LOW = 1")
pie(slices, labels = lbls, main="", col = c("dodgerblue", "navy"))

##Birth2
slices <- c(131,57)
lbls <- c("LOW = 0", "LOW = 1")
pie(slices, labels = lbls, main="", col = c("dodgerblue", "navy"))

##Birth3
slices <- c(62,36)
lbls <- c("LOW = 0", "LOW = 1")
pie(slices, labels = lbls, main="", col = c("dodgerblue", "navy"))

##Birth4
slices <- c(9,5)
lbls <- c("LOW = 0", "LOW = 1")
pie(slices, labels = lbls, main="", col = c("dodgerblue", "navy"))


#-----------------------#
#---Sestaveni modelu----#
#-----------------------#

#saturated model
modmax1<-glm(LOW~AGE+BIRTH+SMOKE+RACE+LWT, family = binomial(link = "logit"))
summary.glm(modmax1)
modmax2<-glm(LOW~AGE+BIRTH+SMOKE+RACE+LWT, family = binomial(link = "probit"))
summary.glm(modmax2)
modmax3<-glm(LOW~AGE+BIRTH+SMOKE+RACE+LWT, family = binomial(link = "cloglog"))
summary.glm(modmax3)

#elimination for AIC
step(modmax1, direction="backward", k=2)
step(modmax2, direction="backward", k=2)
step(modmax3, direction="backward", k=2)



#actual model
model1<-glm(LOW~AGE+SMOKE+RACE+LWT, family = binomial(link = "logit"))
summary(model1)
model2<-glm(LOW~SMOKE+RACE+LWT, family = binomial(link = "probit"))
summary(model2)
model3<-glm(LOW~AGE+SMOKE+RACE+LWT, family = binomial(link = "cloglog"))
summary(model3)

#AIC and BIC

AIC(model1)
AIC(model2)
AIC(model3)

BIC(model1)
BIC(model2)
BIC(model3)

#deviance

deviance(model1)
deviance(model2)
deviance(model3)

#-----------------------------#
#----- Diagnostika modelu ----#
#-----------------------------#


#zamerime se na model logit
model<-model1
modmax<-modmax1

# Hosmer-Lemeshow test
#install.packages("ResourceSelection")
library(ResourceSelection)
pok <- hoslem.test(model1$y, fitted(model1),g=10); pok


#compare to saturated
anova(model,modmax, test="Chisq")

#odds ratio a prislusne 95% intervay spolehlivosti
exp(cbind(coef(model), confint(model)))
confint(model)


# predikovane pravdepodobnosti 
prob <- fitted(model) 


# Scatterplot pro "Alive" a "predikovane pravdepodobnosti".
plot(data$LOW,prob, xlab = "True LOW", , ylab = "Prediction")


# rezidua a Cookova vzdalenost
res.ps <- rstandard(model, type = "pearson")  # standardizovana Pearsonova rezidua
res.ds <- rstandard(model, type = "deviance")  # standardizovana deviacni rezidua

cook.d <- cooks.distance(model)  # Cookova vzdalenost
leverage <- hatvalues(model)  # diag. prvky projekcni matice
sort(cook.d)   #serazeni indexu podle hodnot Cook dist. a hat val.  
sort(leverage)       

#pom. cisla pro vypocet mezi
X.model<-model.matrix(model)
n.model<-nrow(X.model); p.model<-ncol(X.model); n.model; p.model

#hat values pro leverage points 
barplot(leverage, ylim = c(0,0.061),xlab="index pozorování",ylab="hat value")
abline(2*p.model/(n.model),0, col = "red")
box()


#model bez vlivnych pozorovani 400, 8, 111, 112, 391 a 392

data_robust <- data[-c(400, 8, 111, 112, 391, 392), ]
data_robust$SMOKE <- factor(data_robust$SMOKE)
data_robust$RACE <- factor(data_robust$RACE)
model_robust<-glm(data_robust$LOW~data_robust$AGE+data_robust$SMOKE+data_robust$RACE+data_robust$LWT, family = binomial(link = "logit"))
summary(model_robust)
coef(model_robust)
confint(model_robust)



#Cooks distance pro infl. meas.
barplot(cook.d, ylim = c(0,0.041),xlab="index pozorování",ylab="Cookova vzdálenost")
abline(8/(n.model-2*p.model),0,col="blue")
box()

## obrazky rezidui 
# rezidua vs. fitted values
plot(prob,res.ps,col="red", cex=1.5, lwd=2, xlab = "prediction", , ylab = "resid. (Pearson)")
abline(0,0)
plot(prob,res.ds,col="red", cex=1.5, lwd=2, xlab = "prediction", , ylab = "resid. (deviane)")
abline(0,0)


# srovnani rezidui
res.ps.clog <- rstandard(model3, type = "pearson")
res.ps.probit <- rstandard(model2, type = "pearson")

res.ds.clog <- rstandard(model3, type = "pearson")
res.ds.probit <- rstandard(model2, type = "pearson")

# Boxploty rezidui
boxplot(res.ps.clog, res.ps.probit, res.ps, col = c(3,5,7), names=c("cloglog","probit","logit"), main=" ")
abline(0,0)

boxplot(res.ds.clog, res.ds.probit, res.ds, col = c(3,5,7), names=c("cloglog","probit","logit"), main=" ")
abline(0,0)


plot(AGE,prob, col = "darkorchid4", xlab = "AGE",  ylab = "prediction")

plot(RACE,prob, col = "darkorchid1", xlab = "RACE",  ylab = "prediction", names=c("White", "Black", "Other"))

plot(SMOKE,prob, col = "firebrick3",  xlab = "SMOKE",  ylab = "prediction", names=c("Non smoker", "Smoker"))


plot(data$AGE,res.ps,col="red", cex=1.5, lwd=2)
abline(0,0)

plot(data$LWT,res.ps,col="red", cex=1.5, lwd=2)
abline(0,0)

plot(data$RACE,res.ps,col="red", cex=1.5, lwd=2)
abline(0,0)

plot(data$SMOKE,res.ps,col="red", cex=1.5, lwd=2)
abline(0,0)



#-----------------------------#
#----- Vyhodnoceni modelu ----#
#-----------------------------#


#sensitivita a specificita

n<-100  
sens<-rep(1,n)
spec<-rep(1,n)
total0<-sum((1-data$LOW))   # pocet pripadu LOW=0
total1<-sum(data$LOW)   # pocet pripadu LOW=1


c<-rep(1,n)
for (i in 1:n) {
  c[i]<-i/n
  sens[i]<-sum(data$LOW* (prob > c[i]))/total1
  spec[i]<-sum((1-data$LOW) * (prob <= c[i]))/total0
}

c; sens; spec

plot(c,spec, main = "", pch=1,col = 1, type = 'l',lty = 2,xlab = "c", ylab = "",ylim=c(0,1),xlim=c(0,1),lwd=2)
lines(c,sens, type = "l", col = 4, pch = 4, lty = 2,lwd=2)
abline(v=0.31,col="green")
text(0.39, 0.1, "c=0,31", col = "green") 
text <- c("Specificita", "Sensitivita")
legend( 0.6, 0.8, text, col = c(1,4), pch=c(1,4), lty = c(2,2), bty="n")

pom<-data.frame(c=c, sensib=sens, specif=spec)
c.val <-subset(pom,0.3<c & c<0.4); c.val
c<-0.31


#boxploty pro prob s c=0,31

plot(LOW,prob,col="firebrick4",xlab = "LOW", ylab="prediction")
abline(0.31,0,lw=3,col="red")

#ROC krivka a AUC

#library(ROCit)
#ROCit_obj <- rocit(score=prob, class=data$LOW)
#plot(ROCit_obj)
#ROCit_obj

library(InformationValue)
plotROC(data$LOW, prob)
confusionMatrix(data$LOW, prob, threshold = 0.29)


#vypocet chyby

# Vypocet chyb pro LOW=0 
R<-data.frame(Y=data$LOW,Y.hat= prob >= c); 
tot0<-sum((1-data$LOW))   # pocet pripadu LOW=0
bad0<-sum(R$Y.hat[R$Y==0]) # pocet false positive
good0<-tot0-bad0    # pocet true negative
error0<-bad0/tot0   
acc0 <- 1-error0  #accuracy pro LOW=0
bad0; tot0; acc0;

# Vypocet chyb pro LOW=1 
tot1<-sum((data$LOW))   # pocet pripadu Alive=0
good1<-sum(R$Y.hat[R$Y==1]) # pocet true positive
bad1<-tot1-good1    # pocet false negative
error1<-bad1/tot1   
acc1 <- 1-error1  #accuracy pro LOW=1
bad1; tot1; acc1;

# Vypocet celkove chyby
bad01<-bad0+bad1
tot01<-tot0+tot1
error01<-bad01/tot01
acc01 <- 1-error01
bad01; tot01; acc01

# Zapis ve forme tabulky
obser1<-rbind(good1,bad1,tot1); obser1
obser0<-rbind(bad0,good0,tot0); obser0
total<-rbind(good1+bad0,bad1+good0,tot01)
tab<-cbind(obser1,obser0,total);
r.names<-c("Clasif=1","Clasif=0","Total")
c.names<-c("Obser=1", "Obser=0", "Total")
dimnames(tab)<-list(r.names,c.names)
tab # tabulka klasifikaci


#control plot histogramu
hist(prob[LOW==1], col=rgb(1,0,0,0.5),xlim=c(0,0.8), ylim=c(0,42), breaks = 20, main="",xlab="c")
hist(prob[LOW==0], col=rgb(0,0,1,0.5),add=T,breaks = 20)
abline(v=0.31,col="red")
text(0.39, 30, "c=0,31", col = "red") 
legend("topright", c("LOW=0", "LOW=1"), fill=c("blue", "red"),bty = "n")
box()




