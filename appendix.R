# set working directory
setwd("C:/Users/alven/capstone/Konovalov_Krajbich/3 Replication package")
# install required packages, add np for robustness check
install.packages(c("multiwayvcov", "sciplot","texreg","lmtest", "dplyr", "viridis", "np","htmlTable"))

rm(list = ls())
library(multiwayvcov)
library(sciplot)
library(texreg)
library(lmtest)
library(dplyr)
library(viridis)
library(np)
library(htmlTable)
source('myfunctions.R')

load('data/bargaining1.RData')
data = fdata2
ps = c()
ids = unique(data$Subject)
for (i in 1:length(unique(data$Subject))){
  temp = filter(data,Subject == ids[i])
  model <- lm(formula=price2 ~ rt + price1,data=temp,x=TRUE,y=TRUE)
  attach(temp)
  X <- data.frame(rt,price1)
  results = npcmstest(model = model, xdat = X, ydat = price2, boot.method="wild")
  ps[i]=results$P
  detach(temp)
} 
n1 = sum(ps < 0.1 ) 


# Robustness check for table B7
data3 = fdata3

data3$pricedif = data3$price1left - data3$price1right 
data3$valuedif = data3$valueleft - data3$valueright 
data3$rtdif = data3$rtleft - data3$rtright


glm1 = glm(choiceleft~valuedif+ rtdif+pricedif,data = data3,x=TRUE,y=TRUE)
X1 <- data.frame(data3$valuedif,data3$rtdif,data3$pricedif)
results_glm1 = npcmstest(model = glm1, xdat = X1, ydat = data3$choiceleft,boot.method="wild")
Jn1 = results_glm1$Jn
pvalue_glm1=results_glm1$P

glm2 = glm(choiceleft~valuedif+ rtdif+pricedif +Period,data = data3,x=TRUE,y=TRUE)
X2 <- data.frame(data3$valuedif,data3$rtdif,data3$pricedif,data3$Period)
results_glm2 = npcmstest(model = glm2, xdat = X2, ydat = data3$choiceleft,boot.method="wild")
Jn2 = results_glm2$Jn
pvalue_glm2=results_glm2$P

glm3 = glm(choiceleft~valuedif+ pricedif*rtdif+Period,data = data3,x=TRUE,y=TRUE)
X3 <- data.frame(data3$valuedif,data3$pricedif*data3$rtdif,data3$Period)
results_glm3 = npcmstest(model = glm3, xdat = X3, ydat = data3$choiceleft,boot.method="wild")
Jn3 = results_glm3$Jn
pvalue_glm3=results_glm3$P

glm4 = glm(choiceleft~ pricedif + valuedif*rtdif+Period,data = data3,x=TRUE,y=TRUE)
X4 <- data.frame(data3$pricedif,data3$valuedif*data3$rtdif,data3$Period)
results_glm4 = npcmstest(model = glm4, xdat = X4, ydat = data3$choiceleft,boot.method="wild")
Jn4 = results_glm4$Jn
pvalue_glm4=results_glm4$P


table = data.frame(cbind(c(Jn1,Jn2,Jn3,Jn4),c(pvalue_glm1,pvalue_glm2,pvalue_glm3,pvalue_glm4)))
#          Jn statistics     p-value
# valuedif     92.340416 0.000000000
# pricedif     92.188791 0.000000000
# rtdif       109.209160 0.000000000
# Period        1.508805 0.002506266
rownames(table) = c("Model (1)","Model (2)","Model (3)","Model (4)")
names(table) = c("Jn statistics","p-value")
tableout = htmlTable (
  table,
  caption = "Replication results Table: Jn statistics and P-values of consistent test for correct specification of linear models."
)
cat(tableout,file= "output/tables/tableB7_rob.html")

# Robustness check for models in TABLE B2

load('data/bargaining2.RData')

fdata$easy = as.numeric(fdata$price1>fdata$value)
fdata$value = fdata$value/100
fdata$price1 = fdata$price1/100

data = fdata[fdata$part==1 & fdata$visible==0,]
b1 = lm(log(rt1) ~ value*accept1+price1*accept1+Period+easy,data = data)
X1 <- data.frame(value*accept1,price1*accept1,Period,easy)
results_b1 = npcmstest(model = model, xdat = X1, ydat = log(rt1),boot.method="wild")
pvalue_b1=results_b1$P

data = fdata[fdata$part==1 & fdata$visible==1,]
b2 = lm(log(rt1) ~ value*accept1+price1*accept1+Period+easy,data = data)
X2 <- data.frame(value*accept1,price1*accept1,Period,easy)
results_b2 = npcmstest(model = model, xdat = X2, ydat = log(rt1),boot.method="wild")
pvalue_b2=results_b2$P

data = fdata[fdata$part==0 & fdata$visible==0,]
b3 = lm(log(rt1) ~ value*accept1+price1*accept1+Period+easy,data = data)
X3 <= data.frame(value*accept1,price1*accept1,Period,easy)
results_b3 = npcmstest(model = model, xdat = X3, ydat = log(rt1),boot.method="wild")
pvalue_b3=results_b3$P

data = fdata[ fdata$part==0 & fdata$visible==1,]
b4 = lm(log(rt1) ~ value*accept1+price1*accept1+Period+easy,data = data)
results_b4 = npcmstest(model = model, xdat = cbind(value*accept1,price1*accept1,Period,easy), ydat = log(rt1),boot.method="wild")
pvalue_b4=results_b4$P

data = fdata[fdata$part==1,]
b5 = lm(log(rt1) ~ value*accept1+price1*accept1+Period+visible+easy,data = data)
results_b5 = npcmstest(model = model, xdat = cbind(value*accept1,price1*accept1,Period,visible,easy), ydat = log(rt1),boot.method="wild")
pvalue_b5=results_b5$P

data = fdata[fdata$part==0,]
b6 = lm(log(rt1) ~ value*accept1+price1*accept1+Period+value+easy+visible,dat
results_b6 = npcmstest(model = model, xdat = cbind(value*accept1,price1*accept1,Period,value,visible,easy), ydat = log(rt1),boot.method="wild")
pvalue_b6=results_b6$P

data = fdata
data$part = 1 - data$part
b7 = lm(log(rt1) ~ value*accept1+price1*accept1+Period+visible*part+easy,data = data)
results_b7 = npcmstest(model = model, xdat = cbind(value*accept1,price1*accept1,Period,visible*part,easy), ydat = log(rt1),boot.method="wild")
pvalue_b7=results_b7$P

data = fdata[fdata$easy == 0,]
data$part = 1 - data$part
b8 = lm(log(rt1) ~ value*accept1+price1*accept1+Period+visible + part,data = data)
results_b8 = npcmstest(model = model, xdat = cbind(value*accept1,price1*accept1,Period,visible,part), ydat = log(rt1),boot.method="wild")
pvalue_b8=results_b8$P

data = fdata[fdata$easy == 1,]
data$part = 1 - data$part
b9 = lm(log(rt1) ~ value*accept1+price1*accept1+Period+visible + part,data = data)
results_b9 = npcmstest(model = model, xdat = cbind(value*accept1,price1*accept1,Period,visible,part), ydat = log(rt1),boot.method="wild")
pvalue_b9=results_b9$P

htmlreg(list(b1,b2,b3,b4,b5,b6,b7,b8,b9),
        file = "output/tables/tableB2_rob.html", 
        override.pvalues=list(pvalue_b1,pvalue_b2,pvalue_b3,pvalue_b4,pvalue_b5,pvalue_b6,pvalue_b7,pvalue_b8,pvalue_b9)
        include.adjrs = F, 
        digits = 3,
        custom.model.names = c('(1)','(2)','(3)','(4)','(5)','(6)','(7)','(8)','(9)'))