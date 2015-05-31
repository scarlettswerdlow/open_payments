########################################                                                                            #
#  Principle Components Analysis       #
#  Coded by Sarah Sajewski             #
#  May 31, 2015                        #                                                           
########################################

#TOC:
#{1}READ IN DATA
#{2}PRINCPLE COMP 

#{1}READ IN DATA
setwd("/Users/sarahsajewski/Dropbox/Harris/BigData2/Final Project/Openpayments data/")
library(data.table)
library(gamlr)
openpay <- fread("/Users/sarahsajewski/Dropbox/Harris/BigData2/Final Project/Openpayments data/12192014_ALLDTL/OPPR_ALL_DTL_GNRL_12192014.csv")
openpay <- as.data.frame(openpay)
set.seed(06132015)
openpay <- openpay[sample(1:nrow(openpay), 1e5, replace=FALSE),]
source("OP.Factors.R")
source("deviance.R")

#{2}PRINCIPLE COMP
openpaypr <- sparse.model.matrix(~ formpay+natpay
                                    +ptype+pspec)
oppc <- prcomp(openpaypr, scale=FALSE)
plot(oppc)
summary(oppc)

oppcpred <- predict(oppc)
plot(oppcpred)
z<-oppcpred[,1:30] # here I arbitrarily picks a level that gives us 36 levels of variance. 
z<-as.data.frame(z)
oppcpred<-as.data.frame(oppcpred)
kfits <- lapply(1:30, 
                function(K) glm(lpayam~., data=oppcpred[,1:K,drop=FALSE]))
#this 1:36 makes kfits 3.4G. Not going to try to expand this. 
aicc <- sapply(kfits, AICc) # apply AICc to each fit
which.min(aicc) 
## you could also use BIC
bic <- sapply(kfits, BIC) 
which.min(bic) 
#both do 29. so stop at 29. 

z<-z[,1:29]
oppcgam <- gamlr(z,lpayam)
plot(oppcgam)
oppcgampred <- predict(oppcgam, z)
plot(oppcgampred,lpayam)

#compare with simple gamlr
opgam <- gamlr(openpaypr, lpayam)
plot(opgam)
opgampred <- predict(opgam, openpaypr)
plot(opgampred, lpayam)

#compare deviances
oppcdev <- deviance(lpayam,oppcgampred)
opgamdev <- deviance(lpayam,opgampred)
oppcdev
opgamdev
#gamlr does better. kinda odd? 
