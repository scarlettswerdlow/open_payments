########################################                                                                            #
#  Predictive Modeling-Take 2          #
#  Coded by Sarah Sajewski             #
#  May 25, 2015                        #                                                           
########################################

#TOC:
#{1}READ IN DATA
#{2}LPAYAM MODELS
#{3}ONE PERCENT MODEL
#{4}PREDICTION

#{1}READ IN DATA
#Note: This hasn't been adjusted to work with the github repo. 
setwd("/Users/sarahsajewski/Dropbox/Harris/BigData2/Final Project/Openpayments data/")
library(data.table)
library(gamlr)
openpay <- fread("/Users/sarahsajewski/Dropbox/Harris/BigData2/Final Project/Openpayments data/12192014_ALLDTL/OPPR_ALL_DTL_GNRL_12192014.csv")
openpay <- as.data.frame(openpay)
set.seed(06132015)
openpay <- openpay[sample(1:nrow(openpay), 1e6, replace=FALSE),]
source("OP.Factors.R")
#source("naref.R")

openpayfact <- sparse.model.matrix(~ charity+dispute+formpay+manu+mcountry+mstate+natpay
                                   +pown+proindic+pspec+ptype+rcountry+rpost+rprovince+rstate
                                   +rtype+rzip+suff+tcountry+thirdparty+thirdpartycover+tstate
                                  )
#For some reason fname messes this up and reduces the nrow by 6. 

#{2}LPAYAM MODELS
#General Lpayam model
paymod <- gamlr(openpayfact, lpayam)
paymodd <-paymod$deviance[which.min(AICc(paymod))]
plot(paymod, main="paymod")
dev.copy(png,'paymod.png')
dev.off()
#lpayampred <- predict(paymod, openpayfact)
#plot(lpayam,lpayampred)
#can talk about prediciting stuff here. 

#Payment info model
openpayform <- sparse.model.matrix(~formpay+natpay)
paymodspec <- gamlr(openpayform,lpayam)
paymodspecd <- paymodspec$deviance[which.min(AICc(paymodspec))]
plot(paymodspec, main="paymodspec")
dev.copy(png,'paymodspec.png')
dev.off()

#Location model
openpayloc <- sparse.model.matrix(~rcountry+rstate+rzip)
paymodloc <- gamlr(openpayloc, lpayam)
paymodlocd <-paymodloc$deviance[which.min(AICc(paymodloc))]
plot(paymodloc, main="paymodloc")
dev.copy(png,'paymodloc.png')
dev.off()

#Physician model
openpayphys <- sparse.model.matrix(~pspec+ptype)
paymodphys <- gamlr(openpayphys,lpayam)
paymodphysd <-paymodphys$deviance[which.min(AICc(paymodphys))]
plot(paymodphys, main="paymodphys")
dev.copy(png,'paymodphys.png')
dev.off()

pmcoef <- coef(paymod)[-1,]
pmcoef.sig <- pmcoef[pmcoef!=0]
pmcoef.pos <- pmcoef[pmcoef>0]
pmcoef.neg <- pmcoef[pmcoef<0]
names(pmcoef.sig)
names(pmcoef.pos)
names(pmcoef.neg)
pmcoef.pos[order(pmcoef.pos,decreasing=TRUE)[1:10]]
pmcoef.neg[order(pmcoef.neg)[1:10]]

#get coefficents for variables. 
# coef.charity <- pmcoef[((as.numeric(grepl("charity*", rownames(pmcoef))))==1),]
# coef.formpay <- pmcoef[((as.numeric(grepl("formpay*", rownames(pmcoef))))==1),]
# coef.natpay <-  pmcoef[((as.numeric(grepl("natpay*", rownames(pmcoef))))==1),]
# coef.numpay <- pmcoef[((as.numeric(grepl("numpay*", rownames(pmcoef))))==1),]
# coef.manu <- pmcoef[((as.numeric(grepl("manu*", rownames(pmcoef))))==1),]
# coef.mcountry <- pmcoef[((as.numeric(grepl("mcountry*", rownames(pmcoef))))==1),]
# coef.pown <- pmcoef[((as.numeric(grepl("pown*", rownames(pmcoef))))==1),]
# coef.proindic <- pmcoef[((as.numeric(grepl("proindic*", rownames(pmcoef))))==1),]
# coef.pspec <- pmcoef[((as.numeric(grepl("pspec*", rownames(pmcoef))))==1),]
# coef.rstate <- pmcoef[((as.numeric(grepl("rstate*", rownames(pmcoef))))==1),]
# coef.rtype <- pmcoef[((as.numeric(grepl("rtype*", rownames(pmcoef))))==1),]
# coef.rcountry <- pmcoef[((as.numeric(grepl("rcounty*", rownames(pmcoef))))==1),]
# coef.rzip <- pmcoef[((as.numeric(grepl("rzip*", rownames(pmcoef))))==1),]
# coef.thirdparty <- pmcoef[((as.numeric(grepl("thirdparty*", rownames(pmcoef))))==1),]
# coef.suff <- pmcoef[((as.numeric(grepl("suff*", rownames(pmcoef))))==1),]

#sample
#openpay <- openpay[sample(1:nrow(openpay), 1e5, replace=FALSE),]
#source("OP.Factors.R")

#{3}ONE PERCENT MODEL
# oneper <- factor(payam >=quantile(payam, c(.99)))
# summary(oneper)
# 
# #limited the model to get it to run.
# #General One Per Model
# paymodoneper <- gamlr(openpayfact, oneper, family="binomial")
# plot(paymodoneper, main="paymodoneper")
# dev.copy(png,'oneper.png')
# dev.off()
# paymodoneperd <-paymodoneper$deviance[which.min(AICc(paymodoneper))]

#{4}PREDICTION 
# oneperpred <- predict(paymodoneper,openpayoneper)
# plot(oneper,oneperpred)
# dev.copy(png,'oneperpred.png')
# dev.off()

paymodspecpred <-predict(paymodspec, openpayform)
plot(lpayam, paymodspecpred)
dev.copy(png,'paymodspecpred.png')
dev.off()

paymodlocpred <-predict(paymodloc, openpayloc)
plot(lpayam, paymodlocpred)
dev.copy(png,'paymodlocpred.png')
dev.off()

paymodpred <- predict(paymod, openpayfact)
plot(lpayam,paymodpred)
dev.copy(png,'paymodpred.png')
dev.off()

#{5}COMPONENT ANALYSIS
#work in progress
openpaypr <- sparse.model.matrix(~ formpay+natpay
                                  +ptype)

oppc <- prcomp(openpaypr, scale=TRUE)
plot(oppc)

