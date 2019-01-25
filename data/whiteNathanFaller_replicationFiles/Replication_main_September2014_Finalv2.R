### Ariel R White, Noah L. Nathan, Julie K. Faller
### This version: September 2014
### What Do I Need to Vote? Replication Code

### Note: Throughout the code state names have been replaced by dummies to protect the anonymity of local bureaucrats. This data is available on request from arwhite@fas.harvard.edu ###

### R 2.15.3 
### arm (Version 1.6-05, built: 2013-3-8)
## MatchIt (Version 2.4-20, built: 2011-10-24)

library(arm)
library(memisc)
library(apsrtable)
library(xtable)
library(texreg)
library(erer)
library(MatchIt)

########################################################################
### Table 2: Summary Statistics, All States
########################################################################
rm(list=ls())
load("voterIDexp_data_sept2014.Rdata")
                                        #data <- newdata
data <- data.table(data)
data <- data[data$code!=41&data$code!=12,]
data <- data[data$week==1,]

	summary <- data.frame()
	summary[1,1] <- nrow(data[data$first_text==1&data$first_name_latino==1,])
	summary[1,2] <- nrow(data[data$first_text==1&data$first_name_latino==0,])
	summary[1,3] <- nrow(data[data$first_text==0&data$first_name_latino==1,])
	summary[1,4] <- nrow(data[data$first_text==0&data$first_name_latino==0,])	
	colnames(summary) <- c("Latino Voter ID", "Non-Latino Voter ID", "Latino Control", "Non Latino Control")
	summary[2,1] <- nrow(data[is.na(data$response)==TRUE & (data$first_text==1&data$first_name_latino==1),])
	summary[2,2] <- nrow(data[is.na(data$response)==TRUE & (data$first_text==1&data$first_name_latino==0),])
	summary[2,3] <- nrow(data[is.na(data$response)==TRUE & (data$first_text==0&data$first_name_latino==1),])
	summary[2,4] <- nrow(data[is.na(data$response)==TRUE & (data$first_text==0&data$first_name_latino==0),])
	
	summary[3,1] <- nrow(data[is.na(data$response)==FALSE & data$response==1 & (data$first_text==1&data$first_name_latino==1),])
	 d1 <- data[is.na(data$response)==FALSE & data$response==1 & (data$first_text==1&data$first_name_latino==1),]
	summary[3,2] <- nrow(data[is.na(data$response)==FALSE & data$response==1 & (data$first_text==1&data$first_name_latino==0),])
	d2 <- data[is.na(data$response)==FALSE & data$response==1 & (data$first_text==1&data$first_name_latino==0),]
	summary[3,3] <- nrow(data[is.na(data$response)==FALSE & data$response==1 & (data$first_text==0&data$first_name_latino==1),])
	d3 <- data[is.na(data$response)==FALSE & data$response==1 & (data$first_text==0&data$first_name_latino==1),]
	summary[3,4] <- nrow(data[is.na(data$response)==FALSE & data$response==1 & (data$first_text==0&data$first_name_latino==0),])
	d4 <- data[is.na(data$response)==FALSE & data$response==1 & (data$first_text==0&data$first_name_latino==0),]
	
	summary[4,1] <- round(100*(summary[3,1] / (summary[1,1] - summary[2,1])), digits=1)
	summary[4,2] <- round(100*(summary[3,2] / (summary[1,2] - summary[2,2])), digits=1)
	summary[4,3] <- round(100*(summary[3,3] / (summary[1,3] - summary[2,3])), digits=1)
	summary[4,4] <- round(100*(summary[3,4] / (summary[1,4] - summary[2,4])), digits=1)

	summary[5,1] <- nrow(data[is.na(data$response)==FALSE & data$accuracy=="Absolutely accurate" & (data$first_text==1&data$first_name_latino==1),])
	summary[5,2] <- nrow(data[is.na(data$response)==FALSE & data$accuracy=="Absolutely accurate" & (data$first_text==1&data$first_name_latino==0),])
	summary[5,3] <- nrow(data[is.na(data$response)==FALSE & data$accuracy=="Absolutely accurate" & (data$first_text==0&data$first_name_latino==1),])
	summary[5,4] <- nrow(data[is.na(data$response)==FALSE & data$accuracy=="Absolutely accurate" & (data$first_text==0&data$first_name_latino==0),])

	summary[6,1] <- round(100*(summary[5,1] / summary[3,1]), digits=1)
	summary[6,2] <- round(100*(summary[5,2] / summary[3,2]), digits=1)
	summary[6,3] <- round(100*(summary[5,3] / summary[3,3]), digits=1)
	summary[6,4] <- round(100*(summary[5,4] / summary[3,4]), digits=1)


	summary[7,1] <- nrow(data[is.na(data$response)==FALSE & (data$accuracy=="General, but accurate"|data$accuracy=="Narrow, but accurate") & (data$first_text==1&data$first_name_latino==1),])
	summary[7,2] <- nrow(data[is.na(data$response)==FALSE & (data$accuracy=="General, but accurate"|data$accuracy=="Narrow, but accurate") & (data$first_text==1&data$first_name_latino==0),])
	summary[7,3] <- "--"
	summary[7,4] <- "--"

	summary[8,1] <- round(100*(summary[7,1] / summary[3,1]), digits=1)
	summary[8,2] <- round(100*(summary[7,2] / summary[3,2]), digits=1)
	summary[8,3] <- "--"
	summary[8,4] <- "--"

	
	summary[9,1] <- nrow(data[is.na(data$response)==FALSE & data$accuracy=="Non-informative response" & (data$first_text==1&data$first_name_latino==1),])
	summary[9,2] <- nrow(data[is.na(data$response)==FALSE & data$accuracy=="Non-informative response" & (data$first_text==1&data$first_name_latino==0),])
	summary[9,3] <- "--"
	summary[9,4] <- "--"

	summary[10,1] <- round(100*(summary[9,1] / summary[3,1]), digits=1)
	summary[10,2] <- round(100*(summary[9,2] / summary[3,2]), digits=1)
	summary[10,3] <- "--"
	summary[10,4] <- "--"

	summary[11,1] <- nrow(data[is.na(data$response)==FALSE & data$accuracy=="Contains inaccurate information" & (data$first_text==1&data$first_name_latino==1),])
	summary[11,2] <- nrow(data[is.na(data$response)==FALSE & data$accuracy=="Contains inaccurate information" & (data$first_text==1&data$first_name_latino==0),])
	summary[11,3] <- "--"
	summary[11,4] <- "--"

	summary[12,1] <- round(100*(summary[11,1] / summary[3,1]), digits=1)
	summary[12,2] <- round(100*(summary[11,2] / summary[3,2]), digits=1)
	summary[12,3] <- "--"
	summary[12,4] <- "--"

	summary[13,1] <- nrow(data[is.na(data$response)==FALSE & data$accuracy=="Ambiguous" & (data$first_text==1&data$first_name_latino==1),])
	summary[13,2] <- nrow(data[is.na(data$response)==FALSE & data$accuracy=="Ambiguous" & (data$first_text==1&data$first_name_latino==0),])
	summary[13,3] <- "--"
	summary[13,4] <- "--"

	summary[14,1] <- round(100*(summary[13,1] / summary[3,1]), digits=1)
	summary[14,2] <- round(100*(summary[13,2] / summary[3,2]), digits=1)
	summary[14,3] <- "--"
	summary[14,4] <- "--"

	summary[15,1] <- ifelse(is.na(table(d1$friendly)[2])==TRUE, 0, table(d1$friendly)[2])
	summary[15,2] <- ifelse(is.na(table(d2$friendly)[2])==TRUE, 0, table(d2$friendly)[2])
	summary[15,3] <- "--"
	summary[15,4] <- "--"

	summary[16,1] <- round(100*(summary[15,1] / summary[3,1]), digits=1)
	summary[16,2] <- round(100*(summary[15,2] / summary[3,2]), digits=1)
	summary[16,3] <- "--"
	summary[16,4] <- "--"


	summary[1,5] <- nrow(data[data$first_name_latino==1,])	
	summary[1,6] <- nrow(data[data$first_name_latino==0,])	

	summary[2,5] <- nrow(data[is.na(data$response)==TRUE & (data$first_name_latino==1),])
	summary[2,6] <- nrow(data[is.na(data$response)==TRUE & (data$first_name_latino==0),])
	summary[3,5] <- nrow(data[is.na(data$response)==FALSE & data$response==1 & (data$first_name_latino==1),])
	d5 <- data[is.na(data$response)==FALSE & data$response==1 & (data$first_name_latino==1),]
	summary[3,6] <- nrow(data[is.na(data$response)==FALSE & data$response==1 & (data$first_name_latino==0),])
	d6 <- data[is.na(data$response)==FALSE & data$response==1 & (data$first_name_latino==0),]

	summary[4,5] <- round(100*(summary[3,5] / (summary[1,5] - summary[2,5])), digits=1)
	summary[4,6] <- round(100*(summary[3,6] / (summary[1,6] - summary[2,6])), digits=1)
	summary[5,5] <- nrow(data[is.na(data$response)==FALSE & data$accuracy=="Absolutely accurate" & (data$first_name_latino==1),])
	summary[5,6] <- nrow(data[is.na(data$response)==FALSE & data$accuracy=="Absolutely accurate" & (data$first_name_latino==0),])
	summary[6,5] <- round(100*(summary[5,5] / summary[3,5]), digits=1)
	summary[6,6] <- round(100*(summary[5,6] / summary[3,6]), digits=1)
	summary[c(7:16),c(5:6)] <- "--"
	
	colnames(summary) <- c("Latino Voter ID", "Non-Latino Voter ID", "Latino Control", "Non Latino Control", "Latino--All", "Non Latino--All")

d <- c("Emails sent", "Incorrect address", "No. responses", 
"Response (%)",  "Accurate, abs.", "Accurate, abs. (%)",  "Accurate, gen. or narrow", "Accurate, gen. or narrow (%)", "Non-informative", "Non-informative (%)", "Inaccurate", "Inaccurate (%)", "Ambiguous", "Ambiguous (%)", "Friendly", "Friendly (%)")
rownames(summary) <- d
xtable(summary)

summary2 <-  as.data.frame(matrix(NA, nrow=10 , ncol=6))
summary2[1,]<- 46
summary2[2,]<- summary[1,] #sent
summary2[3,]<- summary[2,] #bounces
summary2[4, ] <- paste(summary[3,], " (", summary[4,], "%)", sep="") #response rate
summary2[5, ] <- paste(summary[5,], " (", summary[6,], "%)", sep="") #abs. acc
summary2[6, ] <- paste(summary[7,], " (", summary[8,], "%)", sep="") #acc. gen/narr
summary2[7, ] <- paste(summary[9,], " (", summary[10,], "%)", sep="") #noninf
summary2[8, ] <- paste(summary[11,], " (", summary[12,], "%)", sep="") #inacc
summary2[9, ] <- paste(summary[13,], " (", summary[14,], "%)", sep="") #ambig
summary2[10, ] <- paste(summary[15,], " (", summary[16,], "%)", sep="") #friendly
d2 <- c("States", "Emails sent", "Incorrect address / bounce ", "Response rate", 
"Accurate, absolutely",  "Accurate, general or narrow", "Non-informative", "Inaccurate", "Ambiguous", "Friendly")
rownames(summary2) <- d2
xtable(summary2)

summary2



########################################################################
### Response Rate Results cited in Text
###  & Figure 1: First Differences in Overall Response Rates
########################################################################
rm(list=ls())
load("voterIDexp_data_sept2014.Rdata")
data <- data.table(data)
data$abs_acc<-0
data[data$accuracy=="Absolutely accurate",]$abs_acc<-1
data[is.na(data$response)==TRUE|data$response==0,]$abs_acc<-NA

data$non_info<-0
data[data$accuracy=="Non-informative response",]$non_info<-1
data[is.na(data$response)==TRUE|data$response==0,]$non_info<-NA

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41 & week1$code!=12,]
week1$code.fac <- as.factor(as.character(week1$code))
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]

week1$code.fac <- relevel(week1$code.fac, 25)
week1id$code.fac <- relevel(week1id$code.fac, 25)

## Response, both texts
w1resp_pooleda<-glm(response ~ first_name_latino + first_text+per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile  +dummy1 +dummy2 +dummy3 +dummy4 +dummy5 +dummy6+dummy7 +dummy8 +dummy9 +dummy10 +dummy11 +dummy13 +dummy14 +dummy15 +dummy16 +dummy17 +dummy18 +dummy19 +dummy20 +dummy21 +dummy22 +dummy23 +dummy24+dummy26 +dummy27 +dummy28 +dummy29 +dummy30 +dummy31 +dummy32 +dummy33 +dummy34 +dummy35 +dummy36 +dummy37 +dummy38 +dummy39 +dummy40+dummy42 +dummy43 +dummy44 +dummy45 +dummy46 +dummy47 +dummy48, data=week1, family="binomial")

## Response, both texts
w1resp_partpooleda<- glmer(response ~ first_name_latino +first_text+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1, family=binomial)

## Response, distinguish texts
w1resp_inter_partpooled<- glmer(response ~ first_name_latino +first_text +first_name_latino:first_text+  per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino +first_name_latino:first_text|code.fac), data=week1, family=binomial)

## Response, distinguish texts
w1resp_inter_pooled<-glm(response ~ first_name_latino+first_text +first_name_latino:first_text+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile+dummy1 +dummy2 +dummy3 +dummy4 +dummy5 +dummy6+dummy7 +dummy8 +dummy9 +dummy10 +dummy11 +dummy13 +dummy14 +dummy15 +dummy16 +dummy17 +dummy18 +dummy19 +dummy20 +dummy21 +dummy22 +dummy23 +dummy24+dummy26 +dummy27 +dummy28 +dummy29 +dummy30 +dummy31 +dummy32 +dummy33 +dummy34 +dummy35 +dummy36 +dummy37 +dummy38 +dummy39 +dummy40+dummy42 +dummy43 +dummy44 +dummy45 +dummy46 +dummy47 +dummy48, data=week1, family="binomial")


## Simulated First Differences
model<-w1resp_pooleda
outcomes<-matrix(data=NA, ncol=2,nrow=3)
set.seed(02138)
N <- 1000
s <- sim(w1resp_pooleda, N)
f <- slot(s, "coef")
betas <- f
dnew <- week1[is.na(week1$income_est)==F,] ##[week1id$code==states[j],]
xt.m4a <- cbind(1, 1, dnew$first_text,dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile,dnew$dummy1,dnew$dummy2,dnew$dummy3,dnew$dummy4,dnew$dummy5 ,dnew$dummy6,dnew$dummy7,dnew$dummy8,dnew$dummy9,dnew$dummy10,dnew$dummy11,dnew$dummy13,dnew$dummy14,dnew$dummy15,dnew$dummy16,dnew$dummy17,dnew$dummy18,dnew$dummy19,dnew$dummy20 ,dnew$dummy21,dnew$dummy22,dnew$dummy23,dnew$dummy24,dnew$dummy26,dnew$dummy27,dnew$dummy28,dnew$dummy29,dnew$dummy30,dnew$dummy31,dnew$dummy32,dnew$dummy33,dnew$dummy34,dnew$dummy35 ,dnew$dummy36,dnew$dummy37,dnew$dummy38,dnew$dummy39,dnew$dummy40,dnew$dummy42,dnew$dummy43,dnew$dummy44,dnew$dummy45,dnew$dummy46,dnew$dummy47,dnew$dummy48)
xc.m4a <- cbind(1, 0, dnew$first_text,dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile,dnew$dummy1,dnew$dummy2,dnew$dummy3,dnew$dummy4,dnew$dummy5 ,dnew$dummy6,dnew$dummy7,dnew$dummy8,dnew$dummy9,dnew$dummy10,dnew$dummy11,dnew$dummy13,dnew$dummy14,dnew$dummy15,dnew$dummy16,dnew$dummy17,dnew$dummy18,dnew$dummy19,dnew$dummy20 ,dnew$dummy21,dnew$dummy22,dnew$dummy23,dnew$dummy24,dnew$dummy26,dnew$dummy27,dnew$dummy28,dnew$dummy29,dnew$dummy30,dnew$dummy31,dnew$dummy32,dnew$dummy33,dnew$dummy34,dnew$dummy35 ,dnew$dummy36,dnew$dummy37,dnew$dummy38,dnew$dummy39,dnew$dummy40,dnew$dummy42,dnew$dummy43,dnew$dummy44,dnew$dummy45,dnew$dummy46,dnew$dummy47,dnew$dummy48)

tprob<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector) 
tprobA <- apply(tprob,1,mean)
cprob<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
cprobA <- apply(cprob,1,mean)
diffprob<-tprobA-cprobA
outcomes[1,1]<- mean(diffprob)
outcomes[2,1]<- quantile(diffprob,.025)
outcomes[3,1]<- quantile(diffprob,.975)
outcomes

##### Partially pooled ####
###both emails

set.seed(02138)
	states <- unique(week1$code)
	N <- 1000
	s <- sim(w1resp_partpooleda, N)
	r <- slot(s, "ranef")
	f <- slot(s, "fixef")
	tprob.all <- matrix(NA, nrow=N, ncol=1)
	cprob.all <- matrix(NA, nrow=N, ncol=1)
	for(i in 1:length(states)){
	betas <- f 	
	betas[,"(Intercept)"] <- f[,"(Intercept)"] + r$code.fac[,toString(states[i]),"(Intercept)"]
	betas[,"first_name_latino"] <- f[,"first_name_latino"] + r$code.fac[,toString(states[i]),"first_name_latino"]
	dnew <- week1[is.na(week1$income_est)==F & week1$code==states[i],] 
	xt.m4a <- cbind(1, 1, dnew$first_text,dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile)
	xc.m4a <- cbind(1, 0, dnew$first_text,dnew$per_notlat, dnew$normal_vote, dnew$per_black, 	dnew$income_est, dnew$populationdensitypersqmile)
	tprob<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
	tprob.all <- cbind(tprob.all,tprob)
	cprob<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
	cprob.all <- cbind(cprob.all, cprob)
	}
tprob.all <- tprob.all[,-1]
cprob.all <- cprob.all[,-1]
tprobA <- apply(tprob.all,1,mean)	
cprobA <- apply(cprob.all,1,mean)	

diffprob<-tprobA-cprobA
outcomes[1,2]<- mean(diffprob)
outcomes[2,2]<- quantile(diffprob, 0.025)
outcomes[3,2]<- quantile(diffprob, 0.975)
outcomes

ruler<-seq(1,5,1)


##########################################
############ FD FIGURE: PANEL B #########
##########################################
outcomes2<-matrix(data=NA,nrow=3,ncol=4)

#### POOOLED
model<-w1resp_inter_pooled
set.seed(02138)
	N <- 1000
	s <- sim(w1resp_inter_pooled, N)
	f <- slot(s, "coef")
	#predictions.m4 <- matrix(NA, nrow=N, ncol=6)
	dnew <- week1[is.na(week1$income_est)==F,] ##[week1id$code==states[j],]
	betas <- f
	xt.m4a <- cbind(1, 1, 1,dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile,dnew$dummy1,dnew$dummy2,dnew$dummy3,dnew$dummy4,dnew$dummy5 ,dnew$dummy6,dnew$dummy7,dnew$dummy8,dnew$dummy9,dnew$dummy10,dnew$dummy11,dnew$dummy13,dnew$dummy14,dnew$dummy15,dnew$dummy16,dnew$dummy17,dnew$dummy18,dnew$dummy19,dnew$dummy20 ,dnew$dummy21,dnew$dummy22,dnew$dummy23,dnew$dummy24,dnew$dummy26,dnew$dummy27,dnew$dummy28,dnew$dummy29,dnew$dummy30,dnew$dummy31,dnew$dummy32,dnew$dummy33,dnew$dummy34,dnew$dummy35 ,dnew$dummy36,dnew$dummy37,dnew$dummy38,dnew$dummy39,dnew$dummy40,dnew$dummy42,dnew$dummy43,dnew$dummy44,dnew$dummy45,dnew$dummy46,dnew$dummy47,dnew$dummy48,1)
	xc.m4a <- cbind(1, 0, 1,dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile,dnew$dummy1,dnew$dummy2,dnew$dummy3,dnew$dummy4,dnew$dummy5 ,dnew$dummy6,dnew$dummy7,dnew$dummy8,dnew$dummy9,dnew$dummy10,dnew$dummy11,dnew$dummy13,dnew$dummy14,dnew$dummy15,dnew$dummy16,dnew$dummy17,dnew$dummy18,dnew$dummy19,dnew$dummy20 ,dnew$dummy21,dnew$dummy22,dnew$dummy23,dnew$dummy24,dnew$dummy26,dnew$dummy27,dnew$dummy28,dnew$dummy29,dnew$dummy30,dnew$dummy31,dnew$dummy32,dnew$dummy33,dnew$dummy34,dnew$dummy35 ,dnew$dummy36,dnew$dummy37,dnew$dummy38,dnew$dummy39,dnew$dummy40,dnew$dummy42,dnew$dummy43,dnew$dummy44,dnew$dummy45,dnew$dummy46,dnew$dummy47,dnew$dummy48, 0)
	xt.m4b <- cbind(1, 1, 0, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile,dnew$dummy1,dnew$dummy2,dnew$dummy3,dnew$dummy4,dnew$dummy5 ,dnew$dummy6,dnew$dummy7,dnew$dummy8,dnew$dummy9,dnew$dummy10,dnew$dummy11,dnew$dummy13,dnew$dummy14,dnew$dummy15,dnew$dummy16,dnew$dummy17,dnew$dummy18,dnew$dummy19,dnew$dummy20 ,dnew$dummy21,dnew$dummy22,dnew$dummy23,dnew$dummy24,dnew$dummy26,dnew$dummy27,dnew$dummy28,dnew$dummy29,dnew$dummy30,dnew$dummy31,dnew$dummy32,dnew$dummy33,dnew$dummy34,dnew$dummy35 ,dnew$dummy36,dnew$dummy37,dnew$dummy38,dnew$dummy39,dnew$dummy40,dnew$dummy42,dnew$dummy43,dnew$dummy44,dnew$dummy45,dnew$dummy46,dnew$dummy47,dnew$dummy48,0)
	xc.m4b<- cbind(1, 0, 0,dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile,dnew$dummy1,dnew$dummy2,dnew$dummy3,dnew$dummy4,dnew$dummy5 ,dnew$dummy6,dnew$dummy7,dnew$dummy8,dnew$dummy9,dnew$dummy10,dnew$dummy11,dnew$dummy13,dnew$dummy14,dnew$dummy15,dnew$dummy16,dnew$dummy17,dnew$dummy18,dnew$dummy19,dnew$dummy20 ,dnew$dummy21,dnew$dummy22,dnew$dummy23,dnew$dummy24,dnew$dummy26,dnew$dummy27,dnew$dummy28,dnew$dummy29,dnew$dummy30,dnew$dummy31,dnew$dummy32,dnew$dummy33,dnew$dummy34,dnew$dummy35 ,dnew$dummy36,dnew$dummy37,dnew$dummy38,dnew$dummy39,dnew$dummy40,dnew$dummy42,dnew$dummy43,dnew$dummy44,dnew$dummy45,dnew$dummy46,dnew$dummy47,dnew$dummy48,0)
	betas <- f # + r3[,1]

tprob1<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
tprobA1 <- apply(tprob1,1,mean)
cprob1<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
cprobA1 <- apply(cprob1,1,mean)
diffprob1<-tprobA1-cprobA1

tprob2<-apply((1/(1+exp(-(as.matrix(xt.m4b))%*% t(betas)))),1,as.vector)
tprobA2 <- apply(tprob2,1,mean)
cprob2<-apply((1/(1+exp(-(as.matrix(xc.m4b))%*% t(betas)))),1,as.vector)
cprobA2 <- apply(cprob2,1,mean)
diffprob2<-tprobA2-cprobA2

outcomes2[1,1]<- mean(diffprob1)
outcomes2[2,1]<- quantile(diffprob1, .025)
outcomes2[3,1]<- quantile(diffprob1, .975)
outcomes2[1,2]<- mean(diffprob2)
outcomes2[2,2]<- quantile(diffprob2,.025)
outcomes2[3,2]<- quantile(diffprob2,.975)

outcomes2

######### PARTIALLY POOLED
set.seed(02138)
states <- unique(week1$code)
N <- 1000
s <- sim(w1resp_inter_partpooled, N)
r <- slot(s, "ranef")
f <- slot(s, "fixef")
tprob.all1 <- matrix(NA, nrow=N, ncol=1)
cprob.all1 <- matrix(NA, nrow=N, ncol=1)
tprob.all2 <- matrix(NA, nrow=N, ncol=1)
cprob.all2 <- matrix(NA, nrow=N, ncol=1)
	for(i in 1:length(states)){
	betas <- f 	
	betas[,"(Intercept)"] <- f[,"(Intercept)"] + r$code.fac[,toString(states[i]),"(Intercept)"]
	betas[,"first_name_latino"] <- f[,"first_name_latino"] + r$code.fac[,toString(states[i]),"first_name_latino"]
	betas[,"first_name_latino:first_text"] <- f[,"first_name_latino:first_text"] + r$code.fac[,toString(states[i]),"first_name_latino:first_text"]
	dnew <- week1[is.na(week1$income_est)==F & week1$code==states[i],] 
	xt.m4a <- cbind(1, 1, 1,dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile,1)
	xc.m4a <- cbind(1, 0, 1,dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	xt.m4b <- cbind(1, 1, 0, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile ,0)
	xc.m4b<- cbind(1, 0, 0,dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile,0)
tprob1<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
cprob1<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
tprob.all1 <- cbind(tprob.all1, tprob1)
cprob.all1 <- cbind(cprob.all1, cprob1)
tprob2<-apply((1/(1+exp(-(as.matrix(xt.m4b))%*% t(betas)))),1,as.vector)
cprob2<-apply((1/(1+exp(-(as.matrix(xc.m4b))%*% t(betas)))),1,as.vector)
tprob.all2 <- cbind(tprob.all2, tprob2)
cprob.all2 <- cbind(cprob.all2, cprob2)
	}
tprob.all1 <- tprob.all1[,-1]
cprob.all1 <- cprob.all1[,-1]
tprob.all2 <- tprob.all2[,-1]
cprob.all2 <- cprob.all2[,-1]
tprobA1 <- apply(tprob.all1,1,mean)
cprobA1 <- apply(cprob.all1,1,mean)
tprobA2 <- apply(tprob.all2,1,mean)
cprobA2 <- apply(cprob.all2,1,mean)

diffprob1<-tprobA1-cprobA1
diffprob2<-tprobA2-cprobA2

outcomes2[1,3]<- mean(diffprob1)
outcomes2[2,3]<- quantile(diffprob1, .025)
outcomes2[3,3]<- quantile(diffprob1, .975)
outcomes2[1,4]<- mean(diffprob2)
outcomes2[2,4]<- quantile(diffprob2,.025)
outcomes2[3,4]<- quantile(diffprob2,.975)

#Panel A (texts combined)
outcomes
#Panel B (texts distinguished)
outcomes2


### plot
ruler<-seq(1,5,1)

pdf(file="response1fd_rep.pdf", height=7, width=9)
par(mfrow=c(1,2))
plot(outcomes[1,1],ruler[1],ylim=c(0,6), xlim=c(-.08,.01),yaxt='n',pch=19, main="A: First Differences Both Texts", xlab="FD in response to Latino v. Non-Latino",ylab="Model Type")
segments(outcomes[2,1], ruler[1], outcomes[3,1],ruler[1],lwd=4)
points(outcomes[1,2],ruler[4],pch=19)
segments(outcomes[2,2], ruler[4], outcomes[3,2],ruler[4],lwd=4)
abline(v=0, col="darkgray", lty="dotted", lwd=4)
axis(2, at=c(1,4),labels=c("Fully Pooled","Partially Pooled"))

plot(outcomes2[1,2],ruler[1],ylim=c(0,6), xlim=c(-.1,.02),yaxt='n',pch=21, main="B: First Differences by Email Type", xlab="FD in response to Latino v. Non-Latino",ylab="Model Type",col=" darkgray",bg="darkgray")
segments(outcomes2[2,2], ruler[1], outcomes2[3,2],ruler[1], col="darkgray",lwd=4)

points(outcomes2[1,1],ruler[2],pch=21,col="black",bg="black")
segments(outcomes2[2,1], ruler[2], outcomes2[3,1],ruler[2],lty="solid",col="black",lwd=4)

points(outcomes2[1,4],ruler[4],pch=21,col="darkgray", bg="darkgray")
segments(outcomes2[2,4], ruler[4], outcomes2[3,4],ruler[4], col="darkgray",lwd=4)

points(outcomes2[1,3],ruler[5],pch=21,col="black",bg="black")
segments(outcomes2[2,3], ruler[5], outcomes2[3,3],ruler[5],lty="solid",col="black",lwd=4)
abline(v=0, col="darkgray", lty="dotted", lwd=4)
legend("topleft",legend=c("Voter ID email","Control email"),pch=c(21,21),col=c("black","darkgray"),lwd=c(3,3),pt.bg=c("black","darkgray"))
axis(2, at=c(1.5,4.5),labels=c("Fully Pooled","Partially Pooled"))
dev.off()


########################################################################
### Characteristics of Responses                                   ###
########################################################################
rm(list=ls())
load("voterIDexp_data_sept2014.Rdata")

library(arm)
library(memisc)
library(apsrtable)
library(xtable)
library(texreg)

data <- data[data$code!=41&data$code!=12&data$code!=2,]

# gen outcome vars
data$abs_acc<-0
data[data$accuracy=="Absolutely accurate",]$abs_acc<-1
data[is.na(data$response)==TRUE|data$response==0,]$abs_acc<-NA

data$non_info<-0
data[data$accuracy=="Non-informative response",]$non_info<-1
data[is.na(data$response)==TRUE|data$response==0,]$non_info<-NA

data[is.na(data$response)==TRUE|data$response==0,]$timely_1<- NA

#set up week 1 data
week1 <- data[data$week==1,]
week1$code.fac <- as.factor(week1$code)
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]

week1$code.fac <- relevel(week1$code.fac, 25)
week1id$code.fac <- relevel(week1id$code.fac, 25)
week1pr$code.fac <- relevel(week1pr$code.fac, 25)

## set up storage
outcomes <- c("NA", "abs_acc", "non_info", "friendly", "wordcount", "gradelevel")
labels <- c("NA", "Absolutely Accurate Response", "Non-informative Response", "Friendly Tone", "Word Count", "Grade Level")
plotvalues <- cbind(outcomes, labels, NA, NA, NA, NA, NA, NA)
colnames(plotvalues) <- c("outcomevar", "label", "MLMest", "MLMlower", "MLMupper", "poolest", "poollower", "poolupper")

## MLM's first
for (i in 2:4){
	outcome <- paste(unlist(plotvalues[i,1]))
	keep <- c(outcome, "first_name_latino", "per_notlat", "normal_vote", "per_black", "income_est", "populationdensitypersqmile", "code.fac")
	dat <- week1id[is.na(week1id$income_est)==F, keep]; colnames(dat)[1]<- "outcome"
	model <- glmer(outcome ~ first_name_latino +per_notlat + normal_vote + per_black +income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=dat, family=binomial)
	modelname <- paste("mlm_", outcome, sep="")
	assign(modelname, model)
	states <- unique(dat$code)
	N <- 1000
	s <- sim(model, N)
	r <- slot(s, "ranef")
	f <- slot(s, "fixef")
	tprob.all <- c()
	cprob.all <- c()
	for(j in 1:length(states)){
		betas <- f 	
		betas[,"(Intercept)"] <- f[,"(Intercept)"] + r$code.fac[,toString(states[j]),"(Intercept)"]
		betas[,"first_name_latino"] <- f[,"first_name_latino"] + r$code.fac[,toString(states[j]),"first_name_latino"]
		dnew <- dat[is.na(dat$income_est)==F & dat$code==states[j],] 
		xt.m4a <- cbind(1, 1,dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile)
		xc.m4a <- cbind(1, 0,dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile)
		tprob<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
		tprob.all <- cbind(tprob.all,tprob)
		cprob<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
		cprob.all <- cbind(cprob.all, cprob)
		}
	tprob.all <- tprob.all[,-1]
	cprob.all <- cprob.all[,-1]
	tprobA <- apply(tprob.all,1,mean)	
	cprobA <- apply(cprob.all,1,mean)
	diffprob<-tprobA-cprobA
	saver <- i
	plotvalues[i,3] <- mean(diffprob)
	plotvalues[i,4] <-quantile(diffprob,.025)
	plotvalues[i,5] <-quantile(diffprob,.975)
}

for (i in 5:6){
	outcome <- paste(unlist(plotvalues[i,1]))
	keep <- c(outcome, "first_name_latino", "per_notlat", "normal_vote", "per_black", "income_est", "populationdensitypersqmile", "code.fac")
	dat <- week1id[is.na(week1id$income_est)==F, keep]; colnames(dat)[1]<- "outcome"
	model <- glmer(outcome ~ first_name_latino + per_notlat + normal_vote + per_black +income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=dat)
	modelname <- paste("mlm_", outcome, sep="")
	assign(modelname, model)
	set.seed(02138)
	N <- 1000
	s <- sim(model, n.sims=N)
	r <- slot(s, "ranef")
	f <- slot(s, "fixef")
	tprob.all <- c()
	cprob.all <- c()
	for(j in 1:length(states)){
		betas <- f 	
		betas[,"(Intercept)"] <- f[,"(Intercept)"] + r$code.fac[,toString(states[j]),"(Intercept)"]
		betas[,"first_name_latino"] <- f[,"first_name_latino"] + r$code.fac[,toString(states[j]),"first_name_latino"]
		dnew <- dat[is.na(dat$income_est)==F & dat$code==states[j],] 
		xt.m4a <- cbind(1, 1,dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile)
		xc.m4a <- cbind(1, 0,dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile)
		tprob<-apply(((as.matrix(xt.m4a)%*% t(betas))),1,as.vector)
		tprob.all <- cbind(tprob.all,tprob)
		cprob<-apply(((as.matrix(xc.m4a)%*% t(betas))),1,as.vector)
		cprob.all <- cbind(cprob.all, cprob)
	}
	tprob.all <- tprob.all[,-1]
	cprob.all <- cprob.all[,-1]
	tprobA <- apply(tprob.all,1,mean)	
	cprobA <- apply(cprob.all,1,mean)
	diffprob<-tprobA-cprobA
	plotvalues[i,3] <- mean(diffprob)
	plotvalues[i,4] <-quantile(diffprob,.025)
	plotvalues[i,5] <-quantile(diffprob,.975)
}

plotvalues


## Fully-pooled models

mean(na.omit(week1$non_info));min(na.omit(week1$non_info)); max(na.omit(week1$non_info))
sum(is.na(week1$non_info)); sum(is.na(week1$timely_1))
sum(is.na(week1$friendly))
dim(week1)

##
set.seed(02138)
for (i in 2:4){
	outcome <- paste(unlist(plotvalues[i,1]))
	loopd <- week1id; dim(loopd); dim(week1id)
	states1 <- unique(week1id $code)
	keep <- c(outcome, "first_name_latino", "per_notlat", "normal_vote", "per_black", "income_est", "populationdensitypersqmile", paste("dummy", states1, sep=""))
	dat <- loopd[is.na(week1id$income_est)==F, keep]; colnames(dat)[1]<- "outcome"
	dnew <- dat 
	model <-glm(outcome ~ first_name_latino +per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile  +dummy1 +dummy3 +dummy4 +dummy5 +dummy6+dummy7 +dummy8 +dummy9 +dummy10 +dummy11 +dummy13 +dummy14 +dummy15 +dummy16 +dummy17 +dummy18 +dummy19 +dummy20 +dummy21 +dummy22 +dummy23 +dummy24+dummy26 +dummy27 +dummy28 +dummy29 +dummy30 +dummy31 +dummy32 +dummy33 +dummy34 +dummy35 +dummy36 +dummy37 +dummy38 +dummy39 +dummy40+dummy42 +dummy43 +dummy44 +dummy45 +dummy46 +dummy47 +dummy48, data=dat, family="binomial") 
	modelname <- paste("pooled_", outcome, sep="")
	assign(modelname, model)
	N <- 1000
	s <- sim(model, N)
	f <- slot(s, "coef")
	betas <- f
	xt.m4a <- cbind(1, 1, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile,dnew$dummy1,dnew$dummy3,dnew$dummy4,dnew$dummy5 ,dnew$dummy6,dnew$dummy7,dnew$dummy8,dnew$dummy9,dnew$dummy10,dnew$dummy11,dnew$dummy13,dnew$dummy14,dnew$dummy15,dnew$dummy16,dnew$dummy17,dnew$dummy18,dnew$dummy19,dnew$dummy20 ,dnew$dummy21,dnew$dummy22,dnew$dummy23,dnew$dummy24,dnew$dummy26,dnew$dummy27,dnew$dummy28,dnew$dummy29,dnew$dummy30,dnew$dummy31,dnew$dummy32,dnew$dummy33,dnew$dummy34,dnew$dummy35 ,dnew$dummy36,dnew$dummy37,dnew$dummy38,dnew$dummy39,dnew$dummy40,dnew$dummy42,dnew$dummy43,dnew$dummy44,dnew$dummy45,dnew$dummy46,dnew$dummy47,dnew$dummy48)
	xc.m4a <- cbind(1, 0, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile,dnew$dummy1,dnew$dummy3,dnew$dummy4,dnew$dummy5 ,dnew$dummy6,dnew$dummy7,dnew$dummy8,dnew$dummy9,dnew$dummy10,dnew$dummy11,dnew$dummy13,dnew$dummy14,dnew$dummy15,dnew$dummy16,dnew$dummy17,dnew$dummy18,dnew$dummy19,dnew$dummy20 ,dnew$dummy21,dnew$dummy22,dnew$dummy23,dnew$dummy24,dnew$dummy26,dnew$dummy27,dnew$dummy28,dnew$dummy29,dnew$dummy30,dnew$dummy31,dnew$dummy32,dnew$dummy33,dnew$dummy34,dnew$dummy35 ,dnew$dummy36,dnew$dummy37,dnew$dummy38,dnew$dummy39,dnew$dummy40,dnew$dummy42,dnew$dummy43,dnew$dummy44,dnew$dummy45,dnew$dummy46,dnew$dummy47,dnew$dummy48)	
	betas <- f 
	
	tprob<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
	tprobA <- apply(tprob,1,mean)
	cprob<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
	cprobA <- apply(cprob,1,mean)
	diffprob<-tprobA-cprobA

	plotvalues[i,6] <- mean(diffprob)
	plotvalues[i,7] <-quantile(diffprob,.025)
	plotvalues[i,8] <-quantile(diffprob,.975)

}

for (i in 5:6){
	outcome <- paste(unlist(plotvalues[i,1]))
	loopd <- week1id; dim(loopd); dim(week1id)
	states1 <- unique(week1id$code)
	keep <- c(outcome, "first_name_latino", "per_notlat", "normal_vote", "per_black", "income_est", "populationdensitypersqmile", paste("dummy", states1, sep=""))
	dat <- loopd[is.na(week1id$income_est)==F, keep]; colnames(dat)[1]<- "outcome"
	dnew <- dat 
	model <-glm(outcome ~ first_name_latino +per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile  +dummy1 +dummy3 +dummy4 +dummy5 +dummy6+dummy7 +dummy8 +dummy9 +dummy10 +dummy11 +dummy13 +dummy14 +dummy15 +dummy16 +dummy17 +dummy18 +dummy19 +dummy20 +dummy21 +dummy22 +dummy23 +dummy24+dummy26 +dummy27 +dummy28 +dummy29 +dummy30 +dummy31 +dummy32 +dummy33 +dummy34 +dummy35 +dummy36 +dummy37 +dummy38 +dummy39 +dummy40+dummy42 +dummy43 +dummy44 +dummy45 +dummy46 +dummy47 +dummy48, data=dat) 
	modelname <- paste("pooled_", outcome, sep="")
	assign(modelname, model)
	N <- 1000
	s <- sim(model, N)
	f <- slot(s, "coef")
	betas <- f
	xt.m4a <- cbind(1, 1, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile,dnew$dummy1,dnew$dummy3,dnew$dummy4,dnew$dummy5 ,dnew$dummy6,dnew$dummy7,dnew$dummy8,dnew$dummy9,dnew$dummy10,dnew$dummy11,dnew$dummy13,dnew$dummy14,dnew$dummy15,dnew$dummy16,dnew$dummy17,dnew$dummy18,dnew$dummy19,dnew$dummy20 ,dnew$dummy21,dnew$dummy22,dnew$dummy23,dnew$dummy24,dnew$dummy26,dnew$dummy27,dnew$dummy28,dnew$dummy29,dnew$dummy30,dnew$dummy31,dnew$dummy32,dnew$dummy33,dnew$dummy34,dnew$dummy35 ,dnew$dummy36,dnew$dummy37,dnew$dummy38,dnew$dummy39,dnew$dummy40,dnew$dummy42,dnew$dummy43,dnew$dummy44,dnew$dummy45,dnew$dummy46,dnew$dummy47,dnew$dummy48)
	xc.m4a <- cbind(1, 0, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile,dnew$dummy1,dnew$dummy3,dnew$dummy4,dnew$dummy5 ,dnew$dummy6,dnew$dummy7,dnew$dummy8,dnew$dummy9,dnew$dummy10,dnew$dummy11,dnew$dummy13,dnew$dummy14,dnew$dummy15,dnew$dummy16,dnew$dummy17,dnew$dummy18,dnew$dummy19,dnew$dummy20 ,dnew$dummy21,dnew$dummy22,dnew$dummy23,dnew$dummy24,dnew$dummy26,dnew$dummy27,dnew$dummy28,dnew$dummy29,dnew$dummy30,dnew$dummy31,dnew$dummy32,dnew$dummy33,dnew$dummy34,dnew$dummy35 ,dnew$dummy36,dnew$dummy37,dnew$dummy38,dnew$dummy39,dnew$dummy40,dnew$dummy42,dnew$dummy43,dnew$dummy44,dnew$dummy45,dnew$dummy46,dnew$dummy47,dnew$dummy48)	
	betas <- f 
	
	tprob<-apply((as.matrix(xt.m4a)%*% t(betas)),1,as.vector)
	tprobA <- apply(tprob,1,mean)
	cprob<-apply((as.matrix(xc.m4a)%*% t(betas)),1,as.vector)
	cprobA <- apply(cprob,1,mean)
	diffprob<-tprobA-cprobA

	plotvalues[i,6] <- mean(diffprob)
	plotvalues[i,7] <-quantile(diffprob,.025)
	plotvalues[i,8] <-quantile(diffprob,.975)

}
plotvalues




#######################################################################
### Figure 2: First Differences in Overall Response Rates by VRA Status########################################################################

### NB: Before running this analysis we fix several coding errors, including the inadvertent omission of a state and several localities which were covered by the VRA at the time of our experiment ###

rm(list=ls())
load("voterIDexp_data_sept2014.Rdata")

data$abs_acc<-0
data[data$accuracy=="Absolutely accurate",]$abs_acc<-1
data[is.na(data$response)==TRUE|data$response==0,]$abs_acc<-NA

data$acc_comb<-0
data[data$accuracy=="Absolutely accurate"|data$accuracy=="General, but accurate"|data$accuracy=="Narrow, but accurate",]$acc_comb<-1
data[is.na(data$response)==TRUE|data$response==0,]$acc_comb<-NA

data$amb<-0
data[data$accuracy=="Ambiguous",]$amb<-1
data[is.na(data$response)==TRUE|data$response==0,]$abs_acc<-NA

data$non_info<-0
data[data$accuracy=="Non-informative response",]$non_info<-1
data[is.na(data$response)==TRUE|data$response==0,]$non_info<-NA

#correct omitted state error:
data$preclear_states[data$code==29] <- 1

data$vra_comb<-0
data$vra_comb[(data$preclear_states==1|data$spanishcov==1)&is.na(data$preclear_states==1)==FALSE &is.na(data$spanishcov)==FALSE ] <- 1

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41&week1$code!=12,]
week1 <- week1[is.na(week1$response)==FALSE,]
week1 <- week1[week1$income_est!=-Inf,]
week1$vra_all<-0
week1$vra_all[(week1$preclear_states==1|week1$spanishcov==1)&is.na(week1 $preclear_states==1)==FALSE &is.na(week1 $spanishcov)==FALSE ]<-1
week1$code.fac <- as.factor(week1$code)
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]

#fix coding error for spanish COV
week1$spanishcov[is.na(week1$spanishcov)==TRUE] <- 0
week1$spanishcov[week1$asgweek_id==1631 |week1$asgweek_id==1633] <- 1
week1$vra_all[week1$asgweek_id==1631 |week1$asgweek_id==1633] <- 1
#fix coding error for preclear states (jurisdictions "bailed out" between experiment and write up):
week1$preclear_states[week1$asgweek_id==7201|week1$asgweek_id==7202|week1$asgweek_id==7145|week1$asgweek_id==7146|week1$asgweek_id==7005|week1$asgweek_id==7005|week1$asgweek_id==6971|week1$asgweek_id==6972|week1$asgweek_id==6983|week1$asgweek_id==6984|week1$asgweek_id==7003|week1$asgweek_id==7004|week1$asgweek_id==7059|week1$asgweek_id==7060|week1$asgweek_id==6931|week1$asgweek_id==6932] <- 1
week1$vra_all[week1$asgweek_id==7201|week1$asgweek_id==7202|week1$asgweek_id==7145|week1$asgweek_id==7146|week1$asgweek_id==7005|week1$asgweek_id==7005|week1$asgweek_id==6971|week1$asgweek_id==6972|week1$asgweek_id==6983|week1$asgweek_id==6984|week1$asgweek_id==7003|week1$asgweek_id==7004|week1$asgweek_id==7059|week1$asgweek_id==7060|week1$asgweek_id==6931|week1$asgweek_id==6932] <- 1

week1$preclear_states[week1$VRA_extra==1] <- 1
week1$vra_all[week1$VRA_extra==1] <- 1

week1$code.fac <- relevel(week1$code.fac, 25)
week1id$code.fac <- relevel(week1id$code.fac, 25)

table(week1$vra_all)
table(week1$preclear_states)
table(week1$spanishcov)

#### #### #### #### #### #### #### #### #### #### #### #### 
#### Results cited in text (excludes those from figure) ####
#### #### #### #### #### #### #### #### #### #### #### #### 

######### Covered (Sec 5 & 203) vs. Not  #######
#split sample into two sets.
vracov <- subset(week1, week1$vra_all==1)
vrauncov <- subset(week1, week1$vra_all==0)
vrauncov <- vrauncov[is.na(vrauncov$code.fac)==FALSE,]
vracov <- vracov[is.na(vracov $code.fac)==FALSE,]

#Covered places
un_vra_cov <- glmer(response ~ first_name_latino+first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data=vracov, family=binomial)
summary(un_vra_cov)

#Uncovered places
un_vra_noncov <- glmer(response ~ first_name_latino+first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=vrauncov, family=binomial)
summary(un_vra_noncov)

#Simulated first differences in uncovered places
outcomes<-matrix(data=NA, ncol=1,nrow=3)
set.seed(02138)
states <- unique(vrauncov$code)
states <- states[is.na(states)==FALSE]
N <- 1000
s <- sim(un_vra_noncov, N)
r <- slot(s, "ranef")
f <- slot(s, "fixef")
tprob.all <- matrix(NA, nrow=N, ncol=1)
cprob.all <- matrix(NA, nrow=N, ncol=1)
tprob.all2 <- matrix(NA, nrow=N, ncol=1)
cprob.all2 <- matrix(NA, nrow=N, ncol=1)

for(i in 1:length(states)){
	betas <- f 	
	betas[,"(Intercept)"] <- f[,"(Intercept)"] + r$code.fac[,toString(states[i]),"(Intercept)"]
	betas[,"first_name_latino"] <- f[,"first_name_latino"] + r$code.fac[,toString(states[i]),"first_name_latino"]
	dnew <- vrauncov[is.na(vrauncov$income_est)==F & vrauncov$code==states[i],] 
		xt.m4a <- cbind(1, 1,dnew$first_text, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile)
	xc.m4a <- cbind(1, 0,dnew$first_text, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile)
	tprob<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
	tprob.all <- cbind(tprob.all,tprob)
	cprob<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
	cprob.all <- cbind(cprob.all, cprob)
	}
tprob.all <- tprob.all[,-1]
cprob.all <- cprob.all[,-1]
tprobA <- apply(tprob.all,1,mean)	
cprobA <- apply(cprob.all,1,mean)	

diffprob<-tprobA-cprobA
outcomes[1,1]<- mean(diffprob)
outcomes[2,1]<- quantile(diffprob, 0.025)
outcomes[3,1]<- quantile(diffprob, 0.975)
outcomes

######### Covered (Sec 5 Only) vs. Not  #######
#split sample into two sets.
vra5cov <- subset(week1, week1$preclear_states==1)
vra5uncov <- subset(week1, week1$preclear_states==0)
vra5uncov <- vra5uncov[is.na(vra5uncov$code.fac)==FALSE,]
vra5cov <- vra5cov[is.na(vra5cov$code.fac)==FALSE,]

#Covered places
un_vra_cov <- glmer(response ~ first_name_latino+first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data=vra5cov, family=binomial)
summary(un_vra_cov)

#Uncovered places
un_vra_noncov <- glmer(response ~ first_name_latino+first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=vra5uncov, family=binomial)
summary(un_vra_noncov)

#Simulated first differences in uncovered places
outcomes<-matrix(data=NA, ncol=1,nrow=3)
set.seed(02138)
states <- unique(vra5uncov$code)
states <- states[is.na(states)==FALSE]
N <- 1000
s <- sim(un_vra_noncov, N)
r <- slot(s, "ranef")
f <- slot(s, "fixef")
tprob.all <- matrix(NA, nrow=N, ncol=1)
cprob.all <- matrix(NA, nrow=N, ncol=1)
tprob.all2 <- matrix(NA, nrow=N, ncol=1)
cprob.all2 <- matrix(NA, nrow=N, ncol=1)

for(i in 1:length(states)){
	betas <- f 	
	betas[,"(Intercept)"] <- f[,"(Intercept)"] + r$code.fac[,toString(states[i]),"(Intercept)"]
	betas[,"first_name_latino"] <- f[,"first_name_latino"] + r$code.fac[,toString(states[i]),"first_name_latino"]
	dnew <- vra5uncov[is.na(vra5uncov$income_est)==F & vra5uncov$code==states[i],] 
		xt.m4a <- cbind(1, 1,dnew$first_text, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile)
	xc.m4a <- cbind(1, 0,dnew$first_text, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile)
	tprob<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
	tprob.all <- cbind(tprob.all,tprob)
	cprob<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
	cprob.all <- cbind(cprob.all, cprob)
	}
tprob.all <- tprob.all[,-1]
cprob.all <- cprob.all[,-1]
tprobA <- apply(tprob.all,1,mean)	
cprobA <- apply(cprob.all,1,mean)	

diffprob<-tprobA-cprobA
outcomes[1,1]<- mean(diffprob)
outcomes[2,1]<- quantile(diffprob, 0.025)
outcomes[3,1]<- quantile(diffprob, 0.975)
outcomes


######### Covered (Sec 203 Only) vs. Not  #######
#split sample into two sets.
vra203cov <- subset(week1, week1$spanishcov==1)
vra203uncov <- subset(week1, week1$spanishcov==0)
vra203uncov <- vra203uncov[is.na(vra203uncov$code.fac)==FALSE,]
vra203cov <- vra203cov[is.na(vra203cov$code.fac)==FALSE,]

#Covered places
un_vra_cov <- glmer(response ~ first_name_latino+first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data=vra203cov, family=binomial)
summary(un_vra_cov)

#Uncovered places
un_vra_noncov <- glmer(response ~ first_name_latino+first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=vra203uncov, family=binomial)
summary(un_vra_noncov)

#Simulated first differences in uncovered places
outcomes<-matrix(data=NA, ncol=1,nrow=3)
set.seed(02138)
states <- unique(vra203uncov$code)
states <- states[is.na(states)==FALSE]
N <- 1000
s <- sim(un_vra_noncov, N)
r <- slot(s, "ranef")
f <- slot(s, "fixef")
tprob.all <- matrix(NA, nrow=N, ncol=1)
cprob.all <- matrix(NA, nrow=N, ncol=1)
tprob.all2 <- matrix(NA, nrow=N, ncol=1)
cprob.all2 <- matrix(NA, nrow=N, ncol=1)

for(i in 1:length(states)){
	betas <- f 	
	betas[,"(Intercept)"] <- f[,"(Intercept)"] + r$code.fac[,toString(states[i]),"(Intercept)"]
	betas[,"first_name_latino"] <- f[,"first_name_latino"] + r$code.fac[,toString(states[i]),"first_name_latino"]
	dnew <- vra203uncov[is.na(vra203uncov$income_est)==F & vra203uncov$code==states[i],] 
		xt.m4a <- cbind(1, 1,dnew$first_text, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile)
	xc.m4a <- cbind(1, 0,dnew$first_text, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile)
	tprob<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
	tprob.all <- cbind(tprob.all,tprob)
	cprob<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
	cprob.all <- cbind(cprob.all, cprob)
	}
tprob.all <- tprob.all[,-1]
cprob.all <- cprob.all[,-1]
tprobA <- apply(tprob.all,1,mean)	
cprobA <- apply(cprob.all,1,mean)	

diffprob<-tprobA-cprobA
outcomes[1,1]<- mean(diffprob)
outcomes[2,1]<- quantile(diffprob, 0.025)
outcomes[3,1]<- quantile(diffprob, 0.975)
outcomes

#### #### #### #### #### #### 
######### FIGURE 2 ##########
#### #### #### #### #### #### 

rm(list=ls())
load("voterIDexp_data_sept2014.Rdata")

data$abs_acc<-0
data[data$accuracy=="Absolutely accurate",]$abs_acc<-1
data[is.na(data$response)==TRUE|data$response==0,]$abs_acc<-NA

data$acc_comb<-0
data[data$accuracy=="Absolutely accurate"|data$accuracy=="General, but accurate"|data$accuracy=="Narrow, but accurate",]$acc_comb<-1
data[is.na(data$response)==TRUE|data$response==0,]$acc_comb<-NA

data$amb<-0
data[data$accuracy=="Ambiguous",]$amb<-1
data[is.na(data$response)==TRUE|data$response==0,]$abs_acc<-NA

data$non_info<-0
data[data$accuracy=="Non-informative response",]$non_info<-1
data[is.na(data$response)==TRUE|data$response==0,]$non_info<-NA

#correct omitted state error:
data$preclear_states[data$code==29] <- 1

data$vra_comb<-0
data$vra_comb[(data$preclear_states==1|data$spanishcov==1)&is.na(data$preclear_states==1)==FALSE &is.na(data$spanishcov)==FALSE ] <- 1

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41&week1$code!=12,]
week1 <- week1[is.na(week1$response)==FALSE,]
week1 <- week1[week1$income_est!=-Inf,]
week1$vra_all<-0
week1$vra_all[(week1$preclear_states==1|week1$spanishcov==1)&is.na(week1 $preclear_states==1)==FALSE &is.na(week1 $spanishcov)==FALSE ]<-1
week1$code.fac <- as.factor(week1$code)
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]

#fix coding error for spanish COV
week1$spanishcov[is.na(week1$spanishcov)==TRUE] <- 0
week1$spanishcov[week1$asgweek_id==1631 |week1$asgweek_id==1633] <- 1
week1$vra_all[week1$asgweek_id==1631 |week1$asgweek_id==1633] <- 1
#fix coding error for preclear states (jurisdictions "bailed out" between experiment and write up):
week1$preclear_states[week1$asgweek_id==7201|week1$asgweek_id==7202|week1$asgweek_id==7145|week1$asgweek_id==7146|week1$asgweek_id==7005|week1$asgweek_id==7005|week1$asgweek_id==6971|week1$asgweek_id==6972|week1$asgweek_id==6983|week1$asgweek_id==6984|week1$asgweek_id==7003|week1$asgweek_id==7004|week1$asgweek_id==7059|week1$asgweek_id==7060|week1$asgweek_id==6931|week1$asgweek_id==6932] <- 1
week1$vra_all[week1$asgweek_id==7201|week1$asgweek_id==7202|week1$asgweek_id==7145|week1$asgweek_id==7146|week1$asgweek_id==7005|week1$asgweek_id==7005|week1$asgweek_id==6971|week1$asgweek_id==6972|week1$asgweek_id==6983|week1$asgweek_id==6984|week1$asgweek_id==7003|week1$asgweek_id==7004|week1$asgweek_id==7059|week1$asgweek_id==7060|week1$asgweek_id==6931|week1$asgweek_id==6932] <- 1

week1$preclear_states[week1$VRA_extra==1] <- 1
week1$vra_all[week1$VRA_extra==1] <- 1

week1$code.fac <- relevel(week1$code.fac, 25)
week1id$code.fac <- relevel(week1id$code.fac, 25)



### panel (A), 1;
unmatchedVRA <- glmer(response ~ first_name_latino+ first_text + vra_all + (vra_all : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data=week1, family="binomial")
summary(unmatchedVRA)

outcomes<-matrix(data=NA, ncol=6,nrow=3)
set.seed(02138)
states <- unique(week1$code)
states <- states[is.na(states)==FALSE]
N <- 1000
s <- sim(unmatchedVRA, N)
r <- slot(s, "ranef")
f <- slot(s, "fixef")
tprob.all <- matrix(NA, nrow=N, ncol=1)
cprob.all <- matrix(NA, nrow=N, ncol=1)
tprob.all2 <- matrix(NA, nrow=N, ncol=1)
cprob.all2 <- matrix(NA, nrow=N, ncol=1)

for(i in 1:length(states)){
	betas <- f 	
	betas[,"(Intercept)"] <- f[,"(Intercept)"] + r$code.fac[,toString(states[i]),"(Intercept)"]
	betas[,"first_name_latino"] <- f[,"first_name_latino"] + r$code.fac[,toString(states[i]),"first_name_latino"]
	dnew <- week1[is.na(week1$income_est)==F & week1$code==states[i],] 
	xt.m4a <- cbind(1, 1, dnew$first_text, 1, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 1)
	xc.m4a <- cbind(1, 0, dnew$first_text, 1,  dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	tprob<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
	tprob.all <- cbind(tprob.all,tprob)
	cprob<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
	cprob.all <- cbind(cprob.all, cprob)

	xt.m4a2 <- cbind(1, 1, dnew$first_text, 0, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	xc.m4a2 <- cbind(1, 0, dnew$first_text, 0,  dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	tprob2<-apply((1/(1+exp(-(as.matrix(xt.m4a2))%*% t(betas)))),1,as.vector)
	tprob.all2 <- cbind(tprob.all2, tprob2)
	cprob2<-apply((1/(1+exp(-(as.matrix(xc.m4a2))%*% t(betas)))),1,as.vector)
	cprob.all2 <- cbind(cprob.all2, cprob2)


	}
tprob.all <- tprob.all[,-1]
cprob.all <- cprob.all[,-1]
tprobA <- apply(tprob.all,1,mean)	
cprobA <- apply(cprob.all,1,mean)	

tprob.all2 <- tprob.all2[,-1]
cprob.all2 <- cprob.all2[,-1]
tprobA2 <- apply(tprob.all2,1,mean)	
cprobA2 <- apply(cprob.all2,1,mean)	

diffprob<-tprobA-cprobA
diffprob2 <- tprobA2 - cprobA2
diffprob3 <- diffprob - diffprob2

outcomes[1,1]<- mean(diffprob3)
outcomes[2,1]<- quantile(diffprob3, 0.025)
outcomes[3,1]<- quantile(diffprob3, 0.975)
outcomes

#panel (A), matched data:
m.out <- matchit(vra_all ~ per_notlat+normal_vote+per_black+income_est+populationdensitypersqmile, data=week1[is.na(week1$income_est)==F,c("response","first_name_latino","vra_all","per_notlat","normal_vote","per_black","income_est","populationdensitypersqmile","code.fac","first_text")], method = "nearest")
summary(m.out)

m.data<-match.data(m.out)

matchedVRA <- glmer(response ~ first_name_latino+ first_text + vra_all + (vra_all : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data= m.data, family="binomial")
summary(matchedVRA)

set.seed(02138)
states <- unique(m.data$code)
states <- states[is.na(states)==FALSE]
N <- 1000
s <- sim(matchedVRA, N)
r <- slot(s, "ranef")
f <- slot(s, "fixef")
tprob.all <- matrix(NA, nrow=N, ncol=1)
cprob.all <- matrix(NA, nrow=N, ncol=1)
tprob.all2 <- matrix(NA, nrow=N, ncol=1)
cprob.all2 <- matrix(NA, nrow=N, ncol=1)

for(i in 1:length(states)){
	betas <- f 	
	betas[,"(Intercept)"] <- f[,"(Intercept)"] + r$code.fac[,toString(states[i]),"(Intercept)"]
	betas[,"first_name_latino"] <- f[,"first_name_latino"] + r$code.fac[,toString(states[i]),"first_name_latino"]
	dnew <- m.data[is.na(m.data$income_est)==F & m.data$code==states[i],] 
	xt.m4a <- cbind(1, 1, dnew$first_text, 1, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 1)
	xc.m4a <- cbind(1, 0, dnew$first_text, 1,  dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	tprob<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
	tprob.all <- cbind(tprob.all,tprob)
	cprob<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
	cprob.all <- cbind(cprob.all, cprob)

	xt.m4a2 <- cbind(1, 1, dnew$first_text, 0, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	xc.m4a2 <- cbind(1, 0, dnew$first_text, 0,  dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	tprob2<-apply((1/(1+exp(-(as.matrix(xt.m4a2))%*% t(betas)))),1,as.vector)
	tprob.all2 <- cbind(tprob.all2, tprob2)
	cprob2<-apply((1/(1+exp(-(as.matrix(xc.m4a2))%*% t(betas)))),1,as.vector)
	cprob.all2 <- cbind(cprob.all2, cprob2)


	}
tprob.all <- tprob.all[,-1]
cprob.all <- cprob.all[,-1]
tprobA <- apply(tprob.all,1,mean)	
cprobA <- apply(cprob.all,1,mean)	

tprob.all2 <- tprob.all2[,-1]
cprob.all2 <- cprob.all2[,-1]
tprobA2 <- apply(tprob.all2,1,mean)	
cprobA2 <- apply(cprob.all2,1,mean)	

diffprob<-tprobA-cprobA
diffprob2 <- tprobA2 - cprobA2
diffprob3 <- diffprob - diffprob2

outcomes[1,2]<- mean(diffprob3)
outcomes[2,2]<- quantile(diffprob3, 0.025)
outcomes[3,2]<- quantile(diffprob3, 0.975)
outcomes


###panel (B), unmatched s5 only:
unmatched5 <- glmer(response ~ first_name_latino+ first_text + preclear_states + (preclear_states : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data=week1, family="binomial")
summary(unmatched5)

set.seed(02138)
states <- unique(week1$code)
states <- states[is.na(states)==FALSE]
N <- 1000
s <- sim(unmatched5, N)
r <- slot(s, "ranef")
f <- slot(s, "fixef")
tprob.all <- matrix(NA, nrow=N, ncol=1)
cprob.all <- matrix(NA, nrow=N, ncol=1)
tprob.all2 <- matrix(NA, nrow=N, ncol=1)
cprob.all2 <- matrix(NA, nrow=N, ncol=1)

for(i in 1:length(states)){
	betas <- f 	
	betas[,"(Intercept)"] <- f[,"(Intercept)"] + r$code.fac[,toString(states[i]),"(Intercept)"]
	betas[,"first_name_latino"] <- f[,"first_name_latino"] + r$code.fac[,toString(states[i]),"first_name_latino"]
	dnew <- week1[is.na(week1$income_est)==F & week1$code==states[i],] 
	xt.m4a <- cbind(1, 1, dnew$first_text, 1, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 1)
	xc.m4a <- cbind(1, 0, dnew$first_text, 1,  dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	tprob<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
	tprob.all <- cbind(tprob.all,tprob)
	cprob<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
	cprob.all <- cbind(cprob.all, cprob)

	xt.m4a2 <- cbind(1, 1, dnew$first_text, 0, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	xc.m4a2 <- cbind(1, 0, dnew$first_text, 0,  dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	tprob2<-apply((1/(1+exp(-(as.matrix(xt.m4a2))%*% t(betas)))),1,as.vector)
	tprob.all2 <- cbind(tprob.all2, tprob2)
	cprob2<-apply((1/(1+exp(-(as.matrix(xc.m4a2))%*% t(betas)))),1,as.vector)
	cprob.all2 <- cbind(cprob.all2, cprob2)


	}
tprob.all <- tprob.all[,-1]
cprob.all <- cprob.all[,-1]
tprobA <- apply(tprob.all,1,mean)	
cprobA <- apply(cprob.all,1,mean)	

tprob.all2 <- tprob.all2[,-1]
cprob.all2 <- cprob.all2[,-1]
tprobA2 <- apply(tprob.all2,1,mean)	
cprobA2 <- apply(cprob.all2,1,mean)	

diffprob<-tprobA-cprobA
diffprob2 <- tprobA2 - cprobA2
diffprob3 <- diffprob - diffprob2

outcomes[1,3]<- mean(diffprob3)
outcomes[2,3]<- quantile(diffprob3, 0.025)
outcomes[3,3]<- quantile(diffprob3, 0.975)
outcomes


#panel (b), s5 only, matched data:

m.out <- matchit(preclear_states ~ per_notlat+normal_vote+per_black+income_est+populationdensitypersqmile, data=week1[is.na(week1$income_est)==F,c("response","first_name_latino","preclear_states","per_notlat","normal_vote","per_black","income_est","populationdensitypersqmile","code.fac","first_text")], method = "nearest")
summary(m.out)

m.data<-match.data(m.out)

matched5 <- glmer(response ~ first_name_latino+ first_text + preclear_states + (preclear_states : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data= m.data, family="binomial")
summary(matched5)

set.seed(02138)
states <- unique(m.data$code)
states <- states[is.na(states)==FALSE]
N <- 1000
s <- sim(matched5, N)
r <- slot(s, "ranef")
f <- slot(s, "fixef")
tprob.all <- matrix(NA, nrow=N, ncol=1)
cprob.all <- matrix(NA, nrow=N, ncol=1)
tprob.all2 <- matrix(NA, nrow=N, ncol=1)
cprob.all2 <- matrix(NA, nrow=N, ncol=1)

for(i in 1:length(states)){
	betas <- f 	
	betas[,"(Intercept)"] <- f[,"(Intercept)"] + r$code.fac[,toString(states[i]),"(Intercept)"]
	betas[,"first_name_latino"] <- f[,"first_name_latino"] + r$code.fac[,toString(states[i]),"first_name_latino"]
	dnew <- m.data[is.na(m.data$income_est)==F & m.data$code==states[i],] 
	xt.m4a <- cbind(1, 1, dnew$first_text, 1, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 1)
	xc.m4a <- cbind(1, 0, dnew$first_text, 1,  dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	tprob<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
	tprob.all <- cbind(tprob.all,tprob)
	cprob<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
	cprob.all <- cbind(cprob.all, cprob)

	xt.m4a2 <- cbind(1, 1, dnew$first_text, 0, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	xc.m4a2 <- cbind(1, 0, dnew$first_text, 0,  dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	tprob2<-apply((1/(1+exp(-(as.matrix(xt.m4a2))%*% t(betas)))),1,as.vector)
	tprob.all2 <- cbind(tprob.all2, tprob2)
	cprob2<-apply((1/(1+exp(-(as.matrix(xc.m4a2))%*% t(betas)))),1,as.vector)
	cprob.all2 <- cbind(cprob.all2, cprob2)


	}
tprob.all <- tprob.all[,-1]
cprob.all <- cprob.all[,-1]
tprobA <- apply(tprob.all,1,mean)	
cprobA <- apply(cprob.all,1,mean)	

tprob.all2 <- tprob.all2[,-1]
cprob.all2 <- cprob.all2[,-1]
tprobA2 <- apply(tprob.all2,1,mean)	
cprobA2 <- apply(cprob.all2,1,mean)	

diffprob<-tprobA-cprobA
diffprob2 <- tprobA2 - cprobA2
diffprob3 <- diffprob - diffprob2

outcomes[1,4]<- mean(diffprob3)
outcomes[2,4]<- quantile(diffprob3, 0.025)
outcomes[3,4]<- quantile(diffprob3, 0.975)
outcomes



###panel (C), unmatched s203 only:
unmatched203 <- glmer(response ~ first_name_latino+ first_text + spanishcov + (spanishcov : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data=week1, family="binomial")
summary(unmatched203)

set.seed(02138)
states <- unique(week1$code)
states <- states[is.na(states)==FALSE]
N <- 1000
s <- sim(unmatched203, N)
r <- slot(s, "ranef")
f <- slot(s, "fixef")
tprob.all <- matrix(NA, nrow=N, ncol=1)
cprob.all <- matrix(NA, nrow=N, ncol=1)
tprob.all2 <- matrix(NA, nrow=N, ncol=1)
cprob.all2 <- matrix(NA, nrow=N, ncol=1)

for(i in 1:length(states)){
	betas <- f 	
	betas[,"(Intercept)"] <- f[,"(Intercept)"] + r$code.fac[,toString(states[i]),"(Intercept)"]
	betas[,"first_name_latino"] <- f[,"first_name_latino"] + r$code.fac[,toString(states[i]),"first_name_latino"]
	dnew <- week1[is.na(week1$income_est)==F & week1$code==states[i],] 
	xt.m4a <- cbind(1, 1, dnew$first_text, 1, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 1)
	xc.m4a <- cbind(1, 0, dnew$first_text, 1,  dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	tprob<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
	tprob.all <- cbind(tprob.all,tprob)
	cprob<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
	cprob.all <- cbind(cprob.all, cprob)

	xt.m4a2 <- cbind(1, 1, dnew$first_text, 0, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	xc.m4a2 <- cbind(1, 0, dnew$first_text, 0,  dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	tprob2<-apply((1/(1+exp(-(as.matrix(xt.m4a2))%*% t(betas)))),1,as.vector)
	tprob.all2 <- cbind(tprob.all2, tprob2)
	cprob2<-apply((1/(1+exp(-(as.matrix(xc.m4a2))%*% t(betas)))),1,as.vector)
	cprob.all2 <- cbind(cprob.all2, cprob2)


	}
tprob.all <- tprob.all[,-1]
cprob.all <- cprob.all[,-1]
tprobA <- apply(tprob.all,1,mean)	
cprobA <- apply(cprob.all,1,mean)	

tprob.all2 <- tprob.all2[,-1]
cprob.all2 <- cprob.all2[,-1]
tprobA2 <- apply(tprob.all2,1,mean)	
cprobA2 <- apply(cprob.all2,1,mean)	

diffprob<-tprobA-cprobA
diffprob2 <- tprobA2 - cprobA2
diffprob3 <- diffprob - diffprob2

outcomes[1,5]<- mean(diffprob3)
outcomes[2,5]<- quantile(diffprob3, 0.025)
outcomes[3,5]<- quantile(diffprob3, 0.975)
outcomes


## panel (C), Section 203 matched
m.out <- matchit(spanishcov ~ per_notlat+normal_vote+per_black+income_est+populationdensitypersqmile, data=week1[is.na(week1$income_est)==F & is.na(week1$spanishcov)==F,c("response","first_name_latino","spanishcov","per_notlat","normal_vote","per_black","income_est","populationdensitypersqmile","code.fac","first_text")], method = "nearest")
summary(m.out)
#names(week1)

m.data<-match.data(m.out)

matched203 <- glmer(response ~ first_name_latino+ first_text + spanishcov + (spanishcov : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data= m.data, family="binomial")
summary(matched203)


set.seed(02138)
states <- unique(m.data$code)
states <- states[is.na(states)==FALSE]
N <- 1000
s <- sim(matched203, N)
r <- slot(s, "ranef")
f <- slot(s, "fixef")
tprob.all <- matrix(NA, nrow=N, ncol=1)
cprob.all <- matrix(NA, nrow=N, ncol=1)
tprob.all2 <- matrix(NA, nrow=N, ncol=1)
cprob.all2 <- matrix(NA, nrow=N, ncol=1)

for(i in 1:length(states)){
	betas <- f 	
	betas[,"(Intercept)"] <- f[,"(Intercept)"] + r$code.fac[,toString(states[i]),"(Intercept)"]
	betas[,"first_name_latino"] <- f[,"first_name_latino"] + r$code.fac[,toString(states[i]),"first_name_latino"]
	dnew <- m.data[is.na(m.data$income_est)==F & m.data$code==states[i],] 
	xt.m4a <- cbind(1, 1, dnew$first_text, 1, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 1)
	xc.m4a <- cbind(1, 0, dnew$first_text, 1,  dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	tprob<-apply((1/(1+exp(-(as.matrix(xt.m4a))%*% t(betas)))),1,as.vector)
	tprob.all <- cbind(tprob.all,tprob)
	cprob<-apply((1/(1+exp(-(as.matrix(xc.m4a))%*% t(betas)))),1,as.vector)
	cprob.all <- cbind(cprob.all, cprob)

	xt.m4a2 <- cbind(1, 1, dnew$first_text, 0, dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	xc.m4a2 <- cbind(1, 0, dnew$first_text, 0,  dnew$per_notlat, dnew$normal_vote, dnew$per_black, dnew$income_est, dnew$populationdensitypersqmile, 0)
	tprob2<-apply((1/(1+exp(-(as.matrix(xt.m4a2))%*% t(betas)))),1,as.vector)
	tprob.all2 <- cbind(tprob.all2, tprob2)
	cprob2<-apply((1/(1+exp(-(as.matrix(xc.m4a2))%*% t(betas)))),1,as.vector)
	cprob.all2 <- cbind(cprob.all2, cprob2)


	}
tprob.all <- tprob.all[,-1]
cprob.all <- cprob.all[,-1]
tprobA <- apply(tprob.all,1,mean)	
cprobA <- apply(cprob.all,1,mean)	

tprob.all2 <- tprob.all2[,-1]
cprob.all2 <- cprob.all2[,-1]
tprobA2 <- apply(tprob.all2,1,mean)	
cprobA2 <- apply(cprob.all2,1,mean)	

diffprob<-tprobA-cprobA
diffprob2 <- tprobA2 - cprobA2
diffprob3 <- diffprob - diffprob2

outcomes[1,6]<- mean(diffprob3)
outcomes[2,6]<- quantile(diffprob3, 0.025)
outcomes[3,6]<- quantile(diffprob3, 0.975)

rownames(outcomes)<-c("estimate","lower","upper")
colnames(outcomes)<-c("all unmatched", "all matched","5 unmatched","5 matched","203 unmatched","203 matched")
outcomes


pdf(file="newVRA_july14.pdf", height=9, width=7)
par(mfrow=c(3,1), mar=c(5, 8, 4, 2) + 0.1)

#panel A, full coverage:
plot(outcomes[1,1],1,ylim=c(0.7,1.1), xlim=c(-.1,.35),yaxt='n',pch=19, main="A: Difference in Bias by VRA Coverage", xlab="Difference in bias against Latinos in response rate, Covered v. Non-Covered",ylab=NA)
abline(v=0, col="lightgray", lty="dotted", lwd=3)
segments(outcomes[2,1], 1, outcomes[3,1],1,lwd=3)
points(outcomes[1,2], 0.8,pch=19, col="firebrick")
segments(outcomes[2,2], 0.8, outcomes[3,2], 0.8,lwd=3, col="firebrick")
axis(2, at=c(0.8, 1),labels=c("Matched data \n (N=1598)","Full data \n (N=6442)"), cex.axis=.85, las=1)

plot(outcomes[1,3],1,ylim=c(0.7,1.1), xlim=c(-.1,.35),yaxt='n',pch=19, main="B: Difference in Bias by Section 5 Coverage", xlab="Difference in bias against Latinos in response rate, Covered v. Non-Covered (Sec. 5 only)",ylab=NA)
abline(v=0, col="lightgray", lty="dotted", lwd=3)
segments(outcomes[2,3], 1, outcomes[3,3],1,lwd=3)
points(outcomes[1,4], 0.8,pch=19, col="firebrick")
segments(outcomes[2,4], 0.8, outcomes[3,4], 0.8,lwd=3, col="firebrick")
axis(2, at=c(0.8, 1),labels=c("Matched data \n (N=1412)","Full data \n (N=6442)"), cex.axis=.85, las=1)

plot(outcomes[1,5],1,ylim=c(0.7,1.1), xlim=c(-.1,.35),yaxt='n',pch=19, main="C: Difference in Bias by Section 203 Coverage", xlab="Difference in bias against Latinos in response rate, Covered v. Non-Covered (Sec. 203 only)",ylab=NA)
abline(v=0, col="lightgray", lty="dotted", lwd=3)
segments(outcomes[2,5], 1, outcomes[3,5],1,lwd=3)
points(outcomes[1,6], 0.8,pch=19, col="firebrick")
segments(outcomes[2,6], 0.8, outcomes[3,6], 0.8,lwd=3, col="firebrick")
axis(2, at=c(0.8, 1),labels=c("Matched data \n (N=372)","Full data \n (N=6442)"), cex.axis=.85, las=1)

dev.off()







