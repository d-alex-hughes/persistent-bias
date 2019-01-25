
###########################################################
########## Faller, Nathan & White #########################
########## "What Do I Need to Vote" #######################
########## Code for Supporting Information Tables #########
###########################################################

### R 2.15.3 
### arm (Version 1.6-05, built: 2013-3-8)
## MatchIt (Version 2.4-20, built: 2011-10-24)


library(arm)
library(texreg)
library(MatchIt)

##### Note: Data for our Mturk pretest, referenced in fn X, is available separately on request, email: arwhite@fas.harvard.edu ############




##########################################
#### SI2:  ROBUST TO 2ND WAVE OF EMAILS ######
##########################################


###
### wave 1 + wave 2, responsiveness and accuracy models 
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

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41&week1$code!=12,]
week1 <- week1[is.na(week1$response)==FALSE,]
#week1$code.fac <- as.factor(week1$code)
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]


data <- data[data$code!=41&data$code!=12,]
#data$code.fac <- as.factor(data$code)
week2<-data[data$week==2,]
week2 <- week2[is.na(week2$response)==FALSE,]
week2<-week2[week2$code!=47 & week2$code!=22&week2$code!=12&week2$code!=4&week2$code!=27 &week2$code!=41&week2$code!=42&week2$code!=8,]
week2id<-week2[week2$first_text==1,]

data_comp<-rbind(week1,week2)
idboth<-rbind(week1id, week2id)
data_comp$latname <- 0
data_comp$latname[data_comp$week==1&data_comp$first_name_latino==1] <- 1
data_comp$latname[data_comp$week==2&data_comp$second_name_latino==1] <- 1
data_comp$text1 <- 0
data_comp$text1[data_comp$week==1&data_comp$first_text==1] <- 1
data_comp$text1[data_comp$week==2&data_comp$second_text==1] <- 1 

idboth$latname <- 0
idboth$latname[idboth$week==1&idboth$first_name_latino==1] <- 1
idboth$latname[idboth$week==2&idboth$second_name_latino==1] <- 1
idboth$text1 <- 0
idboth$text1[idboth$week==1&idboth$first_text==1] <- 1
idboth$text1[idboth$week==2&idboth$second_text==1] <- 1 


w1resp_pooleda<-glm(response ~ latname + text1 +per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile +code.fac, data=data_comp, family="binomial")
summary(w1resp_pooleda)

w1resp_partpooleda<- glmer(response ~ latname +text1+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=data_comp, family=binomial)
summary(w1resp_partpooleda)
 
 ######################
 ### SI 3: ABS ACC ####
 #### SECOND WAVE ######
 ######################
 
w1resp_pooledb<-glm(abs_acc ~ latname + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile+code.fac, data=idboth, family="binomial")
summary(w1resp_pooledb)

set.seed(02138)
w1resp_partpooledb<- glmer(abs_acc ~ latname + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=idboth, family=binomial)
summary(w1resp_partpooledb)

texreg(list(w1resp_pooledb,w1resp_partpooledb),omit.coef="code.fac")

##########################################
#### Fig SI2 : COVARIATE BALANCE AFTER MATCHING ##
##########################################

#overall balance for the full sample:
rm(list=ls())
load("voterIDexp_data_sept2014.Rdata")

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41 & week1$code!=12,]
#week1$code.fac <- as.factor(week1$code)
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]

pdf(file="OVERALL_balance_20140423.pdf", family="Helvetica", width=8, height=12)
par(mfrow=c(5,2))
#normal vote
qqplot(week1$normal_vote[week1$first_text==1], week1$normal_vote[week1$first_text==0], plot.it=TRUE, ylab="Control Email", xlab="Voter ID Email", main="2008 Pres. Dem. Vote Share", pch=16, col="dodgerblue4", cex=.9, xlim=c(0,1), ylim=c(0,1))
abline(a=0, b=1, col="firebrick", lty="dashed", lwd=1.3)

qqplot(week1$normal_vote[week1$first_name_latino==1], week1$normal_vote[week1$first_name_latino==0], plot.it=TRUE, ylab="Non-Latino", xlab="Latino", main="2008 Pres. Dem. Vote Share (Both Emails)", pch=16, col="dodgerblue4", cex=.9, xlim=c(0,1), ylim=c(0,1))
abline(a=0, b=1, col="firebrick", lty="dashed", lwd=1.3)

#population density
qqplot(week1$populationdensitypersqmile[week1$first_text==1], week1$populationdensitypersqmile[week1$first_text==0], plot.it=TRUE, ylab="Control Email", xlab="Voter ID Email", main="Population Density (2010 Census)", pch=16, col="dodgerblue4", cex=.9)
abline(a=0, b=1, col="firebrick", lty="dashed", lwd=1.3)

qqplot(week1$populationdensitypersqmile[week1$first_name_latino==1], week1$populationdensitypersqmile[week1$first_name_latino==0], plot.it=TRUE, ylab="Non-Latino", xlab="Latino", main="Population Density (2010 Census)", pch=16, col="dodgerblue4", cex=.9)
abline(a=0, b=1, col="firebrick", lty="dashed", lwd=1.3)

#proportion black
qqplot(week1$per_black[week1$first_text==1], week1$per_black[week1$first_text==0], plot.it=TRUE, ylab="Control Email", xlab="Voter ID Email", main="Proportion Black (2010 Census)", pch=16, col="dodgerblue4", cex=.9, xlim=c(0,.5), ylim=c(0,.5))
abline(a=0, b=1, col="firebrick", lty="dashed", lwd=1.3)

qqplot(week1$per_black[week1$first_name_latino==1], week1$per_black[week1$first_name_latino==0], plot.it=TRUE, ylab="Non-Latino", xlab="Latino", main="Proportion Black (2010 Census)", pch=16, col="dodgerblue4", cex=.9, xlim=c(0,.5), ylim=c(0,.5))
abline(a=0, b=1, col="firebrick", lty="dashed", lwd=1.3)

#proportion not-latino
qqplot(week1$per_notlat[week1$first_text==1], week1$per_notlat[week1$first_text==0], plot.it=TRUE, ylab="Control Email", xlab="Voter ID Email", main="Proportion Not Latino (2010 Census)", pch=16, col="dodgerblue4", cex=.9, xlim=c(.2,1), ylim=c(.2,1))
abline(a=0, b=1, col="firebrick", lty="dashed", lwd=1.3)

qqplot(week1$per_notlat[week1$first_name_latino==1], week1$per_notlat[week1$first_name_latino==0], plot.it=TRUE, ylab="Non-Latino", xlab="Latino", main="Proportion Not Latino (2010 Census)", pch=16, col="dodgerblue4", cex=.9, xlim=c(.2,1), ylim=c(.2,1))
abline(a=0, b=1, col="firebrick", lty="dashed", lwd=1.3)

#income est
qqplot(week1$income_est[week1$first_text==1], week1$income_est[week1$first_text==0], plot.it=TRUE, ylab="Control Email", xlab="Voter ID Email", main="Income (2010 Census)", pch=16, col="dodgerblue4", cex=.9)
abline(a=0, b=1, col="firebrick", lty="dashed", lwd=1.3)

qqplot(week1$income_est[week1$first_name_latino==1], week1$income_est[week1$first_name_latino==0], plot.it=TRUE, ylab="Non-Latino", xlab="Latino", main="Income (2010 Census)", pch=16, col="dodgerblue4", cex=.9)
abline(a=0, b=1, col="firebrick", lty="dashed", lwd=1.3) 
dev.off()

#Contact the authors if you would like the code for the balance plots separately by state. We are glad to provide this separate code on request: Email: arwhite@fas.harvard.edu.


########################################################################
### Table SI.7, Response Rate by Ethnicity, Fully, Partially Pooled Models
########################################################################
rm(list=ls())
load("voterIDexp_data_sept2014.Rdata")

rm(list=ls())
load("voterIDexp_data_sept2014.Rdata")
data$abs_acc<-0
data[data$accuracy=="Absolutely accurate",]$abs_acc<-1
data[is.na(data$response)==TRUE|data$response==0,]$abs_acc<-NA

data$non_info<-0
data[data$accuracy=="Non-informative response",]$non_info<-1
data[is.na(data$response)==TRUE|data$response==0,]$non_info<-NA

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41 & week1$code!=12,]
#week1$code.fac <- as.factor(as.character(week1$code))
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]

week1$code.fac <- relevel(week1$code.fac, "25")
week1id$code.fac <- relevel(week1id$code.fac, "25")

#setting levels for replicability
week1id$code.fac<-factor(week1id$code.fac,c("25", "26", "1",  "2",  "3",  "4", "5",  "6",  "7","24",  "8",  "9",  "10", "11", "13" ,"14", "15" ,"16", "17", "18", "19", "20", "21", "22", "23","27" ,"28", "29", "31", "32", "33", "34", "35", "36", "37", "38" ,"39" ,"40" ,"42", "43", "44", "45", "46", "47", "48","30")) 
levels(week1id$code.fac)

w1resp_pooleda<-glm(response ~ first_name_latino + first_text+per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data=week1, family="binomial")
summary(w1resp_pooleda)

w1resp_partpooleda<- glmer(response ~ first_name_latino +first_text+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1, family=binomial)
summary(w1resp_partpooleda)

w1resp_pooledb<-glm(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile+code.fac, data=week1id, family="binomial")
summary(w1resp_pooledb)

w1resp_partpooledb<- glmer(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(w1resp_partpooledb)

w1resp_inter_partpooled<- glmer(response ~ first_name_latino +first_text +first_name_latino:first_text+  per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino +first_name_latino:first_text|code.fac), data=week1, family=binomial)
summary(w1resp_inter_partpooled)

w1resp_inter_pooled<-glm(response ~ first_name_latino+first_text +first_name_latino:first_text+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile+code.fac, data=week1, family="binomial")
summary(w1resp_inter_pooled)

texreg(list(w1resp_pooleda,w1resp_partpooleda,w1resp_pooledb,w1resp_partpooledb,w1resp_inter_pooled,w1resp_inter_partpooled),omit.coef="code.fac", model.names=c("Fully Pooled, Both Emails","Partially Pooled, Both Emails", "Fully Pooled, Voter ID","Partially Pooled, Voter ID","Fully Pooled,Both Emails","Partially Pooled, Both Emails"),sideways=T,)


########################################################################
### Figure SI.5: First Differences in Response Rates by State
########################################################################

## To protect bureaucrats' anonymity, the code linking states to outcomes is not included. Please email the authors if you would like access to this code for an academic project at arwhite@fas.harvard.edu. ##

###################################################
### SI 8: Tests for Hypothesis 3 #########
###################################################
rm(list=ls())
load("voterIDexp_data_sept2014.Rdata")

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41 & week1$code!=12,]
week1 <- week1[is.na(week1$response)==FALSE,]
#week1$code.fac <- as.factor(week1$code)

## strict photo enacted or enforced vs others
week1$strict_both<-0
week1[week1$strict_enforced==1|week1$strict_enacted==1,"strict_both"]<-1

#Photo (strict or not strict) enacted or enforced vs others
week1$photo_both<-0
week1[week1$strict_enforced==1|week1$strict_enacted==1|week1$photo_enforced==1|week1$photo_enacted==1,"photo_both"]<-1

## Enacted not enforced laws vs all others
week1$enacted_both<-0
week1[week1$strict_enacted==1|week1$photo_enacted==1,"enacted_both"]<-1

week1$photolaw<-0
week1[week1$strict_enforced==1|week1$photo_enforced==1,"photolaw"]<-1

week1$nonphotolaw<-0
week1[week1$strict_nonphoto==1|week1$nonphoto==1,"nonphotolaw"]<-1

week1$idlaw_overall<-0
week1[week1$strict_nonphoto==1|week1$nonphoto==1|week1$strict_enforced==1|week1$photo_enforced==1,"idlaw_overall"]<-1
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]

week1$code.fac <- relevel(week1$code.fac, "25")
week1id$code.fac <- relevel(week1id$code.fac, "25")

#setting levels for replicability
week1id$code.fac<-factor(week1id$code.fac,c("25", "26", "1",  "2",  "3",  "4", "5",  "6",  "7","24",  "8",  "9",  "10", "11", "13" ,"14", "15" ,"16", "17", "18", "19", "20", "21", "22", "23","27" ,"28", "29", "31", "32", "33", "34", "35", "36", "37", "38" ,"39" ,"40" ,"42", "43", "44", "45", "46", "47", "48","30")) 
levels(week1id$code.fac)
 
## Law vs. No Law ###
m2a <- glmer(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + idlaw_overall + idlaw_overall:first_name_latino+(1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(m2a)

m2b <- glm(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + idlaw_overall +idlaw_overall:first_name_latino +code.fac, data=week1id, family=binomial(logit))
summary(m2b)

m3a <- glmer(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile  + photolaw+photolaw:first_name_latino+ (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(m3a)

m3b <- glm(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + photolaw +photolaw:first_name_latino+ as.factor(code.fac), data=week1id, family=binomial)
summary(m3b)

m4a <- glmer(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + nonphotolaw + photolaw+photolaw:first_name_latino+nonphotolaw:first_name_latino+ (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(m4a)

m4b <- glm(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + photolaw + nonphotolaw +photolaw:first_name_latino+nonphotolaw:first_name_latino+ code.fac, data=week1id, family=binomial)
summary(m4b)


texreg(list(m2a,m2b,m3a,m3b,m4a,m4b), model.names=c("Partially Pooled","Pooled","Partially Pooled","Pooled","Partially Pooled","Pooled"),omit.coef="code.fac",sideways=T, override.pvals=pvals)


################################################
##### SI 9 REGRESSION TABLES FOR 
##### ADDITIONAL OUTCOMES  ######
################################################
rm(list=ls())
load("voterIDexp_data_sept2014.Rdata")
data$abs_acc<-0
data[data$accuracy=="Absolutely accurate",]$abs_acc<-1
data[is.na(data$response)==TRUE|data$response==0,]$abs_acc<-NA

data$non_info<-0
data[data$accuracy=="Non-informative response",]$non_info<-1
data[is.na(data$response)==TRUE|data$response==0,]$non_info<-NA

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41 & week1$code!=12 &week1$code!=2,]
#week1$code.fac <- as.factor(as.character(week1$code))
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]

week1id$code.fac<-factor(week1id$code.fac,c("25", "26", "1",  "2",  "3",  "4", "5",  "6",  "7","24",  "8",  "9",  "10", "11", "13" ,"14", "15" ,"16", "17", "18", "19", "20", "21", "22", "23","27" ,"28", "29", "31", "32", "33", "34", "35", "36", "37", "38" ,"39" ,"40" ,"42", "43", "44", "45", "46", "47", "48","30")) 
levels(week1id$code.fac)

data[is.na(data$response)==TRUE|data$response==0,]$timely_1<- NA

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
	states <- unique(dat$code)
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

####################
#### Table SI.9 ####
####################

pvals <- list(coef(summary(mlm_abs_acc))[,4], coef(summary(mlm_non_info))[,4], coef(summary(mlm_friendly))[,4]) 
texreg(list(mlm_abs_acc, mlm_non_info, mlm_friendly), omit.coef="code.fac", override.pval=pvals)



####################
#### Table SI.10 ####
####################

mean(na.omit(week1$non_info));min(na.omit(week1$non_info)); max(na.omit(week1$non_info))
sum(is.na(week1$non_info)); sum(is.na(week1$timely_1))
sum(is.na(week1$friendly))
dim(week1)

##
set.seed(02138)
for (i in 2:4){
	outcome <- paste(unlist(plotvalues[i,1]))
	loopd <- week1id; dim(loopd); dim(week1id)
	states1 <- unique(week1id$code)
	keep <- c(outcome, "first_name_latino", "per_notlat", "normal_vote", "per_black", "income_est", "populationdensitypersqmile", paste("dummy", states1, sep=""))
	dat <- loopd[is.na(week1id$income_est)==F, keep]; colnames(dat)[1]<- "outcome"
	dnew <- dat 
	model <-glm(outcome ~ first_name_latino +per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile+dummy1+dummy3+dummy4+dummy5+dummy6+dummy7+dummy8+dummy9+dummy10+dummy11+dummy13+dummy14+dummy15+dummy16+dummy17+dummy18+dummy19+dummy20+dummy21+dummy22+dummy23+dummy24+dummy26+dummy27+dummy28+dummy29+dummy30+dummy31+dummy32+dummy33+dummy34+dummy35+dummy36+dummy37+dummy38+dummy39+dummy40+dummy42+dummy43+dummy44+dummy45+dummy46+dummy47+dummy48, data=dat, family="binomial") 
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
	model <-glm(outcome ~ first_name_latino +per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile+dummy1+dummy3+dummy4+dummy5+dummy6+dummy7+dummy8+dummy9+dummy10+dummy11+dummy13+dummy14+dummy15+dummy16+dummy17+dummy18+dummy19+dummy20+dummy21+dummy22+dummy23+dummy24+dummy26+dummy27+dummy28+dummy29+dummy30+dummy31+dummy32+dummy33+dummy34+dummy35+dummy36+dummy37+dummy38+dummy39+dummy40+dummy42+dummy43+dummy44+dummy45+dummy46+dummy47+dummy48, data=dat) 
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

##now, table of fully pooled models for appendix:
texreg(list(pooled_abs_acc, pooled_non_info, pooled_friendly),omit.coef="dummy")


#######################################################################
### SI 11: Overall Response Rates by VRA Status
########################################################################
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
#week1$code.fac <- as.factor(week1$code)
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

week1$code.fac <- relevel(week1$code.fac, "25")
week1id$code.fac <- relevel(week1id$code.fac, "25")

table(week1$vra_all)
table(week1$preclear_states)
table(week1$spanishcov)

###################################################################
####### Regression Tables underlying Figure 2 (Section 4.3 Bias Under Monitoring)
###################################################################
unmatchedVRA <- glmer(response ~ first_name_latino+ first_text + vra_all + (vra_all : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data=week1, family="binomial")
summary(unmatchedVRA)
#N=6442

table(week1$vra_all)
m.out <- matchit(vra_all ~ per_notlat+normal_vote+per_black+income_est+populationdensitypersqmile, data=week1[is.na(week1$income_est)==F,c("response","first_name_latino","vra_all","per_notlat","normal_vote","per_black","income_est","populationdensitypersqmile","code.fac","first_text")], method = "nearest")
summary(m.out)

m.data<-match.data(m.out)

matchedVRA <- glmer(response ~ first_name_latino+ first_text + vra_all + (vra_all : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data= m.data, family="binomial")
summary(matchedVRA)


unmatched5 <- glmer(response ~ first_name_latino+ first_text + preclear_states + (preclear_states : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data=week1, family="binomial")
summary(unmatched5)
#N=6442

m.out <- matchit(preclear_states ~ per_notlat+normal_vote+per_black+income_est+populationdensitypersqmile, data=week1[is.na(week1$income_est)==F,c("response","first_name_latino","preclear_states","per_notlat","normal_vote","per_black","income_est","populationdensitypersqmile","code.fac","first_text")], method = "nearest")
summary(m.out)

m.data<-match.data(m.out)

matched5 <- glmer(response ~ first_name_latino+ first_text + preclear_states + (preclear_states : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data= m.data, family="binomial")
summary(matched5)


unmatched203 <- glmer(response ~ first_name_latino+ first_text + spanishcov + (spanishcov : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino| code.fac), data=week1, family="binomial")
summary(unmatched203)

m.out <- matchit(spanishcov ~ per_notlat+normal_vote+per_black+income_est+populationdensitypersqmile, data=week1[is.na(week1$income_est)==F & is.na(week1$spanishcov)==F,c("response","first_name_latino","spanishcov","per_notlat","normal_vote","per_black","income_est","populationdensitypersqmile","code.fac","first_text")], method = "nearest")
summary(m.out)


m.data<-match.data(m.out)

matched203 <- glmer(response ~ first_name_latino+ first_text + spanishcov + (spanishcov : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= m.data, family="binomial")
summary(matched203)

pvals<-list(coef(summary(unmatchedVRA))[,4],coef(summary(matchedVRA))[,4],coef(summary(unmatched5))[,4],coef(summary(matched5))[,4],coef(summary(unmatched203))[,4],coef(summary(matched203))[,4])

texreg(list(unmatchedVRA,matchedVRA,unmatched5,matched5,unmatched203,matched203), omit.coef="code.fac", model.names=c("Unmatched VRA (5 & 203)", "Matched VRA (5 & 203)", "Unmatched Section 5","Matched Section 5", "Unmatched Section 203","Matched Section 203"),sideways=T, override.pval=pvals)


###################################################################
####### Table SI 12: VRA results, additional matching variables
###################################################################
########################################################################
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
week1$totalpop_latino <- log(week1$totalpop_latino)
week1$totalpop_latino <- ifelse(week1$totalpop_latino=="-Inf", NA, data$totalpop_latino)
week1 <- week1[is.na(week1$totalpop_latino)==FALSE,]
week1$vra_all<-0
week1$vra_all[(week1$preclear_states==1|week1$spanishcov==1)&is.na(week1 $preclear_states==1)==FALSE &is.na(week1 $spanishcov)==FALSE ]<-1
#week1$code.fac <- as.factor(week1$code)
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

#QUESTION: again, is missingness a problem?
m.out <- matchit(vra_all ~ per_notlat+normal_vote+per_black+income_est+populationdensitypersqmile+ totalpop_latino, data=week1[is.na(week1$income_est)==F,c("response","first_name_latino","vra_all","per_notlat","normal_vote","per_black","income_est","populationdensitypersqmile","code.fac","first_text", "totalpop_latino")], method = "nearest")
summary(m.out)

m.data<-match.data(m.out)

matchedVRA <- glmer(response ~ first_name_latino+ first_text + vra_all + (vra_all : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + totalpop_latino + (1 + first_name_latino|code.fac), data= m.data, family="binomial")
summary(matchedVRA)
#N=1466

m.out <- matchit(preclear_states ~ per_notlat+normal_vote+per_black+income_est+populationdensitypersqmile+ totalpop_latino, data=week1[is.na(week1$income_est)==F,c("response","first_name_latino","preclear_states","per_notlat","normal_vote","per_black","income_est","populationdensitypersqmile","code.fac","first_text", "totalpop_latino")], method = "nearest")
summary(m.out)

m.data<-match.data(m.out)

matched5 <- glmer(response ~ first_name_latino+ first_text + preclear_states + (preclear_states : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + totalpop_latino + (1 + first_name_latino|code.fac), data= m.data, family="binomial")
summary(matched5) #QUESTION: is it okay there are missing values?

m.out <- matchit(spanishcov ~ per_notlat+normal_vote+per_black+income_est+populationdensitypersqmile + totalpop_latino, data=week1[is.na(week1$income_est)==F & is.na(week1$spanishcov)==F,c("response","first_name_latino","spanishcov","per_notlat","normal_vote","per_black","income_est","populationdensitypersqmile","code.fac","first_text", "totalpop_latino")], method = "nearest")
summary(m.out)


m.data<-match.data(m.out)

matched203 <- glmer(response ~ first_name_latino+ first_text + spanishcov + (spanishcov : first_name_latino) + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + totalpop_latino +(1 + first_name_latino|code.fac), data= m.data, family="binomial")
summary(matched203)

pvals<-list(coef(summary(matchedVRA))[,4],coef(summary(matched5))[,4],coef(summary(matched203))[,4])

texreg(list(matchedVRA,matched5,matched203), omit.coef="code.fac", model.names=c("Matched VRA (5 & 203)", "Matched Section 5", "Matched Section 203"),sideways=T, override.pval=pvals)




########################################################################
### SI 13 : Regression Tables for Partisanship of Officials
########################################################################

rm(list=ls())
load("voterIDexp_data_wparty_sept2014.Rdata")
data <- d3
head(data)
data$abs_acc<-0
data[data$accuracy=="Absolutely accurate",]$abs_acc<-1
data[is.na(data$response)==TRUE|data$response==0,]$abs_acc<-NA

data$non_info<-0
data[data$accuracy=="Non-informative response",]$non_info<-1
data[is.na(data$response)==TRUE|data$response==0,]$non_info<-NA

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41 & week1$code!=12,]
#week1$code.fac <- as.factor(week1$code)
week1id <- week1[week1$first_tt==1,]
week1pr <- week1[week1$first_tt==0,]

	#make new variable here
table(week1$elected_update)
week1$elec_new[week1$elected_update=="-1"|week1$elected_update=="0"] <- 0
week1$elec_new[week1$elected_update=="1"] <- 1
table(week1$elec_new)


week1id$elec_new[week1id$elected_update=="-1"|week1id$elected_update=="0"] <- 0
week1id$elec_new[week1id$elected_update=="1"] <- 1
table(week1id$elec_new)

week1pr$elec_new[week1pr$elected_update=="-1"|week1pr$elected_update=="0"] <- 0
week1pr$elec_new[week1pr$elected_update=="1"] <- 1
table(week1pr$elec_new)

table(week1$partisan_update)
table(week1$partisan_update, week1$party_update)


#Fixing partisan coding
week1$partisan_update[week1$partisan_update==1&week1$party_update==5] <- 0
week1$partisan_update[week1$partisan_update==1&week1$party_update==4] <- 0
week1$partisan_update[week1$partisan_update==0&week1$party_update==1] <- 1
week1$partisan_update[week1$partisan_update==0&week1$party_update==2] <- 1
table(week1$partisan_update, week1$party_update)
table(week1$partisan_update)

week1id$partisan_update[week1id$partisan_update==1&week1id$party_update==5] <- 0
week1id$partisan_update[week1id$partisan_update==1&week1id$party_update==4] <- 0
week1id$partisan_update[week1id$partisan_update==0&week1id$party_update==1] <- 1
week1id$partisan_update[week1id$partisan_update==0&week1id$party_update==2] <- 1

week1pr$partisan_update[week1pr$partisan_update==1&week1pr$party_update==5] <- 0
week1pr$partisan_update[week1pr$partisan_update==1&week1pr$party_update==4] <- 0
week1pr$partisan_update[week1pr$partisan_update==0&week1pr$party_update==1] <- 1
week1pr$partisan_update[week1pr$partisan_update==0&week1pr$party_update==2] <- 1


#####summary stats on partisan and elected (quoted in footnote in paper)
dim(week1)
table(week1$partisan_update)
table(week1$partisan_update) / sum(table(week1$partisan_update))
table(week1$elec_new)
table(week1$elec_new) / sum(table(week1$elec_new))
table(week1$partisan_update, week1$elec_new) / sum(table(week1$partisan_update, week1$elec_new))


m1<- glmer(response ~ first_name_latino + elec_new  + first_tt+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1, family=binomial)
summary(m1)

m1a<- glmer(response ~ first_name_latino +  (first_name_latino: elec_new ) + elec_new  + first_tt+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1, family=binomial)
summary(m1a)

a1<- glmer(abs_acc ~ first_name_latino + elec_new +per_notlat + normal_vote + per_black +income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1id, family=binomial)
summary(a1)

a1a <- glmer(abs_acc ~ first_name_latino + (first_name_latino: elec_new) + elec_new + per_notlat + normal_vote + per_black +income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1id, family=binomial)
summary(a1a)

m2 <- glmer(response ~ first_name_latino + partisan_update  + first_tt+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1, family=binomial)
summary(m2)

m2a <- glmer(response ~ first_name_latino +  (first_name_latino: partisan_update ) + partisan_update  + first_tt+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1, family=binomial)
summary(m2a)

a2 <- glmer(abs_acc ~ first_name_latino + partisan_update +per_notlat + normal_vote + per_black +income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1id, family=binomial)
summary(a2)

a2a <- glmer(abs_acc ~ first_name_latino + (first_name_latino: partisan_update  ) + partisan_update + per_notlat + normal_vote + per_black +income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1id, family=binomial)
summary(a2a)

pvals<-list(coef(summary(m1))[,4],coef(summary(m1a))[,4],coef(summary(m2))[,4],coef(summary(m2a))[,4],coef(summary(a1))[,4],coef(summary(a1a))[,4],coef(summary(a2))[,4],coef(summary(a2a))[,4])

texreg(list(m1,m1a,m2,m2a,a1,a1a, a2, a2a), omit.coef="code.fac", model.names=c("Response","Response","Response", "Response" ,"Abs.~Accuracy", "Abs.~Accuracy", "Abs.~Accuracy", "Abs.~Accuracy"),sideways=T, override.pval=pvals)


########################################################################
### SI 14 : Regression Tables for Party of Officials
########################################################################
##NB: Use same data as for SI 13

week1_part <- week1[is.na(week1$party_update) == FALSE & (week1$party_update==1 |week1$party_update==2) ,]
week1_part$republican[week1_part$party_update==1] <- 1
week1_part$republican[week1_part$party_update==2] <- 0
week1_part$code.fac <- as.factor(week1_part$code)
week1id_part <- week1_part[week1_part $first_tt==1,]
week1pr_part <- week1_part[week1_part $first_tt==0,]

m1 <- glmer(response ~ first_name_latino + republican  + first_tt+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1_part, family=binomial)
summary(m1)

m1a <- glmer(response ~ first_name_latino +  (first_name_latino: republican ) + republican  + first_tt+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1_part, family=binomial)

m2 <- glmer(response ~ first_name_latino +  (first_name_latino: normal_vote )  + first_tt+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1_part[week1_part$republican==1,], family=binomial)

m3 <- glmer(response ~ first_name_latino +  (first_name_latino: normal_vote )  + first_tt+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1_part[week1_part$republican==0,], family=binomial)

a1 <- glmer(abs_acc ~ first_name_latino + republican +per_notlat + normal_vote + per_black +income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1id_part, family=binomial)

a1a <- glmer(abs_acc ~ first_name_latino + (first_name_latino: republican  ) + republican + per_notlat + normal_vote + per_black +income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1id_part, family=binomial)

a2<- glmer(abs_acc ~ first_name_latino +  (first_name_latino: normal_vote )  + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1id_part[week1id_part$republican==1,], family=binomial)

a3<- glmer(abs_acc ~ first_name_latino +  (first_name_latino: normal_vote )  + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1id_part[week1id_part$republican==0,], family=binomial)


pvals<-list(coef(summary(m1))[,4],coef(summary(m1a))[,4],coef(summary(m2))[,4],coef(summary(m3))[,4],coef(summary(a1))[,4],coef(summary(a1a))[,4],coef(summary(a2))[,4],coef(summary(a3))[,4])

texreg(list(m1,m1a,m2, m3,a1,a1a, a2, a3), omit.coef="code.fac", model.names=c("Response","Response","Response", "Response" ,"Abs.~Accuracy", "Abs.~Accuracy", "Abs.~Accuracy", "Abs.~Accuracy"),sideways=T, override.pval=pvals)



########################################################################
### Potential Mechanisms: 
### SI 15: Interaction of Latino Population and VRA coverage ########################################################################

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


#Apparent interaction 
w1_perlat_partpooled <-glmer(response ~ first_name_latino + first_text+per_notlat+first_name_latino:per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile +(1 + first_name_latino|code.fac), data=week1, family="binomial")
summary(w1_perlat_partpooled)

#run interaction term without VRA places
w1_perlat_partpooled_noVRA<-glmer(response ~ first_name_latino + first_text+per_notlat+first_name_latino:per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile +(1 + first_name_latino|code.fac), data=week1[week1$spanishcov==0 & week1$preclear_states==0,], family="binomial")
summary(w1_perlat_partpooled_noVRA)
    
 
####section 5 effect exists without the most Latino places
w1_perlat_partpooled_noLat<-glmer(response ~ first_name_latino + first_text+per_notlat+first_name_latino: preclear_states  +  preclear_states  + normal_vote + per_black + income_est + populationdensitypersqmile +(1 + first_name_latino|code.fac), data=week1[week1$per_notlat >= 0.90,], family="binomial")
summary(w1_perlat_partpooled_noLat)


pvals<-list(coef(summary(w1_perlat_partpooled))[,4],coef(summary(w1_perlat_partpooled_noVRA))[,4], coef(summary(w1_perlat_partpooled_noLat))[,4])

texreg(list(w1_perlat_partpooled,w1_perlat_partpooled_noVRA, w1_perlat_partpooled_noLat), override.pval=pvals,symbol = "\\cdot", stars = c(0.001,0.01, 0.05, .1))


###################################################
### SI 16 DROPPING BOTTTOM 10% BY POPULATION #########
###################################################
rm(list=ls())
load("voterIDexp_data_sept2014.Rdata")

data$abs_acc<-0
data[data$accuracy=="Absolutely accurate",]$abs_acc<-1
data[is.na(data$response)==TRUE|data$response==0,]$abs_acc<-NA

data$non_info<-0
data[data$accuracy=="Non-informative response",]$non_info<-1
data[is.na(data$response)==TRUE|data$response==0,]$non_info<-NA

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41&week1$code!=12,]
week1 <- week1[is.na(week1$response)==FALSE,]
week1$el1 <- 0
week1$el1[week1$elected==1|week1$elected==2] <- 1 
#week1$code.fac <- as.factor(week1$code)
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]

summary(week1$totalpopulation)
quantile(week1$totalpopulation, .1)
week1a <- week1[week1$totalpopulation > 588.1,]
week1ida <- week1a[week1a$first_text==1,]

m1a <- glm(response ~ first_name_latino + first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data= week1a, family=binomial(logit))
summary(m1a)
 
m2a <- glm(abs_acc ~ first_name_latino + first_text+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data= week1ida, family=binomial(logit))
summary(m2a)


m1 <- glmer(response ~ first_name_latino + first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1a, family=binomial)
summary(m1)
  
m2 <- glmer(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data= week1ida, family=binomial)
summary(m2)

texreg(list(m1a,m1,m2a,m2), omit.coef="code.fac", model.names=c("Response","Response","Abs.~Accuracy", "Abs.~Accuracy"),sideways=F)



################################################
##### SI 17 Response Rate by Email Address Type  ######
################################################

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


week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41&week1$code!=12,]
week1 <- week1[is.na(week1$response)==FALSE,]
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]


m2 <- glmer(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + addresstype4 + (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(m2)
#  professional gov addresses are more likely to respond

m2a <- glmer(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + addresstype4 + addresstype4:first_name_latino+(1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(m2a)
#but no interaction effect with treatment
 
m2b <- glm(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + addresstype4 + addresstype4:first_name_latino+ code.fac, data=week1id, family=binomial(logit))
summary(m2b)
#  professional gov addresses are more likely to respond

m3 <- glmer(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + office + (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(m3)
#  office addresses are more likely to respond

m3a <- glmer(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + office + office:first_name_latino+(1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(m3a)
#more likely, but no  interactioneffect with treatment
 
m3b <- glm(response ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + office + office:first_name_latino+ code.fac, data=week1id, family=binomial(logit))
summary(m3b)

texreg(list(m2,m2a,m2b,m3,m3a,m3b),omit.coef="code.fac")

################################################
##### SI 18 Accuracy by Email Address Type  ######
################################################

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

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41&week1$code!=12,]
week1 <- week1[is.na(week1$response)==FALSE,]
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]

m2 <- glmer(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + addresstype4 + (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(m2)
# professional address more likely to be accurate

m2a <- glmer(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + addresstype4 + addresstype4:first_name_latino + (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(m2a)
# no interaction with treatment effect

m3 <- glmer(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + office + (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(m3)
# doesn't hold for office addresses

m3a <- glmer(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + office + office:first_name_latino + (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(m3a)
# no interaction with treatment effect

##########################################
### SI 19 DROPPING THE AMBIGUOUS EMAILS ###
##########################################

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


week1 <- data[data$week==1&data$amb==0,]
week1 <- week1[week1$code!=41&week1$code!=12,]
week1 <- week1[is.na(week1$response)==FALSE,]
#week1$code.fac <- as.factor(week1$code)
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]

m2b <- glmer(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(m2b)

m3a <- glm(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data=week1id, family=binomial(logit))
summary(m3a)

fr1<- glmer(friendly ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(fr1)

fr2 <- glm(friendly ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data=week1id, family=binomial(logit))
summary(fr2)

non1<- glmer(non_info ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(non1)

non2 <- glm(non_info ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data=week1id, family=binomial(logit))
summary(non2)

pvals <- list(coef(summary(m2b))[,4],coef(summary(m3a))[,4],coef(summary(fr1))[,4],coef(summary(fr2))[,4],coef(summary(non1))[,4],coef(summary(non2))[,4])

texreg(list(m2b, m3a,fr1,fr2,non1,non2), model.names=c("Absolutely Accurate","Absolutely Accurate", "Friendly","Friendly", "Non-Informative","Non-Informative"),omit.coef="as.factor", sideways=T, override.pvals=pvals)



#############################################
#### SI 20 DROPPING THE AUTHOR CODED EMAILS ###
##############################################

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

table(data$week,data$author_coded)
week1 <- data[data$week==1&data$amb==0 & is.na(data$author_coded)==T,]
week1 <- week1[week1$code!=41&week1$code!=12,]
week1 <- week1[is.na(week1$response)==FALSE,]
#week1$code.fac <- as.factor(week1$code)
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]

m2b <- glmer(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1id, family=binomial(logit))
summary(m2b)

m3a <- glm(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data=week1id, family=binomial(logit))
summary(m3a)

fr1a<- glmer(friendly ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(fr1a)

fr2a <- glm(friendly ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data=week1id, family=binomial(logit))
summary(fr2a)

non1a<- glmer(non_info ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(non1a)

non2a <- glm(non_info ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data=week1id, family=binomial(logit))
summary(non2a)

texreg(list(m2b, m3a,fr1a,fr2a,non1a,non2a), model.names=c("Absolutely Accurate","Absolutely Accurate", "Friendly","Friendly", "Non-Informative","Non-Informative"),omit.coef="as.factor", sideways=T)

#####################################################
###### SI 21 ACCURACY AS A SINGLE CATEGORY ###########
#####################################################
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

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41&week1$code!=12,]
week1 <- week1[is.na(week1$response)==FALSE,]
#week1$code.fac <- as.factor(week1$code)
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]


table(week1id$acc_comb)

m2b <- glmer(acc_comb ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1id, family=binomial(logit))
summary(m2b)

m3a <- glm(acc_comb ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data=week1id, family=binomial(logit))
summary(m3a)

texreg(list(m2b,m3a), model.names=c("Partially Pooled","Pooled"),omit.coef="as.factor")

#######################################################
############# SI 22 RESPONSE:LAW CATEGORY 
###############     AND REGION ###############
#######################################################
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

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41&week1$code!=12,]
week1 <- week1[is.na(week1$response)==FALSE,]
#week1$code.fac <- as.factor(week1$code)
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]

#with and without id laws, fully pooled models within these subsets 
w1id1 <- week1[week1$idlaw==1,] 
fp1 <- glm(response ~ first_name_latino + first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data=w1id1, family=binomial(logit))
summary(fp1)

w1id2 <- week1[week1$idlaw==2|week1$idlaw==3,]
fp2 <- glm(response ~ first_name_latino + first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data=w1id2, family=binomial(logit))
summary(fp2)

#four regions, fully pooled models within the regions
w1reg1 <- week1[week1$statereg2==1,] 
fp1a <- glm(response ~ first_name_latino + first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data= w1reg1, family=binomial(logit))
summary(fp1a)

w1reg2 <- week1[week1$statereg2==2,]
fp2a <- glm(response ~ first_name_latino + first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data= w1reg2, family=binomial(logit))
summary(fp2a)

w1reg3 <- week1[week1$statereg2==3,]
fp3a <- glm(response ~ first_name_latino + first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data= w1reg3, family=binomial(logit))
summary(fp3a)

w1reg4 <- week1[week1$statereg2==4,]
fp4a <- glm(response ~ first_name_latino + first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data= w1reg4, family=binomial(logit))
summary(fp4a)  

texreg(list(fp1,fp2,fp1a,fp2a,fp3a,fp4a), model.names=c("HAVA","ID Law","West","Midwest","East Coast"),omit.coef="as.factor",sideways=T)

#######################################################
############# SI 23 ACCURACY:LAW CATEGORY 
###############     AND REGION ###############
#######################################################

w1id1 <- week1id[week1id$idlaw==1,]
fp1 <- glm(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data=w1id1, family=binomial(logit))
summary(fp1)
w1id2 <- week1id[week1id$idlaw==2|week1id$idlaw==3,]

fp2 <- glm(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data=w1id2, family=binomial(logit))
summary(fp2)

w1reg1 <- week1id[week1id$statereg2==1,]
fp1a <- glm(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data= w1reg1, family=binomial(logit))
summary(fp1a)

w1reg2 <- week1id[week1id$statereg2==2,]
fp2a <- glm(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data= w1reg2, family=binomial(logit))
summary(fp2a)
 
w1reg3 <- week1id[week1id$statereg2==3,]
fp3a <- glm(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data= w1reg3, family=binomial(logit))
summary(fp3a) 

w1reg4 <- week1id[week1id$statereg2==4,]
fp4a <- glm(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + code.fac, data= w1reg4, family=binomial(logit))
summary(fp4a)  

texreg(list(fp1,fp2,fp1a,fp2a,fp3a,fp4a), model.names=c("HAVA","ID Law","West","Midwest","East Coast", "South"),omit.coef="code.fac",sideways=T)



####################################################################
####### SI 24 PARTIALLY POOLED TRIPLE LEVEL CLUSTERING ##########
####################################################################
  

week1$law.fac <- as.factor(week1$idlaw)
week1$reg.fac2 <- as.factor(week1$statereg2)

week1id$law.fac <- as.factor(week1id$idlaw)
week1id$reg.fac2 <- as.factor(week1id$statereg2)

m4 <- glmer(response ~ first_name_latino + first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino + first_text|code.fac) + (1 + first_name_latino + first_text|law.fac), data=week1, family=binomial)
summary(m4)

m4a <- glmer(response ~ first_name_latino + first_text + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino + first_text|code.fac) + (1 + first_name_latino + first_text|reg.fac2), data=week1, family=binomial)
summary(m4a)

m4b <- glmer(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac) + (1 + first_name_latino|law.fac), data=week1id, family=binomial)
summary(m4b)

m4c <- glmer(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac) + (1 + first_name_latino|reg.fac2), data=week1id, family=binomial)
summary(m4c)

texreg(list(m4,m4a,m4b,m4c), model.names=c("Law","Region","Law","Region"),omit.coef="as.factor",sideways=F)


#######################################################
####### SI 25-28: DROPPING LARGE SAMPLE STATES ##########
########################################################

## To protect bureaucrats' anonymity, the code linking states to outcomes is not included. Please email the authors if you would like access to this code for an academic project at arwhite@fas.harvard.edu. ##

##################################################
####### SI 29-30 DROPPING PAIRS OF UNTREATED ##########
###################################################

library(Hmisc)
rm(list=ls())
load("voterIDexp_data_sept2014.Rdata")

data$abs_acc<-0
data[data$accuracy=="Absolutely accurate",]$abs_acc<-1
data[is.na(data$response)==TRUE|data$response==0,]$abs_acc<-NA

data$acc_comb<-0
data[data$accuracy=="Absolutely accurate"|data$accuracy=="General, but accurate"|data$accuracy=="Narrow, but accurate",]$acc_comb<-1
data[is.na(data$response)==TRUE|data$response==0,]$acc_comb<-NA

week1 <- data[data$week==1,]
week1 <- week1[week1$code!=41&week1$code!=12,]


week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]


states <- unique(week1$code)
week1a <- matrix(NA, nrow=1, ncol=ncol(week1))
colnames(week1a) <- colnames(week1)
for(i in 1:length(states)){
	d <- week1[week1$code==states[i],]
	ids <- d$fullpair[is.na(d$response)==TRUE]
	d2 <- d[d$fullpair %nin% ids,]
	week1a <- rbind(week1a, d2)
}
dim(week1)
dim(week1a)
week1 <- week1a[-1,]
week1 <- week1[week1$code!=41&week1$code!=12,]
week1id <- week1[week1$first_text==1,]
week1pr <- week1[week1$first_text==0,]


################ ##
## SI 29: RESPONSE ##
################ ##


w1resp_pooleda<-glm(response ~ first_name_latino + first_text+per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile +as.factor(code.fac), data=week1, family="binomial")
summary(w1resp_pooleda)

w1resp_partpooleda<- glmer(response ~ first_name_latino +first_text+ per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1, family=binomial)
summary(w1resp_partpooleda)


##########################
####### SI 30: Acc #######
##########################

w1resp_pooledb<-glm(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile+as.factor(code.fac), data=week1id, family="binomial")
summary(w1resp_pooledb)


w1resp_partpooledb<- glmer(abs_acc ~ first_name_latino + per_notlat + normal_vote + per_black + income_est + populationdensitypersqmile + (1 + first_name_latino|code.fac), data=week1id, family=binomial)
summary(w1resp_partpooledb)



