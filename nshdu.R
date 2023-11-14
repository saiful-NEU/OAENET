########################################
library("msaenet")
library("glmnet")
library("MatchIt")
library("lmtest")
library("phonTools")
library("MASS")
library("sandwich")
library(CovSel)
#install.packages("BCEE")
library(BCEE)
#install.packages("mlbench")
library(mlbench)
#install.packages("Boruta")
library(Boruta)
#install.packages("bacr")
library(bacr)
#library(rJava)
#library(CovSelHigh)

library("matrixStats")
library(dplyr)

#install.packages("dplyr")
library(dplyr)
library(tictoc)

# install.packages("boot")
library(boot)
########################################################

raw_data = read.csv("C:\\Users\\Md Saiful Islam\\OneDrive - Northeastern University\\High Dim Papers\\Experiments\\NSHDU\\2015-19_OUD_data1.csv")

#converting the categories into numeric factors

raw_data$year = as.numeric(factor(raw_data$year))
raw_data$year = factor(raw_data$year)


raw_data = subset(raw_data, select = -c(PNRLWD3SX_flag) )

dat_NA_removed = subset(raw_data, dstworst_flag != "Not Available")
dat_NA_removed = subset(dat_NA_removed, c(pnrlndmor_flag, ANYHLTI2_flag,pnrwygamt_flag,pnrwyoftn_flag,pnrwylngr_flag,pnrrspain_flag,
                          pnrrsrelx_flag,pnrrsexpt_flag,pnrrshigh_flag,pnrrsslep_flag,pnrrsemot_flag,pnrrsdgfx_flag,
                          pnrrshook_flag,pnrrssor_flag,pnrllottm_flag,pnrllimit_flag,pnrlcutdn_flag)!= "Not Available")

dat_NA_removed = subset(dat_NA_removed, pnrlndmor_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, ANYHLTI2_flag != "Not Available") 
dat_NA_removed = subset(dat_NA_removed, pnrwygamt_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrwyoftn_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrwylngr_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrrspain_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrrsrelx_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrrsexpt_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrrshigh_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrrsslep_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrrsemot_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrrsdgfx_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrrshook_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrrssor_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrllottm_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrllimit_flag!= "Not Available")
dat_NA_removed = subset(dat_NA_removed, pnrlcutdn_flag!= "Not Available")

dat_NA_removed = subset(dat_NA_removed, pnrlndmor_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, ANYHLTI2_flag != "Not Availaible") 
dat_NA_removed = subset(dat_NA_removed, pnrwygamt_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrwyoftn_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrwylngr_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrrspain_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrrsrelx_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrrsexpt_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrrshigh_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrrsslep_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrrsemot_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrrsdgfx_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrrshook_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrrssor_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrllottm_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrllimit_flag!= "Not Availaible")
dat_NA_removed = subset(dat_NA_removed, pnrlcutdn_flag!= "Not Availaible")




dat_NA_removed$CATAG3 = factor(dat_NA_removed$CATAG3)

dat_NA_removed$ANYHLTI2_flag = as.numeric(factor(dat_NA_removed$ANYHLTI2_flag))-1
#dat_NA_removed$ANYHLTI2_flag = factor(dat_NA_removed$ANYHLTI2_flag)

dat_NA_removed$pnrwygamt_flag = as.numeric(factor(dat_NA_removed$pnrwygamt_flag))-1
#dat_NA_removed$pnrwygamt_flag = factor(dat_NA_removed$pnrwygamt_flag)

dat_NA_removed$pnrwyoftn_flag = as.numeric(factor(dat_NA_removed$pnrwyoftn_flag))-1
#dat_NA_removed$pnrwyoftn_flag = factor(dat_NA_removed$pnrwyoftn_flag)

dat_NA_removed$pnrwylngr_flag = as.numeric(factor(dat_NA_removed$pnrwylngr_flag))-1
#dat_NA_removed$pnrwylngr_flag = factor(dat_NA_removed$pnrwylngr_flag)

dat_NA_removed$pnrrspain_flag = as.numeric(factor(dat_NA_removed$pnrrspain_flag))-1
#dat_NA_removed$pnrrspain_flag = factor(dat_NA_removed$pnrrspain_flag)

dat_NA_removed$pnrrsrelx_flag = as.numeric(factor(dat_NA_removed$pnrrsrelx_flag))-1
#dat_NA_removed$pnrrsrelx_flag= factor(dat_NA_removed$pnrrsrelx_flag)

dat_NA_removed$pnrrsexpt_flag = as.numeric(factor(dat_NA_removed$pnrrsexpt_flag))-1
#dat_NA_removed$pnrrsexpt_flag = factor(dat_NA_removed$pnrrsexpt_flag)

dat_NA_removed$pnrrshigh_flag = as.numeric(factor(dat_NA_removed$pnrrshigh_flag))-1
#dat_NA_removed$pnrrshigh_flag = factor(dat_NA_removed$pnrrshigh_flag)

dat_NA_removed$pnrrsslep_flag = as.numeric(factor(dat_NA_removed$pnrrsslep_flag))-1
#dat_NA_removed$pnrrsslep_flag = factor(dat_NA_removed$pnrrsslep_flag)

dat_NA_removed$pnrrsemot_flag = as.numeric(factor(dat_NA_removed$pnrrsemot_flag))-1
#dat_NA_removed$pnrrsemot_flag = factor(dat_NA_removed$pnrrsemot_flag)

dat_NA_removed$pnrrsdgfx_flag = as.numeric(factor(dat_NA_removed$pnrrsdgfx_flag))-1
#dat_NA_removed$pnrrsdgfx_flag = factor(dat_NA_removed$pnrrsdgfx_flag)

dat_NA_removed$pnrrshook_flag = as.numeric(factor(dat_NA_removed$pnrrshook_flag))-1
#dat_NA_removed$pnrrshook_flag = factor(dat_NA_removed$pnrrshook_flag)

dat_NA_removed$pnrrssor_flag = as.numeric(factor(dat_NA_removed$pnrrssor_flag))-1
#dat_NA_removed$pnrrssor_flag = factor(dat_NA_removed$pnrrssor_flag)

dat_NA_removed$dstworst_flag = as.numeric(factor(dat_NA_removed$dstworst_flag))-1
#dat_NA_removed$dstworst_flag = factor(dat_NA_removed$dstworst_flag)

dat_NA_removed$pnrllottm_flag = as.numeric(factor(dat_NA_removed$pnrllottm_flag))-1
#dat_NA_removed$pnrllottm_flag = factor(dat_NA_removed$pnrllottm_flag)

dat_NA_removed$pnrllimit_flag = as.numeric(factor(dat_NA_removed$pnrllimit_flag))-1
#dat_NA_removed$pnrllimit_flag = factor(dat_NA_removed$pnrllimit_flag)

dat_NA_removed$pnrlndmor_flag = as.numeric(factor(dat_NA_removed$pnrlndmor_flag))-1
#dat_NA_removed$pnrlndmor_flag = factor(dat_NA_removed$pnrlndmor_flag)

dat_NA_removed$pnrlcutdn_flag = as.numeric(factor(dat_NA_removed$pnrlcutdn_flag))-1
#dat_NA_removed$pnrlcutdn_flag = factor(dat_NA_removed$pnrlcutdn_flag)

################################
#Processing No-OUD data

raw_data_no_oud = read.csv("C:\\Users\\Md Saiful Islam\\OneDrive - Northeastern University\\High Dim Papers\\Experiments\\NSHDU\\2015-19_no_OUD_data1.csv")


raw_data_no_oud$year = as.numeric(factor(raw_data_no_oud$year))
raw_data_no_oud$year = factor(raw_data_no_oud$year)

dat_NA_removed_no_oud = subset(raw_data_no_oud, dstworst_flag != "Not Available")

#dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, c(pnrlndmor_flag, ANYHLTI2_flag,pnrwygamt_flag,pnrwyoftn_flag,pnrwylngr_flag,pnrrspain_flag,
#                                          pnrrsrelx_flag,pnrrsexpt_flag,pnrrshigh_flag,pnrrsslep_flag,pnrrsemot_flag,pnrrsdgfx_flag,
#                                          pnrrshook_flag,pnrrssor_flag,pnrllottm_flag,pnrllimit_flag,pnrlcutdn_flag)!= "Not Available")

dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrlndmor_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, ANYHLTI2_flag != "Not Available") 
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrwygamt_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrwyoftn_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrwylngr_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrspain_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrsrelx_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrsexpt_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrshigh_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrsslep_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrsemot_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrsdgfx_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrshook_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrssor_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrllottm_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrllimit_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrlcutdn_flag!= "Not Available")

dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrlndmor_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, ANYHLTI2_flag != "Not Availaible") 
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrwygamt_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrwyoftn_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrwylngr_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrspain_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrsrelx_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrsexpt_flag!= "Not Available")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrshigh_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrsslep_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrsemot_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrsdgfx_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrshook_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrrssor_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrllottm_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrllimit_flag!= "Not Availaible")
dat_NA_removed_no_oud = subset(dat_NA_removed_no_oud, pnrlcutdn_flag!= "Not Availaible")




dat_NA_removed_no_oud$CATAG3 = factor(dat_NA_removed_no_oud$CATAG3)



#raw_data_no_oud = subset(raw_data_no_oud, select = -c(PNRLWD3SX_flag) )

dat_NA_removed_no_oud$ANYHLTI2_flag = as.numeric(factor(dat_NA_removed_no_oud$ANYHLTI2_flag))-1
#dat_NA_removed_no_oud$ANYHLTI2_flag = factor(dat_NA_removed_no_oud$ANYHLTI2_flag)

dat_NA_removed_no_oud$pnrwygamt_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrwygamt_flag))-1
#dat_NA_removed_no_oud$pnrwygamt_flag = factor(dat_NA_removed_no_oud$pnrwygamt_flag)

dat_NA_removed_no_oud$pnrwyoftn_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrwyoftn_flag))-1
#dat_NA_removed_no_oud$pnrwyoftn_flag = factor(dat_NA_removed_no_oud$pnrwyoftn_flag)

dat_NA_removed_no_oud$pnrwylngr_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrwylngr_flag))-1
#dat_NA_removed_no_oud$pnrwylngr_flag = factor(dat_NA_removed_no_oud$pnrwylngr_flag)

dat_NA_removed_no_oud$pnrrspain_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrrspain_flag))-1
#dat_NA_removed_no_oud$pnrrspain_flag = factor(dat_NA_removed_no_oud$pnrrspain_flag)

dat_NA_removed_no_oud$pnrrsrelx_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrrsrelx_flag))-1
#dat_NA_removed_no_oud$pnrrsexpt_flag = factor(dat_NA_removed_no_oud$pnrrsexpt_flag)

dat_NA_removed_no_oud$pnrrsexpt_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrrsexpt_flag))-1
#dat_NA_removed_no_oud$pnrrsexpt_flag = factor(dat_NA_removed_no_oud$pnrrsexpt_flag)

dat_NA_removed_no_oud$pnrrshigh_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrrshigh_flag))-1
#dat_NA_removed_no_oud$pnrrshigh_flag = factor(dat_NA_removed_no_oud$pnrrshigh_flag)

dat_NA_removed_no_oud$pnrrsslep_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrrsslep_flag))-1
#dat_NA_removed_no_oud$pnrrsslep_flag = factor(dat_NA_removed_no_oud$pnrrsslep_flag)

dat_NA_removed_no_oud$pnrrsemot_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrrsemot_flag))-1
#dat_NA_removed_no_oud$pnrrsemot_flag = factor(dat_NA_removed_no_oud$pnrrsemot_flag)

dat_NA_removed_no_oud$pnrrsdgfx_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrrsdgfx_flag))-1
#dat_NA_removed_no_oud$pnrrsdgfx_flag = factor(dat_NA_removed_no_oud$pnrrsdgfx_flag)

dat_NA_removed_no_oud$pnrrshook_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrrshook_flag))-1
#dat_NA_removed_no_oud$pnrrshook_flag = factor(dat_NA_removed_no_oud$pnrrshook_flag)

dat_NA_removed_no_oud$pnrrssor_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrrssor_flag))-1
#dat_NA_removed_no_oud$pnrrssor_flag = factor(dat_NA_removed_no_oud$pnrrssor_flag)

dat_NA_removed_no_oud$dstworst_flag = as.numeric(factor(dat_NA_removed_no_oud$dstworst_flag))-1
#dat_NA_removed_no_oud$dstworst_flag = factor(dat_NA_removed_no_oud$dstworst_flag)

dat_NA_removed_no_oud$pnrllottm_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrllottm_flag))-1
#dat_NA_removed_no_oud$pnrllottm_flag = factor(dat_NA_removed_no_oud$pnrllottm_flag)

dat_NA_removed_no_oud$pnrllimit_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrllimit_flag))-1
#dat_NA_removed_no_oud$pnrllimit_flag = factor(dat_NA_removed_no_oud$pnrllimit_flag)

dat_NA_removed_no_oud$pnrlndmor_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrlndmor_flag))-1
#dat_NA_removed_no_oud$pnrlndmor_flag = factor(dat_NA_removed_no_oud$pnrlndmor_flag)

dat_NA_removed_no_oud$pnrlcutdn_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrlcutdn_flag))-1
#dat_NA_removed_no_oud$pnrlcutdn_flag = factor(dat_NA_removed_no_oud$pnrlcutdn_flag)

#dat_NA_removed_no_oud$pnrrsexpt_flag = as.numeric(factor(dat_NA_removed_no_oud$pnrrsexpt_flag))-1
#dat_NA_removed_no_oud$pnrrsexpt_flag = factor(dat_NA_removed_no_oud$pnrrsexpt_flag)

#####################################

itr_max = 1000

max_var = 50

# #declare a zero matrix to hold att
att.adnet = zeros(itr_max)
att.adlas = zeros(itr_max)
att.Boruta_T = zeros(itr_max)
att.Boruta_Y = zeros(itr_max)
att.BACR = zeros(itr_max)
att.BCEE = zeros(itr_max)
# 
# #declare zero matrices to store the indexes of selected variables
select.var.list.onet = zeros(itr_max, max_var)
select.var.list.adlas = zeros(itr_max, max_var)
select.var.list.bort = zeros(itr_max, max_var)
select.var.list.bory = zeros(itr_max, max_var)
select.var.list.bacr = zeros(itr_max, max_var)
select.var.list.bcee = zeros(itr_max, max_var)
# 
# #Declare zero matrices to store the total number of variables selected
total.num.var.onet = zeros(itr_max)
total.num.var.adlas = zeros(itr_max)
total.num.var.bort = zeros(itr_max)
total.num.var.bory = zeros(itr_max)
total.num.var.bacr = zeros(itr_max)
total.num.var.bcee = zeros(itr_max)

# #Declare zero matrices to store the total number of variables selected
time.adnet = zeros(itr_max)
time.adlas = zeros(itr_max)
time.bort = zeros(itr_max)
time.bory = zeros(itr_max)
time.bacr = zeros(itr_max)
time.bcee = zeros(itr_max)


for (i in 1:1){
  itr = i

  #combining both OUD and No-Oud data
  
  
dat_NA_removed_no_oud_sample = sample_n(dat_NA_removed_no_oud, 5000)

combined_data = rbind(dat_NA_removed,dat_NA_removed_no_oud_sample)


########################
# Preparing the covariates, outcomes, and treat data frame

df = combined_data
Y = df$suicide_flag
A = df$udpypnr

XA = subset(df, select = -c(suicide_flag))
XX = subset(XA, select = -c(udpypnr))


############################
# Applying Outcome adaptive elastic net
# Step 1: Logistic regression for penalty

my_logit_mod = glm(suicide_flag ~ ., data = df, family = "binomial")
odds_ratio = (coef(my_logit_mod))
odds_ratio = odds_ratio[-c(1,24)]



# Setting the penalty
gamma = 5

penalty = 1/(abs(odds_ratio))^gamma

penalty[is.na(penalty)] <- 10000000000

#Perform cross validation
tic()

K.cv.fit = cv.glmnet(data.matrix(XX), data.matrix(A),  type.measure = "mse",
                     nfolds = 5, gamma = c(0, 0.25, 0.5, 0.75, 1), relax = TRUE, family="binomial")

cv.alpha = K.cv.fit$relaxed
alpha.opt = cv.alpha$gamma.1se
#print(alpha.opt)
#lambda = K.cv.fit$lambda.1se,
fit2=glmnet(as.matrix(XX), as.matrix(A), alpha =alpha.opt, lambda = K.cv.fit$lambda.1se, penalty.factor = penalty, family="binomial")
t=toc()
time.adnet [itr]= t$toc - t$tic

beta_ps_raw = as.matrix(coef(fit2))
beta_ps_raw = as.matrix(coef(fit2, s = 0.02))
beta_ps_allvar = as.matrix(beta_ps_raw[2:(43)])

beta_ps_non_zero = row(beta_ps_allvar)[which(abs(beta_ps_allvar) >= 0.1)]
sel.var.index  = beta_ps_non_zero

#Storing the Indexes of selected variables
select.var.list.onet[itr, 1:length(sel.var.index)] = sel.var.index

#Storing the total number of variables selected
total.num.var.onet [itr] = length(sel.var.index)

xx_true = XX[,beta_ps_non_zero]

true.var.list = names(xx_true)
treat.form = formula(paste("udpypnr~",paste(true.var.list,collapse="+")))
mm = matchit(treat.form, data = df, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "control")

effect.form = formula(paste("suicide_flag ~ udpypnr+",paste(true.var.list,collapse="+")))
fit.effect = lm(effect.form, data = match.data(mm))
final.coef = fit.effect$coefficients
#final.coef[2]
att.adnet[itr] = final.coef[2]
print("Onet")
print(att.adnet[itr])



######### Outcome Adaptive LASSO
tic()
K.cv.fit = cv.glmnet(data.matrix(XX), data.matrix(A), penalty.factor = penalty, type.measure = "mse",
                     nfolds = 5, gamma = 1,
                     relax = FALSE, family="binomial")
fit2=glmnet(as.matrix(XX),as.matrix(A), alpha = 1, penalty.factor = pen, lambda = K.cv.fit$lambda.1se, family="binomial")
t=toc()
time.adlas[itr] = t$toc - t$tic

beta_ps_raw = as.matrix(coef(fit2))
beta_ps_allvar = as.matrix(beta_ps_raw[2:43])

beta_ps_non_zero = row(beta_ps_allvar)[which(abs(beta_ps_allvar) >= 0.1)]
#beta_ps_non_zero = row(beta_ps_allvar)[which(!beta_ps_allvar == 0)]
sel.var.index  = beta_ps_non_zero

#Storing the Indexes of selected variables
select.var.list.adlas[itr, 1:length(sel.var.index)] = sel.var.index

#Storing the totoal number of variables selected
total.num.var.adlas [itr] = length(sel.var.index)

xx_true = XX[,beta_ps_non_zero]

true.var.list = names(xx_true)
treat.form = formula(paste("udpypnr~",paste(true.var.list,collapse="+")))
mm = matchit(treat.form, data = df, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "control")

effect.form = formula(paste("suicide_flag ~ udpypnr+",paste(true.var.list,collapse="+")))
fit.effect = lm(effect.form, data = match.data(mm))
final.coef = fit.effect$coefficients
att.adlas[itr] = final.coef[2]
print("Adap LASSO")
print(att.adlas[itr])



# #########Boruta_T
tic()
bort = Boruta(XX,A,pValue = 0.10)
t=toc()
time.bort[itr] = t$toc - t$tic
#
bort_df = data.frame(bort$finalDecision)
bort_df$Index = 1:dim.data.frame(bort_df)[1]
bort_df = bort_df[bort_df$bort.finalDecision=="Confirmed",]
sel.var.index = bort_df$Index
bort.cov = XX[ , sel.var.index]
#
n_bort = length(sel.var.index)
bort.cov = data.matrix(bort.cov)
bor.data = data.frame(bort.cov, A, Y)
bor.data_1 = data.frame(bort.cov, A)
mm = matchit(A ~ bort.cov, data = bor.data, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "control")
#
matched_data = (match.data(mm))
#
df_b = matched_data[,1:(n_bort+2)]
fit.effect = lm(Y ~ A + . , data = df_b)
final.coef = fit.effect$coefficients
att.Boruta_T[itr] =  final.coef[2]
#
# #Storing the Indexes of selected variables
select.var.list.bort[itr, 1:length(sel.var.index)] = sel.var.index

#Storing the total number of variables selected
total.num.var.bort [itr] = n_bort
#
print("Boruta T")
print(att.Boruta_T[itr])
#
#
# #########Boruta_Y
tic()
bory = Boruta(XX,Y,pValue = 0.10)
t=toc()
time.bory[itr] = t$toc - t$tic

bory_df = data.frame(bory$finalDecision)
bory_df$Index = 1:dim.data.frame(bory_df)[1]
bory_df = bory_df[bory_df$bory.finalDecision=="Confirmed",]
sel.var.index = bory_df$Index
bory.cov = XX[ , sel.var.index]
#
n_bory = length(sel.var.index)
bory.cov = data.matrix(bory.cov)
bor.y.data = data.frame(bory.cov, A, Y)
mm = matchit(A ~ bory.cov, data = bor.y.data, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "control")

matched_data = (match.data(mm))

df_by = matched_data[,1:(n_bory + 2)]
fit.effect = lm(Y ~ A + . , data = df_by)
final.coef = fit.effect$coefficients
att.Boruta_Y[itr] = final.coef[2]
#
# #Storing the Indexes of selected variables
select.var.list.bory[itr, 1:length(sel.var.index)] = sel.var.index
#
#Storing the total number of variables selected
total.num.var.bory [itr] = n_bory
#
print("Boruta Y")
print(att.Boruta_Y[itr])

print(i)

}

###################### Writing att data frame
df_att = data.frame(att.adnet, att.adlas, att.Boruta_T, att.Boruta_Y)
#df_att = rbind(df_att,ATT)
write.csv(df_att, "NSDUH_att.boot_new.csv")


###################### Writing att data frame
df_time = data.frame(time.adnet, time.adlas, time.bort, time.bory)
#df_att = rbind(df_att,ATT)
write.csv(df_time, "NSDUH_time.boot_new.csv")


##################### Writing total number of variable selected 
df_sel.var = data.frame(total.num.var.onet, total.num.var.adlas, total.num.var.bort, total.num.var.bory)
write.csv(df_sel.var, "NSDUH_total.var.boot_new.csv")


#################### Writing the selected variable indexes for each iteration
#df_sel.var.list.onet = rbind(df_sel.var.list.onet, select.var.list.onet[itr,])
write.csv(select.var.list.onet, "NSDUH_onet.sel.var.list.boot_new.csv")

#df_sel.var.list.adlas = rbind(df_sel.var.list.adlas, select.var.list.adlas[itr,])
write.csv(select.var.list.adlas, "NSDUH_adlas.sel.var.list.boot_new.csv")

#df_sel.var.list.bort = rbind(df_sel.var.list.bort, select.var.list.bort[itr,])
write.csv(select.var.list.bort, "NSDUH_bort.sel.var.list.boot_new.csv")

#df_sel.var.list.bory = rbind(df_sel.var.list.bory, select.var.list.bory[itr,])
write.csv(select.var.list.bory, "NSDUH_bory.sel.var.list.boot_new.csv")





##############################
#comb_data = rbind(dat_NA_removed,dat_NA_removed_no_oud)
write.csv(dat_NA_removed, "Final_OUD.csv")
write.csv(dat_NA_removed_no_oud, "Final_NO_OUD.csv")

###############
#BACR
tic()
bacr = bac(data=((combined_data)), exposure="udpypnr", outcome="suicide_flag", confounders=paste(names(XX), sep = ""),
           interactors=NULL, familyX="binomial", familyY="binomial", omega=Inf,
           num_its=200, burnM=1, burnB=1, thin=1)
t=toc()
time.bacr[itr] = t$toc - t$tic

bacr_df = data.frame(bacr$models)
bacr_df = bacr_df[1,1:dim(XX)[2]]
sel.var.index = which(bacr_df>0)
bacr.cov = df[ , sel.var.index]

n_bacr = length(sel.var.index)
bacr.cov = as.matrix(bacr.cov)
bacr.data = data.frame((bacr.cov), A, Y)
mm = matchit(A ~ bacr.cov, data = bacr.data, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "both")

matched_data = (match.data(mm))

df_b = matched_data[,1:(n_bacr + 2)]
fit.effect = lm(Y ~ A + ., data = df_b)
final.coef = fit.effect$coefficients
att.BACR[itr] = final.coef[2]

#Storing the Indexes of selected variables
select.var.list.bacr[itr, 1:length(sel.var.index)] = sel.var.index

#Storing the total number of variables selected
total.num.var.bacr [itr] = n_bacr

#att.BACR[itr] = mean(bacr$ACE)

print("BACR")
print(att.BACR[itr])


########################################BCEE
xxx = as.matrix(unname(as.matrix(XX)))
tic()
bcee = GBCEE((as.matrix(A)), as.matrix(Y), xxx, omega = 300*sqrt(6659), niter = 5000, family.X = "binomial", family.Y = "binomial", 
             X1 = 1, X0 = 0, priorX = NA, priorY = NA, maxsize = NA, OR = 20, truncation = c(0.01, 0.99), var.comp = "asymptotic", B = 200)
t=toc()
time.bcee[itr] = t$toc - t$tic

#Considering the Posterior distribution of the outcome model
bcee_df = data.frame(bcee$models.Y)

#Selecting the top 5 models as they includes 80% of the posterior probability
freq_var_sel = colSums(bcee_df[1:5,])

#Select the variables who appear in at least 50% of the models
sel.var.index = which((freq_var_sel[1:100] > 2))
bcee.cov = df[, sel.var.index]

n_bcee = length(sel.var.index)
bcee.cov = as.matrix(bcee.cov)
df_bcee = data.frame(bcee.cov, A, Y)
mm = matchit(A ~ bcee.cov, data = df_bcee, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "both")

matched_data = (match.data(mm))

df_b = matched_data[,1:(n_bcee + 2)]
fit.effect = lm(Y ~ A + ., data = df_b)
final.coef = fit.effect$coefficients
att.BCEE[itr] = final.coef[2]

#Storing the Indexes of selected variables
select.var.list.bcee[itr, 1:length(sel.var.index)] = sel.var.index

#Storing the total number of variables selected
total.num.var.bcee [itr] = n_bcee

print("BCEE")
print(att.BCEE[itr])






