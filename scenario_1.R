
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

#install.packages("dplyr")
library(dplyr)










##########################
#Set the number of simulation
itr_max = 1000
# sample size
n = 1000
# total number of predictors
p = 100
# number of good variables
gd.var = 20
### set true average treatment effect
bA = 0.5
# set information for simulating coviariates
mean_x = 0 
sig_x = 1
rho = 0.5
rho_noise = 0.3

# Set strength of relationship between covariates and outcome
beta_v =  1*c( 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, rep(0, p - gd.var) )
# Set strength of relationship between covariates and treatment
alpha_v = 1*c( 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, rep(0, p - gd.var) )



max_var= p
pC = pI = pP = 10
pS = p - (pC+pI+pP)
#all variable list
var.list = c(paste("Xc",1:pC,sep=""),paste("Xp",1:pP,sep=""),paste("Xi",1:pI,sep=""),paste("Xs",1:pS,sep=""))
#Some other list we will use
target.var.list = c(paste("Xc",1:pC,sep=""),paste("Xp",1:pP,sep=""))
potconf.var.list = c(paste("Xc",1:pC,sep=""),paste("Xp",1:pP,sep=""),paste("Xi",1:pI,sep=""))
conf.var.list = c(paste("Xc",1:pC,sep=""))

# 

names(beta_v) = names(alpha_v) = var.list


### define some functions for generating data, ATE estimates, and the wAMD,
expit = function(x){ 
  pr = ( exp(x) / (1+exp(x)) ) 
  return(pr)
}

# #declare a zero matrix to hold att
 att.adnet = zeros(itr_max)
 att.adlas = zeros(itr_max)
 att.target = zeros(itr_max)
 att.potconf = zeros(itr_max)
 att.conf = zeros(itr_max)
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

#Running the simulation 
for (i in 1:itr_max) {
  itr = i
  if (i == 1){
    df_att = data.frame()
    df_sel.var = data.frame()
    df_sel.var.list.onet = data.frame()
    df_sel.var.list.adlas = data.frame()
    df_sel.var.list.bort = data.frame()
    df_sel.var.list.bory = data.frame()
    df_sel.var.list.bacr = data.frame()
    df_sel.var.list.bcee = data.frame()
  }
  
  if (i != 1){
    ###################### Writing att data frame
    df_att = read.csv("att.cor_0.0.csv")
    df_att = df_att[,-1]

    ##################### Writing total number of variable selected 
    df_sel.var = read.csv("sel.var.cor_0.0.csv")
    df_sel.var = df_sel.var [,-1]
    
    #################### Writing the selected variable indexes for each iteration
    df_sel.var.list.onet = read.csv("onet.sel.var.list.cor_0.0")
    df_sel.var.list.onet = df_sel.var.list.onet [,-1]
      
    df_sel.var.list.adlas = read.csv("adlas.sel.var.list.cor_0.0")
    df_sel.var.list.adlas = df_sel.var.list.adlas [,-1]
    
    df_sel.var.list.bort = read.csv("bort.sel.var.list.cor_0.0")
    df_sel.var.list.bort = df_sel.var.list.bort [,-1]
      
    df_sel.var.list.bory = read.csv("bory.sel.var.list.cor_0.0")
    df_sel.var.list.bory = df_sel.var.list.bory [,-1]
      
    df_sel.var.list.bacr = read.csv("bacr.sel.var.list.cor_0.0")
    df_sel.var.list.bacr = df_sel.var.list.bacr [,-1]
      
    df_sel.var.list.bcee = read.csv("bcee.sel.var.list.cor_0.0")
    df_sel.var.list.bcee = df_sel.var.list.bcee [,-1]
  }
  
  ### simulate data in old ways
  Sigma_x = matrix(rho*sig_x^2,nrow=length(var.list),ncol=length(var.list)) 
  diag(Sigma_x) = sig_x^2
  Mean_x = rep(mean_x,length(var.list))
  Data = as.data.frame(mvrnorm(n = n,mu=Mean_x,Sigma = Sigma_x,empirical = FALSE))
  names(Data) = var.list
  
  # ## Simulating data with different correlation
  # #Simulated the good variabels (the potential confounders)
  # Sigma_x = matrix(rho*sig_x^2, nrow = length(potconf.var.list), ncol = length(potconf.var.list))
  # diag(Sigma_x) = sig_x^2
  # Mean_x = rep(mean_x,length(potconf.var.list))
  # gd_data = mvrnorm(n = n, mu = Mean_x, Sigma = Sigma_x, empirical = FALSE)
  # #Simulating the noisy variables
  # Sigma_noise = matrix(rho_noise*sig_x^2,nrow = pS, ncol = pS)
  # diag(Sigma_noise) = sig_x^2
  # Mean_noise = rep(mean_x,pS)
  # noisy_data = mvrnorm(n = n, mu = Mean_noise, Sigma = Sigma_noise, empirical = FALSE)
  # #Creating data frame by combining the good and noisy data
  # Data = data.frame(gd_data, noisy_data)
  # names(Data) = var.list
  
  #Creating treatment
  gA_x = rowSums(Data[,var.list]*matrix(alpha_v,nrow=n,ncol=length(var.list),byrow=TRUE))
  pA = expit( gA_x )
  Data$A = as.numeric( runif(n=length(pA)) < pA) # simulate A 
  
  #Creating outcoem
  gY_xA = rowSums(Data[,var.list]*matrix(beta_v,nrow=n,ncol=length(var.list),byrow=TRUE)) 
    #+ Data[,1]*Data[,12] + Data[,10]*Data[,8] + + Data[,3]^2  
  Data$Y = gY_xA + rnorm(n=n,sd=sig_x) #adding random noise
  Data$Y = Data$Y + Data$A*bA
  
  # Normlize coviarates to have mean 0 and standard deviation 1
  temp.mean = colMeans(Data[,var.list])
  Temp.mean = matrix(temp.mean,ncol=length(var.list),nrow=nrow(Data),byrow=TRUE)
  Data[,var.list] = Data[,var.list] - Temp.mean
  temp.sd = apply(Data[var.list],FUN=sd,MARGIN=2)
  Temp.sd = matrix(temp.sd,ncol=length(var.list),nrow=nrow(Data),byrow=TRUE)
  Data[var.list] = Data[,var.list] / Temp.sd
  rm(list=c("temp.mean","Temp.mean","temp.sd","Temp.sd"))
  
  
  #########
  p=max_var
  xx=Data[,1:p]
  xa=Data[,1:(p+1)]
  yy=Data[,(p+2)]
  aa = Data[,(p+1)]
  
  #print(p)
  # estimate outcome model
  y.form = formula(paste("Y~A+",paste(var.list,collapse="+")))
  lm.Y = lm(y.form,data=Data) #(with OLS)
  betaXY = coef(lm.Y)[var.list]
  #enet.y = glmnet(as.matrix(xa), as.matrix(yy), alpha = 0,  data = Data)
  #e.betaXY = coef(enet.y, s=1.00)
  #e.betaXY[1:10]
  
  #########Outcome adaptive enet
  #pen = matrix(c(.1, 5, 7, 0.1, 0.1), 1, 5)
  gamma = 5
  #pen = 1/(abs(betaXY)+(1/500))^gamma
  pen = 1/(abs(betaXY))^gamma
  pen[which(!is.finite(pen))] <- 10000000000
  #Perform cross validation
  K.cv.fit = cv.glmnet(as.matrix(xx), as.matrix(aa), penalty.factor = pen, type.measure = "mse",
                       nfolds = 5, gamma = c(0, 0.25, 0.5, 0.75, 1),
                       relax = TRUE, family="binomial")
  cv.alpha = K.cv.fit$relaxed
  alpha.opt = cv.alpha$gamma.1se
  print(alpha.opt)
  #lambda = K.cv.fit$lambda.1se,
  fit2=glmnet(as.matrix(xx), as.matrix(aa), alpha = alpha.opt, lambda = K.cv.fit$lambda.1se, penalty.factor = pen, family="binomial")

  beta_ps_raw = as.matrix(coef(fit2))
  #beta_ps_raw = as.matrix(coef(fit2, s = 0.02))
  beta_ps_allvar = as.matrix(beta_ps_raw[2:(p+1)])

  beta_ps_non_zero = row(beta_ps_allvar)[which(abs(beta_ps_allvar) >= 0.001)]
  sel.var.index  = beta_ps_non_zero

  #Storing the Indexes of selected variables
  select.var.list.onet[itr, 1:length(sel.var.index)] = sel.var.index

  #Storing the total number of variables selected
  total.num.var.onet [itr] = length(sel.var.index)

  xx_true = xx[,beta_ps_non_zero]

  true.var.list = names(xx_true)
  treat.form = formula(paste("A~",paste(true.var.list,collapse="+")))
  mm = matchit(treat.form, data = Data, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "both")

  effect.form = formula(paste("Y~A+",paste(true.var.list,collapse="+")))
  fit.effect = lm(effect.form, data = match.data(mm))
  final.coef = fit.effect$coefficients
  att.adnet[itr] = final.coef[2]

  print("ENet")
  print(att.adnet[itr])
  
  ######### Outcome Adaptive LASSO
  K.cv.fit = cv.glmnet(as.matrix(xx), as.matrix(aa), penalty.factor = pen, type.measure = "mse",
                       nfolds = 5, gamma = 1,
                       relax = FALSE, family="binomial")
  fit2=glmnet(as.matrix(xx),as.matrix(aa), alpha = 1, penalty.factor = pen, lambda = K.cv.fit$lambda.1se, family="binomial")

  beta_ps_raw = as.matrix(coef(fit2))
  beta_ps_allvar = as.matrix(beta_ps_raw[2:(p+1),])
  
  beta_ps_non_zero = row(beta_ps_allvar)[which(abs(beta_ps_allvar) >= 0.001)]
  #beta_ps_non_zero = row(beta_ps_allvar)[which(!beta_ps_allvar == 0)]
  sel.var.index  = beta_ps_non_zero

  #Storing the Indexes of selected variables
  select.var.list.adlas[itr, 1:length(sel.var.index)] = sel.var.index

  #Storing the totoal number of variables selected
  total.num.var.adlas [itr] = length(sel.var.index)

  xx_true = xx[,beta_ps_non_zero]

  true.var.list = names(xx_true)
  treat.form = formula(paste("A~",paste(true.var.list,collapse="+")))
  mm = matchit(treat.form, data = Data, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "both")

  effect.form = formula(paste("Y~A+",paste(true.var.list,collapse="+")))
  fit.effect = lm(effect.form, data = match.data(mm))
  final.coef = fit.effect$coefficients
  att.adlas[itr] = final.coef[2]
  
  print("lasso")
  print(att.adlas[itr])

  #########Target model
  target.form = formula(paste("A~",paste(target.var.list,collapse="+")))
  mm = matchit(target.form, data = Data, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "both")

  effect.form = formula(paste("Y~A+",paste(target.var.list,collapse="+")))
  fit.effect = lm(effect.form, data = match.data(mm))
  coeftest(fit.effect, vcov. = vcovCL)["A",,drop=FALSE]
  final.coef = fit.effect$coefficients
  att.target[itr] = final.coef[2]

  print("target")
  print(att.target[itr])



  #########Potential confounder model
  potconf.form = formula(paste("A~",paste(potconf.var.list,collapse="+")))
  mm = matchit(potconf.form, data = Data, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "both")


  effect.form = formula(paste("Y~A+",paste(potconf.var.list,collapse="+")))
  fit.effect = lm(effect.form, data = match.data(mm))
  #coeftest(fit.effect, vcov. = vcovCL)["A",,drop=FALSE]
  final.coef = fit.effect$coefficients
  att.potconf[itr] = final.coef[2]



  #########Confounder model
  conf.form = formula(paste("A~",paste(conf.var.list,collapse="+")))
  mm = matchit(conf.form, data = Data, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "both")


  effect.form = formula(paste("Y~A+",paste(conf.var.list,collapse="+")))
  fit.effect = lm(effect.form, data = match.data(mm))
  #coeftest(fit.effect, vcov. = vcovCL)["A",,drop=FALSE]
  final.coef = fit.effect$coefficients
  att.conf[itr] = final.coef[2]

  print("Conf")

  # #########Boruta_T
  # bort = Boruta(xx,aa,pValue = 0.10)
  # 
  # bort_df = data.frame(bort$finalDecision)
  # bort_df$Index = 1:dim.data.frame(bort_df)[1]
  # bort_df = bort_df[bort_df$bort.finalDecision=="Confirmed",]
  # sel.var.index = bort_df$Index
  # bort.cov = Data[ , sel.var.index]
  # 
  # n_bort = length(sel.var.index)
  # bort.cov = as.matrix(bort.cov)
  # bor.data = data.frame(bort.cov, aa, yy)
  # mm = matchit(aa ~ bort.cov, data = bor.data, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "both")
  # 
  # matched_data = (match.data(mm))
  # 
  # df_b = matched_data[,1:(n_bort + 2)]
  # fit.effect = lm(yy ~ aa + . , data = df_b)
  # final.coef = fit.effect$coefficients
  # att.Boruta_T[itr] = final.coef[2]
  # 
  # #Storing the Indexes of selected variables
  # select.var.list.bort[itr, 1:length(sel.var.index)] = sel.var.index
  # 
  # #Storing the total number of variables selected
  # total.num.var.bort [itr] = n_bort
  # 
  # print("Boruta T")
  # print(att.Boruta_T[itr])
  # 
  # 
  # #########Boruta_Y
  # bory = Boruta(xx,yy,pValue = 0.10)
  # 
  # bory_df = data.frame(bory$finalDecision)
  # bory_df$Index = 1:dim.data.frame(bory_df)[1]
  # bory_df = bory_df[bory_df$bory.finalDecision=="Confirmed",]
  # sel.var.index = bory_df$Index
  # bory.cov = Data[ , sel.var.index]
  # 
  # n_bory = length(sel.var.index)
  # bory.cov = as.matrix(bory.cov)
  # bor.y.data = data.frame(bory.cov, aa, yy)
  # mm = matchit(aa ~ bory.cov, data = bor.y.data, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "both")
  # 
  # matched_data = (match.data(mm))
  # 
  # df_by = matched_data[,1:(n_bory + 2)]
  # fit.effect = lm(yy ~ aa + . , data = df_by)
  # final.coef = fit.effect$coefficients
  # att.Boruta_Y[itr] = final.coef[2]
  # 
  # #Storing the Indexes of selected variables
  # select.var.list.bory[itr, 1:length(sel.var.index)] = sel.var.index
  # 
  # #Storing the total number of variables selected
  # total.num.var.bory [itr] = n_bory
  # 
  # print("Boruta Y")
  # print(att.Boruta_Y[itr])

  # ###############BACR
  # bacr = bac(data=Data, exposure="A", outcome="Y", confounders=paste(var.list, sep = ""),
  #            interactors=NULL, familyX="binomial", familyY="gaussian", omega=Inf,
  #            num_its=200, burnM=1, burnB=1, thin=1)
  # bacr_df = data.frame(bacr$models)
  # bacr_df = bacr_df[1,1:dim(xx)[2]]
  # sel.var.index = which(bacr_df>0)
  # bacr.cov = Data[ , sel.var.index]
  # 
  # n_bacr = length(sel.var.index)
  # bacr.cov = as.matrix(bacr.cov)
  # bacr.data = data.frame((bacr.cov), aa, yy)
  # mm = matchit(aa ~ bacr.cov, data = bacr.data, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "both")
  # 
  # matched_data = (match.data(mm))
  # 
  # df_b = matched_data[,1:(n_bacr + 2)]
  # fit.effect = lm(yy ~ aa + ., data = df_b)
  # final.coef = fit.effect$coefficients
  # att.BACR[itr] = final.coef[2]
  # 
  # #Storing the Indexes of selected variables
  # select.var.list.bacr[itr, 1:length(sel.var.index)] = sel.var.index
  # 
  # #Storing the total number of variables selected
  # total.num.var.bacr [itr] = n_bacr
  # 
  # #att.BACR[itr] = mean(bacr$ACE)
  # 
  # print("BACR")
  # print(att.BACR[itr])


########################BCEE
  xxx = unname(as.matrix(xx))
  bcee = GBCEE(as.matrix(aa), as.matrix(yy), xxx, omega = 300*sqrt(n), niter = 5000, family.X = "binomial", family.Y = "gaussian", X1 = 1, X0 = 0, priorX = NA, priorY = NA, maxsize = NA, OR = 20, truncation = c(0.01, 0.99), var.comp = "asymptotic", B = 200)

  #Considering the Posterior distribution of the outcome model
  bcee_df = data.frame(bcee$models.Y)

  #Selecting the top 5 models as they includes 80% of the posterior probability
  freq_var_sel = colSums(bcee_df[1:5,])

  #Select the variables who appear in at least 50% of the models
  sel.var.index = which((freq_var_sel[1:100] > 2))
  bcee.cov = Data[, sel.var.index]

  n_bcee = length(sel.var.index)
  bcee.cov = as.matrix(bcee.cov)
  df_bcee = data.frame(bcee.cov, aa, yy)
  mm = matchit(aa ~ bcee.cov, data = df_bcee, method = "nearest", distance = "glm", ratio = 1, caliper = .25, link = "logit", estimand = "ATT", replace = FALSE, discard = "both")

  matched_data = (match.data(mm))

  df_b = matched_data[,1:(n_bcee + 2)]
  fit.effect = lm(yy ~ aa + ., data = df_b)
  final.coef = fit.effect$coefficients
  att.BCEE[itr] = final.coef[2]

  #Storing the Indexes of selected variables
  select.var.list.bcee[itr, 1:length(sel.var.index)] = sel.var.index

  #Storing the total number of variables selected
  total.num.var.bcee [itr] = n_bcee

  print("BCEE")
  print(att.BCEE[itr])
  
  print(itr)
  
  ###################### Writing att data frame
  #df_att = data.frame(att.target, att.conf, att.potconf, att.adnet, att.adlas, att.Boruta_T, att.Boruta_Y, att.BACR, att.BCEE)
  ATT = c(att.target [itr], att.conf[itr], att.potconf[itr], att.adnet[itr], att.adlas[itr], att.Boruta_T[itr], att.Boruta_Y[itr], att.BACR[itr], att.BCEE[itr])
  df_att = rbind(df_att,ATT)
  write.csv(df_att, "att.cor_0.0")
  
  
  ##################### Writing total number of variable selected 
  #df_sel.var = data.frame(total.num.var.onet, total.num.var.adlas, total.num.var.bort, total.num.var.bory, total.num.var.bacr, total.num.var.bcee)
  SV = c(total.num.var.onet [itr], total.num.var.adlas[itr], total.num.var.bort[itr], total.num.var.bory[itr], total.num.var.bacr[itr], total.num.var.bcee[itr])
  df_sel.var = rbind(df_sel.var, SV)
  write.csv(df_sel.var, "sel.var.cor_0.0")
  
  
  #################### Writing the selected variable indexes for each iteration
  df_sel.var.list.onet = rbind(df_sel.var.list.onet, select.var.list.onet[itr,])
  write.csv(df_sel.var.list.onet, "onet.sel.var.list.cor_0.0")
  
  df_sel.var.list.adlas = rbind(df_sel.var.list.adlas, select.var.list.adlas[itr,])
  write.csv(df_sel.var.list.adlas, "adlas.sel.var.list.cor_0.0")
  
  df_sel.var.list.bort = rbind(df_sel.var.list.bort, select.var.list.bort[itr,])
  write.csv(df_sel.var.list.bort, "bort.sel.var.list.cor_0.0")
  
  df_sel.var.list.bory = rbind(df_sel.var.list.bory, select.var.list.bory[itr,])
  write.csv(df_sel.var.list.bory, "bory.sel.var.list.cor_0.0")
  
  df_sel.var.list.bacr = rbind(df_sel.var.list.bacr, select.var.list.bacr[itr,])
  write.csv(df_sel.var.list.bacr, "bacr.sel.var.list.cor_0.0")
  
  df_sel.var.list.bcee = rbind(df_sel.var.list.bcee, select.var.list.bcee[itr,])
  write.csv(df_sel.var.list.bcee, "bcee.sel.var.list.cor_0.0")
  
}

 
 
 ############################
png(filename="att_cor_0.0_var_100_gdVar_20.png")
boxplot(att.target, att.conf, att.potconf, att.adnet, att.adlas, att.Boruta_T, att.Boruta_Y, att.BACR, att.BCEE,  
        names = c("Targ", "Conf", "Pot.Conf", "ONet", "OLas", "Bor(T)", "bor(Y)", "BACR", "BCEE"), ylim = c(-0.2,1.3))
abline(h=bA, lty = 2)
dev.off()


## Plotting the proportions of number of var selected
tab.onet = table(df_sel.var$total.num.var.onet)
tab.adlas = table(df_sel.var$total.num.var.adlas)
tab.bort = table(df_sel.var$total.num.var.bort)
tab.bory = table(df_sel.var$total.num.var.bory)
tab.bacr = table(df_sel.var$total.num.var.bacr)
tab.bcee = table(df_sel.var$total.num.var.bcee)
num.var = c(1:max_var)
freq = zeros(7,max_var)
#freq[1:(min(total.num.var.onet)-1)] = 1
freq[1,(min(df_sel.var$total.num.var.onet)):(min(df_sel.var$total.num.var.onet)+dim(tab.onet)-1)]=tab.onet/itr_max
freq[2,(min(df_sel.var$total.num.var.adlas)):(min(df_sel.var$total.num.var.adlas)+dim(tab.adlas)-1)]=tab.adlas/itr_max
freq[3,(min(df_sel.var$total.num.var.bort)):(min(df_sel.var$total.num.var.bort)+dim(tab.bort)-1)]=tab.bort/itr_max
freq[4,(min(df_sel.var$total.num.var.bory)):(min(df_sel.var$total.num.var.bory)+dim(tab.bory)-1)]=tab.bory/itr_max
freq[5,(min(df_sel.var$total.num.var.bacr)):(min(df_sel.var$total.num.var.bacr)+dim(tab.bacr)-1)]=tab.bacr/itr_max
freq[6,(min(df_sel.var$total.num.var.bcee)):(min(df_sel.var$total.num.var.bcee)+dim(tab.bcee)-1)]=tab.bcee/itr_max
freq[7,pC+pP] = 1

freq = rowCumsums(freq)
  
freq.tr = t(freq)
nn = ncol(freq.tr)

png(filename="prop_cor_0.0_var_100_gdVar_20.png")

matplot(freq.tr, type='l', xlim = c(10,50), lty = c(1, 2, 3, 4, 5, 6, 5), col=seq_len(nn), lwd = 2, 
        xlab = "Number of Covariates", ylab =  "Cumulative Proportion of times covariates selected")
legend(40,0.98, legend = c("ONet", "OLas", "Bor(T)", "Bor(Y)", "BACR", "BCEE", "Target"), 
       col=seq_len(nn),cex=0.9,fill=seq_len(nn), lty = c(1, 2, 3, 4, 5, 6, 5))

dev.off()



