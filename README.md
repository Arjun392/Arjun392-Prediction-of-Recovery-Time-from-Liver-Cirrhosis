# Prediction-of-Recovery-Time-from-Liver-Cirrhosis
M.Sc. Project ,The University of Burdwan
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("broom")
#install.packages("dplyr")

library(ggplot2)
library(ggpubr)
library(broom)
library(dplyr)

cirrhosis_prediction<-read.csv("D:/DataSets/Cirrohis prediction/cirrhosis prediction.csv")
nrow(cirrhosis_prediction)
ncol(cirrhosis_prediction)
summary(cirrhosis_prediction)
str(cirrhosis_prediction)
#cirrhosis_prediction2<-na.omit(cirrhosis_prediction)
#summary(cirrhosis_prediction2)
#str(cirrhosis_prediction2)
#View(cirrhosis_prediction2)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Bilirubin)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Cholesterol)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Albumin)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Copper)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Alk_Phos)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$SGOT)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Prothrombin)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Platelets)
cor(cirrhosis_prediction$Albumin,cirrhosis_prediction$Tryglicerides)

cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Bilirubin)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Cholesterol)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Albumin)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Copper)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Alk_Phos)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$SGOT)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Prothrombin)
cor(cirrhosis_prediction$N_Days,cirrhosis_prediction$Platelets)
cor(cirrhosis_prediction$Albumin,cirrhosis_prediction$Tryglicerides)






sf<-table(cirrhosis_prediction$Status)
sf
Drugf<-table(cirrhosis_prediction$Drug)
Drugf
Sexf<-table(cirrhosis_prediction$Sex)
Sexf
Ascitesy<-table(cirrhosis_prediction$Ascites)
Ascitesy
Hepatof<-table(cirrhosis_prediction$Hepatomegaly)
Hepatof
Spiders<-table(cirrhosis_prediction$Spiders)
Spiders
Edm<-table(cirrhosis_prediction$Edema)
Edm

cirrhosis_prediction$Status=factor(cirrhosis_prediction$Status,levels<-c('D','C','CL'),levels<-c(1,0,0)) #Dummy 'Death' to'1','Censored' to '0','CL_censored due to liver tx' to '0'
#View(cirrhosis_prediction2)
cirrhosis_prediction$Drug=factor(cirrhosis_prediction$Drug,levels<-c('D-penicillamine','Placebo'),levels<-c(1,0)) #dummy 'Penicilliamine' to '1',placebo to '0'
cirrhosis_prediction$Sex=factor(cirrhosis_prediction$Sex,levels<-c('F','M'),levels<-c(1,2)) #male to '1', female to '2'
cirrhosis_prediction$Ascites=factor(cirrhosis_prediction$Ascites,levels<-c('Y','N'),levels<-c(1,0)) # 'Y' to '1','N' to '0'
cirrhosis_prediction$Hepatomegaly=factor(cirrhosis_prediction$Hepatomegaly,levels<-c('Y','N'),levels<-c(1,0)) 
cirrhosis_prediction$Spiders=factor(cirrhosis_prediction$Spiders,levels<-c('Y','N'),levels<-c(1,0))
cirrhosis_prediction$Edema=factor(cirrhosis_prediction$Edema,levels<-c('Y','N','S'),levels<-c(1,0,3)) #N to no edima & diuretic,Y present both,S only edima

View(cirrhosis_prediction)


cirrhosis_prediction$Cholesterol[is.na(cirrhosis_prediction$Cholesterol)]<-median(cirrhosis_prediction$Cholesterol,na.rm = TRUE)
cirrhosis_prediction$Copper[is.na(cirrhosis_prediction$Copper)]<-median(cirrhosis_prediction$Copper,na.rm = TRUE)

cirrhosis_prediction$Alk_Phos[is.na(cirrhosis_prediction$Alk_Phos)]<-median(cirrhosis_prediction$Alk_Phos,na.rm = TRUE)
cirrhosis_prediction$SGOT[is.na(cirrhosis_prediction$SGOT)]<-median(cirrhosis_prediction$SGOT,na.rm = TRUE)

cirrhosis_prediction$Cholesterol[is.na(cirrhosis_prediction$Cholesterol)]<-median(cirrhosis_prediction$Cholesterol,na.rm = TRUE)
cirrhosis_prediction$Tryglicerides[is.na(cirrhosis_prediction$Tryglicerides)]<-median(cirrhosis_prediction$Tryglicerides,na.rm = TRUE)

cirrhosis_prediction$Platelets[is.na(cirrhosis_prediction$Platelets)]<-median(cirrhosis_prediction$Platelets,na.rm = TRUE)
cirrhosis_prediction$Prothrombin[is.na(cirrhosis_prediction$Prothrombin)]<-median(cirrhosis_prediction$Prothrombin,na.rm = TRUE)


cirrhosis_prediction$Stage[is.na(cirrhosis_prediction$Stage)]<-median(cirrhosis_prediction$Stage,na.rm = TRUE)




#heteroscadasticity check
#lmMod<-lm(N_Days ~Status+Drug+Age+Sex+Ascites+Hepatomegaly+Spiders+Edema+Bilirubin+Cholesterol+Albumin+Copper+Alk_Phos+SGOT+Tryglicerides+Platelets+Prothrombin,data = cirrhosis_prediction)
#summary(lmMod)
#par(mfrow=c(2,2))
#plot(lmMod)
#par(mfrow=c(1,1))
#plot(lmMod)
#install.packages("MASS")
#library(MASS)
#box-cox transformation
#bc<-boxcox(lmMod)

#(lambda<-bc$x[which.max(bc$y)])
#new_lm<-lm(((N_Days^0.5-1)/0.5)~.,data = cirrhosis_prediction)
#op<-par(pty ="s" , mfrow<-c(1,2))
#plot(new_lm)
# Breusch-Pagan test
#install.packages("lmtest")
#library(lmtest)
#lmtest::bptest(lmMod)  
#studentized Breusch-Pagan test



#multicolinearity check
#install.packages("car")
#library(car)
#vif(new_lm)

#glm.mod<-glm(N_Days~.,data = cirrhosis_prediction,family=poisson())
#summary(glm.mod)
#par(mfrow=c(1,1))
#plot(glm.mod)

#box cox
#library(MASS)
#bcg<-boxcox(glm.mod)
#(lambda<-bcg$x[which.max(bcg$y)])
#new_glm<-glm(((N_Days^lambda-1)/lambda)~.,data = cirrhosis_prediction)
#op<-par(pty ="s" , mfrow<-c(1,2))
#plot(new_glm)

#library(MASS)

#new_glm1<-glm(((N_Days^0.8-1)/0.8)~.,data = cirrhosis_prediction)
#op<-par(pty ="s" , mfrow<-c(1,2))
#plot(new_glm1)


# Cox proportional hazard model

#install.packages("survival")
#install.packages("survminer")
library(survival)
library(survminer)
# Load the library.
library("survival")

# Create the survival object. 
sfit<-survfit(Surv(cirrhosis_prediction$N_Days,cirrhosis_prediction$Status==1  )~1,data = cirrhosis_prediction)
summary(sfit)

sfit<-survfit(Surv(cirrhosis_prediction$N_Days,cirrhosis_prediction$Status==0  )~1,data = cirrhosis_prediction)
summary(sfit)



#fit by sex
sfit<-survfit(Surv(cirrhosis_prediction$N_Days,cirrhosis_prediction$Status==1  )~Sex,data = cirrhosis_prediction)
summary(sfit)

#km plot

sfit<-survfit(Surv(cirrhosis_prediction$N_Days,cirrhosis_prediction$Status==1  )~Sex,data = cirrhosis_prediction)
plot(sfit)

library(survminer)
ggsurvplot(sfit)

#ggsurvplot(sfit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           #legend.labs=c("Male", "Female"), legend.title="Sex",  
          # palette=c("dodgerblue2", "orchid2"), 
           #title="Kaplan-Meier Curve for Cirrhosis Prediction Survival", 
           #risk.table.height=.15)

ggsurvplot(sfit, data = cirrhosis_prediction,
           conf.int = TRUE,
           pval = TRUE,
           fun = "pct",
           risk.table = TRUE,
           size = 1,
           linetype = "strata",
           palette = c("dodgerblue2", "orchid2"),title="Kaplan-Meier Curve for Cirrhosis Prediction Survival",
           legend = "bottom",
           legend.title = "Sex",
           legend.labs = c("Male",
                           "Female"))

#sfitc<-survfit(Surv(cirrhosis_prediction$N_Days,cirrhosis_prediction$Status==1  )~Drug+Age+Sex+Ascites+Hepatomegaly+Spiders+Edema+Bilirubin+Cholesterol+Albumin+Copper+Alk_Phos+SGOT+Tryglicerides+Platelets+Prothrombin+Stage,data = cirrhosis_prediction)
#plot(sfitc)
#summary(sfitc)

library(survival)
coxm<-coxph(Surv(cirrhosis_prediction$N_Days,cirrhosis_prediction$Status)~Drug+Age+Sex+Ascites+Hepatomegaly+Spiders+Edema+Bilirubin+Cholesterol+Albumin+Copper+Alk_Phos+SGOT+Tryglicerides+Platelets+Prothrombin+Stage,id=ID,data = cirrhosis_prediction)

library(car)
vif(coxm)

summary(coxm)
ggforest(coxm,data = cirrhosis_prediction)
#Diagnostics of Cox Model
ftestcox<-cox.zph(coxm)
ftestcox

library(survminer)
ggcoxzph(ftestcox)


#library(survival)
#library(survminer)
#fitcx<-coxph(Surv(cirrhosis_prediction$N_Days,cirrhosis_prediction$Status)~cirrhosis$Sex+cirrhosis$Age,id=ID,data = cirrhosis_prediction)
#ggcoxdiagnostics(fitcx,type = "deviance",ox.scale = "linear.predictions")
#ggcoxdiagnostics(fitcx,type = "schoenfeld",ox.scale = "cirrhosis$N_Days")



#Summary of Cox Model


#cirrhosis_prediction$Sex <- ifelse(cirrhosis_prediction$Sex == 2,
                  # "Male", "Female")
#lo<-coxph(Surv(cirrhosis_prediction$N_Days,cirrhosis_prediction$Status)~Sex+Age+strata(Sex),id=ID,data = cirrhosis_prediction)

#ggadjustedcurves(coxm1,data = cirrhosis_prediction)


#AFT model

library(dplyr)
library(ggplot2)
library(knitr)
library(ciTools)
library(here)
set.seed(20180925)
kable(head(cirrhosis_prediction))
ggplot(cirrhosis_prediction,aes(x=Bilirubin+Albumin+Copper+Alk_Phos+Prothrombin,N_Days))+geom_point(aes(color = factor(Status)))+ggtitle("censored obs. in red")+theme_bw()
ggplot(cirrhosis_prediction,aes(x=Drug,N_Days))+geom_point(aes(color = factor(Status)))+ggtitle("censored obs. in red")+theme_bw()
cirrho01<-na.omit(cirrhosis_prediction)
f11<-survreg(Surv(N_Days,Status==1)~.,data = cirrho01)

summary(f11)
f11

with_ints <- ciTools::add_ci(cirrho01,f11, names = c("lcb", "ucb")) %>%
  ciTools::add_pi(f11, names = c("lpb", "upb"))
kable(head(with_ints))

ggplot(with_ints, aes(x = Bilirubin+Albumin+Copper+Alk_Phos+Prothrombin, y = N_Days)) +
  geom_point(aes(color = Status)) +
  facet_wrap(~Status)+
  theme_bw() +
  ggtitle("Model fit with 95% CIs and PIs",
          "solid line = mean, dotted line = median") +
  geom_line(aes(y = mean_pred), linetype = 1) +
  geom_line(aes(y = median_pred), linetype = 2) +
  geom_ribbon(aes(ymin = lcb, ymax = ucb), alpha = 0.5) +
  geom_ribbon(aes(ymin = lpb, ymax = upb), alpha = 0.1)

probs <- ciTools::add_probs(cirrho01, f11, q = 4000,
                            name = c("prob", "lcb", "ucb"),
                            comparison = ">")


ggplot(probs, aes(x = Bilirubin+Albumin+Copper+Alk_Phos+Prothrombin, y = prob)) +
  ggtitle("Estimated prob. of avg. time lasting longer than 1000 days") +
  ylim(c(0,1)) +
  facet_wrap(~Status)+
  theme_bw() +
  geom_line(aes(y = prob)) +
  geom_ribbon(aes(ymin = lcb, ymax = ucb), alpha = 0.5)

quants <- ciTools::add_quantile(cirrho01, f11, p = 0.90,
                                name = c("quant", "lcb", "ucb"))

ggplot(quants, aes(x =Bilirubin+Albumin+Copper+Alk_Phos+Prothrombin , y = N_Days)) +
  geom_point(aes(color = Status)) +
  ggtitle("Estimated 90th percentile of condtional failure distribution, with CI") +
  facet_wrap(~Status)+
  theme_bw() +
  geom_line(aes(y = quant)) +
  geom_ribbon(aes(ymin = lcb, ymax = ucb), alpha = 0.5)



library(flexsurv)

# Define a function for analysing one simulated dataset
sim_run <- function() {
  # Create a data frame with the subject IDs and treatment covariate
  cov <- data.frame(ID = 1:2000,
                    trt = rbinom(2000, 1, 0.5))
  
  # Simulate the event times
  dat <- simsurv(lambdas = 0.1, 
                 gammas = 1.5, 
                 betas = c(trt = -0.5), 
                 x = cov, 
                 maxt = 5)
  
  # Merge the simulated event times onto covariate data frame
  dat <- merge(cov, dat)
  
  # Fit a Weibull proportional hazards model
  mod <- flexsurv::flexsurvspline(Surv(eventtime, status) ~ trt, data = dat)
  
  # Obtain estimates, standard errors and 95% CI limits
  est <- mod$coefficients[["trt"]]
  ses <- sqrt(diag(mod$cov))[["trt"]]
  cil <- est + qnorm(.025) * ses
  ciu <- est + qnorm(.975) * ses
  
  # Return bias and coverage indicator for treatment effect
  c(bias = est - (-0.5), 
    coverage = ((-0.5 > cil) && (-0.5 < ciu)))
}

# Set seed for simulations
set.seed(908070)

# Perform 100 replicates in simulation study
rowMeans(replicate(100, sim_run()))

data.class(cirrhosis_prediction)

# Fit the Weibull survival model
mod_weib <- flexsurvspline(Surv(N_Days,Status==1) ~Drug+Bilirubin+Prothrombin+Copper
                      ,data = cirrhosis_prediction, k = 0)

# Fit the flexible parametric survival model
mod_flex <- flexsurv::flexsurvspline(Surv(N_Days,Status==1) ~ Drug+Bilirubin+Prothrombin+Copper, 
                                     data = cirrhosis_prediction, k = 3)


par(mfrow = c(1,2), cex = 0.85) # graphics parameters
plot(mod_weib,
     main = "Weibull model",
     ylab = "Survival probability",
     xlab = "Time") 
plot(mod_flex,
     main = "Flexible parametric model",
     ylab = "Survival probability",
     xlab = "Time")



# Fit the model to the brcancer dataset to obtain the "true"
# parameter values that will be used in our simulation study
true_mod <- flexsurv::flexsurvspline(Surv(N_Days,Status==1) ~ Stage, 
                                     data = cirrhosis_prediction, k = 3)

# Define a function to generate one simulated dataset, fit
# our two models (Weibull and flexible) to the simulated data
# and then return the bias in the estimated effect of hormone
# therapy under each fitted model
sim_run <- function(true_mod) {
  # Create a data frame with the subject IDs and treatment covariate
  cov <- data.frame(ID = 1:2000, Stage = rbinom(2000, 1, 0.5))
  
  # Simulate the event times
  dat <- simsurv(betas = true_mod$coefficients, # "true" parameter values
                 x = cov,                   # covariate data for 200 individuals
                 knots = true_mod$knots,    # knot locations for splines
                 logcumhazard = logcumhazard,  # definition of log cum hazard
                 maxt = NULL,               # no right-censoring
                 interval = c(1E-8,100000)) # interval for root finding
  
  # Merge the simulated event times onto covariate data frame
  dat <- merge(cov, cirrhosis_prediction)
  
  # Fit a Weibull proportional hazards model
  weib_mod <- flexsurv::flexsurvspline(Surv(N_Days,Status==1) ~ Stage, 
                                       data = cirrhosis_prediction, k = 0)
  
  # Fit a flexible parametric proportional hazards model
  flex_mod <- flexsurv::flexsurvspline(Surv(N_Days,Status==1) ~ Stage, 
                                       data = cirrhosis_prediction, k = 3)
  
  # Obtain estimates, standard errors and 95% CI limits for hormone effect
  true_loghr <- true_mod$coefficients[["Stage"]]
  weib_loghr <- weib_mod$coefficients[["Stage"]]
  flex_loghr <- flex_mod$coefficients[["Stage"]]
  
  # Return bias and coverage indicator for hormone effect
  c(weib_bias = weib_loghr - true_loghr, 
    flex_bias = flex_loghr - true_loghr)
}

# Set a seed for the simulations
set.seed(543543)

# Perform the simulation study using 100 replicates
rowMeans(replicate(100, sim_run(true_mod = true_mod)))





survfitw<-survreg(Surv(N_Days,Status==0)~.,dist = "weibull",data = cirrhosis_prediction)
summary(survfitw)

survfitw
survfite<-survreg(Surv(N_Days,Status==1)~.,dist = "exponential",data = cirrhosis_prediction)
summary(survfite)
survfitln<-survreg(Surv(N_Days,Status==1)~.,dist = "lognormal",data = cirrhosis_prediction)
summary(survfitln)

survfitlg<-survreg(Surv(N_Days,Status==1)~.,dist = "loglogistic",data = cirrhosis_prediction)
summary(survfitlg)
