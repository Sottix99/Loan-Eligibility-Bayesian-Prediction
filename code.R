library(ggplot2)
library(ggExtra)
library(psych)
library(lessR)
library(R2jags)
library(arm)
library(questionr)
library(ggmcmc)
library(coda)
library(LaplacesDemon)
library(runjags)
library(highcharter)
set.seed(123)
loan<-read.csv("Loan_eligibility.csv")
loan$Income<-loan$ApplicantIncome + loan$CoapplicantIncome

data<-data.frame(loan$Income, loan$Credit_History, loan$LoanAmount,  loan$Education, loan$Loan_Status)


names(data)<-c("Income", "Credit_History", "Amount", "Education", "Output")

# Feature engeenering

data<-na.omit(data)
head(data)

data$Output<-ifelse(data$Output=="N",0,1)
data$Education<-ifelse(data$Education=="Not Graduate",0,1)
# ADDONS
# INVESTIGATING

loan$Output<-ifelse(loan$Loan_Status =="N",0,1)
loan$Education<-ifelse(loan$Education=="Not Graduate",0,1)
loan$Married<-ifelse(loan$Married=="No",0,1)
loan$Gender<-ifelse(loan$Gender=="Female",0,1)
Investigate<-loan[which(loan$Output==1 & loan$Credit_History == 0),]
Investigate<-Investigate[-c(13,15,11,10,8,7,1)]
Investigate
summary(Investigate)

hist(Investigate$Income,breaks = "Freedman-Diaconis")
hist(Investigate$LoanAmount,breaks = "Freedman-Diaconis")




N<-length(train$Output)

first_model <- function(){
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    logit(p[i]) <-  beta0  + beta2*x2[i] 
  }
  
  beta0 ~ dnorm(0, 1.0E-6)
  beta1 ~ dnorm(0, 1.0E-6)
  beta2 ~ dnorm(0, 1.0E-6)
  beta3 ~ dnorm(0, 1.0E-6)
  beta4 ~ dnorm(0, 1.0E-6)
  
}

data.jags <- list("y" = train$Output, "N" = N,
                   "x2" = train$Credit_History)

mod.params <- c("beta0", "beta2")

mod_11<- jags(data = data.jags,                                                   
              model.file = first_model,                                                                 parameters.to.save = mod.params, n.chains = 2, n.iter = 10000, n.burnin = 1000, n.thin = 10)

mod_11



### EDA ###

summary(data)


# PIE of Y
{
  pie(table(data$Output), col = c("#EE9A00","#CD3700"), labels = paste(round(prop.table(table(data$Output))*100), "%", sep = ""),main="Pie chart of Output")
text(0.2,0.3,"0",lty=2)
text(-0.2,-0.15,"1",lty=2)
}

# PIE of Credit History

{
  pie(table(data$Credit_History), col = c("#2E8B57","#473C8B"),main="Pie chart of Credit History", labels = paste(round(prop.table(table(data$Credit_History))*100), "%", sep = ""))
  text(0.5,0.3,"0",lty=2)
  text(-0.2,-0.15,"1",lty=2)
}


# PIE of Education

{
  pie(table(data$Education), col = c("#CD3278","#00C5CD"),main="Pie chart of Education", labels = paste(round(prop.table(table(data$Education))*100), "%", sep = ""))
  text(0.5,0.3,"0",lty=2)
  text(-0.2,-0.15,"1",lty=2)
}

# Income

hist(data$Income,breaks = "Freedman-Diaconis", col = "#CD96CD",main="Histogram of Income",xlab="Income",ylim=c(0,120))


# Amount

hist(data$Amount,breaks = "Freedman-Diaconis", col = "#00CD66",main="Histogram of Amount",xlab="Amount",ylim = c(0,120))


# log(income) vs log(amount)
p <- ggplot(data, aes(x = log(data$Income), y = log(data$Amount))) +
  geom_point()

s = p + xlab("Log(Income)") + ylab("Log(Amount)")  + theme_bw()

ggMarginal(s, type = "histogram", 
           xparams = list(fill = "#CD96CD"),
           yparams = list(fill = "#00CD66"))


# income vs amount
p <- ggplot(data, aes(x = data$Income, y = data$Amount)) +
  geom_point()

s = p + xlab("Income") + ylab("Amount")  + theme_bw()

ggMarginal(s, type = "histogram", 
           xparams = list(fill = "#CD96CD"),
           yparams = list(fill = "#00CD66"))



# or plot(data$Income, data$Amount,xlab="Income", ylab= "Amount",col="#8B2500",pch=16)

corr.test(data$Income, data$Amount)


# correlation among variables

corPlot(data, scale= FALSE,gr = colorRampPalette(heat.colors(10)))


# credit vs output
{
barplot(prop.table(table(data$Credit_History, data$Output)),xlab="Output",col=c("darkred", "#27408B"),ylim=c(0,0.7),main="Has the costumer obtained the loan?", names.arg = c("No", "Yes"))
box()
legend("topleft", title="Credit History", legend=c("0", "1"),
      fill=c("darkred", "#27408B"), cex=1.2)

}





##############
### MODELS ###
##############





### SPLITTING TRAIN, TEST

index= sample(c(rep(0,0.8*nrow(data)), rep(1,0.2*nrow(data))))
train= data[index == 0,]
test= data[index == 1,]


### First Model ###

N<-length(train$Output)

first_model <- function(){
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    logit(p[i]) <-  beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i]
  }
 
  beta0 ~ dnorm(0, 1.0E-6)
  beta1 ~ dnorm(0, 1.0E-6)
  beta2 ~ dnorm(0, 1.0E-6)
  beta3 ~ dnorm(0, 1.0E-6)
  beta4 ~ dnorm(0, 1.0E-6)
  
}

data.jags <- list("y" = train$Output, "N" = N,
                  "x1" = log(train$Income), "x2" = train$Credit_History, "x3" = log(train$Amount), "x4" = train$Education)

mod.params <- c("beta0", "beta1","beta2","beta3","beta4")

mod_1<- jags(data = data.jags,                                                   
                model.file = first_model,                                                                 parameters.to.save = mod.params, n.chains = 2, n.iter = 10000, n.burnin = 1000, n.thin = 10)                                 
mod_1



### MOD


N<-length(train$Output)

first_model <- function(){
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    logit(p[i]) <-  beta0  + beta2*x2[i] + beta3*x3[i] 
  }
  
  beta0 ~ dnorm(0, 1.0E-6)
  beta1 ~ dnorm(0, 1.0E-6)
  beta2 ~ dnorm(0, 1.0E-6)
  beta3 ~ dnorm(0, 1.0E-6)
  beta4 ~ dnorm(0, 1.0E-6)
  
}

data.jags <- list("y" = train$Output, "N" = N, "x2" = train$Credit_History, "x3" = log(train$Amount))

mod.params <- c("beta0","beta2","beta3")

mod_1<- jags(data = data.jags,                                                   
             model.file = first_model,                                                                 parameters.to.save = mod.params, n.chains = 2, n.iter = 10000, n.burnin = 1000, n.thin = 10)                                 
mod_1










S=ggs(as.mcmc(mod_1))

ggs_running(S, greek = TRUE)
# ggs_running(S,family="beta0")

ggs_traceplot(S)
abline(h=4)

ggs_autocorrelation(S)

ggs_crosscorrelation(S, greek = TRUE)

ggs_density(S)





# Diagnostic

raftery.diag(as.mcmc(mod_1))


a<-geweke.diag(as.mcmc(mod_1))
geweke.plot(as.mcmc(mod_1),col="red")

# computing p-value

as.numeric(unlist(a[2]))

pnorm(q=as.numeric(unlist(a[1])), lower.tail=TRUE)

2*pnorm(q=as.numeric(unlist(a[2])), lower.tail=FALSE)


gelman.diag(as.mcmc(mod_1))
gelman.plot(as.mcmc(mod_1),col="red")

heidel.diag(as.mcmc(mod_1))







# Prameters recovery

Logit_model_recovery  <- 'data{
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    logit(p[i]) <-  beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i]
  }
  }
model{
fake<-0
  
}'

N=length(train$Output)

x1 = rescale(train$Income)
x2 = train$Credit_History
x3 = rescale(train$Amount)
x4 = train$Education

beta0= mod_1$BUGSoutput$mean$beta0
beta1= mod_1$BUGSoutput$mean$beta1
beta2= mod_1$BUGSoutput$mean$beta2
beta3= mod_1$BUGSoutput$mean$beta3
beta4= mod_1$BUGSoutput$mean$beta4



recovery <- list(N=N,  x1=X1, x2=X2, x3=X3, x4=X4, beta0=beta0, beta1=beta1, beta2=beta2, beta3=beta3, beta4=beta4)

set.seed(123)
out_rec= run.jags(Logit_model_recovery, data= recovery, monitor=c("y"),sample=1, n.chains=1, summarise=FALSE)

sim<-as.mcmc(out_rec)
end_rec <- as.vector(sim)

# pie
{
  pie(table(end_rec), col = c("#EE9A00","#CD3700"), labels = paste(round(prop.table(table(end_rec))*100), "%", sep = ""),main="Pie chart of the simulated response variable")
  text(0.2,0.3,"0",lty=2)
  text(-0.2,-0.15,"1",lty=2)
}


data_jags_recovery <- list(y=end_rec, N= length(end_rec), x1=x1, x2=x2, x3=x3, x4=x4)


param <- c("beta0", "beta1", "beta2", "beta3", "beta4")

end<-jags(data=data_jags_recovery, parameters.to.save = param, model.file = first_model, n.chains = 1, n.iter = 10000, n.burnin = 1000, n.thin = 1)

end
mod_1


# Prediction

test[,1]
test[c(1,3)] <- lapply(test[c(1,3)], function(x) c(scale(x))) # scaling for predicting better than before

# Saving the estimated beta parameters
beta_est <- list("beta0" = mod_1$BUGSoutput$summary["beta0", "mean"],         "beta1" = mod_1$BUGSoutput$summary["beta1", "mean"]
                 ,
                 "beta2" = mod_1$BUGSoutput$summary["beta2", "mean"],
                 "beta3" = mod_1$BUGSoutput$summary["beta3", "mean"],
                 "beta4" = mod_1$BUGSoutput$summary["beta4", "mean"])



# Iterating and found each predictive values
test$pred <- apply(test, 1, function(x){
  
  o <- beta_est$beta0 + beta_est$beta1*x["Income"] + beta_est$beta2*x["Credit_History"] + beta_est$beta3*x["Amount"] + beta_est$beta4*x["Education"]
  
  o_perc <- 1/(1+exp(-o))
  
  y_pred <- rbinom(n = 10000, size = 1, prob = o_perc)
  
  # get the most repeated value
  predicted <- unique(y_pred)
  predicted <- predicted[which.max(tabulate(match(y_pred, predicted)))]
  
  return(predicted)
})


conf_mtx_percent <- as.data.frame(round(prop.table(table(test$Output, test$pred)), digits = 3))

conf_mtx <- as.data.frame(round(table(test$Output, test$pred)), digits = 3)

{
hchart(conf_mtx, type = "heatmap", hcaes(x = Var1, y = Var2, value = Freq)) %>%
  hc_title(text = "The Confusion Matrix") %>%
  hc_plotOptions(
    series = list(
      borderColor = "#fcfbfa",
      borderWidth = 1,
      animation=(durtion=1000),
      dataLabels = list(enabled = TRUE)
    )) %>%
  hc_xAxis(title=list(text="Actual Values"))  %>%
  hc_yAxis(title=list(text="Predicted Values") )
}



{
  hchart(conf_mtx_percent, type = "heatmap", hcaes(x = Var1, y = Var2, value = Freq)) %>%
    hc_title(text = "The Confusion Matrix") %>%
    hc_plotOptions(
      series = list(
        borderColor = "#fcfbfa",
        borderWidth = 1,
        animation=(durtion=1000),
        dataLabels = list(enabled = TRUE)
      )) %>%
    hc_xAxis(title=list(text="Actual Values"))  %>%
    hc_yAxis(title=list(text="Predicted Values") )
}


tot<- conf_mtx$Freq[1]+conf_mtx$Freq[2]+conf_mtx$Freq[3]+conf_mtx$Freq[4]




mod1_metrics<-data.frame(NA)
mod1_metrics$Accuracy<- (conf_mtx$Freq[4]+conf_mtx$Freq[1])/(tot)
mod1_metrics$Precision<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[3])
mod1_metrics$Recall<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[2])
mod1_metrics$F1<-2*((mod1_metrics$Precision*mod1_metrics$Recall)/(mod1_metrics$Precision+ mod1_metrics$Recall))


mod1_metrics<-mod1_metrics[-1]
mod1_metrics


### Frequentist GLM ###

freq_fit=glm(Output ~ rescale(Income) + Credit_History + rescale(Amount) + Education, family=binomial(link="logit"), data=train)

data$Income
display(freq_fit)

mod_1
summary(freq_fit)

freq_fit$coefficients

# Saving the estimated beta parameters
beta_est_freq <- list("beta0" = freq_fit$coefficients[1],         "beta1" = freq_fit$coefficients[2]
                 ,
                 "beta2" = freq_fit$coefficients[3],
                 "beta3" = freq_fit$coefficients[4],
                 "beta4" = freq_fit$coefficients[5])



# Iterating and found each predictive values
test$pred_freq <- apply(test, 1, function(x){
  
  g <- beta_est_freq$beta0 + beta_est_freq$beta1*x["Income"] + beta_est_freq$beta2*x["Credit_History"] + beta_est_freq$beta3*x["Amount"] + beta_est_freq$beta4*x["Education"]
  
  g_perc <- 1/(1+exp(-g))
  
  y_predf <- rbinom(n = 10000, size = 1, prob = g_perc)
  
  # get the most repeated value
  predictedf <- unique(y_predf)
  predictedf <- predictedf[which.max(tabulate(match(y_predf, predictedf)))]
  
  return(predictedf)
})


conf_mtx_percent <- as.data.frame(round(prop.table(table(test$Output, test$pred_freq)), digits = 3))

conf_mtx <- as.data.frame(round(table(test$Output, test$pred_freq)), digits = 3)

{
  hchart(conf_mtx, type = "heatmap", hcaes(x = Var1, y = Var2, value = Freq)) %>%
    hc_title(text = "The Confusion Matrix") %>%
    hc_plotOptions(
      series = list(
        borderColor = "#fcfbfa",
        borderWidth = 1,
        animation=(durtion=1000),
        dataLabels = list(enabled = TRUE)
      )) %>%
    hc_xAxis(title=list(text="Actual Values"))  %>%
    hc_yAxis(title=list(text="Predicted Values") )
}



{
  hchart(conf_mtx_percent, type = "heatmap", hcaes(x = Var1, y = Var2, value = Freq)) %>%
    hc_title(text = "The Confusion Matrix") %>%
    hc_plotOptions(
      series = list(
        borderColor = "#fcfbfa",
        borderWidth = 1,
        animation=(durtion=1000),
        dataLabels = list(enabled = TRUE)
      )) %>%
    hc_xAxis(title=list(text="Actual Values"))  %>%
    hc_yAxis(title=list(text="Predicted Values") )
}


tot<- conf_mtx$Freq[1]+conf_mtx$Freq[2]+conf_mtx$Freq[3]+conf_mtx$Freq[4]




modf_metrics<-data.frame(NA)
modf_metrics$Accuracy<- (conf_mtx$Freq[4]+conf_mtx$Freq[1])/(tot)
modf_metrics$Precision<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[3])
modf_metrics$Recall<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[2])
modf_metrics$F1<-2*((modf_metrics$Precision*modf_metrics$Recall)/(modf_metrics$Precision+ modf_metrics$Recall))


modf_metrics<-modf_metrics[-1]
modf_metrics



















### Cloglog ###

N<-length(train$Output)

second_model <- function(){
  # Likelihood
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    cloglog(p[i]) <-  beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i]
  }
  
  beta0 ~ dnorm(0, 1.0E-6)
  beta1 ~ dnorm(0, 1.0E-6)
  beta2 ~ dnorm(0, 1.0E-6)
  beta3 ~ dnorm(0, 1.0E-6)
  beta4 ~ dnorm(0, 1.0E-6)
  
}

data2.jags <- list("y" = train$Output, "N" = N,
                  "x1" = rescale(train$Income), "x2" = train$Credit_History, "x3" = rescale(train$Amount), "x4" = train$Education)

mod.params <- c("beta0", "beta1","beta2","beta3","beta4")

set.seed(123)
mod_2<- jags(data = data2.jags,                                                   
             model.file = second_model,                                                                 parameters.to.save = mod.params, n.chains = 2, n.iter = 10000, n.burnin = 1000, n.thin = 10)  

# 1 vs 2

mod_1
mod_2


# Check

h=ggs(as.mcmc(mod_2))

ggs_running(h, greek = TRUE)
# ggs_running(S,family="beta0")

ggs_traceplot(h)
abline(h=4)

ggs_autocorrelation(h)

ggs_crosscorrelation(h, greek = TRUE)

ggs_density(h)


beta_est2 <- list("beta0" = mod_2$BUGSoutput$summary["beta0", "mean"],         "beta1" = mod_2$BUGSoutput$summary["beta1", "mean"]
                 ,
                 "beta2" = mod_2$BUGSoutput$summary["beta2", "mean"],
                 "beta3" = mod_2$BUGSoutput$summary["beta3", "mean"],
                 "beta4" = mod_2$BUGSoutput$summary["beta4", "mean"])



# Iterating and found each predictive values
test$pred2 <- apply(test, 1, function(x){
  
  py <- beta_est2$beta0 + beta_est2$beta1*x["Income"] + beta_est2$beta2*x["Credit_History"] + beta_est2$beta3*x["Amount"] + beta_est2$beta4*x["Education"]
  
  py_perc <- 1-exp(-exp(py))
  
  y_pred2 <- rbinom(n = 10000, size = 1, prob = py_perc)
  
  # get the most repeated value
  predicted2 <- unique(y_pred2)
  predicted2 <- predicted2[which.max(tabulate(match(y_pred2, predicted2)))]
  
  return(predicted2)
})


conf_mtx_percent <- as.data.frame(round(prop.table(table(test$Output, test$pred2)), digits = 3))

conf_mtx <- as.data.frame(round(table(test$Output, test$pred2)), digits = 3)

{
  hchart(conf_mtx, type = "heatmap", hcaes(x = Var1, y = Var2, value = Freq)) %>%
    hc_title(text = "The Confusion Matrix") %>%
    hc_plotOptions(
      series = list(
        borderColor = "#fcfbfa",
        borderWidth = 1,
        animation=(durtion=1000),
        dataLabels = list(enabled = TRUE)
      )) %>%
    hc_xAxis(title=list(text="Actual Values"))  %>%
    hc_yAxis(title=list(text="Predicted Values") )
}



{
  hchart(conf_mtx_percent, type = "heatmap", hcaes(x = Var1, y = Var2, value = Freq)) %>%
    hc_title(text = "The Confusion Matrix") %>%
    hc_plotOptions(
      series = list(
        borderColor = "#fcfbfa",
        borderWidth = 1,
        animation=(durtion=1000),
        dataLabels = list(enabled = TRUE)
      )) %>%
    hc_xAxis(title=list(text="Actual Values"))  %>%
    hc_yAxis(title=list(text="Predicted Values") )
}


tot<- conf_mtx$Freq[1]+conf_mtx$Freq[2]+conf_mtx$Freq[3]+conf_mtx$Freq[4]




mod2_metrics<-data.frame(NA)
mod2_metrics$Accuracy<- (conf_mtx$Freq[4]+conf_mtx$Freq[1])/(tot)
mod2_metrics$Precision<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[3])
mod2_metrics$Recall<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[2])
mod2_metrics$F1<-2*((mod2_metrics$Precision*mod2_metrics$Recall)/(mod2_metrics$Precision+ mod2_metrics$Recall))


mod2_metrics<-mod2_metrics[-1]
mod2_metrics
mod1_metrics
modf_metrics










# model with differents seeds

set.seed(1999)

### SPLITTING TRAIN, TEST

index= sample(c(rep(0,0.8*nrow(data)), rep(1,0.2*nrow(data))))
train= data[index == 0,]
test= data[index == 1,]
test[,1]
test[c(1,3)] <- lapply(test[c(1,3)], function(x) c(scale(x))) #

### First Model ###

N<-length(train$Output)

first_model <- function(){
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    logit(p[i]) <-  beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i]
  }
  
  beta0 ~ dnorm(0, 1.0E-6)
  beta1 ~ dnorm(0, 1.0E-6)
  beta2 ~ dnorm(0, 1.0E-6)
  beta3 ~ dnorm(0, 1.0E-6)
  beta4 ~ dnorm(0, 1.0E-6)
  
}

data.jags <- list("y" = train$Output, "N" = N,
                  "x1" = rescale(train$Income), "x2" = train$Credit_History, "x3" = rescale(train$Amount), "x4" = train$Education)

mod.params <- c("beta0", "beta1","beta2","beta3","beta4")

mod_11<- jags(data = data.jags,                                                   
             model.file = first_model,                                                                 parameters.to.save = mod.params, n.chains = 2, n.iter = 10000, n.burnin = 1000, n.thin = 10)



{# Saving the estimated beta parameters
  beta_est <- list("beta0" = mod_11$BUGSoutput$summary["beta0", "mean"],         "beta1" = mod_11$BUGSoutput$summary["beta1", "mean"]
                   ,
                   "beta2" = mod_11$BUGSoutput$summary["beta2", "mean"],
                   "beta3" = mod_11$BUGSoutput$summary["beta3", "mean"],
                   "beta4" = mod_11$BUGSoutput$summary["beta4", "mean"])
  
  
  
  # Iterating and found each predictive values
  test$pred <- apply(test, 1, function(x){
    
    o <- beta_est$beta0 + beta_est$beta1*x["Income"] + beta_est$beta2*x["Credit_History"] + beta_est$beta3*x["Amount"] + beta_est$beta4*x["Education"]
    
    o_perc <- 1/(1+exp(-o))
    
    y_pred <- rbinom(n = 10000, size = 1, prob = o_perc)
    
    # get the most repeated value
    predicted <- unique(y_pred)
    predicted <- predicted[which.max(tabulate(match(y_pred, predicted)))]
    
    return(predicted)
  })
  
  
  conf_mtx_percent <- as.data.frame(round(prop.table(table(test$Output, test$pred)), digits = 3))
  
  conf_mtx <- as.data.frame(round(table(test$Output, test$pred)), digits = 3)
  


  
  tot<- conf_mtx$Freq[1]+conf_mtx$Freq[2]+conf_mtx$Freq[3]+conf_mtx$Freq[4]
  
  
  
  
  mod1_metrics<-data.frame(NA)
  mod1_metrics$Accuracy<- (conf_mtx$Freq[4]+conf_mtx$Freq[1])/(tot)
  mod1_metrics$Precision<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[3])
  mod1_metrics$Recall<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[2])
  mod1_metrics$F1<-2*((mod1_metrics$Precision*mod1_metrics$Recall)/(mod1_metrics$Precision+ mod1_metrics$Recall))
  
  
  mod1_metrics<-mod1_metrics[-1]
  mod1_metrics
  raz1<-mod1_metrics$Recall
}











### Cloglog ###

N<-length(train$Output)

second_model <- function(){
  # Likelihood
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    cloglog(p[i]) <-  beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i]
  }
  
  beta0 ~ dnorm(0, 1.0E-6)
  beta1 ~ dnorm(0, 1.0E-6)
  beta2 ~ dnorm(0, 1.0E-6)
  beta3 ~ dnorm(0, 1.0E-6)
  beta4 ~ dnorm(0, 1.0E-6)
  
}

data2.jags <- list("y" = train$Output, "N" = N,
                   "x1" = rescale(train$Income), "x2" = train$Credit_History, "x3" = rescale(train$Amount), "x4" = train$Education)

mod.params <- c("beta0", "beta1","beta2","beta3","beta4")

mod_21<- jags(data = data2.jags,                                                   
             model.file = second_model,                                                                 parameters.to.save = mod.params, n.chains = 2, n.iter = 10000, n.burnin = 1000, n.thin = 10)  

freq_fit1=glm(Output ~ rescale(Income) + Credit_History + rescale(Amount) + Education, family=binomial(link="logit"), data=train)


mod_11
mod_21
freq_fit1


{
  
  beta_est2 <- list("beta0" = mod_21$BUGSoutput$summary["beta0", "mean"],         "beta1" = mod_21$BUGSoutput$summary["beta1", "mean"]
                    ,
                    "beta2" = mod_21$BUGSoutput$summary["beta2", "mean"],
                    "beta3" = mod_21$BUGSoutput$summary["beta3", "mean"],
                    "beta4" = mod_21$BUGSoutput$summary["beta4", "mean"])
  
  
  
  
  test$pred2 <- apply(test, 1, function(x){
    
    py <- beta_est2$beta0 + beta_est2$beta1*x["Income"] + beta_est2$beta2*x["Credit_History"] + beta_est2$beta3*x["Amount"] + beta_est2$beta4*x["Education"]
    
    py_perc <- 1-exp(-exp(py))
    
    y_pred2 <- rbinom(n = 10000, size = 1, prob = py_perc)
    
    # get the most repeated value
    predicted2 <- unique(y_pred2)
    predicted2 <- predicted2[which.max(tabulate(match(y_pred2, predicted2)))]
    
    return(predicted2)
  })
  
  conf_mtx_percent <- as.data.frame(round(prop.table(table(test$Output, test$pred2)), digits = 3))
  conf_mtx <- as.data.frame(round(table(test$Output, test$pred2)), digits = 3)
  
  
  tot<- conf_mtx$Freq[1]+conf_mtx$Freq[2]+conf_mtx$Freq[3]+conf_mtx$Freq[4]
  
  
  
  
  mod2_metrics<-data.frame(NA)
  mod2_metrics$Accuracy<- (conf_mtx$Freq[4]+conf_mtx$Freq[1])/(tot)
  mod2_metrics$Precision<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[3])
  mod2_metrics$Recall<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[2])
  mod2_metrics$F1<-2*((mod2_metrics$Precision*mod2_metrics$Recall)/(mod2_metrics$Precision+ mod2_metrics$Recall))
  
  
  mod2_metrics<-mod2_metrics[-1]
  mod2_metrics
  Rec1<-mod2_metrics$Recall
}
raz1



set.seed(9635)

### SPLITTING TRAIN, TEST

index= sample(c(rep(0,0.8*nrow(data)), rep(1,0.2*nrow(data))))
train= data[index == 0,]
test= data[index == 1,]
test[,1]
test[c(1,3)] <- lapply(test[c(1,3)], function(x) c(scale(x))) #

### First Model ###

N<-length(train$Output)

first_model <- function(){
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    logit(p[i]) <-  beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i]
  }
  
  beta0 ~ dnorm(0, 1.0E-6)
  beta1 ~ dnorm(0, 1.0E-6)
  beta2 ~ dnorm(0, 1.0E-6)
  beta3 ~ dnorm(0, 1.0E-6)
  beta4 ~ dnorm(0, 1.0E-6)
  
}

data.jags <- list("y" = train$Output, "N" = N,
                  "x1" = rescale(train$Income), "x2" = train$Credit_History, "x3" = rescale(train$Amount), "x4" = train$Education)

mod.params <- c("beta0", "beta1","beta2","beta3","beta4")

mod_12<- jags(data = data.jags,                                                   
              model.file = first_model,                                                                 parameters.to.save = mod.params, n.chains = 2, n.iter = 10000, n.burnin = 1000, n.thin = 10)

{# Saving the estimated beta parameters
  beta_est <- list("beta0" = mod_12$BUGSoutput$summary["beta0", "mean"],         "beta1" = mod_12$BUGSoutput$summary["beta1", "mean"]
                   ,
                   "beta2" = mod_12$BUGSoutput$summary["beta2", "mean"],
                   "beta3" = mod_12$BUGSoutput$summary["beta3", "mean"],
                   "beta4" = mod_12$BUGSoutput$summary["beta4", "mean"])
  
  
  
  # Iterating and found each predictive values
  test$pred <- apply(test, 1, function(x){
    
    o <- beta_est$beta0 + beta_est$beta1*x["Income"] + beta_est$beta2*x["Credit_History"] + beta_est$beta3*x["Amount"] + beta_est$beta4*x["Education"]
    
    o_perc <- 1/(1+exp(-o))
    
    y_pred <- rbinom(n = 10000, size = 1, prob = o_perc)
    
    # get the most repeated value
    predicted <- unique(y_pred)
    predicted <- predicted[which.max(tabulate(match(y_pred, predicted)))]
    
    return(predicted)
  })
  
  
  conf_mtx_percent <- as.data.frame(round(prop.table(table(test$Output, test$pred)), digits = 3))
  
  conf_mtx <- as.data.frame(round(table(test$Output, test$pred)), digits = 3)
  
  
  
  
  tot<- conf_mtx$Freq[1]+conf_mtx$Freq[2]+conf_mtx$Freq[3]+conf_mtx$Freq[4]
  
  
  
  
  mod1_metrics<-data.frame(NA)
  mod1_metrics$Accuracy<- (conf_mtx$Freq[4]+conf_mtx$Freq[1])/(tot)
  mod1_metrics$Precision<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[3])
  mod1_metrics$Recall<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[2])
  mod1_metrics$F1<-2*((mod1_metrics$Precision*mod1_metrics$Recall)/(mod1_metrics$Precision+ mod1_metrics$Recall))
  
  
  mod1_metrics<-mod1_metrics[-1]
  mod1_metrics
  raz2<-mod1_metrics$Recall
}


### Cloglog ###

N<-length(train$Output)

second_model <- function(){
  # Likelihood
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    cloglog(p[i]) <-  beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i]
  }
  
  beta0 ~ dnorm(0, 1.0E-6)
  beta1 ~ dnorm(0, 1.0E-6)
  beta2 ~ dnorm(0, 1.0E-6)
  beta3 ~ dnorm(0, 1.0E-6)
  beta4 ~ dnorm(0, 1.0E-6)
  
}

data2.jags <- list("y" = train$Output, "N" = N,
                   "x1" = rescale(train$Income), "x2" = train$Credit_History, "x3" = rescale(train$Amount), "x4" = train$Education)

mod.params <- c("beta0", "beta1","beta2","beta3","beta4")

mod_22<- jags(data = data2.jags,                                                   
              model.file = second_model,                                                                 parameters.to.save = mod.params, n.chains = 2, n.iter = 10000, n.burnin = 1000, n.thin = 10)  

freq_fit2=glm(Output ~ rescale(Income) + Credit_History + rescale(Amount) + Education, family=binomial(link="logit"), data=train)


mod_12
mod_22
freq_fit2





{

beta_est2 <- list("beta0" = mod_22$BUGSoutput$summary["beta0", "mean"],         "beta1" = mod_22$BUGSoutput$summary["beta1", "mean"]
                  ,
                  "beta2" = mod_22$BUGSoutput$summary["beta2", "mean"],
                  "beta3" = mod_22$BUGSoutput$summary["beta3", "mean"],
                  "beta4" = mod_22$BUGSoutput$summary["beta4", "mean"])




test$pred2 <- apply(test, 1, function(x){
  
  py <- beta_est2$beta0 + beta_est2$beta1*x["Income"] + beta_est2$beta2*x["Credit_History"] + beta_est2$beta3*x["Amount"] + beta_est2$beta4*x["Education"]
  
  py_perc <- 1-exp(-exp(py))
  
  y_pred2 <- rbinom(n = 10000, size = 1, prob = py_perc)
  
  # get the most repeated value
  predicted2 <- unique(y_pred2)
  predicted2 <- predicted2[which.max(tabulate(match(y_pred2, predicted2)))]
  
  return(predicted2)
})

conf_mtx_percent <- as.data.frame(round(prop.table(table(test$Output, test$pred2)), digits = 3))
conf_mtx <- as.data.frame(round(table(test$Output, test$pred2)), digits = 3)


tot<- conf_mtx$Freq[1]+conf_mtx$Freq[2]+conf_mtx$Freq[3]+conf_mtx$Freq[4]




mod2_metrics<-data.frame(NA)
mod2_metrics$Accuracy<- (conf_mtx$Freq[4]+conf_mtx$Freq[1])/(tot)
mod2_metrics$Precision<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[3])
mod2_metrics$Recall<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[2])
mod2_metrics$F1<-2*((mod2_metrics$Precision*mod2_metrics$Recall)/(mod2_metrics$Precision+ mod2_metrics$Recall))


mod2_metrics<-mod2_metrics[-1]
mod2_metrics
Rec2<-mod2_metrics$Recall
}


set.seed(999)

### SPLITTING TRAIN, TEST

index= sample(c(rep(0,0.8*nrow(data)), rep(1,0.2*nrow(data))))
train= data[index == 0,]
test= data[index == 1,]
test[,1]
test[c(1,3)] <- lapply(test[c(1,3)], function(x) c(scale(x))) #

### First Model ###

N<-length(train$Output)

first_model <- function(){
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    logit(p[i]) <-  beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i]
  }
  
  beta0 ~ dnorm(0, 1.0E-6)
  beta1 ~ dnorm(0, 1.0E-6)
  beta2 ~ dnorm(0, 1.0E-6)
  beta3 ~ dnorm(0, 1.0E-6)
  beta4 ~ dnorm(0, 1.0E-6)
  
}

data.jags <- list("y" = train$Output, "N" = N,
                  "x1" = rescale(train$Income), "x2" = train$Credit_History, "x3" = rescale(train$Amount), "x4" = train$Education)

mod.params <- c("beta0", "beta1","beta2","beta3","beta4")

mod_13<- jags(data = data.jags,                                                   
              model.file = first_model,                                                                 parameters.to.save = mod.params, n.chains = 2, n.iter = 10000, n.burnin = 1000, n.thin = 10)


{# Saving the estimated beta parameters
  beta_est <- list("beta0" = mod_13$BUGSoutput$summary["beta0", "mean"],         "beta1" = mod_13$BUGSoutput$summary["beta1", "mean"]
                   ,
                   "beta2" = mod_13$BUGSoutput$summary["beta2", "mean"],
                   "beta3" = mod_13$BUGSoutput$summary["beta3", "mean"],
                   "beta4" = mod_13$BUGSoutput$summary["beta4", "mean"])
  
  
  
  # Iterating and found each predictive values
  test$pred <- apply(test, 1, function(x){
    
    o <- beta_est$beta0 + beta_est$beta1*x["Income"] + beta_est$beta2*x["Credit_History"] + beta_est$beta3*x["Amount"] + beta_est$beta4*x["Education"]
    
    o_perc <- 1/(1+exp(-o))
    
    y_pred <- rbinom(n = 10000, size = 1, prob = o_perc)
    
    # get the most repeated value
    predicted <- unique(y_pred)
    predicted <- predicted[which.max(tabulate(match(y_pred, predicted)))]
    
    return(predicted)
  })
  
  
  conf_mtx_percent <- as.data.frame(round(prop.table(table(test$Output, test$pred)), digits = 3))
  
  conf_mtx <- as.data.frame(round(table(test$Output, test$pred)), digits = 3)
  
  
  
  
  tot<- conf_mtx$Freq[1]+conf_mtx$Freq[2]+conf_mtx$Freq[3]+conf_mtx$Freq[4]
  
  
  
  
  mod1_metrics<-data.frame(NA)
  mod1_metrics$Accuracy<- (conf_mtx$Freq[4]+conf_mtx$Freq[1])/(tot)
  mod1_metrics$Precision<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[3])
  mod1_metrics$Recall<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[2])
  mod1_metrics$F1<-2*((mod1_metrics$Precision*mod1_metrics$Recall)/(mod1_metrics$Precision+ mod1_metrics$Recall))
  
  
  mod1_metrics<-mod1_metrics[-1]
  mod1_metrics
  raz3<-mod1_metrics$Recall
}



### Cloglog ###

N<-length(train$Output)

second_model <- function(){
  # Likelihood
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    cloglog(p[i]) <-  beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i]
  }
  
  beta0 ~ dnorm(0, 1.0E-6)
  beta1 ~ dnorm(0, 1.0E-6)
  beta2 ~ dnorm(0, 1.0E-6)
  beta3 ~ dnorm(0, 1.0E-6)
  beta4 ~ dnorm(0, 1.0E-6)
  
}

data2.jags <- list("y" = train$Output, "N" = N,
                   "x1" = rescale(train$Income), "x2" = train$Credit_History, "x3" = rescale(train$Amount), "x4" = train$Education)

mod.params <- c("beta0", "beta1","beta2","beta3","beta4")

mod_23<- jags(data = data2.jags,                                                   
              model.file = second_model,                                                                 parameters.to.save = mod.params, n.chains = 2, n.iter = 10000, n.burnin = 1000, n.thin = 10)  

freq_fit3=glm(Output ~ rescale(Income) + Credit_History + rescale(Amount) + Education, family=binomial(link="logit"), data=train)


mod_13
mod_23
freq_fit3

{
  
  beta_est2 <- list("beta0" = mod_23$BUGSoutput$summary["beta0", "mean"],         "beta1" = mod_23$BUGSoutput$summary["beta1", "mean"]
                    ,
                    "beta2" = mod_23$BUGSoutput$summary["beta2", "mean"],
                    "beta3" = mod_23$BUGSoutput$summary["beta3", "mean"],
                    "beta4" = mod_23$BUGSoutput$summary["beta4", "mean"])
  
  
  
  
  test$pred2 <- apply(test, 1, function(x){
    
    py <- beta_est2$beta0 + beta_est2$beta1*x["Income"] + beta_est2$beta2*x["Credit_History"] + beta_est2$beta3*x["Amount"] + beta_est2$beta4*x["Education"]
    
    py_perc <- 1-exp(-exp(py))
    
    y_pred2 <- rbinom(n = 10000, size = 1, prob = py_perc)
    
    # get the most repeated value
    predicted2 <- unique(y_pred2)
    predicted2 <- predicted2[which.max(tabulate(match(y_pred2, predicted2)))]
    
    return(predicted2)
  })
  
  conf_mtx_percent <- as.data.frame(round(prop.table(table(test$Output, test$pred2)), digits = 3))
  conf_mtx <- as.data.frame(round(table(test$Output, test$pred2)), digits = 3)
  
  
  tot<- conf_mtx$Freq[1]+conf_mtx$Freq[2]+conf_mtx$Freq[3]+conf_mtx$Freq[4]
  
  
  
  
  mod2_metrics<-data.frame(NA)
  mod2_metrics$Accuracy<- (conf_mtx$Freq[4]+conf_mtx$Freq[1])/(tot)
  mod2_metrics$Precision<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[3])
  mod2_metrics$Recall<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[2])
  mod2_metrics$F1<-2*((mod2_metrics$Precision*mod2_metrics$Recall)/(mod2_metrics$Precision+ mod2_metrics$Recall))
  
  
  mod2_metrics<-mod2_metrics[-1]
  mod2_metrics
  Rec3<-mod2_metrics$Recall
}

set.seed(10)

### SPLITTING TRAIN, TEST

index= sample(c(rep(0,0.8*nrow(data)), rep(1,0.2*nrow(data))))
train= data[index == 0,]
test= data[index == 1,]
test[,1]
test[c(1,3)] <- lapply(test[c(1,3)], function(x) c(scale(x))) #

### First Model ###

N<-length(train$Output)

first_model <- function(){
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    logit(p[i]) <-  beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i]
  }
  
  beta0 ~ dnorm(0, 1.0E-6)
  beta1 ~ dnorm(0, 1.0E-6)
  beta2 ~ dnorm(0, 1.0E-6)
  beta3 ~ dnorm(0, 1.0E-6)
  beta4 ~ dnorm(0, 1.0E-6)
  
}

data.jags <- list("y" = train$Output, "N" = N,
                  "x1" = rescale(train$Income), "x2" = train$Credit_History, "x3" = rescale(train$Amount), "x4" = train$Education)

mod.params <- c("beta0", "beta1","beta2","beta3","beta4")

mod_14<- jags(data = data.jags,                                                   
              model.file = first_model,                                                                 parameters.to.save = mod.params, n.chains = 2, n.iter = 10000, n.burnin = 1000, n.thin = 10)



{# Saving the estimated beta parameters
  beta_est <- list("beta0" = mod_14$BUGSoutput$summary["beta0", "mean"],         "beta1" = mod_14$BUGSoutput$summary["beta1", "mean"]
                   ,
                   "beta2" = mod_14$BUGSoutput$summary["beta2", "mean"],
                   "beta3" = mod_14$BUGSoutput$summary["beta3", "mean"],
                   "beta4" = mod_14$BUGSoutput$summary["beta4", "mean"])
  
  
  
  # Iterating and found each predictive values
  test$pred <- apply(test, 1, function(x){
    
    o <- beta_est$beta0 + beta_est$beta1*x["Income"] + beta_est$beta2*x["Credit_History"] + beta_est$beta3*x["Amount"] + beta_est$beta4*x["Education"]
    
    o_perc <- 1/(1+exp(-o))
    
    y_pred <- rbinom(n = 10000, size = 1, prob = o_perc)
    
    # get the most repeated value
    predicted <- unique(y_pred)
    predicted <- predicted[which.max(tabulate(match(y_pred, predicted)))]
    
    return(predicted)
  })
  
  
  conf_mtx_percent <- as.data.frame(round(prop.table(table(test$Output, test$pred)), digits = 3))
  
  conf_mtx <- as.data.frame(round(table(test$Output, test$pred)), digits = 3)
  
  
  
  
  tot<- conf_mtx$Freq[1]+conf_mtx$Freq[2]+conf_mtx$Freq[3]+conf_mtx$Freq[4]
  
  
  
  
  mod1_metrics<-data.frame(NA)
  mod1_metrics$Accuracy<- (conf_mtx$Freq[4]+conf_mtx$Freq[1])/(tot)
  mod1_metrics$Precision<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[3])
  mod1_metrics$Recall<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[2])
  mod1_metrics$F1<-2*((mod1_metrics$Precision*mod1_metrics$Recall)/(mod1_metrics$Precision+ mod1_metrics$Recall))
  
  
  mod1_metrics<-mod1_metrics[-1]
  mod1_metrics
  raz4<-mod1_metrics$Recall
}




### Cloglog ###

N<-length(train$Output)

second_model <- function(){
  # Likelihood
  for (i in 1:N){
    y[i] ~ dbern(p[i])
    cloglog(p[i]) <-  beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i]
  }
  
  beta0 ~ dnorm(0, 1.0E-6)
  beta1 ~ dnorm(0, 1.0E-6)
  beta2 ~ dnorm(0, 1.0E-6)
  beta3 ~ dnorm(0, 1.0E-6)
  beta4 ~ dnorm(0, 1.0E-6)
  
}

data2.jags <- list("y" = train$Output, "N" = N,
                   "x1" = rescale(train$Income), "x2" = train$Credit_History, "x3" = rescale(train$Amount), "x4" = train$Education)

mod.params <- c("beta0", "beta1","beta2","beta3","beta4")

mod_24<- jags(data = data2.jags,                                                   
              model.file = second_model,                                                                 parameters.to.save = mod.params, n.chains = 2, n.iter = 10000, n.burnin = 1000, n.thin = 10)  

freq_fit4=glm(Output ~ rescale(Income) + Credit_History + rescale(Amount) + Education, family=binomial(link="logit"), data=train)


mod_14
mod_24
freq_fit4



mod_13
mod_23
freq_fit3




{
  
  beta_est2 <- list("beta0" = mod_24$BUGSoutput$summary["beta0", "mean"],         "beta1" = mod_24$BUGSoutput$summary["beta1", "mean"]
                    ,
                    "beta2" = mod_24$BUGSoutput$summary["beta2", "mean"],
                    "beta3" = mod_24$BUGSoutput$summary["beta3", "mean"],
                    "beta4" = mod_24$BUGSoutput$summary["beta4", "mean"])
  
  
  
  
  test$pred2 <- apply(test, 1, function(x){
    
    py <- beta_est2$beta0 + beta_est2$beta1*x["Income"] + beta_est2$beta2*x["Credit_History"] + beta_est2$beta3*x["Amount"] + beta_est2$beta4*x["Education"]
    
    py_perc <- 1-exp(-exp(py))
    
    y_pred2 <- rbinom(n = 10000, size = 1, prob = py_perc)
    
    # get the most repeated value
    predicted2 <- unique(y_pred2)
    predicted2 <- predicted2[which.max(tabulate(match(y_pred2, predicted2)))]
    
    return(predicted2)
  })
  
  conf_mtx_percent <- as.data.frame(round(prop.table(table(test$Output, test$pred2)), digits = 3))
  conf_mtx <- as.data.frame(round(table(test$Output, test$pred2)), digits = 3)
  
  ####
  conf_mtx_percent2 <- as.data.frame(round(prop.table(table(test$Output, test$pred)), digits = 3))
  conf_mtx2 <- as.data.frame(round(table(test$Output, test$pred)), digits = 3)
  mod2_metrics2<-data.frame(NA)
  mod2_metrics2$Accuracy<- (conf_mtx2$Freq[4]+conf_mtx2$Freq[1])/(tot)
  mod2_metrics2$Precision<- conf_mtx2$Freq[4]/(conf_mtx2$Freq[4] + conf_mtx2$Freq[3])
  mod2_metrics2$Recall<- conf_mtx2$Freq[4]/(conf_mtx2$Freq[4] + conf_mtx2$Freq[2])
  mod2_metrics2$F1<-2*((mod2_metrics2$Precision*mod2_metrics2$Recall)/(mod2_metrics2$Precision+ mod2_metrics2$Recall))
  
  
  mod2_metrics2<-mod2_metrics2[-1]
  mod2_metrics2
  
  ####
  
  
  tot<- conf_mtx$Freq[1]+conf_mtx$Freq[2]+conf_mtx$Freq[3]+conf_mtx$Freq[4]
  
  
  
  
  mod2_metrics<-data.frame(NA)
  mod2_metrics$Accuracy<- (conf_mtx$Freq[4]+conf_mtx$Freq[1])/(tot)
  mod2_metrics$Precision<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[3])
  mod2_metrics$Recall<- conf_mtx$Freq[4]/(conf_mtx$Freq[4] + conf_mtx$Freq[2])
  mod2_metrics$F1<-2*((mod2_metrics$Precision*mod2_metrics$Recall)/(mod2_metrics$Precision+ mod2_metrics$Recall))
  
  
  mod2_metrics<-mod2_metrics[-1]
  mod2_metrics
  Rec4<-mod2_metrics$Recall
}




Rec1
Rec2
Rec3
Rec4

raz1
raz2
raz3
raz4


test$pred
test$pred2
# COMPARING MODELS

mod1_metrics
mod2_metrics
modf_metrics

Total_metrics<-merge(mod2_metrics,mod1_metrics,all.x = TRUE, all.y = TRUE)
Total_metrics[3,]<-modf_metrics


Index<-c("Cloglog","Frequentist Logistic", "Bayesian Logistic")

vect<-rep(names(Total_metrics[3,]),3)
Val<-c(as.numeric(Total_metrics[1,]),as.numeric(Total_metrics[2,]),as.numeric(Total_metrics[3,]))


Data_plot <- data.frame(Model = c("Cloglog","Cloglog","Cloglog","Cloglog","Frequentist Logistic","Frequentist Logistic","Frequentist Logistic","Frequentist Logistic","Bayesian Logistic","Bayesian Logistic","Bayesian Logistic","Bayesian Logistic"), Metrics = vect, Values = Val)


ggplot(data = Data_plot, mapping = aes(x=Metrics, y=Values, fill=Model)) +geom_bar(stat="identity", position = "dodge")+
  scale_fill_manual(values = c("#66CDAA", "#CD5B45","#B452CD"))


# seeds comparisons

Seeds_vect<-rep(c("123","1999","9635","999","10"),2)
Recs<-c(0.933,0.986,1,0.987,0.962,0.946,0.986,1,0.987,0.962)

seed_plot <- data.frame(Model = c("Cloglog","Cloglog","Cloglog","Cloglog","Cloglog","Bayesian Logistic","Bayesian Logistic","Bayesian Logistic","Bayesian Logistic","Bayesian Logistic"), Seed = Seeds_vect, Recall = Recs)

ggplot(data = seed_plot, mapping = aes(x=Seed, y=Recall, fill=Model)) +geom_bar(stat="identity", position = "dodge", width=0.6)+
  scale_fill_manual(values = c("#66CDAA", "#CD5B45"))


Rec1
Rec2
Rec1








# Improvements comparison

merged<-merge(mod2_metrics,mod1_metrics,all.x = TRUE, all.y = TRUE)

vect<-rep(names(merged[3,]),2)
Val<-c(as.numeric(merged[1,]),as.numeric(merged[2,]))


Data_plot <- data.frame(Model = c("Improv2","Improv2","Improv2","Improv2","Improv1","Improv1","Improv1","Improv1"), Metrics = vect, Values = Val)


ggplot(data = Data_plot, mapping = aes(x=Metrics, y=Values, fill=Model)) +geom_bar(stat="identity", position = "dodge")+
  scale_fill_manual(values = c("#EEC900", "#1874CD"))
