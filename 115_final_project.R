library(Stat2Data)

library(tidyverse)

library(ggplot2)
library(bayesplot)
library(rstan)
library(rstanarm)

data("Overdrawn")

df <- Overdrawn

df <- df %>%
  drop_na() 

df$id <- seq.int(nrow(df))

df_train <- df %>%
  sample_frac(0.8)

df_test <- anti_join(df,df_train,by='id')

set.seed(84735)

log_reg_model <- "
  data{
  int <lower = 0> n;
  int <lower=0, upper = 1> Y[n];
  vector[n] X1;
  vector[n] X2;
  vector[n] X3;
}

parameters{
  real beta_0;
  real beta_1;
  real beta_2;
  real beta_3;
}

model{
  Y ~ bernoulli_logit(beta_0 + beta_1*X1 + beta_2 * X2 + beta_3 * X3);

}

"

log_reg_sim<- stan(
  model_code = log_reg_model,
  data = list(Y = df$Overdrawn,
              X1 = df$Age,
              X2 = df$Sex,
              X3 = df$DayDrink, 
              n = nrow(df)),
  chains = 4,
  iter = 5000*2
)
#
bayesplot::mcmc_trace(log_reg_sim)

for (i in 1:nrow(df_test)){
  row <- df_test[i,]
  row[1,1]
  
}
view(df_test)

b = c(1,1,1,1,1,1,1,11,3,5,6,3)
b = c(b,2)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- getmode(b)
print(result)



getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
##########################################################
log_reg_arm <- stan_glm(Overdrawn ~ Age+Sex+DaysDrink,
                        data = df_train,
                        family = binomial(link = 'logit'))

pred_test_arm <- posterior_predict(log_reg_arm,
                                   newdata = df_test)
pred_test_arm

predict1 = c()
for (i in 1:nrow(df_test)){
  col <- pred_test_arm[,i]
  mode = getmode(as.vector(col))
  predict1 = c(predict1,mode)
}


confusionMatrix(data = as.factor(predict1), reference = as.factor(df_test$Overdrawn))


###############


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


log_reg_arm <- stan_glm(Overdrawn ~ Age+Sex+DaysDrink,
                        data = df_train,
                        family = binomial(link = 'logit'))

pred_test_arm <- posterior_predict(log_reg_arm,
                                   newdata = df_test)
pred_test_arm

predict1 = c()
for (i in 1:nrow(df_test)){
  col <- pred_test_arm[,i]
  mode = getmode(as.vector(col))
  predict1 = c(predict1,mode)
}



confusionMatrix(data = as.factor(predict1), reference = as.factor(df_test$Overdrawn))








#########################################################################################
library(InformationValue)
library(caret)
library(e1071)

logit_reg_freq <- glm(Overdrawn ~ Age + Sex + DaysDrink, data=df_train, family=binomial(link="logit"))
summary(logit_reg_freq)


predicted_response <- plogis(predict(logit_reg_freq, df_test))  # predicted scores


cutOff <- optimalCutoff(df_test, predicted_response)[1] 
cutOff

data1 = as.numeric(predicted_response>cutOff)

confusionMatrix(data = as.factor(data1), reference = as.factor(df_test$Overdrawn))

