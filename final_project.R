# Final Project
library(tidyverse)
df <- read.csv2("bank.csv")
for (j in 2:10){
  df[,j] <- as.factor(df[,j])
}

# Model
# Var. select
null <- glm(y~1, data = df, family = binomial(link = "logit"))
full <- glm(y~age*education + job + marital + default + housing + loan + duration + contact + month + day_of_week, data = df, family = binomial(link = "logit"))
step(full, scope = list(lower = null, upper = full), direction = 'both')
# Call:  glm(formula = y ~ age + education + job + marital + default + duration + contact + month + day_of_week + age:education,
# family = binomial(link = "logit"), data = df)
glm(formula = y ~ age + education + job + marital + default + duration + contact + month + day_of_week + age:education,
    family = binomial(link = "logit"), data = df)
df %>% ggplot(aes(x=day_of_week, fill = factor(y))) +
  geom_bar() # exclude day_of_week
logit1 <- glm(formula = y ~ age + education + job + marital + default + duration + contact + month  + age:education,
               family = binomial(link = "logit"), data = df)
logit2 <- glm(formula = y ~ age + education + job + marital + default + duration + contact + month + day_of_week + age:education,
               family = binomial(link = "logit"), data = df)
G2<-logit1$dev-logit2$dev
c(G2, 1-pchisq(G2, 2)) # 9.165328e-05, remove day_of_week

df$default %>% table() # remove default

# seperate campaign type
ind <- c('age', 'job', 'marital', 'education', 'default', 'housing', 'loan') # individual
camp <- c('duration', 'contact','month') # campaign
ref <- df %>% select(camp) %>% select(2,3) %>% distinct()
df$camp.index <- NA
for (i in 1:nrow(ref)){
  v <- ref[i,] %>% unlist() %>% unname()
  M <- df[,8:9] %>% as.matrix()
  logi.index <- rowSums(M == v[col(M)]) == ncol(M)
  df[logi.index,ncol(df)] <- i
}
df$camp.index <- as.factor(df$camp.index)

# multilevel model
library(lme4) # lmer
df$age <- scale(df$age)
reg <- glmer(y~age + education + job + marital + age:education  + (1 |camp.index),
     data = df, family = binomial(link="logit"),control=glmerControl(optimizer="bobyqa",
                                                                     optCtrl=list(maxfun=2e5)))
# try different optimizer
# afurl <- "https://raw.githubusercontent.com/lme4/lme4/master/misc/issues/allFit.R"
# eval(parse(text=RCurl::getURL(afurl)))
# aa <- allFit(reg)
# is.OK <- sapply(aa,is,"merMod")  ## nlopt NELDERMEAD failed, others succeeded
# aa.OK <- aa[is.OK]
# lapply(aa.OK,function(x) x@optinfo$conv$lme4$messages)
arm::display(reg)

