source('1830.R')
# logistic regression on quit and # cig smoke, 
current_smoker_smoke <- current_smoker %>%
  select(TriedQuit,cigarette_5,cigarette_30,alcohol_12,drug_yn,hypertension_yn,diabete_yn)


#recdode 2 to 0 
library(MASS)
cc_smoker_smoke$TriedQuit <- ifelse(cc_smoker_smoke$TriedQuit == 2, 0, cc_smoker_smoke$TriedQuit)
table(cc_smoker_smoke$TriedQuit)

#inital model
l5<-glm(TriedQuit ~ cigarette_5 + cigarette_30 + drug_yn+alcohol_12+hypertension_yn+diabete_yn ,data = cc_smoker_smoke,family = 'binomial')
summary(l5)

stepAIC(l5)
# suggest model
l6 <- glm(formula = TriedQuit ~ cigarette_5 + drug_yn, family = "binomial", 
    data = cc_smoker_smoke)
summary(l6)

library(glmnet)
y2 <- cc_smoker_smoke$TriedQuit
x2 <- data.matrix(cc_smoker_smoke[, c('cigarette_5', 'cigarette_30', 'drug_yn','alcohol_12','hypertension_yn','diabete_yn')])
lasso_model <- cv.glmnet(x2,y2, family = "binomial", alpha = 1)
print(lasso_model)
## Extract the coefficients of the LASSO model
lasso_coef <- coef(lasso_model, s = "lambda.min")

# Identify the non-zero coefficients
non_zero_coef <- which(lasso_coef != 0)
print(non_zero_coef)

#refit model
l7<-glm(TriedQuit ~ cigarette_5+cigarette_30+drug_yn + alcohol_12, data = cc_smoker_smoke,family = 'binomial')
summary(l7)
vif(l7)
# remove one of the predicator due to AIC results and multicollinarity
l8<-glm(TriedQuit ~ cigarette_5++drug_yn + alcohol_12, data = cc_smoker_smoke,family = 'binomial')
summary(l8)

library(odds.n.ends)
odds.n.ends(l8)
vif(l8)
# no multiconllinarity present


#outliers - cook's distance
plot(l8, which=4, id.n=5, col="red")

# Specify indices to exclude
l9 <- c(4804, 3425, 908,748)

# Calculate Cook's distance
c2 <- cooks.distance(l8)

# Find corresponding indices in c2
n3 <- sapply(l9, function(x) which(names(c2) == as.character(x)))

# Create a new data frame excluding specified observations
cc_smoker_smoke_2 <- cc_smoker_smoke[-unlist(n3),]

# Refit model
l9 <- glm(TriedQuit ~ cigarette_5 + drug_yn + alcohol_12, data = cc_smoker_smoke_2, family = 'binomial')

summary(l9)
vif(l9)

# check linearity
library(gvlma)

cc_smoker_smoke_2 <- cc_smoker_smoke_2 %>%
  mutate(cigarette_5.log = as.numeric(cigarette_5) * log(as.numeric(cigarette_5)),
         alcohol_12.log = alcohol_12 * log(alcohol_12))


formula_l9 <- formula(TriedQuit ~ cigarette_5 + drug_yn + alcohol_12 + cigarette_5.log + alcohol_12.log)

l10 <- glm(formula_l9, data = cc_smoker_smoke_2, family = 'binomial')

# Perform Box-Tidwell test for the continuous variable (cigarette_5)
box_tidwell_result<- glm(TriedQuit ~ cigarette_5 + drug_yn + alcohol_12 + cigarette_5.log + alcohol_12.log, data = cc_smoker_smoke_2, family = 'binomial')

summary(box_tidwell_result)

# no violation of linearity assumption
library(odds.n.ends)
odds.n.ends(l7)

table()


