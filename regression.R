# Shuting
#complete case analysis for current smoker 
# 1021 current smoker, complete case for demographic 

source('1830.R')
current_smoker_demographic <- current_smoker %>%
  select(TriedQuit,FaNu_smokeAtHome, gender, age, citizenship, marital_sta, education, poverty, race, household, less5child, olderchild)

#recode binary outcome
current_smoker_demographic$TriedQuit = ifelse(current_smoker_demographic$TriedQuit == 2, 0, current_smoker_demographic$TriedQuit)

# initial model
library(MASS)
l1 <- glm(TriedQuit ~ (FaNu_smokeAtHome) + gender + age + citizenship + factor(marital_sta) + education + poverty
          + factor(race) + household + less5child +olderchild, data = current_smoker_demographic, family = 'binomial')

summary(l1)
stepAIC(l1)
step(l1, test="LRT")

l2<-glm(formula = TriedQuit ~ FaNu_smokeAtHome + education + factor(race) + 
      olderchild, family = "binomial", data = complete_case_smoker)
summary(l2)
# suggest using Call:  glm(formula = TriedQuit ~ FaNu_smokeAtHome + education + olderchild, 
# family = "binomial", data = complete_case_smoker)

library(odds.n.ends)
odds.n.ends(l2)

# lasso regression for variable selection
library(glmnet)
y1 <- complete_case_smoker$TriedQuit
x1 <- data.matrix(complete_case_smoker[, c('FaNu_smokeAtHome', 'gender', 'age', 
                                           'citizenship', 'marital_sta', 'education', 'poverty', 'race', 'household', 'less5child', 'olderchild')])
lasso_model <- cv.glmnet(x,y, family = "binomial", alpha = 1)
print(lasso_model)
## Extract the coefficients of the LASSO model
lasso_coef <- coef(lasso_model, s = "lambda.min")

# Identify the non-zero coefficients
non_zero_coef <- which(lasso_coef != 0)
print(non_zero_coef)
#result indicate non-zero coefficient for FaNu_smokeAtHome, education, less5child, olderchild

# refit model
l3 <-glm(formula = TriedQuit ~ FaNu_smokeAtHome + education + factor(race) + less5child+
           olderchild, family = "binomial", data = complete_case_smoker)
summary(l3)
odds.n.ends(l3)




#outliers - cook's distance
plot(l3, which=4, id.n=5, col="red")

# Specify indices to exclude
l4 <- c(2522, 4507, 6195,2318)

# Calculate Cook's distance
c2 <- cooks.distance(l3)

# Find corresponding indices in c2
n3 <- sapply(l4, function(x) which(names(c2) == as.character(x)))

# Create a new data frame excluding specified observations
complete_case_smoker_2 <- complete_case_smoker[-unlist(n3),]

# Refit model with corrected formula
l4 <- glm(TriedQuit ~ FaNu_smokeAtHome + education + factor(race) +less5child+ olderchild,
          data = complete_case_smoker_2,
          family = 'binomial')

summary(l4)

odds.n.ends(l4)
odds.n.ends(l3)
#testing for multidisciplinary

vif(l3)
vif(l4)
# no multidisciplinary

current_smoker_demographic <- current_smoker_demographic %>%
  mutate(
    FaNu_smokeAtHome.log = FaNu_smokeAtHome * log(FaNu_smokeAtHome),
    age.log = age * log(age),
    poverty.log = poverty * log(poverty)
  )

# Specify the logistic regression formula
formula <- formula(TriedQuit ~ FaNu_smokeAtHome + age + poverty + FaNu_smokeAtHome.log + age.log + poverty.log + gender + citizenship + marital_sta + education)


# Perform Box-Tidwell tests for each continuous variable
box_tidwell_results <- lapply(
  c("FaNu_smokeAtHome", "age", "poverty"),
  function(variable) {
    new_var_name <- paste0(variable, ".log")
    formula_box_tidwell <- as.formula(paste("TriedQuit ~", variable, "+", new_var_name))
    glm(formula_box_tidwell, data = current_smoker_demographic, family = "binomial")
  }
)


# Display the summaries of Box-Tidwell tests
lapply(box_tidwell_results, summary)
#linearity assumption was not violated.


# use linear mix model to model the effect of race and less5child
library(Matrix)
library(lme4)
fit2 <-glmer(TriedQuit ~ FaNu_smokeAtHome + education + (1|race) + (1|less5child) + olderchild,
             data = current_smoker_demographic,
             family = 'binomial')
summary(fit2)
# Exponentiate fixed effects estimates to obtain odds ratios
odds_ratios <- exp(fixef(fit2));odds_ratios


                   