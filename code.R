# R version 4.4.1  
library(foreign) 
library(boot) 
library("readxl") 
library("xlsx") 
library("survival") 
library("survminer") 
library("riskRegression", verbose = FALSE, quietly = TRUE) 
library(glmnet) 
library("ggplot2") 
library(prodlim) 
library(caret) 
library(pROC) 
# Import dataset 
mydata <- read_excel("data_treatment.xlsx") 
data = data.frame(mydata) 
#------------------Features-------------------- 
# AGE <45 is reference 
AGE_45_54 <- ifelse(data$AGE >= 45 & data$AGE <= 54.99 ,1,0) 
AGE_55_64 <- ifelse(data$AGE >= 55 & data$AGE <= 64.99 ,1,0) 
AGE_65_74 <- ifelse(data$AGE >= 65 & data$AGE <= 74 ,1,0) 
#AGE_75 <- ifelse(data$AGE > 74 ,1,0) 
AGE_75_84 <- ifelse(data$AGE >= 75 & data$AGE <= 84 ,1,0) 
AGE_85 <- ifelse(data$AGE > 84 ,1,0) 
#CHARLSON_INDEX 0-1 is reference 
CI_more_than_thr <- ifelse(data$CHARLSON_INDEX > 1 ,1,0) 
#NEUTROPHILS between 1500 & 5900 is reference 
NEUTROPENIA <- ifelse(data$NEUTROPHILS < 1500 ,1,0) 
NEUTROCYTOSIS <- ifelse(data$NEUTROPHILS > 5900 ,1,0) 
#LYMPHOCYTES  between 1100 & 3700 is reference 
LYMPHOPENIA <- ifelse(data$LYMPHOCYTES < 1100 ,1,0) 
LYMPHOCTOSIS <- ifelse(data$LYMPHOCYTES > 3700 ,1,0) 
#PLETELET  between 150000 & 400000 is reference 
THROMBOCYTOPENIA <- ifelse(data$PLETELET < 150000 ,1,0) 
PLETELET_High <- ifelse(data$PLETELET > 400000 ,1,0) 
# SATO2 between 90 & 100 is reference 
SATO2_80 <- ifelse(data$SATO2 < 80 , 1,0)  
SATO2_80_90 <- ifelse(data$SATO2 >= 80 & data$SATO2 <= 90 ,1,0) 
# TACHYPNEA_MORE_THAN_20BPM 
TACHYPNEA_MORE_THAN_20BPM <- data$TACHYPNEA_MORE_THAN_20BPM 
# LDH less than 280 is reference 
LDH_more_than_thr <- ifelse(data$LDH > 280 ,1,0) 
# COPD 
COPD <- data$COPD 
#-------------------------------------------- 
#Naïve Approach 
data$Outcome  <- factor(data$Outcome, levels=c(0, 1), labels=c("Discharged", "Death")) 
model0 <- CSC(formula = Hist(time = time, event= Outcome) ~ SEX + AGE + NEUTROPHILS + 
              LYMPHOCYTES + PLETELET + SATO2 + CHARLSON_INDEX , data=data) 
print(model0) 
model1 <- CSC(formula = Hist(time, Outcome) ~ factor(SEX) + factor(AGE_45_54) + 
                factor(AGE_55_64) + factor(AGE_65_74)+ factor(AGE_75_84) + factor(AGE_85) + 
                factor(LYMPHOPENIA) + factor(LYMPHOCTOSIS) + factor(TACHYPNEA_MORE_THAN_20BPM) + 
                factor(CI_more_than_thr) , data=data) 
print(model1) 
model2 <- CSC(formula = Hist(time, Outcome) ~ factor(SEX) + factor(AGE_45_54) + 
                factor(AGE_55_64) + factor(AGE_65_74)+ factor(AGE_75_84) + factor(AGE_85) + 
                factor(LYMPHOPENIA) + factor(TACHYPNEA_MORE_THAN_20BPM) + 
                factor(LDH_more_than_thr)+ factor(COPD) + factor(CI_more_than_thr) , data=data) 
print(model2) 
 
#****************** 
# Computation of the absolute risk 
newdata <- data.frame( 
SEX = factor(data$SEX), 
AGE_45_54 = factor(AGE_45_54), 
AGE_55_64 = factor(AGE_55_64), 
AGE_65_74 = factor(AGE_65_74), 
AGE_75 = factor(AGE_75), 
AGE_75_84 = factor(AGE_75_84), 
AGE_85 = factor(AGE_85), 
NEUTROPENIA = factor(NEUTROPENIA), 
NEUTROCYTOSIS = factor(NEUTROCYTOSIS), 
LYMPHOPENIA = factor(LYMPHOPENIA), 
LYMPHOCTOSIS = factor(LYMPHOCTOSIS), 
THROMBOCYTOPENIA = factor(THROMBOCYTOPENIA), 
PLETELET_High = factor(PLETELET_High), 
SATO2_80 = factor(SATO2_80), 
SATO2_80_90 = factor(SATO2_80_90), 
TACHYPNEA_MORE_THAN_20BPM = factor(TACHYPNEA_MORE_THAN_20BPM), 
CI_more_than_thr = factor(CI_more_than_thr) 
) 
tm <- max(data$time) 
pfit <- predict(model1, newdata=newdata, cause='Death', time=tm) 
abs_risk <- pfit$absRisk 
#------------------------------------------------------------------------------------ 
# Gentic Algorithm 
library(GA) 
data1 = data.frame(mydata) 
data1$TYPE_DIABETES[is.na(data1$TYPE_DIABETES)] <- 0 
data1$TYPE_DIABETES <- ifelse(data1$TYPE_DIABETES ==0 ,0,1) 
data1$SMOKINGBEHAVIOR <- ifelse(data1$SMOKINGBEHAVIOR == 0,0,1) 
data1 <- data1[ !is.na(data1$DDIMER) & !is.na(data1$BMI_impu) & 
!is.na(data1$SATO2) & !is.na(data1$HB) &  
!is.na(data1$NEUTROPHILS) &!is.na(data1$ALT) & 
!is.na(data1$WBC) & !is.na(data1$LDH) & 
!is.na(data1$CRP) & !is.na(data1$LYMPHOCYTES) & !is.na(data1$PLETELET) & 
!is.na(data1$CHARLSON_INDEX), ] 
actual <- data1$Outcome 
time <- data1$time 
status <- factor(data1$Outcome, levels=c(0, 1), labels=c("Discharged", "Death")) 
#data1$bmi <- ifelse(data1$bmi =="Normal" ,0,1) 
data1$BMI_impu <- ifelse(data1$BMI_impu < 24.9,0,1) 
x1 <- data1$SEX  
x2 <- data1$AGE  
#x3 <- data1$bmi  
x3 <- data1$BMI_impu 
x4 <- data1$DDIMER  
x5 <- data1$ARTERIAL_HYPERTENSION 
x6 <- data1$SATO2 
x7 <- data1$HB 
x8 <- data1$DYSPNEA 
x9 <- data1$NEUTROPHILS 
x10 <- data1$ALT 
x11 <- data1$CHRONIC_HEART_FAILURE 
x12 <- data1$WBC 
x13 <- data1$TYPE_DIABETES 
x14 <- data1$LDH 
x15 <- data1$CRP 
x16 <- data1$LYMPHOCYTES 
x17 <- data1$PLETELET 
x18 <- data1$SMOKINGBEHAVIOR 
x19 <- data1$COMPLICATION_RENAL_FAILURE 
x20 <- data1$CHARLSON_INDEX 
# Genetic Algorithm parameters 
GA_params <- list( 
  popSize = 10, # Population size 
  maxiter = 500, # Maximum number of iterations 
  pmutation = 0.8, # Mutation probability 
  pcrossover = 0.5, # Crossover probability 
  elitism = 2 # Number of elite individuals 
) 
 
# Create a data frame 
data <- data.frame(time = time, status = status, x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5=x5, 
                   x6=x6, x7=x7, x8=x8, x9=x9, x10=x10, x11 = x11, x12 = x12, x13 = x13, 
                   x14 = x14, x15=x15, x16=x16, x17=x17, x18=x18, x19=x19, x20=x20) 
tm = max(data$time) 
# Initialize a list to store fitness values 
best_fitness_values <- list() 
 
# Fitness  1 : R2 + ROC  
# Define the objective function for the genetic algorithm 
objective_function <- function(selected_vars) { 
  # Convert binary vector to variable names 
  vars <- names(data)[-c(1, 2)][which(selected_vars == 1)] 
  # Penalize if no variables are selected 
  if (length(vars) == 0) { 
    return(-Inf)  # Large penalty for no variables selected 
  } 
  # Fit CSC model 
  fit <- CSC(formula = Hist(time, status) ~ ., data = data[, c("time", "status", vars)]) 
   # Predict risk scores 
  pfit <- predict(fit, newdata = data, cause = "Death", time = tm, product.limit = FALSE) 
  pred <- pfit$absRisk 
   
# Check for invalid predictions (outside [0, 1]) 
invalid_predictions <- sum(pred < 0 | pred > 1) 
# Penalize invalid predictions 
if (invalid_predictions > 0) { 
return(-Inf)  # Large penalty for invalid predictions 
} 
# Evaluate calibration and discrimination 
calPerf <- val.prob.ci.2(pred, actual) 
score <- (calPerf['R2'] + calPerf['C (ROC)']) 
return(score) 
} 
# Define a monitor function to save the best fitness value in each iteration 
monitor_function <- function(obj) { 
# Get the best fitness value in the current iteration 
best_fitness <- max(obj@fitness) 
# Append the best fitness value to the vector 
best_fitness_values <<- c(best_fitness_values, best_fitness) 
# Print the best fitness value (optional) 
cat("Iteration:", obj@iter, "Best Fitness:", best_fitness, "\n") 
} 
# Define a function to save the best fitness values to an Excel file 
save_best_fitness_values <- function() { 
write.xlsx(data.frame(Iteration = 1:length(best_fitness_values), BestFitness = best_fitness_values),  
"best_fitness_values.xlsx") 
} 
# Run the Genetic Algorithm for variable selection 
ga_result <- ga( 
type = "binary", 
fitness = function(selected_vars) objective_function(selected_vars), # Maximize fitness 
nBits = 20, # Number of predictors 
monitor = monitor_function, # Use the custom monitor function 
popSize = GA_params$popSize, 
maxiter = GA_params$maxiter, 
pmutation = GA_params$pmutation, 
pcrossover = GA_params$pcrossover, 
elitism = GA_params$elitism 
) 
# Save fitness values to an Excel file after the GA run  
save_best_fitness_values() 
# Summary of the results 
summary(ga_result) 
ga_result@solution 
#****************** 
# Fitness  2 : R2 + ROC - penalty 
# Initialize a list to store fitness values 
best_fitness2_values <- list() 
objective_function <- function(selected_vars) { 
#print(selected_vars) 
penalty <- 0 
# Convert binary vector to variable names 
vars <- names(data)[-c(1, 2)][which(selected_vars == 1)] 
#print(vars) 
# If no variables are selected, return a large value 
if (length(vars) == 0) { 
return(-Inf)  # Large penalty for no variables selected 
} 
# Fit CSC model  
  fit <- CSC(formula = Hist(time, status) ~ ., data = data[, c("time", "status", vars)]) 
  for(item in vars){ 
    if(item == "x1"){ 
      y <- coefficients(fit,'x1')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x1']) 
      if (yy >= 1){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x2"){  
      y <- coefficients(fit,'x2')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x2']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x3"){  
      y <- coefficients(fit,'x3')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x3']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x4"){  
      y <- coefficients(fit,'x4')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x4']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x5"){  
      y <- coefficients(fit,'x5')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x5']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x6"){ 
      y <- coefficients(fit,'x6')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x6']) 
      if (yy >= 0.999){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x7"){  
      y <- coefficients(fit,'x7')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x7']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x8"){  
      y <- coefficients(fit,'x8')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x8']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x9"){  
      y <- coefficients(fit,'x9')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x9']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x10"){  
      y <- coefficients(fit,'x10')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x10']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
      if(item == "x11"){  
      y <- coefficients(fit,'x11')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x11']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
        if(item == "x12"){  
      y <- coefficients(fit,'x12')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x12']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x13"){  
      y <- coefficients(fit,'x13')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x13']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
        if(item == "x14"){  
      y <- coefficients(fit,'x14')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x14']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
     if(item == "x15"){  
      y <- coefficients(fit,'x15')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x15']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
     if(item == "x16"){ 
      y <- coefficients(fit,'x16')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x16']) 
      if (yy >= 0.999){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x17"){  
      y <- coefficients(fit,'x17')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x17']) 
      if (yy >= 0.999){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x18"){  
      y <- coefficients(fit,'x18')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x18']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x19"){  
      y <- coefficients(fit,'x19')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x19']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
      } 
    } 
    if(item == "x20"){  
      y <- coefficients(fit,'x20')[2] 
      yy <- y$`Cause Death` 
      yy <- exp(yy['x20']) 
      if (yy <= 1){ 
        penalty <- penalty +1 
} 
} 
} 
# Predict risk scores 
pfit <- predict(fit, newdata = data,cause='Death', time=tm, product.limit=FALSE) 
pred <- pfit$absRisk 
# Check for invalid predictions (outside [0, 1]) 
invalid_predictions <- sum(pred < 0 | pred > 1) 
# Penalize invalid predictions 
if (invalid_predictions > 0) { 
return(-Inf)  # Large penalty for invalid predictions 
} 
calPerf = val.prob.ci.2(pred, actual) 
score <- calPerf['R2'] + calPerf['C (ROC)'] - (0.04 * penalty) 
return(score) 
} 
# Define a monitor function to save the best fitness value in each iteration 
monitor_function <- function(obj) { 
# Get the best fitness value in the current iteration 
best_fitness <- max(obj@fitness) 
# Append the best fitness value to the vector 
best_fitness2_values <<- c(best_fitness2_values, best_fitness) 
# Print the best fitness value (optional) 
cat("Iteration:", obj@iter, "Best Fitness:", best_fitness, "\n") 
} 
save_best_fitness_values <- function() { 
write.xlsx(data.frame(Iteration = 1:length(best_fitness2_values), BestFitness = best_fitness2_values),  
"best_fitness_values.xlsx") 
} 
#------------------------------------------------------------------------------------ 
# Regularization Techniques 
data <- data[data$time > 0, ] 
actual <- data$Outcome 
tm = max(data$time) 
# Prepare the response matrix (Surv object) 
surv_obj <- Surv(data$time, data$Outcome) 
# Extract features (predictors) 
features <- data[, c("SEX", "AGE","BMI_impu", "DDIMER", "SATO2", "HB",  
                     "DYSPNEA", "NEUTROPHILS", "ALT", "CHRONIC_HEART_FAILURE",  
                     "WBC", "TYPE_DIABETES", "LDH", "CRP", "LYMPHOCYTES",  
                     "PLETELET", "SMOKINGBEHAVIOR", "COMPLICATION_RENAL_FAILURE",  
                     "CHARLSON_INDEX", "ARTERIAL_HYPERTENSION")] 
 
# Convert categorical variables to dummy variables 
features_matrix <- model.matrix(~ ., data = features)[, -1]  # Exclude intercept 
# Step 1: Apply ridge penalty using glmnet (for lasso penalty set alpha = 0) 
ridge_cox <- glmnet(features_matrix, surv_obj, family = "cox", alpha = 1) 
# Cross-validation to find optimal lambda 
cv_ridge_cox <- cv.glmnet(features_matrix, surv_obj, family = "cox", alpha = 1) 
opt_lambda <- cv_ridge_cox$lambda.min 
opt_lambda 
# Extract coefficients at optimal lambda 
coefficients <- as.matrix(coef(ridge_cox, s = opt_lambda))  # Convert to matrix 
non_zero_indices <- which(coefficients != 0, arr.ind = TRUE)  # Find non-zero coefficients 
selected_features <- rownames(coefficients)[non_zero_indices[, 1]]  # Extract feature names 
selected_features 
# Step 2: Fit final CSC model using selected features 
data$Outcome  <- factor(data$Outcome, levels=c(0, 1), labels=c("Discharged", "Death")) 
formula <- as.formula(paste("Hist(time, Outcome) ~", paste(selected_features, collapse = " + "))) 
csc_model <- CSC(formula, data = data) 
# Print the summary of the fitted model 
print(csc_model) 
 
# Predict risk scores  
new_data <- features_matrix   
pfit <- predict(csc_model, newdata = new_data,cause='Death', time=tm) 
pred <- pfit$absRisk 
