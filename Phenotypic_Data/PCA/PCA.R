## PCA for soil data

library(agricolae)
library(xtable)
Compiled <- read.csv("PCA_Soil.csv", header = TRUE)
Compiled <- type.convert(replace(Compiled, Compiled == "." & !is.na(Compiled), NA), as.is = TRUE)

## Use MissForest to estimate missing values
#install.packages("missForest")
library(missForest)
set.seed(123)
n_trees <- 100
imputed_data <- missForest(Compiled[c(10:27)], ntree = n_trees)
Clean <- imputed_data$ximp
colSums(is.na(Clean))

#export to conduct Pca in python and extract score
library(readr)
write.csv(Clean, "soilvalues.csv")


Clean2 <- cbind(Compiled[c(1:9)], Clean)


pca <- prcomp(Clean2[,c(10:27)], center = TRUE, scale = TRUE) #scale = true is for variables measured in different units, if values are measures in same unit, scale=false
print(pca)
summary(pca)

plot(pca)
screeplot(pca, type = "line", main = "Scree plot")

library(factoextra)
fviz_eig(pca)  ###Scree plot with a line
Clean2$YEAR <- as.character(Clean2$YEAR)

install.packages("ggfortify")
install.packages("devtools")
#install_github("vqv/ggbiplot", force = TRUE)
install.packages("ggbiplot", force = TRUE)

library(devtools)
library(ggplot2)
library(plyr)
library(scales)
library(grid)
require(ggbiplot)
bplot<- ggbiplot(pcobj=pca,
                 choices=c(1,2),
                 obs.scale=1, var.scale=1,
                 labels = row.names(Clean),
                 varname.size = 3,
                 varname.abbrev = FALSE,
                 var.axes = FALSE, #arrows
                 circle = FALSE,
                 ellipse = TRUE, groups=Clean2$YEAR)
print(bplot)



## PCA For Weather data
weather <- read.csv("PCA_Weather.csv", header = TRUE)

pca_weather <- prcomp(weather[,c(10:34)], center = TRUE, scale = TRUE) #scale = true is for variables measured in different units, if values are measures in same unit, scale=false
print(pca_weather)
summary(pca_weather)

plot(pca_weather)
screeplot(pca_weather, type = "line", main = "Scree plot")

library(factoextra)
fviz_eig(pca_weather)  ###Scree plot with a line

weather$YEAR <- as.character(weather$YEAR)

bplot2<- ggbiplot(pcobj=pca_weather,
                 choices=c(1,2),
                 obs.scale=1, var.scale=1,
                 labels = row.names(Clean),
                 varname.size = 3,
                 varname.abbrev = FALSE,
                 var.axes = FALSE, #arrows
                 circle = FALSE,
                 ellipse = TRUE, groups=weather$YEAR)   #Year also gave some patterns
print(bplot2)


## Add design variables and management and yield to both soil and weather scores from python
Design <- read.csv("Design_Management.csv", header = TRUE)

## Import from python 
soilscores <- read.csv("Soilscores.csv", header = TRUE)
weatherscores   <- read.csv("weatherscores.csv", header = TRUE)

PCA_Data <-  cbind(Design, soilscores[c(1:6)], weatherscores[c(1:4)])

write.csv(PCA_Data, "PCA_Data.csv")

# Now Estimate Reliability and run ML Models

PCA_Data  <- read.csv("PCA_Data.csv", header = TRUE)

#Removing farmers without our check
library(dplyr)
filtered_df <- PCA_Data %>%
  filter(FAR != "Glazik")

## Estimating yield differences between hybrid and check (ORG4)
Yield_Diff <- filtered_df %>%
  group_by(YEAR,FAR,REP) %>%
  mutate(YD = YIELD - YIELD[HYB == "ORG4"]) %>%
  ungroup()

# Model 1. Using yes or no response for reliability
Model1 <- Yield_Diff
Model1$R <- ifelse(Model1$YD >= 0, "Yes", "No")

Model1  <- Model1[-c(1:2, 4:6,11,29)] #remove ENV, City, PED, FAR, Yield and YD

Model1 <- Model1%>%filter(HYB != "ORG4")  #remove rows with ORG4 since they have Na for Reliability
write.csv(Model1 , "Model1.csv")



## Model 2: Using high or low for values above and below 0.5 reliability

## Summation of yield differences
Model2 <- Yield_Diff
Model2 <- Model2 %>%
  group_by(YEAR,HYB) %>%
  mutate(
    MeanYD = mean(YD, na.rm = TRUE),
    SDYD = sd(YD, na.rm = TRUE)
  ) %>%
  ungroup()

## Calculate Zscore for each hybrid
Model2 <- Model2 %>%
  mutate(Zscore = MeanYD/SDYD)


# Estimating Reliability of each hybrid using the zscores. 

#This is the probability that a hybrid will outperform the check hybrid.
#To estimate probabilities using Z-scores, you can use the standard normal distribution (also known as the Z-distribution) 
#and the cumulative distribution function (CDF) of the standard normal distribution. The CDF will give you the probability 
#that a Z-score falls below a certain value.
#In R, you can use the pnorm() function to calculate probabilities from Z-scores.
#Here's how you can estimate probabilities for each Z-score in your dataset:

Model2 <- Model2  %>%
  mutate(R = pnorm(Zscore))

Model2$Re <- ifelse(Model2$R < 0.5, "Low", "High")

Model2 <- Model2[-c(1,2, 4:6,11,27:32)]  #if we use the non-seasonal dataset

Model2 <- Model2%>%filter(HYB != "ORG4") #remove rows with ORG4 since they have Na for Reliability

library(readr)
write.csv(Model2 , "Model2.csv")


# --------------------  Machine Learning: Model 1.-----------------


# Filter data for the first three years (2018, 2019, and 2020)

Model1   <- read.csv("Model1.csv", header = TRUE)  ##changed NA wRT to medium, and HW CI. CD, PER NAs to previous HW data

library(dplyr)
Training_Phase <- Model1  %>%
  filter(YEAR %in% c(2018, 2019, 2020, 2021))

Training_Phase$R <- ifelse(Training_Phase$R == "Yes", "1", "0")
Training_Phase  <- Training_Phase[-c(4:5)] #remove year and rep
Training_Phase$R <- as.numeric(Training_Phase$R)


# Split training_phgase data into training and testing

library(caret)
set.seed(123)
split_index <- createDataPartition(Training_Phase$R, p = 0.8, list = FALSE)
Training_set1 <- Training_Phase[split_index, ]
Testing_set1 <- Training_Phase[-split_index, ]


# Full model
model.formula <- as.formula("R ~.")
model_full <- glm(model.formula, data = Training_Phase, family = binomial)

summary(model_full)

# Stepwise procedure
library(MASS)
model_stepwise <- stepAIC(model_full, scope = list(lower = ~ 1, upper = ~ .^2), 
                          direction = "both")

summary (model_stepwise)
anova(model_full,model_stepwise, test='Chisq')

# 1. USING CART 

# Start with small values of control parameters
library(rpart)
cont.parms <- rpart.control(minsplit = 10,cp = 0.002)
Strips.rp <-  rpart("R ~.",data = Training_set1, control =cont.parms, method = "anova")
plotcp(Strips.rp) 
printcp(Strips.rp)


## Control parameters based on output of plotcp() and printcp()

# Initialize an empty list to store the results. I want 3 values to plot later. 
D_tree <- list()

# Set the number of runs
num_runs <- 3   ## rum model multiple times 

for (i in 1:num_runs) {
  # Set up the model and control parameters
  cont.parms <- rpart.control(minsplit = 5, cp = 0.024)
  
  # Train the model
  Strips.rp <- rpart("R ~.", data = Training_set1, control = cont.parms, method = "class")
  
  # Make predictions on the test set
  Pred <- predict(Strips.rp, type = "class", newdata = Testing_set1)
  Pred <- as.data.frame(Pred)
  Pred$Actual <- Testing_set1$R
  
  # Create a confusion matrix
  cm_test <- table(Pred)
  
  # Calculate metrics and store in the results list
  D_tree[[i]] <- metrics(cm_test)
  
  # Combine results from all runs into a data frame
  D_tree <- do.call(rbind, results)
  D_tree <- as.data.frame(D_tree)
}


## Classification tree and Area under the curve

#fallen leaves. 
library(rpart.plot)
rpart.plot(Strips.rp, digits = 3, fallen.leaves = T, type=3, extra=101, cex=0.5,
           main = "Classification Tree For Hybrid Reliability") 

#Plots
library(caret)
Strip.rp <-  rpart("R ~ .", data = Training_set1,
                   control=  cont.parms, method = "class")

pred.test <-predict(Strip.rp, type = "class",newdata = Testing_set1)
class(pred.test)
pred.test <- as.numeric(as.character(pred.test)) #change it to numeric


library(ROCR)
predROCR = prediction(pred.test, Testing_set1$R)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values


#vALIDATION
pred.test2 <-predict(Strip.rp, type = "class",newdata = Validation_Phase1)
Pred2 <- as.data.frame(pred.test2)
Pred2$Actual <- Validation_Phase1$R
cm_test2 <-table(Pred2)

metrics <- function(cm) {
  accuracy <- sum(diag(cm))/sum(cm)
  c(accuracy = accuracy)}

metrics(cm_test2)  #Accuracy of validation set

##Area under the curve
library(ROCR)
pred.test2 <- as.numeric(as.character(pred.test2)) #change it to numeric
predROCR = prediction(pred.test2, Validation_Phase1$R)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values








###______________________________RANDOM FOREST VANILLA ____________________________________________

library(randomForest)
# Set the number of runs
R_Forest <- list()
num_runs <- 3  #run model multiple times and extact metrics 

#set the metric extraction
metrics <- function(cm) {
  accuracy <- sum(diag(cm))/sum(cm)
  precision <- cm[2,2]/sum(cm[2,])
  recall <- cm[2,2]/sum(cm[ ,2])
  baseline<- sum(cm[1,])/sum(cm)
  c(accuracy = accuracy, precision = precision, recall = recall, baseline = baseline)
}

for (i in 1:num_runs) {
  # Set up the Random Forest model
  Model_RF <- randomForest(R ~ STATE + HYB + FS + CC + WRT + Density + CI + CD + PER +
                             SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + 
                             WPC1 + WPC2 + WPC3 + WPC4, data = Training_set1, 
                           importance = TRUE, ntree = 200, nodesize = 25)
  
  # Make predictions on the test set
  pred_test <- predict(Model_RF, type = "response", newdata = Testing_set1)
  pred_test <- as.data.frame(pred_test)
  
  # Apply threshold to predict binary outcomes
  pred_test$pred_test <- ifelse(pred_test[, 1] > 0.48, "1", "0")
  
  # Add actual values
  pred_test$Actual <- Testing_set1$R
  
  # Create a confusion matrix
  cm_test <- table(pred_test$pred_test, pred_test$Actual)
  # Calculate metrics and store in the results list
  R_Forest[[i]] <- metrics(cm_test)
  R_Forest_df <- do.call(rbind, R_Forest)
  RF_Results  <- as.data.frame(R_Forest_df)
}
RF_Results$accuracy


### b. Plot the explanatory variable importance to identify what are the most influential variables to classify oak presence.
var_imp <- varImp(Model_RF)
varImpPlot(Model_RF,   
           main = "Random Forest Variable Importance")
attributes(Model_RF)

### c. Estimate area under the curve

library(caret)
library(ROCR)
pred_test <- predict(Model_RF ,type = "class",  newdata = Testing_set1)

predROCR = prediction(pred_test, Testing_set1$R)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

performance(predROCR, "auc")@y.values

## Validation
pred.val2 <-predict(Model_RF ,type = "class",newdata = Validation_Phase1)
pred.val2  <- as.data.frame(pred.val2)
pred.val2$pred.val2 <-ifelse(pred.val2[,1] > 0.48,"1","0")
pred.val2$Actual <- Validation_Phase1$R
cm_val2 <-table(pred.val2)

metrics <- function(cm) {
  accuracy <- sum(diag(cm))/sum(cm)
  c(accuracy = accuracy)}

metrics(cm_val2)  #Accuracy of validation set

##Area under the curve
library(ROCR)
pred.test2 <- as.numeric(as.character(pred.test2)) #change it to numeric
predROCR = prediction(pred.test2, Validation_Phase1$R)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values





####### Using Cross Validation and Bootstrapping####

# Train a RF model
library(caret)
model_RF2 <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + 
                     SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                   data= Training_set1,  method="rf")

predict_RF <- predict(model_RF2,Testing_set1)

predict_RF <- as.data.frame(predict_RF)

predict_RF$Pred <-ifelse(predict_RF [,1] > 0.46,"1","0")
predict_RF$Actual <- Testing_set1$R



cm2 <- table(predict_RF$Pred, predict_RF$Actual)


## Calculating metrics from confusion matrix ##
metrics <- function(cm) {
  accuracy <- sum(diag(cm))/sum(cm)
  precision <- cm[2,2]/sum(cm[2,])
  recall <- cm[2,2]/sum(cm[ ,2])
  baseline<- sum(cm[1,])/sum(cm)
  c(accuracy = accuracy, precision = precision, recall = recall, baseline = baseline)
}

metrics(cm2)



##-----------------RESAMPLING ALGOLITHMS--------------------------------------
  
#### Training model using different re-sampling methods such as k-fold cv, bootstrapping and oth
library(dplyr)
library(caret)
library(adabag) #for Adaptive Boosting (AdaBoost)
library(randomForest)
library(earth)
library(gbm) #for gradient boosting 
library(e1071) #for naive beyes, svm, 
library(rpart)
library(ggplot2)

Model1   <- read.csv("Model1.csv", header = TRUE)  ##changed NA wRT to medium, and HW CI. CD, PER NAs to previous HW data

Model1$R            <- as.factor(Model1$R )
Model1$HYB    <- as.factor(Model1$ HYB)
Model1$STATE    <- as.factor(Model1$STATE )
Model1$FS    <- as.factor(Model1$FS )
Model1$CC    <- as.factor(Model1$CC)
Model1$WRT    <- as.factor(Model1$WRT )
Model1$Density    <- as.factor(Model1$Density )
Model1$CI    <- as.factor(Model1$CI )
Model1$CD    <- as.factor(Model1$CD )
Model1$PER    <- as.factor(Model1$PER )


Training_Phase2 <- Model1  %>%
  filter(YEAR %in% c(2018, 2019, 2020, 2021))
Training_Phase2  <- Training_Phase2[-c(4:5)] #remove year and rep

# Split training_phase data into training and testing
set.seed(123)
split_index <- createDataPartition(Training_Phase2$R, p = 0.8, list = FALSE)
Training_set2 <- Training_Phase2[split_index, ]
Testing_set2 <- Training_Phase2[-split_index, ]


## Using different Ensemble Techniques 
## configuring train controls
model1 <- trainControl(method = "boot", number = 20, search = "random")
model2 <- trainControl(method = "boot632", number = 20, search = "random")
model3 <- trainControl(method = "optimism_boot", number = 20, search = "random")
model4 <- trainControl(method = "boot_all", number = 20, search = "random")
model5 <- trainControl(method = "cv", number = 20, search = "random")
model6 <- trainControl(method = "repeatedcv", number = 20,repeats = 10, search = "random")
model7 <- trainControl(method = "LGOCV", number = 20, search = "random")
model8 <- trainControl(method = "adaptive_cv", number = 20,repeats = 10, search = "random")
model9 <- trainControl(method = "adaptive_boot", number = 20, search = "random")




###______________________ 1. Using Random forest_________________________________

RF_boot <- train(R ~ HYB + STATE + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                      data = Training_set2, trControl=model1, method="rf")

RF_boot632 <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                       data = Training_set2, trControl=model2, method="rf")

RF_Optboot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                       data = Training_set2, trControl=model3, method="rf")

RF_bootall  <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                        data = Training_set2, trControl=model4, method="rf")

RF_cv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                  data = Training_set2, trControl=model5, method="rf")

RF_repcv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                     data = Training_set2, trControl=model6, method="rf")

RF_lGOCV <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                     data = Training_set2, trControl=model7, method="rf")

RF_adptcv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                      data = Training_set2, trControl=model8, method="rf")

RF_adtboot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                       data = Training_set2, trControl=model9, method="rf")

# extract accuracies from models
RF_adtboot$results
RF_boot$results
RF_boot632$results
RF_Optboot$results
RF_cv$results
RF_adptcv$results
RF_LGOCV$results
RF_repcv$results

# Extract 'Accuracy' metric in the 'results'
models_RF <- list(
  RF_Adtboot = RF_adtboot$results$Accuracy,
  RF_Boot    = RF_boot$results$Accuracy,
  RF_Boot632 = RF_boot632$results$Accuracy,
  RF_Optboot = RF_Optboot$results$Accuracy,
  RF_CV      = RF_cv$results$Accuracy,
  RF_Adptcv  = RF_adptcv$results$Accuracy,
  RF_LGOCV   = RF_lGOCV$results$Accuracy,
  RF_Repcv   = RF_repcv$results$Accuracy
)

# Convert the list to a data frame for easy plotting
RF_final <- data.frame(Model = rep(names(models_RF), sapply(models_RF, length)),
                        Accuracy = unlist(models_RF))

# Plotting using ggplot2 (you may need to install ggplot2 if you haven't)
library(ggplot2)

# Boxplot of model accuracies
ggplot(RF_final, aes(y = Model, x = Accuracy, fill = Model)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  labs(title = "Random Forest Model Comparisons", x = "Accuracy", y = "Resampling Method") +
  theme_minimal()+
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size=1))+
  theme(legend.position = "none")


#___________________glm (Generalized Linear Model (Logistic regression) ______________________
## Train and compare the models Using Random forest
glm_boot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                  data = Training_set2, trControl=trControl_list$boot, method="glm")

glm_boot632 <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                     data = Training_set2, trControl=model2, method="glm")

glm_Optboot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                     data = Training_set2, trControl=model3, method="glm")

glm_bootall  <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                      data = Training_set2, trControl=model4, method="glm")

glm_cv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                data = Training_set2, trControl=model5, method="glm")

glm_repcv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                   data = Training_set2, trControl=model6, method="glm")

glm_lGOCV <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                   data = Training_set2, trControl=model7, method="glm")

glm_adptcv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                    data = Training_set2, trControl=model8, method="glm")

glm_adtboot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                     data = Training_set2, trControl=model9, method="gml")

# extract accuracies from models
glm_adtboot$results
glm_boot$results
glm_boot632$results
glm_Optboot$results
glm_cv$results
glm_adptcv$results
glm_LGOCV$results
glm_repcv$results



# Assuming each model has an 'Accuracy' metric in the 'results'
  glm_models <- list(
  glm_Adtboot = glm_adtboot$results$Accuracy,
  glm_Boot    = glm_boot$results$Accuracy,
  glm_Boot632 = glm_boot632$results$Accuracy,
  glm_Optboot = glm_Optboot$results$Accuracy,
  glm_CV      = glm_cv$results$Accuracy,
  glm_Adptcv  = glm_adptcv$results$Accuracy,
  glm_LGOCV   = glm_lGOCV$results$Accuracy,
  glm_Repcv   = glm_repcv$results$Accuracy
)

# Convert the list to a data frame for easy plotting
  glm_final <- data.frame(Model = rep(names(glm_models), sapply(glm_models, length)),
                  Accuracy = unlist(glm_models))

# Plotting using ggplot2 (you may need to install ggplot2 if you haven't)
library(ggplot2)

###Boxplot of model accuracies
ggplot( glm_final, aes(y = Model, x = Accuracy, fill = Model )) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  labs(title = "GLB Model Comparisons", x = "Accuracy") +
  theme_minimal()+
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size=1))+
  theme(legend.position = "none")




#___________________AdaBoost______________________
## Train and compare the models Using Random forest
install.packages("htmltools")
library(adabag)
AdaBoost_boot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                  data = Training_set2, trControl=model1, method="ada")

AdaBoost_boot632 <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                     data = Training_set2, trControl=model2, method="ada")

AdaBoost_Optboot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                     data = Training_set2, trControl=model3, method="ada")

AdaBoost_cv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                data = Training_set2, trControl=model5, method="ada")

AdaBoost_repcv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                   data = Training_set2, trControl=model6, method="ada")

AdaBoost_lGOCV <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                   data = Training_set2, trControl=model7, method="ada")

AdaBoost_adptcv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                    data = Training_set2, trControl=model8, method="ada")

AdaBoost_adtboot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                     data = Training_set2, trControl=model9, method="ada")

# extract accuracies from models
ada_adtboot$results
ada_boot$results
ada_boot632$results
ada_Optboot$results
ada_cv$results
ada_adptcv$results
ada_LGOCV$results
ada_repcv$results



# Assuming each model has an 'Accuracy' metric in the 'results'
  ada_models <- list(
  ada_Adtboot = ada_adtboot$results$Accuracy,
  ada_Boot    = ada_boot$results$Accuracy,
  ada_Boot632 = ada_boot632$results$Accuracy,
  ada_Optboot = ada_Optboot$results$Accuracy,
  ada_CV      = ada_cv$results$Accuracy,
  ada_Adptcv  = ada_adptcv$results$Accuracy,
  ada_LGOCV   = ada_lGOCV$results$Accuracy,
  ada_Repcv   = ada_repcv$results$Accuracy
)

# Convert the list to a data frame for easy plotting
ada_final <- data.frame(Model = rep(names(ada_models), sapply(ada_models, length)),
                  Accuracy = unlist(ada_models))

# Plotting using ggplot2 (you may need to install ggplot2 if you haven't)
library(ggplot2)

###Boxplot of model accuracies
ggplot(ada_final, aes(y = Model, x = Accuracy, fill = Model )) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  labs(title = "KNN Model Comparisons", x = "Accuracy") +
  theme_minimal()+
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size=1))+
  theme(legend.position = "none")



###________________________Naive Bayes_______________
## Train and compare the models Using Random forest
NB_boot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                    data = Training_set2, trControl=model1, method="naive_bayes")

NB_boot632 <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                       data = Training_set2, trControl=model2, method="naive_bayes")

NB_Optboot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                       data = Training_set2, trControl=model3, method="naive_bayes")

NB_bootall  <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                        data = Training_set2, trControl=model4, method="naive_bayes")

NB_cv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                  data = Training_set2, trControl=model5, method="naive_bayes")

NB_repcv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                     data = Training_set2, trControl=model6, method="naive_bayes")

NB_lGOCV <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                     data = Training_set2, trControl=model7, method="naive_bayes")

NB_adptcv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                      data = Training_set2, trControl=model8, method="naive_bayes")

NB_adtboot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                       data = Training_set2, trControl=model9, method="naive_bayes")


# extract accuracies from models
NB_adtboot$results
NB_boot$results
NB_boot632$results
NB_Optboot$results
NB_cv$results
NB_adptcv$results
NB_LGOCV$results
NB_repcv$results



# Assuming each model has an 'Accuracy' metric in the 'results'
NB_models <- list(
  NB_Adtboot = NB_adtboot$results$Accuracy,
  NB_Boot    = NB_boot$results$Accuracy,
  NB_Boot632 = NB_boot632$results$Accuracy,
  NB_Optboot = NB_Optboot$results$Accuracy,
  NB_CV      = NB_cv$results$Accuracy,
  NB_Adptcv  = NB_adptcv$results$Accuracy,
  NB_LGOCV   = NB_lGOCV$results$Accuracy,
  NB_Repcv   = NB_repcv$results$Accuracy
)

# Convert the list to a data frame for easy plotting
models_df <- data.frame(Model = rep(names(models), sapply(models, length)),
                        Accuracy = unlist(models))

# Plotting using ggplot2 (you may need to install ggplot2 if you haven't)
library(ggplot2)

# Boxplot of model accuracies
ggplot(models_df, aes(y = Model, x = Accuracy, fill = Model )) +
  geom_boxplot() +
  labs(title = "Model Comparisons", x = "Accuracy") +
  theme_minimal()+
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size=1))+
  theme(legend.position = "none")




## _______________Gradient Boosting ______________________
## Train and compare the models Using Random forest
model_boot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                    data = Training_set2, trControl=model1, method="rf")

model_boot632 <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                       data = Training_set2, trControl=model2, method="rf")

model_Optboot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                       data = Training_set2, trControl=model3, method="knn")

model_bootall  <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                        data = Training_set2, trControl=model4, method="rf")

model_cv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                  data = Training_set2, trControl=model5, method="rf")

model_repcv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                     data = Training_set2, trControl=model6, method="rf")

model_LGOCV <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                     data = Training_set2, trControl=model7, method="rf")

model_adptcv <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                      data = Training_set2, trControl=model8, method="rf")

model_adtboot <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
                       data = Training_set2, trControl=model9, method="rf")

#model_svm <- train(R ~ HYB + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3,
#data = Training_set2, trControl=model3, method="svmRadial")


# extract accuracies from models
model_adtboot$results
model_boot$results
model_boot632$results
model_Optboot$results
model_cv$results
model_adptcv$results
#model_svm$results
model_LGOCV$results
model_repcv$results



# Assuming each model has an 'Accuracy' metric in the 'results'
models <- list(
  Adtboot = model_adtboot$results$Accuracy,
  Boot = model_boot$results$Accuracy,
  Boot632 = model_boot632$results$Accuracy,
  Optboot = model_Optboot$results$Accuracy,
  CV = model_cv$results$Accuracy,
  Adptcv = model_adptcv$results$Accuracy,
  LGOCV = model_LGOCV$results$Accuracy,
  Repcv = model_repcv$results$Accuracy,
  D_Tree = D_tree$accuracy,
  R_Forest = RF_Results$accuracy
)

# Convert the list to a data frame for easy plotting
models_df <- data.frame(Model = rep(names(models), sapply(models, length)),
                        Accuracy = unlist(models))

# Plotting using ggplot2 (you may need to install ggplot2 if you haven't)
library(ggplot2)

# Boxplot of model accuracies
ggplot(models_df, aes(y = Model, x = Accuracy, fill = Model )) +
  geom_boxplot() +
  labs(title = "Model Comparisons", x = "Accuracy") +
  theme_minimal()+
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size=1))+
  theme(legend.position = "none")





# USING LOOPS TO CONSOLIDATE ALL MODELS 

## Set the model first
library(caret)
LR_Model <- R ~ HYB + STATE + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3

# 1. Random Forest 
RF_models <- c("RF_adtboot",
               "RF_boot",
               "RF_boot632",
               "RF_Optboot",
               "RF_cv",
               "RF_adptcv",
               "RF_LGOCV",
               "RF_repcv")

# Create a named list of train controls
trControl_list <- list(
  RF_adtboot = trainControl(method = "adaptive_boot", number = 20, search = "random"),
  RF_boot = trainControl(method = "boot", number = 20, search = "random"),
  RF_boot632 = trainControl(method = "boot632", number = 20, search = "random"),
  RF_Optboot = trainControl(method = "optimism_boot", number = 20, search = "random"),
  RF_cv = trainControl(method = "cv", number = 20, search = "random"),
  RF_adptcv = trainControl(method = "adaptive_cv", number = 20, repeats = 10, search = "random"),
  RF_LGOCV = trainControl(method = "LGOCV", number = 20, search = "random"),
  RF_repcv = trainControl(method = "repeatedcv", number = 20, repeats = 10, search = "random")
)

# Initialize an empty data frame to store results
Rf_final <- data.frame(Model = character(), Accuracy = numeric())

# Run loop for Random Forest models and store results
for (model_name in RF_models) {
  model <- train(LR_Model, data = Training_set2, trControl = trControl_list[[model_name]], method = "rf")
  accuracy <- model$results$Accuracy
  Rf_final <- rbind(Rf_final, data.frame(Model = model_name, Accuracy = accuracy))
}

# Reorder factor levels of Model in DT_final
RF_final$Model <- factor(RF_final$Model, levels = RF_models)

# Plotting using ggplot2
RF_plot <- ggplot(Rf_final, aes(y = Model, x = Accuracy, fill = Model)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  labs(title = "Random Forest Model Comparisons", x = "Accuracy", y = "Resampling Method") +
  theme_minimal() +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size=1)) +
  theme(legend.position = "none")
RF_plot 



# 2. Decision Tree 
DT_models <- c("DT_adtboot",
               "DT_boot",
               "DT_boot632",
               "DT_Optboot",
               "DT_cv",
               "DT_adptcv",
               "DT_LGOCV",
               "DT_repcv")
trControl_list <- list(
  DT_adtboot = trainControl(method = "adaptive_boot", number = 20, search = "random"),
  DT_boot = trainControl(method = "boot", number = 20, search = "random"),
  DT_boot632 = trainControl(method = "boot632", number = 20, search = "random"),
  DT_Optboot = trainControl(method = "optimism_boot", number = 20, search = "random"),
  DT_cv = trainControl(method = "cv", number = 20, search = "random"),
  DT_adptcv = trainControl(method = "adaptive_cv", number = 20, repeats = 10, search = "random"),
  DT_LGOCV = trainControl(method = "LGOCV", number = 20, search = "random"),
  DT_repcv = trainControl(method = "repeatedcv", number = 20, repeats = 10, search = "random"))

DT_final <- data.frame(Model = character(), Accuracy = numeric())

for (model_name in DT_models) {
  model <- train(LR_Model, data = Training_set2, trControl = trControl_list[[model_name]], method = "rpart")
  accuracy <- model$results$Accuracy
  DT_final <- rbind(DT_final, data.frame(Model = model_name, Accuracy = accuracy))
}

DT_final$Model <- factor(DT_final$Model, levels = DT_models)
DT_plot <- ggplot(DT_final, aes(y = Model, x = Accuracy, fill = Model)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  labs(title = "Decision Tree Model Comparisons", x = "Accuracy", y = "Resampling Method") +
  theme_minimal() +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size=1)) +
  theme(legend.position = "none")
DT_plot 


## 3. Naive Bayes 
NB_models <- c("NB_adtboot",
               "NB_boot",
               "NB_boot632",
               "NB_Optboot",
               "NB_cv",
               "NB_adptcv",
               "NB_LGOCV",
               "NB_repcv")

trControl_list <- list(
  NB_adtboot = trainControl(method = "adaptive_boot", number = 20, search = "random"),
  NB_boot = trainControl(method = "boot", number = 20, search = "random"),
  NB_boot632 = trainControl(method = "boot632", number = 20, search = "random"),
  NB_Optboot = trainControl(method = "optimism_boot", number = 20, search = "random"),
  NB_cv = trainControl(method = "cv", number = 20, search = "random"),
  NB_adptcv = trainControl(method = "adaptive_cv", number = 20, repeats = 10, search = "random"),
  NB_LGOCV = trainControl(method = "LGOCV", number = 20, search = "random"),
  NB_repcv = trainControl(method = "repeatedcv", number = 20, repeats = 10, search = "random"))

NB_final <- data.frame(Model = character(), Accuracy = numeric())
for (model_name in NB_models) {
  model <- train(LR_Model, data = Training_set2, trControl = trControl_list[[model_name]], method = "naive_bayes")
  accuracy <- model$results$Accuracy
  NB_final <- rbind(NB_final, data.frame(Model = model_name, Accuracy = accuracy))
}

NB_final$Model <- factor(NB_final$Model, levels = NB_models)
NB_plot <- ggplot(NB_final, aes(y = Model, x = Accuracy, fill = Model)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  labs(title = "Naive Bayes Model Comparisons", x = "Accuracy", y = "Resampling Method") +
  theme_minimal() +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size=1)) +
  theme(legend.position = "none")
NB_plot 


## 4. Gradient Boosting

NB_models <- c("NB_adtboot",
               "NB_boot",
               "NB_boot632",
               "NB_Optboot",
               "NB_cv",
               "NB_adptcv",
               "NB_LGOCV",
               "NB_repcv")

trControl_list <- list(
  NB_adtboot = trainControl(method = "adaptive_boot", number = 20, search = "random"),
  NB_boot = trainControl(method = "boot", number = 20, search = "random"),
  NB_boot632 = trainControl(method = "boot632", number = 20, search = "random"),
  NB_Optboot = trainControl(method = "optimism_boot", number = 20, search = "random"),
  NB_cv = trainControl(method = "cv", number = 20, search = "random"),
  NB_adptcv = trainControl(method = "adaptive_cv", number = 20, repeats = 10, search = "random"),
  NB_LGOCV = trainControl(method = "LGOCV", number = 20, search = "random"),
  NB_repcv = trainControl(method = "repeatedcv", number = 20, repeats = 10, search = "random"))

NB_final <- data.frame(Model = character(), Accuracy = numeric())
for (model_name in NB_models) {
  model <- train(LR_Model, data = Training_set2, trControl = trControl_list[[model_name]], method = "naive_bayes")
  accuracy <- model$results$Accuracy
  NB_final <- rbind(NB_final, data.frame(Model = model_name, Accuracy = accuracy))
}

NB_final$Model <- factor(NB_final$Model, levels = NB_models)
NB_plot <- ggplot(NB_final, aes(y = Model, x = Accuracy, fill = Model)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  labs(title = "Naive Bayes Model Comparisons", x = "Accuracy", y = "Resampling Method") +
  theme_minimal() +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size=1)) +
  theme(legend.position = "none")
NB_plot 




