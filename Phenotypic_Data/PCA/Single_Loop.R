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

## Imported from python 
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


###______________________________RANDOM FOREST VANILLA ____________________________________________

# import cleaned data
Model1   <- read.csv("Model2.csv", header = TRUE)  ##changed NA wRT to medium, and HW CI. CD, PER NAs to previous HW data

library(dplyr)
Training_Phase <- Model1  %>%
  filter(YEAR %in% c(2018, 2019, 2020, 2021))

Training_Phase$R <- ifelse(Training_Phase$R == "High", "1", "0")
Training_Phase  <- Training_Phase[-c(4:5)] #remove year and rep
Training_Phase$R <- as.numeric(Training_Phase$R)


# Split training_phgase data into training and testing
library(caret)
set.seed(123)
split_index <- createDataPartition(Training_Phase$R, p = 0.8, list = FALSE)
Training_set1 <- Training_Phase[split_index, ]
Testing_set1 <- Training_Phase[-split_index, ]

#set the metric extraction
metrics <- function(cm) {
  accuracy <- sum(diag(cm))/sum(cm)
  precision <- cm[2,2]/sum(cm[2,])
  recall <- cm[2,2]/sum(cm[ ,2])
  baseline<- sum(cm[1,])/sum(cm)
  c(accuracy = accuracy, precision = precision, recall = recall, baseline = baseline)
}

library(randomForest)
# Set the number of runs
R_Forest <- list()
num_runs <- 3  #run model multiple times and extract metrics 
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
  pred_test$pred_test <- ifelse(pred_test[, 1] > 0.7, "1", "0")
  
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


## Identify The influential variables.

#var_imp <- varImp(Model_RF)
#varImpPlot(Model_RF, main = "Random Forest Variable Importance")

var_imp <- as.data.frame(importance(Model_RF))

library(tibble)  #change rowname to column
var_imp <- rownames_to_column(var_imp, var = "Variable")
var_imp <- var_imp[order(var_imp$`%IncMSE`), ] #rearrange
var_imp$Variable <- reorder(var_imp$Variable, var_imp$`%IncMSE`)

MSE <- ggplot(var_imp, aes(x = var_imp$`%IncMSE` , y = Variable)) +
       geom_point(stat = "identity", fill = "skyblue", color = "black") +
       labs(title = "",
            y = "Variables",
            x = "%MSE Increase") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 0, hjust = 1))+
       scale_x_continuous(breaks = seq(0, max(var_imp$`%IncMSE`), by = 5))

## Node purity
var_imp <- var_imp[order(var_imp$IncNodePurity), ] #rearrange
var_imp$Variable <- reorder(var_imp$Variable, var_imp$IncNodePurity)

Purity <-ggplot(var_imp, aes(x = var_imp$IncNodePurity , y = Variable)) +
         geom_point(stat = "identity", fill = "skyblue", color = "black") +
         labs(title = "",
              y = "",
              x = "%NodePurity Increase") +
         theme_minimal() +
         theme(axis.text.x = element_text(angle = 0, hjust = 1))+
         scale_x_continuous(breaks = seq(0, max(var_imp$`%IncMSE`), by = 5))

library(gridExtra)
combined_plot <- grid.arrange(MSE, Purity, ncol = 2, top = "Variable Importance") 

### c. Estimate area under the curve
library(caret)
library(ROCR)
pred_test <- predict(Model_RF ,type = "class",  newdata = Testing_set1)
predROCR = prediction(pred_test, Testing_set1$R)
perfROCR = performance(predROCR, "tpr", "fpr")
auc <- as.numeric(performance(predROCR, "auc")@y.values[[1]])
plot(perfROCR, colorize=TRUE)
text(0.5, 0.3, paste("AUC =", round(auc, 2)), col = "black", cex = 1.5)


##_________ Comparing other ensemble methods____________

## Import Libraries
library(caret)
library(ggplot2)
library(dplyr)
library(caret)
library(adabag) #for Adaptive Boosting (AdaBoost)
library(randomForest)
library(earth)
library(gbm) #for gradient boosting 
library(e1071) #for naive beyes, svm, 
library(rpart) #for decision tree

## Import data
Model1   <- read.csv("Model1.csv", header = TRUE)  ##changed NA wRT to medium, and HW CI. CD, PER NAs to previous HW data

Training_Phase2 <- Model1  %>%
  filter(YEAR %in% c(2018, 2019, 2020, 2021))
Training_Phase2  <- Training_Phase2[-c(4:5)] #remove year and rep

# Split training_phase data into training and testing
set.seed(123)
split_index <- createDataPartition(Training_Phase2$R, p = 0.8, list = FALSE)
Training_set2 <- Training_Phase2[split_index, ]
Testing_set2 <- Training_Phase2[-split_index, ]

## Set logistic regression model to be fit
LR_Model <- R ~ HYB + STATE + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3

## Create a named list of resampling methods and train controls
resampling <- c("adtboot", "boot","boot632","Optboot","cv","LGOCV") #"adptcv" and "repcv" removed since they give trouble with bagging ensemble

trControl_list <- list(
  adtboot = trainControl(method = "adaptive_boot", number = 20,              search = "grid"),
  boot = trainControl(   method = "boot",          number = 20,              search = "grid"),
  boot632 = trainControl(method = "boot632",       number = 20,              search = "grid"),
  Optboot = trainControl(method = "optimism_boot", number = 20,              search = "grid"),
  cv = trainControl(     method = "cv",            number = 20,              search = "grid"),
  adptcv = trainControl( method = "adaptive_cv",   number = 20, repeats = 10,search = "grid"),
  LGOCV = trainControl(  method = "LGOCV",         number = 20,              search = "grid"),
  repcv = trainControl(  method = "repeatedcv",    number = 20, repeats = 10,search = "grid"))

## Create a list of models to be fit (ensemble methods)
Ensemble <- c( "AdaBoost","Random_Forest", "Decision_Trees", "Naive_Bayes", "Gradient_Boosting")
Method <- c( "ada",  "rf",  "rpart", "naive_bayes", "xgbTree")

# Initialize a data frame to store accuracy results
Model_Accuracy <- data.frame(Ensemble = character(), Method = character(), Resampling = character(), Accuracy = numeric())

# Run the loop for each model
for (model_index in seq_along(Ensemble)) {
  model_name <- Ensemble[model_index]
  method <- Method[model_index]
  
  for (sample in resampling) {
    # Fit the model
    model <- train(LR_Model, data = Training_set2, trControl = trControl_list[[sample]], method = method)
    
    # Get the accuracy from model results
    accuracy <- model$results$Accuracy
    
    # Append results to the data frame
    Model_Accuracy <- rbind(Model_Accuracy, data.frame(Ensemble = model_name, Method = method, Resampling = sample, Accuracy = accuracy))
  }
}
Model_Accuracy$Resampling <- factor(Model_Accuracy$Resampling , levels = resampling)

## Plotting using ggplot2
ggplot(Model_Accuracy, aes(x = Accuracy, y = Resampling, fill = Resampling)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  facet_wrap(~ Ensemble, scales = "free_y", ncol = 2) +
  labs(title = "Machine Learning Model Comparisons", x = "Resampling Method", y = "Accuracy") +
  theme_minimal() +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size = 1)) +
  theme(legend.position = "none")


## 6. Run Bagging alone since it doesnt accept adaptive bootstrapping

Bagging_Models <- data.frame(Ensemble = character(), Method = character(), Resampling = character(), Accuracy = numeric())

Ensemble_Bag <- "Bagging"
Method_bag <- "treebag"

Bagging_Resampling <- c(
  "boot",
  "boot632",
  "Optboot",
  "cv",
  "LGOCV")

Bagging_trcontrol <- list(boot = trainControl(   method = "boot",          number = 20,              search = "grid"),
                          boot632 = trainControl(method = "boot632",       number = 20,              search = "grid"),
                          Optboot = trainControl(method = "optimism_boot", number = 20,              search = "grid"),
                          cv = trainControl(     method = "cv",            number = 20,              search = "grid"),
                          LGOCV = trainControl(  method = "LGOCV",         number = 20,              search = "grid"))
for (i in 1:20) {
  for (model_name in Bagging_Resampling) {
    model <- train(LR_Model, data = Training_set2, trControl = Bagging_trcontrol[[model_name]], method = "treebag")
    accuracy <- model$results$Accuracy
    Bagging_Models <- rbind(Bagging_Models, data.frame( Model = model_name, Accuracy = accuracy))
  }
}


for (i in 1:20) {
  for (model_index in seq_along(Ensemble_Bag)) {
  model_name <- Ensemble_Bag[model_index]
  method <- Method_bag[model_index]
     for (sample in Bagging_Resampling) {
    # Fit the model
    model <- train(LR_Model, data = Training_set2, trControl = Bagging_trcontrol[[sample]], method = method)
    accuracy <- model$results$Accuracy
    Bagging_Models <- rbind(Bagging_Models, data.frame(Ensemble = model_name, Method = method, Resampling = sample, Accuracy = accuracy))
     }
  }
}


Bagging_Models$Resampling  <- factor(Bagging_Models$Resampling, levels = Bagging_Resampling)

Bagging_Models_plot <- ggplot(Bagging_Models, aes(y = Resampling, x = Accuracy, fill =  Resampling)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  labs(title = "Bagging Model Comparisons", x = "Accuracy", y = "Resampling Method") +
  theme_minimal() +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size=1)) +
  theme(legend.position = "none")

Bagging_Models_plot

#add Bagging to the rest of the model results
write.csv(Bagging_Models, "Bagging_Models.csv", row.names = F)  
Bagging_Models <- read.csv("Bagging_Models.csv")

Bagging_Models$Resampling <- factor(Bagging_Models$Resampling, levels = Bagging_Resampling)

Model_Accuracy2 <- rbind(Model_Accuracy, Bagging_Models)

## Plotting using ggplot2
ggplot(Model_Accuracy2, aes(x = Accuracy, y = Resampling, fill = Resampling)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  facet_wrap(~ Ensemble, scales = "free_y", ncol = 2) +
  labs(title = "Machine Learning Model Comparisons", x = "Resampling Method", y = "Accuracy") +
  theme_minimal() +
  theme_bw() +
  theme(panel.background = element_rect(colour = "black", size = 1)) +
  theme(legend.position = "none")


## Extract best models and run validation using Testing set

## i dont think this is the est way. I need to find how to extact the trained model and use it for test

RF_Optboot <- train(R ~ HYB + STATE + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                 data = Training_set2, trControl= trControl_list$Optboot, method="rf")

RF_boot632 <- train(R ~ HYB + STATE + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                    data = Training_set2, trControl= trControl_list$boot632, method="rf")

Bagging_Optboot <- train(R ~ HYB + STATE + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                 data = Training_set2, trControl= trControl_list$Optboot, method="treebag")

Bagging_boot632 <- train(R ~ HYB + STATE + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                    data = Training_set2, trControl= trControl_list$boot632, method="treebag")

GB_Optboot <- train(R ~ HYB + STATE + FS + CC + WRT + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                 data = Training_set2, trControl= trControl_list$Optboot, method="xgbTree")


# Validate models using the Testing set
Predicted_RF_Optboot       <- predict(RF_Optboot     , newdata = Testing_set2)
Predicted_RF_boot632       <- predict(RF_boot632     , newdata = Testing_set2)
Predicted_Bagging_Optboot  <- predict(Bagging_Optboot, newdata = Testing_set2)
Predicted_Bagging_boot632  <- predict(Bagging_boot632, newdata = Testing_set2)
Predicted_GB_Optboot        <- predict(GB_Optboot     , newdata = Testing_set2)

# Combine predictions with actual values
Predicted_df <- data.frame(
                Farm              = Testing_set2$FC,
                Hybrid            = Testing_set2$HYB,
                Actual            = Testing_set2$R,
                RF_Optboot        = Predicted_RF_Optboot,
                RF_boot632        = Predicted_RF_boot632,
                Bagging_Optboot   = Predicted_Bagging_Optboot,
                Bagging_boot632   = Predicted_Bagging_boot632,
                GB_Optboot        = Predicted_GB_Optboot)
                

# Confusion matrix and metrics for each model
cm_RF_Optboot      <- table(Predicted_df$RF_Optboot     ,  Predicted_df$Actual)
cm_RF_boot632      <- table(Predicted_df$RF_boot632     ,  Predicted_df$Actual)
cm_Bagging_Optboot <- table(Predicted_df$Bagging_Optboot,  Predicted_df$Actual)
cm_Bagging_boot632 <- table(Predicted_df$Bagging_boot632,  Predicted_df$Actual)
cm_GB_Optboot      <- table(Predicted_df$GB_Optboot     ,  Predicted_df$Actual)

Models = c("RF_Optboot", "RF_boot632", "Bagging_Optboot", "Bagging_boot632", "GB_Optboot" )

Metric <- rbind(metrics(cm_RF_Optboot     ), 
                metrics(cm_RF_boot632     ), 
                metrics(cm_Bagging_Optboot), 
                metrics(cm_Bagging_boot632), 
                metrics(cm_GB_Optboot     ))

Metric <- as.data.frame(Metric)
Validation <- cbind(Models, Metric)
write.csv(Validation, "Validation_Accuracy")




