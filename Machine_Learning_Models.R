## Machine Learning Workflow

## 1. Obtaining weather data using farmer cordinates.

library(daymetr)
df1 <- download_daymet(site = "Glazik2018",
                       lat = 40.443875,
                       lon = -88.2255508,
                       start = 2018,
                       end = 2018,
                       internal = TRUE,
                       simplify = TRUE)

df2 <- download_daymet(site = "Erisman2018",
                       lat = 39.4780916,
                       lon = -89.0052032,
                       start = 2018,
                       end = 2018,
                       internal = TRUE,
                       simplify = TRUE)

df3 <- download_daymet(site = "Buxton2018",
                       lat = 39.62134,
                       lon = -88.673871,
                       start = 2018,
                       end = 2018,
                       internal = TRUE,
                       simplify = TRUE)


df4 <- download_daymet(site = "Gray2018",
                       lat = 40.824304,
                       lon = -87.763472,
                       start = 2018,
                       end = 2018,
                       internal = TRUE,
                       simplify = TRUE)

df5 <- download_daymet(site = "Wilken2018",
                       lat = 40.85682,
                       lon = -88.0813341,
                       start = 2018,
                       end = 2018,
                       internal = TRUE,
                       simplify = TRUE)

df6 <- download_daymet(site = "Gruver2018",
                       lat = 40.666679,
                       lon = -90.75169,
                       start = 2018,
                       end = 2018,
                       internal = TRUE,
                       simplify = TRUE)

df7 <- download_daymet(site = "Ambriole2018",
                       lat = 40.9754707,
                       lon = -85.3479351,
                       start = 2018,
                       end = 2018,
                       internal = TRUE,
                       simplify = TRUE)

df8 <- download_daymet(site = "Dahnert2018",
                      lat = 42.921663,
                      lon = -88.793579,
                      start = 2018,
                      end = 2018,
                      internal = TRUE,
                      simplify = TRUE)

df9 <- download_daymet(site = "Adsit2018",
                      lat = 42.793254,
                      lon = -88.444177,
                      start = 2018,
                      end = 2018,
                      internal = TRUE,
                      simplify = TRUE)

df10 <- download_daymet(site = "Beiler2018",
                      lat = 42.850064,
                      lon = -90.360395,
                      start = 2018,
                      end = 2018,
                      internal = TRUE,
                      simplify = TRUE)

df11 <- download_daymet(site = "Stoltzfus2018",
                      lat = 42.735827,
                      lon = -90.042083,
                      start = 2018,
                      end = 2018,
                      internal = TRUE,
                      simplify = TRUE)

df12 <- download_daymet(site = "Zinniker2018",
                      lat = 42.700138,
                      lon = -88.444531,
                      start = 2018,
                      end = 2018,
                      internal = TRUE,
                      simplify = TRUE)

df13 <- download_daymet(site = "Doonan2019",
                      lat = 41.3356241,
                      lon = -90.6676853,
                      start = 2019,
                      end = 2019,
                      internal = TRUE,
                      simplify = TRUE)


df14 <- download_daymet(site = "Smith2019",
                      lat = 40.629017,
                      lon = -89.7754,
                      start = 2019,
                      end = 2019,
                      internal = TRUE,
                      simplify = TRUE)


df15 <- download_daymet(site = "Gruver2019",
                      lat = 40.6666053,
                      lon = -90.7516163,
                      start = 2019,
                      end = 2019,
                      internal = TRUE,
                      simplify = TRUE)

df16 <- download_daymet(site = "Ambriole2019",
                      lat = 41.355278,
                      lon = -85.596944,
                      start = 2019,
                      end = 2019,
                      internal = TRUE,
                      simplify = TRUE)

df17 <- download_daymet(site = "Longwell2019",
                      lat = 39.9203647,
                      lon = -85.0189317,
                      start = 2019,
                      end = 2019,
                      internal = TRUE,
                      simplify = TRUE)

df18 <- download_daymet(site = "Bryan2019",
                      lat = 40.7112831,
                      lon = -84.8614994,
                      start = 2019,
                      end = 2019,
                      internal = TRUE,
                      simplify = TRUE)

df20 <- download_daymet(site = "Steiner2019",
                        lat = 40.631,
                        lon = -84.998,
                        start = 2019,
                        end = 2019,
                        internal = TRUE,
                        simplify = TRUE)

df21 <- download_daymet(site = "Anibas2019",
                        lat = 44.627644,
                        lon = -92.061415,
                        start = 2019,
                        end = 2019,
                        internal = TRUE,
                        simplify = TRUE)

df22 <- download_daymet(site = "Adsit2019",
                        lat = 42.796285,
                        lon = -88.441323,
                        start = 2019,
                        end = 2019,
                        internal = TRUE,
                        simplify = TRUE)

df23 <- download_daymet(site = "Beiler2019",
                        lat = 42.849739,
                        lon = -90.360484,
                        start = 2019,
                        end = 2019,
                        internal = TRUE,
                        simplify = TRUE)

df24 <- download_daymet(site = "Doudlah2019",
                        lat = 42.8647845, 
                        lon = -89.3399422,
                        start = 2019,
                        end = 2019,
                        internal = TRUE,
                        simplify = TRUE)

df25 <- download_daymet(site = "Stoltzfus12019",
                        lat = 42.7355969, 
                        lon = -90.0418907,
                        start = 2019,
                        end = 2019,
                        internal = TRUE,
                        simplify = TRUE)

df26 <- download_daymet(site = "Stoltzfus22019",
                        lat = 42.7355969, 
                        lon = -90.0418907,
                        start = 2019,
                        end = 2019,
                        internal = TRUE,
                        simplify = TRUE)

df27 <- download_daymet(site = "Zinniker2019",
                        lat = 42.7606545,
                        lon = -88.4466442,
                        start = 2019,
                        end = 2019,
                        internal = TRUE,
                        simplify = TRUE)

df28 <- download_daymet(site = "Smith2020",
                        lat = 40.6551163,
                        lon = -89.7281125,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df29 <- download_daymet(site = "Wilken2020",
                        lat = 40.530603,
                        lon = -87.701674,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df30 <- download_daymet(site = "Gruver2020",
                        lat = 40.667232,
                        lon = -90.752979,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df31 <- download_daymet(site = "Doonan2020",
                        lat = 41.3298261,
                        lon = -90.6632141,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df32 <- download_daymet(site = "Ambriole2020",
                        lat = 40.919109,
                        lon = -85.371743,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df33 <- download_daymet(site = "Longwell2020",
                        lat = 39.8732343,
                        lon = -85.2288494,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df34 <- download_daymet(site = "Bryan2020",
                        lat = 40.745211,
                        lon = -84.870239,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df35 <- download_daymet(site = "Steiner2020",
                        lat = 40.627568,
                        lon = -85.076058,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df36 <- download_daymet(site = "Anibas2020",
                        lat = 44.627644,
                        lon = -92.061415,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df37 <- download_daymet(site = "Clark2020",
                        lat = 42.96033708,
                        lon = -89.65322593,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df38 <- download_daymet(site = "Esch2020",
                        lat = 42.97558367,
                        lon = -90.52991733,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df39 <- download_daymet(site = "Egre2020",
                        lat = 42.93260886,
                        lon = -89.07142282,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df40 <- download_daymet(site = "Beiler2020",
                        lat = 42.849739,
                        lon = -90.360484,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df41 <- download_daymet(site = "Goldstein2020",
                        lat = 42.78314973,
                        lon = -88.42265091,
                        start = 2020,
                        end = 2020,
                        internal = TRUE,
                        simplify = TRUE)

df42 <- download_daymet(site = "Smith2021",
                        lat = 40.61628,
                        lon = -89.843491,
                        start = 2021,
                        end = 2021,
                        internal = TRUE,
                        simplify = TRUE)

df43 <- download_daymet(site = "Doonan2021",
                        lat = 41.3298261,
                        lon = -90.6632141,
                        start = 2021,
                        end = 2021,
                        internal = TRUE,
                        simplify = TRUE)

df44 <- download_daymet(site = "Gruver2021",
                        lat = 40.664636,
                        lon = -90.750887,
                        start = 2021,
                        end = 2021,
                        internal = TRUE,
                        simplify = TRUE)

df45 <- download_daymet(site = "Ambriole2021",
                        lat = 40.934951,
                        lon = -85.34473,
                        start = 2021,
                        end = 2021,
                        internal = TRUE,
                        simplify = TRUE)



library(tidyverse)
Weather <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18,df20,df21,df22,df23,df24,df25,df26,df27,df28,df29,df30,df31,df32,df33,df34,df35,df36,df37,df38,df39,df40,df41,df42,df43,df44,df45)

library(readr)
write.csv(Weather, "Raw_Weather_data_Orei.csv")



## Changing names and Unstacking columns

# Variable names are too Long and stucked into one column. This will make names shorter and unstacked

Weather$measurement<-strtrim(Weather$measurement, 4) #this reduces name into 4 characters

Weather<-Weather %>% 
  pivot_wider(names_from = c(measurement), #This helps to unstack a coloumn with multiple variables into multiple colomns
              values_from = c(value))


## Date conversion
# Dates in the daymetr download are formatted the as year and day number. Accordingly the format was modified to a more useful one.

Weather$Date <-as.Date(paste(Weather$year, 1, 1, sep = "-"))+ (Weather$yday-1) # beginning of year
Weather$month<-format(Weather$Date,"%h")
Weather$year<-format(Weather$Date, "%Y")


## Data Manipulatio using Tidyverse and Dplyr. 
# we can create sub-dataWeather2018sets from the main df by Selecting columns and rows

library(tidyverse)
weather_sorted<-select(Weather, site,year,month,Date,dayl:tmin)
weather_sorted$temp<-(weather_sorted$tmin+weather_sorted$tmax)/2
weather_sorted$month<-factor(weather_sorted$month, levels=month.abb)


# You can also select a specific year using the select () function.Using the pipe operator %>% we could chain operations and avoid saving intermediate objects. Forexample, here were are secting precipitation data from 2019 only

Weather_GS  <- weather_sorted %>% 
            filter(month %in% c("May", "Jun", "Jul", "Aug", "Sep"))%>% 
              select(site,year, month,Date, prcp,srad, tmax,tmin, temp)

## Variable Uniy conversions. 
# if we want to change the units of the variables, we can use mutate function.

Weather_GS <- Weather_GS %>% 
  mutate(
    #prcp_in = prcp / 25.4,
    tmax = (tmax* 9/5)+32,
    tmin = (tmin* 9/5)+32,
    temp = (temp* 9/5)+32,
    month = factor(month, levels = month.abb))

#Estimating GDD. (tmax+tmin)/2 - 50 . But if tmax is above 86, we change to 86 and if below 50 we change it to 50. 

library(dplyr)
Weather_GS$tmax_gdd <- Weather_GS$tmax

Weather_GS <- Weather_GS%>%
  mutate(tmax_gdd = ifelse(tmax_gdd > 86, 86, ifelse(tmax_gdd < 50, 50, tmax_gdd)))

Weather_GS$GDD <- (((Weather_GS$tmin+Weather_GS$tmax_gdd)/2)-50)


## Summarizing and group Data.
# We can use the group_by function and summarize function to group data

Weather_GS <- Weather_GS %>% group_by(site,year,month) %>% 
  summarize(prcp = sum(prcp),srad = mean(srad), tmax = mean(tmax),tmin = mean(tmin), temp = mean(temp), GDD = sum(GDD))

library(readr)
write.csv(Weather_GS, "Weather_Data_Stacked.csv")


Weather_GS_clean <-Weather_GS %>% 
  pivot_wider(names_from = c(month), #This helps to unstack a column with multiple variables into multiple colomns
              values_from = c(prcp, srad, tmax, tmin, temp, GDD))

library(readr)
write.csv(Weather_GS_clean, "Weather_Data_UnStacked.csv")



## This data was joined with soil data, management data and agronomic data to form one dataset for further statistical analysis. 

Weather_GS$month <- as.factor(Weather_GS$month)
Weather_GS$prcp <- as.numeric(Weather_GS$prcp)

library(ggplot2)
ggplot(data = Weather_GS) +
  geom_point(mapping = aes(x = month, y = prcp, color = month)) +
  facet_wrap(~ site) +
  labs(title = "Seasonal Precipitation at each Strip Trial Sites",
    y = "Precipitation (mm)",
    x = "Month") +
  theme_bw()


library(ggplot2)
ggplot(data = Weather_GS) +
  geom_point(mapping = aes(x = month, y = tmax, color = month)) +
  facet_wrap(~ site) +
  labs(title = "Seasonal Maximum Temperature at each Strip Trial Sites",
       y = "Precipitation (mm)",
       x = "Month") +
  theme_bw()


library(ggplot2)
ggplot(data = Weather_GS) +
  geom_point(mapping = aes(x = month, y = tmin, color = month)) +
  facet_wrap(~ site) +
  labs(title = "Seasonal Minimum Temperature at each Strip Trial Sites",
       y = "Precipitation (mm)",
       x = "Month") +
  theme_bw()


library(ggplot2)
ggplot(data = Weather_GS) +
  geom_point(mapping = aes(x = month, y = temp, color = month)) +
  facet_wrap(~ site) +
  labs(title = "Average Monthly temperature at each Strip Trial Site",
       y = "Precipitation (mm)",
       x = "Month") +
  theme_bw()


library(ggplot2)
ggplot(data = Weather_GS) +
  geom_point(mapping = aes(x = month, y = srad, color = month)) +
  facet_wrap(~ site) +
  labs(title = "Average Monthly Solar radiation at each Strip Trial Site",
       y = "Precipitation (mm)",
       x = "Month") +
  theme_bw()



## Estimating the Probability of An Experimental Hybrid to Outperform the Check 
##Open ML_LinearRegressionfolder

library(readr)
Strip1  <- read.csv("ML_All_WeatherMeans.csv", stringsAsFactors = T)
Strip1 <- Strip1[-c(1:4)]  #remove design columns

Strip2  <- read.csv("ML_All_SeasonalWeather.csv", stringsAsFactors = T)
Strip2 <- Strip2[-c(1:4)]  #remove design columns


## Missing values
missing_values <- colSums(is.na(Strip2))
print(missing_values)

## Use MissForest to estimate missing values
install.packages("missForest")
library(missForest)
set.seed(123)
n_trees <- 100
imputed_data <- missForest(Strip2, ntree = n_trees)
Clean <- imputed_data$ximp
colSums(is.na(Clean))




## Another method: replacing missing values with means. Dont recommend
my_df <- Strip1 %>% 
  group_by(FAR) %>% 
  mutate(YIELD = replace_na(YIELD, mean(YIELD, na.rm = TRUE)))%>% 
  mutate(CEC = replace_na(CEC, mean(CEC, na.rm = TRUE)))%>% 
  mutate(pH = replace_na(pH, mean(pH, na.rm = TRUE)))%>% 
  mutate(OM = replace_na(OM, mean(OM, na.rm = TRUE)))%>% 
  mutate(EstNRel = replace_na(EstNRel, mean(EstNRel, na.rm = TRUE)))%>% 
  mutate(Su = replace_na(Su, mean(Su, na.rm = TRUE)))%>% 
  mutate(P = replace_na(P, mean(P, na.rm = TRUE)))%>% 
  mutate(Ca = replace_na(Ca, mean(Ca, na.rm = TRUE)))%>% 
  mutate(Mg = replace_na(Mg, mean(Mg, na.rm = TRUE)))%>% 
  mutate(K = replace_na(K, mean(K, na.rm = TRUE)))%>% 
  mutate(Na = replace_na(Na, mean(Na, na.rm = TRUE)))%>% 
  mutate(SOC = replace_na(SOC, mean(SOC, na.rm = TRUE)))%>% 
  mutate(TN = replace_na(TN, mean(TN, na.rm = TRUE)))%>% 
  mutate(SoilCN = replace_na(SoilCN, mean(SoilCN, na.rm = TRUE)))%>% 
  mutate(InorgN = replace_na(InorgN, mean(InorgN, na.rm = TRUE)))%>% 
  mutate(Bray1P = replace_na(Bray1P, mean(Bray1P, na.rm = TRUE)))%>% 
  mutate(POMC = replace_na(POMC, mean(POMC, na.rm = TRUE)))%>% 
  mutate(POMN = replace_na(POMN, mean(POMN, na.rm = TRUE)))%>% 
  mutate(POMCN = replace_na(POMCN, mean(POMCN, na.rm = TRUE)))%>% 
  mutate(PMN = replace_na(PMN, mean(PMN, na.rm = TRUE)))%>% 
  mutate(POXC = replace_na(POXC, mean(POXC, na.rm = TRUE)))

my_df <- Clean
								
mean(my_df$YIELD, na.rm = TRUE)
min(my_df$YIELD, na.rm = TRUE)
max(my_df$YIELD, na.rm = TRUE)

#Removing farmers without our check
filtered_df <- my_df %>%
  filter(FAR != "Glazik")

## Estimating yield differences between hybrid and check (ORG4)
Yield_Diff <- filtered_df %>%
  group_by(YEAR,FAR,REP) %>%
  mutate(YD = YIELD - YIELD[HYB == "ORG4"]) %>%
  ungroup()

## Summation or yield differences 
MeanYD  <- Yield_Diff %>%
  group_by(YEAR,HYB) %>%
  mutate(
    MeanYD = mean(YD, na.rm = TRUE),
    SDYD = sd(YD, na.rm = TRUE)
  ) %>%
  ungroup()

## Calculate Zscore for each hybrid
Zscore <- MeanYD  %>%
  mutate(Zscore = MeanYD/SDYD)


# Estimating Reliability of each hybrid using the zscores. 

#This is the probability that a hybrid will outperform the check hybrid.
#To estimate probabilities using Z-scores, you can use the standard normal distribution (also known as the Z-distribution) 
#and the cumulative distribution function (CDF) of the standard normal distribution. The CDF will give you the probability 
#that a Z-score falls below a certain value.
#In R, you can use the pnorm() function to calculate probabilities from Z-scores.
#Here's how you can estimate probabilities for each Z-score in your dataset:

Reliability <- Zscore  %>%
  mutate(R = pnorm(Zscore))

Reliability$Re <- ifelse(Reliability$R < 0.5, "Low", "High")
Reliability$R2 <- ifelse(Reliability$YD >= 0, "Yes", "No")


library(readr)
write.csv(Reliability, "Reliable.csv")



## Removing unwanted columns
Reliability <-  read.csv("Reliable.csv", stringsAsFactors = T)
Reliability <-  read.csv("R_estimates_Rstudio.csv", stringsAsFactors = T)

Reliability2 <- Reliability[-c(1,7,39:40)]  #if we use the non-seasonal dataset
Reliability2 <- Reliability[-c(1,2,5,60:66)]

#remove rows with ORG4 since they have Na for Reliability
Reliability2 <- Reliability2%>%filter(HYB != "ORG4")

#Creat a separate dataset with one-hot enconding , changing categorical to numeric
#install.packages("mltools")
#library(mltools)
#library(data.table)
#Onehot_data <- one_hot(as.data.table(Reliability2[c(1:10, 37)]))



## Separating 2021 data and reserve it for validation

library(dplyr)

# Filter data for the first three years (2018, 2019, and 2020)
Training_Phase <- Reliability2  %>%
  filter(YEAR %in% c(2018, 2019, 2020))

Training_Phase$R3 <- ifelse(Training_Phase$R == "YES", "1", "0")
Training_Phase  <- Training_Phase [-c(1,3, 37)]
Training_Phase$R3 <- as.numeric(Training_Phase$R3)

#Training_Phase <- na.omit(Training_Phase)

# Filter data for the year 2021
Validation_Phase <-Reliability2   %>%filter(YEAR == 2021)
Validation_Phase$R3 <- ifelse(Validation_Phase$R == "YES", "1", "0")
#Validation_Phase <- Validation_Phase[-c(2,57)] #for seasona;
Validation_Phase <- Validation_Phase[-c(1,3, 37)]  #for nonseasonal

Validation_Phase$R3 <- as.numeric(Validation_Phase$R3)


#Validation_Phase <- na.omit(Validation_Phase)

# Split training_phgase data into training and testing

library(caret)
set.seed(123)
split_index <- createDataPartition(Training_Phase$R3, p = 0.8, list = FALSE)
Training_set <- Training_Phase[split_index, ]
Testing_set <- Training_Phase[-split_index, ]


# Full model
model.formula <- as.formula("R3 ~.")
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
Strips.rp <-  rpart("R3 ~.",data = Training_Phase, control =cont.parms, method = "anova")
plotcp(Strips.rp) 
printcp(Strips.rp)


## Control parameters based on output of plotcp() and printcp()

cont.parms <- rpart.control(minsplit = 20, cp = 0.026)
Strips.rp <-  rpart("R3 ~.",
                    data = Training_set,control = cont.parms, method = "class")

Pred <-predict(Strips.rp, type = "class",newdata = Testing_set)
Pred <- as.data.frame(Pred)
Pred$Actual <- Testing_set$R3
cm_test <-table(Pred)

metrics <- function(cm) {
  accuracy <- sum(diag(cm))/sum(cm)
  c(accuracy = accuracy)}

metrics(cm_test)


## Classification tree and Area under the curve

#fallen leaves. 
library(rpart.plot)
rpart.plot(Strips.rp, digits = 3, fallen.leaves = T, type=3, extra=101, cex=0.5,
           main = "Classification Tree For Hybrid Reliability") 

#Plots
library(caret)
Strip.rp <-  rpart("R3 ~ .", data = Training_set,
                     control= , method = "anova")

pred.test <-predict(Strip.rp, type = "matrix",newdata = Testing_set)

library(ROCR)
predROCR = prediction(pred.test, Testing_set$R3)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#vALIDATION
pred.test2 <-predict(Strip.rp, type = "matrix",newdata = Validation_Phase)
library(ROCR)
predROCR = prediction(pred.test2, Validation_Phase$R3)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC
performance(predROCR, "auc")@y.values






# Random Forest
library(randomForest)
Model_RF <- randomForest(R3 ~ HYB + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P +  SOC + TN + SoilCN
                         + POMC + POMN + PMN + POXC + Prcp + Tmax + Tmin + TGDD,
                          data = Training_Phase, importance = TRUE, ntree=200, nodesize=25)

Model_RF <- randomForest(R3 ~., data =Training_set, importance = TRUE, ntree=200, nodesize=25)

pred_test <- predict(Model_RF ,type = "class",  newdata = Testing_set)
pred_test  <- as.data.frame(pred_test)

pred_test$Pred <-ifelse(pred_test[,1] > 0.5,"1","0")
pred_test$Actual <- Testing_set$R3

# Confusion matrix with threshold of 0.5 training and test set
cm_test <- table(pred_test$Pred, pred_test$Actual)

## Calculating metrics from confusion matrix ##
metrics <- function(cm) {
  accuracy <- sum(diag(cm))/sum(cm)
  precision <- cm[2,2]/sum(cm[2,])
  recall <- cm[2,2]/sum(cm[ ,2])
  baseline<- sum(cm[1,])/sum(cm)
  c(accuracy = accuracy, precision = precision, recall = recall, baseline = baseline)
}

metrics(cm_test)
### b. Plot the explanatory variable importance to identify what are the most influential variables to classify oak presence.

varImpPlot(Model_RF,   
           main = "Variable Importance")

attributes(Model_RF)

### c. Estimate area under the curve

library(caret)
library(ROCR)
pred_test <- predict(Model_RF ,type = "class",  newdata = Testing_set)

predROCR = prediction(pred_test, Testing_set$R3)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

performance(predROCR, "auc")@y.values



library(caret)
library(ROCR)
pred_val <- predict(Model_RF ,type = "class",  newdata = Validation_Phase)

predROCR = prediction(pred_val, Validation_Phase$R3)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

performance(predROCR, "auc")@y.values


####### Using Cross Validation and Bootstrapping####

# Train a RF model
library(caret)
model_RF2 <- train(R3 ~ HYB + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P + Ca + Mg + K + Na + SOC + TN + SoilCN + InorgN 
                   + Bray1P + POMC + POMN + POMCN + PMN + POXC + Prcp + Tmax + Tmin + TGDD,data= Training_set,  method="rf")

predict_RF <- predict(model_RF2,Testing_set)

predict_RF <- as.data.frame(predict_RF)

predict_RF$Pred <-ifelse(predict_RF [,1] > 0.5,"1","0")
predict_RF$Actual <- Testing_set$R3



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

#### Training model using different re-sampling methods such as k-fold cv, bootstrapping and other 

Training_Phase <- Reliability2  %>%
  filter(YEAR %in% c(2018, 2019, 2020))

#Training_Phase$R3 <- ifelse(Training_Phase$R == "YES", "1", "0")
Training_Phase  <- Training_Phase [-c(1)]
Training_Phase$R <- as.factor(Training_Phase$R)

set.seed(123)
split_index <- createDataPartition(Training_Phase$R, p = 0.8, list = FALSE)
Training_set <- Training_Phase[split_index, ]
Testing_set <- Training_Phase[-split_index, ]

Validation_Phase <-Reliability2   %>%filter(YEAR == 2021)
Validation_Phase <- Validation_Phase[-c(1)]
Validation_Phase$R <- as.factor(Validation_Phase$R)



library(caret)
model1 <- trainControl(method = "boot", number = 10, search = "random")
model2 <- trainControl(method = "boot632", number = 10, search = "random")
model3 <- trainControl(method = "optimism_boot", number = 10, search = "random")
model4 <- trainControl(method = "boot_all", number = 10, search = "random")
model5 <- trainControl(method = "cv", number = 10, search = "random")
model6 <- trainControl(method = "repeatedcv", number = 10,repeats = 3, search = "random")
model7 <- trainControl(method = "LGOCV", number = 10, search = "random")
model8 <- trainControl(method = "adaptive_cv", number = 10,repeats = 3, search = "random")
model9 <- trainControl(method = "adaptive_boot", number = 10, search = "random")

## Train and compare the models
model_boot <- train(R ~ HYB + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P + Ca + Mg + K + Na + SOC + TN + SoilCN + InorgN 
                    + Bray1P + POMC + POMN + POMCN + PMN + POXC + Prcp + Tmax + Tmin + TGDD, data = Training_set, trControl=model1, method="rf")

model_boot632 <- train(R ~ HYB  + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P + Ca + Mg + K + Na + SOC + TN + SoilCN + InorgN 
                       + Bray1P + POMC + POMN + POMCN + PMN + POXC + Prcp + Tmax + Tmin + TGDD, data = Training_set, trControl=model2, method="rf")

model_Optboot <- train(R ~ HYB  + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P + Ca + Mg + K + Na + SOC + TN + SoilCN + InorgN 
                       + Bray1P + POMC + POMN + POMCN + PMN + POXC + Prcp + Tmax + Tmin + TGDD, data = Training_set, trControl=model3, method="rf")

model_bootall  <- train(R ~ HYB  + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P + Ca + Mg + K + Na + SOC + TN + SoilCN + InorgN 
                        + Bray1P + POMC + POMN + POMCN + PMN + POXC + Prcp + Tmax + Tmin + TGDD, data = Training_set, trControl=model4, method="rf")

model_cv <- train(R ~ HYB  + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P + Ca + Mg + K + Na + SOC + TN + SoilCN + InorgN 
                  + Bray1P + POMC + POMN + POMCN + PMN + POXC + Prcp + Tmax + Tmin + TGDD,data = Training_set, trControl=model5, method="rf")

model_repcv <- train(R ~ HYB  + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P + Ca + Mg + K + Na + SOC + TN + SoilCN + InorgN 
                     + Bray1P + POMC + POMN + POMCN + PMN + POXC + Prcp + Tmax + Tmin + TGDD,data = Training_set, trControl=model6, method="rf")

model_LGOCV <- train(R ~ HYB  + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P + Ca + Mg + K + Na + SOC + TN + SoilCN + InorgN 
                     + Bray1P + POMC + POMN + POMCN + PMN + POXC + Prcp + Tmax + Tmin + TGDD, data = Training_set, trControl=model7, method="rf")

model_adptcv <- train(R ~ HYB  + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P + Ca + Mg + K + Na + SOC + TN + SoilCN + InorgN 
                      + Bray1P + POMC + POMN + POMCN + PMN + POXC + Prcp + Tmax + Tmin + TGDD,data = Training_set, trControl= model8, method="rf")

model_adtboot <- train(R ~ HYB + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P + Ca + Mg + K + Na + SOC + TN + SoilCN + InorgN 
                       + Bray1P + POMC + POMN + POMCN + PMN + POXC + Prcp + Tmax + Tmin + TGDD,data = Training_set, trControl=model9, method="rf")

model_svm2 <- train(R ~ HYB + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P + Ca + Mg + K + Na + SOC + TN + SoilCN + InorgN 
                    + Bray1P + POMC + POMN + POMCN + PMN + POXC + Prcp + Tmax + Tmin + TGDD, data = Training_set, trControl=model9, method="svmRadial")


# Compare model performances using resample()

models_boot <- resamples(list(boot= model_boot,
                              boot_632 = model_boot632,
                              optimism_boot = model_Optboot,
                              adaboot = model_adtboot,
                              boot_all = model_bootall,
                              CV = model_cv))

model_adtboot$results
model_boot$results
model_boot632$results
model_Optboot$results
model_cv$results
model_bootall$results
model_adptcv$results
model_svm2 $results
model_LGOCV$results
model_repcv$results


# Assuming each model has an 'Accuracy' metric in the 'results'
models <- list(
  model_adtboot = model_adtboot$results$Accuracy,
  model_boot = model_boot$results$Accuracy,
  model_boot632 = model_boot632$results$Accuracy,
  model_Optboot = model_Optboot$results$Accuracy,
  model_cv = model_cv$results$Accuracy,
  model_bootall = model_bootall$results$Accuracy,
  model_adptcv = model_adptcv$results$Accuracy,
  model_svm2 = model_svm2$results$Accuracy,
  model_LGOCV = model_LGOCV$results$Accuracy,
  model_repcv = model_repcv$results$Accuracy
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


#summary(models_boot)
#scales1 <- list(x=list(relation="free"), y=list(relation="free"))
#bwplot(models_boot, scales=scales1)



### Testing the best cv model 
library(caret)
adptcv_pred <- predict(model_adptcv,Validation_Phase)
confusionMatrix(adptcv_pred, Validation_Phase$R)

predictions <- predict(model_adptcv, Validation_Phase)
confusionMatrix(predictions,  Validation_Phase$R)

predictions <- predict(model_cv, Testing_set)
confusionMatrix(predictions, Testing_set$R)

predictions <- predict(model_repcv,Testing_set)
confusionMatrix(predictions, Testing_set$R)

predictions <- predict(model_LGOCV,Testing_set)
confusionMatrix(predictions, Testing_set$R)


predictions <- predict(model_boot,Testing_set)
confusionMatrix(predictions, Testing_set$R)

predictions <- predict(model_boot632,Testing_set)
confusionMatrix(predictions, Testing_set$R)

predictions <- predict(model_Optboot,Testing_set)
confusionMatrix(predictions, Testing_set$R)

predictions <- predict(model_bootall,Testing_set)
confusionMatrix(predictions, Testing_set$R)

predictions <- predict(model_adtboot,Testing_set)
confusionMatrix(predictions, Testing_set$R3)



## Comparing classification methods RF and SVM

model_rf = train(R ~ HYB  + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P + Ca + Mg + K + Na + SOC + TN + SoilCN + InorgN 
                 + Bray1P + POMC + POMN + POMCN + PMN + POXC + Prcp + Tmax + Tmin + TGDD, data=Training_set, method='rf', tuneLength=5, trControl = model3)

pred_RF <- predict(model_rf, Testing_set)
confusionMatrix(reference = Testing_set$R3,  pred_RF)


model_svm = train(R ~ HYB + FS + CC +  Density + Rotation + CEC + pH  + OM + EstNRel + Su + P + Ca + Mg + K + Na + SOC + TN + SoilCN + InorgN 
                  + Bray1P + POMC + POMN + POMCN + PMN + POXC + Prcp + Tmax + Tmin + TGDD, data=Training_set, method='svmRadial', tuneLength=5, trControl = model3)

predic_svm <- predict(model_svm, Testing_set)
confusionMatrix(reference = Testing_set$R3,  predic_svm)


# Compare model performances using resample()
models_compare2 <- resamples(list(RF=model_rf, SVM=model_svm))
summary(models_compare2)

# Draw box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare2, scales=scales)


## Test on validation set
Val_RF <- predict(model_rf, Validation_Phase)

Val_svm<- predict(model_svm, Validation_Phase)



# Compare model performances using resample()
models_compare2 <- resamples(list(RF=model_rf, SVM=model_svm))
summary(models_compare2)

# Draw box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare2, scales=scale)

model_adtboot$results$Accuracy 
library(ggplot2)
model_svm $results$Accuracy


#________________________Another way to plot ____________________________________________________________
# Create a data frame with model names and their accuracies
model_names <- c("model_boot", "model_boot","model_boot",
                 "model_boot632","model_boot632","model_boot632",
                 "model_Optboot", "model_Optboot", "model_Optboot", 
                 "model_bootall", "model_bootall", "model_bootall", 
                 "model_cv", "model_cv","model_cv",
                 "model_repcv", "model_repcv", "model_repcv", 
                 "model_LGOCV", "model_LGOCV", "model_LGOCV", 
                 "model_adptcv",  "model_adptcv",  "model_adptcv", 
                 "model_adtboot", "model_adtboot", "model_adtboot")
accuracies <- c(
  model_boot$results$Accuracy,
  model_boot632$results$Accuracy,
  model_Optboot$results$Accuracy,
  model_bootall$results$Accuracy,
  model_cv$results$Accuracy,
  model_repcv$results$Accuracy,
  model_LGOCV$results$Accuracy,
  model_adptcv$results$Accuracy,
  model_adtboot$results$Accuracy)

model_comparison <- data.frame(Model = model_names, Accuracy = accuracies)

# Create a bwplot plot to compare model accuracies

install.packages("latticeExtra")
library(latticeExtra)

scales <- list(x=list(relation="free", rot = 90), y=list(relation="free", rot = 90))
bwplot(Accuracy ~ Model, data = model_comparison, 
       main = "Model Accuracy Comparison",
       xlab = "Model", ylab = "Accuracy",
       #scales = list(x = list(rot = 90 )),
       ylim = c(0.7, 0.9),
       scale =scales)








