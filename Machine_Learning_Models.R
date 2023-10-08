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

library(readr)
Strips  <- read.csv("ALL.csv", stringsAsFactors = T)
Strips <- Strips[-c(1,3:8)]

## Converting planting data to julian

Strips$PDT <- as.Date(Strips$PDT, format = "%m/%d/%y")
Strips$PDT <- julian(Strips$PDT)

#Changing Na to means of the column
Strips$YIELD <- as.numeric(Strips$YIELD)
Strips$DEN <- as.numeric(Strips$DEN)
on_numeric_values <- grep("[^0-9.]", Strips$YIELD, value = TRUE)
on_numeric_values

## This is probably better than the previous method. 
my_df <- Strips %>% 
  group_by(ENV) %>% 
  mutate(YIELD = replace_na(YIELD, mean(YIELD, na.rm = TRUE)))
my_df

Strips$PHT[is.na(Strips$PHT)]<-mean(Strips$PHT,na.rm=TRUE)
Strips$EHT[is.na(Strips$EHT)]<-mean(Strips$EHT,na.rm=TRUE)
Strips$SDL[is.na(Strips$SDL)]<-mean(Strips$SDL,na.rm=TRUE)
Strips$SDS[is.na(Strips$SDS)]<-mean(Strips$SDS,na.rm=TRUE)
Strips$MST[is.na(Strips$MST)]<-mean(Strips$MST,na.rm=TRUE)
Strips$TWT[is.na(Strips$TWT)]<-mean(Strips$TWT,na.rm=TRUE)
Strips$KWT300[is.na(Strips$KWT300)]<-mean(Strips$KWT300,na.rm=TRUE)
Strips$YIELD[is.na(Strips$YIELD)]<-mean(Strips$YIELD,na.rm=TRUE)
Strips$PRO[is.na(Strips$PRO)]<-mean(Strips$PRO,na.rm=TRUE)
Strips$OIL[is.na(Strips$OIL)]<-mean(Strips$OIL,na.rm=TRUE)
Strips$STA[is.na(Strips$STA)]<-mean(Strips$STA,na.rm=TRUE)
Strips$DEN[is.na(Strips$DEN)]<-mean(Strips$DEN,na.rm=TRUE)
Strips$FIB[is.na(Strips$FIB)]<-mean(Strips$FIB,na.rm=TRUE)

mean(Strips$YIELD, na.rm = TRUE)
min(Strips$YIELD, na.rm = TRUE)
max(Strips$YIELD, na.rm = TRUE)


Strips$CEC[is.na(Strips$CEC)]<-mean(Strips$CEC,na.rm=TRUE)
Strips$pH[is.na(Strips$pH)]<-mean(Strips$pH,na.rm=TRUE)
Strips$OM[is.na(Strips$OM)]<-mean(Strips$OM,na.rm=TRUE)
Strips$Su[is.na(Strips$Su)]<-mean(Strips$Su,na.rm=TRUE)
Strips$P[is.na(Strips$P)]<-mean(Strips$P,na.rm=TRUE)
Strips$Ca[is.na(Strips$Ca)]<-mean(Strips$Ca,na.rm=TRUE)
Strips$Mg[is.na(Strips$Mg)]<-mean(Strips$Mg,na.rm=TRUE)
Strips$K[is.na(Strips$K)]<-mean(Strips$K,na.rm=TRUE)
Strips$Na[is.na(Strips$Na)]<-mean(Strips$Na,na.rm=TRUE)
Strips$NO3N[is.na(Strips$NO3N)]<-mean(Strips$NO3N,na.rm=TRUE)
Strips$TOC[is.na(Strips$TOC)]<-mean(Strips$TOC,na.rm=TRUE)
Strips$TON[is.na(Strips$TON)]<-mean(Strips$TON,na.rm=TRUE)
Strips$Sand[is.na(Strips$Sand)]<-mean(Strips$Sand,na.rm=TRUE)
Strips$Silt[is.na(Strips$Silt)]<-mean(Strips$Silt,na.rm=TRUE)
Strips$Clay[is.na(Strips$Clay)]<-mean(Strips$Clay,na.rm=TRUE)
Strips$PDTY[is.na(Strips$PDTY)]<-mean(Strips$PDTY,na.rm=TRUE)


# Create subdataset

library (caTools)
set.seed(123)
split = sample.split(Strips [,"R"], SplitRatio = 0.8)
Training_set = subset(Strips, subset = split)
Testing_set = subset(Strips, subset = !split)


# Full model
model.formula <- as.formula("R ~.")
model_full <- glm(model.formula, data = Strips, family = binomial)

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
cont.parms <- rpart.control(minsplit = 20,cp = 0.002)
Strips.rp <-  rpart("R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC  + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + tmin + temp", 
                    data = Strips, control = cont.parms, method = "anova")
plotcp(Strips.rp) 
printcp(Strips.rp)


## Control parameters based on output of plotcp() and printcp()

cont.parms <- rpart.control(minsplit = 20, cp = 0.021)
Strips.rp <-  rpart("R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
               NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + tmin + temp",
                    data = Training_set,control = cont.parms, method = "class")

Pred <-predict(Strips.rp, type = "matrix",newdata = Testing_set)

# Confusion matrix with threshold of 0.5 test set
table(Testing_set$R, Pred > 0.5)
cm<-table(Testing_set$R, Pred  > 0.5)

cm_test2 <-table(Testing_set$R, Pred > 0.5)

metrics <- function(cm) {
  accuracy <- sum(diag(cm))/sum(cm)
  c(accuracy = accuracy)}

metrics(cm_test2)


## Classification tree and Area under the curve

plot(Strips.rp,branch = 0.4,uniform = T,margin = 0.1,
     main = "Classification Tree For Hybrid Reliability",
     cex.main = 2) 
text(Strips.rp,use.n = T,all = T, cex = 0.8)


#fallen leaves. 
library(rpart.plot)
rpart.plot(Strips.rp, digits = 3, fallen.leaves = T, type=3, extra=101, cex=0.5,
           main = "Classification Tree For Hybrid Reliability") 

#order and rules of split
library(rattle)
fancyRpartPlot(Strips.rp, cex = 0.5,
               main = "Classification Tree For Hybrid Reliability")


rpart.plot(Strips.rp, # middle graph
           type=3,
           extra=101, 
           box.palette="GnBu",
           branch.lty=3, 
           shadow.col="gray", 
           nn=TRUE,
           cex = 0.6
)



library(ROCR)
predROCR = prediction(Pred, Testing_set$R)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


# Random Forest
library(randomForest)
Model_RF <- randomForest(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                           NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                           tmin + temp, data = Training_set, importance = TRUE, ntree=200, nodesize=25)


pred_test <- predict(Model_RF ,type = "response",  newdata = Testing_set)


# Confusion matrix with threshold of 0.5 test set
table(Testing_set$R, pred_test)
cm<-table(Testing_set$R, pred_test)

# Confusion matrix with threshold of 0.5 training and test set
cm_test <- table(Testing_set$R, pred_test)

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

#Plots
library(rpart)
Strips.rp <-  rpart("R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                     NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                     tmin + temp", data = Training_set,
                    control= , method = "anova")

pred.test <-predict(Strips.rp, type = "matrix",newdata = Testing_set)

library(ROCR)
predROCR = prediction(pred.test, Testing_set$R)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


####### Using Cross Validation and Bootstrapping####

# Train a RF model
library(caret)
model_RF2 <- train(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                     NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                     tmin + temp,data= Training_set,  method="rf")

predict_RF <- predict(model_RF2,Testing_set)
confusionMatrix(predict_RF, Testing_set$R)



#### Training model using different re-sampling methods such as k-fold cv, bootstrapping and other 

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
model_boot <- train(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                      NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                      tmin + temp, data = Training_set, trControl=model1, method="rf")

model_boot632 <- train(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                         NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                         tmin + temp, data = Training_set, trControl=model2, method="rf")

model_Optboot <- train(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                         NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                         tmin + temp, data = Training_set, trControl=model3, method="rf")

model_bootall  <- train(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                          NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                          tmin + temp, data = Training_set, trControl=model4, method="rf")

model_cv <- train(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                    NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                    tmin + temp,data = Training_set, trControl=model5, method="rf")

model_repcv <- train(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                       NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                       tmin + temp,data = Training_set, trControl=model6, method="rf")

model_LGOCV <- train(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                       NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                       tmin + temp, data = Training_set, trControl=model7, method="rf")

model_adptcv <- train(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                        NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                        tmin + temp,data = Training_set, trControl= model8, method="rf")

model_adtboot <- train(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                         NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                         tmin + temp,data = Training_set, trControl=model9, method="rf")


# Compare model performances using resample()

models_boot <- resamples(list(boot= model_boot,
                              boot_632 = model_boot632,
                              optimism_boot = model_Optboot,
                              adaboot = model_adtboot,
                              boot_all = model_bootall))

summary(models_boot)
scales1 <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_boot, scales=scales1)



### Testing the best cv model 
library(caret)
predictions <- predict(model_adptcv,Testing_set)
confusionMatrix(predictions, Testing_set$R)

predictions <- predict(model_cv,Testing_set)
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
confusionMatrix(predictions, Testing_set$R)



## Comparing classification methods RF and SVM

model_rf = train(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                   NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                   tmin + temp, data=Training_set, method='rf', tuneLength=5, trControl = model3)

pred_RF <- predict(model_rf, Testing_set)
confusionMatrix(reference = Testing_set$R,  pred_RF)


model_svm = train(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                    NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                    tmin + temp, data=Training_set, method='svmRadial', tuneLength=15, trControl = model3)
predic_svm<- predict(model_svm, Testing_set)
confusionMatrix(reference = Testing_set$R,  predic_svm)


# Compare model performances using resample()
models_compare2 <- resamples(list(RF=model_rf, SVM=model_svm))
summary(models_compare2)

# Draw box plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare2, scales=scales)

#### USING NEURAL NETWORKS ###### 

# Note: I could not install the package keras or tensorflow to build the neural networks. 
# I was getting an error message below : Error: Python module tensorflow.keras was not found. Detected Python configuration:
# However, I found an alternative package to us called library(neuralnet). 

library(caret)
Strips_NN <- Strips


### Convert categorical variables to numeric dummy variables since neural networks only accept numeric
Strips_NN  <- sapply(Strips_NN, unclass)
Strips_NN <- data.frame(Strips_NN)


## Normalizing the data 
normalize <- function(x) {return((x - min(x)) / (max(x) - min(x)))}
Strips_NN <-as.data.frame(lapply(Strips_NN, normalize))


#Assigning training and testing sets
set.seed(1234)
train_index <- sample(seq_len(nrow(Strips_NN)), size = 0.75*nrow(Strips_NN))
Strip_train<-Strips_NN[train_index, ]
Strip_test<-Strips_NN[-train_index, ]


# install.packages("neuralnet")
library(neuralnet)
model_NN <-neuralnet(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                       NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                       tmin + temp, data=Strip_train)
plot(model_NN)

#Model performance 
NN_Pred <-compute(model_NN, Strip_test[, c(2:9, 13:33)])
pred_results <-NN_Pred$net.result
Accuracy <- cor(pred_results, Strip_test$R)
results1 <- data.frame(actual = Strip_test$R, predicted = NN_Pred$net.result)

roundedresults<-sapply(results1,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
cm_test1 <- table(actual,predicted)

metrics <- function(cm) {
  accuracy <- sum(diag(cm))/sum(cm)
  precision <- cm[2,2]/sum(cm[2,])
  recall <- cm[2,2]/sum(cm[ ,2])
  baseline<- sum(cm[1,])/sum(cm)
  c(accuracy = accuracy, precision = precision, recall = recall, baseline = baseline)}

metrics(cm_test1)


## Adding hidden layers and activation function
library(neuralnet)
model_NN2 <-neuralnet(R ~ HYBRID + PROG + CCT + RTNL + WRT + PDTY + NSRC + CEC + pH + OM + Su + P + Ca + Mg + K + Na + 
                        NO3N + TOC + TON + Sand + Silt + Clay + prcp + srad + tmax + 
                        tmin + temp,data=Strip_train, hidden = c(6,5))




plot(model_NN2,rep="best")
plot(model_NN2)

#Model performance 
NN_Pred2 <-compute(model_NN2, Strip_test[, c(2:9, 13:33)])
pred_results2 <-NN_Pred2$net.result
Correlation <- cor(pred_results2, Strip_test$R)

results <- data.frame(actual = Strip_test$R, predicted = NN_Pred2$net.result)

roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
cm_test <- table(actual,predicted)

metrics <- function(cm) {
  accuracy <- sum(diag(cm))/sum(cm)
  precision <- cm[2,2]/sum(cm[2,])
  recall <- cm[2,2]/sum(cm[ ,2])
  baseline<- sum(cm[1,])/sum(cm)
  c(accuracy = accuracy, precision = precision, recall = recall, baseline = baseline)}

metrics(cm_test)












