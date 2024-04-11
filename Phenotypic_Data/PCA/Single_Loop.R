
## _______________Data Processing, Wrangling and Cleaning______


R.version.string ##################CHECK THE CURRENT VERSION OF R #########
install.packages("installr") 
library(installr)
updateR() 

rm(list = ls())

## we obtain weather data, soil data, and combined it with the agronomic data to conduct statistical analysis as well as build models for prediction with ML models

## Obtaining weather data using farmer cordinates.
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

#Weather <- read.csv("Raw_Weather_data_Orei.csv")
Weather$measurement<-strtrim(Weather$measurement, 4) #this reduces name into 4 characters

Weather2 <- Weather %>% 
  pivot_wider(names_from = c(measurement), #This helps to unstack a coloumn with multiple variables into multiple colomns
              values_from = c(value))

## Date conversion
# Dates in the daymetr download are formatted the as year and day number. Accordingly the format was modified to a more useful one.

Weather2$Date <-as.Date(paste(Weather2$year, 1, 1, sep = "-"))+ (Weather2$yday-1) # beginning of year
Weather2$month<-format(Weather2$Date,"%h")
Weather2$year<-format(Weather2$Date, "%Y")


## Data Manipulatio using Tidyverse and Dplyr. 
# we can create sub-dataWeather2018sets from the main df by Selecting columns and rows

library(tidyverse)
weather_sorted<-select(Weather2, site,year,month,Date,dayl:tmin)
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
write.csv(Weather_GS_clean, "Weather_by_Season.csv")  #this will show how weather changes throughout the growing season

## This data was joined with soil data, management data and agronomic data to form one dataset for further statistical analysis. 
## This was done manually in excel. 

## Plotting weather variables for each farm usign this loop
Weather_GS$month <- as.factor(Weather_GS$month)
Weather_GS$prcp <- as.numeric(Weather_GS$prcp)

library(ggplot2)
Traits <- c("prcp", "srad", "tmax", "tmin", "temp", "GDD")

for (i in Traits) {
 plot <- ggplot(data = Weather_GS) +
    geom_point(mapping = aes(x = month, y = !!sym(i), color = month)) +
    facet_wrap(~ site) +
    labs(title = paste("Seasonal", i, "at each Strip Trial Sites"),
         y = i,
         x = "Month") +
    theme_bw()
 print(plot) 
}
plot

##______________DATASET DESCRIPTION________
## Combining, Soil, weather, and hybrid performance data. This was done manually in excel (vlookup). 
## 2 datasets were created. These only have yield in them 
## One with mean weather data, and another will 
## These  were name as ML_ALL_WeatherMeans.csv for mean weather values and ML_All_SeasonalWeather.cvs for seasonal weather values
## The third dataset also contains seasonal weather, soil data and all agronomic traits. "Allstates_Soil_Weath_Agronomic.csv"......


####  __________________STATISTICAL ANALYSIS_________
#Data is in LinearRegression folder
OREI_ALL  <- read.csv("Allstates_Soil_Weath_Agronomic.csv", header = TRUE)   
OREI_ALL   <- type.convert(replace(OREI_ALL , OREI_ALL == "." & !is.na(OREI_ALL), NA), as.is = TRUE)

## Use MissForest to estimate missing values
#install.packages("missForest")
library(missForest)
set.seed(123)
n_trees <- 100
OREI_imputed <- missForest(OREI_ALL[c(17:76)], ntree = n_trees)
OREI_Clean <- OREI_imputed$ximp
colSums(is.na(OREI_Clean))
OREI_COMPLETE  <- cbind(OREI_ALL[c(1:16)], OREI_Clean)

## Change yield and test weight to standard units

OREI_COMPLETE$YIELD<- OREI_COMPLETE$YIELD*0.0673  #t/ha
OREI_COMPLETE$TWT  <- OREI_COMPLETE$YIELD*1.25

#Change categorical variables to factors
OREI_COMPLETE$ENV      <- as.factor(OREI_COMPLETE$ENV     )
OREI_COMPLETE$STATE    <- as.factor(OREI_COMPLETE$STATE   )
OREI_COMPLETE$CITY     <- as.factor(OREI_COMPLETE$CITY    )
OREI_COMPLETE$PED      <- as.factor(OREI_COMPLETE$PED     )
OREI_COMPLETE$FAR      <- as.factor(OREI_COMPLETE$FAR     )
OREI_COMPLETE$FC       <- as.factor(OREI_COMPLETE$FC      )
OREI_COMPLETE$HYB      <- as.factor(OREI_COMPLETE$HYB     )
OREI_COMPLETE$YEAR     <- as.factor(OREI_COMPLETE$YEAR    )
OREI_COMPLETE$REP      <- as.factor(OREI_COMPLETE$REP     )
OREI_COMPLETE$FS       <- as.factor(OREI_COMPLETE$FS      )
OREI_COMPLETE$CC       <- as.factor(OREI_COMPLETE$CC      )
OREI_COMPLETE$WRT      <- as.factor(OREI_COMPLETE$WRT     )
OREI_COMPLETE$Density  <- as.factor(OREI_COMPLETE$Density )
OREI_COMPLETE$CI       <- as.factor(OREI_COMPLETE$CI      )
OREI_COMPLETE$CD       <- as.factor(OREI_COMPLETE$CD      )
OREI_COMPLETE$PER      <- as.factor(OREI_COMPLETE$PER     )

### ____Reliability Estimation using 59R5 as check (only for illinois)

# Estimating Reliability of each hybrid using the zscores. 
#This is the probability that a hybrid will outperform the check hybrid.
#To estimate probabilities using Z-scores, you can use the standard normal distribution (also known as the Z-distribution) 
#and the cumulative distribution function (CDF) of the standard normal distribution. The CDF will give you the probability 
#that a Z-score falls below a certain value.
#In R, you can use the pnorm() function to calculate probabilities from Z-scores.
#Here's how you can estimate probabilities for each Z-score in your dataset:



##Split data to obtain illinos and wisconsin

library(dplyr)
IL_IN_ALL <- OREI_COMPLETE  %>%
                filter(STATE%in% c("ILLINOIS", "INDIANA"))%>%
                 filter(YEAR != 2021)



# Illinois and Indiana analysis

## Trait distribution
Traits <- c("PHT", "EHT", "SDL", "SDS", "TWT", "KWT300", "YIELD", "PRO", "STA", "OIL")
library(ggplot2)
for (trait in Traits) {
  Plot <- ggplot(IL_IN_ALL, aes(x = .data[[trait]], fill = STATE, color = STATE)) +
    geom_histogram(position="identity") +
    labs(title = paste("Histogram of", trait), x = trait, y = "Frequency")
  print(Plot)
}

boxplot(IL_IN_ALL[17:27],las = 2, plot=TRUE)$out


outliers <- function(x) {
  Q1 <- quantile(x, probs = .25) # calculate first quantile
  Q3 <- quantile(x, probs = .75) # calculate third quantile
  iqr <- Q3 - Q1           # calculate interquartile range
  upper_limit <- Q3 + (iqr * 1.5)  # Calculate upper limit
  lower_limit <- Q1 - (iqr * 1.5)  # Calculate lower limit
  x > upper_limit | x < lower_limit   # return true or false
}

#Remove outliers 
IL_IN_ALL2 <- IL_IN_ALL
IL_IN_ALL2 <- IL_IN_ALL2[!outliers(IL_IN_ALL2$OIL), ]
IL_IN_ALL2 <- IL_IN_ALL2[!outliers(IL_IN_ALL2$TWT), ]
IL_IN_ALL2 <- IL_IN_ALL2[!outliers(IL_IN_ALL2$PRO), ]
IL_IN_ALL2 <- IL_IN_ALL2[!outliers(IL_IN_ALL2$KWT300), ]


# check distribution again
boxplot(IL_IN_ALL2[17:27],las = 2, plot=TRUE)$out

library(ggplot2)
for (trait in Traits) {
  Plot <- ggplot(IL_IN_ALL2, aes(x = .data[[trait]], fill = STATE, color = STATE)) +
    geom_histogram(position="identity") +
    labs(title = paste("Histogram of", trait), x = trait, y = "Frequency")
  print(Plot)
}



## ESTIMATE BLUPS

Agron <- (IL_IN_ALL2[1:26]) 
str(Agron)
# STAGE ONE ANALYSIS - FIT A SIMPLE MODEL WITH FARMERS, HYB, YEAR 

DataOutput_HYB <- data.frame(matrix(vector(),13,1, dimnames=list(c(), c("HYB"))))
DataOutput_FAR <- data.frame(matrix(vector(),12,1, dimnames=list(c(), c("FAR"))))
DataOutput_YEAR <- data.frame(matrix(vector(),3,1, dimnames=list(c(), c("YEAR"))))

#fill empty dataframe with 1-300 so that the cbind will work later on
DataOutput_HYB$HYB   <-  unique(Agron[,7]) #fill in Entry numbers
DataOutput_FAR$FAR   <-  unique(Agron[,5]) #fill in Entry numbers
DataOutput_YEAR$YEAR <-unique(Agron[,8]) #fill in Entry numbers

colnames(Agron)
str(Agron[,17:26]) #take only the columns with numerical data
colnum =c(17:ncol(Agron))

i = 1 
for(i in 1:10){ #this second loop runs through each TRAIT, one at a time
  x = colnum[i] #set the current [i] column as x
  trait <- colnames(Agron)[x] #sets the current column header as the trait name
  DF <- Agron #make a copy that we can use for our analysis
  colnames(DF)[x]="y"  #renames the trait variable as "y" for the model analysis below
  
  #We are interested in random effects, which estimates the proportion of variation and not fixed effects. 
  #Random-effects terms are distinguished by vertical bars or pipes (|) 

  library(lme4) 
  model.lmer = lmer(y ~ (1|YEAR) +(1|FAR) + (1|HYB) + + (1|FAR:HYB) + (1|YEAR:FAR:HYB), data= DF)
  summary(model.lmer) 
  
  ## normality test
  #qqnorm(residuals(model.lmer), pch = 1, frame = FALSE)
  #qqline(residuals(model.lmer), col = "steelblue", lwd = 2)
  #hist(residuals(model.lmer))
  #boxplot(residuals(model.lmer))
  #shapiro.test((residuals(model.lmer)))
  
  varComp<-as.data.frame(VarCorr(model.lmer,comp="vcov")) #function calculates estimated variances between random-effects terms in a mixed-effects model  blup = coef(model)$Entry
  HYB_Blups <- coef(model.lmer)$HYB #coef extracts model coefficients from lmer objects returned by modeling functions
  HYB_Blups <- round(HYB_Blups, 1)
  colnames(HYB_Blups) <- trait #rename the BLUP column by the trait in our loop
  
  FAR_Blups <- coef(model.lmer)$FAR
  FAR_Blups <- round(FAR_Blups, 1)
  colnames(FAR_Blups) <- trait #rename the BLUP column by the trait in our loop
  
  YEAR_Blups <- coef(model.lmer)$YEAR
  YEAR_Blups <- round(YEAR_Blups, 1)
  colnames(YEAR_Blups) <- trait #rename the BLUP column by the trait in our loop

  #hist(HYB_Blups[,1], main = paste("Histogram of", names(HYB_Blups)[i]))
  #add columns to existing dataframe  
  DataOutput_HYB <- cbind(DataOutput_HYB,HYB_Blups) #ammends our dataframe with the new BLUP column for the new trait
  DataOutput_FAR <- cbind(DataOutput_FAR,FAR_Blups) 
  DataOutput_YEAR <- cbind(DataOutput_YEAR,YEAR_Blups) 
}

DataOutput_HYB <- DataOutput_HYB[, -1]
DataOutput_HYB <- cbind(HYB =rownames(DataOutput_HYB), data.frame(DataOutput_HYB, row.names=NULL))

DataOutput_FAR <- DataOutput_FAR[, -1]
DataOutput_FAR <- cbind(FAR =rownames(DataOutput_FAR), data.frame(DataOutput_FAR, row.names=NULL))

DataOutput_YEAR <- DataOutput_FAR[, -1]
DataOutput_YEAR <- cbind(YEAR =rownames(DataOutput_YEAR), data.frame(DataOutput_YEAR, row.names=NULL))


#create functions for summary statistics 
colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)
colMean <- function(data) sapply(data, mean, na.rm = TRUE)
colMedian <- function(data) sapply(data, median, na.rm = TRUE)
colStdev <- function(data) sapply(data, sd, na.rm = TRUE)
std_error <- std_error <- function(data)sapply{sd(data)/sqrt(length(data))}
#extract summary statistics
Max <- colMax(DF[,colnum])
Min <- colMin(DF[,colnum])
Mean <- colMean(DF[,colnum])
Stdev <- colStdev(DF[,colnum])
SE <- Stdev/(sqrt(length(DF[,colnum])))
DataVarCompOutput <- cbind(Min,Max,Mean,Median,SE)

## Export Results
#write.csv(DataVarCompOutput , "Summary_Stats.csv")
#write.csv(DataOutput_HYB, "BLUPs_HYB.csv")
#write.csv(DataOutput_FAR, "BLUPs_FAR.csv")
#write.csv(DataOutput_YEAR, "BLUP_YEAR.csv")


## Checking for significant differences
### using the lm function, we can conduct mean separations, and assign significant differences

library(agricolae)
model.lm = lm(YIELD ~ YEAR+ FAR +  HYB + FAR:HYB + YEAR:FAR:HYB, data= DF)
anova(model.lm)
means <- LSD.test(model.lm,"HYB",alpha = 0.05, p.adj = c("none","holm","hommel", "hochberg", "bonferroni", "BH", "BY", "fdr"), group=TRUE)
print(means$groups)

mean_sep  <- data.frame(matrix(vector(),13,1, dimnames=list(c(), c("HYB"))))
mean_sep$HYB <- unique(Agron[,7])

Meansep_year <- data.frame(matrix(vector(),3,1, dimnames=list(c(), c("YEAR"))))
Meansep_year$YEAR <- unique(Agron[,8])

Meansep_Far <- data.frame(matrix(vector(),12,1, dimnames=list(c(), c("FAR"))))
Meansep_Far$FAR <- unique(Agron[,5])


# Loop through each trait
for (trait in colnames(Agron)[17:26]) {
  # Fit linear model
  model <- lm(paste(trait, "~ YEAR + FAR + HYB + FAR:HYB + YEAR:FAR:HYB"), data = Agron)
  
  #LSD_test  <- HSD.test(model, "HYB", group = TRUE)  # Conduct Tukey's HSD test
  LSD_test <-  LSD.test(model,alpha = 0.05, "HYB",p.adj = "bonferroni")
  # Extract mean and groups
  mean_groups <- data.frame(LSD_test$groups)
  mean_groups  <- cbind(HYB =rownames(mean_groups), data.frame(mean_groups, row.names=NULL))
  mean_groups <- mean_groups %>%
                mutate(!!paste0(trait, "g") := groups) 
  mean_groups <- mean_groups[c(1:2,4)] 
  mean_groups <- mean_groups %>%
    mutate_at(2, ~ round(., digits = 1))
  # Append results to the dataframe
  mean_sep <- left_join(mean_sep, mean_groups, by = "HYB")  

  LSD_FAR <-  LSD.test(model,alpha = 0.01, "FAR",p.adj = "bonferroni")
  # Extract mean and groups
  mean_groups <- data.frame( LSD_FAR$groups)
  mean_groups  <- cbind(FAR =rownames(mean_groups), data.frame(mean_groups, row.names=NULL))
  mean_groups <- mean_groups %>%
    mutate(!!paste0(trait, "g") := groups) 
  mean_groups <- mean_groups[c(1:2,4)] 
  mean_groups <- mean_groups %>%
    mutate_at(2, ~ round(., digits = 1))
  # Append results to the dataframe
  Meansep_Far  <- left_join(Meansep_Far, mean_groups, by = "FAR")  
  
  
  LSD_YEAR <-  LSD.test(model,alpha = 0.05, "YEAR",p.adj = "bonferroni")
  # Extract mean and groups
  mean_groups <- data.frame( LSD_YEAR$groups)
  mean_groups  <- cbind(YEAR =rownames(mean_groups), data.frame(mean_groups, row.names=NULL))
  mean_groups <- mean_groups %>%
    mutate(!!paste0(trait, "g") := groups) 
  mean_groups <- mean_groups[c(1:2,4)] 
  mean_groups <- mean_groups %>%
    mutate_at(2, ~ round(., digits = 1))
  # Append results to the dataframe
  Meansep_year <- left_join(Meansep_year, mean_groups, by = "YEAR")  
  
}

# Print the resulting dataframe
write.csv(mean_sep , "HSD_test_HYB.csv")
write.csv(Meansep_year , "HSD_test_year.csv")
write.csv(Meansep_Far , "HSD_test_far.csv")


### ____TRAIT CORRELATIONS _____________
## Used the extracted hybrid blups to check for correlations

library(car)
library(corrplot)
Correlation <- cor(DataOutput_HYB[,c(2:11)], use = "pairwise.complete.obs")
png("Trait_Correlations.png", width = 20, height = 20, units = "cm", res = 300)
corplot<- corrplot(Correlation,method = "color",type="upper", order="hclust", #Type = upper,lower, #method=circle,pie,color,number
         addCoef.col="black", # Add coefficient of correlation
         diag=FALSE, # hide correlation coefficient on the principal diagonal
         tl.col="black", tl.srt=45, #Text label color and rotation
         p.mat=NULL, sig.level = 0.01, insig = "blank")  # Add coefficient of correlation

print(corplot)
dev.off()
#invisible(dev.off())



### ____STAGE 2 ANALYSIS _____________ AGRONOMIC PRACTICES _____________

## 1. iLLINOIS AND INDIANA

## Using stepwise regression with step() function
Data <- Agron[c(7,10:16,23)]
full_model <- lm(YIELD ~., data = Data)
step_model <- step(full_model, direction = "backward")
anova(step_model)

#with interactions
step_model2  <- step(full_model, scope = . ~ .^2, direction = 'both')
anova(step_model2)

## Removing interactions that are not significant

step_model3  <- lm(formula = YIELD ~ HYB + FS + CC + WRT + Density + CI + CD + 
                     PER + HYB:Density + HYB:WRT + HYB:CI, data = Data)
anova <- anova(step_model3)

## Mean separations
LSD_FS   <-  LSD.test(step_model3,alpha = 0.05, "FS",p.adj = "bonferroni")
LSD_WRT  <-  LSD.test(step_model3,alpha = 0.05, "WRT",p.adj = "bonferroni")
LSD_Dens <-  LSD.test(step_model3,alpha = 0.05, "Density",p.adj = "bonferroni")
LSD_CI   <-  LSD.test(step_model3,alpha = 0.05, "CI",p.adj = "bonferroni")
LSD_CD   <-  LSD.test(step_model3,alpha = 0.05, "CD",p.adj = "bonferroni")
LSD_PER  <-  LSD.test(step_model3,alpha = 0.05, "PER",p.adj = "bonferroni")


#check number of datapoints for each management level
table(Agron$PER)
table(Agron$CI)
table(Agron$CD)
table(Agron$WRT)
table(Agron$FS)
table(Agron$Density)

# 1. Fertility source
LSD_df <- as.data.frame(LSD_FS$means)
LSD_df <- cbind(FS =rownames(LSD_df), data.frame(LSD_df, row.names=NULL))
LSD_groups <- as.data.frame(LSD_FS$groups)
LSD_groups <- cbind(FS =rownames(LSD_groups), data.frame(LSD_groups, row.names=NULL))
LSD_groups <- LSD_groups[-2]

# Plot the bar graph
order <- c("Green", "Mix", "Animal")
LSD_df$FS <- factor(LSD_df$FS, levels = order)
LSD_df <- merge(LSD_df, LSD_groups, by = "FS")

fs <- ggplot(data = LSD_df , aes(y = YIELD, x = FS, fill = FS, color = FS)) +
       geom_bar(stat = "identity",width = 0.9, color="black",position=position_dodge(width=1)) +
       geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, color = "black", linewidth = 0.6, position = position_dodge(0.9)) +
       geom_text(data = LSD_df, aes(label = groups, y = UCL + 0.5), color = "black", size = 5, position = position_dodge(width = 0.9)) +  
       labs(title = "",
            x = "Fertility Sources",
            y = "Grain Yield [t/Ha]") +
       scale_y_continuous(breaks = seq(0, 14, by = 2.5)) +  # Adjust y-axis breaks
       theme_classic() +
       guides(fill = "none", color = "none") +  # Remove legend for fill and color aesthetics
       #theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, size = 12),  # Increase x-axis label font size
           axis.text.y = element_text(size = 12),  # Increase y-axis label font size
           axis.title = element_text(size = 14),
           plot.title = element_text(hjust = 0.5))   # Increase axis title font size
     
#2. Weed pressure

LSD_wp <- as.data.frame(LSD_WRT$means)
LSD_wp <- cbind(WRT =rownames(LSD_wp), data.frame(LSD_wp, row.names=NULL))
groups_wp <- as.data.frame(LSD_WRT$groups)
groups_wp <- cbind(WRT =rownames(groups_wp), data.frame(groups_wp, row.names=NULL))
groups_wp <- groups_wp[-2]


# Plot the bar graph
order_wp <- c("Low", "Moderate", "High")
LSD_wp$WRT  <- factor(LSD_wp$WRT, levels = order_wp)
LSD_wp   <- merge(LSD_wp, groups_wp, by = "WRT")

wp <- ggplot(data = LSD_wp, aes(y = YIELD, x =WRT, fill = WRT, color = WRT)) +
       geom_bar(stat = "identity",width = 0.9, color="black",position=position_dodge(width=1)) +
       geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, color = "black", linewidth = 0.6, position = position_dodge(0.9)) +
       geom_text(data = LSD_wp, aes(label = groups, y = UCL + 0.5), color = "black", size = 5, position = position_dodge(width = 0.9)) +  
       labs(title = "",
            x = "Weed Pressure Rate",
            y = "") +
       scale_y_continuous(breaks = seq(0, 14, by = 2.5)) +  # Adjust y-axis breaks
       theme_classic() +
       guides(fill = "none", color = "none") +  # Remove legend for fill and color aesthetics
       #theme_minimal() +
       theme(axis.text.x = element_text(angle = 0, size = 12),  # Increase x-axis label font size
             axis.text.y = element_text(size = 12),  # Increase y-axis label font size
             axis.title = element_text(size = 14),
             plot.title = element_text(hjust = 0.5))   # Increase axis title font size


# 3. Planting Density

LSD_pd <- as.data.frame(LSD_Dens$means)
LSD_pd <- cbind(Density =rownames(LSD_pd), data.frame(LSD_pd, row.names=NULL))
groups_pd <- as.data.frame(LSD_Dens$groups)
groups_pd <- cbind(Density =rownames(groups_pd ), data.frame(groups_pd, row.names=NULL))
groups_pd <- groups_pd[-2]


# Plot the bar graph
order_pd <- c("Low", "Medium", "High")
LSD_pd$Density <- factor(LSD_pd$Density, levels = order_pd)
LSD_pd <- merge(LSD_pd, groups_pd, by = "Density")

pd <- ggplot(data = LSD_pd , aes(y = YIELD, x = Density, fill = Density, color = Density)) +
       geom_bar(stat = "identity",width = 0.9, color="black",position=position_dodge(width=1)) +
       geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, color = "black", linewidth = 0.6, position = position_dodge(0.9)) +
       geom_text(data = LSD_pd, aes(label = groups, y = UCL + 0.5), color = "black", size = 5, position = position_dodge(width = 0.9)) +  
       labs(title = "",
            x = "Planting Density",
            y = "") +
       scale_y_continuous(breaks = seq(0, 14, by = 2.5)) +  # Adjust y-axis breaks
       theme_classic() +
       guides(fill = "none", color = "none") +  # Remove legend for fill and color aesthetics
       #theme_minimal() +
       theme(axis.text.x = element_text(angle = 0, size = 12),  # Increase x-axis label font size
             axis.text.y = element_text(size = 12),  # Increase y-axis label font size
             axis.title = element_text(size = 14),
             plot.title = element_text(hjust = 0.5))   # Increase axis title font size


# 4. CI 

LSD_ci <- as.data.frame(LSD_CI$means)
LSD_ci <- cbind(CI =rownames(LSD_ci), data.frame(LSD_ci, row.names=NULL))
groups_ci <- as.data.frame(LSD_CI$groups)
groups_ci <- cbind(CI =rownames(groups_ci), data.frame(groups_ci, row.names=NULL))
groups_ci <- groups_ci[-2]


# Plot the bar graph
order_ci <- c("High", "Medium", "Low")
LSD_ci$CI <- factor(LSD_ci$CI, levels = order_ci)
LSD_ci <- merge(LSD_ci,groups_ci, by = "CI")

ci <- ggplot(data = LSD_ci , aes(y = YIELD, x = CI, fill =CI, color = CI)) +
       geom_bar(stat = "identity",width = 0.9, color="black",position=position_dodge(width=1)) +
       geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, color = "black", linewidth = 0.6, position = position_dodge(0.9)) +
       geom_text(data = LSD_ci, aes(label = groups, y = UCL + 0.5), color = "black", size = 5, position = position_dodge(width = 0.9)) +  
       labs(title = "",
            x = "Cropping Intensity",
            y = "Grain Yield [t/Ha]") +
       scale_y_continuous(breaks = seq(0, 14, by = 2.5)) +  # Adjust y-axis breaks
       theme_classic() +
       guides(fill = "none", color = "none") +  # Remove legend for fill and color aesthetics
       #theme_minimal() +
       theme(axis.text.x = element_text(angle = 0, size = 12),  # Increase x-axis label font size
             axis.text.y = element_text(size = 12),  # Increase y-axis label font size
             axis.title = element_text(size = 14),
             plot.title = element_text(hjust = 0.5))   # Increase axis title font size



# 5. CD

LSD_cd <- as.data.frame(LSD_CD$means)
LSD_cd <- cbind(CD =rownames(LSD_cd), data.frame(LSD_cd, row.names=NULL))
groups_cd <- as.data.frame(LSD_CD$groups)
groups_cd <- cbind(CD =rownames(groups_cd), data.frame(groups_cd, row.names=NULL))
groups_cd <- groups_cd[-2]


# Plot the bar graph
order_cd <- c("High", "Medium", "Low")
LSD_cd$CD <- factor(LSD_cd$CD, levels = order_cd)
LSD_cd <- merge(LSD_cd, groups_cd, by = "CD")

cd <- ggplot(data = LSD_cd , aes(y = YIELD, x = CD, fill = CD, color = CD)) +
      geom_bar(stat = "identity",width = 0.9, color="black",position=position_dodge(width=1)) +
      geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, color = "black", linewidth = 0.6, position = position_dodge(0.9)) +
      geom_text(data = LSD_cd, aes(label = groups, y = UCL + 0.5), color = "black", size = 5, position = position_dodge(width = 0.9)) +  
      labs(title = "",
           x = "Crop Diversity",
           y = "") +
      scale_y_continuous(breaks = seq(0, 14, by = 2.5)) +  # Adjust y-axis breaks
      theme_classic() +
      guides(fill = "none", color = "none") +  # Remove legend for fill and color aesthetics
      #theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, size = 12),  # Increase x-axis label font size
            axis.text.y = element_text(size = 12),  # Increase y-axis label font size
            axis.title = element_text(size = 14),
            plot.title = element_text(hjust = 0.5))   # Increase axis title font size

# 6. Perrenials 

LSD_pe <- as.data.frame(LSD_PER$means)
LSD_pe <- cbind(PER =rownames(LSD_pe), data.frame(LSD_pe, row.names=NULL))
groups_pe <- as.data.frame(LSD_PER$groups)
groups_pe <- cbind(PER =rownames(groups_pe), data.frame(groups_pe, row.names=NULL))
groups_pe <- groups_pe[-2]


# Plot the bar graph
order_pe <- c("Absent","Present")
LSD_pe$PER <- factor(LSD_pe$PER, levels = order_pe)
LSD_pe <- merge(LSD_pe, groups_pe, by = "PER")

pe <- ggplot(data = LSD_pe , aes(y = YIELD, x = PER, fill = PER, color = PER)) +
      geom_bar(stat = "identity",width = 0.9, color="black",position=position_dodge(width=1)) +
      geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, color = "black", linewidth = 0.6, position = position_dodge(0.9)) +
      geom_text(data = LSD_pe, aes(label = groups, y = UCL + 0.5), color = "black", size = 5, position = position_dodge(width = 0.9)) +  
      labs(title = "",
           x = "Perrenials",
           y = "" ) +
      scale_y_continuous(breaks = seq(0, 14, by = 2.5)) +  # Adjust y-axis breaks
      theme_classic() +
      guides(fill = "none", color = "none") +  # Remove legend for fill and color aesthetics
      #theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, size = 12),  # Increase x-axis label font size
            axis.text.y = element_text(size = 12),  # Increase y-axis label font size
            axis.title = element_text(size = 14),
            plot.title = element_text(hjust = 0.5))   # Increase axis title font size


library(gridExtra)
grid.arrange(fs, wp, pd,ci, cd, pe, ncol = 3) 




## Effect of management on other traits  ________make it a loop somehow___

Data2 <- Agron[c(7,10:16,17:22, 24:26)]
str(Data2)

Traits <- c("PHT","EHT","SDL","SDS","TWT","KWT300","PRO","OIL","STA")

FS   <- data.frame(FS = factor(c("Animal", "Mix", "Green")))
WRT  <- data.frame(WRT = factor(c("Low", "Moderate", "High")))
Dens <- data.frame(Density = factor(c("Low", "Medium", "High")))
CI   <- data.frame(CI = factor(c("High", "Medium", "Low")))
CD   <- data.frame(CD = factor(c("High", "Low")))
PER  <- data.frame(PER = factor(c("Absent", "Present")))

for (trait2 in Traits){
  step_model <- lm(formula = paste(trait2, "~ HYB + FS + CC + WRT + Density + CI + CD + PER + HYB:Density + HYB:WRT + HYB:CI"), data = Data2)

    ## Mean separations
    LSD_FS   <-  LSD.test(step_model,alpha = 0.05, "FS",p.adj = "bonferroni")
    LSD_WRT  <-  LSD.test(step_model,alpha = 0.05, "WRT",p.adj = "bonferroni")
    LSD_Dens <-  LSD.test(step_model,alpha = 0.05, "Density",p.adj = "bonferroni")
    LSD_CI   <-  LSD.test(step_model,alpha = 0.05, "CI",p.adj = "bonferroni")
    LSD_CD   <-  LSD.test(step_model,alpha = 0.05, "CD",p.adj = "bonferroni")
    LSD_PER  <-  LSD.test(step_model,alpha = 0.05, "PER",p.adj = "bonferroni")
    
    LSD_df <- as.data.frame(LSD_FS$means)
    LSD_df <- cbind(FS =rownames(LSD_df), data.frame(LSD_df, row.names=NULL))
    LSD_groups <- as.data.frame(LSD_FS$groups)
    LSD_groups <- cbind(FS =rownames(LSD_groups), data.frame(LSD_groups, row.names=NULL))
    LSD_groups <- LSD_groups[-2]
    order <- c("Green", "Mix", "Animal")
    LSD_df$FS <- factor(LSD_df$FS, levels = order)
    LSD_df <- merge(LSD_df, LSD_groups, by = "FS")
    
    LSD_wp <- as.data.frame(LSD_WRT$means)
    LSD_wp <- cbind(WRT =rownames(LSD_wp), data.frame(LSD_wp, row.names=NULL))
    groups_wp <- as.data.frame(LSD_WRT$groups)
    groups_wp <- cbind(WRT =rownames(groups_wp), data.frame(groups_wp, row.names=NULL))
    groups_wp <- groups_wp[-2]
    order_wp <- c("Low", "Moderate", "High")
    LSD_wp$WRT  <- factor(LSD_wp$WRT, levels = order_wp)
    LSD_wp   <- merge(LSD_wp, groups_wp, by = "WRT")
    
    LSD_pd <- as.data.frame(LSD_Dens$means)
    LSD_pd <- cbind(Density =rownames(LSD_pd), data.frame(LSD_pd, row.names=NULL))
    groups_pd <- as.data.frame(LSD_Dens$groups)
    groups_pd <- cbind(Density =rownames(groups_pd ), data.frame(groups_pd, row.names=NULL))
    groups_pd <- groups_pd[-2]
    order_pd <- c("Low", "Medium", "High")
    LSD_pd$Density <- factor(LSD_pd$Density, levels = order_pd)
    LSD_pd <- merge(LSD_pd, groups_pd, by = "Density")
    
    LSD_ci <- as.data.frame(LSD_CI$means)
    LSD_ci <- cbind(CI =rownames(LSD_ci), data.frame(LSD_ci, row.names=NULL))
    groups_ci <- as.data.frame(LSD_CI$groups)
    groups_ci <- cbind(CI =rownames(groups_ci), data.frame(groups_ci, row.names=NULL))
    groups_ci <- groups_ci[-2]
    order_ci <- c("High", "Medium", "Low")
    LSD_ci$CI <- factor(LSD_ci$CI, levels = order_ci)
    LSD_ci <- merge(LSD_ci,groups_ci, by = "CI")
    
    LSD_cd <- as.data.frame(LSD_CD$means)
    LSD_cd <- cbind(CD =rownames(LSD_cd), data.frame(LSD_cd, row.names=NULL))
    groups_cd <- as.data.frame(LSD_CD$groups)
    groups_cd <- cbind(CD =rownames(groups_cd), data.frame(groups_cd, row.names=NULL))
    groups_cd <- groups_cd[-2]
    order_cd <- c("High", "Low")
    LSD_cd$CD <- factor(LSD_cd$CD, levels = order_cd)
    LSD_cd <- merge(LSD_cd, groups_cd, by = "CD")
    
    LSD_pe <- as.data.frame(LSD_PER$means)
    LSD_pe <- cbind(PER =rownames(LSD_pe), data.frame(LSD_pe, row.names=NULL))
    groups_pe <- as.data.frame(LSD_PER$groups)
    groups_pe <- cbind(PER =rownames(groups_pe), data.frame(groups_pe, row.names=NULL))
    groups_pe <- groups_pe[-2]
    order_pe <- c("Absent","Present")
    LSD_pe$PER <- factor(LSD_pe$PER, levels = order_pe)
    LSD_pe <- merge(LSD_pe, groups_pe, by = "PER")
    
    LSD_df <-  LSD_df[c(1,2,13)]
    new_colname <- paste("groups", trait2, sep = "_")
    colnames(LSD_df)[3] <- new_colname
    FS <- left_join(FS, LSD_df, by = "FS")
    
    LSD_wp <-  LSD_wp[c(1,2,13)]
    colnames(LSD_wp)[3] <- new_colname
    WRT <- left_join(WRT, LSD_wp, by = "WRT")
    
    LSD_pd  <- LSD_pd[c(1,2,13)]
    colnames(LSD_pd)[3] <- new_colname
    Dens <- left_join(Dens, LSD_pd, by = "Density")
    
    LSD_ci <-  LSD_ci[c(1,2,13)]
    colnames(LSD_ci)[3] <- new_colname
    CI <- left_join(CI, LSD_ci, by = "CI")
    
    LSD_cd <-  LSD_cd[c(1,2,13)]
    colnames(LSD_cd)[3] <- new_colname
    CD <- left_join(CD, LSD_cd, by = "CD")

    LSD_pe <-  LSD_pe[c(1,2,13)]
    colnames(LSD_pe)[3] <- new_colname
    PER  <- left_join( PER, LSD_pe, by = "PER")
}

## I want to merge these different managemet practices together with their effects on each trait

mFS <- data.frame(Management = factor(c("FS", "FS", "FS"), levels = c("FS", "WRT", "Dens", "CI", "CD", "PER")),
                 Levels = factor(c("Animal", "Mix", "Green"), levels = c("Animal", "Mix", "Green")))
mWRT <- data.frame(Management = factor(c("WRT", "WRT", "WRT"), levels = c("FS", "WRT", "Dens", "CI", "CD", "PER")),
                  Levels = factor(c("Low", "Moderate", "High"), levels = levels(WRT$WRT)))
mDens <- data.frame(Management = factor(c("Dens", "Dens", "Dens"), levels = c("FS", "WRT", "Dens", "CI", "CD", "PER")),
                   Levels = factor(c("Low", "Medium", "High"), levels = levels(Dens$Density)))
mCI <- data.frame(Management = factor(c("CI", "CI", "CI"), levels = c("FS", "WRT", "Dens", "CI", "CD", "PER")),
                 Levels = factor(c("High", "Medium", "Low"), levels = levels(CI$CI)))
mCD <- data.frame(Management = factor(c("CD", "CD"), levels = c("FS", "WRT", "Dens", "CI", "CD", "PER")),
                 Levels = factor(c("High", "Low"), levels = levels(CD$CD)))
mPER <- data.frame(Management = factor(c("PER", "PER"), levels = c("FS", "WRT", "Dens", "CI", "CD", "PER")),
                  Levels = factor(c("Absent", "Present"), levels = levels(PER$PER)))

Management <- rbind(mFS, mWRT, mDens, mCI, mCD, mPER)  

# Combine all data frames into a single dataframe
Effect_Table <- rbind(FS[(2:19)],WRT[(2:19)],Dens[c(2:19)],CI[c(2:19)] ,CD[c(2:19)] ,PER[c(2:19)] )

Management_Effect <- cbind(Management,Effect_Table)
write.csv(Management_Effect, "Management_Effect_ILIN.csv")


## RELIABILITY ESTIMATION USING 59R5 AS CHECK

ReliabiltyData <- IL_IN_ALL

#Means on each farm (average of 2 reps)
library(dplyr)
IL_IN_means <- ReliabiltyData %>%
  group_by(FC, HYB) %>%
  summarize(across(PHT:GDD_Sep,~ mean(., na.rm = TRUE), .names = "{.col}"))

## Estimate yield differences, mean Yd, standard deviations, and zscores. Also estimate reliability with non-parametric method (n/env)
IL_IN_YD <- IL_IN_means[c(1,2,9)]  %>%
  group_by(FC) %>%  #FC is based on years. same farmer in different years has different codes
  mutate(YD = YIELD - YIELD[HYB == "CHECK1"]) %>%
  group_by(HYB) %>%
  mutate(MeanY = mean(YIELD, na.rm = TRUE),
         SDY = sd(YIELD, na.rm = TRUE),
         MeanYD = mean(YD, na.rm = TRUE),
         SDYD = sd(YD, na.rm = TRUE)) %>%
  mutate(Env = n_distinct(FC),    # Count the number of unique FC values
         n = sum(YD > 0)) %>%
  mutate(MeanY   =  mean(MeanY , na.rm = TRUE), 
         YD      =  mean(YD , na.rm = TRUE),
         SDY     =  mean(SDY, na.rm = TRUE),
         MeanYD  =  mean(MeanYD, na.rm = TRUE),
         SDYD    =  mean(SDYD , na.rm = TRUE),
         Env     =  mean(Env, na.rm = TRUE),
         n       =  mean(n , na.rm = TRUE))%>%
  mutate(Zscore  = MeanYD/SDYD)%>%
  mutate(Ra      = pnorm(Zscore), Rb =  (n/Env) ) %>%  #here are the 2 reliabilities 
  ungroup() 

##Remove FA, Yield, Raw YD difference and Zscores. Also remove Check variety since it doesnt have reliability values
IL_IN_Rel <- IL_IN_YD[-c(1,3,4,11)] %>%   
  filter(HYB != "CHECK1") %>%  #remove YD
  group_by(HYB) %>%
  summarise(MeanY   =  mean(MeanY , na.rm = TRUE), 
            SDY     =  mean(SDY, na.rm = TRUE),
            MeanYD  =  mean(MeanYD, na.rm = TRUE),
            SDYD    =  mean(SDYD , na.rm = TRUE),
            Env     =  mean(Env, na.rm = TRUE),
            n       =  mean(n , na.rm = TRUE),
            Ra      =  mean(Ra, na.rm = TRUE),
            Rb      =  mean(Rb , na.rm = TRUE)) %>%
  mutate(across(c(2:5, 8:9), ~round(., 2))) %>%
  ungroup ()

write.csv(IL_IN_Rel, "Table1_Reliability.csv")  #Add Check mean sd, and envi


#2. Wisconsin

#Means on each farm (average of 2 reps)


library(dplyr)
IL_IN_means <- ReliabiltyData %>%
  group_by(FC, HYB) %>%
  summarize(across(PHT:GDD_Sep,~ mean(., na.rm = TRUE), .names = "{.col}"))

## Estimate yield differences, mean Yd, standard deviations, and zscores. Also estimate reliability with non-parametric method (n/env)
IL_IN_YD <- IL_IN_means[c(1,2,9)]  %>%
  group_by(FC) %>%  #FC is based on years. same farmer in different years has different codes
  mutate(YD = YIELD - YIELD[HYB == "CHECK1"]) %>%
  group_by(HYB) %>%
  mutate(MeanY = mean(YIELD, na.rm = TRUE),
         SDY = sd(YIELD, na.rm = TRUE),
         MeanYD = mean(YD, na.rm = TRUE),
         SDYD = sd(YD, na.rm = TRUE)) %>%
  mutate(Env = n_distinct(FC),    # Count the number of unique FC values
         n = sum(YD > 0)) %>%
  mutate(MeanY   =  mean(MeanY , na.rm = TRUE), 
         YD      =  mean(YD , na.rm = TRUE),
         SDY     =  mean(SDY, na.rm = TRUE),
         MeanYD  =  mean(MeanYD, na.rm = TRUE),
         SDYD    =  mean(SDYD , na.rm = TRUE),
         Env     =  mean(Env, na.rm = TRUE),
         n       =  mean(n , na.rm = TRUE))%>%
  mutate(Zscore  = MeanYD/SDYD)%>%
  mutate(Ra      = pnorm(Zscore), Rb =  (n/Env) ) %>%  #here are the 2 reliabilities 
  ungroup() 

##Remove FA, Yield, Raw YD difference and Zscores. Also remove Check variety since it doesnt have reliability values
IL_IN_Rel <- IL_IN_YD[-c(1,3,4,11)] %>%   
  filter(HYB != "CHECK1") %>%  #remove YD
  group_by(HYB) %>%
  summarise(MeanY   =  mean(MeanY , na.rm = TRUE), 
            SDY     =  mean(SDY, na.rm = TRUE),
            MeanYD  =  mean(MeanYD, na.rm = TRUE),
            SDYD    =  mean(SDYD , na.rm = TRUE),
            Env     =  mean(Env, na.rm = TRUE),
            n       =  mean(n , na.rm = TRUE),
            Ra      =  mean(Ra, na.rm = TRUE),
            Rb      =  mean(Rb , na.rm = TRUE)) %>%
  mutate(across(c(2:5, 8:9), ~round(., 2))) %>%
  ungroup ()





######### __________________MACHINE LEARNING PHASE ________________________

### PCA FOR WEATHER AND SOIL DATA FOR DIMENSION REDUCTION
##Open ML_LinearRegression folder
## Soil and weather datasets are multidimensional and will make our models weak. 
## I conducted PCA analysis to extract PCs that are most influential to the dataset


## Move to PCA folder

# PCA for soil data
library(agricolae)
library(xtable)
Soil_Data <- read.csv("Soil_Data.csv", header = TRUE)
Soil_Data  <- type.convert(replace(Soil_Data, Soil_Data == "." & !is.na(Soil_Data), NA), as.is = TRUE)

## Use MissForest to estimate missing values
#install.packages("missForest")
library(missForest)
set.seed(123)
n_trees <- 100
imputed_data <- missForest(Soil_Data[c(10:27)], ntree = n_trees)
Clean <- imputed_data$ximp
colSums(is.na(Clean))

#export to conduct Pca in python and extract score
library(readr)
write.csv(Clean, "soilvalues.csv")

Clean2 <- cbind(Soil_Data[c(1:9)], Clean)

pca <- prcomp(Clean2[,c(10:27)], center = TRUE, scale = TRUE) #scale = true is for variables measured in different units, if values are measures in same unit, scale=false
print(pca)
summary(pca)
#pcs  <- pca$x  ## This is how you extract the pcs for dimension reduction in R
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

PCA_Data <-  cbind(Design, soilscores[c(1:6)], weatherscores[c(1:4)])  #COMPLETE DATASET

write.csv(PCA_Data, "PCA_Data.csv")


#__________Reliability calculation using ORG4 as check############

# Now Estimate Reliability and run ML Models
PCA_Data  <- read.csv("PCA_Data.csv", header = TRUE)

## Estimate hybrid means per farm before proceeding. 
Farmmeans  <-  PCA_Data%>%
                 arrange(FC, HYB)

Categorical <- Farmmeans[c(1:9,11:17)]
Numeric <- Farmmeans[c(6,7,10, 18:27)]

library(dplyr)
Farm_means <- Numeric%>%
  group_by(FC, HYB) %>%
  summarize(across(YIELD:WPC4,~ mean(., na.rm = FALSE, names = "{.col}"))) %>%
 group_by(FC, HYB)


## # Merge based on common column "ID"
#Before merging, these need to be exactly the same number of rows, so I filter categorical, to leave only rep1 per farmer
Categorical <- Categorical %>%
               group_by(FC) %>%
                filter(REP %in% c("1"))%>%
               group_by(FC)

Farm_All <- cbind(Categorical, Farm_means[-c(1:2)])

#Removing farmers without our check
library(dplyr)
filtered_df <- Farm_All  %>%
  filter(FAR != "Glazik")

## Estimating yield differences between hybrid and check (ORG4)
Yield_Diff <- filtered_df %>%
  group_by(FC) %>%
  mutate(YD = YIELD - YIELD[HYB == "ORG4"]) %>%
  ungroup()

# Model 1. Using yes or no response for reliability
Farm_Hybrid_Means <- Yield_Diff
Farm_Hybrid_Means$R <- ifelse(Farm_Hybrid_Means$YD >= 0, "Yes", "No")

Farm_Hybrid_Means  <- Farm_Hybrid_Means[-c(1, 3:5, 9,28)] #remove ENV, City, PED, FAR, Yield and YD
Farm_Hybrid_Means <- Farm_Hybrid_Means%>%filter(HYB != "ORG4")  #remove rows with ORG4 since they have Na for Reliability

write.csv(Farm_Hybrid_Means , "Farm_Hybrid_Means.csv")  #this was the model1 data





###______________________________RANDOM FOREST VANILLA ____________________________________________

# import cleaned data
Model1   <- read.csv("Farm_Hybrid_Means.csv", header = TRUE)  ##changed NA wRT to medium, and HW CI. CD, PER NAs to previous HW data
Model1  <- Model1[-1]



library(dplyr)
Training_Phase <- Model1  %>%
  filter(YEAR %in% c(2018, 2019, 2020, 2021))

Training_Phase$R <- ifelse(Training_Phase$R == "Yes", "1", "0")
Training_Phase  <- Training_Phase[-4] #remove year and rep
Training_Phase$R <- as.numeric(Training_Phase$R)


# Split training_phgase data into training and testing
#install.packages("data.table")
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
  Model_RF <- randomForest(R ~ STATE + HYB + FS + CC + Density + CI + CD + PER +
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
       scale_x_continuous(breaks = seq(0,14, by = 2))

## Node purity
var_imp2 <- as.data.frame(importance(Model_RF))
var_imp2 <- rownames_to_column(var_imp2, var = "Variable")
var_imp2 <- var_imp2[order(var_imp2$IncNodePurity), ] #rearrange
var_imp2$Variable <- reorder(var_imp2$Variable, var_imp2$IncNodePurity)

Purity <-ggplot(var_imp2, aes(x = var_imp2$IncNodePurity , y = Variable)) +
         geom_point(stat = "identity", fill = "skyblue", color = "black") +
         labs(title = "",
              y = "",
              x = "%NodePurity Increase") +
         theme_minimal() +
         theme(axis.text.x = element_text(angle = 0, hjust = 1))+
         scale_x_continuous(breaks = seq(0, 8, by = 2))

library(gridExtra)
combined_plot <- grid.arrange(MSE, Purity, ncol = 2, top = "Variable Importance From Random Forest Model") 

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
library(adabag) #for Adaptive Boosting (AdaBoost)
library(randomForest)
library(earth)
library(gbm) #for gradient boosting 
library(e1071) #for naive beyes, svm, 
library(rpart) #for decision tree

## Import data   #Before importing, I removed ORG9 because it was tested in one field. If it shows in the testing set, model will fail since it wasnt trained
Model2   <- read.csv("Farm_Hybrid_Means.csv", header = TRUE)  ##changed NA wRT to medium, and HW CI. CD, PER NAs to previous HW data

Training_Phase2 <- Model2  %>%
  filter(YEAR %in% c(2018, 2019, 2020, 2021))
Training_Phase2  <- Training_Phase2[-c(1,5)] #remove year and rep

# Split training_phase data into training and testing
set.seed(111)
split_index <- createDataPartition(Training_Phase2$R, p = 0.8, list = FALSE)
Training_set2 <- Training_Phase2[split_index, ]
Testing_set2 <- Training_Phase2[-split_index, ]

## Set logistic regression model to be fit
LR_Model <- R ~ HYB + STATE + FS + CC  + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3

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


for (i in 1:10) {
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

RF_Optboot <- train(R ~ HYB + STATE + FS + CC + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                 data = Training_set2, trControl= trControl_list$Optboot, method="rf")

RF_boot632 <- train(R ~ HYB + STATE + FS + CC + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                    data = Training_set2, trControl= trControl_list$boot632, method="rf")

Bagging_Optboot <- train(R ~ HYB + STATE + FS + CC + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                 data = Training_set2, trControl= trControl_list$Optboot, method="treebag")

Bagging_boot632 <- train(R ~ HYB + STATE + FS + CC  + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
                    data = Training_set2, trControl= trControl_list$boot632, method="treebag")

GB_Optboot <- train(R ~ HYB + STATE + FS + CC + Density + CI + CD + PER + SPC1 + SPC2 + SPC3 + SPC4 + SPC5 + SPC6 + WPC1 + WPC2 + WPC3, 
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

metrics <- function(cm) {
  accuracy <- sum(diag(cm))/sum(cm)
  precision <- cm[2,2]/sum(cm[2,])
  recall <- cm[2,2]/sum(cm[ ,2])
  baseline<- sum(cm[1,])/sum(cm)
  c(accuracy = accuracy, precision = precision, recall = recall, baseline = baseline)
}


Metric <- rbind(metrics(cm_RF_Optboot     ), 
                metrics(cm_RF_boot632     ), 
                metrics(cm_Bagging_Optboot), 
                metrics(cm_Bagging_boot632), 
                metrics(cm_GB_Optboot     ))

Metric <- as.data.frame(Metric)
Validation <- cbind(Models, Metric)
write.csv(Validation, "Validation_Accuracy.csv")




