# STRIP TRIAL DATA ANALYSIS

### Check current R version and update if necessary 
R.version.string 
install.packages("installr") 
library(installr)
updateR()  

### Clear all previous objects, values, graphics and windows
rm(list=ls(all=TRUE))
graphics.off()
shell("cls")


### Importing data
ALL <- read.csv("Data.csv", header = T) 

#### Converting grain ield from bu/acre to kg/ha and test weight from lbs/bu to kg/cm3

ALL$YIELD<- ALL$YIELD*0.0673  
ALL$TWT<- ALL$TWT*1.25

#### Specifying the design and response variables to factor and numeric variables, respectively. 

ALL$EXP <- as.factor(ALL$EXP)
ALL$YEAR <- as.factor(ALL$YEAR)
ALL$FAR <- as.factor(ALL$FAR)
ALL$STATE <- as.factor(ALL$STATE)
ALL$HYB <- as.factor(ALL$HYB)
ALL$CODE <- as.factor(ALL$CODE)
ALL$OME <- as.factor(ALL$OME)
ALL$CCT <- as.factor(ALL$CCT)
ALL$RTN <- as.factor(ALL$RTN)
ALL$WRT <- as.factor(ALL$WRT)
ALL$NSRC <- as.factor(ALL$NSRC)
ALL$MCComb <- as.factor(ALL$MCComb)
ALL$PDTY <- as.factor(ALL$PDTY)
ALL$Density <- as.factor(ALL$Density)
ALL$PROG <- as.factor(ALL$PROG)
ALL$SRC <- as.factor(ALL$SRC)
ALL$RTNL <- as.factor(ALL$RTNL)
ALL$PHT <- as.numeric(ALL$PHT)
ALL$EHT <- as.numeric(ALL$EHT)
ALL$SDL <- as.numeric(ALL$SDL)
ALL$SDS <- as.numeric(ALL$SDS)
ALL$GWT <- as.numeric(ALL$GWT)
ALL$MST <- as.numeric(ALL$MST)
ALL$TWT <- as.numeric(ALL$TWT)
ALL$THKWT <- as.numeric(ALL$THKWT)
ALL$KWT <- as.numeric(ALL$KWT)
ALL$YIELD <- as.numeric(ALL$YIELD)
ALL$PROT <- as.numeric(ALL$PROT)
ALL$OIL <- as.numeric(ALL$OIL)
ALL$STR <- as.numeric(ALL$STR)

## TAKING A SUBSET OF THE DATA
library(tidyverse)
library(dplyr)
ALL2020 <- ALL %>% filter(YEAR=="2020")
ALL2020 <- ALL %>% filter(YEAR=="2020")
ALL2020 <- ALL %>% filter(YEAR=="2020")
ALL2020 <- ALL %>% filter(YEAR=="2020")


## Simple Linear Models and Model Comparison
model10 <- lm(YIELD~CODE + YEAR + NSRC + WRT+ CCT + RTNL + Density, ALL)
mod1 <- lm(YIELD ~ CODE, data = ALL)
mod2 <- lm(YIELD ~ YEAR + CODE + FARM, data = ALL)
mod3 <- lm(YIELD ~ YEAR + CODE + FARM + FARM:CODE, data = ALL)
summary(model10)
anova(model10)
install.packages("AICcmodavg")
library(AICcmodavg)
models <- list(mod1, mod2, mod3)
model.names <- c('mod1', 'mod2', 'mod3')
aictab(cand.set = models, modnames = model.names)

### Models based on farmers management practices

model1 <- lm(YIELD~CODE, ALL)
model2 <- lm(YIELD~CODE + YEAR, ALL)
model3<- lm(YIELD~CODE + YEAR + STATE, ALL)
model4 <- lm(YIELD~CODE + YEAR + STATE + CODE:YEAR, ALL)
model5 <- lm(YIELD~CODE + YEAR + STATE + CODE:YEAR + STATE:CODE, ALL)
model6<- lm(YIELD~CODE + YEAR + STATE + CODE:YEAR + STATE:CODE+ NSRC,ALL)
model7<- lm(YIELD~CODE + YEAR + STATE + CODE:YEAR + STATE:CODE + NSRC + WRT,ALL)
model8 <- lm(YIELD~CODE + YEAR + STATE + CODE:YEAR + STATE:CODE + NSRC + WRT + CCT, ALL)
model9 <- lm(YIELD~CODE + YEAR + STATE + CODE:YEAR + STATE:CODE + NSRC + WRT+ CCT + RTNL, ALL) 
model10 <- lm(YIELD~CODE + YEAR + STATE + CODE:YEAR + STATE:CODE + NSRC + WRT+ CCT + RTNL + Density, ALL)
model11 <- lm(YIELD~CODE + YEAR + STATE + CODE:YEAR + STATE:CODE+ NSRC + WRT+ CCT + RTNL + Density + CODE:NSRC, ALL)
model12 <- lm(YIELD~CODE + YEAR + STATE + CODE:YEAR + STATE:CODE + NSRC + WRT+ CCT + RTNL + Density + CODE:NSRC + CODE:WRT, ALL)
model13 <- lm(YIELD~CODE + YEAR + STATE + CODE:YEAR + STATE:CODE+ NSRC + WRT+ CCT + RTNL + Density + CODE:NSRC + CODE:WRT + CODE:CCT, ALL)


## Collective model comparison using AIC 
install.packages("AICcmodavg")
library(AICcmodavg)
models <- list(model1, model2, model3,model4, model5,model6,model7,model8,model9,model10,model11,model12,model13)
model.names <- c('model1', 'model2', 'model3','model4', 'model5','model6','model7','model8',"model9","model10","model11","model12","model13")
aictab(cand.set = models, modnames = model.names)

anova(model8,model9,test="Chisq")  #pairwise model comparison


### Using mixed linear model, some variables are random
require(lme4)
library(jtools)
library(lmerTest)
library(car)
mod1 <- lmer(YIELD ~ (1|YEAR)  + CODE +  NSRC + WRT+ CCT + RTNL + Density, (1|YEAR:CODE:NSRC:WRT:CCT:RTNL:Density),REML = FALSE, data=ALL)
mod1 <- lmer(YIELD ~ (1|YEAR)  + CODE +  NSRC + WRT+ CCT + RTNL + Density, data = ALL)
mod2 <- lmer(YIELD ~ (1|YEAR)  + CODE +  NSRC + WRT+ CCT + RTNL + Density + (1|YEAR:CODE), data = ALL)
summary(mod1)
Anova(mod1,type=3, test.statistic = "F")
Anova(mod2,type=3, test.statistic = "F")

install.packages("AICcmodavg")
library(AICcmodavg)
models <- list(model1, model2)
model.names <- c('model1', 'model2')
aictab(cand.set = models, modnames = model.names)

require(nlme)
vc<- VarCorr(model)
print(vc,comp=c("Variance","Std.Dev."),digits=2) ###gives you both standard deviation and variance
anova(model)


##ANOVA Precedure
anova(model6)
library(agricolae)
out <- LSD.test(model,"CODE")


####Calculating R2 or Coefficient of Determination####

f <- model7
y <- f$model$YIELD
yhat <- f$fitted.values
calc_rsquared(y = y, yhat = yhat)

###OR####
summary(modelyield)$r.squared



#############LSD Least significant differences ###############

library(agricolae)
modelyield <- lm(YIELD ~ CODE + YEAR + NSRC + WRT + CCT + RTNL + Density, ALL)
modelpht <- lm(PHT ~ CODE + YEAR + NSRC + WRT+ CCT + RTNL + Density, ALL)
modeleht <- lm(EHT ~ CODE + YEAR + NSRC + WRT+ CCT + RTNL + Density, ALL)
modelsdl <- lm(SDL ~ CODE + YEAR + NSRC + WRT+ CCT + RTNL + Density, ALL)
modelsds <- lm(SDS ~ CODE + YEAR + NSRC + WRT+ CCT + RTNL + Density, ALL)
modelmst <- lm(MST ~ CODE + YEAR + NSRC + WRT+ CCT + RTNL + Density, ALL)
modeltwt <- lm(TWT ~ CODE + YEAR + NSRC + WRT+ CCT + RTNL + Density, ALL)
modelkwt <- lm(KWT ~ CODE + YEAR + NSRC + WRT+ CCT + RTNL + Density, ALL)
modelprot <- lm(PROT ~ CODE + YEAR + NSRC + WRT+ CCT + RTNL + Density, ALL)
modelstr <- lm(STR ~ CODE + YEAR + NSRC + WRT+ CCT + RTNL + Density, ALL)
modeloil <- lm(OIL ~ CODE + YEAR + NSRC + WRT+ CCT + RTNL + Density, ALL)


library(agricolae)
outyield <- LSD.test(modelyield,alpha = 0.05,"CCT") 
outpht <- LSD.test(modelpht,alpha = 0.05,"Density") 
outeht <- LSD.test(modeleht,alpha = 0.05,"Density") 
outkwt <- LSD.test(modelkwt,alpha = 0.05,"Density") 
outtwt <- LSD.test(modeltwt,alpha = 0.05,"Density") 
outsdl <- LSD.test(modelsdl,alpha = 0.05,"Density") 
outsds <- LSD.test(modelsds,alpha = 0.05,"Density") 
outmst <- LSD.test(modelmst,alpha = 0.05,"Density") 
outstr <- LSD.test(modelstr,alpha = 0.05,"RTNL") 
outprot <- LSD.test(modelprot,alpha = 0.05,"RTNL") 
outoil <- LSD.test(modeloil,alpha = 0.05,"Density") 


library(agricolae)
outyear <- LSD.test(modelyield,alpha = 0.05, "YEAR")
outcct <- LSD.test(modelyield,alpha = 0.05, "CCT")
outnsrc <- LSD.test(modelyield,alpha = 0.05,"NSRC")
outwrt <- LSD.test(modelyield,alpha = 0.05,"WRT")
outrtn<- LSD.test(modelyield,alpha = 0.05,"RTN")
outdensity<- LSD.test(modelyield,alpha = 0.05,"Density")

par(mfrow=c(1,2),cex=0.85,cex.axis = 1,cex.lab=1.2,lwd=2)
#par(mfrow=c(1,2),cex=1)
bar.err(outyear$means,variation = "SE", xlab="", ylab="Grain yield [ton/ha]",las=2, ylim=c(0,12), col = "grey44")
bar.err(outcct$means,variation = "SE",  xlab="", ylab="Grain yield [ton/ha]",las=1, ylim=c(0,12),las=2,col = "grey44")
bar.err(modelyield$means,variation = "IQR", xlab="",lwd=1.3, ylab="Grain yield [ton/ha]",las=2, ylim=c(0,12), col = "grey44")
bar.err(outwrt$means,variation = "SE", xlab="", ylab="Grain yield [ton/ha]", ylim=c(0,12), las=2, col = "grey44")
bar.err(outrtn$means,variation = "SE", xlab="", ylab="Grain yield [ton/ha]", ylim=c(0,12), las=2, col = "grey44")
bar.err(outdensity$means,variation = "SE",  xlab="", ylab="Grain yield [ton/ha]", ylim=c(0,12),las=2,  col = "grey44")

###plots with mean separations###
plot(out1,las=2)
plot(out2)
plot(out3)
plot(out4)


############################GROUPED BAR GRAPHS###MULTIPLE LEVELS OF THE INDEPENDENT VARIABLES########

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(lubridate)

se = sd(ALL$YIELD)/sqrt(length(ALL$YIELD))

ALL %>% 
  group_by(WRT,SRC) %>% 
  mutate(se = sd(YIELD)/sqrt(length(YIELD))) %>% 
  ggplot(aes(x = WRT, y = YIELD, color=SRC , fill = SRC)) +  
  geom_bar(stat="identity", alpha=0.5,
           position=position_dodge()) +
  geom_errorbar(aes(ymin =YIELD-se, ymax=YIELD+se), width=.25, colour="black", 
                position=position_dodge(0.9)) +
  labs(title="Hybrid Response to Weed Pressure", 
       x="Weed Pressure Levels", y = "Yield [Ton/Ha]")+
  scale_fill_brewer(palette="Paired") + theme_minimal()



##########Normality#######

par(mfrow=c(2,2),cex=1)
qqnorm(residuals(modelyield), pch = 1, frame = FALSE)
qqline(residuals(modelyield), col = "steelblue", lwd = 2)
qqPlot(residuals(modelyield))
hist(residuals(modelyield))
boxplot(residuals(modelyield))
shapiro.test((residuals(mod1)))


#####MULTIPLE REGRESSION HOMOSCEDASTICITY ##MULTIPLE REGRESSION HOMOSCEDASTICITY

plot(model4,3)
#This plot shows if residuals are spread equally along the ranges of predictors. 
#it is good if you see a horizontal line with equally spread points. 

# Non-Constant Error Variance https://rpubs.com/aryn999/LinearRegressionAssumptionsAndDiagnosticsInR (Perse lab stat)
#1 ncvTest() For Homoscedasticity
library(car)
ncvTest(modelyield)

#2  Breusch-Pagan Test For Homoscedasticity
library(quantmod)
library(lmtest)
bptest(model6) 


####BOX PLOTS FOR GRAIN QUALITY ###

library(ggplot2)
ggplot(ALL, aes(x=RTNL, y=YIELD, fill=CODE)) 
+
  geom_boxplot(lwd=1.2) +
  scale_fill_manual(values=c("#69b3a2", "turquoise", "red", "blue", "orange", "green", "yellow")) +
  scale_color_manual(values=c("black", "firebrick4", "tan3")) +
  theme(text=element_text(size=20, angle=360))

library(ggplot2)
ggplot(ALL, aes(x=NSRC, y=YIELD, fill=PROG))  +
  geom_boxplot(alpha=0.5) +
  theme_minimal()+
  theme(legend.position="bottom") +
  scale_fill_brewer(palette="Dark2")
  
  
  ggplot(ALL, aes(x=WRT, y=PROT, fill=WRT)) +
  geom_boxplot() 

ggplot(ALL, aes(x=CCT, y=PROT, fill=CCT)) +
  geom_boxplot() 

ggplot(ALL, aes(x=RTN, y=PROT, fill=RTN)) +
  geom_boxplot() 

## BOX PLOTS WITHOUT GGPLOT

boxplot(COmpiled$YIELD ~ Compiled$Code,
        col=c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"),
        ylab= "Grain Yield [Bu/Acre]", xlab="",las=2, cex.lab=1.5, cex.axis=0.2, lwd=1.5)

boxplot(ALL$YIELD ~ ALL$SRC, 
        col= c("chocolate3", "steelblue3","darkorchid", "palegreen2"),
        ylab= "Grain Yield [ton/ha]", xlab="",las=1, cex=1, cex.axis=1.2, lwd=1.7,cex.lab=1.5)

modelyield <- lm(YIELD ~ SRC, ALL)
anova(modelyield)
out1 <- LSD.test(modelyield,"SRC")


boxplot(Compiled$YIELD ~ Compiled$Set , 
        col=c("red", "#2AEBD7", "#732AEB", "#EBE22A"),
        ylab="Plant Height [cm]" , xlab="Rotation length", las=2)


mod1<-lm(STR ~  YEAR + CODE + PROG, data = ALL)
out1 <- LSD.test(mod1,"PROG") 


########################PLOTING INTERRACTIONS ################

library(sjPlot)
library(sjmisc)
library(ggplot2)
theme_set(theme_sjplot())
# fit model with interaction
fit <- lm(YIELD~CODE + NSRC + WRT + CODE*NSRC + CODE*WRT + NSRC*WRT + CODE*NSRC*WRT ,ALL)
fit2 <- lm(YIELD~CODE + FARM +YEAR + CODE*YEAR + CODE*YEAR*FARM,ALL) 
par(mfrow=c(1,2),cex=0.6)
plot_model(fit2, type = "int", terms = c("FARM", "YEAR"))


########PCA TO GROUP FARMERS ###################PCA TO GROUP FARMERS ###################PCA TO GROUP FARMERS ############

pca <- prcomp(ALL[,c(22:31)], center = TRUE, scale = TRUE) #scale = true is for variables measured in different units, if values are measures in same unit, scale=false
print(pca)
summary(pca)

plot(pca)
screeplot(pca, type = "line", main = "Scree plot")

library(factoextra)
fviz_eig(pca)  ###Scree plot with a line


install.packages("ggfortify")
install.packages("devtools")
install_github("vqv/ggbiplot", force = TRUE)

library(devtools)
library(ggplot2)
library(plyr)
library(scales)
library(grid)
require(ggbiplot)
bplot<- ggbiplot(pcobj=pca,
                 choices=c(1,2),
                 obs.scale=1, var.scale=1,
                 labels = row.names(ALL),
                 varname.size = 3,
                 varname.abbrev = FALSE,
                 var.axes = TRUE, #arrows
                 circle = FALSE,
                 ellipse = TRUE, groups=ALL$YEAR)
print(bplot)


###########BOX PLOTS ####################################BOX PLOTS ############
library(ggplot2)
ggplot(ALL, aes(x = PDTY, y = YIELD, fill = CODE, color = CODE, las=2)) +
  geom_boxplot(outlier.colour="Black", outlier.shape=1,
               outlier.size=2, notch = FALSE,varwidth = FALSE) +
  theme(panel.background = element_rect(fill = "grey", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        color = "white"))


library(ggplot2)
ggplot(ALL, aes(x = MCComb, y = YIELD, group = MCComb, col = MCComb)) + 
  geom_point() + stat_summary(fun.y = mean, geom = "line")


#######################Estimating genetic variability ###################3

library(variability)
gen.var(data, genotypevector, replicationvector)
gen.var(ALL [19:32], ALL$CODE, ALL$FARM)


######################### PEARSON CORRELATION ###################### PEARSON CORRELATION ############# PEARSON CORRELATION ###

install.packages("read_excel")            #######reads excel files
install.packages("Hmisc")                 #######produces correlation matrices
install.packages("PerformanceAnalytics")  #######will be used to produce a figure that shows a lot!
install.packages("car")                   ####### will be used for scatterplot    



##########################################################

library(readxl)       #### pull up out read excel package
Corrdata<- read_excel("C:/Users/prc/Desktop/Covid-19/BOOK/Correlation data/correlations.xlsx")
View(Corrdata)        #### look at our data (dont need to run this!)
attach(Corrdata)      #### means R can search our data and find column titles 


##################simplest form of correlation command

cor(Pain_Tolerance_P, Tears_P) #### real basic correlation fuction, gives a pearson coefficent
cor(Pain_Tolerance_P, Tears_P, method = "spearman") ##### change pearson to "spearman" or "Kendall"
cor(Pain_Tolerance_P, Tears_P, method = "kendall") ##### change pearson to "spearman" or "Kendall"


####################Full set of info for your correlations

cor.test(Pain_Tolerance_P, Tears_P)#### this gives you more detail, p and CI. As default it's 
#### pearsons, two tailed, with 95% CI
cor.test(Pain_Tolerance_P, Tears_P, method = "pearson", alternative = "two.sided", conf.level = .95) ### written in full

cor.test(Pain_Tolerance_S, Heat_rating_S, method = "spearman", exact = FALSE) ######## switch to spearman, 
####but wont give you the p value unless you state exact =FALSE (this is needed with tied ranks)
cor.test(Pain_Tolerance_K, Heat_rating_k, method = "kendall", exact = FALSE) ####switch to kendall, 
###but wont give you the p value unless you state exact =FALSE (this is needed with tied ranks)


###############################creating a correlation matrix####################################

############################### using a different data set
library(readxl)  #### dont need to do this repeatedly in each R session 
Matrixdata<- read_excel("C:/Users/prc/Desktop/Covid-19/BOOK/Correlation data/correlations_matrix.xlsx")
View(Matrixdata)
attach(Matrixdata)


################################

library(Hmisc) ###### package for the matrix 

matrix1 <-rcorr(as.matrix(MatrixData), type = "spearman") ###create a matrix of your Matrix data, spearmans
print(matrix1)  ###dsiplay the matrix


matrix1 <-rcorr(as.matrix(Matrixdata), type = "pearson") ###create a matrix of your Matrix data, Pearsons note this is default dont need to state pearsons
print(matrix1)                                            ###dsiplay the matrix


#######################################:-(cant do a Kendall matrix in Hmisc:-(

###########making a matrix with a subset of data 

set1<-c("Heat_rating", "Enjoyment", "weekly_cons") ###create a subset to matrix - name the subset
mynewmatrix<-Matrixdata[set1] ###take the subset from your data set and give it a name



matrix2<-rcorr(as.matrix(mynewmatrix)) ### produce a matrix 
print(matrix2)                         ###dsiplay the matrix


Data1819<- read.csv(file.choose(), header = T)
Data2020<- read.csv(file.choose(), header = T)

Matrix201819 <- Data1819[-(8)]
Matrix2020 <- Data2020[-(6)]


Matrix2018 <- A2018[c(21:34)]
Matrix2019 <- A2019[c(21:34)]


MatrixData <- read.csv(file.choose(), header = T)

library(Hmisc)
Matrix2<-rcorr(as.matrix(Matrix201819), type = "pearson")
print(Matrix2) 

Matrix3<-rcorr(as.matrix(Matrix2020), type = "pearson")
print(Matrix3) 


cor(A2018$YIELD, A2018$STR,  method = "pearson", use = "complete.obs")
cor(A20192$YIELD, A20192$STRI,  method = "pearson", use = "complete.obs")
cor(A2020$YIELD, A2020$STR,  method = "pearson", use = "complete.obs")

########################################Some graphs##########################

plot(Heat_rating, Enjoyment)            ####simple scatter
plot(Heat_rating, Enjoyment, xlab = "Heat Rating", ylab = "Enjoyment") ##### edit axis label
abline(lm(Heat_rating~Enjoyment))   ###best fit

library(car)
scatterplot(Heat_rating, Enjoyment, xlab = "Heat Rating") ###alt scatter plot


library(PerformanceAnalytics)
chart.Correlation(mynewmatrix)  ###correlations, histogram and scatterplots in one figure




## Plotting Weather Data for all locations. Check weather data summary. 

# 1. Precipitation 

library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)
RWD %>%
  ggplot( aes(x=Week, y=Precipitation, group=Location, color=Location)) +
  geom_line(aes(linetype=Location, color=Location))+
  geom_line(size=1)+
  geom_point(aes(color=Location))+
  guides(shape = guide_legend(override.aes = list(size=2)),
         color = guide_legend(override.aes = list(size=1))) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Rainfall Distribution") +
  theme_ipsum() +
  ylab("Total Precipitation [mm]") +
  xlab("Weeks After Planting")

# 2. Rainfall

library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)
RWD %>%
  ggplot( aes(x=Week, y=Temperature, group=Location, color=Location)) +
  geom_line(aes(linetype=Location, color=Location))+
  geom_line(size=1)+
  geom_point(aes(color=Location))+
  guides(shape = guide_legend(override.aes = list(size=2)),
         color = guide_legend(override.aes = list(size=1))) +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Average Growing Season Temperature") +
  theme_ipsum() +
  ylab("Average Temperature [?C]") +
  xlab("Weeks After Planting")



# Plotting farm locations on the map to show distribution

map <- read.csv(file.choose(), header = TRUE, na = "")
map <- read.csv("Cordinates_ALL.csv", header = TRUE)

Year <- as.factor(map$Year)
Farmer <- as.factor(map$Farmer)
State <- as.factor(map$State)
Site <- as.factor(map$Site)
Lat <- as.numeric(map$Lat)
Long <- as.numeric(map$Long)

install.packages(c("cowplot", "lubridate", "googleway", "ggplot2", "ggrepel", "dplyr", "tidyverse", "leaflet",
                   "ggspatial", "mapview", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))


library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)

library(tidyverse)
library(sf)
library(leaflet)
library(mapview)
mapview(map, xcol = "Long", ycol = "Lat",  crs = 4269, grid = TRUE)

library(sf)
sbux_sf <- st_as_sf(map, coords = c("Long", "Lat"),  crs=4326)
mapview(sbux_sf)%>%
  addStaticLabels(label = sbux_sf$Site,
                  noHide = TRUE,
                  direction = 'top',
                  textOnly = TRUE,
                  textsize = "20px")

install.packages("ggplot2")
install.packages("ggmap")
install.packages("maps")
install.packages("mapproj")
install.packages("mapdata")
install.packages("rgeos")
install.packages("maptools")
install.packages("sp")
install.packages("raster")
install.packages("rgdal")
install.packages("dismo")

require(ggplot2)
require(ggmap)
require(maps)
require(mapproj)
require(mapdata)
require(rgeos)
require(maptools)
require(sp)
require(raster)
require(rgdal)
require(dismo)
library(ggrepel)

Trial_map <- get_stamenmap(
  bbox = c(left = -91.5, bottom = 38.5, right = -83.5, top = 44), 
  maptype = "terrain-background",color = c("bw"),force=TRUE,
  zoom = 7)

ggmap(Trial_map)

ggmap(Trial_map) +
  geom_point(data=map, aes(Long, Lat, colour = factor(Year), fill = factor(Year), shape=factor(Year)), size = 3) +
  scale_shape_manual(values=c(21, 23, 22)) +
  theme(legend.position="bottom") +
  geom_text(data = map, aes(Long, Lat, label = Site, group = NULL), size = 3,cex=2, hjust=-0.1,angle=0)


###############################SUBSAMPLE ANALYSIS - STRIP TRIAL ANALYSIS WITH SUBSAMPLES AS REPS #### COMBINED ANALYSIS#######################
ALL<- read.csv(file.choose(), header = T)

ALL$YIELD<- ALL$YIELD*0.0673

ALL$EXP <- as.factor(ALL$EXP)
ALL$YEAR<- as.factor(ALL$YEAR)
ALL$CODE <- as.factor(ALL$CODE)
ALL$REP<- as.factor(ALL$REP)
ALL$FAR<- as.factor(ALL$FAR)
ALL$STATE<- as.factor(ALL$STATE)
ALL$PROG<- as.factor(ALL$PROG)
ALL$PHT <- as.numeric(ALL$PHT)
ALL$EHT <- as.numeric(ALL$EHT)
ALL$GWT <- as.numeric(ALL$GWT)
ALL$MAH <- as.numeric(ALL$MAH)
ALL$TWT <- as.numeric(ALL$TWT)
ALL$YIELD <- as.numeric(ALL$YIELD)


#Histograms to see the data distribution
par(mfrow=c(1,3),cex=0.85)
hist(ALL$YIELD)
hist(ALL$PHT)
hist(ALL$EHT)

##############Box Plot########
library(nlme)
library(agricolae)
boxplot(YIELD ~ YEAR, xlab= "", ylab= "Grain Yield [Ton/ha", las=2, lwd=1,cex.axis=0.75, col = c("#FFA726"),data=ALL)

##############Nested Anova, Use / to show nexting of variables Group and Hybrid  ##COMBINED DATA SET
Model <- aov(formula = YIELD ~ YEAR + CODE + FAR +  YEAR:CODE + YEAR:FAR + CODE:FAR + YEAR:CODE:FAR, data = ALL)
anova(Model)
out1 <- LSD.test(Model,alpha = 0.05, "YEAR")
out <- LSD.test(Model,alpha = 0.05, "CODE")
out3 <- LSD.test(Model,alpha = 0.05, "FAR")

par(mfrow=c(1,2),cex=0.85)
bar.err(out1$means,variation = "SE", density=30, angle=11, xlab="", ylab="Grain Yield [tons/ha]",las=2, ylim=c(0,15), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
boxplot(YIELD ~ CODE, xlab= "", ylab= "Grain Yield [Ton/ha", las=2, lwd=1,cex.axis=0.75, col = c("#FFA726"),data=Data18)





########2019 WITH 2020, AND 2018 SEPARATE. #################################

Data2018 <- read.csv(file.choose(), header = T)
Data18 <- Data2018
Otherscompiled <- Data18

Data18$YIELD<- Data18$YIELD*0.0673
Data18$YEAR<- as.factor(Data18$YEAR)
Data18$CODE <- as.factor(Data18$CODE)
Data18$REP<- as.factor(Data18$REP)
Data18$FAR<- as.factor(Data18$FAR)
Data18$STATE<- as.factor(Data18$STATE)
Data18$PROG<- as.factor(Data18$PROG)
Data18$PHT <- as.numeric(Data18$PHT)
Data18$EHT <- as.numeric(Data18$EHT)
Data18$GWT <- as.numeric(Data18$GWT)
Data18$MAH <- as.numeric(Data18$MAH)
Data18$TWT <- as.numeric(Data18$TWT)
Data18$YIELD <- as.numeric(Data18$YIELD)


Data1920<- read.csv(file.choose(), header = T)
Data201920 <- Data1920
Others1920 <- Data1920

Data1920$YIELD<- Data1920$YIELD*0.0673
Data1920$YEAR<- as.factor(Data1920$YEAR)
Data1920$CODE <- as.factor(Data1920$CODE)
Data1920$REP<- as.factor(Data1920$REP)
Data1920$FAR<- as.factor(Data1920$FAR)
Data1920$STATE<- as.factor(Data1920$STATE)
Data1920$PROG<- as.factor(Data1920$PROG)
Data1920$PHT <- as.numeric(Data1920$PHT)
Data1920$EHT <- as.numeric(Data1920$EHT)
Data1920$GWT <- as.numeric(Data1920$GWT)
Data1920$MAH <- as.numeric(Data1920$MAH)
Data1920$TWT <- as.numeric(Data1920$TWT)
Data1920$YIELD <- as.numeric(Data1920$YIELD)


####OUTLIERS OUTLIERS ##############################
boxplot(x$PHT, plot=TRUE)$out
outliers <- boxplot(Data1920$EHT,Data1920$PHT,Data1920$SDL,Data1920$SDS, plot=TRUE)$out  # save the outliers in a vector
x<-Data1920
x<- x[-which(c(x$PHT)%in% outliers),]
Otherscompiled <- Others18


#################ANOVA 2018
Model1 <- aov(formula = YIELD ~ CODE + FCODE + CODE:FCODE, data = Others18)
anova(Model1)

library(agricolae)

out1 <- LSD.test(Model1,alpha = 0.05, "CODE")
out5 <- LSD.test(Model1,alpha = 0.05, p.adj="bonferroni", "FCODE")

par(mfrow=c(1,2),cex=0.85)
bar.err(out1$means,variation = "SE", density=30, angle=11, xlab="", ylab="Grain Yield [tons/ha]",las=2, ylim=c(0,15), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out5$means,variation = "SE", density=30, angle=11, xlab="", ylab="Grain Yield [tons/ha]",las=2, ylim=c(0,15), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))

boxplot(YIELD ~ CODE, xlab= "", ylab= "Grain Yield [Ton/ha", las=2, lwd=1,cex.axis=0.75, col = c("#FFA726"),data=Data18)

####Reordering farmer variable in increasing order of yield for interaction plots

##FAR18  = factor(Data18$FAR, levels=c("Gray", "Wilken","Erisman","Gruver",  "Ambriole", "Buxton")) #Order of farmer performance
FAR18  = factor(Data18$FCODE, levels=c("E", "G","F","C",  "A", "B"))  #with farmer codes.
interaction.plot(FAR18, Data18$CODE, Data18$YIELD, #wont work if there NAs in the data set.
                 fun = mean,
                 xlab = "Farmers in the 2018 VTN", ylab = "Grain Yield [Ton/acre]", 
                 main = "Farmer and Hybrid Interaction", 
                 ylim = c(1,15), trace.label = "Hybrid", type = "b", 
                 col=c("red","blue","#B2182B","green","black","#009999", "#0000FF","#2166AC","orange"),
                 pch = c(19,17), cex.lab=1, cex.axis=1, cex.main=1, lwd =2, las = 1, fixed = TRUE)



##############SUBSAMPLE ANALYSIS - ANOVA 2019 AND 2020 ############
Model2 <- aov(formula = SDS ~ YR + CODE + FAR +  YR:CODE + YR:FAR + CODE:FAR + YR:CODE:FAR, data =  Data1920)
anova(Model2)

modelyield1 <- lm(YIELD ~ YEAR + CODE + FAR + YEAR/CODE/FAR, data = Data1920)
modelyield <- lm(YIELD ~  YEAR + CODE + FAR + YEAR/CODE/FAR, data = Otherscompiled)
anova(modelyield1)
anova(modelyield)

library(agricolae)
out2 <- LSD.test(Model2,alpha = 0.05, "CODE")
out3 <- LSD.test(Model2,alpha = 0.05, "YEAR")
out4 <- LSD.test(Model2,alpha = 0.05,p.adj="bonferroni", "FAR")

par(mfrow=c(1,2),cex=0.85)
bar.err(out2$means,variation = "SE", density=30, angle=11, xlab="", ylab="Grain Yield [tons/ha]",las=2, ylim=c(0,15), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out3$means,variation = "SE", density=30, angle=11, xlab="", ylab="Grain Yield [tons/ha]",las=2, ylim=c(0,15), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out4$means,variation = "SE", density=30, angle=11, xlab="", ylab="Grain Yield [tons/ha]",las=2, ylim=c(0,15), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))

interaction.plot(Data1920$YEAR, Data1920$CODE,Data1920$YIELD, col = cbind("red","blue","#B2182B","green","#D1E5F0","#009999", "#0000FF","#2166AC","orange"))
interaction.plot(Data1920$FAR, Data1920$CODE,Data1920$YIELD, col = cbind("red","blue","#B2182B","green","#D1E5F0","#009999", "#0000FF","#2166AC","orange"))

#FAR1920  = factor(Data1920$FCODE, levels=c("Doonan", "Ambriole", "Longwell", "Wilken", "Smith", "Steiner", "Gruver", "Bryan" ))
FAR1920  = factor(Data1920$FCODE, levels=c("H", "A", "I", "G", "J", "K", "F", "L"))

interaction.plot(FAR1920, Data1920$CODE, Data1920$YIELD, 
                 xlab = "Farmers in the 2019 and 2020 VTN", ylab = "Grain Yield [Ton/acre]", 
                 main = "Farmer and Hybrid Interation", 
                 ylim = c(1,15), trace.label = "Hybrid", type = "b", 
                 col=c("red","blue","#B2182B","green","black","#009999", "#0000FF","#2166AC","orange"),
                 pch = c(19,17), cex.lab=1, cex.axis=1, cex.main=1, lwd =2,las=1, fixed = TRUE)




####################Combined analysis with all years and all farmers##################################

modelyield1 <- lm(YIELD ~ YEAR + CODE + FAR + YEAR*CODE*FAR, data = ALL)
anova(modelyield1)



library(agricolae)
out2 <- HSD.test(Model2,alpha = 0.05, "CODE")
out3 <- HSD.test(Model2,alpha = 0.05, "YEAR")
out4 <- SD.test(Model2,alpha = 0.05,p.adj="bonferroni", "FAR")

par(mfrow=c(1,2),cex=0.85)
bar.err(out2$means,variation = "SE", density=30, angle=11, xlab="", ylab="Grain Yield [tons/ha]",las=2, ylim=c(0,15), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out3$means,variation = "SE", density=30, angle=11, xlab="", ylab="Grain Yield [tons/ha]",las=2, ylim=c(0,15), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out4$means,variation = "SE", density=30, angle=11, xlab="", ylab="Grain Yield [tons/ha]",las=2, ylim=c(0,15), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))



###USING LINEAR MIXED MODELS#######USING LINEAR MIXED MODELS#######USING LINEAR MIXED MODELS#####

#Considering farmers and hybrids as fixed and year as random. 

library(lme4)
library(lmerTest)
library(afex)

#Model for individual year analysis. since we have different farmers and hybrids in each year. 
model.lmer1 = lmer(YIELD ~ FAR + CODE + CODE:FAR, data=Others18)
anova(model.lmer1, type = 3, test.statistic="F")
summary(model.lmer1) 


## combined analysis for the 3 years 
model.lmer2 = lmer(SDL ~ (1|YR) + FAR + CODE + FAR:CODE +(1|YR:CODE)+ (1|YR:FAR) +(1|YR:CODE:FAR),REML = FALSE, data= Data1920)
anova(model.lmer2,type = 3, test.statistic="F" )
summary(model.lmer2) 


library(lme4)
library(lmerTest)
library(afex)
model <- lmer(y~(1 | CODE) + (1 | YEAR) + NSRC + WRT+ CCT + RTNL + Density, DF)
anova(model.lmer1, type = 3, test.statistic="F")
summary(model.lmer1) 

###test for normality of residues

qqnorm(residuals(mod1), pch = 1, frame = FALSE)
qqline(residuals(mod1), col = "steelblue", lwd = 2)
qqPlot(residuals(mod1))
hist(residuals(mod1))
boxplot(residuals(mod1))
shapiro.test((residuals(mod1)))



#Normality and homosquedasiicity of yield  differences###. In order to use Approach one: Normally distributed differences, (K. M. Eskridge and R. E Mumm, 1991),
#If d i is normally distributed over the population of envi- ronments with mean #di and standard deviation adi then the reliability
#(Eq. (1)) of the ith test cultivar may be stated as: p (z > -di/SDdi) 

Model1 <- lm(YIELD ~ FAR + CODE, data = ALL)  ## For each year. 
Model2 <- lm(Yd~ FARM + CODE, data = YD2019) 
Model3 <- lm(Yd ~ FARM + CODE, data = YD2020) 

library(car)
shapiro.test(residuals(Model1))   #If the Sig. value of the Shapiro-Wilk Test is greater than 0.05, the data is normal.
shapiro.test(residuals(Model2)) #If it is below 0.05, the data significantly deviate from a normal distribution.
shapiro.test(residuals(Model3))
hist(YD2018$Yd)
hist(YD2019$Yd)
hist(YD2020$Yd)


#2  Breusch-Pagan Test For Homoscedasticity # studentized Breusch-Pagan test
library(quantmod)
library(lmtest)
bptest(Model1)  # If p < 0.05, reject the null hypothesis and conclude that heteroscedasticity is present.
bptest(Model2) 
bptest(Model3) 



### Analysis of variance and mean separations of hybrids per year of testing
Model <- lm(YIELD ~ YEAR + FARM + CODE, data = ALL)   #For combined data set

Model4 <- lm(YIELD ~ FARM + CODE, data = ALL2018)  ## For each year. 
Model5 <- lm(YIELD ~ FARM + CODE, data = ALL2019) 
Model6 <- lm(YIELD ~ FARM + CODE, data = ALL2020)
Anova(Model6)


library(agricolae)
out1 <- LSD.test(Model1,alpha = 0.05, "CODE")
out2 <- LSD.test(Model5,alpha = 0.05, "CODE")
out3 <- LSD.test(Model6,alpha = 0.05, "CODE")

