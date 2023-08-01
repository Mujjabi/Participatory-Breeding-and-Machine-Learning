# STRIP TRIAL DATA ANALYSIS


### Check current R version and update if neccessary 
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
ALL$TWT<- ALL$YIELD*1.25

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


## Data Cleaning and Outlier Removal 

boxplot(x$EHT, plot=TRUE)$out
outliers <- boxplot(ALL$PHT, plot=TRUE)$out  # save the outliers in a vector
x<-ALL
x<- x[-which(c(x$EHT)%in% outliers),]
ALL <-  x


## TAKING A SUBSET OF THE DATA
library(tidyverse)
library(dplyr)
ALL2020 <- ALL %>% filter(YEAR=="2020")




## Estimating Blups for each Traut 

DataOutput<- data.frame(matrix(vector(),13,1, dimnames=list(c(), c("CODE"))))

#fill empty dataframe with 1-300 so that the cbind will work later on
DataOutput$CODE <- unique(ALL[,7]) #fill in Entry numbers
#DataOutput$FAR <- unique(ALL[,3])
#DataOutput$YEAR <- unique(ALL[,2])
#DataOutput$Day <- c(9) #fill in Day of data collection

#this empty dataframe is for variance components
DataVarComp <- data.frame()
DataVarCompOutput <- data.frame()
#HeritabilityData <- data.frame()

#this empty dataframe is for dropped variance components
drops <- c("var1","var2","sdcor") 

colnames(ALL)
str(ALL[,9:ncol(ALL)])
#take only the columns with numerical data
colnum=c(4:ncol(ALL))
i = 1 
for(i in 1:13){ #this second loop runs through each TRAIT, one at a time
  x=colnum[i] #set the current [i] column as x
  trait=colnames(ALL)[x] #sets the current column header as the trait name
  DF <- ALL #make a copy that we can use for our analysis
  colnames(DF)[x]="y"  #renames the trait variable as "y" for the model analysis below
  
  #We are interested in random effects, which estimates the proportion of variation and not fixed effects. 
  #Knowing variability components allows us to calculate Heritability.
  #Random-effects terms are distinguished by vertical bars or pipes (|) 
  #library(lme4) #our random effects mixed model
  #model <- lmer(y~(1 | CODE) + (1 | YEAR) + NSRC + WRT+ CCT + RTNL + Density, DF)
  #summary(model)
  #anova(model)
  
  
  library(lme4) 
  model.lmer2 = lmer(y ~ (1|YEAR) +(1| FAR) + (1|CODE) + (1|FAR:CODE) +(1|YEAR:CODE)+ (1|YEAR:FAR) +(1|YEAR:CODE:FAR),REML = FALSE, data= DF)
  anova(model.lmer2,type = 3, test.statistic="F" )
  summary(model.lmer2) 
  
  
  
  #qqnorm(residuals(mod1), pch = 1, frame = FALSE)
  #qqline(residuals(mod1), col = "steelblue", lwd = 2)
  #qqPlot(residuals(mod1))
  #hist(residuals(mod1))
  #boxplot(residuals(mod1))
  #shapiro.test((residuals(mod1)))
  
  
  varComp<-as.data.frame(VarCorr(model.lmer2,comp="vcov")) #function calculates estimated variances between random-effects terms in a mixed-effects model  blup = coef(model)$Entry
  blup = coef(model.lmer2)$CODE #coef extracts model coefficients from lmer objects returned by modeling functions
  hist(blup[,1]) #plot it out
  colnames(blup) <- trait #rename the BLUP column by the trait in our loop
  #add columns to existing dataframe  
  DataOutput <- cbind(DataOutput,blup) #ammends our dataframe with the new BLUP column for the new trait
  
  #Modify variance component df by
  #deleting columns in variance component dataframe (we don't need it)
  varComp<-varComp[ , !(names(varComp) %in% drops)]
  #set the trait name from the loop
  varComp$Trait<-trait
  #add columns to existing dataframe
  DataVarComp <- rbind(DataVarComp,varComp) 
}

#reshape our variance components dataframe so that we can run the heritability script
DataVarCompOutput <- reshape(DataVarComp, idvar = "Trait", timevar = "grp", direction = "wide")

#the broad sense heritability script
#Taken from Gioia et al. 2017
nloc = 20  #Number of locations the hybrids were tested. 7 in 2018, 6 in 2019 and 7 in 2020
HeritabilityData <- ((DataVarCompOutput[,2])) / (((DataVarCompOutput[,2])) + (((DataVarCompOutput[,4])) / (nloc)))
##H2 is genetic variance divide by the sum of genetic variance and error variance divided by the number of observations/locations

#create function to identify maximum value in a column
colMax <- function(data) sapply(data, max, na.rm = TRUE)
#create function to identify minimum value in a column
colMin <- function(data) sapply(data, min, na.rm = TRUE)
#create function to identify mean value in a column
colMean <- function(data) sapply(data, mean, na.rm = TRUE)
#create function to identify median value in a column
colMedian <- function(data) sapply(data, median, na.rm = TRUE)
#create function to identify standard dev value in a column
colStdev <- function(data) sapply(data, sd, na.rm = TRUE)


#summary statistics
DataColMax <- colMax(DF[,colnum])
DataColMin <- colMin(DF[,colnum])
DataColMean <- colMean(DF[,colnum])
DataColMedian <- colMean(DF[,colnum])
DataColStdev <- colStdev(DF[,colnum])
DataColSE<-std.error(DF[,colnum])

CVg <- (sqrt(DataVarCompOutput[,2])) / ((DataColMean))  #Coefficience of variance, genetic variance divided by mean
#CVp <- will be calculated as the sqrt of the sum of error and genetic variance divided by the mean

DataVarCompOutput <- cbind(DataVarCompOutput,HeritabilityData,CVg,DataColMin,DataColMax,DataColMean,DataColMedian,DataColStdev,DataColSE)
DataVarCompOutput[1:14,]

DataVarCompOutput %>% filter(Trait == "TRL") %>% select(HeritabilityData) 

install.packages("xlsx")
library(xlsx)
write.xlsx(DataOutput, "BLUP5.xlsx")
write.xlsx(DataVarCompOutput, "H5.xlsx")


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


#Another way of directly comparing the two models is with the analysis of deviance,
#which can be performed with the function anova:
#This test compares the residual deviance of the two models to see whether they are difference and 
#calculates a p-values. In this case the p-value is highly significant, meaning that the models are different. 
#Since we already compared the AIC, we can conclude that pois.mod2 is significantly (low p-value) better (lower AIC) than mod1.
DF <- OREI2019
library(lme4)
mod1 <- lmer(YIELD~ (1|CODE), ALL)
mod2 <- lmer(YIELD~ (1|CODE) + (1|FARM), ALL)
mod3 <- lmer(YIELD~ (1|CODE) + (1|FARM) + (1|CODE:FARM), ALL) #WE DONT HAVE ENOUGH DF

mod4 <- lmer(YIELD~(1|CODE) + (1|CCT), ALL)
mod5 <- lmer(YIELD~(1|CODE) + (1|CCT) + (1|CCT:CODE), ALL)# Mod4 is better
mod6 <- lmer(YIELD~(1|CODE) + (1|CCT) + (1|NSRC), ALL) #BETTER THAT MOD4
mod7 <- lmer(YIELD~(1|CODE) + (1|CCT) + (1|NSRC) + (1|WRT), ALL) #BETTER THAN 6

mod8 <- lmer(YIELD~(1|CODE) + (1|CCT) + (1|NSRC) + (1|PDTY),ALL) #NOT BETTER THAN 7 OR 6, PDTY NOT SIGNIFICANT

mod9 <- lmer(YIELD~(1|CODE) + (1|CCT) + (1|NSRC) + (1|WRT) + (1|RTN),ALL)#BETTER THAN 7, THIS IS GOOD FOR COMBINED ANALYSIS


summary(mod9)
anova(mod7,mod9,test="Chisq")

install.packages("AICcmodavg")
library(AICcmodavg)
models <- list(mod1, mod2, mod3,mod4, mod5,mod6,mod7,mod8,mod9)
model.names <- c('mod1', 'mod2', 'mod3','mod4', 'mod5','mod6','mod7','mod8','mod9')
aictab(cand.set = models, modnames = model.names)


###############Pairwise comparisons ###########
AOV <- aov(YIELD ~ CODE,data =OREI2020)

require(graphics)
TukeyHSD(AOV, "CODE", ordered = TRUE)
plot(TukeyHSD(AOV, "CODE"))


mod1 <- lm(YIELD ~ CODE + CCT + NSRC + WRT, data = OREI2020)
mod1 <- lm(YIELD~ YEAR + CODE + NSRC + CCT + WRT + RTN, ALL)
mod.av <- aov(mod1)
summary(mod.av)
tukey.test2 <- TukeyHSD(mod.av, trt = 'CODE')
par(mfrow=c(2,2))
plot(tukey.test2,las=2,cex=0.25,col="black")

library(agricolae)
tukey.test3 <- HSD.test(mod.av, trt = 'CODE')
tukey.test3

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


##colineality diagonistic
library(olsrr)
ols_eigen_cindex(modelyield)
ols_vif_tol(modelyield)
ols_plot_resid_fit_spread(modelyield)

modelyield <- lm(YIELD ~ CODE + WRT + CCT + RTNL + Density, ALL2020) # Explore cover crop and manure effect 
anova(modelyield)
anova(modelpht)
anova(modeleht)
anova(modelsdl)
anova(modelsds)
anova(modelmst)
anova(modeltwt)
anova(modelkwt)
anova(modelprot)
anova(modelstr)
anova(modeloil)

#mod1<-lm(YIELD ~ CODE,data = ALL)
#out2 <- LSD.test(mod1,"CCT", p.adj="bonferroni"), I didnt find significant differences with adjusted p values

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

##out<-duncan.test(modelyield,"CODE",group = T, console = T)  #we can also use duncan test but I need to investigate which test is more apropriate

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
#bar.err(outwrt$means,variation = "SE", xlab="", ylab="Grain yield [ton/ha]", ylim=c(0,12), las=2, col = "grey44")
#bar.err(outrtn$means,variation = "SE", xlab="", ylab="Grain yield [ton/ha]", ylim=c(0,12), las=2, col = "grey44")
#bar.err(outdensity$means,variation = "SE",  xlab="", ylab="Grain yield [ton/ha]", ylim=c(0,12),las=2,  col = "grey44")
###plots with mean separatios###
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
































##### ANALYSIS FOR FARMER REPORTS 2020  #################

mod1<-lm(PROT ~ CODE + CCT + NSRC + WRT + RTN, data = ALL)


out1 <- LSD.test(model8,"CODE") 
out2 <- LSD.test(model8,"CCT")
out3 <- LSD.test(model8,"NSRC")
out4 <- LSD.test(model8,"WRT")
out5<- LSD.test(model8,"RTN")

par(mfrow=c(1,2),cex=1)
bar.err(out1$means,variation = "SE", density=30, angle=11, xlab="", ylab="Yield [Bu/Acre]",las=2, ylim=c(0,200), col = c("red", "green", "blue", "orange","#2AEBD7","#732AEB", "#EBE22A"))
bar.err(out2$means,variation = "SE", density=30,las=2, angle=11, xlab="", ylab="Yield [Bu/Acre]", ylim=c(0,200),col = "#317")
bar.err(out3$means,variation = "SE", density=30, las=2,angle=11, xlab="", ylab="Yield [Bu/Acre]", ylim=c(0,170), col = c("red", "blue", "green","#2AEBD7", "#732AEB", "#EBE22A","#EB2A76"))
#bar.err(out4$means,variation = "SE", density=30, angle=11, xlab="Weed Pressure", ylab="Yield [Bu/Acre]", ylim=c(0,200), col = c("red", "blue","green"))
#bar.err(out5$means,variation = "SE",  density=30, angle=11,xlab="Rotation Length", ylab="Yield [Bu/Acre]",las=1, ylim=c(0,200), col = c("red", "blue","green","orange"))



##########Normality################################################Normality############
#par(mfrow=c(2,2),cex=1)
qqnorm(residuals(modelyield), pch = 1, frame = FALSE)
qqline(residuals(modelyield), col = "steelblue", lwd = 2)
qqPlot(residuals(modelyield))
hist(residuals(modelyield))
boxplot(residuals(modelyield))
shapiro.test((residuals(mod1)))


library(agricolae)
library(car)
leveneTest(modelyield,  center=mean, data=ALL)


kruskal.test(YIELD ~ PDTY, data = ALL)
library(car)
library(MASS)
boxcox(model8)

bc <- boxcox(model8)
lambda <- bc$x[which.max(bc$y)]
lambda=2
new_model <- lm(((YIELD^lambda-1)/lambda) ~ CODE + NSRC + WRT+CCT+RTN+PDTY, data = ALL)

mod<- lm(((YIELD^lambda-1)/lambda)~ CODE+NSRC+WRT+CCT+RTN+PDTY, data=ALL)
anova(mod)



leveneTest(((YIELD^lambda-1)/lambda)~ CODE*NSRC*WRT*CCT*RTN*PDTY,  center=mean, data=ALL)

library(car)
leveneTest(newvariable,newdata$FAR+newdata$Hybrid) 
Mod3 <- manova(cbind(YLD,PHT,EHT,SDLf,RTA,FDTop) ~ MTRT, data = newdata)
summary.aov(Mod3)

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

#Testing the Multicollinearity Assumption
#Correlation Matrix
# creating subset of continuous independent variables from the dataframe


expVar <- ALL[c("PHT", "EHT","PROT", "OIL","STR" ,"TWT")]
corMatrix <- round(cor(expVar), 2)
corMatrix

library(caret)
findCorrelation(corMatrix, cutoff = 0.7, names = TRUE)




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
  #scale_fill_brewer(palette="Dark2")
  
  
  ggplot(ALL, aes(x=WRT, y=PROT, fill=WRT)) +
  geom_boxplot() 

ggplot(ALL, aes(x=CCT, y=PROT, fill=CCT)) +
  geom_boxplot() 

ggplot(ALL, aes(x=RTN, y=PROT, fill=RTN)) +
  geom_boxplot() 

######BOX PLOTS WITHOUT GGPLOT

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
#using the generated blups 


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








########## EXTRACTING EXCEL FILE #################################EXTRACTING EXCEL FILE ########

# Statistical procedures for agricultural research, pag 143
# Grain Yields of Three Rice Varieties Grown under
#Three Management practices and Five Nitrogen levels; in a
#split-split-plot design with nitrogen as main-plot,
#management practice as subplot, and variety as sub-subplot
#factores, with three replications.
library(agricolae)
f <- system.file("external/ssp.csv", package="agricolae")
ssp<-read.csv(f)
model<-with(ssp,ssp.plot(block,nitrogen,management,variety,yield))
gla<-model$gl.a; glb<-model$gl.b; glc<-model$gl.c
Ea<-model$Ea; Eb<-model$Eb; Ec<-model$Ec
op<-par(mfrow=c(1,3),cex=0.6)
out1<-with(ssp,LSD.test(yield,nitrogen,gla,Ea,console=TRUE))
out2<-with(ssp,LSD.test(yield,management,glb,Eb,console=TRUE))
out3<-with(ssp,LSD.test(yield,variety,glc,Ec,console=TRUE))
plot(out1,xlab="Nitrogen",las=1,variation="IQR")
plot(out2,xlab="Management",variation="IQR")
plot(out3,xlab="Variety",variation="IQR")
# with aov
AOV<-aov(yield ~ block + nitrogen*management*variety + Error(block/nitrogen/management),data=ssp)
summary(AOV)
par

################# MACHINE LEARNING ALGOLITHYMS ################ MACHINE LEARNING ALGOLITHYMS ##########

data <- read.csv(file.choose(), header = TRUE, na = "")
ALL <- data

install.packages("ClustOfVar", dependencies = TRUE)
install.packages("cluster", dependencies = TRUE)

library(ClustOfVar)

names(ALL)

xquant <- ALL[,c(21:34)] # Numeric variables
xqual <- ALL[,c(10:17)]      # Categorical variables


tree <- hclustvar(xquant, xqual)
plot(tree)


k.means <- kmeansvar(xquant, xqual, init=4)
summary(k.means)

library(cluster)

d <- daisy(ALL, metric="gower")


#####################WEATHER DATA FOR REPLICATED TRIAL ################

Means <- read.csv(file.choose(), header = TRUE, na = "")

Temperature <- as.numeric(WD$Temperature)
Precipitation <- as.numeric(WD$Precipitation)
week <- as.factor(WD$Week)
Location <- as.factor(WD$Location)


#library(dplyr)
#RWD <- d %>%
#group_by(Location, Week) %>% 
#summarise_at(vars("Temperature","Precipitation" ), mean) 


library(dplyr)
RWD <- WD %>%
  group_by(Location, Week) %>% 
  summarise(Temperature= mean(Temperature), Precipitation=sum(Precipitation))

install.packages("xlsx")
library(xlsx)
write.xlsx(RWD, "RWD.xlsx")


write.csv (map, "Strip_Cordinates.csv")

##########PLOT RAINFAL##############PLOT RAINFAL##############PLOT RAINFAL##############PLOT RAINFAL###########
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

#####TEMPERATURE#########################################TEMPERATURE###########################
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



##################PLOTTING LOCATIONS ON THE MAP #########################################################
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

###########Replicated Trial Maps#########
map <- read.csv("Repli_Cordinates.csv", header = TRUE)

Year <- as.factor(map$Year)
Site <- as.factor(map$Site)
Lat <- as.numeric(map$Lat)
Long <- as.numeric(map$Long)

Trial_map <- get_stamenmap(
  bbox = c(left = -91.6, bottom = 38, right = -87, top = 42.094), 
  maptype = "terrain",color = c("bw"),force=TRUE,
  zoom = 7)

ggmap(Trial_map)

ggmap(Trial_map) +
  geom_point(data=map, aes(Long, Lat, colour = factor(Year), fill = factor(Year), shape=factor(Year)), size = 6) +
  scale_shape_manual(values=c(21, 23, 19)) +
  theme(legend.position="bottom") +
  geom_text(data = map, aes(Long, Lat, label = Site, group = NULL), size = 6,cex=3, hjust=-0.3,angle=0)

###############MEANS BY MULTIPLE GROUPS ############
Compiled <- read.csv(file.choose(), header = T)

Compiled$Set <- as.factor(Compiled$Set)
Compiled$Year <- as.factor(Compiled$Year)
Compiled$Nitro <- as.factor(Compiled$Nitro)
Compiled$Code <- as.factor(Compiled$Code)
Compiled$Weed <- as.factor(Compiled$Weed)
Compiled$EFFECT <- as.factor(Compiled$EFFECT)
Compiled$DTA <- as.numeric(Compiled$DTA)
Compiled$DTS <- as.numeric(Compiled$DTS)
Compiled$X.RTL <- as.numeric(Compiled$RTL)
Compiled$X.STL<- as.numeric(Compiled$STL)
Compiled$PHT <- as.numeric(Compiled$PHT)
Compiled$EHT <- as.numeric(Compiled$EHT)
Compiled$SDL <- as.numeric(Compiled$SDL)
Compiled$GWT <- as.numeric(Compiled$GWT)
Compiled$MST <- as.numeric(Compiled$MST)
Compiled$TWT <- as.numeric(Compiled$TWT)
Compiled$YIELD <- as.numeric(Compiled$YIELD)
Compiled$PROT <- as.numeric(Compiled$PROT)
Compiled$OIL <- as.numeric(Compiled$OIL)
Compiled$STR <- as.numeric(Compiled$STR)

library(dplyr)
Means <- Compiled %>%
  group_by(code, effect) %>%
  summarise(across(FDS:RTA, mean))
write.csv (Means, "RootEffects.csv")


#####Calculating plot means for root data, 2021

Compiled$Experiment <- as.factor(Compiled$Experiment)
Compiled$Pedigree <- as.factor(Compiled$Pedigree)
Compiled$Code <- as.factor(Compiled$Code)
Compiled$Source <- as.factor(Compiled$Source)
Compiled$Plot <- as.factor(Compiled$Plot)
Compiled$Rotation <- as.factor(Compiled$Rotation)
Compiled$Rep <- as.factor(Compiled$Rep)
Compiled$Nitro <- as.factor(Compiled$Nitro)
Compiled$PHT <- as.numeric(Compiled$PHT)
Compiled$EHT <- as.numeric(Compiled$EHT)
Compiled$SDL<- as.numeric(Compiled$SDL)
Compiled$SDS <- as.numeric(Compiled$SDS)
Compiled$Moisture <- as.numeric(Compiled$Moisture)
Compiled$Test.Weight <- as.numeric(Compiled$Test.Weight)
Compiled$Yield<- as.numeric(Compiled$Yield)
Compiled$FDTop <- as.numeric(Compiled$FDTop)
Compiled$FDS <- as.numeric(Compiled$FDS)
Compiled$RAA <- as.numeric(Compiled$RAA)
Compiled$SDA <- as.numeric(Compiled$SDA)



library(dplyr)
Means <- Compiled %>%
  group_by(Plot) %>%
  summarise(across(PHT:SDS, mean))
write.csv (Means, "Plotmeans.csv")


boxplot(Compiled$OIL ~ Compiled$Code , 
        col=c("red", "#2AEBD7", "#732AEB", "#EBE22A"),
        ylab="Starch Content [%]" , xlab="Breeding Program",las=2)






#########################CLUSTER ANALYSIS#################DENDROGRAMS###############
seeds_df <- ALL



str(seeds_df)
summary(seeds_df)
any(is.na(seeds_df))
seeds_label <- seeds_df$FARM
seeds_df$FARM <- NULL
str(seeds_df)
seeds_df_sc <- as.data.frame(scale(seeds_df[23:36]))
na.omit(seeds_df_sc)
summary(seeds_df_sc)

dist_mat <- dist(seeds_df_sc, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 6)
plot(hclust_avg)
rect.hclust(hclust_avg , k = 6, border = 2:6)
abline(h = 3, col = 'red')

suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)


######################ANCOVA ####################################ANCOVA ############ancova


library(car)
ancova_model <- aov(YLD~ CODE + DTA, data = ALL)
Anova(ancova_model, type="III")



###############################HIGH IMPACT HYBRID ANALYSIS #######################
ALL<- read.csv(file.choose(), header = T)

ALL$YLD<- ALL$YLD*0.0673

ALL$EXP <- as.factor(ALL$EXP)
ALL$CODE <- as.factor(ALL$CODE)
ALL$GROUP<- as.factor(ALL$GROUP)
ALL$DTA <- as.numeric(ALL$DTA)
ALL$DTS <- as.numeric(ALL$DTS)
ALL$RTLPCT <- as.numeric(ALL$RTLPCT)
ALL$STLPCT <- as.numeric(ALL$STLPCT)
ALL$PHT <- as.numeric(ALL$PHT)
ALL$EHT <- as.numeric(ALL$EHT)
ALL$GWT <- as.numeric(ALL$GWT)
ALL$MAH <- as.numeric(ALL$MAH)
ALL$TWT <- as.numeric(ALL$TWT)
ALL$YLD <- as.numeric(ALL$YLD)
ALL$PROT <- as.numeric(ALL$PROT)
ALL$OIL <- as.numeric(ALL$OIL)
ALL$MST <- as.numeric(ALL$MST)
ALL$STR <- as.numeric(ALL$STR)

str(ALL)
nlevels(ALL$GROUP)
unique(ALL$GROUP)
nlevels(ALL$CODE)

#Histograms to see the data distribution
par(mfrow=c(1,3),cex=0.85)
hist(ALL$YLD)
hist(ALL$PHT)
hist(ALL$EHT)
hist(ALL$TWT)
hist(ALL$MAH)
hist(ALL$DTA)
hist(ALL$DTS)
hist(ALL$STLPCT)
hist(ALL$RTLPCT)
hist(ALL$PROT)
hist(ALL$STR)
hist(ALL$OIL)
hist(ALL$MST)

##############Box Plot########
library(nlme)
library(agricolae)
boxplot(YLD ~ GROUP, xlab= "", ylab= "Grain Yield [Ton/ha", las=2, lwd=1,cex.axis=0.75, col = c("#FFA726"),data=ALL)
boxplot(PROT ~ GROUP, xlab= "", ylab= "Protein Content [%]",las=2, lwd=1,cex.axis=0.75,col = c("#FFA726"), data=ALL)
boxplot(STR ~ GROUP, xlab= "", ylab= "Starch Content [%]", las=2,lwd=1,cex.axis=0.75, col = c("#FFA726"), data=ALL)
boxplot(OIL ~ GROUP, xlab= "", ylab= "Oil Content [%]",las=2,lwd=1,cex.axis=0.75, col = c("#FFA726"), data=ALL)

#Nested Anova, Use / to show nexting of variables Group and Hybrid
aov(formula = YLD ~ CODE, data = ALL)

model1<-lm(YLD ~ CODE + Rep, data = ALL)
model2<-lm(PHT ~ GROUP , data = ALL)
model3<-lm(EHT ~ GROUP , data = ALL)
model4<-lm(TWT ~ GROUP , data = ALL)
model5<-lm(MST ~ GROUP , data = ALL)
model6<-lm(DTA ~ GROUP , data = ALL)
model7<-lm(DTS ~ GROUP , data = ALL)
model8<-lm(RTLPCT ~ GROUP , data = ALL)
model9<-lm(STLPCT ~ GROUP , data = ALL)
model10<-lm(PROT ~ GROUP , data = ALL)
model11<-lm(STR ~ GROUP , data = ALL)
model12<-lm(OIL~ GROUP , data = ALL)

Anova(model1)

out1 <- LSD.test(model1,alpha = 0.05, "CODE")
out2 <- LSD.test(model2,alpha = 0.05, "GROUP")
out3 <- LSD.test(model3,alpha = 0.05, "GROUP")
out4 <- LSD.test(model4,alpha = 0.05, "GROUP")
out5 <- LSD.test(model5,alpha = 0.05, "GROUP")
out6 <- LSD.test(model6,alpha = 0.05, "GROUP")
out7 <- LSD.test(model7,alpha = 0.05, "GROUP")
out8 <- LSD.test(model8,alpha = 0.05, "GROUP")
out9 <- LSD.test(model9,alpha = 0.05, "GROUP")
out10 <- LSD.test(model7,alpha = 0.05, "GROUP")
out11 <- LSD.test(model8,alpha = 0.05, "GROUP")
out12 <- LSD.test(model9,alpha = 0.05, "GROUP")


par(mfrow=c(1,2),cex=0.85)
bar.err(out1$means,variation = "SE", density=30, angle=11, xlab="", ylab="Grain Yield [tons/ha]",las=2, ylim=c(0,15), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out2$means,variation = "SE", density=30, angle=11, xlab="", ylab="Plant Height [cm]",las=2, ylim=c(0,300), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out3$means,variation = "SE", density=30, angle=11, xlab="", ylab="Ear Height [cm]",las=2, ylim=c(0,150), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out4$means,variation = "SE", density=30, angle=11, xlab="", ylab="Test Weight [lbs/bu]",las=2, ylim=c(0,80), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out5$means,variation = "SE", density=30, angle=11, xlab="", ylab="Moisture [%]",las=2, ylim=c(0,30), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out6$means,variation = "SE", density=30, angle=11, xlab="", ylab="Days to Anthesis",las=2, ylim=c(0,70), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out7$means,variation = "SE", density=30, angle=11, xlab="", ylab="Days to Silking",las=2, ylim=c(0,70), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out8$means,variation = "SE", density=30, angle=11, xlab="", ylab="Root Lodging [%]",las=2, ylim=c(0,20), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out9$means,variation = "SE", density=30, angle=11, xlab="", ylab="Stem Lodging [%]",las=2, ylim=c(0,5), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))

bar.err(out7$means,variation = "SE", density=30, angle=11, xlab="", ylab="Protein Content [%]",las=2, ylim=c(0,70), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out8$means,variation = "SE", density=30, angle=11, xlab="", ylab="Starch Content [%]",las=2, ylim=c(0,20), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))
bar.err(out9$means,variation = "SE", density=30, angle=11, xlab="", ylab="Oil Content [%]",las=2, ylim=c(0,5), col = c("red", "blue", "orange", "#2AEBD7","#732AEB", "#EBE22A", "#69b3a2", "turquoise","green", "yellow"))

###Mixed model
library(lme4)
model.lmer = lmer(YLD ~ Rep + (1|GROUP/CODE), data=ALL)
anova(model.lmer)
summary(model.lmer)




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

Model <- aov(formula = SDS ~ YEAR + CODE + FAR +  YEAR:CODE + YEAR:FAR + CODE:FAR + YEAR:CODE:FAR, data =  ALL)
anova(Model2)

modelyield1 <- lm(YIELD ~ YEAR + CODE + FAR + YEAR*CODE*FAR, data = ALL)
anova(modelyield1)

####OR### this gives the same results

Model3 <- aov(formula = SDS ~ YEAR + CODE + FAR + YEAR*CODE*FAR, data = ALL)
anova(Model3)


library(agricolae)
out2 <- LSD.test(Model2,alpha = 0.05, "CODE")
out3 <- LSD.test(Model2,alpha = 0.05, "YEAR")
out4 <- LSD.test(Model2,alpha = 0.05,p.adj="bonferroni", "FAR")

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



#######STANDARD ERROR CALCULATION#######STANDARD ERROR CALCULATION#######STANDARD ERROR CALCULATION###

Otherscompiled <- Data1920

install.packages("plotrix")                           
library("plotrix")   
std.error(Otherscompiled$PHT,na.rm = TRUE)
std.error(Otherscompiled$EHT,na.rm = TRUE)
std.error(Otherscompiled$SDL,na.rm = TRUE)
std.error(Otherscompiled$SDS,na.rm = TRUE)
std.error(Otherscompiled$YIELD,na.rm = TRUE)

mean(Otherscompiled$PHT, na.rm = TRUE)
mean(Otherscompiled$EHT, na.rm = TRUE)
mean(Otherscompiled$SDL, na.rm = TRUE)
mean(Otherscompiled$SDS, na.rm = TRUE)
mean(Otherscompiled$YIELD, na.rm = TRUE)


min(Otherscompiled$PHT, na.rm = TRUE)
max(Otherscompiled$PHT, na.rm = TRUE)
min(Otherscompiled$EHT, na.rm = TRUE)
max(Otherscompiled$EHT, na.rm = TRUE)
min(Otherscompiled$SDL, na.rm = TRUE)
max(Otherscompiled$SDL, na.rm = TRUE)
min(Otherscompiled$SDS, na.rm = TRUE)
max(Otherscompiled$SDS, na.rm = TRUE)
min(Otherscompiled$YIELD, na.rm = TRUE)
max(Otherscompiled$YIELD, na.rm = TRUE)


hist(Otherscompiled$SDS)
hist(Otherscompiled$SDL)
hist(Otherscompiled$PHT)
hist(Otherscompiled$EHT)
hist(Otherscompiled$YIELD)







###############ESTIMATING RELIABILITY AS THE PROBABILITY OF A HYBRID TO OUTPERFORM THE CHECK VARIERTY#####
###### ESTIMATED USING K.M ESKRIDGE AND R.F.MUMM, 1991. Choosing plant cultivars based on the probability of outperforming a check*##

### 1. nonparametric approach using Cobhran's Q test and 
####  2. Normally distributed difference 

ALL <- read.csv("SubSamples_Combined.csv", header = TRUE)
YD <-read.csv("DistributionYD.csv", header=TRUE)

ALL$EXP <- as.factor(ALL$EXP)
ALL$YEAR <- as.factor(ALL$YEAR)
ALL$FAR <- as.factor(ALL$FAR)
ALL$STATE <- as.factor(ALL$STATE)
ALL$HYB <- as.factor(ALL$HYB)
ALL$CODE <- as.factor(ALL$CODE)
ALL$PHT <- as.numeric(ALL$PHT)
ALL$EHT <- as.numeric(ALL$EHT)
ALL$SDL <- as.numeric(ALL$SDL)
ALL$SDS <- as.numeric(ALL$SDS)
ALL$GWT <- as.numeric(ALL$GWT)
ALL$MAH <- as.numeric(ALL$MAH)
ALL$TWT <- as.numeric(ALL$TWT)
ALL$THKWT <- as.numeric(ALL$THKWT)
ALL$KWT <- as.numeric(ALL$KWT)
ALL$YIELD <- as.numeric(ALL$YIELD)
ALL$PROT <- as.numeric(ALL$PROT)
ALL$OIL <- as.numeric(ALL$OIL)
ALL$STR <- as.numeric(ALL$STR)

YD$EXP <- as.factor(YD$EXP)
YD$YEAR <- as.factor(YD$YEAR)
YD$FARM <- as.factor(YD$FARM)
YD$STATE <- as.factor(YD$STATE)
YD$HYB <- as.factor(YD$HYB)
YD$CODE <- as.factor(YD$CODE)
YD$YIELD <- as.numeric(YD$YIELD)
YD$Yd <- as.numeric(YD$Yd)

#ALL$YIELD<- ALL$YIELD*0.0673  #change yield tons/ha
#ALL$TWT<- ALL$YIELD*1.25   #change test weight to kg/hl.


#Taking  subset of the data####
library(tidyverse)
library(dplyr)
ALL2020 <- ALL %>% filter(YEAR=="2020")
ALL2019 <- ALL %>% filter(YEAR=="2019")
ALL2018 <- ALL %>% filter(YEAR=="2018")

library(tidyverse)
library(dplyr)
YD2020 <- YD %>% filter(YEAR=="2020")
YD2019 <- YD %>% filter(YEAR=="2019")
YD2018 <- YD %>% filter(YEAR=="2018")

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



##### COnducting a dunnet test for pairwise comparison between check and test hybrids. 
##### 95% family-wise confidence level

model <- aov(value ~ Group, data = data)
summary(model)

library(DescTools)
DunnettTest(YIELD ~ CODE, data = ALL2018, control="CHECK", conf.level=0.95)
DunnettTest(YIELD ~ CODE, data = ALL2019, control="CHECK", conf.level=0.95)
DunnettTest(YIELD ~ CODE, data = ALL2020, control="CHECK", conf.level=0.95)


##Combined analsis for all location and years
library(DescTools)
DunnettTest(YIELD ~ CODE, data = ALL, control="CHECK", conf.level=0.95)

boxplot(YIELD ~ CODE, data = ALL)



############# Kitale grain quality ######

ALL <- read.csv("KF4_GQ.csv", header = TRUE)

ALL$Pedigree <- as.factor(ALL$Pedigree)
ALL$Family <- as.factor(ALL$Family)
ALL$DTS <- as.numeric(ALL$DTS)
ALL$PROT <- as.numeric(ALL$PROT)
ALL$STR <- as.numeric(ALL$STR)
ALL$OIL <- as.numeric(ALL$OIL)


boxplot(DTS ~ Family, data = ALL, las= 2, cex.axis=0.75)
boxplot(PROT ~ Family, data = ALL, las= 2, cex.axis=0.75)
boxplot(STR ~ Family, data = ALL, las= 2, cex.axis=0.75)
boxplot(OIL ~ Family, data = ALL,las= 2, cex.axis=0.75)



##################### CALCULATING WALD STATISTIC TEST FOR RELIABILITY VALUES #########

Wald <- read.csv(file.choose(), header = T)

Wald$Variety <- as.factor(Wald$Variety)

library(aod)

model <- lm(RN ~ Variety, data = Wald)


wald.test(Sigma = vcov(model), b = coef(model), Terms = 1)

fm <- quasibin(cbind(y, n - y) ~ seed * root, data = Wald)
# Wald test for the effect of root
wald.test(b = coef(fm), Sigma = vcov(fm), Terms = 3:4)




