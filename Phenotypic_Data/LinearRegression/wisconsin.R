

## Analysis for Wiscon Farmers
rm(list=ls(all=TRUE))
graphics.off()
shell("cls") 

WI_ALL <-  OREI_COMPLETE  %>%
  filter(STATE %in% c("WISCONSIN"))

## check Wisconsin 
boxplot(WI_ALL[17:27],las = 2, plot=TRUE)$out

library(ggplot2)
Traits2 <- c("TWT", "KWT300", "YIELD", "PRO", "STA", "OIL")  #these are the only measured traits. Others are estimated with randomforest
for (trait in Traits2) {
  Plot <- ggplot(WI_ALL, aes(x = .data[[trait]], fill = YEAR, color = YEAR)) +
    geom_histogram(position="identity",  alpha = 1) +
    labs(title = paste("Histogram of", trait), x = trait, y = "Frequency")
  print(Plot)
}

# Remove outliers

outliers <- function(x) {
  Q1 <- quantile(x, probs = .25) # calculate first quantile
  Q3 <- quantile(x, probs = .75) # calculate third quantile
  iqr <- Q3 - Q1           # calculate interquartile range
  upper_limit <- Q3 + (iqr * 1.5)  # Calculate upper limit
  lower_limit <- Q1 - (iqr * 1.5)  # Calculate lower limit
  x > upper_limit | x < lower_limit   # return true or false
}

WI_ALL2 <- WI_ALL
WI_ALL2 <- WI_ALL2[!outliers(WI_ALL2$PRO), ]
WI_ALL2 <- WI_ALL2[!outliers(WI_ALL2$STA), ]
WI_ALL2 <- WI_ALL2[!outliers(WI_ALL2$OIL), ]
WI_ALL2 <- WI_ALL2[!outliers(WI_ALL2$TWT), ]

#check distribution again
for (trait in Traits2) {
  Plot <- ggplot(WI_ALL2, aes(x = .data[[trait]], fill = YEAR, color = YEAR)) +
    geom_histogram(position="identity") +
    labs(title = paste("Histogram of", trait), x = trait, y = "Frequency")
  print(Plot)
}


## ESTIMATE BLUPS

Agron3 <- (WI_ALL2[1:26]) 
str(Agron3)
table(Agron3$HYB)

# STAGE ONE ANALYSIS - FIT A SIMPLE MODEL WITH FARMERS, HYB, YEAR 

DataOutput_HYB <- data.frame(matrix(vector(),21,1, dimnames=list(c(), c("HYB"))))
DataOutput_FAR <- data.frame(matrix(vector(),13,1, dimnames=list(c(), c("FAR"))))
DataOutput_YEAR <- data.frame(matrix(vector(),3,1, dimnames=list(c(), c("YEAR"))))

#fill empty dataframe with 1-300 so that the cbind will work later on
DataOutput_HYB$HYB   <-  unique(Agron3[,7]) #fill in Entry numbers
DataOutput_FAR$FAR   <-  unique(Agron3[,5]) #fill in Entry numbers
DataOutput_YEAR$YEAR <-unique(Agron3[,8]) #fill in Entry numbers

colnames(Agron3)
str(Agron3[,17:26]) #take only the columns with numerical data
colnum =c(17:ncol(Agron3))

i = 1 
for(i in 1:10){ #this second loop runs through each TRAIT, one at a time
  x = colnum[i] #set the current [i] column as x
  trait <- colnames(Agron3)[x] #sets the current column header as the trait name
  DF <- Agron3 #make a copy that we can use for our analysis
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
#extract summary statistics
Max <- colMax(DF[,colnum])
Min <- colMin(DF[,colnum])
Mean <- colMean(DF[,colnum])
Stdev <- colStdev(DF[,colnum])
SE <- Stdev/(sqrt(length(DF[,colnum])))
DataVarCompOutput <- cbind(Min,Max,Mean,SE)

## Export Results
write.csv(DataVarCompOutput , "Summary_Stats.csv")
write.csv(DataOutput_HYB, "BLUPs_HYB.csv")
write.csv(DataOutput_FAR, "BLUPs_FAR.csv")
write.csv(DataOutput_YEAR, "BLUP_YEAR.csv")


### ____TRAIT CORRELATIONS _____________
## Used the extracted hybrid blups to check for correlations

library(car)
library(corrplot)
Correlation <- cor(mean_sep[,c(2,4,6,8,10,12,14,16,18,20)], use = "pairwise.complete.obs")
png("Trait_Correlations.png", width = 20, height = 20, units = "cm", res = 300)
corplot<- corrplot(Correlation,method = "color",type="upper", order="hclust", #Type = upper,lower, #method=circle,pie,color,number
                   addCoef.col="black", # Add coefficient of correlation
                   diag=FALSE, # hide correlation coefficient on the principal diagonal
                   tl.col="black", tl.srt=45, #Text label color and rotation
                   p.mat=NULL, sig.level = 0.01, insig = "blank")  # Add coefficient of correlation

print(corplot)
dev.off()
#invisible(dev.off())







## Checking for significant differences
### using the lm function, we can conduct mean separations, and assign significant differences

library(agricolae)
library(dplyr)
mean_sep  <- data.frame(matrix(vector(),21,1, dimnames=list(c(), c("HYB"))))
mean_sep$HYB <- unique(Agron3[,7])

Meansep_year <- data.frame(matrix(vector(),3,1, dimnames=list(c(), c("YEAR"))))
Meansep_year$YEAR <- unique(Agron3[,8])

Meansep_Far <- data.frame(matrix(vector(),13,1, dimnames=list(c(), c("FAR"))))
Meansep_Far$FAR <- unique(Agron3[,5])


# Loop through each trait
for (trait in colnames(Agron3)[17:26]) {
  # Fit linear model
  model <- lm(paste(trait, "~ YEAR + FAR + HYB + FAR:HYB + YEAR:FAR:HYB"), data = Agron3)
  
  #LSD_test  <- HSD.test(model, "HYB", group = TRUE)  # Conduct Tukey's HSD test
  LSD_test <-  LSD.test(model,alpha = 0.05, "HYB",p.adj = "bonferroni")
  # Extract mean and groups
  mean_groups <- data.frame(LSD_test$groups)
  mean_groups  <- cbind(HYB =rownames(mean_groups), data.frame(mean_groups, row.names=NULL))
  mean_groups <- mean_groups %>%
                   mutate(!!paste0(trait, "g") := groups)
  mean_groups <- mean_groups[c(1:3)] 
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
  mean_groups <- mean_groups[c(1:3)] 
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
  mean_groups <- mean_groups[c(1:3)] 
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
Correlation <- cor(DataOutput_HYB[,c(6:11)], use = "pairwise.complete.obs")
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

## Using stepwise regression with step() function
Data_ws <- Agron3[c(7,10,11, 13:16,23)]
full_model <- lm(YIELD ~., data = Data_ws)
step_model <- step(full_model, direction = "backward")
anova(step_model)

#with interactions
step_model2  <- step(full_model, scope = . ~ .^2, direction = 'both')
anova(step_model2)

## Removing interactions that are not significant

step_model4  <- lm(formula = YIELD ~ HYB + FS + CC + Density + CI + CD + PER + CC:CD, data = Data_ws )
anova <- anova(step_model4)

## Mean separations
LSD_FS   <-  LSD.test(step_model4,alpha = 0.05, "FS",p.adj = "bonferroni")
LSD_CC   <-  LSD.test(step_model4,alpha = 0.05, "CC",p.adj = "bonferroni")
LSD_Dens <-  LSD.test(step_model4,alpha = 0.05, "Density",p.adj = "bonferroni")
LSD_CI   <-  LSD.test(step_model4,alpha = 0.05, "CI",p.adj = "bonferroni")
LSD_CD   <-  LSD.test(step_model4,alpha = 0.05, "CD",p.adj = "bonferroni")
LSD_PER  <-  LSD.test(step_model4,alpha = 0.05, "PER",p.adj = "bonferroni")


#check number of datapoints for each management level
table(Agron3$PER)
table(Agron3$CI)
table(Agron3$CD)
table(Agron3$FS)
table(Agron3$Density)
table(Agron3$CC)
library(ggplot2)
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


#2. Cover Crops -- WRT is for 

LSD_cc <- as.data.frame(LSD_CC$means)
LSD_cc <- cbind(CC =rownames(LSD_cc), data.frame(LSD_cc, row.names=NULL))
groups_cc <- as.data.frame(LSD_CC$groups)
groups_cc <- cbind(CC =rownames(groups_cc), data.frame(groups_cc, row.names=NULL))
groups_cc <- groups_cc[-2]


# Plot the bar graph
order_cc <- c("Legume", "Mixed", "Grass")
LSD_cc$CC  <- factor(LSD_cc$CC, levels = order_cc)
LSD_cc   <- merge(LSD_cc, groups_cc, by = "CC")
cc <- ggplot(data = LSD_cc, aes(y = YIELD, x =CC, fill = CC, color = CC)) +
  geom_bar(stat = "identity",width = 0.9, color="black",position=position_dodge(width=1)) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2, color = "black", linewidth = 0.6, position = position_dodge(0.9)) +
  geom_text(data = LSD_cc, aes(label = groups, y = UCL + 0.5), color = "black", size = 5, position = position_dodge(width = 0.9)) +  
  labs(title = "",
       x = "Cover Crop",
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
        plot.title = element_text(hjust = 0.5)) +   # Increase axis title font size
  scale_fill_manual(values = c("#F8766D", "#619CFF")) 

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
        plot.title = element_text(hjust = 0.5)) + # Increase axis title font size
  scale_fill_manual(values = c("#F8766D", "#619CFF")) 


library(gridExtra)
grid.arrange(fs,cc, pd,ci, cd,pe,  ncol = 3) 


## Effect of management on other traits  ________make it a loop somehow___

Data2 <- Agron3[c(7,10:16,17:22, 24:26)]
str(Data2)

Traits <- c("PHT","EHT","SDL","SDS","TWT","KWT300","PRO","OIL","STA")

FS   <- data.frame(FS = factor(c("Animal", "Mix", "Green")))
CC  <- data.frame(CC = factor(c("Legume", "Mixed", "Grass")))
Dens <- data.frame(Density = factor(c("Low", "Medium", "High")))
CI   <- data.frame(CI = factor(c("High", "Medium", "Low")))
CD   <- data.frame(CD = factor(c("High", "Low")))
PER  <- data.frame(PER = factor(c("Absent", "Present")))
library(agricolae)
library(dplyr)
for (trait2 in Traits){
  step_model <- lm(formula = paste(trait2, "~ HYB + FS + CC + Density + CI + CD + PER + CC:CD"), data = Data2)

  ## Mean separations
  LSD_FS   <-  LSD.test(step_model,alpha = 0.05, "FS",p.adj = "bonferroni")
  LSD_CC  <-  LSD.test(step_model,alpha = 0.05, "CC",p.adj = "bonferroni")
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
  
  LSD_cc <- as.data.frame(LSD_CC$means)
  LSD_cc <- cbind(CC =rownames(LSD_cc), data.frame(LSD_cc, row.names=NULL))
  groups_cc <- as.data.frame(LSD_CC$groups)
  groups_cc <- cbind(CC =rownames(groups_cc), data.frame(groups_cc, row.names=NULL))
  groups_cc <- groups_cc[-2]
  order_cc <- c("Legume", "Mixed", "Grass")
  LSD_cc$CC  <- factor(LSD_cc$CC, levels = order_cc)
  LSD_cc   <- merge(LSD_cc, groups_cc, by = "CC")
  
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
  
  LSD_cc <-  LSD_cc[c(1,2,13)]
  colnames(LSD_cc)[3] <- new_colname
  CC <- left_join(CC, LSD_cc, by = "CC")
  
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

mFS <- data.frame(Management = factor(c("FS", "FS", "FS"), levels = c("FS", "CC", "Dens", "CI", "CD", "PER")),
                  Levels = factor(c("Animal", "Mix", "Green"), levels = c("Animal", "Mix", "Green")))
mCC <- data.frame(Management = factor(c("CC", "CC", "CC"), levels = c("FS", "CC", "Dens", "CI", "CD", "PER")),
                   Levels = factor(c("Legume", "Mixed", "Grass"), levels = levels(CC$CC)))
mDens <- data.frame(Management = factor(c("Dens", "Dens", "Dens"), levels = c("FS", "CC", "Dens", "CI", "CD", "PER")),
                    Levels = factor(c("Low", "Medium", "High"), levels = levels(Dens$Density)))
mCI <- data.frame(Management = factor(c("CI", "CI", "CI"), levels = c("FS", "CC", "Dens", "CI", "CD", "PER")),
                  Levels = factor(c("High", "Medium", "Low"), levels = levels(CI$CI)))
mCD <- data.frame(Management = factor(c("CD", "CD"), levels = c("FS", "CC", "Dens", "CI", "CD", "PER")),
                  Levels = factor(c("High", "Low"), levels = levels(CD$CD)))
mPER <- data.frame(Management = factor(c("PER", "PER"), levels = c("FS", "CC", "Dens", "CI", "CD", "PER")),
                   Levels = factor(c("Absent", "Present"), levels = levels(PER$PER)))

Management <- rbind(mFS, mCC, mDens, mCI, mCD, mPER)  

# Combine all data frames into a single dataframe
Effect_Table <- rbind(FS[(2:19)],CC[(2:19)],Dens[c(2:19)],CI[c(2:19)] ,CD[c(2:19)] ,PER[c(2:19)] )

Management_Effect <- cbind(Management,Effect_Table)
write.csv(Management_Effect, "Management_Effect_WISCON.csv")


## RELIABILITY ESTIMATION USING ORG4

ReliabiltyData <- WI_ALL

#Means on each farm (average of 2 reps)
library(dplyr)
WS_means <- ReliabiltyData %>%
  group_by(FC, HYB) %>%
  summarize(across(PHT:GDD_Sep,~ mean(., na.rm = TRUE), .names = "{.col}")) #restart R to run this if u have issues with error msg

## Estimate yield differences, mean Yd, standard deviations, and zscores. Also estimate reliability with non-parametric method (n/env)
WS_YD <- WS_means[c(1,2,9)]  %>%
  group_by(FC) %>%  #FC is based on years. same farmer in different years has different codes
  mutate(YD = YIELD - YIELD[HYB == "ORG4"]) %>%  #checks changed across years. 
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
WS_Rel <- WS_YD[-c(1,3,4,11)] %>%   
  filter(HYB != "ORG4") %>%  #remove YD
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

write.csv(WS_Rel, "Table_WS_Reliability.csv")  #Add Check mean sd, and envi





