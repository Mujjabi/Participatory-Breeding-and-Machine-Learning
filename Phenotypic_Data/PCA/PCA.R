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
                 ellipse = TRUE, groups=Clean2$ENV)
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
                 ellipse = TRUE, groups=weather$STATE)   #Year also gave some patterns
print(bplot2)


## Add design variables and management and yield to both soil and weather scores from python
Design <- read.csv("Design_Management.csv", header = TRUE)

## Import from python 
soilscores <- read.csv("Soilscores.csv", header = TRUE)
weatherscores   <- read.csv("weatherscores.csv", header = TRUE)

PCA_Data <-  cbind(Design, soilscores[c(2:7)], weatherscores[c(2:4)])




