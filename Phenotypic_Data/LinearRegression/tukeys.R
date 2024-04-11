

model = lm(PHT ~ YEAR+ FAR +  HYB + FAR:HYB + YEAR:FAR:HYB, data= Agron)
ANOVA=aov(model)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'HYB', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")

generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}
library(multcompView)
LABELS <- generate_label_df(TUKEY , "HYB")



