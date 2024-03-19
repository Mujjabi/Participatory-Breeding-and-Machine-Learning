library(tidyr)
library(dplyr)

# List the column pairs you want to stack
column_pairs <- list(
  c("PHT1", "PHT2"),
  c("EHT1", "EHT2"),
  c("SDL1", "SDL2"),
  c("SDS1", "SDS2"),
  c("MST1", "MST2"),  # Assuming you have two columns named MST1 and MST2
  c("TWT1", "TWT2"),
  c("NOP1", "NOP2"),
  c("GWT1", "GWT2"),
  c("KWT1", "KWT2"),
  c("PRO1", "PRO2"),
  c("OIL1", "OIL2"),
  c("STA1","STA2"),
  c("DEN1", "DEN2"),
  c("FIB1", "FIB2")
)

# Use dplyr and tidyr to stack each pair of columns and combine them

my_df3  <- read.csv("Unstacked_2020IL.csv", stringsAsFactors = T)

  
 stacked2020 <- reshape2::melt(my_df3, id.var = c('EXPERIMENT', 'FARMER',	'STATE', 'HYBRID',	'CODE', 'CEC',	'pH',	'OM',	'EstNRel','S',	'P',	'Ca', 'Mg',	'K',	'Na'),
                             variable.name = 'Traits') 
  print(stacked2020)
  
  library(readr)
  write.csv(stacked2020, "stacked_2020IL.csv")
  

# Unstacking values again into one column
  unstack  <- read.csv("stacked_2019IL.csv", stringsAsFactors = T)
  unstack$value <- as.numeric(unstack$value)
  
  library(tidyverse)
 p <-  unstack  %>% 
    pivot_wider(names_from = Trait,
                values_from = value,
                values_fn = list) %>% 
    unnest(cols = c(PHT,EHT,SDL,SDS,MST,TWT,NOP,GWT,KWT300,PRO,OIL,STA,DEN,FIB))
P <-  data.frame(p)



P <- unstack %>%
  group_by(FAR,REP,CODE) %>%
  mutate(index = row_number()) %>%
  pivot_wider(names_from = "Trait", values_from = "value")



