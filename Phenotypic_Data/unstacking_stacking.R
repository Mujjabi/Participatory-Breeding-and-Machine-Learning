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
  c("KWT3001", "KWT3002"),
  c("PRO1", "PRO2"),
  c("OIL1", "OIL2"),
  c("STA1","STA2"),
  c("DEN1", "DEN2"),
  c("FIB1", "FIB2")
)

# Use dplyr and tidyr to stack each pair of columns and combine them

my_df2  <- read.csv("Unstacked_2019IL.csv", stringsAsFactors = T)

  
 stacked <- reshape2::melt(my_df2, id.var = c('YR',	'FAR',	'FC',	'STATE',	'HYB',	'CODE'
)
                             variable.name = 'Traits') 
  print(data_mod)
  library(readr)
  write.csv(stactacked_2019IL.csv)
  
  