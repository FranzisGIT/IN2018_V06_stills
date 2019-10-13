# data exploration of Percent Cover for substrate types; input filrs from cleaning up 
# the TM output data (script: Tidy_TM_Concat_data.R)

library(tidyverse)
PCcoverbyImage <- read_csv("Results/PCcoverbyImage.csv")

PC_cover <- read_csv("Results/PCcover.csv")
