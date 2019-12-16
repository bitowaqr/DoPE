library(reshape2)
library(dplyr)
library(tidyverse)
library(ggplot2)
library("viridis")
#install.packages("corrplot")
library(corrplot)
library(ggplot2)
library(fitdistrplus)
#setwd('C:/Users/Robert/Google Drive/Other Projects/PARKRUN/DoPE/Dissemination')

df <- read.csv("../output/lsoa_df.csv") %>%
  dplyr::select(run_count,imd, perc_bme, mn_dstn,  
                total_pop, pop_density,perc_non_working_age) %>% 
  mutate(run_rate = run_count/total_pop)

before <- df$run_rate
df$run_rate[df$run_rate < median(df$run_rate)] <- median(df$run_rate)
after  <- df$run_rate

sum((after - before)*df$total_pop) / sum(df$run_count)



