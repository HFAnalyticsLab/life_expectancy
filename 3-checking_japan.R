### checking Japan 2017 data against Hiam et al. life disparity calculations

# clear R environment
rm(list = ls())

# load packages
library(aws.s3)

pacman::p_load(readxl,
               haven, 
               dplyr, 
               survey, 
               janitor,
               questionr, 
               epiDisplay, 
               rio, 
               ggplot2, 
               apyramid,
               magrittr, 
               stringr, 
               here,
               data.table, 
               tidyverse, 
               ggrepel, 
               here,
               corrr, 
               knitr,
               kableExtra, 
               openxlsx,
               MortalityLaws, 
               patchwork, 
               data.table)

# Import data ####
## data were downloaded from: https://www.mortality.org/ 
## using 1x5 life tables for each country
countrydta <- s3read_using(read_excel
                           , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/japan_1x1_lifetable.xlsx' 
                           , skip = 2)

countrydta <- countrydta %>%
  dplyr::filter(Year == 2017)


# recode 110+ to 110
countrydta <- countrydta %>% 
  mutate(Age = replace(Age, Age == '110+', '110' ))
countrydta <- countrydta %>% 
  mutate(Age = as.numeric(Age))

# Measures of variation for age 0 ####
# Calculate life disparity at age 0
countrydta <- countrydta %>%
  mutate(tmp=dx*ex)

countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(ld0=sum(tmp)/100000)

