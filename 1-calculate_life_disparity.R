## Calculating life disparity and variation in life expectancy

# Housekeeping ####
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
               patchwork)

# check working directory
here()


# create vector of country names
countries <- c("uk")

# create empty list to store results
results <- vector(mode = "list", length = length(countries))


for (country in countries){
  
# Import data ####
    ## data were downloaded from: https://www.mortality.org/ 
    ## using 1x5 life tables for each country
[country]dta <- s3read_using(read_excel
                      , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_[country]_total.xlsx' 
                      , skip = 2)
}

# drop last three columns
ukdta <- ukdta %>% 
  dplyr::select(-...11, -...12, -...13)


# Calculate life disparity
ukdta <- ukdta %>%
  mutate(tmp=dx*ex)

ukdta <- ukdta %>%
  group_by(Year) %>%
  mutate(ldsp=sum(tmp)/100000)


# Extract life expectancy and life disparity and add to list
e0 <- ukdta$ex[ukdta$Year == '2015-2018' & ukdta$Age == 0]
e0
ld <- ukdta$ldsp_1518[ukdta$Year == '2015-2018' & ukdta$Age == 0]
ld

df <- data.frame(e0, ld, "uk")

results <- c(results, L1 = list(df))



# France
frdta <- s3read_using(read_excel
                      , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_france_total.xlsx', 
                      skip = 2)

# Calculate life disparity
frdta <- frdta %>%
  mutate(tmp=dx*ex)

frdta <- frdta %>%
  filter(Year=='2015-2019') %>%
  mutate(ldsp_1518=sum(tmp)/100000)


# Germany
gedta <- s3read_using(read_excel
                      , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_germany_total.xlsx', 
                      skip = 2)

# Calculate life disparity
gedta <- gedta %>%
  mutate(tmp=dx*ex)

gedta <- gedta %>%
  filter(Year=='2015-2017') %>%
  mutate(ldsp_1518=sum(tmp)/100000)

# Netherlands
nldta <- s3read_using(read_excel
                      , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_netherlands_total.xlsx', 
                      skip = 2)

# Calculate life disparity
nldta <- nldta %>%
  mutate(tmp=dx*ex)

nldta <- nldta %>%
  filter(Year=='2015-2019') %>%
  mutate(ldsp_1518=sum(tmp)/100000)


# Norway
nwdta <- s3read_using(read_excel
                      , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_norway_total.xlsx', 
                      skip = 2)

# Calculate life disparity
nwdta <- nwdta %>%
  mutate(tmp=dx*ex)

nwdta <- nwdta %>%
  filter(Year=='2015-2019') %>%
  mutate(ldsp_1518=sum(tmp)/100000)


# USA
usdta <- s3read_using(read_excel
                      , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_usa_total.xlsx', 
                      skip = 2)

# Calculate life disparity
usdta <- usdta %>%
  mutate(tmp=dx*ex)

usdta <- usdta %>%
  filter(Year=='2015-2019') %>%
  mutate(ldsp_1518=sum(tmp)/100000)


# Japan
jpdta <- s3read_using(read_excel
                      , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_japan_total.xlsx', 
                      skip = 2)

# Calculate life disparity
jpdta <- jpdta %>%
  mutate(tmp=dx*ex)

jpdta <- jpdta %>%
  filter(Year=='2015-2019') %>%
  mutate(ldsp_1518=sum(tmp)/100000)


# Korea
krdta <- s3read_using(read_excel
                      , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_korea_total.xlsx', 
                      skip = 2)

# Calculate life disparity
krdta <- krdta %>%
  mutate(tmp=dx*ex)

krdta <- krdta %>%
  filter(Year=='2015-2018') %>%
  mutate(ldsp_1518=sum(tmp)/100000)


# Australia
aldta <- s3read_using(read_excel
                      , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_australia_total.xlsx', 
                      skip = 2)

# Calculate life disparity
aldta <- aldta %>%
  mutate(tmp=dx*ex)

aldta <- aldta %>%
  filter(Year=='2015-2019') %>%
  mutate(ldsp_1518=sum(tmp)/100000)


# Austria
audta <- s3read_using(read_excel
                      , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_austria_total.xlsx', 
                      skip = 2)

# Calculate life disparity
audta <- audta %>%
  mutate(tmp=dx*ex)

audta <- audta %>%
  filter(Year=='2015-2019') %>%
  mutate(ldsp_1518=sum(tmp)/100000)



# Poland
podta <- s3read_using(read_excel
                      , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_poland_total.xlsx', 
                      skip = 2)

# Calculate life disparity
podta <- podta %>%
  mutate(tmp=dx*ex)

podta <- podta %>%
  filter(Year=='2015-2019') %>%
  mutate(ldsp_1518=sum(tmp)/100000)



# Czechia
czdta <- s3read_using(read_excel
                      , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_czechia_total.xlsx', 
                      skip = 2)

# Calculate life disparity
czdta <- czdta %>%
  mutate(tmp=dx*ex)

czdta <- czdta %>%
  filter(Year=='2015-2019') %>%
  mutate(ldsp_1518=sum(tmp)/100000)


# Italy
itdta <- s3read_using(read_excel
                      , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_italy_total.xlsx', 
                      skip = 2)

# Calculate life disparity
itdta <- itdta %>%
  mutate(tmp=dx*ex)

itdta <- itdta %>%
  filter(Year=='2015-2019') %>%
  mutate(ldsp_1518=sum(tmp)/100000)


# Following bit of code is from the Hiam et al. paper on life disparity
  ## https://github.com/JonMinton/rising_tide/blob/master/analyses.Rmd

calc_e_dagger_parts <- function(LT, omega = 100){
  calc_ell_a <- function(LT, a){
    exp(-sum(LT$mx[LT$x <= a]))
  }
  LT %>% 
    filter(x <= omega) %>% 
    mutate(ell_x = map_dbl(x, calc_ell_a, LT = LT)) %>% 
    mutate(e_dagger_component = ell_x * ex * mx) %>% 
    select(x, e_dagger_component)
}

mutate(lifetable = map(data, ~LifeTable(x = .x$age, mx = .x$Mx)$lt)) %>%
  mutate(e_dagger_parts = map(lifetable, calc_e_dagger_parts))





calc_e_dagger_parts <- function(LT, omega = 100){
  calc_ell_a <- function(LT, a){
    exp(-sum(LT$mx[LT$x <= a]))
  }
  LT %>% 
    filter(x <= omega) %>% 
    mutate(ell_x = map_dbl(x, calc_ell_a, LT = LT)) %>% 
    mutate(e_dagger_component = ell_x * ex * mx) %>% 
    select(x, e_dagger_component)
}
tmp <- calc_e_dagger_parts(lt_jpn_f$lt)
tmp
sum(tmp$e_dagger_component)

```{r}
calc_e_dagger_parts <- function(LT, omega = 100){
  calc_ell_a <- function(LT, a){
    exp(-sum(LT$mx[LT$x <= a]))
  }
  LT %>% 
    filter(x <= omega) %>% 
    mutate(ell_x = map_dbl(x, calc_ell_a, LT = LT)) %>% 
    mutate(e_dagger_component = ell_x * ex * mx) %>% 
    select(x, e_dagger_component)
}
e_dagger_parts <- 
  dta_Mx %>% 
  left_join(country_labels) %>% 
  filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>%
  filter(year >= 1975) %>% 
  rename(Country = label) %>% 
  select(Country, sex, year, age, Mx) %>% 
  group_by(Country, sex, year) %>% 
  nest() %>% 
  mutate(lifetable = map(data, ~LifeTable(x = .x$age, mx = .x$Mx)$lt)) %>% 
  mutate(e_dagger_parts = map(lifetable, calc_e_dagger_parts))
e_daggers <- 
  e_dagger_parts %>% 
  mutate(e_dagger       = map_dbl(e_dagger_parts, ~sum(.x$e_dagger_component))) %>% 
  select(Country, sex, year, e_dagger)
e_daggers 
e_dagger_parts <-
  e_dagger_parts %>% 
  select(Country, sex, year, e_dagger_parts) %>% 
  unnest(e_dagger_parts) %>% 
  ungroup()
e_dagger_parts
```