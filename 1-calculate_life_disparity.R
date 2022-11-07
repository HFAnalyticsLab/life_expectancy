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
               patchwork, 
               data.table)

# check working directory
here()


# create vector of country names
countries <- c("australia", "austria", "belarus", "belgium", "bulgaria", "canada", 
               "chile", "croatia", "czechia", "denmark", "estonia", "finland", "france", 
               "germany", "greece", "hongkong", "hungary", "iceland", "ireland",
               "israel", "italy", "japan", "korea", "latvia", "lithuania", "luxembourg",
               "netherlands", "newzealand", "norway", "poland", "portugal", "slovakia", "slovenia",
               "spain", "sweden", "switzerland", "taiwan", "uk", "usa")
  # russia and ukraine data available but only for 2010-13

# Create empty data frame
results <- data.frame(Year=character(),
                 e0=numeric(),
                 e10=numeric(),
                 ld0=numeric(),
                 ld10=numeric(), 
                 sd0=numeric(),
                 sd10=numeric(), 
                 gini=numeric(),
                 country=character())



for (country in countries){

# Import data ####
    ## data were downloaded from: https://www.mortality.org/ 
    ## using 1x5 life tables for each country
countrydta <- s3read_using(read_excel
                      , object = paste0('s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_', country, '_total.xlsx') 
                      , skip = 2)


# drop last three columns
countrydta <- countrydta %>% 
  dplyr::select(Year:ex)

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

# Calculate standard deviation of life expectancy at birth

  # create variable with life expectancy at birth for each year  
countrydta <- countrydta %>%
    group_by(Year) %>%
    mutate(e0 = ex[Age == 0L])
  # calculate mean age at death - works out the same as e0
countrydta <- countrydta %>%
  mutate(mu=sum((Age + ax)*dx)/100000)
  # calculate distance to life expectancy in each age band, weighted by number of deaths in interval
countrydta <- countrydta %>%
  mutate(tmp=((Age + ax)-e0)^2*dx)
  # calculate standard deviation
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(sd0=sqrt(sum(tmp)/(100000-1)))


# Calculate Gini coefficient

# cumulative percentage of population deaths
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(dx_cum = cumsum(dx)/sum(dx))

# cumulative percentage of life years lived by people before death
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(Lx_deaths = ((Age + ax)*dx))
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(Lx_prop = (Lx_deaths/sum(Lx_deaths)))
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(Lx_cum = cumsum(Lx_deaths)/sum(Lx))

# calculate area below curve
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(area = (dx_cum - Lx_cum)*(Lx_prop))

# calculate gini coefficient
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(gini = (sum(area)/0.5))



# Measures of variation at age 10 ####
# Calculate life disparity at age 10
countrydta <- countrydta %>%
  mutate(tmp=case_when(Age>=10 ~ dx*ex))

countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(l10 = lx[Age == 10L])

countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(ld10=sum(tmp, na.rm = TRUE)/l10)

# remove unnecessary columns
countrydta <- dplyr::select(countrydta, -mx, -Lx)


# Calculate standard deviation

# create variable with life expectancy at age 10 for each year  
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(e10 = ex[Age == 10L])
# calculate mean age at death - works out the same as e10 +10
countrydta <- countrydta %>%
  group_by(Year) %>%
  dplyr::filter(Age >=10) %>%
  mutate(mu=case_when( Age>=10 ~ sum((Age + ax)*dx)/l10))

countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(mu2=sum((Age + ax)*dx)/l10)

# calculate distance to life expectancy in each age band, weighted by number of deaths in interval
countrydta <- countrydta %>%
  mutate(tmp= ((mu)-(Age + ax))^2*dx)
# calculate standard deviation
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(sd10=sqrt(sum(tmp)/(l10-1)))

  # NOTE: sd10 is slightly lower than sd0, this is consistent


# Keep only one row per year
countrydta <- countrydta %>% 
  dplyr::filter(Age == 10)

# Select only variables of interest
countrydta <- countrydta %>% 
  dplyr::select(Year, e0, e10, ld0, ld10, sd0, sd10, gini)

# Add in column for country
countrydta <- countrydta %>%
  mutate(country = country)

# Append country to results df with all countries
results <- rbind(results, countrydta)
}

# NOTE: Tuljapurkar 2010 uses e0 as central measure and s10 as measure of dispersion



results <- results %>%
  mutate(country = str_to_title(country))


# Change name for USA/UK
results <- results %>%
  mutate(country = replace(country, country == "Uk", "UK"),
        country = replace(country, country == "Usa", "USA"),
        country = replace(country, country == "Hongkong", "Hong Kong"),
        country = replace(country, country == "Newzealand", "New Zealand"))


nrow(results)
tabyl(results$country)



# Load data for population size
popsize <- s3read_using(read_excel
                        , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/country_population_worldbank.xls' 
                        , skip = 3)
  
# Keep only 2015 population
popsize <- popsize %>%
  dplyr::select(country = `Country Name`, popsize = `2015`, code = `Country Code`)

# Add in Taiwan
popsize[nrow(popsize) + 1, 1] = "Taiwan"
popsize[nrow(popsize), 2] = 23492000
popsize[nrow(popsize), 3] = "TWN"


# Edit country names to match results dataframe
popsize <- popsize %>%
  mutate(country = replace(country, country == "United Kingdom", "UK"),
         country = replace(country, country == "United States", "USA"), 
         country = replace(country, country == "Korea, Rep.", "Korea"),
         country = replace(country, country == "Czech Republic", "Czechia"), 
         country = replace(country, country == "Hong Kong SAR, China", "Hong Kong"), 
         country = replace(country, country == "Slovak Republic", "Slovakia"))


# Merge onto results dataframe
results <- merge(results, popsize, by="country")

nrow(results)


#Save dataset 

buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy' ## my bucket name

s3write_using(results # What R object we are saving
              , FUN = write_rds # Which R function we are using to save
              , object = 'results.RDS' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above

# Save excel version for Flourish
s3write_using(results # What R object we are saving
              , FUN = write.csv  # Which R function we are using to save
              , object = 'results.csv' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above




# below should not be necessary, since have now included all historical data for all countries in the results df
  # to be removed once code has been checked 


# UK data over time for bubble plot ####
country <- "uk"
countrydta <- s3read_using(read_excel
                           , object = paste0('s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_', country, '_total.xlsx') 
                           , skip = 2)


# drop last three columns
countrydta <- countrydta %>% 
  dplyr::select(Year:ex)

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

# Calculate standard deviation of life expectancy at birth

# create variable with life expectancy at birth for each year  
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(e0 = ex[Age == 0L])
# calculate mean age at death - works out the same as e0
countrydta <- countrydta %>%
  mutate(mu=sum((Age + ax)*dx)/100000)
# calculate distance to life expectancy in each age band, weighted by number of deaths in interval
countrydta <- countrydta %>%
  mutate(tmp=((Age + ax)-e0)^2*dx)
# calculate standard deviation
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(sd0=sqrt(sum(tmp)/(100000-1)))


# Calculate Gini coefficient

# cumulative percentage of population deaths
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(dx_cum = cumsum(dx)/sum(dx))

# cumulative percentage of life years lived by people before death
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(Lx_deaths = ((Age + ax)*dx))
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(Lx_prop = (Lx_deaths/sum(Lx_deaths)))
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(Lx_cum = cumsum(Lx_deaths)/sum(Lx))

# calculate area below curve
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(area = (dx_cum - Lx_cum)*(Lx_prop))

# calculate gini coefficient
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(gini = (sum(area)/0.5))



# Measures of variation at age 10 ####
# Calculate life disparity at age 10
countrydta <- countrydta %>%
  mutate(tmp=case_when(Age>=10 ~ dx*ex))

countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(l10 = lx[Age == 10L])

countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(ld10=sum(tmp, na.rm = TRUE)/l10)

# remove unnecessary columns
countrydta <- dplyr::select(countrydta, -mx, -Lx)


# Calculate standard deviation

# create variable with life expectancy at age 10 for each year  
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(e10 = ex[Age == 10L])
# calculate mean age at death - works out the same as e10 +10
countrydta <- countrydta %>%
  group_by(Year) %>%
  dplyr::filter(Age >=10) %>%
  mutate(mu=case_when( Age>=10 ~ sum((Age + ax)*dx)/l10))

countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(mu2=sum((Age + ax)*dx)/l10)

# calculate distance to life expectancy in each age band, weighted by number of deaths in interval
countrydta <- countrydta %>%
  mutate(tmp= ((mu)-(Age + ax))^2*dx)
# calculate standard deviation
countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(sd10=sqrt(sum(tmp)/(l10-1)))


countrydta <- countrydta %>%
  dplyr::filter(Age == 10)



# Append uk data to countrydta
countrydta <- countrydta %>% 
  dplyr::select(Year, e0, e10, ld0, ld10, sd0, sd10)

countrydta <- countrydta %>%
  mutate(country = "UK hist")


combined <- bind_rows(results, countrydta)

combined <- combined %>%
  mutate(group=country == "UK hist",
         popsize = replace_na(popsize, 3000000))


# Save dataset
s3write_using(combined # What R object we are saving
              , FUN = write_rds # Which R function we are using to save
              , object = 'combined.RDS' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above






# Following bit of code is from the Hiam et al. paper on life disparity
  ## https://github.com/JonMinton/rising_tide/blob/master/analyses.Rmd
Hiam_paper_dta <- s3read_using(import
                           , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/dta_Mx.RDS' )

dta_e0 <- s3read_using(import
                               , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/dta_e0.RDS' )

Hiam_paper_dta <- Hiam_paper_dta %>%
  dplyr::filter(code == 'JPN' & year == '2017')

dta_e0 <- dta_e0 %>%
  dplyr::filter(code== 'JPN' & year == '2017')

Hiam_paper_dta <-  merge(Hiam_paper_dta, dta_e0, by="sex")




Hiam_paper_dta <- Hiam_paper_dta %>%
  mutate(tmp=dx*ex)

countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(ldsp0=sum(tmp)/100000)



calc_e_dagger_parts <- function(countrydta, omega = 100){
    exp(-sum(countrydta$mx[countrydta$x <= a]))
  }
countrydta %>% 
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
