#### Calculating future trends in life disparity based on ONS mortality projections

# clear R environment
rm(list = ls())

#load packages
pacman::p_load(tidyverse, 
               gtsummary,
               janitor,
               rio, 
               ggplot2, 
               stringr, 
               here, 
               aws.s3,
               readr,
               Hmisc, 
               readxl)


# Men --------------------------------------------------------------------------
## Data load --------------------------------------------------------------------
    # data were downloaded from: https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/mortalityratesqxprincipalprojectionenglandandwales/2020based/ewppp20qx.xlsx
    # and manually saved to R workbench because S3 buckets not available at the time of analysis
men <- read_excel(here::here("ONS_mortality_projections.xlsx"), 5)
  
men <- slice(men, -(1:3)) %>%
  row_to_names(., 1) %>%
  clean_names()

men <- men %>% 
  mutate(across(.cols=1:91, .fns=as.numeric))

# average mortality rates over 5 year intervals
men <- men %>%
  rowwise %>%
  dplyr::mutate(p198185 = mean(c(x1981, x1982, x1983, x1984, x1985))/100000, 
                p198690 = mean(c(x1986, x1987, x1988, x1989, x1990))/100000, 
                p199195 = mean(c(x1991, x1992, x1993, x1994, x1995))/100000, 
                p199600 = mean(c(x1996, x1997, x1998, x1999, x2000))/100000, 
                p200105 = mean(c(x2001, x2002, x2003, x2004, x2005))/100000, 
                p200610 = mean(c(x2006, x2007, x2008, x2009, x2010))/100000, 
                p201115 = mean(c(x2011, x2012, x2013, x2014, x2015))/100000, 
                p201620 = mean(c(x2016, x2017, x2018, x2019, x2020))/100000, 
                p202125 = mean(c(x2021, x2022, x2023, x2024, x2025))/100000, 
                p202630 = mean(c(x2026, x2027, x2028, x2029, x2030))/100000, 
                p203135 = mean(c(x2031, x2032, x2033, x2034, x2035))/100000, 
                p203640 = mean(c(x2036, x2037, x2038, x2039, x2040))/100000, 
                p204145 = mean(c(x2041, x2042, x2043, x2044, x2045))/100000, 
                p204650 = mean(c(x2046, x2047, x2048, x2049, x2050))/100000, 
                p205155 = mean(c(x2051, x2052, x2053, x2054, x2055))/100000, 
                p205660 = mean(c(x2056, x2057, x2058, x2059, x2060))/100000, 
                p206165 = mean(c(x2061, x2062, x2063, x2064, x2065))/100000, 
                p206670 = mean(c(x2066, x2067, x2068, x2069, x2070))/100000) 
                
# drop year-specific mortality rates
men <- men %>%
  dplyr::select(-(starts_with("x")))

# create a(x) column
men <- men %>%
  mutate(ax = case_when(
    age == 0 ~ 0.14,
    age == 100 ~ 1.33, 
    TRUE ~ 0.50))
  

# reshape to long so 5 year periods are one after the other
men <- men %>%
  gather(years, qx, p198185:p206670)



## Construct life table --------------------------------------------------------
# replace all qx = 1 for highest age group
men <- men %>%
  mutate(qx = ifelse(age == 100, 1, qx))

# calculate lx and dx
men <- men %>%
  mutate(lx = ifelse(age == 0, 100000, NA_real_),
         dx = ifelse(age == 0, 100000*qx, NA_real_))

for (yr in c(1:100)) {
 men <- men %>%
   mutate(lx = if_else(age == yr, lag(lx) - lag(dx), lx), 
         dx = if_else(age == yr, qx*lx, dx))
}


# calculate Lx
men <- men %>%
  mutate(Lx = if_else(age == 0, ax*lx + (1-ax)*lead(lx), NA_real_))

for (yr in c(1:99)) {
  men <- men %>% 
    mutate(Lx = if_else(age == yr, ax*lx + (1-ax)*lead(lx), Lx))
}

men <- men %>%
  mutate(Lx = if_else(age == 100, ax*lx, Lx))


# calculate Tx
men <- men %>%
  group_by(years) %>%
  arrange(desc(age)) %>%
  mutate(Tx = cumsum(Lx))

men <- men %>%
  arrange(years, age)


# calculate Ex
men <- men %>%
  mutate(ex = Tx/lx)


# Calculate life disparity at age 0
men <- men %>%
  mutate(tmp=dx*ex)

men <- men %>%
  group_by(years) %>%
  mutate(ld0=sum(tmp)/100000)


# Calculate life disparity at age 10
men <- men %>%
  mutate(tmp=case_when(age>=10 ~ dx*ex))

men <- men %>%
  group_by(years) %>%
  mutate(l10 = lx[age == 10L])

men <- men %>%
  group_by(years) %>%
  mutate(ld10=sum(tmp, na.rm = TRUE)/l10)





# Women --------------------------------------------------------------------------
## Data load --------------------------------------------------------------------
# data were downloaded from: https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/mortalityratesqxprincipalprojectionenglandandwales/2020based/ewppp20qx.xlsx
# and manually saved to R workbench because S3 buckets not available at the time of analysis
women <- read_excel(here::here("ONS_mortality_projections.xlsx"), 6)

women <- slice(women, -(1:3)) %>%
  row_to_names(., 1) %>%
  clean_names()

women <- women %>% 
  mutate(across(.cols=1:91, .fns=as.numeric))

# average mortality rates over 5 year intervals
women <- women %>%
  rowwise %>%
  dplyr::mutate(p198185 = mean(c(x1981, x1982, x1983, x1984, x1985))/100000, 
                p198690 = mean(c(x1986, x1987, x1988, x1989, x1990))/100000, 
                p199195 = mean(c(x1991, x1992, x1993, x1994, x1995))/100000, 
                p199600 = mean(c(x1996, x1997, x1998, x1999, x2000))/100000, 
                p200105 = mean(c(x2001, x2002, x2003, x2004, x2005))/100000, 
                p200610 = mean(c(x2006, x2007, x2008, x2009, x2010))/100000, 
                p201115 = mean(c(x2011, x2012, x2013, x2014, x2015))/100000, 
                p201620 = mean(c(x2016, x2017, x2018, x2019, x2020))/100000, 
                p202125 = mean(c(x2021, x2022, x2023, x2024, x2025))/100000, 
                p202630 = mean(c(x2026, x2027, x2028, x2029, x2030))/100000, 
                p203135 = mean(c(x2031, x2032, x2033, x2034, x2035))/100000, 
                p203640 = mean(c(x2036, x2037, x2038, x2039, x2040))/100000, 
                p204145 = mean(c(x2041, x2042, x2043, x2044, x2045))/100000, 
                p204650 = mean(c(x2046, x2047, x2048, x2049, x2050))/100000, 
                p205155 = mean(c(x2051, x2052, x2053, x2054, x2055))/100000, 
                p205660 = mean(c(x2056, x2057, x2058, x2059, x2060))/100000, 
                p206165 = mean(c(x2061, x2062, x2063, x2064, x2065))/100000, 
                p206670 = mean(c(x2066, x2067, x2068, x2069, x2070))/100000) 

# drop year-specific mortality rates
women <- women %>%
  dplyr::select(-(starts_with("x")))

# create a(x) column
women <- women %>%
  mutate(ax = case_when(
    age == 0 ~ 0.14,
    age == 100 ~ 1.33, 
    TRUE ~ 0.50))


# reshape to long so 5 year periods are one after the other
women <- women %>%
  gather(years, qx, p198185:p206670)


## Construct life table --------------------------------------------------------
# replace all qx = 1 for highest age group
women <- women %>%
  mutate(qx = ifelse(age == 100, 1, qx))

# calculate lx and dx
women <- women %>%
  mutate(lx = ifelse(age == 0, 100000, NA_real_),
         dx = ifelse(age == 0, 100000*qx, NA_real_))

for (yr in c(1:100)) {
  women <- women %>%
    mutate(lx = if_else(age == yr, lag(lx) - lag(dx), lx), 
           dx = if_else(age == yr, qx*lx, dx))
}


# calculate Lx
women <- women %>%
  mutate(Lx = if_else(age == 0, ax*lx + (1-ax)*lead(lx), NA_real_))

for (yr in c(1:99)) {
  women <- women %>% 
    mutate(Lx = if_else(age == yr, ax*lx + (1-ax)*lead(lx), Lx))
}

women <- women %>%
  mutate(Lx = if_else(age == 100, ax*lx, Lx))


# calculate Tx
women <- women %>%
  group_by(years) %>%
  arrange(desc(age)) %>%
  mutate(Tx = cumsum(Lx))

women <- women %>%
  arrange(years, age)


# calculate Ex
women <- women %>%
  mutate(ex = Tx/lx)


# Calculate life disparity at age 0
women <- women %>%
  mutate(tmp=dx*ex)

women <- women %>%
  group_by(years) %>%
  mutate(ld0=sum(tmp)/100000)

# Calculate life disparity at age 10
women <- women %>%
  mutate(tmp=case_when(age>=10 ~ dx*ex))

women <- women %>%
  group_by(years) %>%
  mutate(l10 = lx[age == 10L])

women <- women %>%
  group_by(years) %>%
  mutate(ld10=sum(tmp, na.rm = TRUE)/l10)




# Keep only one line per year to export datasets
men <- men %>%
  filter(age == 0) %>%
  select(years, ex, ld10) %>%
  mutate(years= sub('.', '', years),
         years = paste0(c(substr(years, 1, 4), substr(years, 5,6)), collapse="-")) %>%
  rename(ex_men = ex, ld10_men = ld10)

  
women <- women %>%
  filter(age == 0) %>%
  select(years, ex, ld10) %>%
  mutate(years= sub('.', '', years),
         years = paste0(c(substr(years, 1, 4), substr(years, 5,6)), collapse="-")) %>%
  rename(ex_women = ex, ld10_women = ld10)

proj_combined <- left_join(women, men, by = "years")

proj_combined <- proj_combined %>%
  pivot_longer(
    cols = starts_with("ex_"),
    names_to = "sex",
    names_prefix = "wk",
    values_to = "e0",
    values_drop_na = TRUE
  )

proj_combined <- proj_combined %>%
  mutate(sex = substr(sex, 4, 9)) %>%
  mutate( ld10_women = case_when(
    sex == 'men' ~ NA_real_, 
    TRUE ~ ld10_women
  )) %>%
  mutate(ld10_men = case_when(
    sex == 'women' ~NA_real_, 
    TRUE ~ ld10_men))

# Save datasets
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy' ## my bucket name

s3write_using(proj_combined # What R object we are saving
              , FUN = write_rds # Which R function we are using to save
              , object = 'proj_combined.RDS' # Name of the file to save to (include file type)
              , bucket = buck) # Bucket name defined above



