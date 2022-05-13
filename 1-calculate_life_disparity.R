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
countries <- c("australia", "austria", "belarus", "belgium", "bulgaria", "canada", 
               "chile", "croatia", "czechia", "denmark", "estonia", "finland", "france", 
               "germany", "greece", "hongkong", "hungary", "iceland", "ireland",
               "israel", "italy", "japan", "korea", "latvia", "lithuania", "luxembourg",
               "netherlands", "norway", "poland", "portugal", "slovakia", "slovenia",
               "spain", "sweden", "switzerland", "taiwan", "uk", "usa")
  # new zealand, russia and ukraine data available but only for 2010-13
  # removed taiwan until get population size
  

# create empty list to store results
results <- vector(mode = "list", length = length(countries))


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


# Calculate life disparity
countrydta <- countrydta %>%
  mutate(tmp=dx*ex)

countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(ldsp=sum(tmp)/100000)



# Extract life expectancy and life disparity and add to list
e0 <- countrydta$ex[substr(countrydta$Year, 1, 4) == '2015' & countrydta$Age == 0]
e0
ld <- countrydta$ldsp[substr(countrydta$Year, 1, 4) == '2015' & countrydta$Age == 0]
ld

df <- data.frame(e0, ld, country)

results <- c(results, list(df))
}



# bind all dataframes together
results <- bind_rows(results)

results <- results %>%
  mutate(country = str_to_title(country))


# Change name for USA/UK
results <- results %>%
  mutate(country = replace(country, country == "Uk", "UK"),
        country = replace(country, country == "Usa", "USA"),
        country = replace(country, country == "Hongkong", "Hong Kong"))


nrow(results)



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


# Bubble plot
pacman::p_load(hrbrthemes, viridis)

codes <- results$code

bubble_countries <- ggplot(results, aes(x=e0, y=ld, size = popsize)) +
  geom_point(alpha=0.5, color='#dd0031') +
  scale_size(range = c(.1, 16), name = "Population (2015)") +
  ylab("Life disparity") +
  xlab("Life expectancy at birth") +
  scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
  theme(legend.position = "none") +
  theme_light() +
  scale_x_continuous(limits = c(72, 87)) +
  scale_y_continuous(limits = c(9, 13)) +
  geom_text( 
    label = codes,
    nudge_x = 0.5, 
    check_overlap = T,
    size = 2.5)
 # option for removing overlapping labels  check_overlap = T, 
bubble_countries


# below command doesn't work - saved manually for now
aws.s3::s3write_using(bubble_countries # What R object we are saving
              , FUN = ggsave # Which R function we are using to save
              , object = 'Francesca/life_expectancy/life_disparity_countries.png' # Name of the file to save to (include file type)
              , bucket = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/' # Bucket name defined above
              , device = NULL)






# UK bubble chart over time
country <- "uk"
countrydta <- s3read_using(read_excel
                           , object = paste0('s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/lifetable_', country, '_total.xlsx') 
                           , skip = 2)


# drop last three columns
countrydta <- countrydta %>% 
  dplyr::select(Year:ex)


# Calculate life disparity
countrydta <- countrydta %>%
  mutate(tmp=dx*ex)

countrydta <- countrydta %>%
  group_by(Year) %>%
  mutate(ldsp=sum(tmp)/100000)

countrydta <- countrydta %>%
  dplyr::filter(Age == 0)


# Rename to match names in results df
countrydta <- countrydta %>% 
  rename(e0 = ex, 
         ld = ldsp)


# Append uk data to countrydta
countrydta <- countrydta %>% 
  dplyr::select(Year, e0, ld)

countrydta <- countrydta %>%
  mutate(country = "UK hist")


combined <- bind_rows(results, countrydta)

combined <- combined %>%
  mutate(group=country == "UK hist",
         popsize = replace_na(popsize, 3000000))


# Bubble plot
years <- countrydta$Year

uk_bubble <- ggplot(countrydta, aes(x=e0, y=ld)) +
  geom_point(alpha=0.5, color='#dd0031') +
  ylab("Life disparity") +
  xlab("Life expectancy at birth") +
  scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
  theme(legend.position = "none") +
  theme_light() +
  scale_x_continuous(limits = c(55, 87)) +
  scale_y_continuous(limits = c(9, 20)) +
  geom_text( 
    label = years,
    nudge_x = 1.5, 
    check_overlap = T,
    size = 2.1)
uk_bubble


# doesn't work either 
s3save_image(ggsave(uk_bubble
                    , 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy')
                    , device = NULL)



# overlay both bubble plots

bubble_overlay <- ggplot(combined, aes(x=e0, y=ld, size=popsize, color = group, group = group)) +
  geom_point(alpha=0.5, aes(color=group)) +
  scale_size(range = c(.1, 16), name = "Population (2015)", breaks = c(1000000, 10000000, 100000000), labels = c("1,000,000", "10,000,000", "100,000,000")) +
  scale_color_manual(values = c('#dd0031', '#00AFBB'), name= "", labels=c("All countries - 2015", "UK - historical")) +
  scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
  theme(legend.position = "none") +
  theme_light() +
  ylab("Life disparity") +
  xlab("Life expectancy at birth")
bubble_overlay


# above works, now trying to add in labels
codes <- combined$codes

bubble_overlay <- ggplot(combined, aes(x=e0, y=ld, size=popsize, color = group, group = group)) +
  geom_point(alpha=0.5, aes(color=group)) +
  geom_text(subset(combined, group == "FALSE"), 
    label = codes,
    nudge_x = 0.5, 
    check_overlap = T,
    size = 2.5) +
  scale_size(range = c(.1, 16), name = "Population (2015)", breaks = c(1000000, 10000000, 100000000), labels = c("1,000,000", "10,000,000", "100,000,000")) +
  scale_color_manual(values = c('#dd0031', '#00AFBB'), name= "", labels=c("All countries - 2015", "UK - historical")) +
  scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
  theme(legend.position = "none") +
  theme_light() +
  ylab("Life disparity") +
  xlab("Life expectancy at birth") 
bubble_overlay
# option for removing overlapping labels  check_overlap = T, 





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