## Plots 

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


# Load data
buck <- 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy' ## my bucket name
results <- s3read_using(import # Which function are we using to read
                        , object = 'results.RDS' # File to open
                        , bucket = buck) # Bucket name defined above




# Bubble plot
pacman::p_load(hrbrthemes, viridis)

codes <- results$code

bubble_countries <- ggplot(results, aes(x=e0, y=ld0, size = popsize)) +
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
                      , bucket = 'buck' # Bucket name defined above
                      , device = NULL)



# UK bubble plot
combined <- s3read_using(import # Which function are we using to read
                        , object = 'combined.RDS' # File to open
                        , bucket = buck) # Bucket name defined above

years <- unique(combined$Year[!is.na(combined$Year)])

uk_bubble <- ggplot(subset(combined, country == 'UK hist'), aes(x=e0, y=ld0)) +
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

bubble_overlay <- ggplot(combined, aes(x=e0, y=ld0, size=popsize, color = group, group = group)) +
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



