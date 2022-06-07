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


# Check correlation between different measures of lifespan variation

ld0_ld10 <- ggplot(results, aes(x=ld0, y=ld10)) +
  geom_point(alpha=0.5, color='#dd0031') +
  labs(caption = "correlation = 0.996") +
  ylab("Life disparity at age 10") +
  xlab("Life disparity at age 0") +
  scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
  theme(legend.position = "none") +
  theme_light() +
  scale_x_continuous(limits = c(9, 13)) +
  scale_y_continuous(limits = c(9, 13)) +
  geom_abline(intercept = 0, slope = 1 ) +
  geom_text( 
    label = codes,
    nudge_y = -0.1, 
    check_overlap = T,
    size = 2.5)
ld0_ld10 
cor(results$ld0, results$ld10)

sd0_sd10 <- ggplot(results, aes(x=sd0, y=sd10)) +
  geom_point(alpha=0.5, color='#dd0031') +
  labs(caption = "correlation = 0.97") +
  ylab("Standard deviation in age at death at age 10") +
  xlab("Standard deviation in age at death at age 0") +
  scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
  theme(legend.position = "none") +
  theme_light() +
  scale_x_continuous(limits = c(10, 16)) +
  scale_y_continuous(limits = c(10, 16)) +
  geom_abline(intercept = 0, slope = 1 ) +
  geom_text( 
    label = codes,
    nudge_y = -0.1, 
    check_overlap = T,
    size = 2.5) 
sd0_sd10 
cor(results$sd0, results$sd10)


sd0_ld0 <- ggplot(results, aes(x=sd0, y=ld0)) +
  geom_point(alpha=0.5, color='#dd0031') +
  labs(caption = "correlation = 0.96") +
  ylab("Life disparity at age 0") +
  xlab("Standard deviation in age at death at age 0") +
  scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
  theme(legend.position = "none") +
  theme_light() +
  scale_x_continuous(limits = c(10, 18)) +
  scale_y_continuous(limits = c(6, 16)) +
  geom_smooth(method = 'lm') +
  geom_text( 
    label = codes,
    nudge_y = -0.1, 
    check_overlap = T,
    size = 2.5)
sd0_ld0 
cor(results$sd0, results$ld0)


sd0_gini <- ggplot(results, aes(x=sd0, y=gini)) +
  geom_point(alpha=0.5, color='#dd0031') +
  labs(caption = "correlation = 0.94") +
  ylab("Gini coefficient (all ages)") +
  xlab("Standard deviation in age at death at age 0") +
  scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
  theme(legend.position = "none") +
  theme_light() +
  scale_x_continuous(limits = c(12, 18)) +
  scale_y_continuous(limits = c(0.05, 0.15)) +
  geom_smooth(method = 'lm') +
  geom_text( 
    label = codes,
    nudge_y = -0.1, 
    check_overlap = T,
    size = 2.5)
sd0_gini
cor(results$sd0, results$gini)




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



