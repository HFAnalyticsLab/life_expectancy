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




# Load packages and create vector for codes
pacman::p_load(hrbrthemes, viridis)

codes <- unique(results$code)

# 1 - Check correlation between different measures of lifespan variation

ld0_ld10 <- ggplot(subset(results, substr(results$Year, 1, 4) == '2015'), aes(x=ld0, y=ld10)) +
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
cor(subset(results,substr(results$Year, 1, 4) == '2015')$ld0, subset(results,substr(results$Year, 1, 4) == '2015')$ld10)


sd0_sd10 <- ggplot(subset(results, substr(results$Year, 1, 4) == '2015'), aes(x=sd0, y=sd10)) +
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
cor(subset(results,substr(results$Year, 1, 4) == '2015')$sd0, subset(results,substr(results$Year, 1, 4) == '2015')$sd10)



sd0_ld0 <- ggplot(subset(results, substr(results$Year, 1, 4) == '2015'), aes(x=sd0, y=ld0)) +
  geom_point(alpha=0.5, color='#dd0031') +
  labs(caption = "correlation = 0.96") +
  ylab("Life disparity at age 0") +
  xlab("Standard deviation in age at death at age 0") +
  scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
  theme(legend.position = "none") +
  theme_light() +
  scale_x_continuous(limits = c(12, 18)) +
  scale_y_continuous(limits = c(9, 13)) +
  geom_smooth(method = 'lm') +
  geom_text( 
    label = codes,
    nudge_y = -0.1, 
    check_overlap = T,
    size = 2.5)
sd0_ld0 
cor(subset(results,substr(results$Year, 1, 4) == '2015')$sd0, subset(results,substr(results$Year, 1, 4) == '2015')$ld0)


sd0_gini <- ggplot(subset(results, substr(results$Year, 1, 4) == '2015'), aes(x=sd0, y=gini)) +
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
cor(subset(results,substr(results$Year, 1, 4) == '2015')$sd0, subset(results,substr(results$Year, 1, 4) == '2015')$gini)




# 2 - Bubble plot all countries 2015-19

# life disparity at age 0
bubble_countries_ld0 <- ggplot(subset(results, substr(results$Year, 1, 4) == '2015'), aes(x=e0, y=ld0, size = popsize)) +
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
bubble_countries_ld0
# save graph
ggsave("bubble_countries_2015.tiff")
put_object(
  file = 'bubble_countries_2015.tiff', 
  object = 'outputs/bubble_countries_2015_lifedisparity0.tiff',
  bucket = buck) 
unlink("bubble_countries_2015.tiff")


# life disparity at age 10
bubble_countries_ld10 <- ggplot(subset(results, substr(results$Year, 1, 4) == '2015'), aes(x=e0, y=ld10, size = popsize)) +
  geom_point(alpha=0.5, color='#dd0031') +
  scale_size(range = c(.1, 16), name = "Population (2015)") +
  ylab("Life disparity at age 10") +
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
bubble_countries_ld10
# save graph
ggsave("bubble_countries_2015.tiff")
put_object(
  file = 'bubble_countries_2015.tiff', 
  object = 'outputs/bubble_countries_2015_lifedisparity10.tiff',
  bucket = buck) 
unlink("bubble_countries_2015.tiff")


# standard deviation in age at death (age 0)
bubble_countries_sd0 <- ggplot(subset(results, substr(results$Year, 1, 4) == '2015'), aes(x=e0, y=sd0, size = popsize)) +
  geom_point(alpha=0.5, color='#dd0031') +
  scale_size(range = c(.1, 16), name = "Population (2015)") +
  ylab("Standard deviation in age at death (age 0)") +
  xlab("Life expectancy at birth") +
  scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
  theme(legend.position = "none") +
  theme_light() +
  scale_x_continuous(limits = c(72, 87)) +
  scale_y_continuous(limits = c(12, 18)) +
  geom_text( 
    label = codes,
    nudge_x = 0.5, 
    check_overlap = T,
    size = 2.5)
# option for removing overlapping labels  check_overlap = T, 
bubble_countries_sd0
# save graph
ggsave("bubble_countries_2015.tiff")
put_object(
  file = 'bubble_countries_2015.tiff', 
  object = 'outputs/bubble_countries_2015_standarddev0.tiff',
  bucket = buck) 
unlink("bubble_countries_2015.tiff")


# standard deviation in age at death (age 10)
bubble_countries_sd10 <- ggplot(subset(results, substr(results$Year, 1, 4) == '2015'), aes(x=e0, y=sd10, size = popsize)) +
  geom_point(alpha=0.5, color='#dd0031') +
  scale_size(range = c(.1, 16), name = "Population (2015)") +
  ylab("Standard deviation in age at death (age 10)") +
  xlab("Life expectancy at birth") +
  scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
  theme(legend.position = "none") +
  theme_light() +
  scale_x_continuous(limits = c(72, 87)) +
  scale_y_continuous(limits = c(12, 18)) +
  geom_text( 
    label = codes,
    nudge_x = 0.5, 
    check_overlap = T,
    size = 2.5)
# option for removing overlapping labels  check_overlap = T, 
bubble_countries_sd10
# save graph
ggsave("bubble_countries_2015.tiff")
put_object(
  file = 'bubble_countries_2015.tiff', 
  object = 'outputs/bubble_countries_2015_standarddev10.tiff',
  bucket = buck) 
unlink("bubble_countries_2015.tiff")


# gini coefficient (all ages)
bubble_countries_gini <- ggplot(subset(results, substr(results$Year, 1, 4) == '2015'), aes(x=e0, y=gini, size = popsize)) +
  geom_point(alpha=0.5, color='#dd0031') +
  scale_size(range = c(.1, 16), name = "Population (2015)") +
  ylab("Gini coefficient - age at death (all ages)") +
  xlab("Life expectancy at birth") +
  scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
  theme(legend.position = "none") +
  theme_light() +
  scale_x_continuous(limits = c(72, 87)) +
  scale_y_continuous(limits = c(0.06, 0.12)) +
  geom_text( 
    label = codes,
    nudge_x = 0.5, 
    check_overlap = T,
    size = 2.5)
# option for removing overlapping labels  check_overlap = T, 
bubble_countries_gini
# save graph
ggsave("bubble_countries_2015.tiff")
put_object(
  file = 'bubble_countries_2015.tiff', 
  object = 'outputs/bubble_countries_2015_gini.tiff',
  bucket = buck) 
unlink("bubble_countries_2015.tiff")


# 3 - UK bubble plot
years <- unique(subset(results,country == 'UK' & !is.na(results$Year))$Year)

uk_bubble <- ggplot(subset(results, country == 'UK'), aes(x=e0, y=ld0)) +
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

# Save graph 
ggsave("uk_bubble_overtime.tiff")
put_object(
  file = 'uk_bubble_overtime.tiff', 
  object = 'outputs/uk_bubble_overtime.tiff',
  bucket = buck) 
unlink("uk_bubble_overtime.tiff")



# 4 - overlay both bubble plots
results <- results %>%
  mutate(group = case_when(
    country == 'UK' ~ 1,
    country != 'UK' & substr(results$Year, 1, 4) == '2015' ~ 0))

bubble_overlay <- ggplot(subset(results, (country == 'UK') | (substr(results$Year, 1, 4) == '2015')), aes(x=e0, y=ld0, size=popsize, color = factor(group), group = group)) +
  geom_point(alpha=0.5, aes(color=factor(group))) +
  scale_size(range = c(.1, 16), name = "Population (2015)", breaks = c(1000000, 10000000, 100000000), labels = c("1,000,000", "10,000,000", "100,000,000")) +
  scale_color_manual(values = c('#dd0031', '#00AFBB'), name= "", labels=c("All countries - 2015", "UK - historical")) +
  scale_fill_viridis(discrete=TRUE, guide= "none", option="A") +
  theme(legend.position = "none") +
  theme_light() +
  ylab("Life disparity") +
  xlab("Life expectancy at birth")
bubble_overlay

# Save graph 
ggsave("bubble_overlay")
put_object(
  file = 'bubble_overlay', 
  object = 'outputs/bubble_overlay',
  bucket = buck) 
unlink("bubble_overlay")


# above works, now trying to add in labels
  # not succeeded so far 
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



