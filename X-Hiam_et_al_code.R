---
#  title: 'Age Specific Mortality Rate Trends '
# output:  word_document: default
#html_document:
#  df_print: paged
---
  
  # Aim 
  
#  This document contains a number of analyses presented both in the main paper, "What can lifespan variation reveal that life expectancy hides? Comparison of five high-income countries", as well as supplementary analyses which make use of the same data. 

# Countries of interest

#* Japan
#* UK
#* USA
#* France
#* Canada


# Dataset 

#Data were extracted from the Human Mortality Database (HMD). 

# Key

#The analyses in this markdown document are not performed in the same order as they are presented in the main manuscript. The following key distinguishes analyses presented in the main manuscript from supplementary analyses only available in this document. 

#* **M**: Main - indicates that the result of the analysis is included in the main manuscript.
#* **W**: Workings - indicates that the analyses is part of the workings required for other results to be produced
#* **S**: Supplementary - indicates that the analyses are not included in the main manuscript

#**Note**: The last section of the analyses involves calculation of the R-squared value of age-specific log Mx trends, and comparisons between Japan and the UK over periods of economic recession. All these analyses are supplementary, and included in case of interest to the reader. 


# Prerequisities 

#The following R code chunk loads packages used in the analyses, and data extracted and processed from the HMD. 

```{r}
pacman::p_load(
  tidyverse, ggrepel, here,
  corrr, knitr, kableExtra, openxlsx,
  MortalityLaws, patchwork
)
#dta_e0 <- read_rds(here("data", "e0.rds"      ))
dta_e0 <- s3read_using(import
                       , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/dta_e0.RDS' )

#dta_Mx <- read_rds(here("data", "Mx.rds"      ))
dta_Mx <- s3read_using(import
                               , object = 's3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp/Francesca/life_expectancy/data/dta_Mx.RDS' )

#source(here("scripts", "country_definitions.R"))

```


## Functions

#The following chunk is a convenience function for writing data to a csv file while continuing the execution of a chain of instructions. 

```{r}
write_to_table <- function(x, location){
  x %>% write_csv(location)
  x
}
```

#The following function calculates the components of lifespan disparity associated with each individual age x. 

```{r}
calc_e_dagger_parts <- function(LT, omega = 100){
  calc_ell_a <- function(LT, a){
    exp(-sum(LT$mx[LT$x <= a]))
  }
  LT %>% 
    dplyr::filter(x <= omega) %>% 
    mutate(ell_x = map_dbl(x, calc_ell_a, LT = LT)) %>% 
    mutate(e_dagger_component = ell_x * ex * mx) %>% 
    dplyr::select(x, e_dagger_component)
}
```

# Analyses 

## e0 - Life expectancy at birth (**W**)

#The following code plots life expectancy from birth over time for the five selected countries. This forms the top row of figure 3 in the main manuscript. 

```{r}
e0_plot <- 
  dta_e0 %>% 
  left_join(country_labels) %>% 
  dplyr::filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
  rename(Country = label) %>% 
  dplyr::filter(year >= 1975) %>% 
  dplyr::filter(sex != "total") %>% 
  #write_to_table(here("tables", "e0_for_select_countries.csv")) %>% 
  ggplot(aes(x = year, y = e0, group = Country, colour = Country, linetype = Country)) + 
  geom_line() + 
  facet_wrap(~sex) +
  labs(
    x = "Year", y = "Life expectancy at birth",
    title = "Life expectancy at birth for selected countries",
    caption = "Source: Human Mortality Database"
  ) + 
  theme_minimal() 
e0_plot
ggsave(here("figures", "00_e0.png"), height = 15, width = 20, units = "cm", dpi = 300)
ggsave(here("figures", "00_e0.svg"), height = 15, width = 20, units = "cm", dpi = 300)
```

## Annual change in life expectancy (**S**)

#The following shows annual variation in e0 in the selected countries. 

```{r}
dta_e0 %>% 
  left_join(country_labels) %>% 
  dplyr::filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
  rename(Country = label) %>% 
  group_by(Country, sex) %>% 
  arrange(year) %>% 
  mutate(ch_e0 = e0 - lag(e0)) %>% 
  ungroup() %>% 
  dplyr::filter(year >= 1975) %>% 
  dplyr::filter(sex != "total") %>% 
  #write_to_table(here("tables", "annual_ch_e0_selected_countries.csv")) %>% 
  ggplot(aes(x = year, y = ch_e0)) + 
  geom_point() + stat_smooth() + 
  facet_grid(Country~sex) +
  labs(
    x = "Year", y = "Change in Life expectancy at birth",
    title = "Annual changes in Life expectancy at birth for selected countries",
    subtitle = "Lines show smoothed trend",
    caption = "Source: Human Mortality Database"
  ) +
  geom_hline(yintercept = 0) +
  theme_minimal() 
#ggsave(here("figures", "01_ch_e0.png"), height = 20, width = 15, units = "cm", dpi = 300)
#ggsave(here("figures", "01_ch_e0.svg"), height = 20, width = 15, units = "cm", dpi = 300)
```

# Illustrative figure - Japan (**M**: Figure 1b)

      # NOTE: this is the problem, "unused arguments in select"
```{r}
e_dagger_parts_jpn <-
  dta_Mx %>%
  left_join(country_labels) %>%
  dplyr::filter(label %in% c("Japan")) %>%
  dplyr::filter(year %in% c(1947, 1975, 2017)) %>%
  rename(Country = label) %>%
  dplyr::select(Country, sex, year, age, Mx) %>%
  group_by(Country, sex, year) %>%
  nest() %>%
  mutate(lifetable = map(data, ~LifeTable(x = .x$age, mx = .x$Mx)$lt)) %>%
  mutate(e_dagger_parts = map(lifetable, calc_e_dagger_parts))
# 
 e_daggers_jpn <- 
   e_dagger_parts_jpn %>% 
   mutate(e_dagger       = map_dbl(e_dagger_parts, ~sum(.x$e_dagger_component))) %>% 
   dplyr::select(Country, sex, year, e_dagger)
# 
# e_daggers_jpn %>% write_csv("clipboard")
# Lifetable alone 
jpn_lt <- 
  dta_Mx %>% 
  left_join(country_labels) %>% 
  filter(label %in% c("Japan")) %>%
  filter(year %in% c(1947, 1975, 2017)) %>% 
  rename(Country = label) %>% 
  dplyr::select(Country, sex, year, age, Mx) %>% 
  group_by(Country, sex, year) %>% 
  nest() %>% 
  mutate(lifetable = map(data, ~LifeTable(x = .x$age, mx = .x$Mx)$lt)) %>% 
  dplyr::select(-data) %>% 
  unnest(lifetable)  
```

```{r}
p1 <- 
  jpn_lt %>% 
  dplyr::filter(sex == "total") %>% 
  ggplot(aes(x = x, y = lx, linetype = factor(year), group = factor(year), colour = factor(year))) + 
  geom_line() + 
  scale_linetype_manual("Year", values = c("dotdash", "dashed", "solid")) + 
  theme_minimal()  + 
  labs(
    title = "Period survival by age",
    x = "Age in years", 
    y = "Proportion (out of 100 000) surviving to given age",
    colour = "Year"
    
  )
 e_dagger_parts_jpn <-
   e_dagger_parts_jpn %>% 
   dplyr::select(Country, sex, year, e_dagger_parts) %>% 
   unnest(e_dagger_parts) %>% 
   ungroup()
 
# e_dagger_parts_jpn %>% write_csv("clipboard")
# ggsave(plot = p1, filename = here("figures", "fig_4_jpn_pt1.svg"))
```

```{r}
p2 <- 
  e_dagger_parts_jpn %>% 
  filter(sex == "total") %>% 
  dplyr::select(year, e_dagger_parts) %>% 
  unnest(e_dagger_parts) %>% 
  dplyr::filter(x <= 10) %>% 
  ggplot(aes(x = x, y = e_dagger_component, group = factor(year), linetype = factor(year), colour = factor(year))) + 
  scale_x_continuous(breaks = 0:9) + 
  scale_y_continuous(limits = c(0, 5)) + 
  geom_line() + 
  theme_minimal() +
  scale_linetype_manual("Year", values = c("dotdash", "dashed", "solid")) +
  labs(
    title = "Life disparity (Infancy/early Childhood)",
    x = "Age in years",
    y = "Contribution to life disparity",
    colour = "Year"
  ) + 
  theme(legend.position = "none")
p3 <- 
  e_dagger_parts_jpn %>% 
  filter(sex == "total") %>% 
  select(year, e_dagger_parts) %>% 
  unnest(e_dagger_parts) %>% 
  filter(x >= 10) %>% 
  ggplot(aes(x = x, y = e_dagger_component, group = factor(year), linetype = factor(year), colour = factor(year))) + 
  geom_line() + 
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.3)) + 
  scale_linetype_manual("Year", values = c("dotdash", "dashed", "solid")) +
  labs(
    title = "Life disparity (Adulthood)",
    x = "Age in years",
    y = NULL,
    colour = "Year"
  ) + 
  theme(legend.position = "none")
#ggsave(plot = p2, filename = here("figures", "fig_4_jpn_pt2.svg"))
#ggsave(plot = p3, filename = here("figures", "fig_4_jpn_pt3.svg"))
```

Now to combine using patchwork

```{r}
p1 / (p2 + p3) + plot_layout(guide = "collect") +
  plot_annotation(
    title = 'Changing mortality survivorship curve and life disparity contributions in Japan',
    caption = 'Source: Human Mortality Database'
  )
ggsave(here("figures", "figure_04_japan_intro.png"), height = 30, width = 30, units = "cm", dpi = 300)
ggsave(here("figures", "figure_04_japan_intro.svg"), height = 30, width = 30, units = "cm", dpi = 300)
```
## Illustrative figure for USA (**M**: Figure 1a)


```{r}
e_dagger_parts_usa <-
  dta_Mx %>%
  left_join(country_labels) %>%
  filter(label %in% c("USA")) %>%
  filter(year %in% c(1947, 1975, 2017)) %>%
  rename(Country = label) %>%
  select(Country, sex, year, age, Mx) %>%
  group_by(Country, sex, year) %>%
  nest() %>%
  mutate(lifetable = map(data, ~LifeTable(x = .x$age, mx = .x$Mx)$lt)) %>%
  mutate(e_dagger_parts = map(lifetable, calc_e_dagger_parts))
# 
# e_daggers_jpn <- 
#   e_dagger_parts_jpn %>% 
#   mutate(e_dagger       = map_dbl(e_dagger_parts, ~sum(.x$e_dagger_component))) %>% 
#   select(Country, sex, year, e_dagger)
# 
# e_daggers_jpn %>% write_csv("clipboard")
# Lifetable alone 
usa_lt <- 
  dta_Mx %>% 
  left_join(country_labels) %>% 
  filter(label %in% c("USA")) %>%
  filter(year %in% c(1947, 1975, 2017)) %>% 
  rename(Country = label) %>% 
  select(Country, sex, year, age, Mx) %>% 
  group_by(Country, sex, year) %>% 
  nest() %>% 
  mutate(lifetable = map(data, ~LifeTable(x = .x$age, mx = .x$Mx)$lt)) %>% 
  select(-data) %>% 
  unnest(lifetable)  
```

```{r}
p1 <- 
  usa_lt %>% 
  filter(sex == "total") %>% 
  ggplot(aes(x = x, y = lx, linetype = factor(year), group = factor(year), colour = factor(year))) + 
  geom_line() + 
  scale_linetype_manual("Year", values = c("dotdash", "dashed", "solid")) + 
  theme_minimal()  + 
  labs(
    title = "Period survival by age",
    x = "Age in years", 
    y = "Proportion (out of 100 000) surviving to given age",
    colour = "Year"
    
  )
# e_dagger_parts_jpn <-
#   e_dagger_parts_jpn %>% 
#   select(Country, sex, year, e_dagger_parts) %>% 
#   unnest(e_dagger_parts) %>% 
#   ungroup()
# 
# e_dagger_parts_jpn %>% write_csv("clipboard")
# ggsave(plot = p1, filename = here("figures", "fig_4_jpn_pt1.svg"))
```

```{r}
p2 <- 
  e_dagger_parts_usa %>% 
  filter(sex == "total") %>% 
  select(year, e_dagger_parts) %>% 
  unnest(e_dagger_parts) %>% 
  filter(x <= 10) %>% 
  ggplot(aes(x = x, y = e_dagger_component, group = factor(year), linetype = factor(year), colour = factor(year))) + 
  scale_x_continuous(breaks = 0:9) + 
  scale_y_continuous(limits = c(0, 5)) + 
  geom_line() + 
  theme_minimal() +
  scale_linetype_manual("Year", values = c("dotdash", "dashed", "solid")) +
  labs(
    title = "Life disparity (Infancy/early Childhood)",
    x = "Age in years",
    y = "Contribution to life disparity",
    colour = "Year"
  ) + 
  theme(legend.position = "none")
p3 <- 
  e_dagger_parts_usa %>% 
  filter(sex == "total") %>% 
  select(year, e_dagger_parts) %>% 
  unnest(e_dagger_parts) %>% 
  filter(x >= 10) %>% 
  ggplot(aes(x = x, y = e_dagger_component, group = factor(year), linetype = factor(year), colour = factor(year))) + 
  geom_line() + 
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.3)) + 
  scale_linetype_manual("Year", values = c("dotdash", "dashed", "solid")) +
  labs(
    title = "Life disparity (Adulthood)",
    x = "Age in years",
    y = NULL,
    colour = "Year"
  ) + 
  theme(legend.position = "none")
#ggsave(plot = p2, filename = here("figures", "fig_4_jpn_pt2.svg"))
#ggsave(plot = p3, filename = here("figures", "fig_4_jpn_pt3.svg"))
```

Now to combine using patchwork

```{r}
p1 / (p2 + p3) + plot_layout(guide = "collect") +
  plot_annotation(
    title = 'Changing mortality survivorship curve and life disparity contributions in USA',
    caption = 'Source: Human Mortality Database'
  )
ggsave(here("figures", "figure_04a_usa_intro.png"), height = 30, width = 30, units = "cm", dpi = 300)
ggsave(here("figures", "figure_04a_usa_intro.svg"), height = 30, width = 30, units = "cm", dpi = 300)
```

## Faceted comparison (**S**)

This shows the same data presented in Figure 1a and Figure 1b, but in a way that makes direct comparisons between the USA and Japan in a particular period easier.

```{r}
e_dagger_parts_usa %>% 
  bind_rows(
    e_dagger_parts_jpn
  ) %>% 
  filter(sex == "total") %>% 
  select(Country, year, e_dagger_parts) %>% 
  unnest(cols = c(e_dagger_parts)) %>% 
  mutate(age_group = case_when(
    x <= 10 ~ "Earliest Ages",
    x > 10  ~ "Later Ages"
  )
  ) %>% 
  ggplot(aes(x = x, y = e_dagger_component, linetype = Country, colour = Country)) + 
  facet_grid(age_group ~ factor(year), scales = "free_y" ) + 
  geom_line()
# Instead should split then recombine with patchwork. 
# younger ages 
tmp <- e_dagger_parts_usa %>% 
  bind_rows(
    e_dagger_parts_jpn
  ) %>% 
  filter(sex == "total") %>% 
  select(Country, year, e_dagger_parts) %>% 
  unnest(cols = c(e_dagger_parts)) %>% 
  mutate(age_group = case_when(
    x <= 10 ~ "Earliest Ages",
    x > 10  ~ "Later Ages"
  )
  )
e_dagger_younger_parts <- tmp %>% 
  filter(age_group == "Earliest Ages")
e_dagger_older_parts <- tmp %>% 
  filter(age_group == "Later Ages")
p1 <- e_dagger_younger_parts %>% 
  ggplot(aes(x = x, y = e_dagger_component, linetype = Country, colour = Country)) + 
  facet_grid(. ~ factor(year)) + 
  geom_line() + 
  scale_x_continuous(breaks = 0:10) + 
  labs(x = "Age in years", y = "Contribution to life disparity in years")
p1
p2 <- e_dagger_older_parts %>% 
  ggplot(aes(x = x, y = e_dagger_component, linetype = Country, colour = Country)) + 
  facet_grid(. ~ factor(year)) + 
  geom_line() + 
  scale_x_continuous() + 
  labs(x = "Age in years", y = "Contribution to life disparity in years")
p2
(p1 / p2) + 
  plot_annotation(
    title = 'Age-specific contributions to life disparity, USA and Japan',
    subtitle = "Selected years: 1947, 1975 and 2017",
    caption = 'Source: Human Mortality Database'
  )
ggsave(here("figures", "figure_x_disp_compare_usa_jpn.png"), height = 20, width = 30, units = "cm", dpi = 300)
```



## Lifespan disparity (**W**)

Life disparity ($e^\dagger$), as defined in [Vaupel, Zhang & van Raalte (2011)](https://bmjopen.bmj.com/content/1/1/e000128), states that: 
  
  > Life disparity is defined as the average remaining life expectancy at the ages when death occurs
To calculate this we should first produce lifetables. We can check that, given $m_x$, the life expectancy $e_0$ is as extracted directly from the HMD above. Then we can calculate life disparity from the lifetable. We can also compare our estimates of $e_0$ and $e^\dagger$ with those in table 1 of the above (though it might not be for exactly the same year, and the data may have been adjusted since the 2011 HMD release). 

```{r, eval = FALSE}
#lifetable for one country and year 
# Japan, 2009 (most likely to be the year in table 1)
tmp <- 
  dta_Mx %>% 
  filter(code == "JPN", year == 2009, sex == "female")
lt_jpn_f <- 
  LifeTable(x = tmp$age, mx = tmp$Mx) # this matches table 1 value 
lt_jpn_f
tmp <- 
  dta_Mx %>% 
  filter(code == "JPN", year == 2009, sex == "male")
lt_jpn_m <- 
  LifeTable(x = tmp$age, mx = tmp$Mx) # this is within 0.1 years
lt_jpn_m
# So is e^dagger just the mean of the e_x columns? 
mean(lt_jpn_f$lt$ex)
# No, it's not. 
# The simplified formula is 
# sum(ex * fx)
# so
# sum(ex * lx * qx)
lt_jpn_f$lt$ex * lt_jpn_f$lt$lx * lt_jpn_f$lt$qx
# No, this isn't it either 
# First need to calculate probability of survival to age a from qx
# exp(-sum(qx))
# Let's do this for a few select ages 
# 0 , 20, 50, 80
plot(lt_jpn_f$lt$x, exp(-cumsum(lt_jpn_f$lt$qx)))
# This checks out 
# So this is a two part process
# 1)
# calculate ell_x for each given x 
# multiply ell_x by q_x (f_x)
# 2) 
# sum up product of e_x and f_x for all x up to \omega (last age)
# Let's start to write a function to do this given contents of an lt object 
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
ggplot(tmp, aes(x = x, y = e_dagger_component)) + 
  geom_line()
# 9.1, compared with 9.2 in table 1
tmp <- calc_e_dagger_parts(lt_jpn_m$lt)
tmp
sum(tmp$e_dagger_component)
ggplot(tmp, aes(x = x, y = e_dagger_component)) + 
  geom_line()
# 10.5, compared with 10.6 in table 1
```

Having established that this seems to be the right approach, let's calculate e_dagger over time for each population, sex and year

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

## Visualising e_daggers (**M**)

The following forms the bottom half of figure 2.

```{r}
e_dagger_series <- 
  e_daggers %>% 
    write_to_table(here("tables", "e_daggers.csv")) %>% 
    filter(sex != "total") %>% 
    ggplot(aes(x = year, y = e_dagger, group = Country, colour = Country, linetype = Country)) + 
    geom_line() + 
    facet_wrap(~sex) + 
    labs(x = "Year", y = "Life Disparity (years)",
         title = "Life disparity over time") +
    theme_minimal() 
e_dagger_series 
ggsave(here("figures", "life_disparity.png"), height = 15, width = 20, units = "cm", dpi = 300)
ggsave(here("figures", "life_disparity.svg"), height = 15, width = 20, units = "cm", dpi = 300)
```
## Figure 2 (**M**)

Now to combine life expectancy and life disparity using patchwork 

```{r}
e0_plot / e_dagger_series + plot_layout(guide = "collect") +
  plot_annotation(
    title = 'Life Expectancy at Birth, and Life Disparity, over time',
    caption = 'Source: Human Mortality Database'
)
ggsave(here("figures", "fig_2_4_combined_e0_disparity.png"), width = 30, height = 25, units = "cm", dpi = 300)
```

## Figure 3 (**M**)

Now a version focusing on 2000 and beyond 


```{r}
e_daggers %>% 
  filter(year >= 2000) %>% 
  filter(sex != "total") %>% 
  ggplot(aes(x = year, y = e_dagger, group = Country, colour = Country, linetype = Country)) + 
  geom_line() + 
  facet_wrap(~sex) + 
  labs(x = "Year", y = "Life Disparity (years)",
       title = "Life disparity since 2000") +
  theme_minimal() 
ggsave(here("figures", "life_disparity_since_2000.png"), height = 15, width = 20, units = "cm", dpi = 300)
ggsave(here("figures", "life_disparity_since_2000.svg"), height = 15, width = 20, units = "cm", dpi = 300)
```
## Change in e_0 and e_dagger over time (**S**)

Let's look at how $e_0$ and $e^\dagger$ compare over time. 

```{r}
e_daggers %>% 
  left_join(
    dta_e0 %>% 
      left_join(country_labels) %>% 
      filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
      rename(Country = label)
  ) %>% 
  filter(sex != "total") %>% 
  ggplot(aes(x = e_dagger, y = e0, colour = Country, group = Country)) +
  geom_path(aes(alpha = year)) +
  facet_wrap(. ~ sex) +
  labs(
    x = "Life disparity (years)", y = "Life expectancy at birth (years)",
    title = "Relationship between life disparity and life expectancy since 1975",
    subtitle = "Older years are more faded"
  ) +
  theme_minimal()
ggsave(here("figures", "e_dagger_e0_paths.png"), height = 12, width = 20, units = "cm", dpi = 300)
ggsave(here("figures", "e_dagger_e0_paths.svg"), height = 12, width = 20, units = "cm", dpi = 300)
```

## Contributions of age-specific components of life disparity over time, as a heatmap (**S**)

```{r}
e_dagger_parts %>%
  filter(sex != "total") %>%
  rename(age = x) %>% 
  ggplot(aes(x = year, y = age, fill = e_dagger_component)) +
  geom_tile() + 
  coord_equal() + 
  scale_fill_viridis_c() + 
  facet_grid(sex ~ Country) + 
  labs(
    x = "Year", y = "Age in years",
    title = "Age-specific contributions to life disparity by age and year",
    subtitle = "All ages"
  )
ggsave(here("figures", "e_dagger_components_all_ages.png"), height = 25, width = 30, units = "cm", dpi = 300)
ggsave(here("figures", "e_dagger_components_all_ages.svg"), height = 25, width = 30, units = "cm", dpi = 300)
```
## Heatmap focused on ages from 15 years old and over (**S**)

As the historic contribution in infancy was so high, let's look at the same but from age 15 onwards. 

```{r}
e_dagger_parts %>%
  filter(sex != "total") %>%
  rename(age = x) %>%
  filter(age >= 15) %>% 
  ggplot(aes(x = year, y = age, fill = e_dagger_component)) +
  geom_tile() + 
  coord_equal() + 
  scale_fill_viridis_c() + 
  facet_grid(sex ~ Country) + 
  labs(
    x = "Year", y = "Age in years",
    title = "Age-specific contributions to life disparity by age and year",
    subtitle = "Ages from 15 years onwards"
  )
ggsave(here("figures", "e_dagger_components_from_15.png"), height = 25, width = 30, units = "cm", dpi = 300)
ggsave(here("figures", "e_dagger_components_from_15.svg"), height = 25, width = 30, units = "cm", dpi = 300)
```

For females in France and Japan, life disparities are lowest, and the age components are concentrated around a relatively narrow age band. For males in Japan, life disparities are also low, and there are indications they improved for cohorts born after around 1930 (i.e. who were in their 60s in 1990).

An age band of higher disparity contributions is visible from around age 18, for males. This was historically much larger in France than it currently is, along with Canada to a lesser extent. It is more evident in the UK than for Japan, and is most elevated and persistent in the USA. 

In the USA the age contributions to disparity are wider/evident at more ages than in the other countries, and have become slightly more spread out. There is a distinct feature for males aged between around 25 and 50 which disappeared rapidly in the mid/late 1990s. A less pronounced varient of this feature is also apparent for males in France. 

## Cumulative sum of e_dagger components up to various different ages (**S**)

Let's now present the above results in a slightly different way, there are a few options. Let's start by doing a cumulative sum plot

```{r}
e_dagger_parts %>% 
  group_by(Country, sex, year) %>% 
  arrange(x) %>% 
  mutate(cumprop_dagger = cumsum(e_dagger_component) / sum(e_dagger_component)) %>% 
  ggplot(aes(x = x, y = cumprop_dagger, colour = year, group = year)) +
  geom_line() + 
  facet_grid(Country ~ sex) +
  scale_colour_viridis_c() +
  labs(x = "Age in years", y = "Cumulative share of e_dagger components",
       title = "Age specific contributions to cumulative share of e_dagger over time")
ggsave(here("figures", "e_dagger_cumulative_shares.png"), height = 20, width = 25, units = "cm", dpi = 300)
```

## Additional request: life disparity for Japan, 1947 and 2017 (**W**)

```{r}
e_dagger_parts_jpn <- 
  dta_Mx %>% 
    left_join(country_labels) %>% 
    filter(label %in% c("Japan")) %>%
    filter(year %in% c(1947, 2017)) %>% 
    rename(Country = label) %>% 
    select(Country, sex, year, age, Mx) %>% 
    group_by(Country, sex, year) %>% 
    nest() %>% 
    mutate(lifetable = map(data, ~LifeTable(x = .x$age, mx = .x$Mx)$lt)) %>% 
    mutate(e_dagger_parts = map(lifetable, calc_e_dagger_parts))
e_daggers_jpn <- 
  e_dagger_parts_jpn %>% 
  mutate(e_dagger       = map_dbl(e_dagger_parts, ~sum(.x$e_dagger_component))) %>% 
  select(Country, sex, year, e_dagger)
e_daggers_jpn %>% write_csv("clipboard")
# Lifetable alone 
dta_Mx %>% 
  left_join(country_labels) %>% 
  filter(label %in% c("Japan")) %>%
  filter(year %in% c(1947, 2017)) %>% 
  rename(Country = label) %>% 
  select(Country, sex, year, age, Mx) %>% 
  group_by(Country, sex, year) %>% 
  nest() %>% 
  mutate(lifetable = map(data, ~LifeTable(x = .x$age, mx = .x$Mx)$lt)) %>% 
  select(-data) %>% 
  unnest(lifetable) %>% write_csv(here("tables", "jpn_lifetable.csv"))
e_dagger_parts_jpn <-
  e_dagger_parts_jpn %>% 
  select(Country, sex, year, e_dagger_parts) %>% 
  unnest(e_dagger_parts) %>% 
  ungroup()
e_dagger_parts_jpn %>% write_csv("clipboard")
```

# Mx at specific ages (**M**: Figure 4 of manuscript)

The following shows Mx for ages 0, 40, 80, and 90 years of age for each of the five countries over time. (This is one of the figures in the main manuscript)

```{r}
dta_Mx %>% 
  left_join(country_labels) %>% 
  filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
  rename(Country = label) %>% 
  filter(age %in% c(0, 40, 80, 90)) %>% 
  filter(year >= 1975) %>% 
  filter(sex != "total") %>% 
  ggplot(aes(x = year, y =  Mx, group = Country, colour = Country, linetype = Country)) +
  geom_line() +
  facet_grid(age ~ sex, scales = "free_y") +
  scale_y_log10() +
  theme_minimal() + 
  labs(
    x = "Year", y = "Probability of death in next 12 months (log scale)",
    title = "Probability of dying in next 12 months by age in years",
    caption = "Source: Human Mortality Database"
  )
ggsave(here("figures", "02_Mx_selected.png"), height = 20, width = 18, units = "cm", dpi = 300)
ggsave(here("figures", "02_Mx_selected.svg"), height = 20, width = 18, units = "cm", dpi = 300)
```
## Alternative Mx figure (**S**)

The following shows the same data as above but using a different visualisation type 

```{r}
dta_Mx %>% 
  left_join(country_labels) %>% 
  filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
  rename(Country = label) %>% 
  filter(age %in% c(0, 40, 80, 90)) %>% 
  filter(year >= 1975) %>% 
  filter(sex != "total") %>% 
  ggplot(aes(x = year, y =  Mx, group = age, colour = as.factor(age), linetype = as.factor(age))) +
  geom_line() +
  facet_grid(Country ~ sex) +
  scale_y_log10() +
  theme_minimal() + 
  labs(
    x = "Year", y = "Probability of death in next 12 months (log scale)",
    title = "Probability of dying in next 12 months by age in years",
    caption = "Source: Human Mortality Database"
  )
ggsave(here("figures", "02a_Mx_selected.png"), height = 20, width = 18, units = "cm", dpi = 300)
ggsave(here("figures", "02a_Mx_selected.svg"), height = 20, width = 18, units = "cm", dpi = 300)
```

## Third alternative way of visualising Mx over time (**S**)

The following shows the data a third way, by indexing trends in Mx to their value in 1975. (i.e. the values in 1975 are set to 100). 

```{r}
dta_Mx %>% 
  left_join(country_labels) %>% 
  filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
  rename(Country = label) %>% 
  filter(age %in% c(0, 40, 80, 90)) %>% 
  filter(year >= 1975) %>% 
  filter(sex != "total") %>%
  group_by(Country, sex, age) %>% 
  mutate(indexed_Mx = 100 * Mx / Mx[year == 1975]) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y =  indexed_Mx, group = age, colour = as.factor(age), linetype = as.factor(age))) +
  geom_line() +
  facet_grid(Country ~ sex) +
  theme_minimal() + 
  labs(
    x = "Year", y = "Trends in probability of dying in next 12 months",
    title = "Age specifc mortality compared with 1975 (1975 = 100)",
    caption = "Source: Human Mortality Database"
  )
ggsave(here("figures", "02b_Mx_selected.png"), height = 20, width = 18, units = "cm", dpi = 300)
ggsave(here("figures", "02b_Mx_selected.svg"), height = 20, width = 18, units = "cm", dpi = 300)
```

## Correlations in log improvement (**W**)

Correlation between log improvement rates by country 

```{r}
corrstretch <- function(x){
  x %>% correlate() %>% stretch()
}
mx_corrs <- 
  dta_Mx %>% 
    left_join(country_labels) %>% 
    filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
    rename(Country = label) %>% 
    filter(age %in% c(0, 40, 80, 90)) %>% 
    filter(year >= 1975) %>% 
    filter(sex != "total") %>% 
    mutate(lnmx = log(Mx)) %>%
    select(-Mx) %>% 
    select(Country, sex, age, year, lnmx) %>% 
    spread(age, lnmx) %>% 
    select(-year) %>% 
    group_by(sex, Country) %>% 
    nest() %>% 
    mutate(correlations = map(data, corrstretch)) %>% 
    select(Country, sex, correlations) %>% 
    unnest(correlations) 
mx_corrs  
```

## Table of correlations in log-mortality rates at specific ages in wide format (**S**)

Rearrange for table 

```{r}
corrs_female <- 
  mx_corrs %>%
    mutate(r = round(r, 3)) %>% 
    spread(y, r) %>% 
  filter(sex == "female") %>%  
  ungroup() %>% select(-sex) %>% 
  set_names(nm = c("Country", "age", "f_0", "f_40", "f_80", "f_90"))
corrs_male <- 
  mx_corrs %>%
    mutate(r = round(r, 3)) %>% 
    spread(y, r) %>% 
  filter(sex == "male") %>%  
  ungroup() %>% select(-sex) %>% 
  set_names(nm = c("Country", "age", "m_0", "m_40", "m_80", "m_90"))
corrs_wide <- full_join(corrs_female, corrs_male, by = c("Country", "age"))
corrs_wide
```

## Nicely formatted version of above table of correlations (**S**)

```{r}
options(knitr.kable.NA = '')
corrs_wide %>% 
  kable(col.names = c("Country", "Age", "0", "40", "80", "90", "0", "40", "80", "90"),
        table.attr = "style = \"color: black;\"") %>% 
  kable_styling(c("bordered")) %>% 
  add_header_above(c(" " = 2, "Female" = 4, "Male" = 4)) %>% 
  collapse_rows(columns = 1)
  
```

## Above table of correlations but split by period (**S**)

As there may be differences over time in how correlated these trends are, let's look at the above but for two periods: 
  
  * 1975-1990
* 1991-latest available year 


```{r}
mx_corrs_1975_1990 <- 
  dta_Mx %>% 
  left_join(country_labels) %>% 
  filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
  rename(Country = label) %>% 
  filter(age %in% c(0, 40, 80, 90)) %>% 
  filter(between(year, 1975, 1990)) %>% 
  filter(sex != "total") %>% 
  mutate(lnmx = log(Mx)) %>%
  select(-Mx) %>% 
  select(Country, sex, age, year, lnmx) %>% 
  spread(age, lnmx) %>% 
  select(-year) %>% 
  group_by(sex, Country) %>% 
  nest() %>% 
  mutate(correlations = map(data, corrstretch)) %>% 
  select(Country, sex, correlations) %>% 
  unnest(correlations) 
mx_corrs_1975_1990  
```

```{r}
corrs_female_1975_1990 <- 
  mx_corrs_1975_1990 %>%
  mutate(r = round(r, 3)) %>% 
  spread(y, r) %>% 
  filter(sex == "female") %>%  
  ungroup() %>% select(-sex) %>% 
  set_names(nm = c("Country", "age", "f_0", "f_40", "f_80", "f_90"))
corrs_male_1975_1990 <- 
  mx_corrs_1975_1990 %>%
  mutate(r = round(r, 3)) %>% 
  spread(y, r) %>% 
  filter(sex == "male") %>%  
  ungroup() %>% select(-sex) %>% 
  set_names(nm = c("Country", "age", "m_0", "m_40", "m_80", "m_90"))
corrs_wide_1975_1990 <- full_join(corrs_female_1975_1990, corrs_male_1975_1990, by = c("Country", "age"))
corrs_wide_1975_1990
```

## Nicely formatted version of table of correlations for period 1 (1975-1990) (**S**)


```{r}
options(knitr.kable.NA = '')
corrs_wide_1975_1990 %>% 
  kable(col.names = c("Country", "Age", "0", "40", "80", "90", "0", "40", "80", "90"),
        table.attr = "style = \"color: black;\"") %>% 
  kable_styling(c("bordered")) %>% 
  add_header_above(c(" " = 2, "Female" = 4, "Male" = 4)) %>% 
  collapse_rows(columns = 1)
```
## Nicely formatted version of table of correlations for period 2 (1991-latest) (**S**)


And for the latter period 


```{r}
mx_corrs_1991_last <- 
  dta_Mx %>% 
  left_join(country_labels) %>% 
  filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
  rename(Country = label) %>% 
  filter(age %in% c(0, 40, 80, 90)) %>% 
  filter(year >= 1991) %>% 
  filter(sex != "total") %>% 
  mutate(lnmx = log(Mx)) %>%
  select(-Mx) %>% 
  select(Country, sex, age, year, lnmx) %>% 
  spread(age, lnmx) %>% 
  select(-year) %>% 
  group_by(sex, Country) %>% 
  nest() %>% 
  mutate(correlations = map(data, corrstretch)) %>% 
  select(Country, sex, correlations) %>% 
  unnest(correlations) 
mx_corrs_1991_last
```

```{r}
corrs_female_1991_last <- 
  mx_corrs_1991_last %>%
  mutate(r = round(r, 3)) %>% 
  spread(y, r) %>% 
  filter(sex == "female") %>%  
  ungroup() %>% select(-sex) %>% 
  set_names(nm = c("Country", "age", "f_0", "f_40", "f_80", "f_90"))
corrs_male_1991_last <- 
  mx_corrs_1991_last %>%
  mutate(r = round(r, 3)) %>% 
  spread(y, r) %>% 
  filter(sex == "male") %>%  
  ungroup() %>% select(-sex) %>% 
  set_names(nm = c("Country", "age", "m_0", "m_40", "m_80", "m_90"))
corrs_wide_1991_last <- full_join(corrs_female_1991_last, corrs_male_1991_last, by = c("Country", "age"))
corrs_wide_1991_last
```



Now to display as a nice table 

```{r, results ='asis'}
options(knitr.kable.NA = '')
corrs_wide_1991_last %>% 
  kable(col.names = c("Country", "Age", "0", "40", "80", "90", "0", "40", "80", "90"),
        table.attr = "style = \"color: black;\"") %>% 
  kable_styling(c("bordered")) %>% 
  add_header_above(c(" " = 2, "Female" = 4, "Male" = 4)) %>% 
  collapse_rows(columns = 1)
```

## New request: Split at 2000-2010; 2011-last year (**S**)

```{r}
mx_corrs_decade_split <- 
  dta_Mx %>% 
  left_join(country_labels) %>% 
  filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
  rename(Country = label) %>%
  mutate(period = case_when(
    between(year, 2000, 2010)    ~ "2000-10",
    between(year, 2011, 2020)    ~ "2011 onwards",
    TRUE                         ~ NA_character_
  )
  ) %>% 
  filter(!is.na(period)) %>% 
  filter(age %in% c(0, 40, 80, 90)) %>% 
  filter(sex != "total") %>% 
  mutate(lnmx = log(Mx)) %>%
  select(-Mx) %>% 
  select(period, Country, sex, age, year, lnmx) %>% 
  spread(age, lnmx) %>% 
  select(-year) %>% 
  group_by(period, sex, Country) %>% 
  nest() %>% 
  mutate(correlations = map(data, corrstretch)) %>% 
  select(period, Country, sex, correlations) %>% 
  unnest(correlations) 
```

## Correlation with even more recent split as heatmap (**S**)


```{r}
mx_corrs_decade_split %>% 
  unite(col = "sex_period", sex, period, remove = TRUE) %>% 
  ggplot(aes(x = x, y = y, fill = r)) + 
  geom_tile() +
  geom_text(aes(label = round(r, 2))) + 
  facet_grid(Country ~ sex_period) + 
  scale_fill_distiller(palette = "RdBu", limits = c(-1,1), direction = 1) +
  labs(
    x = "Age in years", 
    y = "Age in years",
    title = "Correlation between trends in log mortality at selected ages",
    subtitle = "Pearson Correlation"
  )
ggsave(here("figures", "r_by_decade_selected_ages.png"), height = 20, width =25, units = "cm", dpi = 300)
```

Actually we want to R-squared by period



## Mx for all ages (heatmap) (**S**)

The above showed the correlation in log trends only for specific ages; the correlations at all ages can be displayed as a heatmap, and may reveal some additional features in the data



```{r}
dta_trnd <- dta_Mx %>% 
  filter(sex != "total") %>%
  left_join(country_labels) %>% 
  filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
  rename(Country = label) %>% 
  filter(year >= 1975)  %>% 
  filter(age <= 109) %>% 
  group_by(Country, sex, year, age) %>% 
  summarise(mean_Mx = mean(Mx, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(log_mean_Mx = log(mean_Mx, 10)) %>% 
  group_by(sex, Country) %>% 
  nest()
cors_df <- dta_trnd %>% 
  mutate(cors = map(
    data, 
    function(X) {
      X %>% 
        select(-mean_Mx) %>% 
        spread(age, log_mean_Mx) %>% 
        select(-year) %>% 
        cor()
    }
  )
  ) %>% 
  mutate(cor_df = map(
    cors,
    function(X){
      X %>% 
        as_tibble() %>% 
        mutate(from_age = rownames(X)) %>% 
        gather(key = "to_age", value = "value", -from_age) %>% 
        mutate(from_age = as.numeric(from_age), to_age = as.numeric(to_age))
    }
  )
  ) %>% 
  select(sex, Country, cor_df) %>% 
  unnest()
cors_df %>% 
  filter(from_age <= 100, to_age <= 100) %>% 
  rename(r = value) %>% 
  ggplot(aes(x = from_age, y = to_age, fill = r)) + 
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", limits = c(-1,1), direction = 1) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_equal() + 
  facet_grid(Country ~ sex) +
  labs(x = "", y = "",
       title = "Correlation between % improvement\ntrends at specific ages on x and y axes")
ggsave(here("figures", "heatmap_allyears.png"), width = 18, height = 35, units = "cm", dpi = 300)
# ggsave(here("figures", "heatmap_allyears.svg"), width = 18, height = 35, units = "cm", dpi = 300)
```

## Heatmap of correlations in age-specific trends, split into two periods (**S**) 

Now the same for first 1975-1990, then 1991-last year 

### Heatmap for period 1 (1975-1990) (**S**)

```{r}
dta_trnd <- dta_Mx %>% 
  filter(sex != "total") %>%
  left_join(country_labels) %>% 
  filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
  rename(Country = label) %>% 
  filter(between(year, 1975, 1990))  %>% 
  filter(age <= 109) %>% 
  group_by(Country, sex, year, age) %>% 
  summarise(mean_Mx = mean(Mx, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(log_mean_Mx = log(mean_Mx, 10)) %>% 
  group_by(sex, Country) %>% 
  nest()
cors_df <- dta_trnd %>% 
  mutate(cors = map(
    data, 
    function(X) {
      X %>% 
        select(-mean_Mx) %>% 
        spread(age, log_mean_Mx) %>% 
        select(-year) %>% 
        cor()
    }
  )
  ) %>% 
  mutate(cor_df = map(
    cors,
    function(X){
      X %>% 
        as_tibble() %>% 
        mutate(from_age = rownames(X)) %>% 
        gather(key = "to_age", value = "value", -from_age) %>% 
        mutate(from_age = as.numeric(from_age), to_age = as.numeric(to_age))
    }
  )
  ) %>% 
  select(sex, Country, cor_df) %>% 
  unnest()
cors_df %>% 
  filter(from_age <= 100, to_age <= 100) %>% 
  rename(r = value) %>% 
  ggplot(aes(x = from_age, y = to_age, fill = r)) + 
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", limits = c(-1,1), direction = 1) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_equal() + 
  facet_grid(Country ~ sex) +
  labs(x = "", y = "",
       title = "Correlation between % improvement\ntrends at specific ages on x and y axes",
       subtitle = "1975-1990")
ggsave(here("figures", "heatmap_1975_1990.png"), width = 18, height = 35, units = "cm", dpi = 300)
# ggsave(here("figures", "heatmap_1975_1990.svg"), width = 18, height = 35, units = "cm", dpi = 300)
```

### Heatmap for period 2 (1991-latest) (**S**)

And now 1991 to last available year 

```{r}
dta_trnd <- dta_Mx %>% 
  filter(sex != "total") %>%
  left_join(country_labels) %>% 
  filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
  rename(Country = label) %>% 
  filter(year >= 1991)  %>% 
  filter(age <= 109) %>% 
  group_by(Country, sex, year, age) %>% 
  summarise(mean_Mx = mean(Mx, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(log_mean_Mx = log(mean_Mx, 10)) %>% 
  group_by(sex, Country) %>% 
  nest()
cors_df <- dta_trnd %>% 
  mutate(cors = map(
    data, 
    function(X) {
      X %>% 
        select(-mean_Mx) %>% 
        spread(age, log_mean_Mx) %>% 
        select(-year) %>% 
        cor()
    }
  )
  ) %>% 
  mutate(cor_df = map(
    cors,
    function(X){
      X %>% 
        as_tibble() %>% 
        mutate(from_age = rownames(X)) %>% 
        gather(key = "to_age", value = "value", -from_age) %>% 
        mutate(from_age = as.numeric(from_age), to_age = as.numeric(to_age))
    }
  )
  ) %>% 
  select(sex, Country, cor_df) %>% 
  unnest()
cors_df %>% 
  filter(from_age <= 100, to_age <= 100) %>% 
  rename(r = value) %>% 
  ggplot(aes(x = from_age, y = to_age, fill = r)) + 
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", limits = c(-1,1), direction = 1) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_equal() + 
  facet_grid(Country ~ sex) +
  labs(x = "", y = "",
       title = "Correlation between % improvement\ntrends at specific ages on x and y axes",
       subtitle = "1991 onwards")
ggsave(here("figures", "heatmap_1991_last.png"), width = 18, height = 35, units = "cm", dpi = 300)
# ggsave(here("figures", "heatmap_1991_last.svg"), width = 18, height = 35, units = "cm", dpi = 300)
```

## New Request : 2000-2010 and 2011 onwards (**S**)

### Heatmap: period 2000-2010 (**S**)

```{r}
dta_trnd <- dta_Mx %>% 
  filter(sex != "total") %>%
  left_join(country_labels) %>% 
  filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
  rename(Country = label) %>% 
  filter(between(year, 2000, 2010))  %>% 
  filter(age <= 109) %>% 
  group_by(Country, sex, year, age) %>% 
  summarise(mean_Mx = mean(Mx, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(log_mean_Mx = log(mean_Mx, 10)) %>% 
  group_by(sex, Country) %>% 
  nest()
cors_df <- dta_trnd %>% 
  mutate(cors = map(
    data, 
    function(X) {
      X %>% 
        select(-mean_Mx) %>% 
        spread(age, log_mean_Mx) %>% 
        select(-year) %>% 
        cor()
    }
  )
  ) %>% 
  mutate(cor_df = map(
    cors,
    function(X){
      X %>% 
        as_tibble() %>% 
        mutate(from_age = rownames(X)) %>% 
        gather(key = "to_age", value = "value", -from_age) %>% 
        mutate(from_age = as.numeric(from_age), to_age = as.numeric(to_age))
    }
  )
  ) %>% 
  select(sex, Country, cor_df) %>% 
  unnest()
cors_df %>% 
  filter(from_age <= 100, to_age <= 100) %>% 
  rename(r = value) %>% 
  ggplot(aes(x = from_age, y = to_age, fill = r)) + 
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", limits = c(-1,1), direction = 1) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_equal() + 
  facet_grid(Country ~ sex) +
  labs(x = "", y = "",
       title = "Correlation between % improvement\ntrends at specific ages on x and y axes",
       subtitle = "2000-2010")
ggsave(here("figures", "heatmap_2000_2010.png"), width = 18, height = 35, units = "cm", dpi = 300)
# ggsave(here("figures", "heatmap_1975_1990.svg"), width = 18, height = 35, units = "cm", dpi = 300)
```

### Heatmap: correlations from 2011 onwards (**S**)

```{r}
dta_trnd <- dta_Mx %>% 
  filter(sex != "total") %>%
  left_join(country_labels) %>% 
  filter(label %in% c("USA", "Canada", "United Kingdom", "Japan", "France")) %>% 
  rename(Country = label) %>% 
  filter(year >= 2011)  %>% 
  filter(age <= 109) %>% 
  group_by(Country, sex, year, age) %>% 
  summarise(mean_Mx = mean(Mx, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(log_mean_Mx = log(mean_Mx, 10)) %>% 
  group_by(sex, Country) %>% 
  nest()
cors_df <- dta_trnd %>% 
  mutate(cors = map(
    data, 
    function(X) {
      X %>% 
        select(-mean_Mx) %>% 
        spread(age, log_mean_Mx) %>% 
        select(-year) %>% 
        cor()
    }
  )
  ) %>% 
  mutate(cor_df = map(
    cors,
    function(X){
      X %>% 
        as_tibble() %>% 
        mutate(from_age = rownames(X)) %>% 
        gather(key = "to_age", value = "value", -from_age) %>% 
        mutate(from_age = as.numeric(from_age), to_age = as.numeric(to_age))
    }
  )
  ) %>% 
  select(sex, Country, cor_df) %>% 
  unnest()
cors_df %>% 
  filter(from_age <= 100, to_age <= 100) %>% 
  rename(r = value) %>% 
  ggplot(aes(x = from_age, y = to_age, fill = r)) + 
  geom_tile() +
  scale_fill_distiller(palette = "RdBu", limits = c(-1,1), direction = 1) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  coord_equal() + 
  facet_grid(Country ~ sex) +
  labs(x = "", y = "",
       title = "Correlation between % improvement\ntrends at specific ages on x and y axes",
       subtitle = "2011 onwards")
ggsave(here("figures", "heatmap_2011_onwards.png"), width = 18, height = 35, units = "cm", dpi = 300)
# ggsave(here("figures", "heatmap_1975_1990.svg"), width = 18, height = 35, units = "cm", dpi = 300)
```

# Workbooks (**S**)

It'll be good to be able to save the values from cors_df as a series of excel worksheets in workbooks. Let's look at the contents 

```{r}
cors_df %>% 
  ungroup() %>% 
  filter(sex == "female", Country == "Canada") %>% 
  select(from_age, to_age, value) %>% 
  spread(to_age, value)
```

Let's first produce a workbook with just the two periods visualised above 

```{r}
calc_and_grab_corrs <- function(DTA, this_country, this_sex, start_period, end_period, continuity_correction = 0){
  selected_data <- 
    DTA %>% 
      filter(sex == this_sex) %>% 
      filter(Country == this_country) %>% 
      filter(between(year, start_period, end_period)) %>% 
      filter(age <= 109) %>% 
      mutate(log_Mx = log(Mx + continuity_correction, 10)) %>% 
      select(year, age, log_Mx)  
    
  selected_data %>%
    spread(age, log_Mx) %>%
    select(-year) %>%
    cor()
}
dta_Mx_labelled_filtered <- 
  dta_Mx %>% 
  left_join(country_labels) %>% 
  select(Country = label, sex, year, age, Mx)
all_corrblocks <- 
  crossing(
    Country = c("France", "Japan", "United Kingdom", "USA", "Canada"),
    sex = c("female", "male"),
    start_period = c(1975, 1991),
    end_period = c(1990, 2019)
  ) %>% 
  filter(magrittr::or(
    start_period == 1975 & end_period %in% c(1990, 2019),
    start_period == 1991 & end_period == 2019    
    )
  ) %>% 
    mutate(corr_data = pmap(
      .l = list(this_country = Country, this_sex = sex, start_period = start_period, end_period = end_period), 
      .f = calc_and_grab_corrs, 
      DTA = dta_Mx_labelled_filtered
      )
    )
```

Write out these blocks

```{r}
add_format_worksheet <- function(wb, label, corr_data){
  wb %>% addWorksheet(sheetName = label, zoom = 25)
  wb %>% writeData(sheet = label, x = corr_data, rowNames = TRUE)
  wb %>% setColWidths(sheet = label, cols = 1:ncol(corr_data), widths = 3)
  wb %>% setRowHeights(sheet = label, rows = 1:(nrow(corr_data)), heights = 20)
  wb %>% conditionalFormatting(sheet = label, cols = 1:ncol(corr_data), rows = 1:(nrow(corr_data)),
                             type = "colorscale", style = c("red", "white", "blue"), rule = c(-1, 0, 1))
  
  NULL
}
wb <- openxlsx::createWorkbook()
all_corrblocks %>% 
  mutate(label = str_c(str_extract(Country, "^.{2}"), str_extract(sex, "^.{2}"), start_period, end_period, sep = "_")) %>% 
  mutate(tmp = walk2(label, corr_data, add_format_worksheet, wb = wb))
saveWorkbook(wb, file = here("tables", "blocks_both_main_periods.xlsx"), overwrite = TRUE)
```


Let's take a rolling series of 25 year periods, each five years apart, starting from 1950.

```{r}
calc_and_grab_corrs <- function(DTA, this_country, this_sex, start_period, window_length = 25, continuity_correction = 0){
  selected_data <- 
    DTA %>% 
    filter(sex == this_sex) %>% 
    filter(Country == this_country) %>% 
    filter(between(year, start_period, start_period + window_length)) %>% 
    filter(age <= 109) %>% 
    mutate(log_Mx = log(Mx + continuity_correction, 10)) %>% 
    select(year, age, log_Mx)  
  
  selected_data %>%
    spread(age, log_Mx) %>%
    select(-year) %>%
    cor()
}
dta_Mx_labelled_filtered <- 
  dta_Mx %>% 
  left_join(country_labels) %>% 
  select(Country = label, sex, year, age, Mx)
all_corrblocks <- 
  crossing(
    Country = c("France", "Japan", "United Kingdom", "USA", "Canada"),
    sex = c("female", "male"),
    start_period = seq(1950, 1995, by = 5)
  ) %>% 
  mutate(corr_data = pmap(
    .l = list(this_country = Country, this_sex = sex, start_period = start_period), 
    .f = calc_and_grab_corrs, 
    DTA = dta_Mx_labelled_filtered
  )
  )
```

Now to figure out how to write two of these in a neat way , before moving to others 

```{r, eval = FALSE}
all_corrblocks %>% 
  slice(1:2)
block_01 <- all_corrblocks %>% pull("corr_data") %>% pluck(1)
block_02 <- all_corrblocks %>% pull("corr_data") %>% pluck(2)
wb <- openxlsx::createWorkbook()
wb %>% addWorksheet(sheetName = "block01")
wb %>% writeData(sheet = "block01", x = block_01, startRow = 4, rowNames = TRUE)
wb %>% writeData(sheet = "block01", startRow = 1, x = all_corrblocks %>% slice(1) %>% select(-corr_data) %>%  mutate(end_period = start_period + 25))
wb %>% setColWidths(sheet = "block01", cols = 1:ncol(block_01), widths = 3)
wb %>% setRowHeights(sheet = "block01", rows = 4:(nrow(block_01)+4), heights = 20)
wb %>% conditionalFormatting(sheet = "block01", cols = 1:ncol(block_01), rows = 4:(nrow(block_01)+4),
                             type = "colorscale", style = c("red", "white", "blue"), rule = c(-1, 0, 1))
wb %>% addWorksheet(sheetName = "block02")
wb %>% writeData(sheet = "block02", x = block_02, startRow = 4, rowNames = TRUE)
wb %>% writeData(sheet = "block02", startRow = 1, x = all_corrblocks %>% slice(2) %>% select(-corr_data) %>% mutate(end_period = start_period + 25))
wb %>% setColWidths(sheet = "block02", cols = 1:ncol(block_02), widths = 3)
wb %>% setRowHeights(sheet = "block02", rows = 4:(nrow(block_02)+4), heights = 20)
saveWorkbook(wb, file = here("tables", "two_blocks.xlsx"), overwrite = TRUE)
```


Now for all blocks

```{r}
add_format_worksheet <- function(wb, label, corr_data){
  wb %>% addWorksheet(sheetName = label, zoom = 25)
  wb %>% writeData(sheet = label, x = corr_data, rowNames = TRUE)
  wb %>% setColWidths(sheet = label, cols = 1:ncol(corr_data), widths = 3)
  wb %>% setRowHeights(sheet = label, rows = 1:(nrow(corr_data)), heights = 20)
  wb %>% conditionalFormatting(sheet = label, cols = 1:ncol(corr_data), rows = 1:(nrow(corr_data)),
                               type = "colorscale", style = c("red", "white", "blue"), rule = c(-1, 0, 1))
  
  NULL
}
wb <- openxlsx::createWorkbook()
all_corrblocks %>% 
  mutate(label = str_c(str_extract(Country, "^.{2}"), str_extract(sex, "^.{2}"), start_period, sep = "_")) %>% 
  mutate(tmp = walk2(label, corr_data, add_format_worksheet, wb = wb))
saveWorkbook(wb, file = here("tables", "many_blocks.xlsx"), overwrite = TRUE)
```

# Linearity of trends (**W**)

A complementary analysis to that produced above is to investigate how (log)linear the trends have been at different ages. It's expected that trends in infancy will be largely (log)linear, along with those in retirement ages, whereas trends in working age will be least (log)linear, i.e. those age groups with the weakest correlations and strongest negative correlations with those at the majority of ages will also be those with where the (log)linearity of the trends is smallest. 

```{r}
get_rsq <- function(DTA, start_year, end_year){
  DTA %>% 
    filter(between(year, start_year, end_year)) %>% 
    mutate(logMx = log(Mx, 10)) %>% 
    lm(logMx ~ year, data = .) %>% 
    broom::glance() %>% 
    pull("r.squared")
}
all_rsq <- 
  dta_Mx_labelled_filtered %>% 
  filter(Country %in% c("France", "Japan", "United Kingdom", "USA", "Canada")) %>% 
    group_by(Country, sex, age) %>% 
    nest() %>% 
    crossing(
      start_year = c(1975, 1991, 2000, 2011),
      end_year   = c(1990, 2000, 2010, 2018)
    ) %>% 
    filter(!(end_year <= start_year)) %>% 
    mutate(rsq = pmap_dbl(
      .l = list(DTA = data, start_year = start_year, end_year = end_year),
      .f = possibly(get_rsq, otherwise = NA_real_)
    ))
all_rsq
```

## Loglinearity of trends, three (non-contiguous) periods (**S**)

The following shows the R-squared values of log mortality trends at different ages. Higher values indicate greater log-linearity. 


```{r}
all_rsq %>% 
  mutate(
    period = case_when(
      start_year == 1975 & end_year == 2018   ~ "1975 onwards",
      start_year == 1975 & end_year == 1990   ~ "1975-1990",
      start_year == 1991 & end_year == 2018   ~ "1991 onwards",
      TRUE                                    ~ NA_character_
    ) %>% factor(., levels = c("1975 onwards", "1975-1990", "1991 onwards"), ordered =TRUE)
  ) %>% 
#  write_to_table(here("tables", "linearity_rsq.csv")) %>% 
  filter(sex != "total") %>% 
  ggplot(aes(x = age, y = rsq, group = period, linetype = period, colour = period)) + 
  geom_line() + 
  facet_grid(Country ~ sex) + 
  labs(
    x = "Age in years", 
    y = "Log-linearity (R-squared)",
    title = "Linearity in log mortality improvement rates"
  ) + 
  theme_minimal() +
  scale_colour_manual(values = c("black", "darkred", "darkblue"))
ggsave(here("figures", "log_linearity_by_age.png"), height = 20, width = 20, units = "cm", dpi = 300)
ggsave(here("figures", "log_linearity_by_age.svg"), height = 20, width = 20, units = "cm", dpi = 300)
```

From this figure the following features are apparent: 

* Overall, high linearity ($R^2$ values around 1) in trends in infancy and old age (from around 70 to 90 years) is apparent in all populations. 
* Over the late 80s and 90s the lienarity in trends reduces.
* In most populations the age at which linearity in trends starts to fall increases when comparing the earlier (1975-1990) to latter (1991 onwards) periods. This is evidence by comparing the fall off in linearity with age within a population of the dot-dashed red line with the solid blue line. It tends to start falling off at a higher age in the latter than the earlier period. 
* The USA is exceptional in that the trends are, from 1991, almost completely nonlinear in the late 20s for both sexes. For females this lack of linear improvement is also apparent in the early thirties, and in the mid 40s. 
* For both sexes combined and for the full period (black dashed line) the USA has a more severe drop in linearity in early adulthood for both sexes than other countries, though the UK has a more severe drop off for males in younger adulthood. However whereas in the UK this male fall in linearity is more severe in the earlier period than the latter period, in the USA it is as or more severe in the latter period than the earlier period. 
* In the USA there is also a fall in linearity in middle age (around age 45 years), which is more pronounced for females than males. 


And let's do a less noisy version with just the full period 

```{r}
all_rsq %>% 
  mutate(
    period = case_when(
      start_year == 1975 & end_year == 2018   ~ "1975 onwards",
      start_year == 1975 & end_year == 1990   ~ "1975-1990",
      start_year == 1991 & end_year == 2018   ~ "1991 onwards",
      TRUE                                    ~ NA_character_
    ) %>% factor(., levels = c("1975 onwards", "1975-1990", "1991 onwards"), ordered =TRUE)
  ) %>% 
  filter(sex != "total") %>% 
  filter(period == "1975 onwards") %>% 
  ggplot(aes(x = age, y = rsq)) + 
  geom_line() + 
  facet_grid(Country ~ sex) + 
  labs(
    x = "Age in years", 
    y = "Log-linearity (R-squared)",
    title = "Linearity in log mortality improvement rates, 1975 onwards"
  ) + 
  theme_minimal() 
ggsave(here("figures", "log_linearity_by_age_1975onwards.png"), height = 20, width = 20, units = "cm", dpi = 300)
ggsave(here("figures", "log_linearity_by_age_1975onwards.svg"), height = 20, width = 20, units = "cm", dpi = 300)
```


And a third version overlaid 


```{r}
all_rsq %>% 
  mutate(
    period = case_when(
      start_year == 1975 & end_year == 2018   ~ "1975 onwards",
      start_year == 1975 & end_year == 1990   ~ "1975-1990",
      start_year == 1991 & end_year == 2018   ~ "1991 onwards",
      TRUE                                    ~ NA_character_
    ) %>% factor(., levels = c("1975 onwards", "1975-1990", "1991 onwards"), ordered =TRUE)
  ) %>% 
  filter(sex != "total") %>% 
  filter(period == "1975 onwards") %>% 
  ggplot(aes(x = age, y = rsq, group = Country, colour = Country)) + 
  geom_line() + 
  facet_grid(. ~ sex) + 
  labs(
    x = "Age in years", 
    y = "Log-linearity (R-squared)",
    title = "Linearity in log mortality improvement rates, 1975 onwards"
  ) + 
  theme_minimal() 
ggsave(here("figures", "log_linearity_by_age_1975onwards_overlaid.png"), height = 10, width = 20, units = "cm", dpi = 300)
ggsave(here("figures", "log_linearity_by_age_1975onwards_overlaid.svg"), height = 10, width = 20, units = "cm", dpi = 300)
```



```{r}
all_rsq %>% 
  mutate(
    period = case_when(
      start_year == 1975 & end_year == 2018   ~ "1975 onwards",
      start_year == 1975 & end_year == 1990   ~ "1975-1990",
      start_year == 1991 & end_year == 2018   ~ "1991 onwards",
      TRUE                                    ~ NA_character_
    ) %>% factor(., levels = c("1975 onwards", "1975-1990", "1991 onwards"), ordered =TRUE)
  ) %>% 
  filter(sex != "total") %>% 
  filter(period == "1975-1990") %>% 
  ggplot(aes(x = age, y = rsq, group = Country, colour = Country)) + 
  geom_line() + 
  facet_grid(. ~ sex) + 
  labs(
    x = "Age in years", 
    y = "Log-linearity (R-squared)",
    title = "Linearity in log mortality improvement rates, 1975-1990"
  ) + 
  theme_minimal() 
ggsave(here("figures", "log_linearity_by_age_1975_1990_overlaid.png"), height = 10, width = 20, units = "cm", dpi = 300)
ggsave(here("figures", "log_linearity_by_age_1975_1990_overlaid.svg"), height = 10, width = 20, units = "cm", dpi = 300)
```

From these it's clear that the USA has less linearity in trends in earlier working age, for both sexes, and that the USA is exceptional in this lack of linearity being observed in both males and females. The UK has a breakdown in linearity in males, and Canada, to a lesser extent, a breakdown in linearity in females. 

It is also apparant that the USA has a breakdown in linearity with old age from a younger age than France and Japan. 



```{r}
all_rsq %>% 
  mutate(
    period = case_when(
      start_year == 1975 & end_year == 2018   ~ "1975 onwards",
      start_year == 1975 & end_year == 1990   ~ "1975-1990",
      start_year == 1991 & end_year == 2018   ~ "1991 onwards",
      TRUE                                    ~ NA_character_
    ) %>% factor(., levels = c("1975 onwards", "1975-1990", "1991 onwards"), ordered =TRUE)
  ) %>% 
  filter(sex != "total") %>% 
  filter(period == "1991 onwards") %>% 
  ggplot(aes(x = age, y = rsq, group = Country, colour = Country)) + 
  geom_line() + 
  facet_grid(. ~ sex) + 
  labs(
    x = "Age in years", 
    y = "Log-linearity (R-squared)",
    title = "Linearity in log mortality improvement rates, 1991 onwards"
  ) + 
  theme_minimal() 
ggsave(here("figures", "log_linearity_by_age_1991_plus_overlaid.png"), height = 10, width = 20, units = "cm", dpi = 300)
ggsave(here("figures", "log_linearity_by_age_1991_plus_overlaid.svg"), height = 10, width = 20, units = "cm", dpi = 300)
```

For completeness perhaps % improvement (the beta?) might also be helpful. 

Finally, let's do for the 2000s and post 2011 period 


```{r}
all_rsq %>% 
  mutate(
    period = case_when(
      start_year == 2000 & end_year == 2018   ~ "2000 onwards",
      start_year == 2000 & end_year == 2010   ~ "2000-2010",
      start_year == 2011 & end_year == 2018   ~ "2011 onwards",
      TRUE                                    ~ NA_character_
    ) %>% factor(., levels = c("2000 onwards", "2000-2010", "2011 onwards"), ordered =TRUE)
  ) %>% 
  filter(sex != "total") %>% 
  filter(!is.na(period)) %>% 
  ggplot(aes(x = age, y = rsq, group = period, linetype = period, colour = period)) + 
  geom_line() + 
  facet_grid(Country ~ sex) + 
  labs(
    x = "Age in years", 
    y = "Log-linearity (R-squared)",
    title = "Linearity in log mortality improvement rates, 2000-2010 and 2011 onwards"
  ) + 
  theme_minimal() +
  scale_colour_manual(values = c("black", "darkred", "darkblue")) +
  scale_linetype_manual(values = c("dashed", "dotdash", "solid"))
ggsave(here("figures", "log_linearity_by_age_twodecades.png"), height = 20, width = 20, units = "cm", dpi = 300)
ggsave(here("figures", "log_linearity_by_age_twodecades.png"), height = 20, width = 20, units = "cm", dpi = 300)
```





# Request: simplified version 

* Japan and UK
* No 2000 onwards overall

```{r}
all_rsq %>% 
  mutate(
    period = case_when(
      start_year == 2000 & end_year == 2010   ~ "2000-2010",
      start_year == 2011 & end_year == 2018   ~ "2011 onwards",
      TRUE                                    ~ NA_character_
    ) %>% factor(., levels = c("2000-2010", "2011 onwards"), ordered =TRUE)
  ) %>% 
  filter(sex != "total") %>% 
  filter(Country %in% c("Japan", "United Kingdom")) %>% 
  filter(!is.na(period)) %>% 
  ggplot(aes(x = age, y = rsq, group = period, linetype = period, colour = period)) + 
  geom_line() + 
  facet_grid(Country ~ sex) + 
  labs(
    x = "Age in years", 
    y = "Log-linearity (R-squared)",
    title = "Linearity in log mortality improvement rates, 2000-2010 and 2011 onwards"
  ) + 
  theme_minimal() +
  scale_colour_manual(values = c("black", "darkred")) +
  scale_linetype_manual(values = c("dashed",  "solid"))
ggsave(here("figures", "ukjpn_log_linearity_by_age_twodecades.png"), height = 15, width = 20, units = "cm", dpi = 300)
ggsave(here("figures", "ukjpn_log_linearity_by_age_twodecades.png"), height = 15, width = 20, units = "cm", dpi = 300)
```


And with points instead 

```{r}
all_rsq %>% 
  mutate(
    period = case_when(
      start_year == 2000 & end_year == 2010   ~ "2000-2010",
      start_year == 2011 & end_year == 2018   ~ "2011 onwards",
      TRUE                                    ~ NA_character_
    ) %>% factor(., levels = c("2000-2010", "2011 onwards"), ordered =TRUE)
  ) %>% 
  filter(sex != "total") %>% 
  filter(Country %in% c("Japan", "United Kingdom")) %>% 
  filter(!is.na(period)) %>% 
  ggplot(aes(x = age, y = rsq, group = period, linetype = period, colour = period, shape = period)) + 
  geom_point() + 
  facet_grid(Country ~ sex) + 
  labs(
    x = "Age in years", 
    y = "Log-linearity (R-squared)",
    title = "Linearity in log mortality improvement rates, 2000-2010 and 2011 onwards"
  ) + 
  theme_minimal() +
  scale_colour_manual(values = c("black", "darkred")) 
ggsave(here("figures", "ukjpn_log_linearity_by_age_twodecades_points.png"), height = 15, width = 20, units = "cm", dpi = 300)
```

And with smoothers too 

```{r}
all_rsq %>% 
  mutate(
    period = case_when(
      start_year == 2000 & end_year == 2010   ~ "2000-2010",
      start_year == 2011 & end_year == 2018   ~ "2011 onwards",
      TRUE                                    ~ NA_character_
    ) %>% factor(., levels = c("2000-2010", "2011 onwards"), ordered =TRUE)
  ) %>% 
  filter(sex != "total") %>% 
  filter(Country %in% c("Japan", "United Kingdom")) %>% 
  filter(!is.na(period)) %>% 
  ggplot(aes(x = age, y = rsq, group = period, linetype = period, colour = period, shape = period)) + 
  geom_point(alpha = 0.35) + 
  stat_smooth(se = FALSE, n = 100) + 
  facet_grid(Country ~ sex) + 
  labs(
    x = "Age in years", 
    y = "Log-linearity (R-squared)",
    title = "Linearity in log mortality improvement rates, 2000-2010 and 2011 onwards"
  ) + 
  theme_minimal() +
  scale_colour_manual(values = c("black", "darkred")) +
  scale_linetype_manual(values = c("dashed",  "solid"))
ggsave(here("figures", "ukjpn_log_linearity_by_age_twodecades_points_smoother.png"), height = 15, width = 20, units = "cm", dpi = 300)
```



# Quick calculation : life expectancy and lifespan disparities in Japan and UK


```{r}
e_daggers %>% 
  filter(Country %in% c("Japan", "United Kingdom")) %>% 
  filter(sex == "total") %>% 
  ggplot(aes(x = year, y = e_dagger)) + 
  facet_wrap(~Country) + 
  geom_line() +
  annotate(geom = "rect", xmin = 1990, xmax = 2001, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "red") + 
  annotate(geom = "rect", xmin = 2008, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "green") + 
  labs(
    x = "year", y = "Lifespan disparity (years)",
    title = "Lifespan disparity in Japan and the UK",
    subtitle = "Shaded regions indicate Japan's and the UK's 'lost decades', respectively"
  )
ggsave(here("figures", "uk_jpn_disparity.png"), height = 20, width = 30, units = "cm", dpi = 300)
```


```{r}
dta_e0 %>% 
  filter(code %in% c("JPN", "GBR_NP")) %>% 
  filter(year > 1975) %>% 
  mutate(Country = case_when(
    code == 'JPN'    ~ "Japan",
    code == "GBR_NP" ~ "United Kingdom" 
  )) %>% 
  filter(sex == "total") %>% 
  ggplot(aes(x = year, y = e0)) + 
  facet_wrap(~Country) + 
  geom_line() +
  annotate(geom = "rect", xmin = 1990, xmax = 2001, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "red") + 
  annotate(geom = "rect", xmin = 2008, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "green") + 
  labs(
    x = "year", y = "Life expectancy at birth (years)",
    title = "Life expectancy in Japan and the UK",
    subtitle = "Shaded regions indicate Japan's and the UK's 'lost decades', respectively"
  )
ggsave(here("figures", "uk_jpn_e0.png"), height = 20, width = 30, units = "cm", dpi = 300)
```

```{r}
dta_e0 %>% 
  filter(code %in% c("JPN", "GBR_NP")) %>% 
  filter(sex == "total") %>% 
  group_by(code) %>% 
  arrange(year) %>% 
  mutate(ch_e0 = e0 - lag(e0)) %>% 
  ungroup() %>% 
  filter(year > 1975) %>% 
  mutate(Country = case_when(
    code == 'JPN'    ~ "Japan",
    code == "GBR_NP" ~ "United Kingdom" 
  )) %>% 
  ggplot(aes(x = year, y = ch_e0)) + 
  facet_wrap(~Country) + 
  geom_line() +
  geom_hline(yintercept = 0) +
  annotate(geom = "rect", xmin = 1990, xmax = 2001, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "red") + 
  annotate(geom = "rect", xmin = 2008, xmax = 2019, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "green") + 
  labs(
    x = "year", y = "Annual change in life expectancy at birth (years)",
    title = "Annual changes in life expectancy in Japan and the UK",
    subtitle = "Shaded regions indicate Japan's and the UK's 'lost decades', respectively"
  )
ggsave(here("figures", "uk_jpn_ch_e0.png"), height = 20, width = 30, units = "cm", dpi = 300)

```