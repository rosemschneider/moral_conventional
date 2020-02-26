#Cross-cultural moral/conventional analysis code
## Info about study and analysis plan here

# SETUP ----
source("0-clean.R") # data cleaning script, produces cleaned data for study 1 and 2
# Load cleaned data - 2 dfs
rm(list = ls())
load("../Data/Cleaned data/Study1_MC_all_data.RData") #study 1
load("../Data/Cleaned data/Study2_MC_iran.RData") #study 2

# load packages ----
library(tidyverse)
library(magrittr)
library(car)
library(lme4)
library(ggpubr)
library(broom)
library(broom.mixed)
library(tidylog)

#to-do: color palettes
# # Custom global variables
# # colorblind friendly red blue green
# myRGBpalette <- c("#D55E00", "#0073B3", "#009E73")
# # colorblind friendly green, orange, bright blue
# myGOBpalette <- c("#009E73", "#E69F00", "#56B4E9")

all.data %<>%
  mutate(subid = factor(subid), 
         task = factor(task), 
         item = factor(item), 
         answer = as.numeric(answer), 
         site = factor(site))

#global theme set
theme_set(theme_bw() + theme(text = element_text(size=9), 
                             axis.title=element_text(size=8),
                             strip.text = element_text(margin=margin(2,0,2,0)), 
                             panel.grid = element_blank()))

# Study 1: Descriptives ----
all.data %>%
  distinct(subid, site, age)%>%
  group_by(site)%>%
  summarise_at('age', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(site, n, mean, sd, median, min, max)

## Now, if we filter out kids older than 11 in india
all.data %>%
  distinct(subid, site, age)%>%
  filter(age < 11)%>%
  group_by(site)%>%
  summarise_at('age', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(site, n, mean, sd, median, min, max)

# ... ratings for moral and conventional items by age and country ----
fit_ratings <- lm(as.numeric(answer) ~ task * age * site, 
                data=subset(all.data, q_kind == 0))
glance(fit_ratings)
Anova(fit_ratings) # interaction n.s, sig. main effects of Age and IHC
tidy(fit_ratings, conf.int=T) %>% mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))


# Study 2: Descriptives ----
iran.study.2 %>%
  distinct(subid, site, age)%>%
  group_by(site)%>%
  summarise_at('age', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(site, n, mean, sd, median, min, max)
