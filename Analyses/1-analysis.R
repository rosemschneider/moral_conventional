#Cross-cultural moral/conventional analysis code
## Info about study and analysis plan here

# SETUP ----
source(here::here("Analyses/0-clean.R")) # data cleaning script, produces cleaned data for study 1 and 2
# Load cleaned data - 2 dfs
rm(list = ls())
load(here::here("/Data/Cleaned data/Study1_MC_all_data.RData"))#study 1
load(here::here("Data/Cleaned data/Study2_MC_iran.RData")) #study 2

# load packages ----
library(tidyverse)
library(magrittr)
library(car)
library(lme4)
library(ggpubr)
library(broom)
library(broom.mixed)
library(tidylog)
library(lmerTest)

#to-do: color palettes
# # Custom global variables
# # colorblind friendly red blue green
# myRGBpalette <- c("#D55E00", "#0073B3", "#009E73")
# # colorblind friendly green, orange, bright blue
# myGOBpalette <- c("#009E73", "#E69F00", "#56B4E9")

# leftover data manipulations
all.data %<>%
  mutate(subid = factor(subid), 
         task = factor(task), 
         item = factor(item), 
         answer = as.numeric(answer), 
         site = factor(site), 
         age.c = as.vector(scale(age, center = TRUE, scale = TRUE)), 
         age.group = as.numeric(as.character(age.group)))%>%
  mutate(age.bin = ifelse(age.group < 7, "5-6", 
                          ifelse((age.group >= 7 & age.group < 9), "7-8", "9-10")), 
         sex = ifelse(sex == "Female", "F",
                      ifelse(sex == "Male", "M", 
                             ifelse(sex == "", NA, as.character(sex)))))


#global theme set
theme_set(theme_bw() + theme(text = element_text(size=13), 
                             axis.title=element_text(size=12),
                             strip.text = element_text(margin=margin(2,0,2,0)), 
                             panel.grid = element_blank()))

# Study 1 ----
# ... study 1 descriptives ---
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
  dplyr::select(site, n, mean, sd, median, min, max)%>%
  mutate(total.n = sum(n))

total.mean <- mean(all.data$age)
total.sd <- sd(all.data$age)

## By sex
print(all.data %>%
  distinct(subid, site, sex)%>%
  group_by(site, sex)%>%
  summarise(n = n())%>%
  group_by(site)%>%
  mutate(total.site.n = sum(n)), n = 21)

# ... ratings for moral and conventional items by age and country ----
## descriptives
all.data %>%
  filter(q_kind == 0)%>%
  group_by(site, task)%>%
  summarise_at('answer', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(site, task, n, mean, sd, median, min, max)

# ... visualization of mean ratings by country and transgression type ----
all.data %>% 
  filter(q_kind == 0)%>%
  mutate(task = factor(task, levels = c("moral", "conv"), 
                       labels = c("Moral", "Conventional")))%>%
  group_by(subid, site, task)%>%
  ggplot(aes(x = task, y = answer, fill = task, color = task)) + 
  stat_summary(fun = mean, position = position_dodge(width = .9),
               geom="bar", alpha = .5, colour = "black") +
  geom_point(position=position_jitter(width = .18, height = .1),
             size=1.5,
             alpha = .6,
             show.legend=FALSE,
             inherit.aes = TRUE) +
  geom_hline(yintercept = 3.5, color = "grey", linetype = "dashed") + 
  facet_grid(~site) + 
  coord_cartesian(ylim = c(1, 6)) +
  stat_summary(fun.data = "mean_cl_boot", geom="linerange", 
               position = position_dodge(width=0.9), size = 1, color = "black") +
  theme(legend.position = "top", 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        legend.key.size = unit(.5, 'cm'), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 11), 
        legend.margin=margin(t = 0, unit='cm')) +
  langcog::scale_color_solarized() + 
  langcog::scale_fill_solarized() + 
  labs(y = "Mean rating", 
       x = "", 
       fill = "Transgression") + 
  scale_y_continuous(breaks = seq(1,6, 1))
ggsave("Analyses/Figures/study1_mean_ratings.png", height = 3)
  
## ...Analyses ----
## Testing whether children's ratings differ by transgression type and age with following lmer: 
## Rating ~ Transgression type (moral; conventional)*Age + (Transgression type|Subject) + (age|item). 
## Pruning: We will iteratively remove item slopes, then subject slopes, then will remove item effects altogether if necessary
ratings.ms <- all.data %>%
  filter(q_kind == 0) 

#sanity check - every child should have two ratings
ratings.check <-  ratings.ms %>%
  group_by(subid)%>%
  summarise(n = n())%>%
  filter(n <2) #length = 0, so we're okay

set.seed(1234) #set random seed for reproducibility

## ..... India ----
ratings.india <- lmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                      data = subset(ratings.ms, site == "India")) # model fails to converge with full random effects structure
ratings.india <- lmer(answer ~ task*age.c + (task|subid) + (1|item), 
                      data = subset(ratings.ms, site == "India")) # singular fit removing item slope
ratings.india <- lmer(answer ~ task*age.c + (1|subid) + (1|item), 
                      data = subset(ratings.ms, site == "India")) # this converges
summary(ratings.india)

## .....Iran ----
ratings.iran <- lmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                      data = subset(ratings.ms, site == "Iran")) # singular fit
ratings.iran <- lmer(answer ~ task*age.c + (task|subid) + (1|item), 
                      data = subset(ratings.ms, site == "Iran")) # Model fails to converge
ratings.iran <- lmer(answer ~ task*age.c + (1|subid) + (1|item), 
                      data = subset(ratings.ms, site == "Iran")) # this converges
summary(ratings.iran)

## .....Canada ----
ratings.canada <- lmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                 data = subset(ratings.ms, site == "Canada")) # singular fit
ratings.canada <- lmer(answer ~ task*age.c + (task|subid) + (1|item), 
                 data = subset(ratings.ms, site == "Canada")) # Model fails to converge
ratings.canada <- lmer(answer ~ task*age.c + (1|subid) + (1|item), 
                     data = subset(ratings.ms, site == "Canada")) # this converges
summary(ratings.canada)

## .....Korea ----
ratings.korea <- lmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                       data = subset(ratings.ms, site == "Korea")) # singular fit
ratings.korea <- lmer(answer ~ task*age.c + (task|subid) + (1|item), 
                       data = subset(ratings.ms, site == "Korea")) # model converges
# ratings.korea <- lmer(answer ~ task*age.c + (1|subid) + (1|item), 
#                        data = subset(ratings.ms, site == "Korea")) # this converges
summary(ratings.korea)

# ... acceptability in different circumstances by site and task ----
## descriptives
all.data %>%
  filter(q_kind != 0)%>%
  group_by(site, task, q_kind)%>%
  summarise_at('answer', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(site, task, q_kind, n, mean, sd)

#... visualization ----
all.data %>%
  filter(q_kind != 0)%>%
  mutate(task = factor(task, levels = c("moral", "conv"), 
                       labels = c("Moral", "Conventional")), 
         q_kind_label = factor(q_kind_label, labels = c("If everyone else were doing it?", 
                                                        "In a faraway country?", 
                                                        "If there's a school rule?")))%>%
  group_by(site, task, q_kind_label)%>%
  langcog::multi_boot_standard("answer", na.rm = TRUE) %>%
  # group_by(subid, site, task)%>% #stopped here, brain dead
  ggplot(aes(x = site, y = mean, fill = task, color = task)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = .9), 
           color = "black") + 
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), 
                 position = position_dodge(width = .9), 
                 color = "black",
                 size = .7) +
  geom_hline(yintercept = .5, color = "grey", linetype = "dashed") + 
  facet_grid(~q_kind_label) + 
  # scale_fill_brewer(palette = "Dark2") +
  langcog::scale_fill_solarized()+
  labs(y = "Mean acceptability rating", 
       x = "Site", 
       fill = "Transgression")  +
  theme(axis.text.x = element_text(hjust = 1, angle = 45), 
        legend.key.size = unit(.5, 'cm'), 
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 9), 
        legend.margin=margin(t = 0, unit='cm')) +
  guides(fill = guide_legend(title.position = "top")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) + 
  ggtitle("Would it be okay...")
ggsave("Figures/study1_acceptability.png", width = 7, height = 3.5 )

## ...would it be okay if...(lmers) ----
acceptability.ms <- all.data %>%
  filter(q_kind != 0)

## ..... India ----
### Far away country
faraway.india <- glmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                      data = subset(acceptability.ms, q_kind == 1 & site == "India"), 
                      family = 'binomial') # fails to converge
faraway.india <- glmer(answer ~ task*age.c + (task|subid) + (1|item), 
                      data = subset(acceptability.ms, q_kind == 1 & site == "India"), 
                      family = 'binomial') # fails to converge
faraway.india <- glmer(answer ~ task*age.c + (1|subid) + (1|item), 
                      data = subset(acceptability.ms, q_kind == 1 & site == "India"), 
                      family = 'binomial') # singular fit
faraway.india <- glmer(answer ~ task*age.c + (1|subid), 
                       data = subset(acceptability.ms, q_kind == 1 & site == "India"), 
                       family = 'binomial') # converges
summary(faraway.india)

### If there were a rule at school
rule.india <- glmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                       data = subset(acceptability.ms, q_kind == 2 & site == "India"), 
                       family = 'binomial') # fails to converge
rule.india <- glmer(answer ~ task*age.c + (task|subid) + (1|item), 
                       data = subset(acceptability.ms, q_kind == 2 & site == "India"), 
                       family = 'binomial') # fails to converge
rule.india <- glmer(answer ~ task*age.c + (1|subid) + (1|item), 
                       data = subset(acceptability.ms, q_kind == 2 & site == "India"), 
                       family = 'binomial') # converges
summary(rule.india)

### If everyone else were doing it
everyone.india <- glmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                    data = subset(acceptability.ms, q_kind == 3 & site == "India"), 
                    family = 'binomial') # fails to converge
everyone.india <- glmer(answer ~ task*age.c + (task|subid) + (1|item), 
                    data = subset(acceptability.ms, q_kind == 3 & site == "India"), 
                    family = 'binomial') # fails to converge
everyone.india <- glmer(answer ~ task*age.c + (1|subid) + (1|item), 
                    data = subset(acceptability.ms, q_kind == 3 & site == "India"), 
                    family = 'binomial') # converges
summary(everyone.india)

## .....Iran ----
### Far away country
faraway.iran <- glmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                       data = subset(acceptability.ms, q_kind == 1 & site == "Iran"), 
                       family = 'binomial') # fails to converge
faraway.iran <- glmer(answer ~ task*age.c + (task|subid) + (1|item), 
                       data = subset(acceptability.ms, q_kind == 1 & site == "Iran"), 
                       family = 'binomial') # fails to converge
faraway.iran <- glmer(answer ~ task*age.c + (1|subid) + (1|item), 
                       data = subset(acceptability.ms, q_kind == 1 & site == "Iran"), 
                       family = 'binomial') # converges
summary(faraway.iran)

### If there were a rule at school
rule.iran <- glmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                    data = subset(acceptability.ms, q_kind == 2 & site == "Iran"), 
                    family = 'binomial') # fails to converge
rule.iran <- glmer(answer ~ task*age.c + (task|subid) + (1|item), 
                    data = subset(acceptability.ms, q_kind == 2 & site == "Iran"), 
                    family = 'binomial') # converges
summary(rule.iran)

### If everyone else were doing it
everyone.iran <- glmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                        data = subset(acceptability.ms, q_kind == 3 & site == "Iran"), 
                        family = 'binomial') # fails to converge
everyone.iran <- glmer(answer ~ task*age.c + (task|subid) + (1|item), 
                        data = subset(acceptability.ms, q_kind == 3 & site == "Iran"), 
                        family = 'binomial') # singular fit
everyone.iran <- glmer(answer ~ task*age.c + (1|subid) + (1|item), 
                        data = subset(acceptability.ms, q_kind == 3 & site == "Iran"), 
                        family = 'binomial') # converges
summary(everyone.india)

## .....Canada ----
### Far away country
faraway.canada <- glmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                      data = subset(acceptability.ms, q_kind == 1 & site == "Canada"), 
                      family = 'binomial') # fails to converge
faraway.canada <- glmer(answer ~ task*age.c + (task|subid) + (1|item), 
                      data = subset(acceptability.ms, q_kind == 1 & site == "Canada"), 
                      family = 'binomial') # fails to converge
faraway.canada <- glmer(answer ~ task*age.c + (1|subid) + (1|item), 
                      data = subset(acceptability.ms, q_kind == 1 & site == "Canada"), 
                      family = 'binomial') # converges
summary(faraway.canada)

### If there were a rule at school
rule.canada <- glmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                   data = subset(acceptability.ms, q_kind == 2 & site == "Canada"), 
                   family = 'binomial') # fails to converge
rule.canada <- glmer(answer ~ task*age.c + (task|subid) + (1|item), 
                   data = subset(acceptability.ms, q_kind == 2 & site == "Canada"), 
                   family = 'binomial') # singular
rule.canada <- glmer(answer ~ task*age.c + (1|subid) + (1|item), 
                     data = subset(acceptability.ms, q_kind == 2 & site == "Canada"), 
                     family = 'binomial') # converges
summary(rule.canada)

### If everyone else were doing it
everyone.canada <- glmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                       data = subset(acceptability.ms, q_kind == 3 & site == "Canada"), 
                       family = 'binomial') # fails to converge
everyone.canada <- glmer(answer ~ task*age.c + (task|subid) + (1|item), 
                       data = subset(acceptability.ms, q_kind == 3 & site == "Canada"), 
                       family = 'binomial') # singular fit
everyone.canada <- glmer(answer ~ task*age.c + (1|subid) + (1|item), 
                       data = subset(acceptability.ms, q_kind == 3 & site == "Canada"), 
                       family = 'binomial') # converges
summary(everyone.canada)

## .....Korea ----
### Far away country
faraway.korea <- glmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                        data = subset(acceptability.ms, q_kind == 1 & site == "Korea"), 
                        family = 'binomial') # fails to converge
faraway.korea <- glmer(answer ~ task*age.c + (task|subid) + (1|item), 
                        data = subset(acceptability.ms, q_kind == 1 & site == "Korea"), 
                        family = 'binomial') # converges
summary(faraway.korea)

### If there were a rule at school
rule.korea <- glmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                     data = subset(acceptability.ms, q_kind == 2 & site == "Korea"), 
                     family = 'binomial') # fails to converge
rule.korea <- glmer(answer ~ task*age.c + (task|subid) + (1|item), 
                     data = subset(acceptability.ms, q_kind == 2 & site == "Korea"), 
                     family = 'binomial') # singular
rule.korea <- glmer(answer ~ task*age.c + (1|subid) + (1|item), 
                     data = subset(acceptability.ms, q_kind == 2 & site == "Korea"), 
                     family = 'binomial') # converges
summary(rule.korea)

### If everyone else were doing it
everyone.korea <- glmer(answer ~ task*age.c + (task|subid) + (age.c|item), 
                         data = subset(acceptability.ms, q_kind == 3 & site == "Korea"), 
                         family = 'binomial') # fails to converge
everyone.korea <- glmer(answer ~ task*age.c + (task|subid) + (1|item), 
                         data = subset(acceptability.ms, q_kind == 3 & site == "Korea"), 
                         family = 'binomial') # singular fit
everyone.korea <- glmer(answer ~ task*age.c + (1|subid) + (1|item), 
                         data = subset(acceptability.ms, q_kind == 3 & site == "Korea"), 
                         family = 'binomial') # converges
summary(everyone.korea)


# Study 2: Descriptives ----
iran.study.2 %<>%
  mutate(age.bin = ifelse(age.group < 7, "5-6", 
                          ifelse((age.group >= 7 & age.group < 9), "7-8", 
                                 ifelse((age.group >= 9 & age.group < 11), "9-10", 
                                         ifelse((age.group >= 11 & age.group < 13), "11-12", "13-14")))))
  

#age info
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

#by sex
print(iran.study.2 %>%
        distinct(subid, site, age.bin, sex)%>%
        group_by(site, age.bin, sex)%>%
        summarise(n = n())%>%
        group_by(site)%>%
        mutate(total.site.n = sum(n)), n = 21)

# ... ratings for items by type (moral/conv/conv.iran/religious) ---

## descriptives
iran.study.2 %>%
  filter(q_kind == 0)%>%
  group_by(task)%>%
  summarise_at('answer', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~median(., na.rm=T),
                    ~min(., na.rm=T),
                    ~max(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(task, n, mean, sd, median, min, max)

# visualization
iran.study.2 %>% 
  filter(q_kind == 0)%>%
  mutate(task = factor(task, levels = c("moral", "conv.iran", "conv", "religious"), 
                       labels = c("Moral", "Conventional - Iran", "Conventional", "Religious")))%>%
  group_by(subid, task)%>%
  langcog::multi_boot_standard("answer", na.rm = TRUE) %>%
  ggplot(aes(x = task, y = mean, fill = task, color = task)) + 
  stat_summary(fun.y = mean, position = position_dodge(width = .9),
               geom="bar", alpha = .5, colour = "black") +
  geom_point(aes(x = task, y = mean, colour = task),
             position=position_jitter(width = .18, height = .035),
             size=1.5,
             alpha = .7,
             show.legend=FALSE,
             inherit.aes = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", geom="linerange", 
               position = position_dodge(width=0.9), size = 1, color = "black") +
  geom_hline(yintercept = 3.5, color = "grey", linetype = "dashed") + 
  theme(legend.position = "none") +
  langcog::scale_color_solarized() + 
  langcog::scale_fill_solarized() + 
  labs(y = "Mean rating", 
       x = "Transgression type") + 
  scale_y_continuous(breaks = seq(0,6, 1), 
                     limits = c(0, 6)) 
ggsave("Figures/iran_study2_mean_ratings.png", width = 4, height = 4)

## lmer - predicting ratings by moral and conventional by site
iran.ratings.ms <- iran.study.2 %>%
  filter(q_kind == 0)%>%
  group_by(subid, task)%>%
  langcog::multi_boot_standard("answer", na.rm = TRUE)

#now add age info
age.iran <- iran.study.2 %>%
  distinct(subid, age.c, age.group.floor, sex)

iran.ratings.ms <- left_join(iran.ratings.ms, age.iran, by = "subid")

set.seed(1234) #for reproducibility

## Main effects
fit_ratings.iran <- lmer(mean ~ task + sex + age.c + (1|subid),
                    data=iran.ratings.ms)
car::Anova(fit_ratings.iran) #main effects
tidy(fit_ratings.iran, conf.int=T) %>% #coefs, cis, p values
  mutate_at(c("estimate", "conf.low", "conf.high"), list(EXP=exp))

## Post-hoc pairwise comparison by task type
fit_rating_group.iran <- lmer(mean ~ task + sex + age.group.floor + (1|subid),
                         data=iran.ratings.ms, 
                         control = lmerControl(optimizer = "bobyqa"))
car::Anova(fit_rating_group.iran) #main effects
emmeans::emmeans(fit_rating_group.iran, list(pairwise~"task"), adjust = 'tukey')

# ... acceptability in different circumstances by task ----
#1 = faraway country
#2 = rule at school
#3 = everyone else doing it
#4 = foreigner
#5 = non-muslim did it
## descriptives
iran.study.2 %>%
  filter(q_kind == 1 | 
           q_kind == 2 | 
           q_kind == 3 | 
           q_kind == 4 |
           q_kind == 5 )%>%
  group_by(task, q_kind)%>%
  summarise_at('answer', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(task, q_kind, n, mean, sd)

##visualizations
iran.study.2 %>%
  filter(q_kind == 1 | 
           q_kind == 2 | 
           q_kind == 3 | 
           q_kind == 4 |
           q_kind == 5 ) %>%
  mutate(task = factor(task, levels = c("moral", "conv.iran", "conv", "religious"), 
                       labels = c("Moral", "Conventional - Iran", "Conventional", "Religious")), 
         q_kind_label = factor(q_kind_label, labels = c("If everyone else did it?", 
                                                        "In a faraway country?", 
                                                        "If a foreigner did it?", 
                                                        "If a non-Muslim did it?", 
                                                        "If there's a school rule?")))%>%
  group_by(task, q_kind_label)%>%
  langcog::multi_boot_standard("answer", na.rm = TRUE) %>%
  ggplot(aes(x = task, y = mean, fill = task)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = .9), 
           color = "black") + 
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), 
                 position = position_dodge(width = .9), 
                 size = .7) + 
  geom_hline(yintercept = .5, color = "grey", linetype = "dashed") + 
  scale_y_continuous(breaks = seq(0, 1, .25)) + 
  theme(legend.position = "none", 
        axis.text.x = element_text(hjust = 1, angle = 45))+
  langcog::scale_fill_solarized() +
  labs(x = "Transgression type", 
       y = "Mean acceptability rating") +
  facet_grid(~q_kind_label, scale = 'free_x') + 
  ggtitle("Would it be okay...")
ggsave("Figures/study2_acceptability.png", width = 8, height = 3.5)

##would it be okay if...(lmers)
acceptability.iran.ms <- iran.study.2 %>%
  filter(q_kind == 1 | 
           q_kind == 2 | 
           q_kind == 3 | 
           q_kind == 4 |
           q_kind == 5 )%>%
  group_by(subid, task, q_kind)%>%
  langcog::multi_boot_standard("answer", na.rm = TRUE)

#now add age info
age.iran <- iran.study.2 %>%
  distinct(subid, age.c, age.group.floor, sex)

acceptability.iran.ms <- left_join(acceptability.iran.ms, age.iran, by = "subid")

##...faraway country
#main effects
faraway.iran.2 <- lmer(mean ~ task + sex + age.c + (1|subid), 
                       data = subset(acceptability.iran.ms, q_kind == 1))
car::Anova(faraway.iran.2)
#pairwise
faraway.iran.2.group <- lmer(mean ~ task + sex + age.group.floor + (1|subid), 
                       data = subset(acceptability.iran.ms, q_kind == 1))
emmeans::emmeans(faraway.iran.2.group, list(pairwise~"task"), adjust = 'tukey')

##...rule at school
#main effects
rule.iran.2 <- lmer(mean ~ task + sex + age.c + (1|subid), 
                       data = subset(acceptability.iran.ms, q_kind == 2))
car::Anova(rule.iran.2)
#pairwise
rule.iran.2.group <- lmer(mean ~ task + sex + age.group.floor + (1|subid), 
                             data = subset(acceptability.iran.ms, q_kind == 2))
emmeans::emmeans(rule.iran.2.group, list(pairwise~"task"), adjust = 'tukey')

##...everyone else is doing it
#main effects
everyone.iran.2 <- lmer(mean ~ task + sex + age.c + (1|subid), 
                    data = subset(acceptability.iran.ms, q_kind == 3))
car::Anova(everyone.iran.2)
#pairwise
everyone.iran.2.group <- lmer(mean ~ task + sex + age.group.floor + (1|subid), 
                          data = subset(acceptability.iran.ms, q_kind == 3))
emmeans::emmeans(everyone.iran.2.group, list(pairwise~"task"), adjust = 'tukey')

##...foreigner
#main effects
foreigner.iran.2 <- lmer(mean ~ task + sex + age.c + (1|subid), 
                        data = subset(acceptability.iran.ms, q_kind == 4))
car::Anova(foreigner.iran.2)
#pairwise
foreigner.iran.2.group <- lmer(mean ~ task + sex + age.group.floor + (1|subid), 
                              data = subset(acceptability.iran.ms, q_kind == 4))
emmeans::emmeans(foreigner.iran.2.group, list(pairwise~"task"), adjust = 'tukey')

##...non-muslim
#main effects
nonmuslim.iran.2 <- lmer(mean ~ task + sex + age.c + (1|subid), 
                         data = subset(acceptability.iran.ms, q_kind == 5))
car::Anova(nonmuslim.iran.2)
#pairwise
nonmuslim.iran.2.group <- lmer(mean ~ task + sex + age.group.floor + (1|subid), 
                               data = subset(acceptability.iran.ms, q_kind == 5))
emmeans::emmeans(nonmuslim.iran.2.group, list(pairwise~"task"), adjust = 'tukey')


# ... should (transgression) be... ----
#6 = haram
#7 = illegal
#8 = zesht
## descriptives
iran.study.2 %>%
  filter(q_kind == 6 | 
           q_kind == 7 | 
           q_kind == 8)%>%
  group_by(task, q_kind)%>%
  summarise_at('answer', 
               list(~mean(., na.rm=T), 
                    ~sd(., na.rm=T),
                    ~sum(!is.na(.))))%>%
  dplyr::rename("n" = "sum")%>%
  dplyr::select(task, q_kind, n, mean, sd)

#visualization
iran.study.2 %>%
  filter(q_kind == 6 | 
           q_kind == 7 | 
           q_kind == 8)%>%
  mutate(task = factor(task, levels = c("moral", "conv.iran", "conv", "religious"), 
                       labels = c("Moral", "Conventional - Iran", "Conventional", "Religious")), 
         q_kind_label = factor(q_kind_label, labels = c("Haram", 
                                                        "Illegal", 
                                                        "Zesht")))%>%
  group_by(task, q_kind_label)%>%
  langcog::multi_boot_standard("answer", na.rm = TRUE) %>%
  ggplot(aes(x = task, y = mean, fill = task)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = .9), 
           color = "black") + 
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper), 
                 position = position_dodge(width = .9), 
                 size = .7) + 
  geom_hline(yintercept = .5, color = "grey", linetype = "dashed") + 
  scale_y_continuous(breaks = seq(0, 1, .25)) + 
  theme(legend.position = "none", 
        axis.text.x = element_text(hjust = 1, angle = 45))+
  langcog::scale_fill_solarized() +
  labs(x = "Transgression type", 
       y = "Mean rating") +
  facet_grid(~q_kind_label, scale = 'free_x') + 
  ggtitle("Would you classify this transgression as...")
ggsave("Figures/study2_classification.png", width = 7, height = 3.5)

#lmers
classify.iran.ms <- iran.study.2 %>%
  filter(q_kind == 6 | 
           q_kind == 7 | 
           q_kind == 8)%>%
  group_by(subid, task, q_kind)%>%
  langcog::multi_boot_standard("answer", na.rm = TRUE)

#now add age info
classify.iran.ms <- left_join(classify.iran.ms, age.iran, by = "subid")

##...haram
#main effects
haram.iran.2 <- lmer(mean ~ task + sex + age.c + (1|subid), 
                         data = subset(classify.iran.ms, q_kind == 6))
car::Anova(haram.iran.2)
#pairwise
haram.iran.2.group <- lmer(mean ~ task + sex + age.group.floor + (1|subid), 
                               data = subset(classify.iran.ms, q_kind == 6))
emmeans::emmeans(haram.iran.2.group, list(pairwise~"task"), adjust = 'tukey')

##...illegal
#main effects
illegal.iran.2 <- lmer(mean ~ task + sex + age.c + (1|subid), 
                     data = subset(classify.iran.ms, q_kind == 7))
car::Anova(illegal.iran.2)
#pairwise
illegal.iran.2.group <- lmer(mean ~ task + sex + age.group.floor + (1|subid), 
                           data = subset(classify.iran.ms, q_kind == 7))
emmeans::emmeans(illegal.iran.2.group, list(pairwise~"task"), adjust = 'tukey')

##...zesht
#main effects
zesht.iran.2 <- lmer(mean ~ task + sex + age.c + (1|subid), 
                       data = subset(classify.iran.ms, q_kind == 8))
car::Anova(zesht.iran.2)
#pairwise
zesht.iran.2.group <- lmer(mean ~ task + sex + age.group.floor + (1|subid), 
                             data = subset(classify.iran.ms, q_kind == 8))
emmeans::emmeans(zesht.iran.2.group, list(pairwise~"task"), adjust = 'tukey')


