#Data cleaning script for cross-cultural moral conventional study

rm(list = ls())
library(tidyverse)
library(magrittr)
library(tidylog)
library(stringr)

#filtering function
'%!in%' <- function(x,y)!('%in%'(x,y))

# Study 1 ----

# Read in Data
## Data needs to be read in separately for each country
## Cleaned and handled separately, and then combined at the end of this script

## Korean Data ====
korea.data <- read.csv('../Data/Raw data/Study1/korea.csv', na.strings=c(""," ","NA", "NA "))

korea.data %<>%
  dplyr::select(subid, age, age_group, task, q, q_kind, score, task_num)%>%
  filter(task == "moral" | 
           task == "conv")%>%
  dplyr::rename("answer" = "score")%>%
  mutate(site = "Korea")

##mutate for items 
korea.data %<>%
  mutate(item = ifelse((task == "moral" & q == 1), "push", 
                       ifelse((task == "moral" & q == 2), "namecall",
                              ifelse((task == "moral" & q == 3),  "rip", 
                                     ifelse((task == "conv" & q == 1), "shoes", 
                                            ifelse((task == "conv" & q == 2), "swim", 
                                                   ifelse((task == "conv" & q == 3), "teachername", "toy")))))))

## Iranian Data ===
#add q_kind convention
iran.data <- read.csv('../Data/Raw data/Study1/iran.csv', na.strings=c(""," ","NA", "NA "))

#add q_kind column based on task number
iran.data %<>%
  mutate(q_kind = ifelse(str_detect(task_num, "\\b.1\\b", negate = FALSE), 1,
                         ifelse(str_detect(task_num, "\\b.2\\b", negate = FALSE), 2, 
                                ifelse(str_detect(task_num, "\\b.3\\b", negate = FALSE), 3, 0))))

#update to fit convention for merging
iran.data %<>%
  dplyr::select(subid, age, age_group, task, q, q_kind, score, task_num)%>%
  mutate(site = "Iran") %>%
  filter(task == "moral" | 
           task == "conv")%>%
  dplyr::rename("answer" = "score")

#add question convention
iran.data %<>%
  mutate(item = ifelse((task == "moral" & q == 1), "push", 
                       ifelse((task == "moral" & q == 2), "namecall",
                              ifelse((task == "moral" & q == 3),  "rip", 
                                     ifelse((task == "conv" & q == 1), "shoes", 
                                            ifelse((task == "conv" & q == 2), "swim", 
                                                   ifelse((task == "conv" & q == 3), "teachername", "toy")))))))

## Canadian Data ====
##canada data
canada.data <- read.csv("../Data/Raw data/Study1/canada.csv")

#add question naming convention
canada.data %<>%
  mutate(item = ifelse((task == "moral" & q == 1), "push", 
                       ifelse((task == "moral" & q == 2), "namecall",
                              ifelse((task == "moral" & q == 3),  "rip", 
                                     ifelse((task == "conv" & q == 1), "shoes", 
                                            ifelse((task == "conv" & q == 2), "swim", 
                                                   ifelse((task == "conv" & q == 3), "teachername", "toy")))))))

#canada
canada.data %<>%
  dplyr::select(subid, age, task, item, q, q_kind, answer, task_num)%>%
  mutate(site = "Canada", 
         age = as.numeric(as.character(age)))

## Indian Data ====
#read in data from India; this does not have ages attached
india.data <- read.csv("../Data/Raw data/Study1/india.csv", na.strings=c(""," ","NA", "NA "))%>%
  filter(task == "moral" | 
           task == "conv", 
         is.na(mcsp_target)) %>% #filter down only to moral/conventional data
  mutate(q_kind_num = ifelse(str_detect(q_kind, "0\\b", negate = FALSE), 1,
                  ifelse(str_detect(q_kind, "1\\b", negate = FALSE), 2, 
                         ifelse(str_detect(q_kind, "2\\b", negate = FALSE), 3, 0)))) ##THIS NEEDS TO BE FIXED

#get unique SIDs from india.data
data.mc.unique <- as.vector(unique(india.data$subid)) #length = 229 unique subids

##read in roster from India
##this roster has the ages of all kids
## Note: Ages were calculated from a private full roster that has both testing date and DOB. 
## Note: Exact DOT not known, calculated as the average of dates that experimenters were testing
## Note: This roster contains only a subset of children; others were tested on other studies
india.roster <- read.csv("../Data/Raw data/Study1/india_mc_roster.csv", na.strings=c(""," ","NA", "NA ", "#VALUE!"))

#clean india roster to remove NAs and obviously incorrect ages
india.roster %<>%
  filter(!is.na(age), 
         age < 50)

#get unique IDs from roster for kids who have MC
roster.mc.unique <- as.vector(unique(india.roster$ID)) #length = 230 unique subids

##now get the SIDs that appear in data but are not on the roster
missing.india.sids <- india.data %>%
  filter(subid %!in% roster.mc.unique)%>%
  distinct(subid) #n = 12

## These are participants who do not have DOBs; we are excluding them from data
india.data %<>%
  filter(subid %!in% missing.india.sids$subid)

#UPDATE: get unique SIDs from india.data
data.mc.unique <- as.vector(unique(india.data$subid)) #length = 217 unique subids

##get the SIDs who are on the roster, but for whom we do not have data
missing.india.sids <- india.roster %>%
  filter(ID %!in% data.mc.unique)%>%
  distinct(ID) #n = 13

#these are kids who only completed dictator game; we do not have data for them. Filter them out of roster.
india.roster %<>%
  filter(ID %!in% missing.india.sids$ID)

#update: get length of unique ids; we now have the same number of participants in each DF (217)
data.mc.unique <- as.vector(unique(india.data$subid)) #length = 217 unique subids
roster.mc.unique <- as.vector(unique(india.roster$ID)) #length = 217 unique subids

## dealing with kids who are coded 2x
moral.conventional.check <- india.data %>%
  group_by(subid)%>%
  summarise(n =n())%>%
  filter(n > 28) #n = 55 kids who were coded 2x

#make sure their data is the same for both trials 
double.data.check <- india.data %>%
  filter(subid %in% moral.conventional.check$subid)%>%
  group_by(subid, q, answer)%>%
  summarise(n = n()) %>%
  filter(n < 2) #A0603 suspect

## Filtering out this kiddo from data and roster
india.data %<>%
  filter(subid %!in% double.data.check$subid)

india.roster %<>%
  filter(ID %!in% double.data.check$subid)

# Pulling out the age information to add to data frame
#pull out unique SID and age
india.sid.age <- india.roster %>%
  distinct(ID, age)%>%
  dplyr::rename("subid" = "ID")

##left join by subid for age for the full india dataset
india.data <- left_join(india.data, india.sid.age, by = "subid")

#renaming and selecting the right columns
india.data %<>%
  dplyr::select(subid, age, task, item, q, q_kind, answer, task_num)%>%
  mutate(q = as.numeric(as.character(substr(q, 2, 2))), 
         q_kind = as.numeric(as.character(substr(q_kind, 1,1))), 
         site = "India", 
         age = as.numeric(as.character(age)))%>%
  filter(task != "practice") # filter out practice

# Merging data ====
#names: subid, age, task, item, q, q_kind, answer, task_num, site

#merge everything together
all.data <- bind_rows(india.data, iran.data, korea.data, canada.data)

## deal with non-numeric answers
non.num <- all.data %>%
  mutate(answer.num = ifelse(answer == "Y", 1, 
                             ifelse(answer == "N", 0, 
                             ifelse(answer == "n", 0, 
                                    ifelse(answer == "y", 1, 
                                           ifelse(answer == "dk", NA, 
                                                  ifelse(is.na(answer), NA, as.numeric(answer))))))))

#save and export
save(all.data, file="../Data/Cleaned data/Study1_MC_all_data.RData")

write.csv(all.data, file="../Data/Cleaned data/Study1_MC_all_data.csv")

# Study 2 ----
#read in data 
iran.study.2 <- read.csv("../Data/Raw data/Study2/iran_study2_data.csv",
                         na.strings = c("", " ", "NA ", "NA", "#VALUE!"))

iran.study.2 %<>%
  mutate(age = as.numeric(as.character(age)))%>% 
  filter(!is.na(age))%>% #one kid without dob
  dplyr::rename("answer" = "score") %>%
  mutate(site = "Iran - Study 2")

#save and export
save(iran.study.2, file="../Data/Cleaned data/Study2_MC_iran.RData")

write.csv(iran.study.2, file="../Data/Cleaned data/Study2_MC_iran.csv")
