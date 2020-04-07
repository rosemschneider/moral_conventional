#Data cleaning script for cross-cultural moral conventional study

rm(list = ls())
library(tidyverse)
library(magrittr)
library(tidylog)
library(stringr)

#filtering function
'%!in%' <- function(x,y)!('%in%'(x,y))

# NB
#Some subids have been further de-identified to ensure that participants cannot be identified. 
#Original and random SID are matched in a file stored on LADLab-internal server.
#Some DOBs and sex information are pulled from external roster, if they have were recorded on original data;
#All original rosters and identified data stored on LADLab-internal server. 

# There are different question types for each question. 
# 0 corresponds to base question: 1-6 scale (how bad/good is this)?
# 1: Would it be okay in a faraway country?
# 2: Would it be okay if there were a rule at school?
# 3: Would it be okay if everyone else were doing it? 

#There 2 different transgression types for study 1: 
# Moral, conventional

#There are 4 different transgression types for study 2: 
# Moral, conventional, convention-iran, religious

#There are different items within each transgression type: 
#Moral
#1. push - protagonist pushed a child down
#2. namecall: protagonist called a child a bad name 
#3. rip - protagonist ripped a child’s drawing 

#Conventional:
#1. shoes - protagonist put shoes up lunch table
#2. swim - protagonist wore swimsuit to school 
#3. teachername - protagonist called teacher by first name 
#4. toy - protagonist didn’t clean up toys 

# Study 1 ----

# Read in Data
## Data needs to be read in separately for each country
## Cleaned and handled separately, and then combined at the end of this script

## Korean Data ====
korea.data <- read.csv('../Data/Raw data/Study1/korea.csv')

korea.data %<>%
  dplyr::select(subid, age, age_group, task, q, q_kind, score, task_num, sex)%>%
  filter(task == "moral" | 
           task == "conv")%>%
  droplevels()%>%
  dplyr::rename("answer" = "score")%>%
  mutate(site = "Korea")

##mutate for items 
korea.data %<>%
  mutate(item = ifelse((task == "moral" & q == 1), "push", 
                       ifelse((task == "moral" & q == 2), "namecall",
                              ifelse((task == "moral" & q == 3),  "rip", 
                                     ifelse((task == "conv" & q == 1), "shoes", 
                                            ifelse((task == "conv" & q == 2), "swim", 
                                                   ifelse((task == "conv" & q == 3), "teachername", "toy")))))), 
         answer = ifelse(answer == "n", 0, 
                         ifelse(answer == "y", 1, 
                         ifelse(answer == "dk", NA, as.character(answer)))),
         answer = as.numeric(as.character(answer)))

## Iranian Data ===
#add q_kind convention
iran.data <- read.csv('../Data/Raw data/Study1/iran.csv')

#add q_kind column based on task number
iran.data %<>%
  mutate(q_kind = ifelse(str_detect(task_num, "\\b.1\\b", negate = FALSE), 1,
                         ifelse(str_detect(task_num, "\\b.2\\b", negate = FALSE), 2, 
                                ifelse(str_detect(task_num, "\\b.3\\b", negate = FALSE), 3, 0))))

#update to fit convention for merging
iran.data %<>%
  dplyr::select(subid, age, age_group, task, q, q_kind, score, task_num, sex)%>%
  mutate(site = "Iran", 
         score = ifelse(score == "N", 0, 
                        ifelse(score == "Y", 1, as.character(score))), 
         score = as.numeric(as.character(score))) %>%
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
                                                   ifelse((task == "conv" & q == 3), "teachername", "toy")))))))%>%
  mutate(answer = ifelse(answer == "N", 0, 
                         ifelse(answer =="Y", 1, answer)))

#canada
canada.data %<>%
  dplyr::select(subid, age, task, item, q, q_kind, answer, task_num, sex)%>%
  mutate(site = "Canada", 
         age = as.numeric(as.character(age)))

## Indian Data ====
#read in data from India; this does not have ages attached
india.data <- read.csv("../Data/Raw data/Study1/india.csv")%>%
  filter(task == "moral" | 
           task == "conv", 
         is.na(mcsp_target)) %>%
  droplevels() %>%#filter down only to moral/conventional data
  mutate(q = ifelse((task == "moral" & item == "push"), 1, 
                       ifelse((task == "moral" & item == "namecall"), 2,
                              ifelse((task == "moral" & item == "rip"),  3, 
                                     ifelse((task == "conv" & item == "shoes"), 1, 
                                            ifelse((task == "conv" & item == "swim"), 2, 
                                                   ifelse((task == "conv" & item == "teachername"), 3, 4)))))))%>%
  mutate(q_kind = str_extract(q_kind, "(\\d)+"))

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

# remove extraneous data by only taking kids with unique values with everything
india.data %<>%
  distinct(subid, task, item, q, q_kind, answer, task_num)

#check for double coding again
moral.conventional.check <- india.data %>%
  group_by(subid)%>%
  summarise(n =n())%>%
  filter(n > 28) #n = 2 kids who were coded 2x

#remove these 2 kids from data

## Filtering out this kiddo from data and roster
india.data %<>%
  filter(subid %!in% moral.conventional.check$subid)

india.roster %<>%
  filter(ID %!in% moral.conventional.check$subid)

# Pulling out the age and sex information to add to data frame
#pull out unique SID and age
india.sid.age <- india.roster %>%
  distinct(ID, age, Gender)%>%
  dplyr::rename("subid" = "ID", 
                "sex" = "Gender")

#update: get length of unique ids; we now have the same number of participants in each DF (217)
data.mc.unique <- as.vector(unique(india.data$subid)) #length = 215 unique subids
roster.mc.unique <- as.vector(unique(india.roster$ID)) #length = 215 unique subids

##left join by subid for age for the full india dataset
india.data <- left_join(india.data, india.sid.age, by = "subid")

#renaming and selecting the right columns
india.data %<>%
  dplyr::select(subid, age, task, item, q, q_kind, answer, task_num, sex)%>%
  mutate(site = "India", 
         answer = as.numeric(as.character(answer)),
         age = as.numeric(as.character(age)), 
         q_kind = as.numeric(q_kind))%>%
  filter(task != "practice") # filter out practice

##NAs in data %>%
na.check <- india.data %>%
  mutate(answer = factor(answer))%>%
  group_by(subid, answer)%>%
  summarise(n = n())%>%
  filter(is.na(answer))

##it looks like some kids did not have a complete dataset. Let's identify them.
incomplete <- india.data %>%
  filter(q_kind == 0)%>%
  mutate(missing.data = ifelse(is.na(answer), "MISSING", "NOT_MISSING"))%>%
  group_by(subid, missing.data)%>%
  summarise(n= n())%>%
  pivot_wider(names_from = missing.data, 
              values_from = n)%>%
  mutate(MISSING = ifelse(is.na(MISSING), 0, as.numeric(as.character(MISSING))), 
         NOT_MISSING = ifelse(is.na(NOT_MISSING), 0, as.numeric(as.character(NOT_MISSING))))

#how many kids have less than 80% of data 
max.missing <- incomplete %>%
  filter(MISSING >= 2)

##filter out kids who are missing more than 20% of their data
india.data %<>%
  filter(subid %!in% max.missing$subid)

# Merging data ====
#names: subid, age, task, item, q, q_kind, answer, task_num, site

#merge everything together
all.data <- bind_rows(india.data, iran.data, korea.data, canada.data)

## deal with non-numeric answers
all.data %<>%
  mutate(answer = ifelse(answer == "Y", 1, 
                             ifelse(answer == "N", 0, 
                             ifelse(answer == "n", 0, 
                                    ifelse(answer == "y", 1, 
                                           ifelse(answer == "dk", NA, 
                                                  ifelse(is.na(answer), NA, as.numeric(answer))))))))

#globally check for missing data 
incomplete <- all.data %>%
  filter(q_kind == 0)%>%
  mutate(missing.data = ifelse(is.na(answer), "MISSING", "NOT_MISSING"))%>%
  group_by(subid, missing.data)%>%
  summarise(n= n())%>%
  pivot_wider(names_from = missing.data, 
              values_from = n)%>%
  mutate(MISSING = ifelse(is.na(MISSING), 0, as.numeric(as.character(MISSING))), 
         NOT_MISSING = ifelse(is.na(NOT_MISSING), 0, as.numeric(as.character(NOT_MISSING))))

max.missing <- incomplete %>%
  filter(MISSING >= 2)

#remove these kids from full data frame
all.data %<>%
  filter(subid %!in% max.missing$subid)

#now check to make sure these kids have ratings data 
ratings.check <- all.data %>%
  filter(is.na(answer), 
         q_kind == 0) #n = 11 kids without ratings data - only one is missing two ratings. 
        #Exclude that kid (A0208), keep the remainder.

all.data %<>%
  filter(subid != "A0208")

all.data %<>%
  mutate(task = factor(task), 
         item = factor(item), 
         task_num = factor(task_num), 
         site = factor(site))

#create age groups and age center
all.data %<>%
  mutate(age.group = ifelse(age %% 1 >= 0.9, round(age), floor(age)), 
         age.c = as.vector(scale(age, center = TRUE, scale = TRUE)))

##filter out kids > 10 in india
all.data %<>%
  filter(age <= 11 | is.na(age))

##add q_kind labels for graphs
all.data %<>%
  mutate(q_kind_label = ifelse(q_kind == 0, "Severity rating", 
         ifelse(q_kind == 1, "Faraway country", 
                ifelse(q_kind == 2, "Rule at school", "Everyone else"))))
         

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
  dplyr::rename("answer" = "score", 
                "sex" = "gender") %>%
  mutate(site = "Iran - Study 2")

#fix one miscoding - one kid had rating has 20, but should be 2 (confirmed on paper data)
iran.study.2 %<>%
  mutate(answer = ifelse(answer == 20, 2, answer))

#center age, group age
iran.study.2 %<>%
  mutate(age.group = ifelse(age %% 1 >= 0.9, round(age), floor(age)), 
         age.c = as.vector(scale(age, center = TRUE, scale = TRUE))) 

##add q_kind labels for graphs
iran.study.2 %<>%
  mutate(q_kind_label = ifelse(q_kind == 0, "Severity rating", 
                               ifelse(q_kind == 1, "Farawary country", 
                                      ifelse(q_kind == 2, "Rule at school", 
                                             ifelse(q_kind == 3, "Everyone else", 
                                                    ifelse(q_kind == 4, "Foreigner", 
                                                           ifelse(q_kind == 5, "Non-muslim", 
                                                                  ifelse(q_kind == 6, "Haram", 
                                                                         ifelse(q_kind == 7, "Illegal", "Zesht")))))))))

#save and export
save(iran.study.2, file="../Data/Cleaned data/Study2_MC_iran.RData")
 
write.csv(iran.study.2, file="../Data/Cleaned data/Study2_MC_iran.csv")

##for sanity check
tmp <- all.data %>%
  group_by(subid)%>%
  summarise(n = n())

s1_distinct <- all.data %>%
  distinct(subid, site)

write.csv(s1_distinct, file = "../Data/sanity check/distinct_subs.csv")


