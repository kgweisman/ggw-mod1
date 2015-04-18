# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(jsonlite)
library(stats)

# clear environment
rm(list=ls())

# --- READING IN DATA OBJECTS -------------------------------------------------

# ----------> US run-01 (2015-03-13) ------------------------------------------

# set working directory
setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1/turk/run-01/")

# mike's json for-loop
files <- dir("production-results/")

d.raw_01 <- data.frame()

for(i in 1:length(files)) {
  # gather files
  f = files[i]
  jf <- paste("production-results/",f,sep="")
  
  # parse JSON object
  jd <- fromJSON(paste(readLines(jf), collapse=""))
  
  # store relevant variables in dataframe 
  id <- data.frame(
    matrix(
    data = c("subid", "condition", "age", "gender", "ethnicity", "education", 
               "religionChild", "religionNow", "job", "d.raw_02atus", 
               "children", "country", "englishNative", "politicalIdeology", 
               "studyMoralPhil", "vegetarian", "beliefGod", "beliefAfterlife", 
               "beliefTradition", "beliefRules", "beliefLeader", "comments", 
               "trialNum", "leftCharacter", "rightCharacter", "response", "rt"),
    nrow = 78, ncol = 0))

  # subject-level data: identity
  id$subid = paste0("S01",i)
  id$condition = jd$answers$data$newData$condition
  
  # subject-level data: demographics
  id$age = ifelse(jd$answers$data$newData$age == "", "NA", jd$answers$data$newData$age)
  id$gender = ifelse(jd$answers$data$newData$gender == "", "NA", jd$answers$data$newData$gender)
#   id$ethnicity = ifelse(jd$answers$data$newData$ethnicity == "", "NA", jd$answers$data$newData$ethnicity)
  id$education = ifelse(jd$answers$data$newData$education == "", "NA", jd$answers$data$newData$education)
#   id$religionChild = ifelse(jd$answers$data$newData$religionChild == "", "NA", jd$answers$data$newData$religionChild)
#   id$religionNow = ifelse(jd$answers$data$newData$religionNow == "", "NA", jd$answers$data$newData$religionNow)
  id$job = ifelse(jd$answers$data$newData$job == "", "NA", jd$answers$data$newData$job)
#   id$maritalStatus = ifelse(jd$answers$data$newData$maritalStatus == "", "NA", jd$answers$data$newData$maritalStatus)
  id$children = ifelse(jd$answers$data$newData$children == "", "NA", jd$answers$data$newData$children)
  id$country = ifelse(jd$answers$data$newData$country == "", "NA", jd$answers$data$newData$country)
#   id$englishNative = ifelse(jd$answers$data$newData$englishNative == "", "NA", jd$answers$data$newData$englishNative)
  id$politicalIdeology = ifelse(jd$answers$data$newData$politicalIdeology == "", "NA", jd$answers$data$newData$politicalIdeology)
#   id$studyMoralPhil = ifelse(jd$answers$data$newData$studyMoralPhil == "", "NA", jd$answers$data$newData$studyMoralPhil)
#   id$vegetarian = ifelse(jd$answers$data$newData$vegetarian == "", "NA", jd$answers$data$newData$vegetarian)
#   id$beliefGod = ifelse(jd$answers$data$newData$beliefGod == "", "NA", jd$answers$data$newData$beliefGod)
  id$beliefAfterlife = ifelse(jd$answers$data$newData$beliefAfterlife == "", "NA", jd$answers$data$newData$beliefAfterlife)
#   id$beliefTradition = ifelse(jd$answers$data$newData$beliefTradition == "", "NA", jd$answers$data$newData$beliefTradition)
#   id$beliefRules = ifelse(jd$answers$data$newData$beliefRules == "", "NA", jd$answers$data$newData$beliefRules)
  id$beliefLeader = ifelse(jd$answers$data$newData$beliefLeader == "", "NA", jd$answers$data$newData$beliefLeader)
  id$comments = ifelse(jd$answers$data$newData$comments == "", "NA", jd$answers$data$newData$comments)
  
  # trial-level data:                    
  id$trialNum = jd$answers$data$newData$trialData$trialNum
  id$leftCharacter = jd$answers$data$newData$trialData$leftCharacter
  id$rightCharacter = jd$answers$data$newData$trialData$rightCharacter
  id$response = jd$answers$data$newData$trialData$response
  id$rt = jd$answers$data$newData$trialData$rt

  # bind into same dataframe
  d.raw_01 <- bind_rows(d.raw_01, id)
}

glimpse(d.raw_01)

# ----------> US run-02 (2015-03-16) ------------------------------------------

# set working directory
setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1/turk/run-02/")

# mike's json for-loop
files <- dir("production-results/")

d.raw_02 <- data.frame()

for(i in 1:length(files)) {
  # gather files
  f = files[i]
  jf <- paste("production-results/",f,sep="")
  
  # parse JSON object
  jd <- fromJSON(paste(readLines(jf), collapse=""))
  
  # store relevant variables in dataframe 
  id <- data.frame(
    matrix(
      data = c("subid", "condition", "age", "gender", "ethnicity", "education", 
               "religionChild", "religionNow", "job", "maritalStatus", 
               "children", "country", "englishNative", "politicalIdeology", 
               "studyMoralPhil", "vegetarian", "beliefGod", "beliefAfterlife", 
               "beliefTradition", "beliefRules", "beliefLeader", "comments", 
               "trialNum", "leftCharacter", "rightCharacter", "response", "rt"),
      nrow = 78, ncol = 0))
  
  # subject-level data: identity
  id$subid = paste0("S02",i)
  id$condition = jd$answers$data$newData$condition
  
  # subject-level data: demographics
  id$age = ifelse(jd$answers$data$newData$age == "", "NA", jd$answers$data$newData$age)
  id$gender = ifelse(jd$answers$data$newData$gender == "", "NA", jd$answers$data$newData$gender)
#   id$ethnicity = ifelse(jd$answers$data$newData$ethnicity == "", "NA", jd$answers$data$newData$ethnicity)
  id$education = ifelse(jd$answers$data$newData$education == "", "NA", jd$answers$data$newData$education)
#   id$religionChild = ifelse(jd$answers$data$newData$religionChild == "", "NA", jd$answers$data$newData$religionChild)
  #   id$religionNow = ifelse(jd$answers$data$newData$religionNow == "", "NA", jd$answers$data$newData$religionNow)
  id$job = ifelse(jd$answers$data$newData$job == "", "NA", jd$answers$data$newData$job)
#   id$maritalStatus = ifelse(jd$answers$data$newData$maritalStatus == "", "NA", jd$answers$data$newData$maritalStatus)
  id$children = ifelse(jd$answers$data$newData$children == "", "NA", jd$answers$data$newData$children)
  id$country = ifelse(jd$answers$data$newData$country == "", "NA", jd$answers$data$newData$country)
#   id$englishNative = ifelse(jd$answers$data$newData$englishNative == "", "NA", jd$answers$data$newData$englishNative)
  id$politicalIdeology = ifelse(jd$answers$data$newData$politicalIdeology == "", "NA", jd$answers$data$newData$politicalIdeology)
#   id$studyMoralPhil = ifelse(jd$answers$data$newData$studyMoralPhil == "", "NA", jd$answers$data$newData$studyMoralPhil)
  #   id$vegetarian = ifelse(jd$answers$data$newData$vegetarian == "", "NA", jd$answers$data$newData$vegetarian)
#   id$beliefGod = ifelse(jd$answers$data$newData$beliefGod == "", "NA", jd$answers$data$newData$beliefGod)
  id$beliefAfterlife = ifelse(jd$answers$data$newData$beliefAfterlife == "", "NA", jd$answers$data$newData$beliefAfterlife)
#   id$beliefTradition = ifelse(jd$answers$data$newData$beliefTradition == "", "NA", jd$answers$data$newData$beliefTradition)
  #   id$beliefRules = ifelse(jd$answers$data$newData$beliefRules == "", "NA", jd$answers$data$newData$beliefRules)
  id$beliefLeader = ifelse(jd$answers$data$newData$beliefLeader == "", "NA", jd$answers$data$newData$beliefLeader)
  id$comments = ifelse(jd$answers$data$newData$comments == "", "NA", jd$answers$data$newData$comments)
  
  # trial-level data:                    
  id$trialNum = jd$answers$data$newData$trialData$trialNum
  id$leftCharacter = jd$answers$data$newData$trialData$leftCharacter
  id$rightCharacter = jd$answers$data$newData$trialData$rightCharacter
  id$response = jd$answers$data$newData$trialData$response
  id$rt = jd$answers$data$newData$trialData$rt
  
  # bind into same dataframe
  d.raw_02 <- bind_rows(d.raw_02, id)
}

glimpse(d.raw_02)

# ----------> US run-03 (2015-03-26) ------------------------------------------

# set working directory
setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1/turk/run-03/")

# mike's json for-loop
files <- dir("production-results/")

d.raw_03 <- data.frame()

for(i in 1:length(files)) {
  # gather files
  f = files[i]
  jf <- paste("production-results/",f,sep="")
  
  # parse JSON object
  jd <- fromJSON(paste(readLines(jf), collapse=""))
  
  # store relevant variables in dataframe 
  id <- data.frame(
    matrix(
      data = c("subid", "condition", "age", "gender", "ethnicity", "education", 
               "religionChild", "religionNow", "job", "maritalStatus", 
               "children", "country", "englishNative", "politicalIdeology", 
               "studyMoralPhil", "vegetarian", "beliefGod", "beliefAfterlife", 
               "beliefTradition", "beliefRules", "beliefLeader", "comments", 
               "trialNum", "leftCharacter", "rightCharacter", "response", "rt"),
      nrow = 78, ncol = 0))
  
  # subject-level data: identity
  id$subid = paste0("S03",i)
  id$condition = jd$answers$data$newData$condition
  
  # subject-level data: demographics
  id$age = ifelse(jd$answers$data$newData$age == "", "NA", jd$answers$data$newData$age)
  id$gender = ifelse(jd$answers$data$newData$gender == "", "NA", jd$answers$data$newData$gender)
  #   id$ethnicity = ifelse(jd$answers$data$newData$ethnicity == "", "NA", jd$answers$data$newData$ethnicity)
  id$education = ifelse(jd$answers$data$newData$education == "", "NA", jd$answers$data$newData$education)
  #   id$religionChild = ifelse(jd$answers$data$newData$religionChild == "", "NA", jd$answers$data$newData$religionChild)
  #   id$religionNow = ifelse(jd$answers$data$newData$religionNow == "", "NA", jd$answers$data$newData$religionNow)
  id$job = ifelse(jd$answers$data$newData$job == "", "NA", jd$answers$data$newData$job)
  #   id$maritalStatus = ifelse(jd$answers$data$newData$maritalStatus == "", "NA", jd$answers$data$newData$maritalStatus)
  id$children = ifelse(jd$answers$data$newData$children == "", "NA", jd$answers$data$newData$children)
  id$country = ifelse(jd$answers$data$newData$country == "", "NA", jd$answers$data$newData$country)
  #   id$englishNative = ifelse(jd$answers$data$newData$englishNative == "", "NA", jd$answers$data$newData$englishNative)
  id$politicalIdeology = ifelse(jd$answers$data$newData$politicalIdeology == "", "NA", jd$answers$data$newData$politicalIdeology)
  #   id$studyMoralPhil = ifelse(jd$answers$data$newData$studyMoralPhil == "", "NA", jd$answers$data$newData$studyMoralPhil)
  #   id$vegetarian = ifelse(jd$answers$data$newData$vegetarian == "", "NA", jd$answers$data$newData$vegetarian)
  #   id$beliefGod = ifelse(jd$answers$data$newData$beliefGod == "", "NA", jd$answers$data$newData$beliefGod)
  id$beliefAfterlife = ifelse(jd$answers$data$newData$beliefAfterlife == "", "NA", jd$answers$data$newData$beliefAfterlife)
  #   id$beliefTradition = ifelse(jd$answers$data$newData$beliefTradition == "", "NA", jd$answers$data$newData$beliefTradition)
  #   id$beliefRules = ifelse(jd$answers$data$newData$beliefRules == "", "NA", jd$answers$data$newData$beliefRules)
  id$beliefLeader = ifelse(jd$answers$data$newData$beliefLeader == "", "NA", jd$answers$data$newData$beliefLeader)
  id$comments = ifelse(jd$answers$data$newData$comments == "", "NA", jd$answers$data$newData$comments)
  
  # trial-level data:                    
  id$trialNum = jd$answers$data$newData$trialData$trialNum
  id$leftCharacter = jd$answers$data$newData$trialData$leftCharacter
  id$rightCharacter = jd$answers$data$newData$trialData$rightCharacter
  id$response = jd$answers$data$newData$trialData$response
  id$rt = jd$answers$data$newData$trialData$rt
  
  # bind into same dataframe
  d.raw_03 <- bind_rows(d.raw_03, id)
}

glimpse(d.raw_03)

# ----------> US run-04 (2015-03-26) ------------------------------------------

# set working directory
setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1/turk/run-04/")

# mike's json for-loop
files <- dir("production-results/")

d.raw_04 <- data.frame()

for(i in 1:length(files)) {
  # gather files
  f = files[i]
  jf <- paste("production-results/",f,sep="")
  
  # parse JSON object
  jd <- fromJSON(paste(readLines(jf), collapse=""))
  
  # store relevant variables in dataframe 
  id <- data.frame(
    matrix(
      data = c("subid", "condition", "age", "gender", "ethnicity", "education", 
               "religionChild", "religionNow", "job", "maritalStatus", 
               "children", "country", "englishNative", "politicalIdeology", 
               "studyMoralPhil", "vegetarian", "beliefGod", "beliefAfterlife", 
               "beliefTradition", "beliefRules", "beliefLeader", "comments", 
               "trialNum", "leftCharacter", "rightCharacter", "response", "rt"),
      nrow = 78, ncol = 0))
  
  # subject-level data: identity
  id$subid = paste0("S04",i)
  id$condition = jd$answers$data$newData$condition
  
  # subject-level data: demographics
  id$age = ifelse(jd$answers$data$newData$age == "", "NA", jd$answers$data$newData$age)
  id$gender = ifelse(jd$answers$data$newData$gender == "", "NA", jd$answers$data$newData$gender)
  #   id$ethnicity = ifelse(jd$answers$data$newData$ethnicity == "", "NA", jd$answers$data$newData$ethnicity)
  id$education = ifelse(jd$answers$data$newData$education == "", "NA", jd$answers$data$newData$education)
  #   id$religionChild = ifelse(jd$answers$data$newData$religionChild == "", "NA", jd$answers$data$newData$religionChild)
  #   id$religionNow = ifelse(jd$answers$data$newData$religionNow == "", "NA", jd$answers$data$newData$religionNow)
  id$job = ifelse(jd$answers$data$newData$job == "", "NA", jd$answers$data$newData$job)
  #   id$maritalStatus = ifelse(jd$answers$data$newData$maritalStatus == "", "NA", jd$answers$data$newData$maritalStatus)
  id$children = ifelse(jd$answers$data$newData$children == "", "NA", jd$answers$data$newData$children)
  id$country = ifelse(jd$answers$data$newData$country == "", "NA", jd$answers$data$newData$country)
  #   id$englishNative = ifelse(jd$answers$data$newData$englishNative == "", "NA", jd$answers$data$newData$englishNative)
  id$politicalIdeology = ifelse(jd$answers$data$newData$politicalIdeology == "", "NA", jd$answers$data$newData$politicalIdeology)
  #   id$studyMoralPhil = ifelse(jd$answers$data$newData$studyMoralPhil == "", "NA", jd$answers$data$newData$studyMoralPhil)
  #   id$vegetarian = ifelse(jd$answers$data$newData$vegetarian == "", "NA", jd$answers$data$newData$vegetarian)
  #   id$beliefGod = ifelse(jd$answers$data$newData$beliefGod == "", "NA", jd$answers$data$newData$beliefGod)
  id$beliefAfterlife = ifelse(jd$answers$data$newData$beliefAfterlife == "", "NA", jd$answers$data$newData$beliefAfterlife)
  #   id$beliefTradition = ifelse(jd$answers$data$newData$beliefTradition == "", "NA", jd$answers$data$newData$beliefTradition)
  #   id$beliefRules = ifelse(jd$answers$data$newData$beliefRules == "", "NA", jd$answers$data$newData$beliefRules)
  id$beliefLeader = ifelse(jd$answers$data$newData$beliefLeader == "", "NA", jd$answers$data$newData$beliefLeader)
  id$comments = ifelse(jd$answers$data$newData$comments == "", "NA", jd$answers$data$newData$comments)
  
  # trial-level data:                    
  id$trialNum = jd$answers$data$newData$trialData$trialNum
  id$leftCharacter = jd$answers$data$newData$trialData$leftCharacter
  id$rightCharacter = jd$answers$data$newData$trialData$rightCharacter
  id$response = jd$answers$data$newData$trialData$response
  id$rt = jd$answers$data$newData$trialData$rt
  
  # bind into same dataframe
  d.raw_04 <- bind_rows(d.raw_04, id)
}

glimpse(d.raw_04)

# ----------> India run-01 (2015-04-17) ---------------------------------------

# set working directory
setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1_india/turk/india_run-01/")

# mike's json for-loop
files <- dir("production-results/")

d.raw_india_01 <- data.frame()

for(i in 1:length(files)) {
  # gather files
  f = files[i]
  jf <- paste("production-results/",f,sep="")
  
  # parse JSON object
  jd <- fromJSON(paste(readLines(jf), collapse=""))
  
  # store relevant variables in dataframe 
  id <- data.frame(
    matrix(
      data = c("subid", "condition", "age", "gender", "ethnicity", "education", 
               "religionChild", "religionNow", "job", "d.raw_02atus", 
               "children", "country", "englishNative", "politicalIdeology", 
               "studyMoralPhil", "vegetarian", "beliefGod", "beliefAfterlife", 
               "beliefTradition", "beliefRules", "beliefLeader", "comments", 
               "trialNum", "leftCharacter", "rightCharacter", "response", "rt"),
      nrow = 78, ncol = 0))
  
  # subject-level data: identity
  id$subid = paste0("Sindia_01",i)
  id$condition = jd$answers$data$newData$condition
  
  # subject-level data: demographics
  id$age = ifelse(jd$answers$data$newData$age == "", "NA", jd$answers$data$newData$age)
  id$gender = ifelse(jd$answers$data$newData$gender == "", "NA", jd$answers$data$newData$gender)
  #   id$ethnicity = ifelse(jd$answers$data$newData$ethnicity == "", "NA", jd$answers$data$newData$ethnicity)
  id$education = ifelse(jd$answers$data$newData$education == "", "NA", jd$answers$data$newData$education)
  #   id$religionChild = ifelse(jd$answers$data$newData$religionChild == "", "NA", jd$answers$data$newData$religionChild)
  #   id$religionNow = ifelse(jd$answers$data$newData$religionNow == "", "NA", jd$answers$data$newData$religionNow)
  id$job = ifelse(jd$answers$data$newData$job == "", "NA", jd$answers$data$newData$job)
  #   id$maritalStatus = ifelse(jd$answers$data$newData$maritalStatus == "", "NA", jd$answers$data$newData$maritalStatus)
  id$children = ifelse(jd$answers$data$newData$children == "", "NA", jd$answers$data$newData$children)
  id$country = ifelse(jd$answers$data$newData$country == "", "NA", jd$answers$data$newData$country)
  #   id$englishNative = ifelse(jd$answers$data$newData$englishNative == "", "NA", jd$answers$data$newData$englishNative)
  id$politicalIdeology = ifelse(jd$answers$data$newData$politicalIdeology == "", "NA", jd$answers$data$newData$politicalIdeology)
  #   id$studyMoralPhil = ifelse(jd$answers$data$newData$studyMoralPhil == "", "NA", jd$answers$data$newData$studyMoralPhil)
  #   id$vegetarian = ifelse(jd$answers$data$newData$vegetarian == "", "NA", jd$answers$data$newData$vegetarian)
  #   id$beliefGod = ifelse(jd$answers$data$newData$beliefGod == "", "NA", jd$answers$data$newData$beliefGod)
  id$beliefAfterlife = ifelse(jd$answers$data$newData$beliefAfterlife == "", "NA", jd$answers$data$newData$beliefAfterlife)
  #   id$beliefTradition = ifelse(jd$answers$data$newData$beliefTradition == "", "NA", jd$answers$data$newData$beliefTradition)
  #   id$beliefRules = ifelse(jd$answers$data$newData$beliefRules == "", "NA", jd$answers$data$newData$beliefRules)
  id$beliefLeader = ifelse(jd$answers$data$newData$beliefLeader == "", "NA", jd$answers$data$newData$beliefLeader)
  id$comments = ifelse(jd$answers$data$newData$comments == "", "NA", jd$answers$data$newData$comments)
  
  # trial-level data:                    
  id$trialNum = jd$answers$data$newData$trialData$trialNum
  id$leftCharacter = jd$answers$data$newData$trialData$leftCharacter
  id$rightCharacter = jd$answers$data$newData$trialData$rightCharacter
  id$response = jd$answers$data$newData$trialData$response
  id$rt = jd$answers$data$newData$trialData$rt
  
  # bind into same dataframe
  d.raw_india_01 <- bind_rows(d.raw_india_01, id)
}

glimpse(d.raw_india_01)

# --- TIDYING -----------------------------------------------------------------

# clean up variables
d_tidy = full_join(d.raw_01, d.raw_02) %>%
  full_join(d.raw_03) %>%
  full_join(d.raw_04) %>%
  full_join(d.raw_india_01) %>%
  mutate(subid = factor(subid),
         condition = ifelse(condition == "Emotion Recognition",
                            "EmotionRecognition",
                            condition),
         condition = factor(condition),
         age = as.numeric(age),
         gender = factor(gender),
#          ethnicity = factor(ethnicity), # redo for multiple selected
         education = factor(education),
#          religionChild = factor(religionChild), # redo for multiple selected
#          religionNow = factor(religionNow), # redo for multiple selected
         children = factor(children),
#          englishNative = factor(englishNative),
         politicalIdeology = factor(politicalIdeology),         
#          studyMoralPhil = factor(studyMoralPhil),         
#          vegetarian = factor(vegetarian),         
#          beliefGod = factor(beliefGod),         
         beliefAfterlife = factor(beliefAfterlife),         
#          beliefTradition = factor(beliefTradition),         
#          beliefRules = factor(beliefRules),         
         beliefLeader = factor(beliefLeader),         
         leftCharacter = factor(leftCharacter),         
         rightCharacter = factor(rightCharacter),         
         response = factor(response),
         responseNum =
           ifelse(response == "much more left", -2,
                  ifelse(response == "slightly more left", -1,
                         ifelse(response == "both equally", 0,
                                ifelse(response == "slightly more right", 1,
                                       ifelse(response == "much more right", 2, NA)))))
         )

glimpse(d_tidy)

# --- WRITING ANONYMIZED CSV --------------------------------------------------

# write to de-identified csv file
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1/data/run-01&02&03&04&india01_2015-04-17_data_anonymized.csv")

d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1/data/run-01&02&03&04&india01_2015-04-17_data_anonymized.csv")[-1] # get rid of column of obs numbers

# view comments
comments = d %>%
#   filter(country == "india") %>%
  select(comments, condition, subid) %>%
  distinct() %>%
  filter(comments != "NA")

View(comments)
