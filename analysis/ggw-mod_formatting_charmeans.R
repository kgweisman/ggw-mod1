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
    # subject-level data: identity
    subid = paste0("S01",i),
    condition = jd$answers$data$newData$condition,
    
    # subject-level data: demographics
    country = ifelse(
      is.null(jd$answers$data$newData$country) == TRUE, "NA",
      jd$answers$data$newData$country),    
    age = ifelse(
      is.null(jd$answers$data$newData$age) == TRUE, "NA",
      jd$answers$data$newData$age),
    gender = ifelse(
      is.null(jd$answers$data$newData$gender) == TRUE, "NA",
      jd$answers$data$newData$gender),
    englishNative = ifelse(
      is.null(jd$answers$data$newData$englishNative) == TRUE, "NA",
      jd$answers$data$newData$englishNative),
    ethnicity = ifelse(
      is.list(jd$answers$data$newData$ethnicity) == TRUE, "NA",
      jd$answers$data$newData$ethnicity),
    education = ifelse(
      is.null(jd$answers$data$newData$education) == TRUE, "NA",
      jd$answers$data$newData$education),
    religionChild = ifelse(
      is.list(jd$answers$data$newData$religionChild) == TRUE, "NA",
      jd$answers$data$newData$religionChild),
    religionNow = ifelse(
      is.list(jd$answers$data$newData$religionNow) == TRUE, "NA",
      jd$answers$data$newData$religionNow),
    politicalIdeology = ifelse(
      is.null(jd$answers$data$newData$politicalIdeology) == TRUE, "NA",
      jd$answers$data$newData$politicalIdeology),
    maritalStatus = ifelse(
      is.null(jd$answers$data$newData$maritalStatus) == TRUE, "NA",
      jd$answers$data$newData$maritalStatus),
    children = ifelse(
      is.null(jd$answers$data$newData$children) == TRUE, "NA",
      jd$answers$data$newData$children),
    job = ifelse(
      is.null(jd$answers$data$newData$job) == TRUE, "NA",
      jd$answers$data$newData$job),
    
    # subject-level data: experiences
    studyMoralPhil = ifelse(
      is.null(jd$answers$data$newData$studyMoralPhil) == TRUE, "NA",
      jd$answers$data$newData$studyMoralPhil),
    dog = ifelse(
      is.null(jd$answers$data$newData$dog) == TRUE, "NA",
      jd$answers$data$newData$dog),
    vegetarian = ifelse(
      is.null(jd$answers$data$newData$vegetarian) == TRUE, "NA",
      jd$answers$data$newData$vegetarian),
    
    # subject-level data: beliefs
    beliefRules = ifelse(
      is.null(jd$answers$data$newData$beliefRules) == TRUE, "NA",
      jd$answers$data$newData$beliefRules),
    beliefGod = ifelse(
      is.null(jd$answers$data$newData$beliefGod) == TRUE, "NA",
      jd$answers$data$newData$beliefGod),
    beliefAfterlife = ifelse(
      is.null(jd$answers$data$newData$beliefAfterlife) == TRUE, "NA",
      jd$answers$data$newData$beliefAfterlife),
    beliefTradition = ifelse(
      is.null(jd$answers$data$newData$beliefTradition) == TRUE, "NA",
      jd$answers$data$newData$beliefTradition),
    
    # subject-level data: open-ended responses
    comments = jd$answers$data$newData$comments,
    
    # character means data:
    gerald_schiff_pvs = jd$answers$data$newData$charScores$gerald_schiff_pvs,
    toby_chimp = jd$answers$data$newData$charScores$toby_chimp,
    fetus = jd$answers$data$newData$charScores$fetus,
    god = jd$answers$data$newData$charScores$god,
    delores_gleitman_deceased = jd$answers$data$newData$charScores$delores_gleitman_deceased,
    sharon_harvey_woman = jd$answers$data$newData$charScores$sharon_harvey_woman,
    green_frog = jd$answers$data$newData$charScores$green_frog,
    todd_billingsley_man = jd$answers$data$newData$charScores$todd_billingsley_man,
    charlie_dog = jd$answers$data$newData$charScores$charlie_dog,
    nicholas_gannon_baby = jd$answers$data$newData$charScores$nicholas_gannon_baby,
    samantha_hill_girl = jd$answers$data$newData$charScores$samantha_hill_girl,
    kismet_robot = jd$answers$data$newData$charScores$kismet_robot,
    you = jd$answers$data$newData$charScores$you)

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
    # subject-level data: identity
    subid = paste0("S02",i),
    condition = jd$answers$data$newData$condition,
    
    # subject-level data: demographics
    country = ifelse(
      is.null(jd$answers$data$newData$country) == TRUE, "NA",
      jd$answers$data$newData$country),    
    age = ifelse(
      is.null(jd$answers$data$newData$age) == TRUE, "NA",
      jd$answers$data$newData$age),
    gender = ifelse(
      is.null(jd$answers$data$newData$gender) == TRUE, "NA",
      jd$answers$data$newData$gender),
    englishNative = ifelse(
      is.null(jd$answers$data$newData$englishNative) == TRUE, "NA",
      jd$answers$data$newData$englishNative),
    ethnicity = ifelse(
      is.list(jd$answers$data$newData$ethnicity) == TRUE, "NA",
      jd$answers$data$newData$ethnicity),
    education = ifelse(
      is.null(jd$answers$data$newData$education) == TRUE, "NA",
      jd$answers$data$newData$education),
    religionChild = ifelse(
      is.list(jd$answers$data$newData$religionChild) == TRUE, "NA",
      jd$answers$data$newData$religionChild),
    religionNow = ifelse(
      is.list(jd$answers$data$newData$religionNow) == TRUE, "NA",
      jd$answers$data$newData$religionNow),
    politicalIdeology = ifelse(
      is.null(jd$answers$data$newData$politicalIdeology) == TRUE, "NA",
      jd$answers$data$newData$politicalIdeology),
    maritalStatus = ifelse(
      is.null(jd$answers$data$newData$maritalStatus) == TRUE, "NA",
      jd$answers$data$newData$maritalStatus),
    children = ifelse(
      is.null(jd$answers$data$newData$children) == TRUE, "NA",
      jd$answers$data$newData$children),
    job = ifelse(
      is.null(jd$answers$data$newData$job) == TRUE, "NA",
      jd$answers$data$newData$job),
    
    # subject-level data: experiences
    studyMoralPhil = ifelse(
      is.null(jd$answers$data$newData$studyMoralPhil) == TRUE, "NA",
      jd$answers$data$newData$studyMoralPhil),
    dog = ifelse(
      is.null(jd$answers$data$newData$dog) == TRUE, "NA",
      jd$answers$data$newData$dog),
    vegetarian = ifelse(
      is.null(jd$answers$data$newData$vegetarian) == TRUE, "NA",
      jd$answers$data$newData$vegetarian),
    
    # subject-level data: beliefs
    beliefRules = ifelse(
      is.null(jd$answers$data$newData$beliefRules) == TRUE, "NA",
      jd$answers$data$newData$beliefRules),
    beliefGod = ifelse(
      is.null(jd$answers$data$newData$beliefGod) == TRUE, "NA",
      jd$answers$data$newData$beliefGod),
    beliefAfterlife = ifelse(
      is.null(jd$answers$data$newData$beliefAfterlife) == TRUE, "NA",
      jd$answers$data$newData$beliefAfterlife),
    beliefTradition = ifelse(
      is.null(jd$answers$data$newData$beliefTradition) == TRUE, "NA",
      jd$answers$data$newData$beliefTradition),
    
    # subject-level data: open-ended responses
    comments = jd$answers$data$newData$comments,
    
    # character means data:
    gerald_schiff_pvs = jd$answers$data$newData$charScores$gerald_schiff_pvs,
    toby_chimp = jd$answers$data$newData$charScores$toby_chimp,
    fetus = jd$answers$data$newData$charScores$fetus,
    god = jd$answers$data$newData$charScores$god,
    delores_gleitman_deceased = jd$answers$data$newData$charScores$delores_gleitman_deceased,
    sharon_harvey_woman = jd$answers$data$newData$charScores$sharon_harvey_woman,
    green_frog = jd$answers$data$newData$charScores$green_frog,
    todd_billingsley_man = jd$answers$data$newData$charScores$todd_billingsley_man,
    charlie_dog = jd$answers$data$newData$charScores$charlie_dog,
    nicholas_gannon_baby = jd$answers$data$newData$charScores$nicholas_gannon_baby,
    samantha_hill_girl = jd$answers$data$newData$charScores$samantha_hill_girl,
    kismet_robot = jd$answers$data$newData$charScores$kismet_robot,
    you = jd$answers$data$newData$charScores$you)
  
  # bind into same dataframe
  d.raw_02 <- bind_rows(d.raw_02, id)
}

glimpse(d.raw_02)

# ----------> US run-03 (2015-03-16) ------------------------------------------

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
    # subject-level data: identity
    subid = paste0("S03",i),
    condition = jd$answers$data$newData$condition,
    
    # subject-level data: demographics
    country = ifelse(
      is.null(jd$answers$data$newData$country) == TRUE, "NA",
      jd$answers$data$newData$country),    
    age = ifelse(
      is.null(jd$answers$data$newData$age) == TRUE, "NA",
      jd$answers$data$newData$age),
    gender = ifelse(
      is.null(jd$answers$data$newData$gender) == TRUE, "NA",
      jd$answers$data$newData$gender),
    englishNative = ifelse(
      is.null(jd$answers$data$newData$englishNative) == TRUE, "NA",
      jd$answers$data$newData$englishNative),
    ethnicity = ifelse(
      is.list(jd$answers$data$newData$ethnicity) == TRUE, "NA",
      jd$answers$data$newData$ethnicity),
    education = ifelse(
      is.null(jd$answers$data$newData$education) == TRUE, "NA",
      jd$answers$data$newData$education),
    religionChild = ifelse(
      is.list(jd$answers$data$newData$religionChild) == TRUE, "NA",
      jd$answers$data$newData$religionChild),
    religionNow = ifelse(
      is.list(jd$answers$data$newData$religionNow) == TRUE, "NA",
      jd$answers$data$newData$religionNow),
    politicalIdeology = ifelse(
      is.null(jd$answers$data$newData$politicalIdeology) == TRUE, "NA",
      jd$answers$data$newData$politicalIdeology),
    maritalStatus = ifelse(
      is.null(jd$answers$data$newData$maritalStatus) == TRUE, "NA",
      jd$answers$data$newData$maritalStatus),
    children = ifelse(
      is.null(jd$answers$data$newData$children) == TRUE, "NA",
      jd$answers$data$newData$children),
    job = ifelse(
      is.null(jd$answers$data$newData$job) == TRUE, "NA",
      jd$answers$data$newData$job),
    
    # subject-level data: experiences
    studyMoralPhil = ifelse(
      is.null(jd$answers$data$newData$studyMoralPhil) == TRUE, "NA",
      jd$answers$data$newData$studyMoralPhil),
    dog = ifelse(
      is.null(jd$answers$data$newData$dog) == TRUE, "NA",
      jd$answers$data$newData$dog),
    vegetarian = ifelse(
      is.null(jd$answers$data$newData$vegetarian) == TRUE, "NA",
      jd$answers$data$newData$vegetarian),
    
    # subject-level data: beliefs
    beliefRules = ifelse(
      is.null(jd$answers$data$newData$beliefRules) == TRUE, "NA",
      jd$answers$data$newData$beliefRules),
    beliefGod = ifelse(
      is.null(jd$answers$data$newData$beliefGod) == TRUE, "NA",
      jd$answers$data$newData$beliefGod),
    beliefAfterlife = ifelse(
      is.null(jd$answers$data$newData$beliefAfterlife) == TRUE, "NA",
      jd$answers$data$newData$beliefAfterlife),
    beliefTradition = ifelse(
      is.null(jd$answers$data$newData$beliefTradition) == TRUE, "NA",
      jd$answers$data$newData$beliefTradition),
    
    # subject-level data: open-ended responses
    comments = jd$answers$data$newData$comments,
    
    # character means data:
    gerald_schiff_pvs = jd$answers$data$newData$charScores$gerald_schiff_pvs,
    toby_chimp = jd$answers$data$newData$charScores$toby_chimp,
    fetus = jd$answers$data$newData$charScores$fetus,
    god = jd$answers$data$newData$charScores$god,
    delores_gleitman_deceased = jd$answers$data$newData$charScores$delores_gleitman_deceased,
    sharon_harvey_woman = jd$answers$data$newData$charScores$sharon_harvey_woman,
    green_frog = jd$answers$data$newData$charScores$green_frog,
    todd_billingsley_man = jd$answers$data$newData$charScores$todd_billingsley_man,
    charlie_dog = jd$answers$data$newData$charScores$charlie_dog,
    nicholas_gannon_baby = jd$answers$data$newData$charScores$nicholas_gannon_baby,
    samantha_hill_girl = jd$answers$data$newData$charScores$samantha_hill_girl,
    kismet_robot = jd$answers$data$newData$charScores$kismet_robot,
    you = jd$answers$data$newData$charScores$you)
  
  # bind into same dataframe
  d.raw_03 <- bind_rows(d.raw_03, id)
}

glimpse(d.raw_03)

# ----------> US run-04 (2015-03-16) ------------------------------------------

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
    # subject-level data: identity
    subid = paste0("S04",i),
    condition = jd$answers$data$newData$condition,
    
    # subject-level data: demographics
    country = ifelse(
      is.null(jd$answers$data$newData$country) == TRUE, "NA",
      jd$answers$data$newData$country),    
    age = ifelse(
      is.null(jd$answers$data$newData$age) == TRUE, "NA",
      jd$answers$data$newData$age),
    gender = ifelse(
      is.null(jd$answers$data$newData$gender) == TRUE, "NA",
      jd$answers$data$newData$gender),
    englishNative = ifelse(
      is.null(jd$answers$data$newData$englishNative) == TRUE, "NA",
      jd$answers$data$newData$englishNative),
    ethnicity = ifelse(
      is.list(jd$answers$data$newData$ethnicity) == TRUE, "NA",
      jd$answers$data$newData$ethnicity),
    education = ifelse(
      is.null(jd$answers$data$newData$education) == TRUE, "NA",
      jd$answers$data$newData$education),
    religionChild = ifelse(
      is.list(jd$answers$data$newData$religionChild) == TRUE, "NA",
      jd$answers$data$newData$religionChild),
    religionNow = ifelse(
      is.list(jd$answers$data$newData$religionNow) == TRUE, "NA",
      jd$answers$data$newData$religionNow),
    politicalIdeology = ifelse(
      is.null(jd$answers$data$newData$politicalIdeology) == TRUE, "NA",
      jd$answers$data$newData$politicalIdeology),
    maritalStatus = ifelse(
      is.null(jd$answers$data$newData$maritalStatus) == TRUE, "NA",
      jd$answers$data$newData$maritalStatus),
    children = ifelse(
      is.null(jd$answers$data$newData$children) == TRUE, "NA",
      jd$answers$data$newData$children),
    job = ifelse(
      is.null(jd$answers$data$newData$job) == TRUE, "NA",
      jd$answers$data$newData$job),
    
    # subject-level data: experiences
    studyMoralPhil = ifelse(
      is.null(jd$answers$data$newData$studyMoralPhil) == TRUE, "NA",
      jd$answers$data$newData$studyMoralPhil),
    dog = ifelse(
      is.null(jd$answers$data$newData$dog) == TRUE, "NA",
      jd$answers$data$newData$dog),
    vegetarian = ifelse(
      is.null(jd$answers$data$newData$vegetarian) == TRUE, "NA",
      jd$answers$data$newData$vegetarian),
    
    # subject-level data: beliefs
    beliefRules = ifelse(
      is.null(jd$answers$data$newData$beliefRules) == TRUE, "NA",
      jd$answers$data$newData$beliefRules),
    beliefGod = ifelse(
      is.null(jd$answers$data$newData$beliefGod) == TRUE, "NA",
      jd$answers$data$newData$beliefGod),
    beliefAfterlife = ifelse(
      is.null(jd$answers$data$newData$beliefAfterlife) == TRUE, "NA",
      jd$answers$data$newData$beliefAfterlife),
    beliefTradition = ifelse(
      is.null(jd$answers$data$newData$beliefTradition) == TRUE, "NA",
      jd$answers$data$newData$beliefTradition),
    
    # subject-level data: open-ended responses
    comments = jd$answers$data$newData$comments,
    
    # character means data:
    gerald_schiff_pvs = jd$answers$data$newData$charScores$gerald_schiff_pvs,
    toby_chimp = jd$answers$data$newData$charScores$toby_chimp,
    fetus = jd$answers$data$newData$charScores$fetus,
    god = jd$answers$data$newData$charScores$god,
    delores_gleitman_deceased = jd$answers$data$newData$charScores$delores_gleitman_deceased,
    sharon_harvey_woman = jd$answers$data$newData$charScores$sharon_harvey_woman,
    green_frog = jd$answers$data$newData$charScores$green_frog,
    todd_billingsley_man = jd$answers$data$newData$charScores$todd_billingsley_man,
    charlie_dog = jd$answers$data$newData$charScores$charlie_dog,
    nicholas_gannon_baby = jd$answers$data$newData$charScores$nicholas_gannon_baby,
    samantha_hill_girl = jd$answers$data$newData$charScores$samantha_hill_girl,
    kismet_robot = jd$answers$data$newData$charScores$kismet_robot,
    you = jd$answers$data$newData$charScores$you)
  
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
    # subject-level data: identity
    subid = paste0("S_india_01",i),
    condition = jd$answers$data$newData$condition,
    
    # subject-level data: demographics
    country = ifelse(
      is.null(jd$answers$data$newData$country) == TRUE, "NA",
      jd$answers$data$newData$country),    
    age = ifelse(
      is.null(jd$answers$data$newData$age) == TRUE, "NA",
      jd$answers$data$newData$age),
    gender = ifelse(
      is.null(jd$answers$data$newData$gender) == TRUE, "NA",
      jd$answers$data$newData$gender),
    englishNative = ifelse(
      is.null(jd$answers$data$newData$englishNative) == TRUE, "NA",
      jd$answers$data$newData$englishNative),
    ethnicity = ifelse(
      is.list(jd$answers$data$newData$ethnicity) == TRUE, "NA",
      jd$answers$data$newData$ethnicity),
    education = ifelse(
      is.null(jd$answers$data$newData$education) == TRUE, "NA",
      jd$answers$data$newData$education),
    religionChild = ifelse(
      is.list(jd$answers$data$newData$religionChild) == TRUE, "NA",
      jd$answers$data$newData$religionChild),
    religionNow = ifelse(
      is.list(jd$answers$data$newData$religionNow) == TRUE, "NA",
      jd$answers$data$newData$religionNow),
    politicalIdeology = ifelse(
      is.null(jd$answers$data$newData$politicalIdeology) == TRUE, "NA",
      jd$answers$data$newData$politicalIdeology),
    maritalStatus = ifelse(
      is.null(jd$answers$data$newData$maritalStatus) == TRUE, "NA",
      jd$answers$data$newData$maritalStatus),
    children = ifelse(
      is.null(jd$answers$data$newData$children) == TRUE, "NA",
      jd$answers$data$newData$children),
    job = ifelse(
      is.null(jd$answers$data$newData$job) == TRUE, "NA",
      jd$answers$data$newData$job),
    
    # subject-level data: experiences
    studyMoralPhil = ifelse(
      is.null(jd$answers$data$newData$studyMoralPhil) == TRUE, "NA",
      jd$answers$data$newData$studyMoralPhil),
    dog = ifelse(
      is.null(jd$answers$data$newData$dog) == TRUE, "NA",
      jd$answers$data$newData$dog),
    vegetarian = ifelse(
      is.null(jd$answers$data$newData$vegetarian) == TRUE, "NA",
      jd$answers$data$newData$vegetarian),
    
    # subject-level data: beliefs
    beliefRules = ifelse(
      is.null(jd$answers$data$newData$beliefRules) == TRUE, "NA",
      jd$answers$data$newData$beliefRules),
    beliefGod = ifelse(
      is.null(jd$answers$data$newData$beliefGod) == TRUE, "NA",
      jd$answers$data$newData$beliefGod),
    beliefAfterlife = ifelse(
      is.null(jd$answers$data$newData$beliefAfterlife) == TRUE, "NA",
      jd$answers$data$newData$beliefAfterlife),
    beliefTradition = ifelse(
      is.null(jd$answers$data$newData$beliefTradition) == TRUE, "NA",
      jd$answers$data$newData$beliefTradition),
    
    # subject-level data: open-ended responses
    comments = jd$answers$data$newData$comments,
    
    # character means data:
    gerald_schiff_pvs = jd$answers$data$newData$charScores$gerald_schiff_pvs,
    toby_chimp = jd$answers$data$newData$charScores$toby_chimp,
    fetus = jd$answers$data$newData$charScores$fetus,
    god = jd$answers$data$newData$charScores$god,
    delores_gleitman_deceased = jd$answers$data$newData$charScores$delores_gleitman_deceased,
    sharon_harvey_woman = jd$answers$data$newData$charScores$sharon_harvey_woman,
    green_frog = jd$answers$data$newData$charScores$green_frog,
    todd_billingsley_man = jd$answers$data$newData$charScores$todd_billingsley_man,
    charlie_dog = jd$answers$data$newData$charScores$charlie_dog,
    nicholas_gannon_baby = jd$answers$data$newData$charScores$nicholas_gannon_baby,
    samantha_hill_girl = jd$answers$data$newData$charScores$samantha_hill_girl,
    kismet_robot = jd$answers$data$newData$charScores$kismet_robot,
    you = jd$answers$data$newData$charScores$you)
  
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
         country_selfrep = factor(country),
         country = factor(ifelse(grepl("S_india", subid) == T,
                                 "india",
                                 "us")),
         condition = ifelse(condition == "Emotion Recognition",
                            "EmotionRecognition",
                            condition),
         condition = factor(condition),
         age = as.numeric(age),
         gender = factor(gender),
         ethnicity = factor(ethnicity), # redo for multiple selected
         education = factor(education),
         religionChild = factor(religionChild), # redo for multiple selected
         religionNow = factor(religionNow), # redo for multiple selected
         children = factor(children),
         englishNative = factor(englishNative),
         politicalIdeology = factor(politicalIdeology),         
         maritalStatus = factor(maritalStatus),         
         children = as.numeric(children),
         job = factor(job),
         dog = factor(dog),                  
         vegetarian = factor(vegetarian),         
         studyMoralPhil = factor(studyMoralPhil),         
         beliefGod = factor(beliefGod),         
         beliefAfterlife = factor(beliefAfterlife),         
         beliefTradition = factor(beliefTradition),         
         beliefRules = factor(beliefRules)    
         #          beliefLeader = factor(beliefLeader), # lost this somewhere?
         )

glimpse(d_tidy)

# write to de-identified csv file
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1/data/run-01&02&03&04&india01_2015-04-17_charmeans.csv")

d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1/data/run-01&02&03&04&india01_2015-04-17_charmeans.csv")[-1] # get rid of column of obs numbers