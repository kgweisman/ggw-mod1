# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(jsonlite)
library(stats)
library(stringr)

# clear environment
rm(list=ls())

# set up function for reading in data from JSON objects
jsonFormat = function(wd, runName) {
  # NOTE: requires dplyr and jsonlite packages
  # library(jsonlite)
  # library(dplyr)
  
  # set working directory
  setwd(wd)
  
  # gather files
  files = dir("production-results/")
  
  # make dataframe
  d.raw = data.frame()
  
  for(i in 1:length(files)) {
    
    # gather files
    f = files[i]
    jf <- paste0("production-results/", f)
    
    # parse JSON object
    jd <- fromJSON(paste(readLines(jf), collapse=""))
    
    # store relevant variables in dataframe 
    id <- data.frame(
      # run
      run = runName,
      
      # subject-level data: identity
      subid = paste0(runName, "_", i),
      condition = jd$answers$data$newData$condition,
      compCheckFree = ifelse(
        runName == "us_run_01" |
          runName == "us_run_02" |
          runName == "us_run_03" |
          runName == "us_run_04" |
          runName == "india_run_01" |
          runName == "india_run_02",
        ifelse(
          is.null(jd$answers$data$newData$comprehensionCheck) == TRUE, "NA",
          paste(jd$answers$data$newData$comprehensionCheck, collapse = ', ')),
        "NA"),
      compCheckMulti = ifelse(
        runName == "us_run_01" |
          runName == "us_run_02" |
          runName == "us_run_03" |
          runName == "us_run_04" |
          runName == "india_run_01" |
          runName == "india_run_02",
        "NA",
        ifelse(
          is.null(jd$answers$data$newData$comprehensionCheck) == TRUE, "NA",
          paste(jd$answers$data$newData$comprehensionCheck, collapse = ', '))),
      
      # subject-level data: demographics
      country = ifelse(
        is.null(jd$answers$data$newData$country) == TRUE, NA,
        jd$answers$data$newData$country),    
      age = ifelse(
        is.null(jd$answers$data$newData$age) == TRUE, NA,
        jd$answers$data$newData$age),
      gender = ifelse(
        is.null(jd$answers$data$newData$gender) == TRUE, NA,
        jd$answers$data$newData$gender),
      englishNative = ifelse(
        is.null(jd$answers$data$newData$englishNative) == TRUE, NA,
        jd$answers$data$newData$englishNative),
      ethnicity = ifelse(
        is.null(jd$answers$data$newData$ethnicity) == TRUE, NA,
        paste(jd$answers$data$newData$ethnicity, collapse = ', ')),
      education = ifelse(
        is.null(jd$answers$data$newData$education) == TRUE, NA,
        jd$answers$data$newData$education),
      religionChild = ifelse(
        is.null(jd$answers$data$newData$religionChild) == TRUE, NA,
        paste(jd$answers$data$newData$religionChild, collapse = ', ')),
      religionNow = ifelse(
        is.null(jd$answers$data$newData$religionNow) == TRUE, NA,
        paste(jd$answers$data$newData$religionNow, collapse = ', ')),
      politicalIdeology = ifelse(
        is.null(jd$answers$data$newData$politicalIdeology) == TRUE, NA,
        jd$answers$data$newData$politicalIdeology),
      maritalStatus = ifelse(
        is.null(jd$answers$data$newData$maritalStatus) == TRUE, NA,
        jd$answers$data$newData$maritalStatus),
      children = ifelse(
        is.null(jd$answers$data$newData$children) == TRUE, NA,
        jd$answers$data$newData$children),
      job = ifelse(
        is.null(jd$answers$data$newData$job) == TRUE | 
          jd$answers$data$newData$job == "", NA,
        jd$answers$data$newData$job),
      
      # subject-level data: experiences
      studyMoralPhil = ifelse(
        is.null(jd$answers$data$newData$studyMoralPhil) == TRUE, NA,
        jd$answers$data$newData$studyMoralPhil),
      dog = ifelse(
        is.null(jd$answers$data$newData$dog) == TRUE, NA,
        jd$answers$data$newData$dog),
      vegetarian = ifelse(
        is.null(jd$answers$data$newData$vegetarian) == TRUE, NA,
        jd$answers$data$newData$vegetarian),
      
      # subject-level data: beliefs
      beliefRules = ifelse(
        is.null(jd$answers$data$newData$beliefRules) == TRUE, NA,
        jd$answers$data$newData$beliefRules),
      beliefGod = ifelse(
        is.null(jd$answers$data$newData$beliefGod) == TRUE, NA,
        jd$answers$data$newData$beliefGod),
      beliefAfterlife = ifelse(
        is.null(jd$answers$data$newData$beliefAfterlife) == TRUE, NA,
        jd$answers$data$newData$beliefAfterlife),
      beliefTradition = ifelse(
        is.null(jd$answers$data$newData$beliefTradition) == TRUE, NA,
        jd$answers$data$newData$beliefTradition),
      
      # subject-level data: open-ended responses
      comments = ifelse(
        is.null(jd$answers$data$newData$comments) | 
          jd$answers$data$newData$comments == "", NA,
        jd$answers$data$newData$comments),
      
      # trial-level data:                    
      trialNum = jd$answers$data$newData$trialData$trialNum,
      leftCharacter = jd$answers$data$newData$trialData$leftCharacter,
      rightCharacter = jd$answers$data$newData$trialData$rightCharacter,
      response = jd$answers$data$newData$trialData$response,
      rt = jd$answers$data$newData$trialData$rt)
    
    # bind into same dataframe
    d.raw = bind_rows(d.raw, id)
  }
  
  return(d.raw)
}

# --- READING IN DATA OBJECTS -------------------------------------------------

# pilot A (2017-05-11)
d_us_pilot_A = jsonFormat(
  wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1b/turk/pilot-A/",
  runName = "d_us_pilot_A")

# # US run 01 (2017-05-12)
# d_us_run_01 = jsonFormat(
#   wd = "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1b/turk/run-01/",
#   runName = "us_run_01")

# --- TIDYING -----------------------------------------------------------------

# clean up variables
d_tidy = d_us_pilot_A %>%
  # full_join(d_us_run_01) %>%
  mutate(
    run = factor(run),
    subid = factor(subid),
    country_selfrep = factor(country),
    country = factor(ifelse(grepl("india", subid) == T, "india", 
                            ifelse(grepl("us", subid) == T, "us",
                                   NA))),
    condition = gsub(" ", "_", condition),
    condition = factor(condition),
    compCheckFree = factor(ifelse(compCheckFree == "NA", NA, compCheckFree)),
    compCheckMulti = factor(ifelse(compCheckMulti == "NA", NA, compCheckMulti)),
    compCheckCount = as.numeric(ifelse(compCheckMulti == "NA", NA,
                            (str_count(compCheckMulti, ',') + 1))),
    age = as.numeric(age),
    gender = factor(gender),
    ethnicity = factor(ethnicity),
    education = factor(education),
    religionChild = factor(religionChild),
    religionNow = factor(religionNow),
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
    beliefRules = factor(beliefRules),
    # beliefLeader = factor(beliefLeader), # lost this somewhere?
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
write.csv(d_tidy, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1b/data/pilot_A_2017-05-11_data_anonymized.csv")

d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1b/data/pilot_A_2017-05-11_data_anonymized.csv")[-1] # get rid of column of obs numbers

# # view comments
# comments = d %>%
# #   filter(country == "india") %>%
#   select(comments, condition, subid) %>%
#   distinct() %>%
#   filter(comments != "NA")
# 
# View(comments)