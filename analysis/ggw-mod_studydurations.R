# calculate length of study for pilot-b

# library
library("chron")
library("jsonlite")
library("langcog")

# --- READING IN DATA OBJECTS -------------------------------------------------

# ----------> run-01 (2015-03-13) ---------------------------------------------

# set working directory for run 01
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
      data = c("AcceptTime", "SubmitTime"),
      nrow = 1, ncol = 0))
  
  id$AcceptTime = jd$AcceptTime  
  id$SubmitTime = jd$SubmitTime
  
  # bind into same dataframe
  d.raw_01 <- bind_rows(d.raw_01, id)
}

glimpse(d.raw_01)

# ----------> run-02 (2015-03-16) ---------------------------------------------

# set working directory for run 02
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
      data = c("AcceptTime", "SubmitTime"),
      nrow = 1, ncol = 0))
  
  id$AcceptTime = jd$AcceptTime  
  id$SubmitTime = jd$SubmitTime
  
  # bind into same dataframe
  d.raw_02 <- bind_rows(d.raw_02, id)
}

glimpse(d.raw_02)

# ----------> run-03 (2015-03-27) ---------------------------------------------

# set working directory for run 03
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
      data = c("AcceptTime", "SubmitTime"),
      nrow = 1, ncol = 0))
  
  id$AcceptTime = jd$AcceptTime  
  id$SubmitTime = jd$SubmitTime
  
  # bind into same dataframe
  d.raw_03 <- bind_rows(d.raw_03, id)
}

glimpse(d.raw_03)

# ----------> run-04 (2015-03-27) ---------------------------------------------

# set working directory for run 04
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
      data = c("AcceptTime", "SubmitTime"),
      nrow = 1, ncol = 0))
  
  id$AcceptTime = jd$AcceptTime  
  id$SubmitTime = jd$SubmitTime
  
  # bind into same dataframe
  d.raw_04 <- bind_rows(d.raw_04, id)
}

glimpse(d.raw_04)

# --- TIDYING -----------------------------------------------------------------

d_times = full_join(d.raw_01, d.raw_02) %>%
  full_join(d.raw_03) %>%
  full_join(d.raw_04) %>%
  mutate(startTime = chron(times = substr(AcceptTime, 12, 19), format = 'h:m:s'),
         endTime = chron(times = substr(SubmitTime, 12, 19), format = 'h:m:s'),
         duration = endTime - startTime,
         duration_mins = as.numeric(substr(duration, 4,5)) + as.numeric(substr(duration, 7,8))/60)

summary(d_times)

qplot(d_times$duration_mins,
      binwidth = 1)

# bootstrap 95% confidence intervals
multi_boot.data.frame(data = d_times, column = "duration_mins", statistics_functions = c("ci_lower", "mean", "ci_upper"))
