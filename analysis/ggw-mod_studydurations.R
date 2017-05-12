# calculate length of study for pilot-b

# library
library("chron")
library("jsonlite")
library("langcog")

# --- READING IN DATA OBJECTS -------------------------------------------------

# ----------> pilot-A (2017-05-11) ---------------------------------------------

# set working directory for pilot A
setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1b/turk/pilot-A/")

# mike's json for-loop
files <- dir("production-results/")

d.raw_A <- data.frame()

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
  d.raw_A <- bind_rows(d.raw_A, id)
}

glimpse(d.raw_A)

# ----------> run-01 (2017-05-12) ---------------------------------------------

# # set working directory for run 01
# setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1b/turk/run-01/")
# 
# # mike's json for-loop
# files <- dir("production-results/")
# 
# d.raw_01 <- data.frame()
# 
# for(i in 1:length(files)) {
#   # gather files
#   f = files[i]
#   jf <- paste("production-results/",f,sep="")
#   
#   # parse JSON object
#   jd <- fromJSON(paste(readLines(jf), collapse=""))
#   
#   # store relevant variables in dataframe 
#   id <- data.frame(
#     matrix(
#       data = c("AcceptTime", "SubmitTime"),
#       nrow = 1, ncol = 0))
#   
#   id$AcceptTime = jd$AcceptTime  
#   id$SubmitTime = jd$SubmitTime
#   
#   # bind into same dataframe
#   d.raw_01 <- bind_rows(d.raw_01, id)
# }
# 
# glimpse(d.raw_01)

# --- TIDYING -----------------------------------------------------------------

d_times = d.raw_A %>%
  # full_join(d.raw_01) %>%
  mutate(startTime = chron(times = substr(AcceptTime, 12, 19), format = 'h:m:s'),
         endTime = chron(times = substr(SubmitTime, 12, 19), format = 'h:m:s'),
         duration = endTime - startTime,
         duration_mins = as.numeric(substr(duration, 4,5)) + as.numeric(substr(duration, 7,8))/60)

summary(d_times)

qplot(d_times$duration_mins,
      binwidth = 1)

# bootstrap 95% confidence intervals
multi_boot(data = d_times, column = "duration_mins", statistics_functions = c("ci_lower", "mean", "ci_upper"))
