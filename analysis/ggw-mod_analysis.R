########################################################### preliminaries #####

# --- PACKAGES & FUNCTIONS ----------------------------------------------------

# libraries
library(tidyverse)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
library(scatterplot3d)
library(lme4)
library(psych)
library(stats)
library(scales)
# library(smacof)

# clear environment
rm(list=ls())

# # clear graphics
# dev.off()

# --- IMPORTING DATA ----------------------------------------------------------

# read in data: character means
d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1b/data/run-01_2017-05-12_charmeans.csv")[-1] %>% # get rid of column of obs numbers
  distinct()

glimpse(d)

# read in data: individual scores
dd = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1b/data/run-01_2017-05-12_data_anonymized.csv")[-1] %>% # get rid of column of obs numbers

glimpse(dd)

# --- FILTERING BY RTS --------------------------------------------------------

# # filter out trials where log_rt < 2SDs below mean
# dd = dd %>%
#   filter(under_lower == 0)
# 
# View(dd %>% group_by(subid) %>% summarise(trials_completed = length(log_rt)))

# # filter out participants where >50% trials have log_rt < 2SDs below mean
# dd = dd %>%
#   filter(prop_under > .5)
# 
# View(dd %>% group_by(subid) %>% summarise(trials_completed = length(log_rt)))

# --- FILTERING BY COUNTRY ----------------------------------------------------

# d_us = d %>% filter(country == "us")
# dd_us = dd %>% filter(country == "us")
# 
# d_india = d %>% filter(country == "india")
# dd_india = dd %>% filter(country == "india")
# 
# # set group of interest
# # ... to US:
# d = d_us
# dd = dd_us
# 
# # # ... to India:
# # d = d_india
# # dd = dd_india

# --- FILTERING BY COMPREHENSION CHECK ----------------------------------------

# # NOTE: currently only works for india run 03 and on!
# d_comp2 = d %>% filter(compCheckCount < 2)
# dd_comp2 = dd %>% filter(compCheckCount < 2)

# d = d_comp2
# dd = dd_comp2

# d_india = d_india %>% filter(compCheckCount < 2)
# dd_india = dd_india %>% filter(compCheckCount < 2)

# --- FILTERING BY ETHNICITY --------------------------------------------------
# 
# d_white = d %>%
#   filter(ethnicity == "white")
# dd_white = dd %>%
#   filter(ethnicity == "white")
# 
# d_nonwhite = d %>%
#   filter(ethnicity != "white" & 
#            ethnicity != "NA" & 
#            ethnicity != "other_prefNo")
# 
# dd_nonwhite = dd %>%
#   filter(ethnicity != "white" & 
#            ethnicity != "NA" & 
#            ethnicity != "other_prefNo")
# 
# # set group of interest
# # ... to white:
# # d = d_white
# # dd = dd_white
# 
# # # ... to nonwhite:
# # d = d_nonwhite
# # dd = dd_nonwhite

# --- FORMATTING DATA ---------------------------------------------------------

# make table of character means by mental capacity
charmeans = d %>%
  select(condition, subid, gerald_schiff_pvs, toby_chimp, fetus, god,
         delores_gleitman_deceased, sharon_harvey_woman, green_frog,
         todd_billingsley_man, charlie_dog, nicholas_gannon_baby,
         samantha_hill_girl, kismet_robot, you) %>%  
  gather(character, response,
         -condition, -subid) %>%
  group_by(condition, character) %>%
  summarise(mean = mean(response, na.rm = T))

glimpse(charmeans)

# format into wideform with characters as rows
charmeans_table = charmeans %>%
  spread(condition, mean)

charnames = as.character(charmeans_table$character)
charnames = ifelse(charnames == "charlie_dog", "dog",
                   ifelse(charnames == "delores_gleitman_deceased", "dead woman",
                          ifelse(charnames == "gerald_schiff_pvs", "PVS man", 
                                 ifelse(charnames == "green_frog", "frog",
                                        ifelse(charnames == "samantha_hill_girl", "girl",
                                               ifelse(charnames == "kismet_robot", "robot",
                                                      ifelse(charnames == "nicholas_gannon_baby", "baby",
                                                             ifelse(charnames == "sharon_harvey_woman", "woman",
                                                                    ifelse(charnames == "toby_chimp", "chimp",
                                                                           ifelse(charnames == "todd_billingsley_man", "man",
                                                                                  as.character(charnames)))))))))))

d1 = charmeans_table %>%
  data.frame() %>%
  rownames_to_column("drop") %>%
  select(-drop) %>%
  column_to_rownames("character")
print(d1)

# make table of mental capacity means by character
# formatted in wideform with characters as rows
condmeans = d %>%
  select(condition, subid, gerald_schiff_pvs, toby_chimp, fetus, god,
         delores_gleitman_deceased, sharon_harvey_woman, green_frog,
         todd_billingsley_man, charlie_dog, nicholas_gannon_baby,
         samantha_hill_girl, kismet_robot, you) %>%  
  gather(character, response,
         -condition, -subid) %>%
  group_by(condition, character) %>%
  summarise(mean = mean(response, na.rm = T))

# format into wideform with characters as rows
condmeans_table = condmeans %>%
  spread(condition, mean)

# subidnames = condmeans_table$subid
# 
# d3 = condmeans_table[-1]
# d3 = d3[-1]
# names(d3) = charnames
# rownames(d3) = subidnames
# print(d3)

d3 = condmeans_table %>%
  data.frame() %>%
  rownames_to_column("drop") %>%
  select(-drop) %>%
  column_to_rownames("character")

d3

########################################################### summary stats #####

# --- DEMOGRAPHICS ------------------------------------------------------------

demo = dd %>% distinct(subid, .keep_all = T)

# total n
demo %>% summarise(n = length(subid))

# condition assignment
dd %>% group_by(condition) %>% distinct(subid) %>% summarise(n = length(subid))

# # gender
# demo %>% count(gender)
# 
# # ethnicity
# demo %>% count(ethnicity)
# 
# # age
# demo %>% summarise(mean_age = mean(age, na.rm = T), sd_age = sd(age, na.rm = T))
# qplot(demo$age)
# 
# # demo %>% filter(age < 100) %>% summarise(mean_age = mean(age, na.rm = T), sd_age = sd(age, na.rm = T))
# # qplot(demo$age[demo$age < 100])
# 
# # education
# levels(demo$education) = c("hs_some", "hs_diploma", "college_some", 
#                      "college_assocDegree", "college_bachDegree",
#                      "grad_some", "grad_degree", "other_prefNo")
# demo %>% count(education)
# 
# # englishNative
# demo %>% count(englishNative)
# 
# # politicalIdeology
# demo %>% count(politicalIdeology)
# 
# # religionChild
# demo %>% count(religionChild)
# 
# demo = demo %>%
#   filter(religionChild != "prefNo" & religionChild != "NA") %>%
#   mutate(religCat = ifelse(grepl("christ", religionChild) == T |
#                              religionChild == "judaism",
#                            "judeo-christian",
#                            ifelse(religionChild == "none",
#                                   "non-religious",
#                                   "other religious")))
# demo %>% count(religCat)
# 
# # religionNow
# demo %>% count(religionNow)
# 
# # maritalStatus
# demo %>% count(maritalStatus)
# 
# # children
# demo %>% summarise(mean_children = mean(children, na.rm = T),
#                    sd_children = sd(children, na.rm = T))
# qplot(demo$children, binwidth = 1)
# 
# # job
# demo %>% 
#   mutate(job = factor(tolower(as.character(job)))) %>%
#   count(job)
# 
# # studyMoralPhil
# demo %>% count(studyMoralPhil)
# 
# # dog
# demo %>% count(dog)
# 
# # vegetarian
# demo %>% count(vegetarian)
# 
# # beliefRules
# levels(demo$beliefRules) = c("disagree_strong", "disagree_moderate", 
#                              "disagree_little", "neither", "agree_little",
#                              "agree_moderate", "agree_strong")
# demo %>% count(beliefRules)
# 
# # beliefGod
# levels(demo$beliefGod) = c("disagree_strong", "disagree_moderate", 
#                              "disagree_little", "neither", "agree_little",
#                              "agree_moderate", "agree_strong")
# demo %>% count(beliefGod)
# 
# # beliefAfterlife
# levels(demo$beliefAfterlife) = c("disagree_strong", "disagree_moderate", 
#                              "disagree_little", "neither", "agree_little",
#                              "agree_moderate", "agree_strong")
# demo %>% count(beliefAfterlife)
# 
# # beliefTradition
# levels(demo$beliefTradition) = c("disagree_strong", "disagree_moderate", 
#                              "disagree_little", "neither", "agree_little",
#                              "agree_moderate", "agree_strong")
# demo %>% count(beliefTradition)

################################################### analysis & plots pt 1 #####

# --- PRINCIPAL COMPONENTS ANALYSIS A: ORIGINAL GGW2007 ----------------------

# NOTES: 
# - should look again at >2 factors when we have more data
# - good resource: http://www.colorado.edu/geography/class_homepages/geog_4023_s11/Lecture18_PCA.pdf

# # --------> 4-factor (maximal) PCA (UNrotated, using principal) ----------
# 
# # extract factors
# pca_A4 = principal(d1, nfactors = 4, rotate = "none"); pca_A4
# # retain components with prop var > 5-10%
# # retain components with cumulative prop var > 70% (... but < 100%?)
# 
# # extract eigenvalues
# pca_A4$values 
# # retain components with eigenvalues > 1
# 
# # scree test
# qplot(y = pca_A4$values) +
#   theme_bw() +
#   labs(title = "Scree test for 4-factor (maximal) PCA",
#        x = "Component",
#        y = "Eigenvalue") +
#   geom_line() 
# # retain components left of "break"
# 
# # extract PCA loadings
# pca_A4_pc1 = pca_A4$loadings[,1]; sort(pca_A4_pc1)
# pca_A4_pc2 = pca_A4$loadings[,2]; sort(pca_A4_pc2)
# pca_A4_pc3 = pca_A4$loadings[,3]; sort(pca_A4_pc3)
# pca_A4_pc4 = pca_A4$loadings[,4]; sort(pca_A4_pc4)
# 
# # --------> 2-factor PCA (varimax rotation, using principal) ----------
# 
# # FROM GGW2007: "For each survey, each character appeared in 12 different comparisons, and mean relative ratings were computed for each character across all respondents to that survey. We merged data sets from the 18 mental capacity surveys to compute correlations between mental capacities across the characters, and submitted these to principal components factor analysis with varimax rotation." (SOM p. 3)
# 
# # extract factors
# # pca_A2 = principal(d1, nfactors = 2, rotate = "varimax"); pca_A2
# pca_A2 = principal(d1, nfactors = 2, rotate = "none"); pca_A2
# 
# # extract eigenvalues
# pca_A2$values
# 
# # extract PCA loadings
# pca_A2_pc1 = pca_A2$loadings[,1]; sort(pca_A2_pc1)
# pca_A2_pc2 = pca_A2$loadings[,2]; sort(pca_A2_pc2)
# 
# # --------------->-> plots ----------------------------------------------------
# 
# # plot PCs against each other
# # NOTE: need to adjust "1:18" depending on how many conditions are run
# ggplot(loadings(pca_A2)[] %>% data.frame(), 
#        aes(x = PC1, y = PC2, label = names(d1))) +
#   geom_text() +
#   theme_bw() +
#   labs(title = "Factor loadings\n",
#        x = "\nRotated Component 2",
#        y = "Rotated Component 1\n")
# 
# # FROM GGW2007: "We used the regression approach to estimate factor scores for each character." (SOM p. 3) 
# # ?principal confirms that "component scores are found by regression"
# 
# # plot characters by principal components
# ggplot(data.frame(pca_A2$scores), aes(x = PC1, y = PC2, label = rownames(d1))) +
#   geom_text() +
#   theme_bw() +
#   labs(title = "Raw character factor scores\n",
#        x = "\nRotated Component 1: 'Agency'",
#        y = "Rotated Component 2: 'Experience'\n")
# 
# # FROM GGW2007: "For ease of interpretation, factor scores in Figure 1 were adjusted to be anchored at 0 and 1" (SOM p. 3)
# 
# # re-plot characters with rescaling (as in GGW2007 original), PC1 on y-axis
# ggplot(data.frame(pca_A2$scores), 
#        aes(x = rescale(PC1, to = c(0,1)), 
#            y = rescale(PC2, to = c(0,1)), 
#            label = rownames(d1))) +
#   geom_point() +
#   geom_text(angle = 0,
#             vjust = -1,
#             size = 7) +
#   xlim(-0.01, 1.01) +
#   ylim(-0.01, 1.01) +
#   theme_bw() +
#   theme(text = element_text(size = 20)) +
#   labs(title = "Adjusted character factor scores\n",
#        x = "\nRotated Component 1, rescaled",
#        y = "Rotated Component 2, rescaled\n")
# 
# # # re-plot characters with rescaling (as in GGW2007 original), PC1 on x-axis
# # ggplot(data.frame(pca_A2$scores), 
# #        aes(x = rescale(RC2, to = c(0,1)), 
# #            y = rescale(RC1, to = c(0,1)), 
# #            label = rownames(d1))) +
# #   geom_point() +
# #   geom_text(angle = 0,
# #             vjust = -1,
# #             size = 6) +
# #   xlim(-0.01, 1.01) +
# #   ylim(-0.01, 1.01) +
# #   theme_bw() +
# #   theme(text = element_text(size = 20)) +
# #   labs(title = "Adjusted character factor scores\n",
# #        x = "\nRotated Component 2, rescaled",
# #        y = "Rotated Component 1, rescaled\n")
# 
# # --------> 1-factor PCA (varimax rotation, using principal) ----------
# # extract factors
# pca_A1 = principal(d1, nfactors = 1, rotate = "varimax"); pca_A1
# 
# # extract PCA loadings
# pca_A1_pc1 = pca_A1$loadings[,1]; sort(pca_A1_pc1)
# 

# --------> 4-factor (maximal) EFA (UNrotated, using efa) ----------

# extract factors
efa_A4 = fa(d1, nfactors = 4, rotate = "none"); efa_A4
# retain components with prop var > 5-10%
# retain components with cumulative prop var > 70% (... but < 100%?)

# extract eigenvalues
efa_A4$values 
# retain components with eigenvalues > 1

# scree test
qplot(y = efa_A4$values) +
  theme_bw() +
  labs(title = "Scree test for 4-factor (maximal) EFA",
       x = "Component",
       y = "Eigenvalue") +
  geom_line() 
# retain components left of "break"

# extract EFA loadings
efa_A4_pc1 = efa_A4$loadings[,1]; sort(efa_A4_pc1)
efa_A4_pc2 = efa_A4$loadings[,2]; sort(efa_A4_pc2)
efa_A4_pc3 = efa_A4$loadings[,3]; sort(efa_A4_pc3)
efa_A4_pc4 = efa_A4$loadings[,4]; sort(efa_A4_pc4)

# --------> 2-factor EFA (varimax rotation, using efa) ----------

# FROM GGW2007: "For each survey, each character appeared in 12 different comparisons, and mean relative ratings were computed for each character across all respondents to that survey. We merged data sets from the 18 mental capacity surveys to compute correlations between mental capacities across the characters, and submitted these to efa components factor analysis with varimax rotation." (SOM p. 3)

# extract factors
efa_A2 = fa(d1, nfactors = 2, rotate = "varimax"); fa.sort(efa_A2)
# efa_A2 = fa(d1, nfactors = 2, rotate = "none"); efa_A2

# extract eigenvalues
efa_A2$values

# extract EFA loadings
efa_A2_pc1 = efa_A2$loadings[,1]; sort(efa_A2_pc1)
efa_A2_pc2 = efa_A2$loadings[,2]; sort(efa_A2_pc2)

# --------------->-> plots ----------------------------------------------------

# plot MRs against each other
# NOTE: need to adjust "1:18" depending on how many conditions are run
ggplot(loadings(efa_A2)[] %>% data.frame(), 
       aes(x = MR1, y = MR2, label = names(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Factor loadings\n",
       x = "\nRotated Component 2",
       y = "Rotated Component 1\n")

# FROM GGW2007: "We used the regression approach to estimate factor scores for each character." (SOM p. 3) 
# ?efa confirms that "component scores are found by regression"

# plot characters by efa components
ggplot(data.frame(efa_A2$scores), aes(x = MR1, y = MR2, label = rownames(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Raw character factor scores\n",
       x = "\nRotated Component 1: 'Agency'",
       y = "Rotated Component 2: 'Experience'\n")

# FROM GGW2007: "For ease of interpretation, factor scores in Figure 1 were adjusted to be anchored at 0 and 1" (SOM p. 3)

# re-plot characters with rescaling (as in GGW2007 original), MR1 on y-axis
ggplot(data.frame(efa_A2$scores), 
       aes(x = rescale(MR1, to = c(0,1)), 
           y = rescale(MR2, to = c(0,1)), 
           label = rownames(d1))) +
  geom_point() +
  geom_text(angle = 0,
            vjust = -1,
            size = 5) +
  xlim(-0.01, 1.01) +
  ylim(-0.01, 1.01) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Adjusted character factor scores\n",
       x = "\nRotated Component 1, rescaled",
       y = "Rotated Component 2, rescaled\n")

# # re-plot characters with rescaling (as in GGW2007 original), MR1 on x-axis
# ggplot(data.frame(efa_A2$scores), 
#        aes(x = rescale(RC2, to = c(0,1)), 
#            y = rescale(RC1, to = c(0,1)), 
#            label = rownames(d1))) +
#   geom_point() +
#   geom_text(angle = 0,
#             vjust = -1,
#             size = 6) +
#   xlim(-0.01, 1.01) +
#   ylim(-0.01, 1.01) +
#   theme_bw() +
#   theme(text = element_text(size = 20)) +
#   labs(title = "Adjusted character factor scores\n",
#        x = "\nRotated Component 2, rescaled",
#        y = "Rotated Component 1, rescaled\n")

efa_A2_loadings <- loadings(efa_A2)[] %>% 
  data.frame() %>% 
  fa.sort() %>%
  select(-order) %>%
  rownames_to_column("condition") %>%
  mutate(factor = ifelse(abs(MR1) > abs(MR2), "MR1",
                         ifelse(abs(MR2) > abs(MR1), "MR2",
                                "equal")))

cat(efa_A2_loadings$condition[efa_A2_loadings$factor == "MR1"], sep = ", ")
cat(efa_A2_loadings$condition[efa_A2_loadings$factor == "MR2"], sep = ", ")

fa.diagram(efa_A2)

# # --------> 1-factor EFA (varimax rotation, using efa) ----------
# # extract factors
# efa_A1 = fa(d1, nfactors = 1, rotate = "varimax"); efa_A1
# 
# # extract EFA loadings
# efa_A1_pc1 = efa_A1$loadings[,1]; sort(efa_A1_pc1)

