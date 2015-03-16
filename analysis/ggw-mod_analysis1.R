# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(psych)
library(stats)
library(scales)

# clear environment
rm(list=ls())

# clear graphics
dev.off()

# read in data: character means
d = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1/data/run-01&02_2015-03-16_charmeans.csv")[-1] # get rid of column of obs numbers

glimpse(d)

# read in data: individual scores
dd = read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-mod/ggw-mod1/data/run-01&02_2015-03-16_data_anonymized.csv")[-1] # get rid of column of obs numbers

glimpse(dd)

# --- DATA FORMATTING ---------------------------------------------------------

# make table of character means by mental capacity
charmeans = d %>%
  gather(character, response, 
         -subid, -condition, -gender, -age, 
         -beliefGod, -education, -politicalIdeology, 
         -maritalStatus, -children, -beliefAfterlife) %>%
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

d1 = charmeans_table[-1]
rownames(d1) = charnames
print(d1)

d %>% group_by(condition) %>% distinct(subid) %>% summarise(n = length(subid))

# make table of mental capacity means by character
# formatted in wideform with characters as rows
condmeans = charmeans %>%
  spread(character, mean)

rows = condmeans$condition
d3 = condmeans[-1]
rownames(d3) = rows
names(d3) = charnames
print(d3)

# --- PRINCIPAL COMPONENTS ANALYSIS A: ORIGINAL GGW2007 ----------------------

# NOTES: 
# - should look again at >2 factors when we have more data
# - good resource: http://www.colorado.edu/geography/class_homepages/geog_4023_s11/Lecture18_PCA.pdf

# --------> 11-factor (maximal) PCA (UNrotated, using principal) ----------

# extract factors
pca_A11 = principal(d1, nfactors = 11, rotate = "none"); pca_A11
# retain 2 components (prop var > 5-10%)
# retain 5 components? (cumulative prop var > 70%... but < 100%?)

# extract eigenvalues
pca_A11$values # retain 2 components (eigenvalue > 1)

# scree test
qplot(y = pca_A11$values) +
  theme_bw() +
  labs(title = "Scree test for 11-factor (maximal) PCA",
       x = "Component",
       y = "Eigenvalue") +
  geom_line() # retain 2 components (left of "break")

# extract PCA loadings for first 2 factors
pca_A11_pc1 = pca_A11$loadings[,1]; pca_A11_pc1
pca_A11_pc2 = pca_A11$loadings[,2]; pca_A11_pc2

# --------> 2-factor PCA (varimax rotation, using principal) ----------

# FROM GGW2007: "For each survey, each character appeared in 12 different comparisons, and mean relative ratings were computed for each character across all respondents to that survey. We merged data sets from the 18 mental capacity surveys to compute correlations between mental capacities across the characters, and submitted these to principal components factor analysis with varimax rotation." (SOM p. 3)

# extract factors
pca_A2_unrot = principal(d1, nfactors = 2, rotate = "none"); pca_A2_unrot
pca_A2 = principal(d1, nfactors = 2, rotate = "varimax"); pca_A2

# extract eigenvalues
pca_A2$values

# extract PCA loadings
pca_A2_pc1 = pca_A2$loadings[,1]; pca_A2_pc1
pca_A2_pc2 = pca_A2$loadings[,2]; pca_A2_pc2

# plot PCs against each other
# NOTE: need to adjust "1:18" depending on how many conditions are run
ggplot(data.frame(pca_A2$loadings[1:18,]), aes(x = RC1, y = RC2, label = names(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Factor loadings\n",
       x = "\nRotated Component 2",
       y = "Rotated Component 1\n")

# FROM GGW2007: "We used the regression approach to estimate factor scores for each character." (SOM p. 3) 
# ?principal confirms that "component scores are found by regression"

# plot characters by principal components, PC1 on y-axis
ggplot(data.frame(pca_A2$scores), aes(x = RC1, y = RC2, label = rownames(d1))) +
  geom_text() +
  theme_bw() +
  labs(title = "Raw character factor scores\n",
       x = "\nRotated Component 2",
       y = "Rotated Component 1\n")

# FROM GGW2007: "For ease of interpretation, factor scores in Figure 1 were adjusted to be anchored at 0 and 1" (SOM p. 3)

# re-plot characters with rescaling (as in GGW2007 original), PC1 on y-axis
ggplot(data.frame(pca_A2$scores), 
       aes(x = rescale(RC1, to = c(0,1)), 
           y = rescale(RC2, to = c(0,1)), 
           label = rownames(d1))) +
  geom_point() +
  geom_text(angle = 0,
            vjust = -1,
            size = 6) +
  xlim(-0.05, 1.05) +
  ylim(-0.05, 1.05) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Adjusted character factor scores\n",
       x = "\nRotated Component 1 (rescaled)",
       y = "Rotated Component 2 (rescaled)\n")

# --------> 1-factor PCA (varimax rotation, using principal) ----------
# extract factors
pca_A1 = principal(d1, nfactors = 1, rotate = "varimax"); pca_A1

# extract PCA loadings
pca_A1_pc1 = pca_A1$loadings[,1]; pca_A1_pc1

# --- PRINCIPAL COMPONENTS ANALYSIS B -----------------------------------------

# NOTES: 
# - in addition to running for all conditions together (as here), need to filter by condition and run for each condition separately!
# - could also look at unrotated solution by specifying rotate = "none"
# - should also look at other numbers of factors when we have more data

# --------> 11-factor (maximal) PCA (UNrotated, using principal) ----------

# extract factors
pca_B11 = principal(d1, nfactors = 11, rotate = "none"); pca_B11
# retain 2 components (prop var > 5-10%)
# retain 5 components? (cumulative prop var > 70%... but < 100%?)

# extract eigenvalues
pca_B11$values # retain 2 components (eigenvalue > 1)

# scree test
qplot(y = pca_B11$values) +
  theme_bw() +
  labs(title = "Scree test for 11-factor (maximal) PCA",
       x = "Component",
       y = "Eigenvalue") +
  geom_line() # retain 2 components (left of "break")

# extract PCA loadings for first 2 factors
pca_B11_pc1 = pca_B11$loadings[,1]; pca_B11_pc1
pca_B11_pc2 = pca_B11$loadings[,2]; pca_B11_pc2

# --------> 2-factor PCA (varimax rotation, using principal) ----------
# extract factors
pca_B2 = principal(d3, nfactors = 2, rotate = "varimax"); pca_B2

# extract PCA loadings
pca_B2_pc1 = pca_B2$loadings[,1]
pca_B2_pc2 = pca_B2$loadings[,2]

# plot PCs against each other
ggplot(data.frame(pca_B2$loadings[1:13,]), aes(x = RC1, y = RC2, label = names(d3))) +
  geom_text() +
  theme_bw() +
  labs(title = "Factor loadings\n",
       x = "\nRotated PC1",
       y = "Rotated PC2\n")

# plot conditions by principal components
ggplot(data.frame(pca_B2$scores), aes(x = RC1, y = RC2, label = rownames(d3))) +
  geom_text() +
  theme_bw() +
  labs(title = "Raw condition factor scores\n",
       x = "\nRotated PC1",
       y = "Rotated PC2\n")

# re-plot conditions with rescaling (as in GGW2007 original), PC1 on y-axis
ggplot(data.frame(pca_B2$scores), aes(x = rescale(RC1, to = c(0,1)), y = rescale(RC2, to = c(0,1)), label = rownames(d3))) +
  geom_text() +
  theme_bw() +
  labs(title = "Adjusted condition factor scores\n",
       x = "\nRotated PC1 (rescaled)",
       y = "Rotated PC2 (rescaled)\n")

# --- MULTIDIMENSIONAL SCALING ANALYSES ---------------------------------------

# NOTE: in addition to running for all conditions together (as here), need to filter by condition and run for each condition separately!

# --------> MDS 1: all conditions ---------------------------------------------

# ----------------> data formatting -------------------------------------------
# make alphabetized list of characters, cycle through to fill in alphabetized pairs
upperDissim <- dd %>%
  mutate(character1 = array(),
         character2 = array())

charsort = sort(levels(upperDissim$leftCharacter), decreasing = TRUE)

for(i in 1:length(charsort)) {
  upperDissim <- upperDissim %>%
    mutate(
      character1 = 
        ifelse(leftCharacter == charsort[i] |
                 rightCharacter == charsort[i],
               as.character(charsort[i]),
               as.character(character1)),
      character2 = 
        ifelse(character1 == leftCharacter,
               as.character(rightCharacter),
               as.character(leftCharacter))) %>%
    mutate(character1 = factor(character1),
           character2 = factor(character2))
}

# make upper matrix of dissimilarity values
upperDissim <- upperDissim %>%
  select(subid, condition, character1, character2, responseNum) %>%
  group_by(character1, character2) %>%
  mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
  summarise(mean = mean(dist, na.rm = TRUE)) %>%
  spread(character2, mean)

# add in NA column for charlie_dog, NA row for you
upperDissim$charlie_dog = NA
upperDissim[13,] = c("you", rep(NA, 13))

# reorder columns
upperDissim = upperDissim[, c(1, 14, 2:13)]

# rename rows and columns
names = sort(charsort, decreasing = FALSE)
upperDissim = upperDissim[-1]
rownames(upperDissim) = names
colnames(upperDissim) = names

# fill in lower triangle matrix
for(i in 1:12) {
  for(j in (i+1):13) {
    upperDissim[j,i] = upperDissim[i,j]
  }
}

# replace NAs with 0 and convert to numeric
for(i in 1:13) {
  upperDissim[i,i] = 0
}

# Convert to numeric matrix form 
upperDissim = data.matrix(upperDissim)

# ----------------> do MDS ----------------------------------------------------
# do MDS, pull out x_all and y_all coords
fit_all <- cmdscale(upperDissim, eig = TRUE, k = 2)
x_all <- fit_all$points[, 1]
y_all <- fit_all$points[, 2]

# convert to a dataframe
pts <- data.frame(x = x_all, y = y_all, character = row.names(upperDissim)) %>%
  mutate(character = 
           ifelse(character == "charlie_dog", "dog",
                  ifelse(character == "delores_gleitman_deceased", "dead woman",
                         ifelse(character == "gerald_schiff_pvs", "PVS man", 
                                ifelse(character == "green_frog", "frog",
                                       ifelse(character == "samantha_hill_girl", "girl",
                                         ifelse(character == "kismet_robot", "robot",
                                                ifelse(character == "nicholas_gannon_baby", "baby",
                                                       ifelse(character == "sharon_harvey_woman", "woman",
                                                              ifelse(character == "toby_chimp",
                                                                     "chimp",
                                                                     ifelse(character == "todd_billingsley_man", "man",
                                                                            as.character(character))))))))))))

# plot!
ggplot(pts, aes(x = x_all, y = y_all, label = character)) +
  geom_point() +
  geom_text(angle = 0,
            vjust = -1,
            size = 6) +
  xlim(-1, 1) +
  ylim(-1, 1) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Multidimensional scaling of characters: All 4 conditions\n",
       x = NULL,
       y = NULL)

# plot! rotated...
ggplot(pts, aes(x = y_all, y = -x_all, label = character)) +
  geom_point() +
  geom_text(angle = 0,
            vjust = -1,
            size = 6) +
  ylim(-1, 1) +
  xlim(-1, 1) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(title = "Multidimensional scaling of characters: All 4 conditions\n",
       x = NULL,
       y = NULL)

# --------> MDS 2: each condition separately ----------------------------------

# ----------------> data formatting & MDS -------------------------------------

for(k in 1:length(levels(dd$condition))) {
  condition_temp = levels(dd$condition)[k]
  
  upperDissim_temp = NULL
  fit_temp = NULL
  x_temp = NULL
  y_temp = NULL
  pts_temp = NULL
  
  # make alphabetized list of characters, cycle through to fill in alphabetized pairs
  upperDissim_temp <- dd %>%
    filter(condition == condition_temp) %>%
    mutate(character1 = array(),
           character2 = array())
  
  charsort = sort(levels(upperDissim_temp$leftCharacter), decreasing = TRUE)
  
  for(i in 1:length(charsort)) {
    upperDissim_temp <- upperDissim_temp %>%
      mutate(
        character1 = 
          ifelse(leftCharacter == charsort[i] |
                   rightCharacter == charsort[i],
                 as.character(charsort[i]),
                 as.character(character1)),
        character2 = 
          ifelse(character1 == leftCharacter,
                 as.character(rightCharacter),
                 as.character(leftCharacter))) %>%
      mutate(character1 = factor(character1),
             character2 = factor(character2))
  }
  
  # make upper matrix of dissimilarity values
  upperDissim_temp <- upperDissim_temp %>%
    select(subid, condition, character1, character2, responseNum) %>%
    group_by(character1, character2) %>%
    mutate(dist = abs(responseNum)) %>% # use absolute values of comparison scores to get distance
    summarise(mean = mean(dist, na.rm = TRUE)) %>%
    spread(character2, mean)
  
  # add in NA column for charlie_dog, NA row for you
  upperDissim_temp <- upperDissim_temp %>%
    mutate(charlie_dog = NA,
           character1 = as.character(character1)) %>%
    rbind(c("you", rep(NA, 13))) %>%
    mutate(character1 = factor(character1))
  
  # reorder columns
  upperDissim_temp = upperDissim_temp[, c(1, 14, 2:13)]
  
  # rename rows and columns
  names = sort(charsort, decreasing = FALSE)
  names = 
    ifelse(names == "charlie_dog", "dog",
           ifelse(names == "delores_gleitman_deceased", "dead woman",
                  ifelse(names == "gerald_schiff_pvs", "PVS man",
                         ifelse(names == "green_frog", "frog",
                                ifelse(names == "samantha_hill_girl", "girl",
                                       ifelse(names == "kismet_robot", "robot",
                                              ifelse(names == "nicholas_gannon_baby", "baby",
                                                     ifelse(names == "sharon_harvey_woman", "woman",
                                                            ifelse(names == "toby_chimp", "chimp",
                                                                   ifelse(names == "todd_billingsley_man", "man",
                                                                          as.character(names)))))))))))
  
  upperDissim_temp = upperDissim_temp[-1]
  rownames(upperDissim_temp) = names
  colnames(upperDissim_temp) = names
  
  # fill in lower triangle matrix
  for(i in 1:12) {
    for(j in (i+1):13) {
      upperDissim_temp[j,i] = upperDissim_temp[i,j]
    }
  }
  
  # replace NAs with 0 and convert to numeric
  for(i in 1:13) {
    upperDissim_temp[i,i] = 0
  }
  
  # Convert to numeric matrix form 
  upperDissim_temp = data.matrix(upperDissim_temp)
  
  # do MDS, pull out x_temp and y_temp coords
  fit_temp <- cmdscale(upperDissim_temp, eig = TRUE, k = 2)
  x_temp <- fit_temp$points[, 1]
  y_temp <- fit_temp$points[, 2]
  
  # convert to a dataframe and join in category_temp labels
  pts_temp <- data.frame(x = x_temp, y = y_temp, character = row.names(upperDissim_temp))
  
  # plot!
  print(
    ggplot(pts_temp, aes(x = x_temp, y = y_temp, label = character)) +
      geom_point() +
      geom_text(angle = 0,
                vjust = -1,
                size = 6) +
#       xlim(-1.2, 1.05) +
#       ylim(-0.8, .75) +
      theme_bw() +
      theme(text = element_text(size = 20)) +
            labs(title = paste0("MDS: ",condition_temp,"\n"),
           x = NULL,
           y = NULL)
  )
}

# --- ADDITIONAL ALTERNATIVE ANALYES ------------------------------------------

# --------> MAXIMUM LIKELIHOOD FACTOR ANALYSIS A ------------------------------
# Roughly equivalent to pca_A?
# Could also do the parallel version of pca_B

# Factor analysis
fa1 = factanal(d1, 
               factors = 1, 
               rotation = "varimax", 
               na.action = na.omit, 
               scores =  'regression', 
               cutoff = .4)
print(fa1)

# --------> HIERARCHICAL CLUSTER ANALYSIS A -----------------------------------
# Roughly equivalent to pca_A
# Could also do the parallel version of pca_B

# Construct dissimilarity matrix
d2 = as.dist((1-cor(d1))/2)

# Conduct hierarchical cluster analysis
hca = hclust(d2); hca

# Plot dendogram
par(mfrow=c(1,2))
rs1=hclust(d2)
rs1$merge
plot(rs1$height)
plot(rs1)

