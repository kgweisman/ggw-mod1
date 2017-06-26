# first run ggw-mod_analysis
library(cluster)

subids_nonzero <- dd %>% 
  group_by(subid) %>%
  summarise(var = var(responseNum)) %>%
  filter(var > 0) %>%
  distinct()

d_clust <- dd %>%
  # filter(subid %in% subids_nonzero$subid) %>%
  mutate(subid = paste(condition, subid, sep = "_")) %>%
  select(subid, leftCharacter, rightCharacter, responseNum) %>% 
  mutate(leftCharacter = factor(leftCharacter),
         rightCharacter = factor(rightCharacter),
         leftCharacterNum = as.numeric(leftCharacter),
         rightCharacterNum = as.numeric(rightCharacter),
         char1 = ifelse(leftCharacterNum < rightCharacterNum, 
                        as.character(leftCharacter), as.character(rightCharacter)),
         char2 = ifelse(leftCharacterNum > rightCharacterNum, 
                        as.character(leftCharacter), as.character(rightCharacter)),
         pair = factor(paste(char1, char2, sep = "_"))) %>%
  select(subid, pair, responseNum) %>% 
  spread(pair, responseNum) %>%
  remove_rownames() %>%
  column_to_rownames("subid")

d_clust_t <- data.frame(t(d_clust))

# fa.sort(principal(d_clust_t[,1:50], nfactors = 10))
# fa.sort(fa(d_clust_t[,1:50], nfactors = 10))
# hclust(d_clust_t[,1:50])

# clust_agnes <- agnes(d_clust)
# clust_agnes
# plot(clust_agnes)
# 
# clust_clara <- clara(d_clust, k = 5)
# clust_clara
# plot(clust_clara)
# 
# clust_pam <- pam(d_clust, k = 5)
# clust_pam
# plot(clust_pam)
# 
# clust_efa <- fa(t(d_clust), nfactors = 5)
# fa.sort(loadings(clust_efa)[]) %>% View()

clust_kmeans <- kmeans(d_clust, centers = 3)
clust_kmeans
kmeans_clusters <- data.frame(cluster = clust_kmeans$cluster) %>%
  rownames_to_column("subid") %>% 
  mutate(capacity = factor(gsub("_us_run.*$", "", subid)),
         cluster = factor(cluster))
kmeans_clusters2 <- kmeans_clusters %>%
  count(capacity, cluster) %>%
  complete(cluster, fill = list(n = 0)) %>%
  mutate(prop = n/sum(n)) %>%
  select(-n) %>%
  group_by(capacity) %>%
  arrange(desc(prop)) %>%
  top_n(1) %>%
  rename(dom_clust = cluster,
         dom_count = prop)
kmeans_clusters3 <- kmeans_clusters %>%
  left_join(kmeans_clusters2 %>% select(capacity, dom_clust, dom_count)) %>%
  mutate(dom_clust = factor(dom_clust))

kmeans_clusters3_labs <- kmeans_clusters3 %>%
  select(capacity, dom_clust, dom_count) %>%
  group_by(capacity) %>%
  top_n(1, wt = dom_clust) %>%
  ungroup() %>%
  distinct()

ggplot(kmeans_clusters3, 
       aes(x = reorder(reorder(capacity, dom_count), desc(as.numeric(dom_clust))), 
           fill = cluster)) +
  geom_bar(stat = "count", position = "fill") +
  annotate("text", x = kmeans_clusters3_labs$capacity, y = 0.1, 
           label = paste0("cluster ", kmeans_clusters3_labs$dom_clust, " (", 
                          100*round(kmeans_clusters3_labs$dom_count, 2), "%)")) +
  # geom_hline(yintercept =  0.5, lty = 3) +
  # geom_hline(yintercept =  1/3, lty = 3) +
  # geom_hline(yintercept =  2/3, lty = 3) +
  theme_bw() + 
  coord_flip() +
  theme(text = element_text(size = 16)) +
  labs(y = "proportion",
       x = "capacity")

#####

d_clust_ave <- dd %>%
  # filter(subid %in% subids_nonzero$subid) %>%
  mutate(subid = paste(condition, subid, sep = "_")) %>%
  select(subid, condition, leftCharacter, rightCharacter, responseNum) %>%
  mutate(leftCharacter = factor(leftCharacter),
         rightCharacter = factor(rightCharacter),
         leftCharacterNum = as.numeric(leftCharacter),
         rightCharacterNum = as.numeric(rightCharacter),
         char1 = ifelse(leftCharacterNum < rightCharacterNum, 
                        as.character(leftCharacter), as.character(rightCharacter)),
         char2 = ifelse(leftCharacterNum > rightCharacterNum, 
                        as.character(leftCharacter), as.character(rightCharacter)),
         pair = factor(paste(char1, char2, sep = "_"))) %>%
  group_by(condition, pair) %>%
  summarise(mean = mean(responseNum, na.rm = T)) %>%
  spread(condition, mean) %>%
  data.frame() %>%
  remove_rownames() %>%
  column_to_rownames("pair")

fa(d_clust_ave, nfactors = 13)
fa(d_clust_ave, nfactors = 2, rotate = "varimax") %>% fa.sort()


#####

library(orclus)

d_clust_groups <- dd %>%
  # filter(subid %in% subids_nonzero$subid) %>%
  select(subid, condition, leftCharacter, rightCharacter, responseNum) %>% 
  mutate(leftCharacter = factor(leftCharacter),
         rightCharacter = factor(rightCharacter),
         leftCharacterNum = as.numeric(leftCharacter),
         rightCharacterNum = as.numeric(rightCharacter),
         char1 = ifelse(leftCharacterNum < rightCharacterNum, 
                        as.character(leftCharacter), as.character(rightCharacter)),
         char2 = ifelse(leftCharacterNum > rightCharacterNum, 
                        as.character(leftCharacter), as.character(rightCharacter)),
         pair = factor(paste(char1, char2, sep = "_"))) %>%
  select(subid, condition, pair, responseNum) %>% 
  spread(pair, responseNum) %>%
  mutate(conditionNum = as.numeric(factor(condition)),
         group_GGW = ifelse(condition %in% c("exercising_self-restraint", "telling_right_from_wrong",
                                             "remembering_things", "understanding_how_others_are_feeling",
                                             "having_intentions", "communicating_with_others",
                                             "having_thoughts"), 
                            "agency",
                            ifelse(condition %in% c("getting_hungry", "experiencing_fear",
                                                    "experiencing_pain", "experiencing_pleasure",
                                                    "getting_angry", "having_desires",
                                                    "having_a_personality", "being_conscious",
                                                    "experiencing_pride", "feeling_embarrassed",
                                                    "experiencing_joy"), 
                                   "experience",
                                   NA)),
         group_GGW_num = as.numeric(as.factor(group_GGW))) %>%
  remove_rownames() %>%
  column_to_rownames("subid")

# clust_or <- orclus(d_clust, k = 2, l = 1, k0 = 20)
clust_or <- orclass(d_clust, grouping = d_clust_groups$group_GGW_num, k = 3, l = 5, k0 = 20)
clust_or_clusters <- data.frame(clust_or$cluster.posteriors) %>%
  mutate(grouping = as.numeric(grouping)) %>%
  left_join(dd %>% select(condition) %>% mutate(grouping = as.numeric(condition)) %>% distinct())
  

or_clusters <- data.frame(cluster = clust_or$cluster) %>%
  rownames_to_column("subid") %>% 
  mutate(capacity = factor(gsub("_us_run.*$", "", subid)),
         cluster = factor(cluster))
or_clusters2 <- or_clusters %>%
  count(capacity, cluster) %>%
  complete(cluster, fill = list(n = 0)) %>%
  mutate(prop = n/sum(n)) %>%
  select(-n) %>%
  group_by(capacity) %>%
  arrange(desc(prop)) %>%
  top_n(1) %>%
  rename(dom_clust = cluster,
         dom_count = prop)
or_clusters3 <- or_clusters %>%
  left_join(or_clusters2 %>% select(capacity, dom_clust, dom_count)) %>%
  mutate(dom_clust = factor(dom_clust))

or_clusters3_labs <- or_clusters3 %>%
  select(capacity, dom_clust, dom_count) %>%
  group_by(capacity) %>%
  top_n(1, wt = dom_clust) %>%
  ungroup() %>%
  distinct()

ggplot(or_clusters3, 
       aes(x = reorder(reorder(capacity, dom_count), desc(as.numeric(dom_clust))), 
           fill = cluster)) +
  geom_bar(stat = "count", position = "fill") +
  annotate("text", x = or_clusters3_labs$capacity, y = 0.1, 
           label = paste0("cluster ", or_clusters3_labs$dom_clust, " (", 
                          100*round(or_clusters3_labs$dom_count, 2), "%)")) +
  # geom_hline(yintercept =  0.5, lty = 3) +
  # geom_hline(yintercept =  1/3, lty = 3) +
  # geom_hline(yintercept =  2/3, lty = 3) +
  theme_bw() + 
  coord_flip() +
  theme(text = element_text(size = 16)) +
  labs(y = "proportion",
       x = "capacity")
