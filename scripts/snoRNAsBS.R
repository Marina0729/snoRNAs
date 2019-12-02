.libPaths(c("C:/Users/ale097/Data School/Packages"))
library(tidyverse)

ribometh <- read_csv("data/Erales_2017_ribomethseq.csv")

snoRNAs <- read_csv("data/Jorjani_2016_snoRNA_atlas.csv")

# tidy data  
ribometh_tidy <- ribometh %>%
  mutate(mean_diff_percent = sub("%","", mean_diff )) %>% 
  mutate(mean_diff_percent = sub("-", "", mean_diff_percent)) %>% 
  mutate(mean_diff_percent = as.numeric(mean_diff_percent)) %>% 
  filter(mean_diff_percent > 40) %>% 
  filter(SNORD != "NA")


filtered_snoRNAs <- snoRNAs %>% 
  filter(Name == "SNORD18B"|Name == "SNORD68"| 
           Name == "SNORD48"|Name == "SNORD24"| 
           Name == "SNORD88C") %>%
  select(Name, Expression, Conservation, `Host gene`) %>% 
  rename(SNORD = Name)

FBL_snoRNA <- left_join(ribometh_tidy, filtered_snoRNAs, by = "SNORD")

ggplot(FBL_snoRNA, aes(x= site, y= mean_diff_percent)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(aes(label = SNORD, color = Conservation), hjust = 0.5, vjust = -0.5, size = 3) +
  geom_text(aes(label = `Host gene`), hjust = 0.5, vjust = 1.5, size = 2.5)+
  labs( y = "% change in methylation by siFBL",
        x = "28S rRNA site")



            