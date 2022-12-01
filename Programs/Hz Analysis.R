#### Wake state ignoredImport Ex3 Data ####
HzOrigional <- vroom('Mid Data/ExperimentalGroupThree_ZT14__HZ_LP.csv.gz')

HzSummary_LP <- HzOrigional %>%
  mutate(HM = hour(DateTime)+minute(DateTime)/60) %>%
  filter(HM > 14 & HM < 15) %>%
  filter(Stage != 'Artifact') %>%
  select(-EpochNo, -DateTime,-Stage, -Date,-FileRed,-FileName,-HM,-(`0.000000Hz`:`0.488281Hz`),-(`25.024414Hz`:`49.926758Hz`)) %>% #-`0.000000Hz`,-`0.122070Hz`,-`0.366211Hz`,-`0.488281Hz`
  group_by(Mouse,Genotype,Injection,GenoInj) %>%
  summarise_all(mean, na.rm = T)

HzSummaryAltered <- HzSummary_LP %>%
  gather(key = 'HZ', value = 'Level', -Mouse,-Genotype,-Injection,-GenoInj)  %>%
  mutate(MouseStage = paste(Mouse,sep = " ")) %>%
  ungroup() 

MouseNames <- unique(HzSummaryAltered$MouseStage)
datalist <- list()

for(i in seq_along(MouseNames)){
  DataFrame <- filter(HzSummaryAltered, MouseStage == MouseNames[i])
  DataFrameMut <- DataFrame %>%
    mutate(Percent = (Level/sum(Level))*100,
           Hz2 = as.numeric(gsub("Hz","", HZ)))
  datalist[[i]] <- DataFrameMut
}

HzFinal <- bind_rows(datalist)

HzCheck <- HzFinal  %>%
  group_by(Mouse) %>%
  summarise(Percent = sum(Percent))

ggplot(HzFinal, aes(x = Hz2,y = Percent, group = Injection, colour = Injection,fill = Injection))+
  annotate(geom = 'rect',xmin = 0.5,xmax = 4,ymin = -Inf,ymax = Inf, fill = 'skyblue',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 4,xmax = 8,ymin = -Inf,ymax = Inf, fill = 'red',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 8,xmax = 12,ymin = -Inf,ymax = Inf, fill = 'green',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 12,xmax = 16,ymin = -Inf,ymax = Inf, fill = 'orange',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 16,xmax = 24,ymin = -Inf,ymax = Inf, fill = 'purple',alpha = 0.1)+
  stat_summary(geom = 'line',fun.y = 'mean')+
  stat_summary(geom = 'errorbar', fun.data = 'mean_se',alpha=0.3)+
  theme_classic()+
  facet_grid(~Genotype)+
  labs(title = "ZT14 LP Wake state ignored", x = "Frequency (Hz)", y = "Relative Power(%)")+
  theme(plot.title = element_text(hjust = 0.5,size = 20))

#### Import Ex3 Data ####
HzOrigional <- vroom('Mid Data/ExperimentalGroupThree_ZT14__HZ_LP.csv.gz')

HzSummary_LP <- HzOrigional %>%
  mutate(HM = hour(DateTime)+minute(DateTime)/60) %>%
  filter(HM > 14 & HM < 15) %>%
  filter(Stage != 'Artifact') %>%
  select(-EpochNo, -DateTime, -Date,-FileRed,-FileName,-HM,-(`0.000000Hz`:`0.488281Hz`),-(`25.024414Hz`:`49.926758Hz`)) %>% #-`0.000000Hz`,-`0.122070Hz`,-`0.366211Hz`,-`0.488281Hz`
  group_by(Stage,Mouse,Genotype,Injection,GenoInj) %>%
  summarise_all(mean, na.rm = T)

HzSummaryAltered <- HzSummary_LP %>%
  gather(key = 'HZ', value = 'Level', -Stage, -Mouse,-Genotype,-Injection,-GenoInj)  %>%
  mutate(MouseStage = paste(Mouse,Stage,sep = " ")) %>%
  ungroup() 

MouseNames <- unique(HzSummaryAltered$MouseStage)
datalist <- list()

for(i in seq_along(MouseNames)){
  DataFrame <- filter(HzSummaryAltered, MouseStage == MouseNames[i])
  DataFrameMut <- DataFrame %>%
    mutate(Percent = (Level/sum(Level))*100,
           Hz2 = as.numeric(gsub("Hz","", HZ)))
  datalist[[i]] <- DataFrameMut
}

HzFinal <- bind_rows(datalist)

HzCheck <- HzFinal  %>%
  group_by(Mouse,Stage) %>%
  summarise(Percent = sum(Percent))

# Control DD vs. Vehicle DD #

ggplot(HzFinal, aes(x = Hz2,y = Percent, group = Genotype, colour = Genotype,fill = Genotype))+
  annotate(geom = 'rect',xmin = 0.5,xmax = 4,ymin = -Inf,ymax = Inf, fill = 'skyblue',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 4,xmax = 8,ymin = -Inf,ymax = Inf, fill = 'red',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 8,xmax = 12,ymin = -Inf,ymax = Inf, fill = 'green',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 12,xmax = 16,ymin = -Inf,ymax = Inf, fill = 'orange',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 16,xmax = 24,ymin = -Inf,ymax = Inf, fill = 'purple',alpha = 0.1)+
  stat_summary(geom = 'line',fun.y = 'mean')+
  stat_summary(geom = 'errorbar', fun.data = 'mean_se',alpha=0.3)+
  theme_classic()+
  facet_grid(Stage~Injection)+
  labs(title = "ZT13 of ZT 14 LP", x = "Frequency (Hz)", y = "Relative Power(%)")+
  theme(plot.title = element_text(hjust = 0.5,size = 20))
