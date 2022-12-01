#### Hypnogram Data ####
HypnoData <- vroom('Mid Data/ExperimentalGroupThree_ZT14LP_Hypnogram.csv.gz')
ExGrp3HZ <- vroom('Mid Data/ExperimentalGroupThree_Baseline_PowerBands.csv.gz')

HypnoDataComp <- HypnoData %>%
  mutate(HMS = (hour(DateTimeFix)+minute(DateTimeFix)/60+second(DateTimeFix)/3600)+0.031166667,
         Hour = (hour(DateTimeFix)),
         Group = 'LP') %>%
  select(FileName,Hour,HMS,Mouse,Genotype,Injection,GenoInj,Label,Group) %>%
  filter(Hour >= 13 & Hour < 16)

ExGrp3 <- ExGrp3HZ %>%
  mutate(HMS = (hour(ZTDateTime)+minute(ZTDateTime)/60+second(ZTDateTime)/3600),
         Hour = (hour(ZTDateTime)),
         Group = 'Baseline') %>%
  select(FileName,Hour,HMS,Mouse,Genotype,Injection,GenoInj,`Rodent Sleep`,Group) %>%
  rename(Label = `Rodent Sleep`)%>%
  filter(Hour >= 13 & Hour < 16)

LPComp <- rbind(HypnoDataComp,ExGrp3) 

LPComp2 <- LPComp %>%
  group_by(Mouse,FileName) %>%
  mutate(StateWS =case_when(Label == "S" | Label == "P" ~ "S", TRUE ~ Label),
         StateNumber = sequence(rle(as.character(StateWS))$lengths),
         SleepLengthCount = case_when(lead(StateNumber,n = 1) == 1~ (StateNumber*10)/60),
         SleepLengthCountFull = case_when(lead(StateNumber,n = 1) == 1 ~ (StateNumber*10)/60,T ~ 0),
         BoutNumber = case_when(SleepLengthCountFull == 0 ~ 0, SleepLengthCountFull > 0 ~ 1))

LPComp3 <- LPComp2 %>%
  filter(StateWS == 'S') %>%
  group_by(Mouse,Genotype, Hour,Injection,GenoInj,Group) %>%
  summarise(SleepAmount = (n()*10)/60,
            SleepLengthAv = mean(SleepLengthCount,na.rm = T),
            BoutNumberCount = sum(BoutNumber,na.rm = T)) %>%
  ungroup() %>% 
  complete(Hour,nesting(Mouse,Genotype,Injection,GenoInj,Group),fill = list(SleepAmount = 0, SleepLengthAv = 0, BoutNumberCount = 0)) %>%
  complete(Group,nesting(Mouse,Genotype,Injection,GenoInj,Hour),fill = list(SleepAmount = 0, SleepLengthAv = 0, BoutNumberCount = 0)) %>%
  arrange(Mouse,Hour,Group)

# Amount of sleep Comparison
TotalSleepComp <- LPComp3 %>% 
  select(-SleepLengthAv,-BoutNumberCount) %>%
  spread(key = Group,value = SleepAmount) %>%
  mutate(Comparison = LP - Baseline)

ggbarplot(TotalSleepComp, x = "GenoInj", y = "Comparison", fill = "GenoInj",group = 'GenoInj',
          add = c('mean_se','point'),palette = 'aaas',facet.by = c('Hour'),
          label = T,lab.pos = 'in',lab.nb.digits = 0,lab.col = 'white',error.plot = 'upper_errorbar')+
  expand_limits(y = 0)+
  stat_compare_means(method = 'anova')+
  theme_classic2() +
  labs(x = '', 
       y = 'Sleep (min)', 
       title = 'Total Amount of Sleep',
       colour = 'Genotype + Injection',
       fill = 'Genotype + Injection')+
  theme(legend.position = 'none')

LPWTSleepComp <- TotalSleepComp %>% filter(Hour == 14 & Genotype == 'WT')

ggbarplot(LPWTSleepComp, x = "GenoInj", y = "Comparison", fill = "GenoInj",group = 'GenoInj',
          add = c('mean_se','point'),palette = 'aaas',
          label = T,lab.pos = 'in',lab.nb.digits = 0,lab.col = 'white',error.plot = 'upper_errorbar')+
  expand_limits(y = 0)+
  stat_compare_means(method = 't.test')+
  theme_classic2() +
  labs(x = '', 
       y = 'Sleep (min)', 
       title = 'Amount of sleep due to 35 Lux LP',
       caption = 'Amount of sleep per mouse at ZT 14 LP compared to Baseline',
       colour = 'Genotype + Injection',
       fill = 'Genotype + Injection')+
  theme(legend.position = 'none')

