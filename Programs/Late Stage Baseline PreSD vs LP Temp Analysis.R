# How much sleep occured during the LP compared to ZT 13 - 15 baseline #
# Baseline Data
BaselineDataG2 <- vroom('Mid Data/Baseline/ExperimentalGroupTwo_4s_SD_Baseline_TempAct.csv.gz')%>%
  mutate(Condition = "Baseline", ZTDateTime = DateTime)  %>%
  filter(!(Mouse %in% c(749,751)))

BaselineDataG5 <- vroom('Raw Data/Experimental Group 5/ExperimentalGroupFive_4s_SD_Baseline_TempAct.csv.gz') %>%
  mutate(Condition = "Baseline", ZTDateTime = DateTime)

BaselineDataG7 <- vroom('Raw Data/Experimental Group 7/ExperimentalGroupSeven_4s_Baseline_TempAct.csv.gz') %>%
  mutate(Condition = "Baseline") %>% dplyr::filter(Day == 0) %>%
  filter(!(Mouse %in% c(1126)))

# Pool Data Together #
BaselineData <- rbind(BaselineDataG2,BaselineDataG5,BaselineDataG7) %>%
  arrange(desc(Genotype),Injection,Mouse,ZTDateTime) %>%
  mutate(Genotype = case_when(Genotype == "PerKO" ~ "Per1KO",
                              T ~ Genotype),
         GenoInj = case_when(GenoInj == "PerKO MONO" ~ "Per1KO MONO",
                             GenoInj == "PerKO PFF" ~ "Per1KO PFF",
                             T ~ GenoInj))


# ZT 14 LP Data
ExG2ZT14LP1Data <- vroom('Mid Data/ZT14 LP 1 4s/ExperimentalGroupTwo_ZT14LP_1_TempAct.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime) %>%
  filter(Mouse %in% c(737,749,750,449,442,446,447,448))

# ZT 14 LP2 Data
ExG2ZT14LP2Data <- vroom('Mid Data/ZT14 LP 2 4s/ExperimentalGroupTwo_ZT14LP_2_TempAct.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime)%>%
  filter(Mouse %in% c(738,751,450)) %>%
  filter(!(Mouse %in% c(751)))

ExGrp5LPPower <- vroom('Raw Data/Experimental Group 5/ExperimentalGroupFive_4s_LP_TempAct.csv.gz') %>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime)

ExGrp7LPPower <- vroom('Raw Data/Experimental Group 7/ExperimentalGroupSeven_G1_4s_LP_TempAct.csv.gz') %>%
  mutate(Condition = "ZT14LP")%>%
  filter(!(Mouse %in% c(1126)))

ZT14LPData <- rbind(ExG2ZT14LP1Data,ExG2ZT14LP2Data,ExGrp5LPPower,ExGrp7LPPower)

# Put them into one dataset and add hours
CombinedData <- rbind(BaselineData,ZT14LPData) %>%
  mutate(Hour = hour(ZTDateTime))

# Select out hours needed 
CombinedData_2 <- CombinedData %>%
  filter(HMS >= 13.5 & HMS <= 15.5)%>%
  mutate(GenoInj = factor(GenoInj, levels = c("WT MONO","WT PFF","Per1KO MONO","Per1KO PFF")),
         Genotype = factor(Genotype, levels = c("WT","Per1KO")))

BaselineLPTemp <- CombinedData_2 %>%
  filter(Condition == "Baseline" & `Markers (X)[Coverage]` == 0 & Hour == 14) %>%
  group_by(Mouse) %>%
  summarise(BaselineTemp = mean(Temp))

CombinedData_3 <- left_join(CombinedData_2,BaselineLPTemp, by = "Mouse") %>%
  mutate(NormalisedTemp = ((Temp/BaselineTemp)))%>%
  filter(`Markers (X)[Coverage]` == 0)  

# Split Light pulse in 30 min bins pre, during first half second half
TempAnalysisLP <- CombinedData_2 %>%
  filter(`Markers (X)[Coverage]` == 0)  

# Average per group for analyisis
BaselineLPTemp <- CombinedData_2 %>%
  filter(Condition == "Baseline" & `Markers (X)[Coverage]` == 0 & Hour == 14) %>%
  group_by(GenoInj) %>%
  summarise(BaselineTemp = mean(Temp))

TempAnalysisLP_Summary <- left_join(TempAnalysisLP,BaselineLPTemp,by = "GenoInj") %>%
  filter(Hour == 14) %>%
  group_by(Mouse,Genotype,Injection,GenoInj,Condition,BaselineTemp) %>%
  summarise(Temp = mean(Temp,na.rm = T)) %>%
  ungroup()%>%
  mutate(NormalisedTemp = Temp/BaselineTemp,
         GenoInj = factor(GenoInj, levels = c("WT MONO","WT PFF","Per1KO MONO","Per1KO PFF")),
         Genotype = factor(Genotype, levels = c("WT","Per1KO")))

LPOnlyActAnalysis_Summary <- CombinedData_2 %>%
  group_by(Mouse,Genotype,Injection,GenoInj,Condition) %>%
  summarise(Activity = sum(Activity,na.rm = T)) %>%
  ungroup()%>%
  mutate(GenoInj = factor(GenoInj, levels = c("WT MONO","WT PFF","Per1KO MONO","Per1KO PFF")),
         Genotype = factor(Genotype, levels = c("WT","Per1KO")))

# Plot data comparison #
PFFcolourscheme <- c("royalblue3","turquoise3","firebrick2","darkorchid2")

ggplot(TempAnalysisLP_Summary, aes(x = Condition, y = Temp, colour = GenoInj, group = Mouse))+
  geom_point()+
  geom_line()+
  facet_grid(Genotype ~ Injection)+ 
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Temprature (C)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggplot(CombinedData_3, aes(x = HMS, y = NormalisedTemp, colour = Condition,fill = Condition,group = Condition))+
  geom_vline(xintercept = c(14,15),linetype = 'dashed')+
  annotate(geom = 'rect',xmin = 13.5,xmax = 14,ymin = -Inf,ymax = Inf,fill = 'grey',alpha = 0.5)+
  annotate(geom = 'rect',xmin = 15,xmax = 15.5,ymin = -Inf,ymax = Inf,fill = 'grey',alpha = 0.5)+
  # annotate(geom = 'rect',xmin = 14,xmax = 15,ymin = -Inf,ymax = Inf,fill = 'gold',alpha = 0.2)+
  stat_summary(geom = "ribbon", fun.data = 'mean_se',alpha = 0.2)+
  stat_summary(geom = "line", fun = 'mean',size = 1)+
  facet_grid(Genotype ~ Injection)+ 
  scale_color_manual(values = c('black','red'))+
  scale_fill_manual(values = c('black','red'))+
  labs(y = "Normalised Temprature (fold change)",
       x = "Hour (ZT)",
       colour = "Genotype + Injection",
       fill = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggplot(CombinedData_3[which(CombinedData_3$Injection == "MONO"),], aes(x = HMS, y = NormalisedTemp, colour = Condition,fill = Condition,group = Condition))+
  geom_vline(xintercept = c(14,15),linetype = 'dashed')+
  annotate(geom = 'rect',xmin = 13.5,xmax = 14,ymin = -Inf,ymax = Inf,fill = 'grey',alpha = 0.5)+
  annotate(geom = 'rect',xmin = 15,xmax = 15.5,ymin = -Inf,ymax = Inf,fill = 'grey',alpha = 0.5)+
  # annotate(geom = 'rect',xmin = 14,xmax = 15,ymin = -Inf,ymax = Inf,fill = 'gold',alpha = 0.2)+
  stat_summary(geom = "ribbon", fun.data = 'mean_se',alpha = 0.2)+
  stat_summary(geom = "line", fun = 'mean',size = 1)+
  facet_grid(~Genotype)+ 
  scale_color_manual(values = c('black','red'))+
  scale_fill_manual(values = c('black','red'))+
  labs(y = "Normalised Body Temprature (fold change)",
       x = "Hour (ZT)",
       colour = "Genotype",
       fill = "Genotype")+
  theme_stata(scheme = 's1mono',base_size = 12)+
  theme(legend.position = 'bottom')


ggplot(TempAnalysisLP_Summary[which(TempAnalysisLP_Summary$Injection == "MONO"),], aes(x = Condition, y = Temp, colour = GenoInj, group = Mouse))+
  stat_summary(aes(group = Genotype),geom = "errorbar", fun.data = 'mean_se', width = 0.5,size = 1)+
  stat_summary(aes(group = Genotype),geom = "point", fun = 'mean',size = 3,shape = 15)+
  stat_summary(aes(group = Genotype),geom = "line", fun = 'mean',size = 1)+
  geom_point()+
  geom_line(size = 0.5,colour = 'grey')+
  facet_grid(~Genotype)+ 
  scale_color_manual(values = c('royalblue3','firebrick2'))+
  labs(y = "Body temprature (C)",
       colour = "Genotype",
       x = "")+
  theme_stata(scheme = 's1mono',base_size = 15)+
  theme(legend.position = 'none')

ggplot(TempAnalysisLP_Summary[which(TempAnalysisLP_Summary$Injection == "MONO"),], aes(x = Condition, y = NormalisedTemp, colour = GenoInj, group = Mouse))+
  stat_summary(aes(group = Genotype),geom = "errorbar", fun.data = 'mean_se', width = 0.5,size = 1)+
  stat_summary(aes(group = Genotype),geom = "point", fun = 'mean',size = 3,shape = 15)+
  stat_summary(aes(group = Genotype),geom = "line", fun = 'mean',size = 1)+
  geom_point()+
  geom_line(size = 0.5,colour = 'grey')+
  facet_grid(~Genotype)+ 
  scale_color_manual(values = c('royalblue3','firebrick2'))+
  labs(y = "Normalised Body temprature (C)",
       colour = "Genotype",
       x = "")+
  theme_stata(scheme = 's1mono',base_size = 15)+
  theme(legend.position = 'none')

# Extract for graphpad prism #

CombinedData_3_Export <- CombinedData_3 %>%
  mutate(AllIDs = paste(Condition,Genotype,Injection,Mouse,sep = " ")) %>%
  arrange(Condition,GenoInj,Mouse)

cols <- unique(CombinedData_3_Export$AllIDs)

CombinedData_3_Export_2 <- CombinedData_3_Export %>%
  select(HMS,AllIDs,NormalisedTemp) %>%
  spread(key = AllIDs,value = NormalisedTemp,convert = F) %>%
  arrange(HMS) %>%
  select(HMS,c(cols))

write.table(CombinedData_3_Export_2,'clipboard-12800',sep="\t",row.names = F)

TempAnalysisLP_Summary_Export <- TempAnalysisLP_Summary %>%
  mutate(AllIDs = paste(Genotype,Injection,Mouse,sep = " ")) %>%
  arrange(Condition,GenoInj,Mouse)

cols <- unique(TempAnalysisLP_Summary_Export$AllIDs)

TempAnalysisLP_Summary_Export_2 <- TempAnalysisLP_Summary_Export %>%
  select(Condition,AllIDs,NormalisedTemp) %>%
  spread(key = AllIDs,value = NormalisedTemp,convert = F) %>%
  arrange(Condition) %>%
  select(Condition,c(cols))

write.table(TempAnalysisLP_Summary_Export_2,'clipboard-12800',sep="\t",row.names = F)

TempAnalysisLP_Summary_Export_2 <- TempAnalysisLP_Summary_Export %>%
  select(Condition,AllIDs,Temp) %>%
  spread(key = AllIDs,value = Temp,convert = F) %>%
  arrange(Condition) %>%
  select(Condition,c(cols))

write.table(TempAnalysisLP_Summary_Export_2,'clipboard-12800',sep="\t",row.names = F)

### Activity ####
ActivityData <- CombinedData %>%
  filter(HMS >= 13.5 & HMS <= 15.66)%>%
  mutate(GenoInj = factor(GenoInj, levels = c("WT MONO","WT PFF","Per1KO MONO","Per1KO PFF")),
         Genotype = factor(Genotype, levels = c("WT","Per1KO")),
         CorrectedAct = Activity - 0.05952,
         CorrectedAct = case_when(CorrectedAct < 0 ~ 0, T ~ CorrectedAct),
         RoundedTime = floor_date(ZTDateTime, "10 min")) %>%
  group_by(Mouse,Genotype,Injection,GenoInj,Condition,RoundedTime) %>%
  summarise(Activity = sum(CorrectedAct)) %>%
  ungroup()%>%
  mutate(HMS = hour(RoundedTime)+minute(RoundedTime)/60)

ggplot(CombinedData_2[which(CombinedData_2$Injection == "MONO"),], aes(x = HMS, y = Activity, colour = Condition,fill = Condition,group = Condition))+
  geom_vline(xintercept = c(14,15),linetype = 'dashed')+
  annotate(geom = 'rect',xmin = 13.5,xmax = 14,ymin = -Inf,ymax = Inf,fill = 'grey',alpha = 0.5)+
  annotate(geom = 'rect',xmin = 15,xmax = 15.5,ymin = -Inf,ymax = Inf,fill = 'grey',alpha = 0.5)+
  # annotate(geom = 'rect',xmin = 14,xmax = 15,ymin = -Inf,ymax = Inf,fill = 'gold',alpha = 0.2)+
  stat_summary(geom = "ribbon", fun.data = 'mean_se',alpha = 0.2)+
  stat_summary(geom = "line", fun = 'mean',size = 1)+
  facet_grid(~Genotype)+ 
  scale_color_manual(values = c('black','red'))+
  scale_fill_manual(values = c('black','red'))+
  labs(y = "Activity",
       x = "Hour (ZT)",
       colour = "Genotype",
       fill = "Genotype")+
  theme_stata(scheme = 's1mono',base_size = 12)+
  theme(legend.position = 'bottom')

ggplot(ActivityData[which(ActivityData$Injection == "MONO"),], aes(x = HMS, y = Activity, colour = Condition,fill = Condition,group = Condition))+
  geom_vline(xintercept = c(14,15),linetype = 'dashed')+
  annotate(geom = 'rect',xmin = 13.5,xmax = 14,ymin = -Inf,ymax = Inf,fill = 'grey',alpha = 0.5)+
  annotate(geom = 'rect',xmin = 15,xmax = 15.5,ymin = -Inf,ymax = Inf,fill = 'grey',alpha = 0.5)+
  # annotate(geom = 'rect',xmin = 14,xmax = 15,ymin = -Inf,ymax = Inf,fill = 'gold',alpha = 0.2)+
  stat_summary(geom = "ribbon", fun.data = 'mean_se',alpha = 0.2)+
  stat_summary(geom = "line", fun = 'mean',size = 1)+
  facet_grid(~Genotype)+ 
  scale_color_manual(values = c('black','red'))+
  scale_fill_manual(values = c('black','red'))+
  labs(y = "Activity",
       x = "Hour (ZT)",
       colour = "Genotype",
       fill = "Genotype")+
  theme_stata(scheme = 's1mono',base_size = 12)+
  theme(legend.position = 'bottom')


