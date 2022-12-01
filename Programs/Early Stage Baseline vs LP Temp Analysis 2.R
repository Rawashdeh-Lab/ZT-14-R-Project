# How much sleep occured during the LP compared to ZT 13 - 15 baseline #
# Baseline Data
BaselineDataG3 <- vroom('Raw Data/Experimental Group 3/ExperimentalGroupThree_4s_SD_Baseline_TempAct.csv.gz') %>%
  mutate(Condition = "Baseline",ZTDateTime = DateTime, ExGrp = "ExGrp3")

BaselineDataGA <- vroom('Raw Data/Experimental Group A/ExperimentalGroupA_SD_Baseline_TempAct_4s.csv.gz') %>%
  mutate(Condition = "Baseline",ZTDateTime = DateTime, ExGrp = "ExGrpA")

BaselineDataGB <- vroom('Raw Data/Experimental Group B/ExperimentalGroupB_SD_Baseline_TempAct_4s.csv.gz') %>%
  mutate(Condition = "Baseline",ZTDateTime = DateTime-21600, ExGrp = "ExGrpB")

BaselineData <- rbind(BaselineDataG3,BaselineDataGA,BaselineDataGB) %>%
  arrange(desc(Genotype),Injection,Mouse,ZTDateTime) %>%
  mutate( GenoInj = case_when(GenoInj == "WT MONO" ~ "WT SAL",
                              GenoInj == "PerKO MONO" ~ "Per1KO MONO",
                              GenoInj == "PerKO OHDA" ~ "Per1KO OHDA",
                              GenoInj == "Per1KO MONO" ~ "Per1KO SAL",
                              GenoInj == "PerKO PFF" ~ "Per1KO PFF",
                             T ~ GenoInj))

# ZT 14 LP Data
ZT14LPG3 <- vroom('Raw Data/Experimental Group 3/ExperimentalGroupThree_4s_ZT14LP_TempAct.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime, ExGrp = "ExGrp3")

ZT14LPGA <- vroom('Raw Data/Experimental Group A/ExperimentalGroupA_ZT14LP_TempAct_4s.csv.gz') %>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime, ExGrp = "ExGrpA")

ZT14LPGB <- vroom('Raw Data/Experimental Group B/ExperimentalGroupB_ZT14LP_TempAct_4s.csv.gz') %>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime-21600, ExGrp = "ExGrpB")

ZT14LPData <- rbind(ZT14LPG3,ZT14LPGA,ZT14LPGB) %>%
  mutate(GenoInj = case_when(GenoInj == "WT MONO" ~ "WT SAL",
                              GenoInj == "PerKO MONO" ~ "Per1KO MONO",
                              GenoInj == "PerKO OHDA" ~ "Per1KO OHDA",
                              GenoInj == "Per1KO MONO" ~ "Per1KO SAL",
                              GenoInj == "PerKO PFF" ~ "Per1KO PFF",
                              T ~ GenoInj))

# Put them into one dataset and add hours
CombinedData <- rbind(BaselineData,ZT14LPData) %>%
  mutate(Hour = hour(ZTDateTime),
         Injection = case_when(Injection == "MONO" ~ "SAL",
                              T ~ Injection),)
View(unique(CombinedData[c("Mouse","GenoInj", "Condition")]) %>%
       arrange(GenoInj,Mouse))

# Select out hours needed 
CombinedData_2 <- CombinedData %>%
  mutate(HMS = hour(ZTDateTime)+minute(ZTDateTime)/60+second(ZTDateTime)/3600) %>%
  filter(HMS >= 13.5 & HMS <= 15.5)%>%
  mutate(GenoInj = factor(GenoInj, levels = c("WT SAL","WT PFF","WT OHDA","Per1KO SAL","Per1KO PFF","Per1KO OHDA")),
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
         GenoInj = factor(GenoInj, levels = c("WT SAL","WT PFF","WT OHDA","Per1KO SAL","Per1KO PFF","Per1KO OHDA")),
         Genotype = factor(Genotype, levels = c("WT","Per1KO")))

LPOnlyActAnalysis_Summary <- CombinedData_2 %>%
  group_by(Mouse,Genotype,Injection,GenoInj,Condition) %>%
  summarise(Activity = sum(Activity,na.rm = T)) %>%
  ungroup()%>%
  mutate(GenoInj = factor(GenoInj, levels = c("WT SAL","WT PFF","WT OHDA","Per1KO SAL","Per1KO PFF","Per1KO OHDA")),
         Genotype = factor(Genotype, levels = c("WT","Per1KO")))

# Plot data comparison #
PFFcolourscheme <- c("royalblue3","turquoise3","firebrick2","darkorchid2")

ggplot(TempAnalysisLP_Summary, aes(x = Condition, y = Temp, colour = GenoInj, group = Mouse))+
  geom_point()+
  geom_line()+
  facet_grid(Genotype ~ Injection)+ 
  # scale_color_manual(values = PFFcolourscheme)+
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

# Extract for graphpad
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
