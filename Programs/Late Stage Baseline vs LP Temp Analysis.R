# How much sleep occured during the LP compared to ZT 13 - 15 baseline #
# Baseline Data
BaselineDataG2 <- vroom('Mid Data/Baseline/ExperimentalGroupTwo_4s_SD_Baseline_TempAct.csv.gz')%>%
  mutate(Condition = "Baseline", ZTDateTime = DateTime) 

BaselineDataG5 <- vroom('Raw Data/Experimental Group 5/ExperimentalGroupFive_4s_SD_Baseline_TempAct.csv.gz') %>%
  mutate(Condition = "Baseline", ZTDateTime = DateTime)

BaselineDataG7 <- vroom('Raw Data/Experimental Group 7/ExperimentalGroupSeven_4s_Baseline_TempAct.csv.gz') %>%
  mutate(Condition = "Baseline") %>% dplyr::filter(Day == 0)

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
  filter(Mouse %in% c(738,751,450))

ExGrp5LPPower <- vroom('Raw Data/Experimental Group 5/ExperimentalGroupFive_4s_LP_TempAct.csv.gz') %>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime)

ExGrp7LPPower <- vroom('Raw Data/Experimental Group 7/ExperimentalGroupSeven_G1_4s_LP_TempAct.csv.gz') %>%
  mutate(Condition = "ZT14LP")

ZT14LPData <- rbind(ExG2ZT14LP1Data,ExG2ZT14LP2Data,ExGrp5LPPower,ExGrp7LPPower)

# Put them into one dataset and add hours
CombinedData <- rbind(BaselineData,ZT14LPData) %>%
  mutate(Hour = hour(ZTDateTime))

# Select out hours needed 
CombinedData_2 <- CombinedData %>%
  filter(Hour >= 13 & Hour <= 16) %>%
  mutate(TimePoint = case_when(HMS >= 13.5 & HMS < 14 ~ "PreLP",
                   HMS >= 14 & HMS < 14.5 ~ "FirstHalf",
                   HMS >= 14.5 & HMS < 15 ~ "SecondHalf"))

# Check Data #
View(CombinedData_2 %>% 
       group_by(Condition,Mouse,Hour) %>% 
       slice(which.min(DateTime)))

# Split Light pulse in 30 min bins pre, during first half second half
LPOnlyAnalysis <- CombinedData_2 %>%
  filter(HMS >= 13.5 & HMS < 15 & `Markers (X)[Coverage]` == 0 & Condition == "ZT14LP")  

LPActOnlyAnalysis <- CombinedData_2 %>%
  filter(HMS >= 13.5 & HMS < 15 & Condition == "ZT14LP")  

# Average per group for analyisis
LPOnlyAnalysis_Summary <- LPOnlyAnalysis %>%
  group_by(Mouse,Genotype,Injection,GenoInj,TimePoint) %>%
  summarise(Temp = mean(Temp,na.rm = T)) %>%
  ungroup()

LPOnlyActAnalysis_Summary <- LPActOnlyAnalysis %>%
  group_by(Mouse,Genotype,Injection,GenoInj,TimePoint) %>%
  summarise(Activity = sum(Activity,na.rm = T)) %>%
  ungroup()

# Create Baseline Temp from non-LP so normalise LP data
BaselineLPTemp <- LPOnlyAnalysis_Summary %>%
  filter(TimePoint == "PreLP") %>%
  select(Mouse,Temp) %>%
  rename(BaselineTemp = Temp)

BaselineActLPTemp <- LPOnlyActAnalysis_Summary %>%
  filter(TimePoint == "PreLP") %>%
  select(Mouse,Activity) %>%
  rename(BaselineAct = Activity)

LPOnlyAnalysis_Summary_Norm <- left_join(LPOnlyAnalysis_Summary,BaselineLPTemp, by = "Mouse") %>%
  mutate(NormalisedTemp = ((Temp/BaselineTemp)*100)-100,
         TimePoint = factor(TimePoint, levels = c("PreLP","FirstHalf","SecondHalf"), labels = c("Pre-Light Pulse","First 30 min","Last 30 min")),
         GenoInj = factor(GenoInj, levels = c("WT MONO","WT PFF","Per1KO MONO","Per1KO PFF")),
         Genotype = factor(Genotype, levels = c("WT","Per1KO")))

LPOnlyAnalysis_Summary_Norm <- left_join(LPOnlyActAnalysis_Summary,BaselineActLPTemp, by = "Mouse") %>%
  mutate(NormalisedAct = ((Activity/BaselineAct)*100)-100,
         TimePoint = factor(TimePoint, levels = c("PreLP","FirstHalf","SecondHalf"), labels = c("Pre-Light Pulse","First 30 min","Last 30 min")),
         GenoInj = factor(GenoInj, levels = c("WT MONO","WT PFF","Per1KO MONO","Per1KO PFF")),
         Genotype = factor(Genotype, levels = c("WT","Per1KO")))

# Plot data comparison #
PFFcolourscheme <- c("royalblue3","turquoise3","firebrick2","darkorchid2")

ggplot(LPOnlyAnalysis_Summary_Norm, aes(x = TimePoint, y = Temp, colour = GenoInj, group = Mouse))+
  geom_point()+
  geom_line()+
  facet_grid(Genotype ~ Injection)+ 
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Temprature (C)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggpaired(data = LPOnlyAnalysis_Summary_Norm,
         x = 'TimePoint',y = 'Temp',
         id = 'Mouse',color = 'GenoInj', line.color = 'grey',
         facet.by = c('Genotype','Injection'))+ 
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Temprature (C)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggplot(LPOnlyAnalysis_Summary_Norm, aes(x = TimePoint, y = Temp, colour = GenoInj, group = GenoInj))+
  stat_summary(geom = "errorbar", fun.data = 'mean_se', width = 0.5,size = 1)+
  stat_summary(geom = "line", fun = 'mean',size = 1)+
  stat_summary(geom = "point", fun = 'mean',size = 4)+
  facet_grid(~ Genotype)+
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Temprature (C)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggplot(LPOnlyAnalysis_Summary_Norm, aes(x = TimePoint, y = Temp, colour = GenoInj, group = GenoInj))+
  stat_summary(geom = "errorbar", fun.data = 'mean_se', width = 0.5,size = 1)+
  stat_summary(geom = "line", fun = 'mean',size = 1)+
  stat_summary(geom = "point", fun = 'mean',size = 4)+
  facet_grid(~ Injection)+
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Temprature (C)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggplot(LPOnlyAnalysis_Summary_Norm, aes(x = TimePoint, y = NormalisedTemp, colour = GenoInj, group = Mouse))+
  geom_point()+
  geom_line()+
  facet_grid(Genotype ~ Injection)+ 
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Normalised Temprature change (%)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggpaired(data = LPOnlyAnalysis_Summary_Norm,
         x = 'TimePoint',y = 'NormalisedTemp',
         id = 'Mouse',color = 'GenoInj', line.color = 'grey',
         facet.by = c('Genotype','Injection'))+ 
  labs(y = "Normalised Temprature change (%)")

ggplot(LPOnlyAnalysis_Summary_Norm, aes(x = TimePoint, y = NormalisedTemp, colour = GenoInj, group = GenoInj))+
  stat_summary(geom = "errorbar", fun.data = 'mean_se', width = 0.5,size = 1)+
  stat_summary(geom = "line", fun = 'mean',size = 1)+
  stat_summary(geom = "point", fun = 'mean',size = 4)+
  facet_grid(~ Injection)+
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Normalised Temprature change (%)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

## Activity ##
ggplot(LPOnlyAnalysis_Summary_Norm, aes(x = TimePoint, y = NormalisedAct, colour = GenoInj, group = Mouse))+
  geom_point()+
  geom_line()+
  facet_grid(Genotype ~ Injection)+ 
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Activity",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggplot(LPOnlyAnalysis_Summary_Norm, aes(x = TimePoint, y = Activity, colour = GenoInj, group = GenoInj))+
  stat_summary(geom = "errorbar", fun.data = 'mean_se', width = 0.5,size = 1)+
  stat_summary(geom = "line", fun = 'mean',size = 1)+
  stat_summary(geom = "point", fun = 'mean',size = 4)+
  facet_grid(~ Injection)+
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Activity",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

## Normalsing Per GenoInj instead of per mouse
# Create Baseline Temp from non-LP to normalise LP data
BaselineLPTemp_GenoInj <- LPOnlyAnalysis_Summary %>%
  filter(TimePoint == "PreLP") %>%
  group_by(GenoInj) %>%
  summarise(Temp = mean(Temp)) %>%
  rename(BaselineTemp = Temp)

LPOnlyAnalysis_Summary_Norm_GenoInj <- left_join(LPOnlyAnalysis_Summary,BaselineLPTemp_GenoInj, by = "GenoInj") %>%
  mutate(NormalisedTemp = ((Temp/BaselineTemp)*100)-100,
         TimePoint = factor(TimePoint, levels = c("PreLP","FirstHalf","SecondHalf"), labels = c("Pre-Light Pulse","First 30 min","Last 30 min")),
         GenoInj = factor(GenoInj, levels = c("WT MONO","WT PFF","Per1KO MONO","Per1KO PFF")),
         Genotype = factor(Genotype, levels = c("WT","Per1KO")))

ggplot(LPOnlyAnalysis_Summary_Norm_GenoInj, aes(x = TimePoint, y = NormalisedTemp, colour = GenoInj, group = Mouse))+
  geom_point()+
  geom_line()+
  facet_grid(Genotype ~ Injection)+ 
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Normalised Temprature change (%)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggpaired(data = LPOnlyAnalysis_Summary_Norm_GenoInj,
         x = 'TimePoint',y = 'NormalisedTemp',
         id = 'Mouse',color = 'GenoInj', line.color = 'grey',
         facet.by = c('Genotype','Injection'))+ 
  labs(y = "Normalised Temprature change (%)")

ggplot(LPOnlyAnalysis_Summary_Norm_GenoInj, aes(x = TimePoint, y = NormalisedTemp, colour = GenoInj, group = GenoInj))+
  stat_summary(geom = "errorbar", fun.data = 'mean_se', width = 0.5,size = 1)+
  stat_summary(geom = "line", fun = 'mean',size = 1)+
  stat_summary(geom = "point", fun = 'mean',size = 4)+
  facet_grid(~ Injection)+
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Normalised Temprature change (%)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')


#### Adding More Time Points ####

# Select out hours needed 
CombinedData_2 <- CombinedData %>%
  filter(Hour >= 13 & Hour <= 16) %>%
  mutate(TimePoint = case_when(HMS >= 13.5 & HMS < 14 ~ "PreLP",
                               HMS >= 14 & HMS < 14.5 ~ "FirstHalf",
                               HMS >= 14.5 & HMS < 15 ~ "SecondHalf",
                               HMS >= 15 & HMS < 15.5 ~ "PostLP1",
                               HMS >= 15.5 & HMS < 16 ~ "PostLP2"))

# Split Light pulse in 30 min bins pre, during first half second half
LPOnlyAnalysis <- CombinedData_2 %>%
  filter(HMS >= 13.5 & HMS < 16 & `Markers (X)[Coverage]` == 0 & Condition == "ZT14LP")  

# Average per group for analyisis
LPOnlyAnalysis_Summary <- LPOnlyAnalysis %>%
  group_by(Mouse,Genotype,Injection,GenoInj,TimePoint) %>%
  summarise(Temp = median(Temp,na.rm = T)) %>%
  ungroup()

# Create Baseline Temp from non-LP so normalise LP data
BaselineLPTemp <- LPOnlyAnalysis_Summary %>%
  filter(TimePoint == "PreLP") %>%
  select(Mouse,Temp) %>%
  rename(BaselineTemp = Temp)

LPOnlyAnalysis_Summary_Norm <- left_join(LPOnlyAnalysis_Summary,BaselineLPTemp, by = "Mouse") %>%
  mutate(NormalisedTemp = ((Temp/BaselineTemp)*100)-100,
         TimePoint = factor(TimePoint, levels = c("PreLP","FirstHalf","SecondHalf","PostLP1","PostLP2"), labels = c("Pre-Light Pulse","First 30 min","Last 30 min","Post LP 1","Post LP 2")),
         GenoInj = factor(GenoInj, levels = c("WT MONO","WT PFF","Per1KO MONO","Per1KO PFF")),
         Genotype = factor(Genotype, levels = c("WT","Per1KO")))

# Plot data comparison #
PFFcolourscheme <- c("royalblue3","turquoise3","firebrick2","darkorchid2")

ggplot(LPOnlyAnalysis_Summary_Norm, aes(x = TimePoint, y = Temp, colour = GenoInj, group = Mouse))+
  geom_point()+
  geom_line()+
  facet_grid(Genotype ~ Injection)+ 
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Temprature (C)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggpaired(data = LPOnlyAnalysis_Summary_Norm,
         x = 'TimePoint',y = 'Temp',
         id = 'Mouse',color = 'GenoInj', line.color = 'grey',
         facet.by = c('Genotype','Injection'))+ 
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Temprature (C)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggplot(LPOnlyAnalysis_Summary_Norm, aes(x = TimePoint, y = Temp, colour = GenoInj, group = GenoInj))+
  stat_summary(geom = "errorbar", fun.data = 'mean_se', width = 0.5,size = 1)+
  stat_summary(geom = "line", fun = 'mean',size = 1)+
  stat_summary(geom = "point", fun = 'mean',size = 4)+
  facet_grid(~ Genotype)+
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Temprature (C)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggplot(LPOnlyAnalysis_Summary_Norm, aes(x = TimePoint, y = Temp, colour = GenoInj, group = GenoInj))+
  stat_summary(geom = "errorbar", fun.data = 'mean_se', width = 0.5,size = 1)+
  stat_summary(geom = "line", fun = 'mean',size = 1)+
  stat_summary(geom = "point", fun = 'mean',size = 4)+
  facet_grid(~ Injection)+
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Temprature (C)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggplot(LPOnlyAnalysis_Summary_Norm, aes(x = TimePoint, y = NormalisedTemp, colour = GenoInj, group = Mouse))+
  geom_point()+
  geom_line()+
  facet_grid(Genotype ~ Injection)+ 
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Normalised Temprature change (%)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

ggpaired(data = LPOnlyAnalysis_Summary_Norm,
         x = 'TimePoint',y = 'NormalisedTemp',
         id = 'Mouse',color = 'GenoInj', line.color = 'grey',
         facet.by = c('Genotype','Injection'))+ 
  labs(y = "Normalised Temprature change (%)")

ggplot(LPOnlyAnalysis_Summary_Norm, aes(x = TimePoint, y = NormalisedTemp, colour = GenoInj, group = GenoInj))+
  stat_summary(geom = "errorbar", fun.data = 'mean_se', width = 0.5,size = 1)+
  stat_summary(geom = "line", fun = 'mean',size = 1)+
  stat_summary(geom = "point", fun = 'mean',size = 4)+
  facet_grid(~ Injection)+
  scale_color_manual(values = PFFcolourscheme)+
  labs(y = "Normalised Temprature change (%)",
       colour = "Genotype + Injection")+
  theme_stata(scheme = 's1mono')+
  theme(legend.position = 'bottom')

#### Minimum point analysis ####
# Split Light pulse in 30 min bins pre, during first half second half
LPMinAnalysis <- CombinedData_2 %>%
  filter(HMS >= 13.5 & HMS < 16 & `Markers (X)[Coverage]` == 0 & Condition == "ZT14LP")

LPMinTemp <- LPMinAnalysis %>%
  filter(HMS >= 14 & HMS < 16) %>%
  group_by(Mouse,Genotype,Injection,GenoInj) %>%
  filter(Temp == min(Temp))

BaselinePreLPTemp <- LPMinAnalysis %>%
  group_by(Mouse,Genotype,Injection,GenoInj,TimePoint) %>%
  summarise(Temp = median(Temp,na.rm = T)) %>%
  ungroup()%>%
  filter(TimePoint == "PreLP") %>%
  select(Mouse,Temp) %>%
  rename(BaselineTemp = Temp)

LPMinTemp_Norm <- left_join(LPMinTemp,BaselinePreLPTemp, by = c("Mouse")) %>%
  mutate(NormalisedTemp = ((Temp/BaselineTemp)*100)-100,
         TimeToMin = (HMS - 14)*60,
         GenoInj = factor(GenoInj, levels = c("WT MONO","WT PFF","Per1KO MONO","Per1KO PFF")),
         Genotype = factor(Genotype, levels = c("WT","Per1KO")))

ggplot(LPMinTemp_Norm, aes(x = GenoInj, y = NormalisedTemp, colour = GenoInj, group = GenoInj))+
  stat_summary(geom = "errorbar", fun.data = 'mean_se', width = 0.5,size = 1)+
  stat_summary(geom = "point", fun = 'mean',size = 5,shape =15)+
  geom_point(colour = 'black',size = 2)+
  scale_y_continuous(breaks = seq(-10,10,2))+
  scale_colour_manual(values = PFFcolourscheme)+
  labs(y = "Maximum reduction in body temp (%)",
       fill = "Genotype + Injection",
       x = "")+
  theme_stata(scheme = 's1mono',base_size = 15)+
  theme(legend.position = 'none',
        panel.grid.major.x =  element_line(size = 1),
        panel.grid.major.y =  element_blank(),
        axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))

ggplot(LPMinTemp_Norm, aes(x = GenoInj, y = TimeToMin, colour = GenoInj, group = GenoInj))+
  stat_summary(geom = "errorbar", fun.data = 'mean_se', width = 0.5,size = 1)+
  stat_summary(geom = "point", fun = 'mean',size = 5,shape =15)+
  geom_point(colour = 'black',size = 2)+
  scale_y_continuous(breaks = seq(0,120,10))+
  scale_x_discrete(limits = rev(levels(LPMinTemp_Norm$GenoInj)))+
  scale_colour_manual(values = PFFcolourscheme)+
  labs(y = "Time to minimum temp (min)",
       fill = "Genotype + Injection",
       x = "")+
  theme_stata(scheme = 's1mono',base_size = 15)+
  theme(legend.position = 'none',
        axis.text.y = element_text(angle = 0))+
  coord_flip()

### TestGroup ###
TestData1 <- CombinedData_2 %>%
    filter(Mouse == 449 & Condition == "ZT14LP")

ggplot(TestData1, aes(x = HMS, y = Temp))+
  geom_line()
