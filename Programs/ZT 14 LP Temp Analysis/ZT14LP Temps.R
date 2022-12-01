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

ZT14LPData2 <- ZT14LPData%>%
  mutate(Hour = hour(ZTDateTime))%>%
  filter(Hour >= 13 & Hour <= 16)

# Put them into one dataset and add hours
CombinedData <- rbind(BaselineData,ZT14LPData) %>%
  mutate(Hour = hour(ZTDateTime))

# Select out hours needed 
CombinedData_2 <- CombinedData %>%
  filter(Hour >= 13 & Hour <= 16)

# Check Data #
View(CombinedData_2 %>% 
       group_by(Condition,Mouse,Hour) %>% 
       slice(which.min(DateTime)))

ggplot(CombinedData_2[which(CombinedData_2$`Markers (X)[Coverage]` == 0),],aes(x = HMS, y = Temp,colour = Condition))+
  geom_vline(xintercept = c(14,15),linetype = 'dashed')+
  stat_summary(geom = 'line', fun = 'mean')+
  stat_summary(geom = 'errorbar', fun.data = 'mean_se')+
  scale_colour_manual(values = c('black','red'))+
  theme_light()+
  facet_grid(GenoInj~.)

ggplot(CombinedData_2[which(CombinedData_2$`Markers (X)[Coverage]` == 0 & CombinedData_2$Genotype == "Per1KO"),],aes(x = HMS, y = Temp,colour = Condition,group = Condition))+
  geom_vline(xintercept = c(14,15),linetype = 'dashed')+
  scale_colour_manual(values = c('black','red'))+
  theme_light()+
  facet_grid(Mouse~.)

ggplot(ZT14LPData2[which(ZT14LPData2$`Markers (X)[Coverage]` == 0 & ZT14LPData2$Genotype == "Per1KO"),],aes(x = HMS, y = Temp,colour = as.factor(Mouse),group = Mouse))+
  geom_vline(xintercept = c(14,15),linetype = 'dashed')+
  geom_line(size =1)+
  theme_light()+
  facet_grid(GenoInj~.)

ggplot(ZT14LPData2[which(ZT14LPData2$`Markers (X)[Coverage]` == 0 & ZT14LPData2$Genotype == "WT"),],aes(x = HMS, y = Temp,colour = as.factor(Mouse),group = Mouse))+
  geom_vline(xintercept = c(14,15),linetype = 'dashed')+
  geom_line(size =1)+
  theme_light()+
  facet_grid(GenoInj~.)
