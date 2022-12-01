Sys.setenv(TZ='UTC')

# How much sleep occured during the LP compared to ZT 13 - 15 baseline #
# Baseline Data
BaselineDataG2 <- vroom('Mid Data/Baseline/ExperimentalGroupTwo_Baseline_4s_SleepStates.csv.gz')%>%
  mutate(Condition = "Baseline", ZTDateTime = DateTime-21600, ExGrp = "ExGrp2") 

BaselineDataG5 <- vroom('Raw Data/Experimental Group 5/ExperimentalGroupFive_4s_SleepStates.csv.gz') %>%
  mutate(Condition = "Baseline", ZTDateTime = DateTime-21600, ExGrp = "ExGrp5") %>%
  dplyr::filter(Day == 0)

BaselineDataG7 <- vroom('Raw Data/Experimental Group 7/ExperimentalGroupSeven_4s_Baseline_SleepStates.csv.gz') %>%
  mutate(Condition = "Baseline", ExGrp = "ExGrp7") %>% dplyr::filter(Day == 0) %>%
  dplyr::filter(Mouse != 681)

# BaselineDataG9 <- vroom('Raw Data/Experimental Group 9/ExperimentalGroupNine_4s_Baseline_SleepStates.csv.gz') %>%
#   dplyr::filter(DateTime >= "2022-04-01 06:00:00 UTC" & DateTime < "2022-04-02 06:00:00 UTC") %>%
#   mutate(Day = 0, Condition = "Baseline", ExGrp = "ExGrp9")

BaselineDataG11 <- vroom('Raw Data/Experimental Group 11/ExperimentalGroupEleven_4s_MonoBaseline_SleepStates.csv.gz') %>%
  dplyr::filter(DateTime >= "2022-06-04 06:00:00 UTC" & DateTime < "2022-06-05 06:00:00 UTC") %>%
  mutate(Condition = "Baseline", ExGrp = "ExGrp11", Day = 0) 

# Pool Data Together #
BaselineData <- rbind(BaselineDataG2,BaselineDataG5,BaselineDataG7,BaselineDataG11) %>%
  arrange(desc(Genotype),Injection,Mouse,ZTDateTime) %>%
  mutate(Genotype = case_when(Genotype == "PerKO" ~ "Per1KO",
                              T ~ Genotype),
         GenoInj = case_when(GenoInj == "PerKO MONO" ~ "Per1KO MONO",
                             GenoInj == "PerKO PFF" ~ "Per1KO PFF",
                             T ~ GenoInj))


# ZT 14 LP Data
ExG2ZT14LP1Data <- vroom('Mid Data/ZT14 LP 1 4s/ExperimentalGroupTwo_ZT14LP_1_SleepStates.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime, ExGrp = "ExGrp2") %>%
  dplyr::filter(Mouse %in% c(737,749,750,449,442,446,447,448))

# ZT 14 LP2 Data
ExG2ZT14LP2Data <- vroom('Mid Data/ZT14 LP 2 4s/ExperimentalGroupTwo_ZT14LP_2_SleepStates.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime, ExGrp = "ExGrp2")%>%
  dplyr::filter(Mouse %in% c(738,751,450))

ExGrp5LPPower <- vroom('Raw Data/Experimental Group 5/ExperimentalGroupFive_4s_LP_SleepStates.csv.gz') %>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime, ExGrp = "ExGrp5")

ExGrp7LPPower <- vroom('Raw Data/Experimental Group 7/ExperimentalGroupSeven_G1_4s_LP_SleepStates.csv.gz') %>%
  mutate(Condition = "ZT14LP", ExGrp = "ExGrp7")

# ExGrp9LPPower <- vroom('Raw Data/Experimental Group 9/ExperimentalGroupNine_4s_Baseline_SleepStates.csv.gz') %>%
#   dplyr::filter(DateTime >= "2022-04-02 18:00:00 UTC" & DateTime < "2022-04-02 23:00:00 UTC") %>%
#   mutate(Day = 1, Condition = "ZT14LP", ExGrp = "ExGrp9")

ExGrp11LPPower <- vroom('Raw Data/Experimental Group 11/ExperimentalGroupEleven_4s_MonoZT14LP_SleepStates.csv.gz') %>%
  dplyr::filter(DateTime >= "2022-06-05 18:00:00 UTC" & DateTime < "2022-06-05 23:00:00 UTC") %>%
  mutate(Day = 1,Condition = "ZT14LP", ExGrp = "ExGrp11")

ZT14LPData <- rbind(ExG2ZT14LP1Data,ExG2ZT14LP2Data,ExGrp5LPPower,ExGrp7LPPower,ExGrp11LPPower)

# Put them into one dataset and add hours
CombinedData <- rbind(BaselineData,ZT14LPData) %>%
  mutate(Hour = hour(ZTDateTime),
         HMS = hour(ZTDateTime)+minute(ZTDateTime)/60+second(ZTDateTime)/3600)

# Select out hours needed 
CombinedData_2 <- CombinedData %>%
  dplyr::filter(Hour >=13 & Hour <17)


# Check all mice #
(MiceLevels <- unique(CombinedData_2[c("Mouse","GenoInj")]))

BaselineDataEdit <- CombinedData_2 %>%
  mutate(Colours = factor(GenoInj,levels = c('WT MONO','WT PFF','Per1KO MONO','Per1KO PFF')),
         MouseNums = factor(Mouse, levels = c(MiceLevels$Mouse)),
         `Wake State` = factor(`Rodent Sleep`, levels = c('W','S','P','X'),labels = c('Wake','NREM','REM','Wake'))) %>%
  filter(Genotype == "Per1KO")

ggplot(BaselineDataEdit,mapping = aes(colour = `Wake State`))+
  geom_segment(aes(x=HMS, xend=HMS+0.00278, y="", yend="", fill = NULL), size=10) +
  scale_color_manual(values = c('gold','royalblue','firebrick2'))+
  geom_vline(xintercept = c(14,15),size = 1)+
  scale_x_continuous(breaks = seq(12,17,1))+
  labs(x = 'ZT (Hour)',
       y = NULL,
       fill = 'Genotype + Injection')+
  theme_minimal(base_family = 'serif',base_size = 15)+
  facet_grid(Injection*MouseNums~Condition)+
  theme(strip.text.y.right = element_text(angle = 0))
