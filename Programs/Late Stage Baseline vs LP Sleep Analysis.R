# How much sleep occured during the LP compared to ZT 13 - 15 baseline #
# Baseline Data
BaselineDataG2 <- vroom('Mid Data/Baseline/ExperimentalGroupTwo_4s_SD_Baseline_SleepStates.csv.gz')%>%
  mutate(Condition = "Baseline", ZTDateTime = DateTime) 

BaselineDataG5 <- vroom('Raw Data/Experimental Group 5/ExperimentalGroupFive_4s_SD_Baseline_SleepStates.csv.gz') %>%
  mutate(Condition = "Baseline", ZTDateTime = DateTime)

BaselineDataG7 <- vroom('Raw Data/Experimental Group 7/ExperimentalGroupSeven_4s_Baseline_SleepStates.csv.gz') %>%
  mutate(Condition = "Baseline") %>% dplyr::filter(Day == 0) %>%
  filter(Mouse != 681)

# Pool Data Together #
BaselineData <- rbind(BaselineDataG2,BaselineDataG5,BaselineDataG7) %>%
  arrange(desc(Genotype),Injection,Mouse,ZTDateTime) %>%
  mutate(Genotype = case_when(Genotype == "PerKO" ~ "Per1KO",
                              T ~ Genotype),
         GenoInj = case_when(GenoInj == "PerKO MONO" ~ "Per1KO MONO",
                             GenoInj == "PerKO PFF" ~ "Per1KO PFF",
                             T ~ GenoInj))


# ZT 14 LP Data
ExG2ZT14LP1Data <- vroom('Mid Data/ZT14 LP 1 4s/ExperimentalGroupTwo_ZT14LP_1_SleepStates.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime) %>%
  filter(Mouse %in% c(737,749,750,449,442,446,447,448))

# ZT 14 LP2 Data
ExG2ZT14LP2Data <- vroom('Mid Data/ZT14 LP 2 4s/ExperimentalGroupTwo_ZT14LP_2_SleepStates.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime)%>%
  filter(Mouse %in% c(738,751,450))

ExGrp5LPPower <- vroom('Raw Data/Experimental Group 5/ExperimentalGroupFive_4s_LP_SleepStates.csv.gz') %>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime)

ExGrp7LPPower <- vroom('Raw Data/Experimental Group 7/ExperimentalGroupSeven_G1_4s_LP_SleepStates.csv.gz') %>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime)

ZT14LPData <- rbind(ExG2ZT14LP1Data,ExG2ZT14LP2Data,ExGrp5LPPower,ExGrp7LPPower)

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

# Hourly Vigilance state calculation # 
VigilanceData1 <- CombinedData_2 %>%
  mutate(`Rodent Sleep` = case_when(`Rodent Sleep` == "X" ~ "W",T ~ `Rodent Sleep`),
         TotalSleepState = case_when(`Rodent Sleep` == "P" ~ "S", T~`Rodent Sleep`),
         AllIDs = paste(Genotype,Injection,Mouse,sep = " "),
         GenoInj = factor(GenoInj,levels = c("WT MONO","WT PFF","Per1KO MONO","Per1KO PFF")))

VigilanceData <- VigilanceData1 %>%
  group_by(AllIDs,Hour,Condition,`Rodent Sleep`) %>%
  summarise(NumberofBouts = n()) %>%
  ungroup() %>%
  complete(Hour = 13:16,
           `Rodent Sleep` = c("W","P","S"),
           Condition = c("Baseline","ZT14LP"),
           nesting(AllIDs),
           fill = list(NumberofBouts = 0)) %>%
  arrange(AllIDs,`Rodent Sleep`,Condition,Hour) %>%
  mutate(SecondsofState = NumberofBouts*4,
         MinutesofState = SecondsofState/60)

# Test table # Ignore
VigilanceDataTable <- VigilanceData %>%
  select(AllIDs,Hour,MinutesofState,`Rodent Sleep`,Condition) %>%
  spread(key = AllIDs, value = MinutesofState) %>%
  arrange(`Rodent Sleep`,Condition,Hour)

# Make into a nice table #
VigilanceDataComparativeTable <- VigilanceData %>%
  select(AllIDs,Hour,MinutesofState,`Rodent Sleep`,Condition) %>%
  spread(key = Condition, value = MinutesofState) %>%
  mutate(Change = ZT14LP - Baseline)%>%
  arrange(AllIDs,`Rodent Sleep`,Hour)

cols <- unique(VigilanceData1$AllIDs)

VigilanceDataFinalTable <- VigilanceDataComparativeTable %>%
  select(AllIDs,Hour,`Rodent Sleep`,Change) %>%
  spread(key = AllIDs, value = Change) %>%
  arrange(desc(`Rodent Sleep`),Hour)%>%
  select(Hour,`Rodent Sleep`,c(cols))

write.table(VigilanceDataFinalTable,'clipboard',sep="\t",row.names = F)

VigilanceDataFinalTable2 <- VigilanceDataComparativeTable %>%
  select(AllIDs,Hour,`Rodent Sleep`,Baseline) %>%
  spread(key = AllIDs, value = Baseline) %>%
  arrange(desc(`Rodent Sleep`),Hour)%>%
  select(Hour,`Rodent Sleep`,c(cols))

write.table(VigilanceDataFinalTable2,'clipboard',sep="\t",row.names = F)

##### Time till first 1 min sleep bout ####
# ZT 14 LP Data
ExG2ZT14LP1Data <- vroom('Mid Data/ZT14 LP 1 4s/ExperimentalGroupTwo_ZT14LP_1_SleepStates.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime) %>%
  filter(Mouse %in% c(737,749,750,449,442,446,447,448))

# ZT 14 LP2 Data
ExG2ZT14LP2Data <- vroom('Mid Data/ZT14 LP 2 4s/ExperimentalGroupTwo_ZT14LP_2_SleepStates.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime)%>%
  filter(Mouse %in% c(738,751,450))

ExGrp5LPPower <- vroom('Raw Data/Experimental Group 5/ExperimentalGroupFive_4s_LP_SleepStates.csv.gz') %>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime)

ExGrp7LPPower <- vroom('Raw Data/Experimental Group 7/ExperimentalGroupSeven_G1_4s_LP_SleepStates.csv.gz') %>%
  mutate(Condition = "ZT14LP")

ZT14LPData <- rbind(ExG2ZT14LP1Data,ExG2ZT14LP2Data,ExGrp5LPPower,ExGrp7LPPower)

ZT14LPDataStart <- ZT14LPData %>%
  mutate(HMS = hour(DateTime) + (minute(DateTime)/60) + (second(DateTime)/3600),
         TotalSleep = case_when(`Rodent Sleep` == "X" ~ "W", T ~ `Rodent Sleep`)) %>%
  filter(HMS >= 14 & TotalSleep == "W") %>% 
  arrange(Mouse,HMS)

ZT14LPDataStart$tdiff <- unlist(tapply(ZT14LPDataStart$DateTime, INDEX = ZT14LPDataStart$Mouse,
                                       FUN = function(x) c(0, `units<-`(diff(x), "secs")))) 

ZT14LPDataNonWake <- ZT14LPDataStart %>%
  filter(tdiff >= 60) %>%
  mutate(StartDateTime = DateTime - tdiff) %>%
  ungroup()

# First Instance #
FistSleepBout <- ZT14LPDataNonWake %>% 
  group_by(Mouse) %>% 
  slice(which.min(StartDateTime)) %>%
  ungroup()

TimeSinceLightsON <- FistSleepBout %>%
  mutate(TimeSince = (HMS - 14)*60,
         AllIDs = paste(Genotype,Injection,Mouse,sep = " ")) %>%
  arrange(desc(Genotype),Injection,Mouse) %>%
  select(AllIDs, TimeSince)

write.table(t(TimeSinceLightsON),'clipboard',sep="\t",row.names = F)


# NREM Delta Power of ZT 14 LP #
# Baseline Data
BaselineDeltaData <- vroom('Mid Data/Baseline/ExperimentalGroupTwo_4s__SD_RawPowerBands_Baseline.csv.gz')%>%
  mutate(Condition = "Baseline") 

BaselinePerMouse <- BaselineDeltaData %>%
  mutate(Hour = hour(DateTime),
         AllIDs = paste(Genotype,Injection,Mouse,sep = " ")) %>%
  filter(Day == 0 & Hour >=0 & Hour <=12 & `Rodent Sleep` == "S") %>%
  group_by(AllIDs) %>%
  summarise(BaselineDeltaPower = mean(`EEG (Delta (0.5-4))`))

ZT14LP1DeltaData <- vroom('Mid Data/ZT14 LP 1 4s/ExperimentalGroupTwo_ZT14LP_1_RawPowerBands.csv.gz')%>%
  mutate(Condition = "ZT14LP") %>%
  filter(Mouse %in% c(737,749,750,449,442,446,447,448))

ZT14LP2DeltaData <- vroom('Mid Data/ZT14 LP 2 4s/ExperimentalGroupTwo_ZT14LP_2_RawPowerBands.csv.gz')%>%
  mutate(Condition = "ZT14LP")%>%
  filter(Mouse %in% c(738,751,450))

ZT14LPDeltaData <- rbind(ZT14LP1DeltaData,ZT14LP2DeltaData)

# ZT 14LP NREM Delta
ZT14LPDelta <- ZT14LPDeltaData %>%
  mutate(Hour = hour(DateTime),
         AllIDs = paste(Genotype,Injection,Mouse,sep = " ")) %>%
  filter(Hour == 14 & `Rodent Sleep` == "S") %>%
  group_by(AllIDs) %>%
  summarise(DeltaPower = mean(`EEG (Delta (0.5-4))`)) %>%
  ungroup() 

DeltaComparison <- left_join(BaselinePerMouse,ZT14LPDelta,by = "AllIDs") %>%
  mutate(Change = DeltaPower/BaselineDeltaPower)

DeltaComparisonTable <- DeltaComparison %>%
  select(AllIDs,Change)

write.table(t(DeltaComparisonTable),'clipboard',sep="\t",row.names = F)
