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
  dplyr::filter(Hour ==14)

# Hourly Vigilance state calculation # 
VigilanceData1 <- CombinedData_2 %>%
  mutate(`Rodent Sleep` = case_when(`Rodent Sleep` == "X" ~ "W",T ~ `Rodent Sleep`),
         TotalSleepState = case_when(`Rodent Sleep` == "P" ~ "S", T~`Rodent Sleep`),
         AllIDs = paste(Genotype,Injection,Mouse,sep = " "),
         GenoInj = factor(GenoInj,levels = c("WT MONO","WT PFF","Per1KO MONO","Per1KO PFF")))

VigilanceData <- VigilanceData1 %>%
  group_by(AllIDs,Hour,Condition,`Rodent Sleep`,ExGrp) %>%
  summarise(NumberofBouts = n()) %>%
  ungroup() %>%
  complete(Hour = 14,
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

VigilanceDataComparativeTable <- VigilanceData %>%
  select(AllIDs,Hour,MinutesofState,`Rodent Sleep`,Condition,ExGrp) %>%
  group_by(AllIDs,Hour,Condition) %>%
  mutate(MinutesOfStateSum = sum(MinutesofState)) %>%
  ungroup()

# Make into a nice table #
VigilanceDataComparativeTable <- VigilanceData %>%
  select(AllIDs,Hour,MinutesofState,`Rodent Sleep`,Condition) %>%
  spread(key = Condition, value = MinutesofState) %>%
  mutate(Change = ZT14LP - Baseline)%>%
  arrange(AllIDs,`Rodent Sleep`,Hour) %>%
  group_by(AllIDs,Hour) %>%
  mutate(BaselineSum = sum(Baseline),
         ZT14Sum = sum(ZT14LP)) %>%
  ungroup()

cols <- unique(VigilanceData1$AllIDs)

VigilanceDataFinalTable <- VigilanceDataComparativeTable %>%
  select(AllIDs,Hour,`Rodent Sleep`,Change) %>%
  spread(key = AllIDs, value = Change) %>%
  arrange(desc(`Rodent Sleep`),Hour)%>%
  select(Hour,`Rodent Sleep`,c(cols))

write_clip(VigilanceDataFinalTable)

VigilanceDataFinalTable2 <- VigilanceDataComparativeTable %>%
  select(AllIDs,Hour,`Rodent Sleep`,Baseline) %>%
  spread(key = AllIDs, value = Baseline) %>%
  arrange(desc(`Rodent Sleep`),Hour)%>%
  select(Hour,`Rodent Sleep`,c(cols))

write_clip(VigilanceDataFinalTable2)

VigilanceDataFinalTable_ZT <- VigilanceDataComparativeTable %>%
  select(AllIDs,Hour,`Rodent Sleep`,ZT14LP) %>%
  spread(key = AllIDs, value = ZT14LP) %>%
  arrange(desc(`Rodent Sleep`),Hour)%>%
  select(Hour,`Rodent Sleep`,c(cols))

write_clip(VigilanceDataFinalTable_ZT)

##### Time till first 1 min sleep bout ####
# ZT 14 LP Data
ExG2ZT14LP1Data <- vroom('Mid Data/ZT14 LP 1 4s/ExperimentalGroupTwo_ZT14LP_1_SleepStates.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime) %>%
  dplyr::filter(Mouse %in% c(737,749,750,449,442,446,447,448))

# ZT 14 LP2 Data
ExG2ZT14LP2Data <- vroom('Mid Data/ZT14 LP 2 4s/ExperimentalGroupTwo_ZT14LP_2_SleepStates.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime)%>%
  dplyr::filter(Mouse %in% c(738,751,450))

ExGrp5LPPower <- vroom('Raw Data/Experimental Group 5/ExperimentalGroupFive_4s_LP_SleepStates.csv.gz') %>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime)

ExGrp7LPPower <- vroom('Raw Data/Experimental Group 7/ExperimentalGroupSeven_G1_4s_LP_SleepStates.csv.gz') %>%
  mutate(Condition = "ZT14LP")

# ExGrp9LPPower <- vroom('Raw Data/Experimental Group 9/ExperimentalGroupNine_4s_Baseline_SleepStates.csv.gz') %>%
#   dplyr::filter(DateTime >= "2022-04-02 18:00:00 UTC" & DateTime < "2022-04-02 23:00:00 UTC") %>%
#   mutate(Day = 1, Condition = "ZT14LP", ExGrp = "ExGrp9")

ExGrp11LPPower <- vroom('Raw Data/Experimental Group 11/ExperimentalGroupEleven_4s_MonoZT14LP_SleepStates.csv.gz') %>%
  dplyr::filter(DateTime >= "2022-06-05 18:00:00 UTC" & DateTime < "2022-06-05 23:00:00 UTC") %>%
  mutate(Day = 1,Condition = "ZT14LP", ExGrp = "ExGrp11")

ZT14LPData <- rbind(ExG2ZT14LP1Data,ExG2ZT14LP2Data,ExGrp5LPPower,ExGrp7LPPower,ExGrp11LPPower)

ZT14LPDataStart <- ZT14LPData %>%
  mutate(HMS = hour(DateTime) + (minute(DateTime)/60) + (second(DateTime)/3600),
         TotalSleep = case_when(`Rodent Sleep` == "X" ~ "W", T ~ `Rodent Sleep`)) %>%
  dplyr::filter(HMS >= 14 & TotalSleep == "W") %>% 
  arrange(Mouse,HMS)

ZT14LPDataStart$tdiff <- unlist(tapply(ZT14LPDataStart$DateTime, INDEX = ZT14LPDataStart$Mouse,
                                       FUN = function(x) c(0, `units<-`(diff(x), "secs")))) 

ZT14LPDataNonWake <- ZT14LPDataStart %>%
  dplyr::filter(tdiff >= 60) %>%
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

write_clip((TimeSinceLightsON))


# NREM Delta Power of ZT 14 LP #
# Baseline Data
BaselineDeltaData <- vroom('Mid Data/Baseline/ExperimentalGroupTwo_4s__SD_RawPowerBands_Baseline.csv.gz')%>%
  mutate(Condition = "Baseline") 

BaselinePerMouse <- BaselineDeltaData %>%
  mutate(Hour = hour(DateTime),
         AllIDs = paste(Genotype,Injection,Mouse,sep = " ")) %>%
  dplyr::filter(Day == 0 & Hour >=0 & Hour <=12 & `Rodent Sleep` == "S") %>%
  group_by(AllIDs) %>%
  summarise(BaselineDeltaPower = mean(`EEG (Delta (0.5-4))`))

ZT14LP1DeltaData <- vroom('Mid Data/ZT14 LP 1 4s/ExperimentalGroupTwo_ZT14LP_1_RawPowerBands.csv.gz')%>%
  mutate(Condition = "ZT14LP") %>%
  dplyr::filter(Mouse %in% c(737,749,750,449,442,446,447,448))

ZT14LP2DeltaData <- vroom('Mid Data/ZT14 LP 2 4s/ExperimentalGroupTwo_ZT14LP_2_RawPowerBands.csv.gz')%>%
  mutate(Condition = "ZT14LP")%>%
  dplyr::filter(Mouse %in% c(738,751,450))

ZT14LPDeltaData <- rbind(ZT14LP1DeltaData,ZT14LP2DeltaData)

# ZT 14LP NREM Delta
ZT14LPDelta <- ZT14LPDeltaData %>%
  mutate(Hour = hour(DateTime),
         AllIDs = paste(Genotype,Injection,Mouse,sep = " ")) %>%
  dplyr::filter(Hour == 14 & `Rodent Sleep` == "S") %>%
  group_by(AllIDs) %>%
  summarise(DeltaPower = mean(`EEG (Delta (0.5-4))`)) %>%
  ungroup() 

DeltaComparison <- left_join(BaselinePerMouse,ZT14LPDelta,by = "AllIDs") %>%
  mutate(Change = DeltaPower/BaselineDeltaPower)

DeltaComparisonTable <- DeltaComparison %>%
  select(AllIDs,Change)

write.table(t(DeltaComparisonTable),'clipboard',sep="\t",row.names = F)

# Hypnogram # Run multitaper_spectrogram_R
library(vroom)
library(data.table)
library(tidyverse)
library(edfReader)
library(GENEAread)
library(signal)
library(FRAPO)
library(lubridate)

PlottingData <- VigilanceData1 %>%
  mutate(SleepNum = case_when(`Rodent Sleep` == "W" ~ 2,
                              `Rodent Sleep` == "S" ~ 0,
                              `Rodent Sleep` == "P" ~ 1),
         NormDateTime = as.POSIXct("2020-04-07 00:00:00")+(HMS*3600)) %>%
  dplyr::filter(HMS >= 13 & HMS <= 15)

ggplot(PlottingData[which(PlottingData$Mouse == "442"),], aes(x = NormDateTime, y = SleepNum))+
  geom_step(size = 0.2)+
  facet_grid(Condition~.)+
  geom_point(aes(colour = as.factor(SleepNum)),size = 1,shape = 15)+
  scale_colour_manual(values = c("blue","red","green"))+
  coord_cartesian(ylim = c(-0.5,2.5))+
  theme_article()

PlottingDataLight<- PlottingData %>%
  dplyr::filter(HMS >= 14 & HMS <15)

ggplot(PlottingDataLight[which(PlottingDataLight$Mouse == "442"),], aes(x = HMS, y = SleepNum))+
  geom_step(size = 0.3)+
  facet_grid(Condition~.)+
  geom_point(aes(colour = as.factor(SleepNum)),size = 0.5,shape = 15)+
  scale_colour_manual(values = c("blue","red","green"))+
  coord_cartesian(ylim = c(-0.5,2.5))+
  theme_article()

WholeMouseEEG <- readEdfHeader(fileName = "EEG Recording/WT MONO 1027 ZT14LP EEG.edf")
WholeMouseEEGDat <- readEdfSignals(WholeMouseEEG) 
WholeMouseEEGSignalDat <- WholeMouseEEGDat[["EEG"]][["signal"]]
WholeMouseEMGSignalDat <- WholeMouseEEGDat[["EMG"]][["signal"]]

Resu<- multitaper_spectrogram_R(data = WholeMouseEEGSignalDat,fs = 500,frequency_range = c(0,25))

TaperPlot <- image.plot(x=Resu[[2]], y=Resu[[3]], nanpow2db(Resu[[1]]), xlab="Time (s)", ylab='Frequency (Hz)')

EEG <- as.data.frame(WholeMouseEEGSignalDat) 
EEG2 <- rownames_to_column(EEG,var = "Time") %>%
  mutate(Time = as.numeric(Time))

EEGPlot <- ggplot(EEG2, aes(x = Time,y=WholeMouseEEGSignalDat))+
  geom_line()+
  theme_void()

EMG <- as.data.frame(WholeMouseEMGSignalDat) 
EMG2 <- rownames_to_column(EEG,var = "Time") %>%
  mutate(Time = as.numeric(Time))

EMGPlot <- ggplot(EMG2, aes(x = Time,y=WholeMouseEMGSignalDat))+
  geom_line()+
  theme_void()
