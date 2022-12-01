# How much sleep occured during the LP compared to ZT 13 - 15 baseline #
# Baseline Data
BaselineDataG3 <- vroom('Raw Data/Experimental Group 3/ExperimentalGroupThree_4s_SD_Baseline_SleepStates.csv.gz') %>%
  mutate(Condition = "Baseline",ZTDateTime = DateTime, ExGrp = "ExGrp3")

BaselineDataGA <- vroom('Raw Data/Experimental Group A/ExperimentalGroupA_SD_Baseline_SleepStates_4s.csv.gz') %>%
  mutate(Condition = "Baseline",ZTDateTime = DateTime, ExGrp = "ExGrpA")

BaselineDataGB <- vroom('Raw Data/Experimental Group B/ExperimentalGroupB_SD_Baseline_SleepStates_4s.csv.gz') %>%
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
ZT14LPG3 <- vroom('Raw Data/Experimental Group 3/ExperimentalGroupThree_4s_ZT14LP_SleepStates.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime, ExGrp = "ExGrp3")

ZT14LPGA <- vroom('Raw Data/Experimental Group A/ExperimentalGroupA_ZT14LP_SleepStates_4s.csv.gz') %>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime, ExGrp = "ExGrpA")

ZT14LPGB <- vroom('Raw Data/Experimental Group B/ExperimentalGroupB_ZT14LP_SleepStates_4s.csv.gz') %>%
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
                               T ~ Injection),
         HMS = hour(ZTDateTime)+minute(ZTDateTime)/60+second(ZTDateTime)/3600)
View(unique(CombinedData[c("Mouse","GenoInj", "Condition")]) %>%
       arrange(GenoInj,Mouse))

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
         GenoInj = factor(GenoInj,levels = c("WT SAL","WT PFF","WT OHDA","Per1KO SAL","Per1KO PFF","Per1KO OHDA")))

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
  arrange(AllIDs,`Rodent Sleep`,Hour)%>%
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

write.table(VigilanceDataFinalTable,'clipboard',sep="\t",row.names = F)

VigilanceDataFinalTable2 <- VigilanceDataComparativeTable %>%
  select(AllIDs,Hour,`Rodent Sleep`,ZT14LP) %>%
  spread(key = AllIDs, value = ZT14LP) %>%
  arrange(desc(`Rodent Sleep`),Hour)%>%
  select(Hour,`Rodent Sleep`,c(cols))

write.table(VigilanceDataFinalTable2,'clipboard',sep="\t",row.names = F)

##### Time till first 1 min sleep bout ####

ZT14LPG3 <- vroom('Raw Data/Experimental Group 3/ExperimentalGroupThree_4s_ZT14LP_SleepStates.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime, ExGrp = "ExGrp3")

ZT14LPGA <- vroom('Raw Data/Experimental Group A/ExperimentalGroupA_ZT14LP_SleepStates_4s.csv.gz') %>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime, ExGrp = "ExGrpA")

ZT14LPGB <- vroom('Raw Data/Experimental Group B/ExperimentalGroupB_ZT14LP_SleepStates_4s.csv.gz') %>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime-21600, ExGrp = "ExGrpB")

ZT14LPData <- rbind(ZT14LPG3,ZT14LPGA,ZT14LPGB) %>%
  mutate(GenoInj = case_when(GenoInj == "WT MONO" ~ "WT SAL",
                             GenoInj == "PerKO MONO" ~ "Per1KO MONO",
                             GenoInj == "PerKO OHDA" ~ "Per1KO OHDA",
                             GenoInj == "Per1KO MONO" ~ "Per1KO SAL",
                             GenoInj == "PerKO PFF" ~ "Per1KO PFF",
                             T ~ GenoInj))

ZT14LPDataStart <- ZT14LPData %>%
  mutate(HMS = hour(ZTDateTime) + (minute(ZTDateTime)/60) + (second(ZTDateTime)/3600),
         Injection = case_when(Injection == "MONO" ~ "SAL",
                               T ~ Injection),
         TotalSleep = case_when(`Rodent Sleep` == "X" ~ "W", T ~ `Rodent Sleep`)) %>%
  filter(HMS >= 14 & TotalSleep == "W") %>% 
  arrange(Mouse,HMS)

ZT14LPDataStart$tdiff <- unlist(tapply(ZT14LPDataStart$ZTDateTime, INDEX = ZT14LPDataStart$Mouse,
                                       FUN = function(x) c(0, `units<-`(diff(x), "secs")))) 

ZT14LPDataNonWake <- ZT14LPDataStart %>%
  filter(tdiff >= 60) %>%
  mutate(StartDateTime = ZTDateTime - tdiff) %>%
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
  dplyr::filter(HMS >= 14 & HMS <= 15)

ggplot(PlottingData[which(PlottingData$Mouse == "842" & PlottingData$Condition == "ZT14LP"),], aes(x = NormDateTime, y = SleepNum))+
  geom_step(size = 0.5)+
  geom_point(aes(colour = as.factor(SleepNum)),size = 1,shape = 15)+
  scale_colour_manual(values = c("blue","red","green"))+
  coord_cartesian(ylim = c(-0.5,2.5))+
  theme_article()

WholeMouseEEG <- readEdfHeader(fileName = "EEG Recording/WT MONO 842 ZT14LP EEG.edf")
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


