# How much sleep occured during the LP compared to ZT 13 - 15 baseline #
# Baseline Data
Sys.setenv(TZ='UTC')

BaselineDataG3 <- vroom('Raw Data/Experimental Group 3/ExperimentalGroupThree_4s_SD_Baseline_SleepStates.csv.gz') %>%
  mutate(Condition = "Baseline",ZTDateTime = DateTime, ExGrp = "ExGrp3")

BaselineDataG10 <- vroom('Raw Data/Experimental Group 10/ExperimentalGroupTen_4s_Baseline_SleepStates.csv.gz') %>%
  mutate(Condition = "Baseline",ZTDateTime = DateTime - 21600, ExGrp = "ExGrp10") %>%
  filter(DateTime >= "2022-01-16 06:00:00 UTC" & DateTime < "2022-01-17 06:00:00 UTC")

BaselineData <- rbind(BaselineDataG3,BaselineDataG10) %>%
  arrange(desc(Genotype),Injection,Mouse,ZTDateTime) 

# ZT 14 LP Data
ZT14LPG3 <- vroom('Raw Data/Experimental Group 3/ExperimentalGroupThree_4s_ZT14LP_SleepStates.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime, ExGrp = "ExGrp3")

ZT14LPG10 <- vroom('Raw Data/Experimental Group 10/ExperimentalGroupTen_4s_ZT14LP_SleepStates.csv.gz')%>%
  mutate(Condition = "ZT14LP",ZTDateTime = DateTime - 21600, ExGrp = "ExGrp10")

ZT14 <- rbind(ZT14LPG3,ZT14LPG10) %>%
  arrange(desc(Genotype),Injection,Mouse,ZTDateTime) 

# Put them into one dataset and add hours
CombinedData <- rbind(BaselineData,ZT14) %>%
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

write_clip(VigilanceDataFinalTable2)
