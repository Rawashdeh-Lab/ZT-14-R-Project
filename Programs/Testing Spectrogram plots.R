# LP Frequency #
# Baseline Data
BaselineData <- vroom('Raw Data/Experimental Group 3/ExperimentalGroupThree_4s_SD_HZ_Baseline.csv.gz') %>%
  mutate(Condition = "Baseline")

# ZT 14 LP Data
ZT14LPData <- vroom('Raw Data/Experimental Group 3/ExperimentalGroupThree_4s_ZT14LP_HZ.csv.gz')%>%
  mutate(Condition = "ZT14LP")

# Put them into one dataset and add hours
CombinedData <- rbind(BaselineData,ZT14LPData) %>%
  mutate(Hour = hour(DateTime))

# Select out hours needed 
CombinedData_2 <- CombinedData %>%
  filter(Hour >= 13 & Hour <= 16)

D1 <- CombinedData_2 %>%
  filter(Stage != "Artifact") %>%
  select(DateTime,EpochNo,Stage,`0.488281Hz`:`99.609375Hz`,Mouse,Genotype,Injection,GenoInj,FileRed,Hour,Condition)%>%
  gather(key = 'HZ', value = 'Level', -Stage,-DateTime, -Mouse, -Genotype, -Injection, -GenoInj,-EpochNo, -FileRed,-Hour,-Condition)  %>%
  mutate(Hz2 = as.numeric(gsub("Hz","", HZ))) %>%
  filter(Hz2 >= 0.5 & Hz2 <= 100) %>%
  ungroup() 


D2 <- D1 %>%
  group_by(Mouse,EpochNo,FileRed,Condition,DateTime) %>%
  mutate(Percent = (Level/sum(Level))*100) %>%
  ungroup()

D3 <- D2 %>%
  group_by(DateTime,Stage,Mouse,Genotype,Injection,GenoInj, Hz2,Condition) %>%
  summarise(Level = mean(Level),
            Percent = mean(Percent,na.rm = T)) %>%
  mutate(Stage2 = factor(Stage,levels = c("Wake","SWS","Paradoxical"),labels = c('Wake',"NREM",'REM'))) %>%
  ungroup()

Mouse835 <- filter(D3, Mouse == 835)%>%
  mutate(Hour = hour(DateTime),
         HMS = hour(DateTime)+(minute(DateTime)/60)+(second(DateTime)/3600))

Mouse835Test <- Mouse835 %>%
  filter(HMS <= 15)

ggplot(Mouse835Test,aes(x = HMS, y = Hz2,fill=Percent))+
  geom_tile() + 
  scale_fill_gradientn(colours = rev(rainbow(5,end = 0.7)),na.value = "red",limit = c(0,10)) +
  facet_grid(Condition~.)

# Example #
ggplot(BaseSpectrogramDataGenoAdj3, aes(x=ZT_Hour, y=Hz2, fill = AvPer) ) +
  geom_tile()+
  facet_grid(Stage~Day, margins = F)+
  scale_fill_viridis(option = "B",trans = 'log10',) +
  scale_x_continuous(breaks = seq(0,23.9,4))+
  coord_cartesian(expand = 0)+
  theme_classic()+
  geom_vline(xintercept = 0)+
  theme(panel.spacing.x = unit(0,'lines'))


D1_2 <- CombinedData_2 %>%
  filter(Stage != "Artifact") %>%
  select(DateTime,EpochNo,Stage,`0.488281Hz`:`99.609375Hz`,Mouse,Genotype,Injection,GenoInj,FileRed,Hour,Condition)%>%
  gather(key = 'HZ', value = 'Level', -Stage,-DateTime, -Mouse, -Genotype, -Injection, -GenoInj,-EpochNo, -FileRed,-Hour,-Condition)  %>%
  mutate(Hz2 = as.numeric(gsub("Hz","", HZ))) %>%
  filter(Hz2 >= 0.5 & Hz2 <= 30) %>%
  ungroup() 

D2_2 <- D1_2 %>%
  group_by(Mouse,EpochNo,FileRed,Condition,DateTime) %>%
  mutate(Percent = (Level/sum(Level))*100) %>%
  ungroup()

D3_2 <- D2_2 %>%
  group_by(DateTime,Stage,Mouse,Genotype,Injection,GenoInj, Hz2,Condition) %>%
  summarise(Level = mean(Level),
            Percent = mean(Percent,na.rm = T)) %>%
  mutate(Stage2 = factor(Stage,levels = c("Wake","SWS","Paradoxical"),labels = c('Wake',"NREM",'REM'))) %>%
  ungroup()

Mouse835_2 <- filter(D3_2, Mouse == 835)%>%
  mutate(Hour = hour(DateTime),
         HMS = hour(DateTime)+(minute(DateTime)/60)+(second(DateTime)/3600))

Mouse835Test_2 <- Mouse835_2 %>%
  filter(HMS >= 13.8 & HMS <= 14.2 & Condition == "Baseline")

ggplot(Mouse835Test_2,aes(x = HMS, y = Hz2,z=Percent))+
  geom_contour_filled()

