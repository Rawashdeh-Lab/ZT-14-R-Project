# How much sleep occured during the LP compared to ZT 13 - 15 baseline #
# Baseline Data
BaselineData <- vroom('Raw Data/Experimental Group 3/ExperimentalGroupThree_4s_SD_Baseline_SleepStates.csv.gz') %>%
  mutate(Condition = "Baseline")

# ZT 14 LP Data
ZT14LPData <- vroom('Raw Data/Experimental Group 3/ExperimentalGroupThree_4s_ZT14LP_SleepStates.csv.gz')%>%
  mutate(Condition = "ZT14LP")

# Put them into one dataset and add hours
CombinedData <- rbind(BaselineData,ZT14LPData) %>%
  mutate(Hour = hour(DateTime))

# Select out hours needed 
CombinedData_2 <- CombinedData %>%
  filter(Hour >= 13 & Hour <= 16)

# Hypnogram #
SelectedMouse <- CombinedData_2 %>% filter(Mouse == 842)

ExGrp3HZHypno <- SelectedMouse %>%
  mutate(HMS = (hour(DateTime)+minute(DateTime)/60+second(DateTime)/3600), # Light Schedule slightly out
         `Wake State` = factor(`Rodent Sleep`, levels = c('W','S','P','X'),labels = c('Wake','NREM','REM','Wake')))

ggplot(ExGrp3HZHypno,mapping = aes(colour = `Wake State`))+
  geom_segment(aes(x=HMS, xend=HMS+0.0011, y="", yend="", fill = NULL), size=10) +
  scale_color_manual(values = c('gold','royalblue','firebrick2'))+
  geom_vline(xintercept = c(14,15),size = 0.5)+
  scale_x_continuous(breaks = seq(13,17,1))+
  labs(x = 'Zeitgeber Time',
       y = NULL)+
  theme_minimal(base_size = 15)+
  facet_grid(Condition~.)

ggsave(filename = "Mouse 842 LP Hypnogram.png",width = 200,height =50,units = "mm",dpi = 320)
ggsave(filename = "Mouse 842 LP Hypnogram.eps",width = 200,height =50,units = "mm",dpi = 320)

ggplot(ExGrp3HZHypno,mapping = aes(colour = `Wake State`))+
  geom_segment(aes(x=HMS, xend=HMS+0.0011, y="", yend="", fill = NULL), size=10) +
  scale_color_manual(values = c('gold','royalblue','firebrick2'))+
  geom_vline(xintercept = c(14,15),size = 0.5)+
  scale_x_continuous(breaks = seq(13,17,1))+
  labs(x = 'Zeitgeber Time',
       y = NULL)+
  theme_void(base_size = 15)+
  facet_grid(Condition~.)

ggsave(filename = "Mouse 842 LP Hypnogram Void.png",width = 200,height =25,units = "mm",dpi = 320)
ggsave(filename = "Mouse 842 LP Hypnogram Void.eps",width = 200,height =25,units = "mm",dpi = 320)


# Hypnogram #
SelectedMouse <- CombinedData_2 %>% filter(Mouse == 546)

ExGrp3HZHypno <- SelectedMouse %>%
  mutate(HMS = (hour(DateTime)+minute(DateTime)/60+second(DateTime)/3600), # Light Schedule slightly out
         `Wake State` = factor(`Rodent Sleep`, levels = c('W','S','P','X'),labels = c('Wake','NREM','REM','Wake')))

ggplot(ExGrp3HZHypno,mapping = aes(colour = `Wake State`))+
  geom_segment(aes(x=HMS, xend=HMS+0.0011, y="", yend="", fill = NULL), size=10) +
  scale_color_manual(values = c('gold','royalblue','firebrick2'))+
  geom_vline(xintercept = c(14,15),size = 0.5)+
  scale_x_continuous(breaks = seq(13,17,1))+
  labs(x = 'Zeitgeber Time',
       y = NULL)+
  theme_minimal(base_size = 15)+
  facet_grid(Condition~.)

ggsave(filename = "Mouse 546 LP Hypnogram.png",width = 200,height =50,units = "mm",dpi = 320)
ggsave(filename = "Mouse 546 LP Hypnogram.eps",width = 200,height =50,units = "mm",dpi = 320)

ggplot(ExGrp3HZHypno,mapping = aes(colour = `Wake State`))+
  geom_segment(aes(x=HMS, xend=HMS+0.0011, y="", yend="", fill = NULL), size=10) +
  scale_color_manual(values = c('gold','royalblue','firebrick2'))+
  geom_vline(xintercept = c(14,15),size = 0.5)+
  scale_x_continuous(breaks = seq(13,17,1))+
  labs(x = 'Zeitgeber Time',
       y = NULL)+
  theme_void(base_size = 15)+
  facet_grid(Condition~.)

ggsave(filename = "Mouse 546 LP Hypnogram Void.png",width = 200,height =25,units = "mm",dpi = 320)
ggsave(filename = "Mouse 546 LP Hypnogram Void.eps",width = 200,height =25,units = "mm",dpi = 320)
