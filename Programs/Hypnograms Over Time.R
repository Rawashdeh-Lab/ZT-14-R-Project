#### Import Ex3 Data ####
ExGrp3LP14HZ <- vroom('Mid Data/ExperimentalGroupThree_ZT14__HZ_LP.csv.gz')

FlipandPercent30Hz<- function(DATASET){
  print('Flipping')
  D1 <- DATASET %>%
    filter(Stage != "Artifact") %>%
    select(EpochNo,Stage,`0.488281Hz`:`29.907227Hz`,Mouse,Genotype,Injection,GenoInj,FileRed,ZT_Hour)%>%
    gather(key = 'HZ', value = 'Level', -Stage, -Mouse, -Genotype, -Injection, -GenoInj,-EpochNo, -FileRed,-ZT_Hour)  %>%
    mutate(Hz2 = as.numeric(gsub("Hz","", HZ))) %>%
    filter(Hz2 >= 0.5 & Hz2 <= 30) %>%
    ungroup() 
  
  print('Dipping')
  D2 <- D1 %>%
    group_by(Mouse,EpochNo,FileRed) %>%
    mutate(Percent = (Level/sum(Level))*100) %>%
    ungroup()
  
  print('Deep Frying')
  D3 <- D2 %>%
    group_by(Stage,Mouse,Genotype,Injection,GenoInj, Hz2) %>%
    summarise(Level = mean(Level),
              Percent = mean(Percent))%>%
    mutate(Stage2 = factor(Stage,levels = c("W","NR","R"),labels = c('Wake',"NREM",'REM'))) %>%
    ungroup()
  
  return(D3)
}
FlipandPercent30Hz_NoStage<- function(DATASET){
  print('Flipping')
  D1 <- DATASET %>%
    filter(Stage != "Artifact") %>%
    select(EpochNo,Stage,`0.488281Hz`:`29.907227Hz`,Mouse,Genotype,Injection,GenoInj,FileRed,ZT_Hour,RoundedTime)%>%
    gather(key = 'HZ', value = 'Level', -Stage, -Mouse, -Genotype, -Injection, -GenoInj,-EpochNo, -FileRed,-ZT_Hour,-RoundedTime)  %>%
    mutate(Hz2 = as.numeric(gsub("Hz","", HZ))) %>%
    filter(Hz2 >= 0.5 & Hz2 <= 30) %>%
    ungroup() 
  
  print('Dipping')
  D2 <- D1 %>%
    group_by(Mouse,EpochNo,FileRed) %>%
    mutate(Percent = (Level/sum(Level))*100) %>%
    ungroup()
  
  print('Deep Frying')
  D3 <- D2 %>%
    group_by(RoundedTime,Mouse,Genotype,Injection,GenoInj, Hz2) %>%
    summarise(Level = mean(Level),
              Percent = mean(Percent))%>%
    ungroup()
  
  return(D3)
}

ExGrp3LP14HZ_2<- ExGrp3LP14HZ %>%
  mutate(ZT_Hour = hour (DateTime)) %>%
  filter(ZT_Hour == 14)

ExGrp3LP15HZ_2<- ExGrp3LP14HZ %>%
  mutate(ZT_Hour = hour (DateTime)) %>%
  filter(ZT_Hour == 15)

ExGrp3LP14HZ_2Percentages <- FlipandPercent30Hz(DATASET =ExGrp3LP14HZ_2)
ExGrp3LP15HZ_2Percentages <- FlipandPercent30Hz(DATASET =ExGrp3LP15HZ_2)

ggplot(ExGrp3LP14HZ_2Percentages, aes(x = Hz2, y = Percent, group = GenoInj, colour = GenoInj,fill = GenoInj))+
  annotate(geom = 'rect',xmin = 0.5,xmax = 4,ymin = -Inf,ymax = Inf, fill = 'skyblue',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 4,xmax = 8,ymin = -Inf,ymax = Inf, fill = 'red',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 8,xmax = 12,ymin = -Inf,ymax = Inf, fill = 'green',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 12,xmax = 16,ymin = -Inf,ymax = Inf, fill = 'orange',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 16,xmax = 24,ymin = -Inf,ymax = Inf, fill = 'purple',alpha = 0.1)+
  stat_summary(geom = 'line',fun = 'mean')+
  stat_summary(geom = 'errorbar', fun.data = 'mean_se',alpha=0.3)+
  theme_classic()+
  scale_colour_manual(values = c('brown1','darkorange','royalblue3','mediumorchid2'))+
  coord_cartesian(ylim = c(0,3))+
  labs(x = "Frequency (Hz)", y = "Relative Power(%)",
       colour = "Genotype + Injection",
       fill = "Genotype + Injection")+
  theme(plot.title = element_text(hjust = 0.5,size = 20))+
  facet_grid(Genotype~Stage2)

ggplot(ExGrp3LP15HZ_2Percentages, aes(x = Hz2, y = Percent, group = GenoInj, colour = GenoInj,fill = GenoInj))+
  annotate(geom = 'rect',xmin = 0.5,xmax = 4,ymin = -Inf,ymax = Inf, fill = 'skyblue',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 4,xmax = 8,ymin = -Inf,ymax = Inf, fill = 'red',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 8,xmax = 12,ymin = -Inf,ymax = Inf, fill = 'green',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 12,xmax = 16,ymin = -Inf,ymax = Inf, fill = 'orange',alpha = 0.1)+
  annotate(geom = 'rect',xmin = 16,xmax = 24,ymin = -Inf,ymax = Inf, fill = 'purple',alpha = 0.1)+
  stat_summary(geom = 'line',fun = 'mean')+
  stat_summary(geom = 'errorbar', fun.data = 'mean_se',alpha=0.3)+
  theme_classic()+
  scale_colour_manual(values = c('brown1','darkorange','royalblue3','mediumorchid2'))+
  coord_cartesian(ylim = c(0,3))+
  labs(x = "Frequency (Hz)", y = "Relative Power(%)",
       colour = "Genotype + Injection",
       fill = "Genotype + Injection")+
  theme(plot.title = element_text(hjust = 0.5,size = 20))+
  facet_grid(Genotype~Stage2)

#### Per Time Period ####
ExGrp3LP14HZ_Round<- ExGrp3LP14HZ %>%
  mutate(ZT_Hour = hour (DateTime),
         RoundedTime = round_date(DateTime,unit = '10 minutes')) 

ExGrp3LP14HZ_2Percentages <- FlipandPercent30Hz_NoStage(DATASET =ExGrp3LP14HZ_Round)
unique(ExGrp3LP13HZ_2Percentages$Mouse)

M835 <- ExGrp3LP14HZ_2Percentages %>% filter(Mouse == 835)
M836 <- ExGrp3LP14HZ_2Percentages %>% filter(Mouse == 836)
M842 <- ExGrp3LP14HZ_2Percentages %>% filter(Mouse == 842)
  
M835P <- ggplot(M835, aes(x=RoundedTime, y=Hz2, fill = Percent) ) +
  geom_tile()+
  scale_fill_viridis(option = "D",limits = c(0,2.5)) +
  coord_cartesian(expand = 0)+
  theme_tufte(base_size = 18,base_family = 'serif')+
  geom_vline(xintercept = c(14,15))+
  scale_y_continuous(breaks = seq(0,30,3))+
  labs(fill = 'Prevalence (%)',y = 'Frequency (Hz)',x = "Time (o'clock)")+
  facet_grid(~GenoInj*Mouse)+
  theme(panel.spacing.x = unit(0,'lines'))+
  theme(legend.position = 'none')

M836P <- ggplot(M836, aes(x=RoundedTime, y=Hz2, fill = Percent) ) +
  geom_tile()+
  scale_fill_viridis(option = "D",limits = c(0,2.5)) +
  coord_cartesian(expand = 0)+
  theme_tufte(base_size = 18,base_family = 'serif')+
  geom_vline(xintercept = c(14,15))+
  scale_y_continuous(breaks = seq(0,30,3))+
  labs(fill = 'Prevalence (%)',y = 'Frequency (Hz)',x = "Time (o'clock)")+
  facet_grid(~GenoInj*Mouse)+
  theme(panel.spacing.x = unit(0,'lines'))+
  theme(legend.position = 'none')

M842P <- ggplot(M842, aes(x=RoundedTime, y=Hz2, fill = Percent) ) +
  geom_tile()+
  scale_fill_viridis(option = "D",limits = c(0,2.5)) +
  coord_cartesian(expand = 0)+
  theme_tufte(base_size = 18,base_family = 'serif')+
  geom_vline(xintercept = c(14,15),size = 5)+
  scale_y_continuous(breaks = seq(0,30,3))+
  labs(fill = 'Prevalence (%)',y = 'Frequency (Hz)',x = "Time (o'clock)")+
  facet_grid(~GenoInj*Mouse)+
  theme(panel.spacing.x = unit(0,'lines'))+
  theme(legend.position = 'none')
  
HerWTM <- ggpubr::ggarrange(M835P,M836P,M842P,nrow = 1,common.legend = TRUE,legend = 'right')

#### Add Hypnograms ####
HypnoData <- vroom('Mid Data/ExperimentalGroupThree_ZT14LP_Hypnogram.csv.gz')

HypnoDataEdit <- HypnoData %>%
  mutate(HMS = (hour(DateTimeFix)+minute(DateTimeFix)/60+second(DateTimeFix)/3600)+0.031166667, # Light Schedule slightly out
         Colours = factor(GenoInj,levels = c('WT MONO','WT PFF','Per1KO MONO','Per1KO PFF')),
         MouseNums = factor(Mouse, levels = c(835,836,842,832,833,851,545,546,548,541,556,557)),
         `Wake State` = factor(Label, levels = c('W','S','P','X'),labels = c('Wake','NREM','REM','Artifact'))) 

HypnoDataEdit2 <- HypnoDataEdit

tp <- unique(HypnoDataEdit[,c('MouseNums','Colours')])

H835 <- ggplot(HypnoDataEdit2[which(HypnoDataEdit2$Mouse == 835),],mapping = aes(colour = `Wake State`))+
  geom_segment(aes(x=HMS, xend=HMS+0.00278, y="", yend="", fill = NULL), size=10,) +
  scale_color_manual(values = c('gold','royalblue','firebrick2','grey'))+
  scale_x_continuous(breaks = seq(13,17,1))+
  labs(x = 'ZT (Hour)',
       y = NULL,
       fill = 'Genotype + Injection')+
  theme_tufte()+
  theme(legend.position = 'none')

H836 <- ggplot(HypnoDataEdit2[which(HypnoDataEdit2$Mouse == 836),],mapping = aes(colour = `Wake State`))+
  geom_segment(aes(x=HMS, xend=HMS+0.00278, y="", yend="", fill = NULL), size=10,) +
  scale_color_manual(values = c('gold','royalblue','firebrick2','grey'))+
  scale_x_continuous(breaks = seq(13,17,1))+
  labs(x = 'ZT (Hour)',
       y = NULL,
       fill = 'Genotype + Injection')+
  theme_minimal()+
  theme(legend.position = 'none')

H842 <- ggplot(HypnoDataEdit2[which(HypnoDataEdit2$Mouse == 842),],mapping = aes(colour = `Wake State`))+
  geom_segment(aes(x=HMS, xend=HMS+0.00278, y="", yend="", fill = NULL), size=10,) +
  scale_color_manual(values = c('gold','royalblue','firebrick2','grey'))+
  scale_x_continuous(breaks = seq(13,17,1))+
  labs(x = 'ZT (Hour)',
       y = NULL,
       fill = 'Genotype + Injection')+
  theme_minimal()+
  theme(legend.position = 'none')

HypWTM<- ggpubr::ggarrange(H835,H836,H842,nrow = 1,common.legend = TRUE,legend = 'right')

ggpubr::ggarrange(HerWTM,HypWTM, nrow = 2,ncol =1,heights = c(2,0.5))

egg::ggarrange(M835P,H835, nrow = 2,ncol =1,heights = c(2,0.5))

# trans = 'sqrt',limits = c(0,5),breaks = c(0, 0.25,1,2.5,5)
