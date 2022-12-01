BaselineData <- vroom('Mid Data/Baseline/ExperimentalGroupTwo_4s_SD_Baseline_SleepStates.csv.gz')%>%
  mutate(Hour = hour(DateTime))%>%
  filter(Day == 0 & Hour >= 13 & Hour <= 16)

BaselineDataEdit <- BaselineData %>%
  mutate(HMS = (hour(DateTime)+minute(DateTime)/60+second(DateTime)/3600), # Light Schedule slightly out
         Colours = factor(GenoInj,levels = c('WT MONO','WT PFF','Per1KO MONO','Per1KO PFF')),
         MouseNums = factor(Mouse, levels = c(737,738,749,750,751,449,450,442,446,447,448)),
         `Wake State` = factor(`Rodent Sleep`, levels = c('W','S','P','X'),labels = c('Wake','NREM','REM','Wake')))

tp <- unique(BaselineDataEdit[,c('MouseNums','Colours')])

ggplot(BaselineDataEdit,mapping = aes(colour = `Wake State`))+
  geom_rect(data= tp, xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,aes(colour = NULL, fill = Colours),alpha = 0.4)+
  geom_segment(aes(x=HMS, xend=HMS+0.00278, y="", yend="", fill = NULL), size=10) +
  scale_fill_manual(values = c('skyblue','purple','red','brown'))+
  scale_color_manual(values = c('gold','royalblue','firebrick2'))+
  geom_vline(xintercept = c(14,15),size = 1)+
  scale_x_continuous(breaks = seq(12,17,1))+
  labs(x = 'ZT (Hour)',
       y = NULL,
       fill = 'Genotype + Injection')+
  theme_minimal(base_family = 'serif',base_size = 15)+
  facet_grid(MouseNums~.)


ExGrp2HZ <- vroom("Mid Data/ExperimentalGroupTwo_ZT14LP_1_SleepStates.csv.gz")
#### Hypnogram ####
ExGrp2HZEdit <- ExGrp2HZ %>%
  mutate(HMS = (hour(ZTDateTime)+minute(ZTDateTime)/60+second(ZTDateTime)/3600), # Light Schedule slightly out
         Colours = factor(GenoInj,levels = c('WT MONO','WT PFF','Per1KO MONO','Per1KO PFF')),
         MouseNums = factor(Mouse, levels = c(737,738,749,750,751,449,450,442,446,447,448)),
         `Wake State` = factor(`Rodent Sleep`, levels = c('W','S','P','X'),labels = c('Wake','NREM','REM','Wake')))

tp <- unique(ExGrp2HZEdit[,c('MouseNums','Colours')])

ggplot(ExGrp2HZEdit,mapping = aes(colour = `Wake State`))+
  geom_rect(data= tp, xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,aes(colour = NULL, fill = Colours),alpha = 0.4)+
  geom_segment(aes(x=HMS, xend=HMS+0.00278, y="", yend="", fill = NULL), size=10) +
  scale_fill_manual(values = c('skyblue','purple','red','brown'))+
  scale_color_manual(values = c('gold','royalblue','firebrick2'))+
  geom_vline(xintercept = c(14,15),size = 1)+
  scale_x_continuous(breaks = seq(12,17,1))+
  labs(x = 'ZT (Hour)',
       y = NULL,
       fill = 'Genotype + Injection')+
  theme_minimal(base_family = 'serif',base_size = 15)+
  facet_grid(MouseNums~.)

P <- ggplot(ExGrp2HZEdit,mapping = aes(colour = `Wake State`))+
  geom_rect(data= tp, xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,aes(colour = NULL, fill = Colours),alpha = 0.4)+
  geom_segment(aes(x=HMS, xend=HMS+0.00278, y="", yend="", fill = NULL), size=10) +
  scale_fill_manual(values = c('skyblue','purple','red','brown'))+
  scale_color_manual(values = c('gold','royalblue','firebrick2'))+
  geom_vline(xintercept = c(14,15),size = 1)+
  scale_x_continuous(breaks = seq(12,17,1))+
  labs(x = 'ZT (Hour)',
       y = NULL,
       fill = 'Genotype + Injection',
       title = 'Run 1')+
  theme_minimal(base_family = 'serif',base_size = 15)+
  facet_grid(MouseNums~.)

P2<-dml(ggobj = P) 

ppt <- read_pptx()
ppt <- add_slide(ppt,layout = 'Title Only')
ppt <- ph_with(ppt,P2,ph_location_fullsize())
print(ppt,'ExGrp2 24 Hr Baseline Hypnogram.pptx')

ExGrp2HZ_2 <- vroom("Mid Data/ExperimentalGroupTwo_ZT14LP_2_SleepStates.csv.gz")
#### Hypnogram ####
ExGrp2HZEdit_2 <- ExGrp2HZ_2 %>%
  mutate(HMS = (hour(ZTDateTime)+minute(ZTDateTime)/60+second(ZTDateTime)/3600), # Light Schedule slightly out
         Colours = factor(GenoInj,levels = c('WT MONO','WT PFF','Per1KO MONO','Per1KO PFF')),
         MouseNums = factor(Mouse, levels = c(737,738,749,750,751,449,450,442,446,447,448)),
         `Wake State` = factor(`Rodent Sleep`, levels = c('W','S','P','X'),labels = c('Wake','NREM','REM','Wake')))

tp_2 <- unique(ExGrp2HZEdit_2[,c('MouseNums','Colours')])

ggplot(ExGrp2HZEdit_2,mapping = aes(colour = `Wake State`))+
  geom_rect(data= tp_2, xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,aes(colour = NULL, fill = Colours),alpha = 0.4)+
  geom_segment(aes(x=HMS, xend=HMS+0.00278, y="", yend="", fill = NULL), size=10) +
  scale_fill_manual(values = c('skyblue','purple','red','brown'))+
  scale_color_manual(values = c('gold','royalblue','firebrick2'))+
  geom_vline(xintercept = c(14,15),size = 1)+
  scale_x_continuous(breaks = seq(12,17,1))+
  labs(x = 'ZT (Hour)',
       y = NULL,
       fill = 'Genotype + Injection')+
  theme_minimal(base_family = 'serif',base_size = 15)+
  facet_grid(MouseNums~.)

P_2 <- ggplot(ExGrp2HZEdit_2,mapping = aes(colour = `Wake State`))+
  geom_rect(data= tp_2, xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,aes(colour = NULL, fill = Colours),alpha = 0.4)+
  geom_segment(aes(x=HMS, xend=HMS+0.00278, y="", yend="", fill = NULL), size=10) +
  scale_fill_manual(values = c('skyblue','purple','red','brown'))+
  scale_color_manual(values = c('gold','royalblue','firebrick2'))+
  geom_vline(xintercept = c(14,15),size = 1)+
  scale_x_continuous(breaks = seq(12,17,1))+
  labs(x = 'ZT (Hour)',
       y = NULL,
       fill = 'Genotype + Injection',
       title = 'Run 2')+
  theme_minimal(base_family = 'serif',base_size = 15)+
  facet_grid(MouseNums~.)

ggpubr::ggarrange(P,P_2,ncol = 2,common.legend = T,legend = 'right')
