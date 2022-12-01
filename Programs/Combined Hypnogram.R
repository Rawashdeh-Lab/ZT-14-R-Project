#### Hypnogram Data ####
HypnoData <- vroom('Mid Data/ExperimentalGroupThree_ZT14LP_Hypnogram.csv.gz')

HypnoDataEdit <- HypnoData %>%
  mutate(HMS = (hour(DateTimeFix)+minute(DateTimeFix)/60+second(DateTimeFix)/3600)+0.031166667, # Light Schedule slightly out
         Colours = factor(GenoInj,levels = c('WT MONO','WT PFF','Per1KO MONO','Per1KO PFF')),
         MouseNums = factor(Mouse, levels = c(835,836,842,832,833,851,545,546,548,541,556,557)),
         `Wake State` = factor(Label, levels = c('W','S','P','X'),labels = c('Wake','NREM','REM','Artifact'))) 

HypnoDataEdit2 <- HypnoDataEdit %>% filter(HMS >=13,HMS <= 16)

tp <- unique(HypnoDataEdit[,c('MouseNums','Colours')])

ggplot(HypnoDataEdit2,mapping = aes(colour = `Wake State`))+
  geom_rect(data= tp, xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf,aes(colour = NULL, fill = Colours),alpha = 0.4)+
  geom_segment(aes(x=HMS, xend=HMS+0.00278, y="", yend="", fill = NULL), size=7) +
  scale_fill_manual(values = c('skyblue','purple','red','brown'))+
  scale_color_manual(values = c('gold','royalblue','firebrick2','grey'))+
  geom_vline(xintercept = c(14,15),size = 1)+
  scale_x_continuous(breaks = seq(13,17,0.25))+
  labs(x = 'ZT (Hour)',
       y = NULL,
       fill = 'Genotype + Injection')+
  theme_minimal()+
  facet_grid(MouseNums~.)

P2<-dml(ggobj = P) 

ppt <- read_pptx()
ppt <- add_slide(ppt,layout = 'Title Only')
ppt <- ph_with(ppt,P2,ph_location_fullsize())
print(ppt,'Hypnogram.pptx')

WTMONO <- filter(HypnoDataEdit,GenoInj == 'WT MONO') 
WTPFF <- filter(HypnoDataEdit,GenoInj == 'WT PFF') 
P1MONO <- filter(HypnoDataEdit,GenoInj == 'Per1KO MONO') 
P1PFF <- filter(HypnoDataEdit,GenoInj == 'Per1KO PFF') 

# VisTime Trial
Animal1 <- filter(HypnoDataEdit,Mouse == 545)
vistime(Animal1, events="Label", start="DateTimeFix", end="DateTimeFix2")
