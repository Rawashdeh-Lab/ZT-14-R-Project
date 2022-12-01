##### Act Data ZT 14 LP ####
ActivityData <- read_csv("Raw Data/ActivityData.csv", 
                         col_types = cols(DateTime = col_datetime(format = "%d/%m/%Y %H:%M:%S")))

ActivitySpread <- ActivityData %>%
  gather(key = 'Mouse', value = 'Activity', -DateTime) %>%
  mutate(Genotype = case_when(Mouse > 700 ~ "Per1KO", Mouse < 700 ~ "WT"),
         Injection = case_when(Mouse %in% c(541,556,557,832,833,851)~ 'PFF', T ~'MONO'),
         GenoInj = paste(Genotype,Injection,sep = " "),
         HM = hour(DateTime)+(minute(DateTime)/60),
         RoundedTime = RoundTo(HM,multiple = 10/60,FUN = floor),
         ActFull = Activity * 60)

ActivitySpread2 <- ActivitySpread %>%
  group_by(Mouse,Genotype,Injection,GenoInj,RoundedTime) %>%
  summarise(Activity = sum(ActFull))

ggplot(ActivitySpread2, aes(x = RoundedTime,y = Activity, group = GenoInj, fill = GenoInj))+
  stat_summary(geom = 'col',fun.y = 'mean')+
  stat_summary(geom = 'errorbar',fun.data = 'mean_se')+
  theme_light(base_family = 'serif',base_size = 13)+
  scale_x_continuous(breaks = seq(0,23.9,4))+
  ylab("Amount of sleep per hour (min)")+
  xlab("ZT (Hour)")+
  coord_cartesian(expand = 0)+
  theme(panel.spacing.x = unit(0,'lines'),
        legend.position = 'none',
        strip.background = element_rect(color="black", fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(colour = 'black'))+
  facet_grid(GenoInj~.)

ggplot(ActivitySpread, aes(x = HM,y = Activity, group = GenoInj, colour = GenoInj))+
  geom_step()+
  theme_light(base_family = 'serif',base_size = 13)+
  scale_x_continuous(breaks = seq(0,23.9,4))+
  ylab("Amount of sleep per hour (min)")+
  xlab("ZT (Hour)")+
  coord_cartesian(expand = 0)+
  theme(panel.spacing.x = unit(0,'lines'),
        legend.position = 'none',
        strip.background = element_rect(color="black", fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(colour = 'black'))+
  facet_grid(Mouse~GenoInj)
