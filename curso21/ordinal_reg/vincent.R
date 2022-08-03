library(tidyverse)
SummerInoculationFinal <- read_csv("https://github.com/juanchiem/agro_data/raw/master/SummerInoculationFinal.csv") %>% 
  mutate_at("ScoreFactor", as.factor)
glimpse(SummerInoculationFinal)

library(ordinal)

Score.0<-(clmm(ScoreFactor~1+(1|Plot),data=SummerInoculationFinal))
Score.All<-update(Score.0,~.+Pruned*Apogee*Year)
Score.PY.YAb<-update(Score.All,~ApogeeBino*Year+Pruned*Year+(1|Plot) )

anova(Score.0,Score.PY.YAb,Score.All)

summary(Score.PY.YAb)

ggplot(SummerInoculationFinal, aes(x = Pruned, fill = ScoreFactor))+
  geom_bar(position = position_fill(reverse = TRUE))+
  facet_grid(cols=vars(Apogee),rows=vars(Year))+
  xlab("") + ylab("Proportion of shoots")+
  scale_fill_grey(name = 'Score',start =.9, end = 0 )+
  scale_x_discrete(labels = c("Pruned", "Control"))
