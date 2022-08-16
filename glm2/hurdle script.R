
# fitting hurdle models to the lesion data to account for excess zero counts
# the idea is to compare the overall level of incidence [zero counts], mean for cultivars 
# also compare the fit of the models with hurdle with Poisson and hurdle with NB for potted/orchard
# trees.

library(MASS)
library(pscl)
library(lmtest)
library(tidyverse)
library(tables)

# read in observed data

dat <- read.csv(here::here("glm2/all_lesions_for_R.csv")) |> 
  janitor::clean_names() |> 
  mutate_at(vars(year, cultivar), as.factor) |> 
  mutate(infected = count > 0, 
         count1 = count+1)

skimr::skim(dat)

dat |> 
  ggplot()+
  aes(cultivar, count1)+
  geom_point()+ 
  facet_wrap("year")
a
# Fitting hurdle models with poisson 

pois <- glm(count1~cultivar * year, family="poisson",data=dat)
pois2 <- glm(count1~cultivar * year, family="quasipoisson",data=dat)
AIC(pois,pois2)

P_hpoisson1 <- hurdle(count~cultivar * year, data=dat)
summary(P_hpoisson1)
P_hpoisson2 <- hurdle(count~cultivar * year|cultivar, data=dat)
summary(P_hpoisson2)

# Fitting hurdle models with negative binomial 

P_hnb1 <- hurdle(count~cultivar * year, dist="negbin",data=dat)
summary(P_hnb1)

P_hnb2 <- hurdle(count~cultivar * year|cultivar, dist="negbin", data=dat)
summary(P_hnb2)

AIC(P_hpoisson2,P_hnb2)

library(emmeans)
cld(emmeans(P_hnb2, ~ cultivar | year, adjust = "sidak") ) 
