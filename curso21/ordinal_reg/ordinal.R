# browseURL("https://www.r-bloggers.com/how-to-perform-ordinal-logistic-regression-in-r/")
browseURL("http://127.0.0.1:8295/help/library/ordinal/doc/clm_article.pdf")

library(ggeffects)
library(ordinal)
library(tidyverse)

# tibble(soup)

mod <- clmm(SURENESS ~ PROD * COLD + (1|RESP), data = soup)
summary(mod)

pred <- ggpredict(mod, c("PROD", "COLD"))
pred
plot(pred)

soup$SURENESS
