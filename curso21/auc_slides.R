dpc <- tibble(t=c(0,2,5,6), y=c(1,2,7, 7.5))
  
p1 <- dpc |>
  ggplot(aes(t, y)) +
  labs(x = "Time (days)",
       y = "Disease severity (%)")+
  geom_area(alpha = 0.5, fill = "orange")+
  geom_line(size = 1, col="mediumblue")+
  geom_point(size = 3, col="mediumblue")+
  theme_bw()+
  lims(y=c(0, 8))

p2 <- p1 + annotate("rect", xmin = dpc$t[1], xmax = dpc$t[2], 
              ymin = 0, ymax = (dpc$y[1]+ dpc$y[2])/2, 
              color = "red", fill = "orange", alpha = 0.5)+
  annotate("rect", xmin = dpc$t[2], xmax = dpc$t[3], 
           ymin = 0, ymax = (dpc$y[2]+ dpc$y[3])/2, 
           color = "red", fill = "orange", alpha = 0.5)+
  annotate("rect", xmin = dpc$t[3], xmax = dpc$t[4], 
           ymin = 0, ymax = (dpc$y[3]+ dpc$y[4])/2,
           color = "red",, fill = "orange", alpha = 0.5)+
  annotate("rect", xmin = dpc$t[4], xmax = dpc$t[5], 
           ymin = 0, ymax = (dpc$y[4]+ dpc$y[5])/2, 
           color = "red", fill = "orange", alpha = 0.5)+
  annotate("rect", xmin = dpc$t[5], xmax = dpc$t[6], 
           ymin = 0, ymax = (dpc$y[5]+ dpc$y[6])/2, 
           color = "red",fill = "orange", alpha = 0.5)+
  annotate("rect", xmin = dpc$t[6], xmax = dpc$t[7], 
           ymin = 0, ymax = (dpc$y[6]+ dpc$y[7])/2, 
           color = "red", fill = "orange", alpha = 0.5)
library(patchwork)
p1+p2
