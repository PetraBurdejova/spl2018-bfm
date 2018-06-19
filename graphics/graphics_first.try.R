##### SPL Data visualization ------
# My first try at graphics.
# just ideas. I am going to do the ggplot 2 courses soon.
# if you have improvements/ other ideas go wild ;)

## LOADING DATA FROM DATA-MERGER & LOADING PACKAGES -----

#source("data_merger.R")
library(ggplot2)

## GRAPHICS -----
## SUN --

# Nice graphic of sun per year 
ggplot(data=df, aes(x=TIME, y=SOLAR)) +  
  geom_point(size=0.5) + 
  geom_smooth(method="loess", span = 0.25, color="orange", se=FALSE) + 
  ggtitle(label = "Solar Production per Day, DE & AT", 
          subtitle = "Daily Solar Production in MW/h") +
  xlab(label= " ") +
  ylab(label = "MW/h")+
  labs(caption = "(based on data from ...)")
