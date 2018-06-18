##### SPL Data visualization ------
# My first try at graphics.
# just ideas. I am going to do the ggplot 2 courses soon.
# if you have improvements/ other ideas go wild ;)

## LOADING DATA FROM DATA-MERGER & LOADING PACKAGES -----

source("data_merger.R")
library(ggplot2)

## GRAPHICS -----
## SUN --

# Nice graphic of sun per year 
ggplot(data=df, aes(x=`TIME`, y=log(`SOLAR.MW/h`))) +  
  geom_point(size=0.5) + 
  # geom_smooth(method="lm", aes(fill=`TIME`)) + 
  facet_wrap(~year(TIME)) +
  geom_smooth(method="loess", color="blue", se=FALSE) + 
  ggtitle(label = "Solar Production per Year, DE & AT", 
          subtitle = "log of Solar MW/h") +
  xlab(label= " ") +
  ylab(label = "lg. MW/h")+
  labs(caption = "(based on data from ...)")



