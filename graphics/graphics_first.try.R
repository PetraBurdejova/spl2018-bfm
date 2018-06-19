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



### ideen:


library(ggplot2)

## SUN DE --

ggplot(data=df, aes(x=`TIME`, y=log(`SOLAR`))) +  
  geom_point(size=0.5) + 
  # geom_smooth(method="lm", aes(fill=`TIME`)) + 
  facet_wrap(~year(TIME)) +
  geom_smooth(method="loess", color="blue", se=FALSE)
# nur ein jahranzeigen pro grafik


## WIND --

ggplot(data=df, aes(x=`TIME`, y=log(`WIND`))) +  
  geom_point(size=0.5) + 
  geom_smooth(method="lm", aes(fill=`TIME`))  
# facet_wrap(~year(TIME)) +
# geom_smooth(method="loess", color="blue", se=FALSE)


## PUN --

ggplot(data=df, aes(x=`TIME`, y=`PUN`))  +
  geom_point(size=0.5, color = "black") + 
  # geom_smooth(method="lm", aes(fill=`TIME`), color = "black")  + 
  geom_smooth(method="loess", color="blue", se=FALSE) 


## DEMAND -- 

ggplot(data=df, aes(x=`TIME`, y=log(`DEM`))) +  
  geom_point(size=0.5) + 
  # geom_smooth(method="lm", aes(fill=`TIME`)) + 
  # facet_wrap(~year(TIME)) +
  geom_smooth(method="loess", color="green", se=FALSE) +
  ylim(15.5, 15.8)
# facet_wrap(~year(TIME)) 


## PUN ~ SOLAR --   

ggplot(data=df, aes(y=`PUN`, x=log(`SOLAR`))) +  
  ylim(200,1500) +
  geom_point(size=0.5) + 
  geom_smooth(method="lm", aes(fill=`TIME`))  
# facet_wrap(~year(TIME)) +
# geom_smooth(method="loess", color="green", se=FALSE) +
# facet_wrap(~year(TIME)) 


## PUN ~ DEMAND --

ggplot(data=df, aes(y=`PUN`, x=log(`DEM`))) +  
  ylim(0,1500) +
  geom_point(size=0.5) + 
  geom_smooth(method="lm", aes(fill=`TIME`))  
# facet_wrap(~year(TIME)) +
# geom_smooth(method="loess", color="green", se=FALSE) 
# facet_wrap(~year(TIME)) 

=======
>>>>>>> d40d2d9b3b8744b4a59983a53f5e87b1fd44ae66
