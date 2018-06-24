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
ggplot(data=df, aes(x=TIME, y=SOLAR)) +  
  geom_point(size=0.5) + 
  geom_smooth(method="loess", span = 0.25, color="orange", se=FALSE) + 
  ggtitle(label = "Solar Energy Production per Day", 
          subtitle = "German and Austrian Production in MW/h") +
  xlab(label= " ") +
  ylab(label = "MW/h")
  # labs(caption = "(based on data from 'Netztransparenz' and 'APG' ")



ggsave("Hourly Demand", plot = last_plot(), device = "pdf", 
       path =  NULL,
       scale = 1.5, width = NA, height = NA, units = c("in", "cm", "mm"),
       dpi = 800, limitsize = TRUE)



## WIND --

ggplot(data=df, aes(x=`TIME`, y=(`WIND`))) +  
  geom_point(size=0.5) + 
  geom_smooth(method="lm", span = 1.5, color="blue", se=T) + 
  ggtitle(label = "Wind Energy Production per Day", 
        subtitle = "German and Austrian Production in MW/h") +
  xlab(label= " ") +
  ylab(label = "MW/h")
  # labs(caption = "(based on data from 'Netztransparenz' and 'APG' ")


## PUN --

ggplot(data=df, aes(x=`TIME`, y=`PUN`))  +
  geom_point(size=0.5, color = "black") + 
  # geom_smooth(method="lm", aes(fill=`TIME`), color = "black")  + 
  geom_smooth(method="lm", color="gold", se=T) +
  ggtitle(label = "Day-Ahead Price MW/h") +
  xlab(label= "") +
  ylab(label = "€ per MW/h ")
  # labs(caption = "(based on data from 'energidataservice - DK' and 'ENTSOE' ")


## DEMAND -- 

ggplot(data=df, aes(x=`TIME`, y=(`DEM`))) +  
  geom_point(size=0.2, color = "black") + 
  # geom_smooth(method="lm", aes(fill=`TIME`)) +
  # facet_wrap(~year(TIME+365*0.5)) +
  geom_smooth(method="loess", span = 0.3, color="blue", se=FALSE) +
  #ylim(3.5e+06, 7.5e+06) +
  ggtitle(label = "Demand Day-Ahead forecast ",
          subtitle = "Daily MW/h") +
  xlab(label= " ") +
  ylab(label = "MW/h")
  # labs(caption = "(based on data from 'energidataservice - DK' and 'ENTSOE' ")



## PUN ~ SOLAR --   

ggplot(data=df, aes(y=`PUN`, x=log(`SOLAR`))) +  
  ylim(200,1500) +
  geom_point(size=0.5) + 
  geom_smooth(method="lm", aes(fill=`TIME`))  
# facet_wrap(~year(TIME)) +
# geom_smooth(method="loess", color="green", se=FALSE) +
# facet_wrap(~year(TIME)) 


## PUN ~ DEMAND --

ggplot(data=df, aes(y=`PUN`, x=(`DEM`))) +  
  ylim(0,1500) +
  geom_point(size=0.5) + 
  geom_smooth(method="lm", aes(fill=`TIME`))+
  ggtitle(label = "Price and Demand") +
  xlab(label= "Demand in MW/h") +
  ylab(label = "€ per MW/h ")+
  labs(caption = "(based on data from 'energidataservice - DK' and 'ENTSOE' ")






## tabplot idea
# dont really get the idea..

install.packages("tabplot")
library("tabplot")
library("MASS")

tableplot(df[, -1], colorNA = "red", colorNA_num = "red")
summary(df)
