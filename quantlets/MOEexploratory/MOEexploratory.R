###############################################################################     
####    MOEexploratory.R    #####################################################     
###############################################################################     
#
# Loads data set from 'MOEmergedata'. Creates exploratory plots of data 
#
# Input:  '.Rdata' file from the 'MOEmergedata' Quantlet.
#
# Ouput:  MOEdata_merge.csv     - data in table form
#         MOEdata_merge.Rdata   - data in Rdata form
#
###############################################################################

# Clear all variables.
rm(list = ls(all = TRUE))
graphics.off()

# Install and load libraries.
libraries = c("ggplot2", "tikzDevice")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


###############################################################################
####    0.  SET WORKING DIRECTORY    ##########################################
###############################################################################
####    ATTENTION: Working directory is assumed to be the root of the MOE 
####    repository, not the MOEmergedata Quantlet subdirectory!!!


# If needed, set working directory accordingly:
#setwd("path/to/MOE_repository")



###############################################################################
####    1.  LOAD DATA    ######################################################
###############################################################################


# Grab data from 'MOEmergedata' Quantlet
load("MOEmergedata/MOEdata_merge.Rdata")


## GRAPHICS -----
## SUN --

# Sun per Year 
options(tz="Berlin")

tikz(file = "plot1.tex", width = 5, height = 5, console = T, engine = getOption("tikzDefaultEngine"))

plot = ggplot(data=df, aes(x=TIME, y=SOLAR)) +  
          geom_point(size=0.5) + 
          geom_smooth(method="loess", span = 0.25, color="orange", se=FALSE) + 
          ggtitle(label = "Solar Energy Production per Day", 
                  subtitle = "German and Austrian Production in MW/h") +
          xlab(label= " ") +
          ylab(label = "MW/h")+
          labs(caption = "(based on data from 'Netztransparenz' and 'APG' ")
        
print(plot)
dev.off()

options(tikzLatex = )
?tikz

ggsave("Solar Production", plot = last_plot(), device = "pdf", 
       path =  NULL, scale = 1.5, width = 7.5 , height = 5, 
       units = c("in", "cm", "mm"), dpi = 800, limitsize = TRUE)




## WIND --

ggplot(data=df, aes(x=`TIME`, y=(`WIND`))) +  
  geom_point(size=0.5) + 
  geom_smooth(method="lm", span = 1.5, color="blue", se=T) + 
 # ggtitle(label = "Wind Energy Production per Day", 
   #     subtitle = "German and Austrian Production in MW/h") +
  xlab(label= " ") +
  ylab(label = "MW/h")+
  theme_bw()
  # labs(caption = "(based on data from 'Netztransparenz' and 'APG' ")



ggsave("Wind", plot = last_plot(), device = "pdf", 
       path =  NULL, scale = 1.5, width = 7.5 , height = 5,  
       units = c("in", "cm", "mm"), dpi = 800, limitsize = TRUE)




## PUN --

ggplot(data=df, aes(x=`TIME`, y=`PUN`))  +
  geom_point(size=0.5, color = "black") + 
  # geom_smooth(method="lm", aes(fill=`TIME`), color = "black")  + 
  geom_smooth(method="lm", color="gold", se=T) +
  # ggtitle(label = "Day-Ahead Price MW/h") +
  xlab(label= "") +
  ylab(label = "Euro per MW/h ")+
  theme_bw()
  # labs(caption = "(based on data from 'energidataservice - DK' and 'ENTSOE' ")


ggsave("Price", plot = last_plot(), device = "pdf", 
       path =  NULL, scale = 1.5, width = 7.5 , height = 5,  
       units = c("in", "cm", "mm"), dpi = 800, limitsize = TRUE)



## DEMAND -- 

ggplot(data=df, aes(x=`TIME`, y=(`DEM`))) +  
  geom_point(size=0.5, color = "black") + 
  # geom_smooth(method="lm", aes(fill=`TIME`)) +
  # facet_wrap(~year(TIME+365*0.5)) +
  geom_smooth(method="loess", span = 0.3, color="blue", se=FALSE) +
  #ylim(3.5e+06, 7.5e+06) +
  #ggtitle(label = "Demand Day-Ahead forecast ",
   #       subtitle = "Daily MW/h") +
  xlab(label= " ") +
  ylab(label = "MW/h")+
  theme_bw()
  # labs(caption = "(based on data from 'energidataservice - DK' and 'ENTSOE' ")



ggsave("Demand", plot = last_plot(), device = "pdf", 
       path =  NULL, scale = 1.5, width = 7.5 , height = 5,  
       units = c("in", "cm", "mm"), dpi = 800, limitsize = TRUE)



## PUN ~ Renewables --   

ggplot(data=df, aes(y=`PUN`, x=(`SOLAR`+`WIND`))) +  
  #ylim(200,1500) +
  geom_point(size=0.5) + 
  geom_smooth(method="lm", aes(fill=`TIME`)) + 
  # facet_wrap(~year(TIME)) +
  # geom_smooth(method="loess", color="green", se=FALSE) +
  # facet_wrap(~year(TIME)) 
  xlab(label = "Renewable Production Day-Ahead MW/h")+
  ylab(label = "Euro per MW/h")+
  theme_bw()



ggsave("Price ~Renewable Production", plot = last_plot(), device = "pdf", 
       path =  NULL, scale = 1.5, width = 7.5 , height = 5,  
       units = c("in", "cm", "mm"), dpi = 800, limitsize = TRUE)


## PUN ~ DEMAND --

ggplot(data=df, aes(y=`PUN`, x=(`DEM`))) +  
  # ylim(0,1500) +
  geom_point(size=0.5) + 
  geom_smooth(method="lm", aes(fill=`TIME`))+
  #ggtitle(label = "Price and Demand") +
  xlab(label= "Demand in MW/h") +
  ylab(label = "Euro per MW/h ")+
  theme_bw()
 #  labs(caption = "(based on data from 'energidataservice - DK' and 'ENTSOE' ")


ggsave("Price ~Demand", plot = last_plot(), device = "pdf", 
       path =  NULL, scale = 1.5, width = 7.5 , height = 5,  
       units = c("in", "cm", "mm"), dpi = 800, limitsize = TRUE)





## tabplot idea
# dont really get the idea..

install.packages("tabplot")
library("tabplot")
library("MASS")

tableplot(df[, -1], colorNA = "red", colorNA_num = "red")
summary(df)







