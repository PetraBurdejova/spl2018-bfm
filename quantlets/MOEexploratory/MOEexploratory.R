###############################################################################     
####    MOEexploratory.R    ###################################################    
###############################################################################     
#
# Loads data set from 'MOEmergedata'. 
# Creates exploratory plots of the variables.
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




###############################################################################
####    2.1.  SOLAR GRAPH      ################################################
###############################################################################

tikz(file = "solar.tex", width = 5, height = 5)

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


###############################################################################
####    2.2.  WIND GRAPH      #################################################
###############################################################################

tikz(file = "wind.tex", width = 5, height = 5)

plot = ggplot(data=df, aes(x=`TIME`, y=(`WIND`))) +  
            geom_point(size=0.5) + 
            geom_smooth(method="lm", span = 1.5, color="blue", se=T) + 
            ggtitle(label = "Wind Energy Production per Day", 
                  subtitle = "German and Austrian Production in MW/h") +
            xlab(label= " ") +
            ylab(label = "MW/h")+
            theme_bw()
           labs(caption = "(based on data from 'Netztransparenz' and 'APG' ")

print(plot)

dev.off()


###############################################################################
####    2.3.  PRICE GRAPH      ################################################
###############################################################################

tikz(file = "price.tex", width = 5, height = 5)

ggplot(data=df, aes(x=`TIME`, y=`PUN`))  +
  geom_point(size=0.5, color = "black") + 
 # geom_smooth(method="lm", aes(fill=`TIME`), color = "black")  + 
  geom_smooth(method="lm", color="gold", se=T) +
  ggtitle(label = "Day-Ahead Price MW/h") +
  xlab(label= "") +
  ylab(label = "Euro per MW/h ")+
  theme_bw() +
  labs(caption = "(based on data from 'energidataservice - DK' and 'ENTSOE' ")

print(plot)

dev.off()



###############################################################################
####    2.4.  DEMAND GRAPH      ###############################################
###############################################################################

tikz(file = "demand.tex", width = 5, height = 5)

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



###############################################################################
####    2.5.  PRICE ON RENEWABLES GRAPH      ##################################
###############################################################################

tikz(file = "price-renewables.tex", width = 5, height = 5)

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



###############################################################################
####    2.6.  PRICE ON DEMAND GRAPH      ######################################
###############################################################################

tikz(file = "price-demand.tex", width = 5, height = 5)

ggplot(data=df, aes(y=`PUN`, x=(`DEM`))) +  
  # ylim(0,1500) +
  geom_point(size=0.5) + 
  geom_smooth(method="lm", aes(fill=`TIME`))+
  #ggtitle(label = "Price and Demand") +
  xlab(label= "Demand in MW/h") +
  ylab(label = "Euro per MW/h ")+
  theme_bw()
 #  labs(caption = "(based on data from 'energidataservice - DK' and 'ENTSOE' ")







