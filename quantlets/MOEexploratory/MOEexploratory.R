###############################################################################     
####    MOEexploratory.R    ###################################################    
###############################################################################     
#
# Loads data set from 'MOEmergedata'. 
# Creates exploratory plots of the variables.
#
# Input:  '.Rdata' file from the 'MOEmergedata' Quantlet.
#
# Ouput:  MOEex_plots.tex       - plots in .tex format
#
###############################################################################


# Clear all variables.
rm(list = ls(all = TRUE))
graphics.off()

# Install and load libraries.
libraries = c("ggplot2", "tikzDevice", "cowplot")
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
####    2.  CREATE PLOTS      #################################################
###############################################################################


# Solar Production plot
plot1 = ggplot(data=df ,aes(x=TIME, y=SOLAR)) +  
          geom_point(size=0.5) + 
          geom_smooth(method="loess", span = 0.25, color="orange", se=FALSE) + 
          ggtitle(label = "Solar Energy Production per Day", 
                 subtitle = "German and Austrian Production in MW/h")+
          xlab(label= " ") +
          ylab(label = "MW/h")+
          theme_bw()+
          labs(caption = "(Data from 'Netztransparenz' and 'APG') ")


# Wind Production plot
plot2 = ggplot(data=df, aes(x=`TIME`, y=(`WIND`))) +  
          geom_point(size=0.5) + 
          geom_smooth(method="lm", span = 1.5, color="blue", se=T) + 
          ggtitle(label = "Wind Energy Production per Day", 
                subtitle = "German and Austrian Production in MW/h") +
          xlab(label= " ") +
          ylab(label = "MW/h")+
           theme_bw()+
          labs(caption = "(Data from 'Netztransparenz' and 'APG')")


# Price per MW/h plot
plot3 = ggplot(data=df, aes(x=`TIME`, y=`PUN`)) + 
          geom_point(size=0.5, color = "black") + 
          #geom_smooth(method="lm", aes(fill=`TIME`), color = "black")  + 
          geom_smooth(method="lm", color="gold", se=T) +
          ggtitle(label = "Price Day-Ahead",
                  subtitle = "Euro per MW/h") +
          xlab(label= "") +
          ylab(label = "Euro per MW/h ")+
           theme_bw() +
          labs(caption = "(Data from 'energidataservice - DK' and 'ENTSOE')")


# Demand plot
plot4 = ggplot(data=df, aes(x=`TIME`, y=(`DEM`))) +  
           geom_point(size=0.5, color = "black") + 
          # geom_smooth(method="lm", aes(fill=`TIME`)) +
          # facet_wrap(~year(TIME+365*0.5)) +
          geom_smooth(method="loess", span = 0.3, color="blue", se=FALSE) +
          #ylim(3.5e+06, 7.5e+06) +
          ggtitle(label = "Demand Day-Ahead forecast ",
                 subtitle = "Daily MW/h") +
          xlab(label= " ") +
          ylab(label = "MW/h")+
          theme_bw()+
          labs(caption = "(Data from 'energidataservice - DK' and 'ENTSOE')")


# Price on renewables plot
 plot5 = ggplot(data=df, aes(y=`PUN`, x=(`SOLAR`+`WIND`))) +  
          #ylim(200,1500) +
          geom_point(size=0.5) + 
          geom_smooth(method="lm", aes(fill=`TIME`)) + 
          ggtitle(label = "Price on Renewables",
                  subtitle = "Correlation between Price (€)
                  and Renewbles Production (Daily MW/h") +
          xlab(label = "Renewable Production Day-Ahead MW/h")+
          ylab(label = "Euro per MW/h")+
          theme_bw()

 
 # Price on demand plot
plot6 = ggplot(data=df, aes(y=`PUN`, x=(`DEM`))) +  
          #ylim(0,1500) +
          geom_point(size=0.5) + 
          geom_smooth(method="lm", aes(fill=`TIME`))+
          ggtitle(label = "Price on Demand",
                  subtitle = "Correlation between Price (€)
                  and Demand (Daily MW/h") +
          #ggtitle(label = "Price and Demand") +
          xlab(label= "Demand in MW/h") +
          ylab(label = "Euro per MW/h ")+
          theme_bw()



###############################################################################
####    3.  SAVE PLOTS AS TEX FILE      #######################################
###############################################################################


# Save variable plot for LaTex
tikz(file = "MOEexploratory/MOEexpl_plots.tex", width = 5, height = 5)
plot_grid(plot1, plot2, plot3, plot4, align= "hv")
dev.off()

# Save correlation plot for LaTex
tikz(file = "MOEexploratory/MOEcorr_plots.tex", width = 5, height = 5)
plot_grid(plot6, plot5, align= "hv")
dev.off()



###############################################################################
####    4. CLEAN UP ENVIRONMENT    ############################################
###############################################################################


# Remove everything except for "df" from environment.
#rm(list=ls()[! ls() %in% c("df")]) 





## COMMENT:
# TODO
# make y axis more similar with units
# check why tex is not working on my mac

### Test ####
ggplot(data=df ,aes(TIME)) +
  geom_point(aes(y = SOLAR, color = "SOLAR"))+
  geom_point(aes(y = WIND, color = "WIND"))+
  scale_colour_manual(values=c("orange", "blue"))
 


