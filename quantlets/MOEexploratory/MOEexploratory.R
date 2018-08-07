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
libraries = c("tidyr", "dplyr", "ggplot2", "tikzDevice", "cowplot", "scales")
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

# Change variable names for better representation
names(df) <- c("TIME", "PRICE", "DEMAND", "SOLAR", "WIND")

# Create tidy datasets
tidy.df = df %>% gather(key = VAR, value = ENERGY, 3:ncol(df))
tidy.df = tidy.df %>% gather(key = LAB, value = PRICE, 2)


###############################################################################
####    2.  CREATE PLOTS      #################################################
###############################################################################


###############################################################################
####    2.1 EXPLORATORY PLOT    ###############################################

plot_pwr = ggplot(data=tidy.df, aes(x = TIME, y = ENERGY)) +
    geom_point(size = 0.5) +
    ggtitle(label = "The German and Austrian Energy Market",
            subtitle = "Selected Day-Ahead Variables, 2015--2018") +
    xlab(label="") +
    ylab(label = "Energy in MWh") +
    scale_y_continuous(label = comma) +
    theme_bw() +
    facet_grid(VAR ~ ., scales = "free")

plot_pun = ggplot(data=tidy.df, aes(x = TIME, y = PRICE)) +
    geom_point(size = 0.5) +
    labs(x = "", y = "Price in Euro/MWh") +
    scale_y_continuous(label = comma) +
    theme_bw() +
    facet_grid(LAB ~ ., scales = "free")

plot_exp = plot_grid(plot_pwr, plot_pun, align = "v", nrow = 2,
                     rel_heights = c(1, 0.39)
                     )


###############################################################################
####    2.2 CORRELATION PLOT    ###############################################

# Price on renewables plot
 plot5 = ggplot(data=df, aes(y=PRICE, x=(`SOLAR`+`WIND`))) +  
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
plot6 = ggplot(data=df, aes(y=PRICE, x=(DEMAND))) +  
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


tikz(file = "MOEexploratory/MOEplot_expl.tex", width = 6, height = 8)
plot(plot_exp)
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
