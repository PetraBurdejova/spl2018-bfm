###############################################################################     
####    MOEexploratory.R    ###################################################    
###############################################################################     
#
# Creates exploratory plots of energy market variables.
#
# Input:  '.Rdata' file from the 'MOEmergedata' and 'MOErawdata' Quantlet.
#
# Output:  MOEplot_expl.tex       
#          MOEplot_expl.png       
#          MOEplot_corr.tex       
#          MOEplot_corr.png       
#          MOEtrend_solar.tex     
#          MOEtrend_solar.png     
#          MOEtrend_demand.tex    
#          MOEtrend_demand.png    
#
###############################################################################

# Clear all variables.
rm(list = ls(all = TRUE))
graphics.off()

# Install and load libraries.
libraries = c("tidyr", "ggplot2", "tikzDevice", "cowplot", "scales")
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

# Grab data from 'MOEmergedata' and 'MOErawdata' Quantlet
load("MOErawdata/MOEdata_clean.Rdata")
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

# Create ENERGY over TIME plot
plot_pwr = ggplot(data=tidy.df, aes(x = TIME, y = ENERGY)) +
    geom_point(size = 0.5) +
    ggtitle(label = "The German and Austrian Energy Market",
            subtitle = "Selected Day-Ahead Variables, 2015--2017") +
    xlab(label="") +
    ylab(label = "Energy in MWh") +
    scale_y_continuous(label = comma) +
    theme_bw() +
    facet_grid(VAR ~ ., scales = "free")


# Create PRICE over TIME plot
plot_pun = ggplot(data=tidy.df, aes(x = TIME, y = PRICE)) +
    geom_point(size = 0.5) +
    labs(x = "", y = "Price in Euro/MWh") +
    scale_y_continuous(label = comma) +
    theme_bw() +
    facet_grid(LAB ~ ., scales = "free")


# Bind plots together
plot_exp = plot_grid(plot_pwr, plot_pun, align = "v", nrow = 2,
                     rel_heights = c(1, 0.39)
                     )


###############################################################################
####    2.2 CORRELATION PLOT    ###############################################

# Create PRICE over ENERGY plot
plot_pr_rn = ggplot(data= tidy.df[tidy.df$VAR != "DEMAND", ], 
                    aes(y = PRICE, x = ENERGY)) +
        geom_point(size=0.5) +
        geom_smooth(method="lm", aes(fill= TIME), fullrange = T) +
        ggtitle(label = "Co-movements on the Energy Market 2015--2017",
                subtitle = "Selected Day-Ahead Variables") +
        xlab(label = "") +
        ylab(label = "Price in Euro/MWh") +
        facet_grid(VAR ~., scales = "free") +
        theme_bw() 


# Create PRICE over DEMAND plot
plot_pr_de = ggplot(data= tidy.df[tidy.df$VAR == "DEMAND", ], 
                    aes(y = PRICE, x = ENERGY)) +
        geom_point(size=0.5) +
        geom_smooth(method="lm", aes(fill=`TIME`),fullrange = T) +
        xlab(label = "Energy in MWh") +
        ylab(label = "Price in Euro/MWh") +
        theme_bw() +
        facet_grid(VAR ~., scales = "free")


# Bind plots together
plot_corr = plot_grid(plot_pr_rn, plot_pr_de, align = "v", nrow = 2,
                                          rel_heights = c(0.7, 0.4))


###############################################################################
####    2.3 SOLAR TREND    ####################################################

trend_solar = ggplot(data=df.solar, aes(x = TIME, y = FzHertz)) +
    geom_point(size = 0.5) +
    labs(x = "", y = "Production in MWh") +
    ggtitle(label = "Example of Daily Pattern in Solar Energy Generation") +
    scale_x_datetime(limits = c(as.POSIXct('2016-07-11 23:30:00'),
                                as.POSIXct('2016-07-13 00:30:00')),
                     labels = date_format("%H:%M"),
                     breaks = date_breaks("3 hour"),
                     expand = c(0,0)) +
    scale_y_continuous(label = comma) +
    theme_bw()


###############################################################################
####    2.4  DEMAND TREND    ##################################################

trend_demand = ggplot(data=df.dm, aes(x = TIME, y = DEM)) +
    geom_point(size = 0.5) +
    labs(x = "", y = "Demand in MWh") +
    ggtitle(label = "Example of Weekly Pattern in Energy Demand") +
    scale_x_datetime(limits = c(as.POSIXct('2016-07-11 00:00:00'),
                                as.POSIXct('2016-08-01 23:45:00')),
                     labels = date_format("%A"),
                     breaks = date_breaks("7 day"),
                     expand = c(0,0)) +
    scale_y_continuous(label = comma) +
    theme_bw()


###############################################################################
####    3.  SAVE PLOTS AS TEX FILE      #######################################
###############################################################################

# Save explorative plot as .tex file
tikz(file = "MOEexploratory/MOEplot_expl.tex", width = 6, height = 8)
plot(plot_exp)
dev.off()

# Save correlation plot as .tex file
tikz(file = "MOEexploratory/MOEplot_corr.tex", width = 6, height = 8)
plot(plot_corr)
dev.off()

# Save solar trend plot as .tex file
tikz(file = "MOEexploratory/MOEtrend_solar.tex", width = 7, height = 3)
plot(trend_solar)
dev.off()

# Save demand trend plot as .tex file
tikz(file = "MOEexploratory/MOEtrend_demand.tex", width = 7, height = 3)
plot(trend_demand)
dev.off()

# Save explorative plot as .png file
png("MOEexploratory/MOEplot_expl.png", width = 512, height = 740)
plot(plot_exp)
dev.off()

# Save correlation plot as .png file
png("MOEexploratory/MOEplot_corr.png", width = 512, height = 740)
plot(plot_corr)
dev.off()

# Save solar_trend plot as .png file
png("MOEexploratory/MOEtrend_solar.png", width = 512, height = 220)
plot(trend_solar)
dev.off()

# Save demand trend plot as .png file
png("MOEexploratory/MOEtrend_demand.png", width = 512, height = 220)
plot(trend_demand)
dev.off()




###############################################################################
####    4. CLEAN UP ENVIRONMENT    ############################################
###############################################################################

rm(list=ls()[! ls() %in% c("df", 
                           "plot_exp", 
                           "plot_corr", 
                           "trend_solar", 
                           "trend_demand"
                           )])

