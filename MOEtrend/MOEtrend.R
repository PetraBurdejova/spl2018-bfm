###############################################################################     
####    MOEtrend.R    ########################################################     
###############################################################################     
#
# Creates exploratory plots of the NA-value structure in the energy 
# market variables.
#
# Input:  '.Rdata' file from the 'MOErawdata' Quantlet.
#
# Output:  MOEtrend_solar.tex       - plot in .tex format
#          MOEtrend_solar.pdf       - plot in .pdf format
#          MOEtrend_demand.tex      - plot in .tex format
#          MOEtrend_demand.pdf      - plot in .pdf format
#
###############################################################################

# Clear all variables.
rm(list = ls(all = TRUE))
graphics.off()

# Install and load libraries.
libraries = c("ggplot2", "tikzDevice", "scales", "lubridate")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)



###############################################################################
####    0.  SET WORKING DIRECTORY    ##########################################
###############################################################################
####    ATTENTION: Working directory is assumed to be the root of the MOE 
####    repository, not the MOErawdata Quantlet subdirectory!!!


# If needed, set working directory accordingly:
#setwd("path/to/MOE_repository")



###############################################################################
####    1.  LOAD ENERGY MARKET DATA    ########################################
###############################################################################


load("MOErawdata/MOEdata_clean.Rdata")


###############################################################################
####    2. SOLAR TREND    #####################################################
###############################################################################

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

# Save explorative plot as .tex file
tikz(file = "MOEtrend/MOEtrend_solar.tex", width = 7, height = 3)
plot(trend_solar)
dev.off()

# Save explorative plot as .pdf file
pdf("MOEtrend/MOEtrend_solar.pdf", width = 7, height = 3)
plot(trend_solar)
dev.off()


###############################################################################
####    3.  DEMAND TREND    ###################################################
###############################################################################

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

# Save explorative plot as .tex file
tikz(file = "MOEtrend/MOEtrend_demand.tex", width = 7, height = 3)
plot(trend_demand)
dev.off()

# Save explorative plot as .pdf file
pdf("MOEtrend/MOEtrend_demand.pdf", width = 7, height = 3)
plot(trend_demand)
dev.off()
