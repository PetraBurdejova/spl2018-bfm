
###############################################################################     
####    MOEplotNAs.R    #######################################################     
###############################################################################     
#
# Creates exploratory plots of energy market variables.
#
# Input:  '.Rdata' file from the 'MOErawdata' Quantlet.
#
# Output:  MOEplot_na.tex       - plot in .tex format
#          MOEplot_na.pdf       - plot in .pdf format
#
###############################################################################

# Clear all variables.
rm(list = ls(all = TRUE))
graphics.off()

# Install and load libraries.
libraries = c("ggplot2", "tikzDevice", "scales")
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
####    2.  DEFINE FUNCTIONS   ################################################
###############################################################################


DiagMissingValues <- function(df, dlevel = 0) {
  # Checks for NA values in dataframe and prints information.
  #
  # Args:
  #   df: The dataframe that will be checked for missing values.
  #   dlevel: Print more detailed information. 0: No information.
  #           1: General information. 2: Information on columns.
  #
  # Returns:
  #   A <dataframe> of timepoints of incomplete cases in df.

  name <- deparse(substitute(df))  # get name of dataframe

  r <- is.na(df)
  cc <- complete.cases(df)

  if (dlevel >= 1) {
    # dlevel 1 diagnostics: general information.
    cat(sprintf("------ NA Diagnostics for '%s' ------\n", name))
    cat(sprintf("Number of complete cases: %i of %i.\n", 
                sum(cc), nrow(df) ))
    cat(sprintf("Number of incomplete cases: %i (%.3f%%).\n",
                (nrow(df)-sum(cc)), (1-sum(cc)/nrow(df))*100) )
    print(apply(r, 2, sum))
  }

  if (dlevel >= 2) {
    # dlevel 2 diagnostics: information per column.
    i <- 1
    for (col in names(df)){
      cat(sprintf("NA's in at least %i columns: %i\n",
                  i, sum(apply(r, 1, sum) >= i) ))
      i <- i+1
    }

  }

  # Get timepoints of incomplete cases and output dataframe
  tp <- subset(df$TIME, complete.cases(df) == FALSE)
  tp <- as.data.frame(tp)
  names(tp) <- "TIME"

  return(tp)
}



###############################################################################
####    3.  CREATE NA PLOT   ##################################################
###############################################################################


df.na.dm            = data.frame(DiagMissingValues(df.dm), "DEMAND")
names(df.na.dm)     = c("TIME", "SOURCE")

df.na.solar         = data.frame(DiagMissingValues(df.solar), "SOLAR")
names(df.na.solar)  = c("TIME", "SOURCE")

df.na.wind          = data.frame(DiagMissingValues(df.wind), "WIND")
names(df.na.wind)   = c("TIME", "SOURCE")

df.na.raw           = rbind(df.na.dm,df.na.solar,df.na.wind)


plot_na = ggplot(df.na.raw) +
    geom_histogram(aes(x=TIME, col=SOURCE, fill=SOURCE),
                   alpha = 0.8, binwidth = 24*3600) +
    labs(x = "Date", y = "NAs per day") +
    scale_x_datetime(limits=c(as.POSIXct('2015-01-01 00:00:00'),
                              as.POSIXct('2018-01-01 00:00:00'))) +
    ggtitle(label = "Missing Values in the Dataset")

###############################################################################
####    4.  SAVE PLOTS AS TEX FILE      #######################################
###############################################################################


# Save explorative plot as .tex file
tikz(file = "MOEplotNAs/MOEplot_na.tex", width = 8, height = 3)
plot(plot_na)
dev.off()

# Save explorative plot as .pdf file
pdf("MOEplotNAs/MOEplot_na.pdf", width = 8, height = 3)
plot(plot_na)
dev.off()



###############################################################################
####    5. CLEAN UP ENVIRONMENT    ############################################
###############################################################################


rm(list=ls()[! ls() %in% c("df.pun",
                           "df.solar",
                           "df.solar.AT",
                           "df.wind", 
                           "df.wind.AT",
                           "df.dm",
                           "plot_na"
                           )])

