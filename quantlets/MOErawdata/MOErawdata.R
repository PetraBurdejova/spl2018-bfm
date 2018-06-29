###############################################################################     
####    MOErawdata.R    #######################################################     
###############################################################################     
#
# Energy market data is read in using 'readr'and  formatted using the 
# 'lubridate' and 'tidyr' packages. Cleaned <dataframes> are saved as .csv and 
# .Rdata files. Working directory may need to be set corectly (see section 0).
#
# Input:  Data '.csv' files from the 'input' subdirectory.
#
# Ouput:  output-<variable_name>.csv    - data in table form
#         output-raw.Rdata              - data in Rdata form
#
###############################################################################

# Clear all variables.
rm(list = ls(all = TRUE))
graphics.off()

# Install and load libraries.
libraries = c("tidyr", "stringr", "lubridate", "readr")
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
####    1.  READ ENERGY MARKET DATA    ########################################
###############################################################################


# Load price data.
df.pun.0        =  read.csv("MOErawdata/inputs/Elspot_Prices_Data-5375228caa4c48ad9b969f250d70fe2e.csv")

# Load renewables DE data (solar and wind are available in seperate files).
df.solar.D      =  read.csv2("MOErawdata/inputs/Solarenergie_DE.csv")
df.wind.D       =  read.csv2("MOErawdata/inputs/Windenergie_DE.csv")

# Load renewables AT data (solar.AT and wind.AT are separate columns in one file).
df.ren.AT1      =  read.csv2("MOErawdata/inputs/AT - renewables/export_daftg_2015-01-01T00_00_00Z_2015-06-30T23_45_00Z_60M_de.csv", header = F)
df.ren.AT2      =  read.csv2("MOErawdata/inputs/AT - renewables/export_daftg_2015-07-01T00_00_00Z_2015-12-31T23_45_00Z_60M_de.csv", header = F)
df.ren.AT3      =  read.csv2("MOErawdata/inputs/AT - renewables/export_daftg_2016-01-01T00_00_00Z_2016-06-30T23_45_00Z_60M_de.csv", header = F)
df.ren.AT4      =  read.csv2("MOErawdata/inputs/AT - renewables/export_daftg_2016-07-01T00_00_00Z_2016-12-31T23_45_00Z_60M_de.csv", header = F)
df.ren.AT5      =  read.csv2("MOErawdata/inputs/AT - renewables/export_daftg_2017-01-01T00_00_00Z_2017-06-30T23_45_00Z_60M_de.csv", header = F)
df.ren.AT6      =  read.csv2("MOErawdata/inputs/AT - renewables/export_daftg_2017-07-01T00_00_00Z_2017-12-31T23_45_00Z_60M_de.csv", header = F)
df.ren.AT7      =  read.csv2("MOErawdata/inputs/AT - renewables/export_daftg_2018-01-01T00_00_00Z_2018-06-04T23_45_00Z_60M_de.csv", header = F)

# Load demand data.
df.dem.2015.0   = read.csv("MOErawdata/inputs/Total Load - Day Ahead _ Actual_201501010000-201601010000.csv")
df.dem.2016.0   = read.csv("MOErawdata/inputs/Total Load - Day Ahead _ Actual_201601010000-201701010000.csv")
df.dem.2017.0   = read.csv("MOErawdata/inputs/Total Load - Day Ahead _ Actual_201701010000-201801010000.csv")
df.dem.2018.0   = read.csv("MOErawdata/inputs/Total Load - Day Ahead _ Actual_201801010000-201901010000.csv")



###############################################################################
####    2.  CLEAN AND FORMAT    ###############################################
###############################################################################


###############################################################################
####    2a. SINGLE DATAFRAMES (df.pun, df.solar, df.wind)    ################## 
###############################################################################

# Remove unwanted columns. Merge date and hour columns. Columns are selected by
# their position and not by their name to prevent parsing errors. (mpff)
# Example: Use 'select = names(df)[c(1)]' instead of 'select = "Date"'.
df.pun    = subset(df.pun.0, select = names(df.pun.0)[c(1,5)])

df.solar  = subset(df.solar.D, select = names(df.solar.D)[-3])
df.solar  = unite(df.solar, TIME, names(df.solar)[c(1,2)], sep = " ")

df.wind   = subset(df.wind.D, select = names(df.wind.D)[-3])
df.wind   = unite(df.wind, TIME, names(df.wind)[c(1,2)], sep = " ")

# Change column names.
names(df.pun)   = c("TIME", "PUN")
names(df.solar) = c("TIME", "FzHertz", "Amprion", "TenneT.TSO", "Transnet.BW")
names(df.wind)  = c("TIME", "FzHertz", "Amprion", "TenneT.TSO", "Transnet.BW")

# Format as POSIXct time.
df.pun$TIME     = ymd_hm(df.pun$TIME)
df.solar$TIME   = dmy_hm(df.solar$TIME)
df.wind$TIME    = dmy_hm(df.wind$TIME)


###############################################################################
####    2b. MULTIPLE DATAFRAMES (df.dm, df.solar.AT, df.wind.AT)    ###########
###############################################################################

###############################################################################
####    DEFINE SUBROUTINES    #################################################

select.ATSOLAR = function(x){
  # Selects the important variables for the ren.AT data
  #
  # Args:
  #   x: Imported raw dataframe
  #
  # Returns:
  #   y: Corrected solar.AT dataframe
  y                 = subset(x, select = names(x)[c(1, 7)])
  names(y)          = c("TIME", "SOLAR.MW.AT")
  y$`SOLAR.MW.AT`   = as.numeric(y$`SOLAR.MW.AT`)
  y$TIME            = dmy_hms(y$TIME)
  return(y)
}

select.ATWIND = function(x){
  # Selects the important variables for the ren.AT data
  #
  # Args:
  #   x: Imported raw dataframe
  #
  # Returns:
  #   y: Corrected wind.AT dataframe
  y                 = subset(x, select = names(x)[c(1,7)])
  names(y)          = c("TIME", "WIND.MW.AT")
  y$`WIND.MW.AT`    = as.numeric(y$`WIND.MW.AT`)
  y$TIME            = dmy_hms(y$TIME)
  return(y)
}

select.DEM = function(x) {
  # Selects the important variables for the demand data
  # Also checks if variable is factor or not
  # Args:
  #   x: Imported raw dataframe
  #
  # Returns:
  #   y: Selection of demand dataframes
  y         = subset(x, select = names(x)[c(1,2)])
  names(y)  = c("TIME", "DEM")
  y         = separate(y, col = TIME, into = c("TIME","bis"), sep =  " - ")
  y         = subset(y, select = c("TIME","DEM"))
  y$TIME    = dmy_hm(y$TIME)
  # y$TIME = dmy_hm(y$TIME, tz = "CET")
  # y$TIME = with_tz(y$TIME, tz="UTC")  
  if (class(y$DEM) == "factor") {
    y$DEM   = as.numeric(levels(y$DEM))[y$DEM]
    return(y)
  } else {
    y$DEM   = as.numeric(y$DEM)
    return(y)
  }  
} 

###############################################################################
####    APPLY SUBROUTINES    ##################################################

df.solar.AT1    = select.ATSOLAR(df.ren.AT1)
df.solar.AT2    = select.ATSOLAR(df.ren.AT2)
df.solar.AT3    = select.ATSOLAR(df.ren.AT3)
df.solar.AT4    = select.ATSOLAR(df.ren.AT4)
df.solar.AT5    = select.ATSOLAR(df.ren.AT5)
df.solar.AT6    = select.ATSOLAR(df.ren.AT6)
df.solar.AT7    = select.ATSOLAR(df.ren.AT7)

df.wind.AT1     = select.ATWIND(df.ren.AT1)
df.wind.AT2     = select.ATWIND(df.ren.AT2)
df.wind.AT3     = select.ATWIND(df.ren.AT3)
df.wind.AT4     = select.ATWIND(df.ren.AT4)
df.wind.AT5     = select.ATWIND(df.ren.AT5)
df.wind.AT6     = select.ATWIND(df.ren.AT6)
df.wind.AT7     = select.ATWIND(df.ren.AT7)

df.dem.2015     = select.DEM(df.dem.2015.0)
df.dem.2016     = select.DEM(df.dem.2016.0)
df.dem.2017     = select.DEM(df.dem.2017.0)
df.dem.2018     = select.DEM(df.dem.2018.0)



###############################################################################
####    3. BIND AND SAVE DATAFRAMES    ########################################
###############################################################################

# Bind dataframes.
df.dm       = rbind(df.dem.2015,df.dem.2016,df.dem.2017,df.dem.2018)
df.solar.AT = rbind(df.solar.AT1, df.solar.AT2,df.solar.AT3,df.solar.AT4,
                    df.solar.AT5,df.solar.AT6,df.solar.AT7
                    )
df.wind.AT  = rbind(df.wind.AT1,df.wind.AT2,df.wind.AT3,df.wind.AT4,
                    df.wind.AT5,df.wind.AT6,df.wind.AT7
                    )

# Save dataframes as '.Rdata' file for easy read-in in R.
save(df.pun, df.solar, df.solar.AT, df.wind, df.wind.AT, df.dm,
     file="MOErawdata/output-raw.Rdata"
     )

# Save dataframes as '.csv' files for use with other software.
####    TODO: Save dataframes as .csv



###############################################################################
####    4. CLEAN UP ENVIRONMENT    ############################################
###############################################################################

rm(list=ls()[! ls() %in% c("df.pun",
                           "df.solar",
                           "df.solar.AT",
                           "df.wind", 
                           "df.wind.AT",
                           "df.dm"
                           )]) 
