###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 1:                             ###
###                    DATA INPUT AND INITIALIZATION                    ###
###                                                                     ###
###########################################################################
###########################################################################
# Helpful code for generating banner comments
# banner("Section 1:", "Data inpsdafasdut and initialization", emph = TRUE)

setwd('C:/Users/seanw/Dropbox/ECON430SeanWeenink/R/ECON430')

# Define required packages
packages <- c("bannerCommenter", "rlang", "gridExtra", "ggpubr", "readxl", "lpirfs", "dplyr", "fredr", "tidyr", "tidyverse", "zoo", "forecast")
# Install packages and load libraries
install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

# Getting data from FRED:
StartDate = as.Date("1994-01-01") # Start date of the BRW shock series
EndDate = as.Date("2019-09-01") # End date of the BRW shock series\
fredr_set_key("2bf2ff8f666611f4f30b27f3feca9a38")
# Additional series can be added here
FREDCodes <- c("INDPRO", "EMRATIO", "UNRATE", "LNS12300012", "CPIAUCSL",  "PPIACO", "GS1")
# INDPRO: Industrial Production: Total Index (Index, Monthly, Seasonally Adjusted)
# EMPRATE: Employment Rate: Aged 15-64: All Persons for the United States (Percent, Monthly, Seasonally Adjusted)
# UNRATE: Unemployment Rate (Percent, Monthly, Seasonally Adjusted)
# CPIAUCSL: Consumer Price Index for All Urban Consumers: All Items in U.S. City Average (Index, Monthly, Seasonally Adjusted)
# PPIACO: Producer Price Index by Commodity: All Commodities (Index, Monthly, Not Seasonally Adjusted)
# GS1: 1-Year Treasury Constant Maturity Rate (Percent, Monthly, Not Seasonally Adjusted)
FREDData <- as.data.frame(lapply(FREDCodes, fredr, observation_start = StartDate ,observation_end = EndDate ))
FREDData <- FREDData %>% 
  dplyr::select("date", "value", "value.1", "value.2", "value.3", "value.4", "value.5", "value.6")
colnames(FREDData) <- c("Date", "INDPRO", "EMRATIO", "UNRate", "Youth", "CPIAUCSL",  "PPIACO", "GS1")

# Importing the BRW shock data
BRWShocks <- read.csv('./ShocksData/brw-shock-series.csv')
BRWShocks <- BRWShocks %>% 
  dplyr::select("month", "BRW_monthly")
BRWShocks <- as.data.frame(BRWShocks)
BRWShocks <- na.omit(BRWShocks)



###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 2:                             ###
###                           GENERATING IRFS                           ###
###                                                                     ###
###########################################################################
###########################################################################

IRF <- function(endog_data, shock){
# Endogenous data
endog_data <- as.data.frame(endog_data)
# Shock variable
shock <- as.data.frame(shock)
# Estimate linear model
results_lin_iv <- lp_lin_iv(endog_data = endog_data, lags_endog_lin = 4,
                            shock = shock, trend = 0,
                            confint = 1.96, hor = 36)
# Make and save linear plots

iv_lin_plots <- plot_lin(results_lin_iv)
iv_lin_plots
}


# BRW EMRATIO
#png('./Output/BRW_EMRATIOStdShock.png')
#IRF(FREDData$EMRATIO,BRWShocks$stdShock)
#dev.off()  

#png('./Output/BRW_INDPRO.png')
#IRF(FREDData$INDPRO,BRWShocks$BRW_monthly)
#dev.off()  

#png('./Output/BRW_UNRATE.png')
#IRF(FREDData$UNRATE,BRWShocks$BRW_monthly)
#dev.off()  

#png('./Output/BRW_CPIAUCSL.png')
#IRF(FREDData$CPIAUCSL,BRWShocks$BRW_monthly)
#dev.off()  

#png('./Output/BRW_PPIACO.png')
#IRF(FREDData$PPIACO,BRWShocks$BRW_monthly)
#dev.off()


png('./Output/BRW_Youth.png')
IRF(FREDData$Youth,BRWShocks$BRW_monthly)
dev.off()














