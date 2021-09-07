###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 1:                             ###
###                    DATA INPUT AND INITIALIZATION                    ###
###                                                                     ###
###########################################################################
###########################################################################
# Helpful code for generating banner comments
# banner("Section 1:", "Data input and initialization", emph = TRUE)
asdfasdfasdfasfdasdf
setwd('C:/Users/seanw/Dropbox/ECON430SeanWeenink/R')

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
  FREDCodes <- c("EMRATIO")
  FREDData <- as.data.frame(lapply(FREDCodes, fredr, observation_start = StartDate ,observation_end = EndDate, frequency="q" ))
  
EMRATIO <- FREDData %>% 
  dplyr::select("date", "value")

colnames(FREDData) <- c("Date", "EMRATIO")


  
# Importing nominal GDP
FREDCodes <- c("GDPC1")
# Repeat to get the unemployment rates (since they are monthly)
FREDData <- as.data.frame(lapply(FREDCodes, fredr, observation_start = StartDate ,observation_end = EndDate ))

GDPC1 <- FREDData %>% 
  dplyr::select("date", "value")

library(zoo)
GDPC1 <- GDPC1 %>% 
  mutate(growth = c(0, 100*diff(log(GDPC1$value))),
         )

# Caculating a 3 period moving mean and subtracting 0.5 such that the switching variable is centered on 0
GDPC1 <- GDPC1 %>% 
  mutate(
    MA = c(0,0, rollmean(GDPC1$growth, 3))
  )

ggplot(GDPC1, aes(x=date, y= MA)) +
  geom_line()

#colnames(EMRATIO) <- c("Date", "GDPCA")

# Calculating a moving average of GDP growth rate




# Importing the BRW shock data
BRWShocks <- read.csv('./ShocksData/brw-shock-series.csv')
BRWShocks <- BRWShocks %>% 
  dplyr::select("month", "BRW_monthly")
BRWShocks <- as.data.frame(BRWShocks)
BRWShocks <- na.omit(BRWShocks)


# WORKINGGGGG

# Convert BRW to ts and aggregate monthly shocks to quarterly 
# When ordering make sure to use the index not the month column since it orders incorrectly
BRW_M <- ts(BRWShocks, start = c(1994, 1), frequency = 12)
BRW_Q <- aggregate(BRW_M, nfrequency = 4)

# Creating a 100 bp standard deviation

BRW_Q <- BRW_Q %>% 
  mutate(stdShock = BRW_monthly/sd(BRW_monthly))


# View 
plot(BRW_M)
plot(BRW_Q)

# Convert back to data frame for use in IRF function 
BRW_Q <- as.data.frame(BRW_Q)




###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 2:                             ###
###                           GENERATING IRFS                           ###
###                                                                     ###
###########################################################################
###########################################################################

IRF_NL <- function(endog_data, shock, exog_data, switching_variable, horizon){
  # Endogenous data
  endog_data <- as.data.frame(endog_data)
  exog_data <- as.data.frame(exog_data)
  # Shock variable
  switching_variable <- as.data.frame(switching_variable)
  shock <- as.data.frame(shock)
  # Estimate linear model
  results_lin_iv <-  lp_nl_iv(endog_data = endog_data, lags_endog_nl = 4,
                              shock = shock, exog_data = exog_data,
                              lags_exog = 7, trend = 0,
                              confint = 1.96, hor = horizon,
                              switching = switching_variable, use_hp = FALSE,
                              use_logistic = TRUE,
                              lag_switching = TRUE,
                              gamma = 10)

 #BRW dates for recessions
  # Make and save linear plots
  
 iv_lin_plots <- plot_nl(results_lin_iv)
 iv_lin_plots
 
}


# Working to include the plotting in a function

plots_nl_iv <- IRF_NL(EMRATIO$value, BRW_Q$stdShock, GDPC1$growth, GDPC1$MA)


## NOTE that the first state is the expansion period 


#png('./Output/NL_Expansion_3YStdShock.png')
#plots_nl_iv$gg_s1[[1]]
#dev.off()  

#png('./Output/NL_Recession_3YStdShock.png')
#plots_nl_iv$gg_s2[[1]]
#dev.off()  


# Potential switching variables
# Recession dates (Can we use a binary indicator)
# Output gap 
# Industrial production (sub part of the economy)

############################################################################
############################################################################
###                                                                      ###
###                              SECTION 2:                              ###
###            NON-LINEAR IRF USING PROXY SWITCHING VARIABLES            ###
###                                                                      ###
############################################################################
############################################################################

# Getting data from FRED:
StartDate = as.Date("1993-05-01") # Start date of the BRW shock series
EndDate = as.Date("2019-12-01") # End date of the BRW shock series\
fredr_set_key("2bf2ff8f666611f4f30b27f3feca9a38")
# Additional series can be added here

# Getting proxy variables
FREDCodes <- c("CPIAUCSL", "INDPRO")
FREDData <- as.data.frame(lapply(FREDCodes, fredr, observation_start = StartDate ,observation_end = EndDate, frequency="m" ))
Proxy <- FREDData %>% 
  dplyr::select("date", "value", "value.1")
colnames(Proxy) <- c("Date", "CPI", "INDPRO")

# Caculating a 7 period moving mean of the proxy variables using the two tailed approach

Proxy <- Proxy %>% 
  mutate(
    CPI_Growth = c(0, 100*diff(log(CPI))),
    INDPRO_Growth = c(0, 100*diff(log(INDPRO))),
  )



Proxy <- Proxy %>% 
  mutate(
    MA_CPI = ma(CPI_Growth, 7))

Proxy <- Proxy %>% 
  mutate(
    MA_INDPRO = ma(INDPRO_Growth,7)
  ) %>% 
  filter(
    Date > as.Date("1993-12-01"),
    Date < as.Date("2019-10-01")
  )




# Getting data from FRED:
StartDate = as.Date("1994-01-01") # Start date of the BRW shock series
EndDate = as.Date("2019-09-01") # End date of the BRW shock series\


# Getting monthy employment ratio
FREDCodes <- c("EMRATIO", "LNS12300060")
FREDData <- as.data.frame(lapply(FREDCodes, fredr, observation_start = StartDate ,observation_end = EndDate, frequency="m" ))
EMRATIO <- FREDData %>% 
  dplyr::select("date", "value", "value.1")

colnames(EMRATIO) <- c("Date", "EMRATIO", "Youth")
  
BRW_M <- as.data.frame(BRW_M)



# Results for CPI

plots_nl_iv <- IRF_NL(EMRATIO$Youth, BRW_M$BRW_monthly, Proxy$MA_CPI, Proxy$MA_CPI,36)


plots_nl_iv$gg_s1

plots_nl_iv$gg_s2


# IRF for CPI Expansion state
#png('./Output/NL_Expansion_CPI.png')
#plots_nl_iv$gg_s1
#dev.off() 

# IRF for CPI Recession Period 
#png('./Output/NL_Recession_CPI.png')
#plots_nl_iv$gg_s2
#dev.off()  

# Evolution of switching variable CPI
#png('./Output/SwitchingCPI.png')
#IRF_NL_HPFILTER(EMRATIO$value, BRW_M$BRW_monthly, Proxy$MA_CPI, Proxy$MA_CPI,36)
#dev.off() 

#######################################################################################
# Results for INDPRO

plots_nl_iv <- IRF_NL(EMRATIO$value, BRW_M$BRW_monthly, Proxy$MA_INDPRO, Proxy$MA_INDPRO,36)


plots_nl_iv$gg_s1

plots_nl_iv$gg_s2


# IRF for INDPRO Expansion state
#png('./Output/NL_Expansion_INDPRO.png')
#plots_nl_iv$gg_s1
#dev.off() 

# IRF for INDPRO Recession Period 
#png('./Output/NL_Recession_INDPRO.png')
#plots_nl_iv$gg_s2
#dev.off()  

# Evolution of switching variable INDPRO
#png('./Output/SwitchingINDPRO.png')
#IRF_NL_HPFILTER(EMRATIO$value, BRW_M$BRW_monthly, Proxy$MA_INDPRO, Proxy$MA_INDPRO,36)
#dev.off() 












