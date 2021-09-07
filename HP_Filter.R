# This shows the evolution of the regime-switching variable.
# Choosing a lower value of gamma makes the regime-switching smooth, wheras higher values of gamma cause the swithcing to be quick



IRF_NL_HPFILTER <- function(endog_data, shock, exog_data, switching_variable, horizon){
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
  
  points <- as.data.frame(c(seq(as.Date("1994-01-01"), as.Date("2019-02-01"), "months")))
  
  points <- points %>% 
    mutate(fz =  results_lin_iv$fz)
  
  colnames(points) <- c("Date","Value")
  
  ggplot(points, aes(x=Date, y=Value))+
    geom_line() +
    theme_minimal() +
    geom_rect(aes(xmin=as.Date("2001-04-01"), xmax=as.Date("2001-11-01"), ymin=0, ymax=1), color="grey", alpha=0.01)+
    geom_rect(aes(xmin=as.Date("2008-01-01"), xmax=as.Date("2009-7-01"), ymin=0, ymax=1), color="grey", alpha=0.01)
  
}


# Get recession dates NBER USREC
# I use these to show the shaded recession area 

setwd('C:/Users/seanw/Dropbox/ECON430SeanWeenink/R')


# Getting data from FRED:
StartDate = as.Date("1994-01-01") # Start date of the BRW shock series
EndDate = as.Date("2019-09-01") # End date of the BRW shock series\
fredr_set_key("2bf2ff8f666611f4f30b27f3feca9a38")
# Additional series can be added here
FREDCodes <- c("USREC")
FREDData <- as.data.frame(lapply(FREDCodes, fredr, observation_start = StartDate ,observation_end = EndDate, frequency="q" ))

USREC <- FREDData %>% 
  dplyr::select("date", "value")

colnames(FREDData) <- c("Date", "USREC")










