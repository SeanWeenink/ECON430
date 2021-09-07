###########################################################################
###########################################################################
###                                                                     ###
###                          BRW REPLICATION:                           ###
###                    DATA INPUT AND INITIALIZATION                    ###
###                                                                     ###
###########################################################################
###########################################################################


setwd('C:/Users/seanw/Dropbox/ECON430SeanWeenink/R/Replication of BRW')


# Define required packages
packages <- c("bannerCommenter", "gridExtra", "ggpubr", "readxl", "lpirfs", "dplyr", "fredr", "tidyr", "splines", "sandwich", "lmtest", "Matrix")
# Install packages and load libraries
install.packages(setdiff(packages, rownames(installed.packages()))) 
lapply(packages, require, character.only = TRUE)

# Getting data from FRED:
StartDate = as.Date("1994-01-01") # Start date of the BRW shock series
EndDate = as.Date("2017-12-12") # End date of the BRW shock series\
fredr_set_key("2bf2ff8f666611f4f30b27f3feca9a38")
# Additional series can be added here
FREDCodes <- c("CPIAUCSL", "INDPRO")
FREDData <- as.data.frame(lapply(FREDCodes, fredr, observation_start = StartDate ,observation_end = EndDate ))

FREDData <- FREDData %>% 
  dplyr::select("date", "value", "value.1")
colnames(FREDData) <- c("date", "CPI", "IndProd")
FREDData$date <- as.Date(FREDData$date)

# Add logs 
FREDData <- FREDData %>% 
  mutate(logCPI = log(CPI),
         logIndProd = log(IndProd)
         )

# Importing the BRW shock data
BRWShocks <- read.csv('./ShocksData/brw-shock-series.csv')
BRWShocks <- BRWShocks %>% 
  filter(month < "2018m1") %>% 
  dplyr::select("month", "BRW_monthly") 
BRWShocks <- as.data.frame(BRWShocks)
BRWShocks <- na.omit(BRWShocks)
sd <- sd(BRWShocks$BRW_monthly)

#test <- as.data.frame(seq(as.Date("1994-01-01"), as.Date("2019-09-01"), by="months"))
# Plot CPI and INDProd
ggplot(data = FREDData, aes(x = date, y = CPI)) +
  geom_point()

ggplot(data = FREDData, aes(x = date, y = IndProd)) +
  geom_point()




  
  IV <- function(endog_data, shock, trend, conf){
    
    endog_data <- as.data.frame(endog_data)
    # Shock variable
    shock <- as.data.frame(shock)
    # Estimate linear model
    
    results_lin_iv <- lp_lin_iv(endog_data = log((endog_data*0.01)/sd), lags_endog_lin = 12,
                                shock = shock, trend = trend,
                                confint = conf, hor = 30)
    # Note that "confint" is the z value for a given confidence interval
    
   toString(conf) <- as.data.frame(results_lin_iv[[1]]) %>% 
      gather("id", "mean", 1:30)
    low <- as.data.frame(results_lin_iv[[2]]) %>% 
      gather("id", "low", 1:30) 
    high <- as.data.frame(results_lin_iv[[3]]) %>% 
      gather("id", "high", 1:30)
    
    result <- left_join(mean, low, by="id") %>% 
      left_join(mean, by="id")
    print(result)
    result
  }



confResult <- list(length(conf))

conf <- c(1.67, 1.96, 1.654)  


for(i in 1:length(conf)){
  if(i==1){
    confResult <- IV(FREDData$IndProd, BRWShocks$BRW_monthly, 1, conf[i])
  }
  else{
    confResult <- left_join(confResult, IV(FREDData$IndProd, BRWShocks$BRW_monthly, 1, conf[i]), by="id")
  }
}
confResult <- confResult %>% 
  dplyr::select(c(2,3,6,))

ggplot(data = all, aes(month, mean)) +
  geom_ribbon(aes(ymin=low*100, ymax=high*100), fill="grey70") +
  geom_line(aes(month, mean*100))



IV(FREDData$IndProd, BRWShocks$BRW_monthly, 1,1.96)


# This is the one that works!
IRF <- function(endog_data, shock, trend, conf){
  # Endogenous data
  endog_data <- as.data.frame(endog_data)
  # Shock variable
  shock <- as.data.frame(shock)
  # Estimate linear model

  endog_data <-endog_data/sd
  
  results_lin_iv <- lp_lin_iv(endog_data = log(endog_data), lags_endog_lin = 12,
                              shock = shock, trend = trend,
                              confint = conf, hor = 30)
  
  # Make and save linear plots
  # Here I use ggplot rather than plot_lin since I can adjust the scale more easily and add multiple confidence intervals
  mean <- as.data.frame(results_lin_iv[[1]]) %>% 
    gather("id", "mean", 1:30)
  low <- as.data.frame(results_lin_iv[[2]]) %>% 
    gather("id", "low", 1:30) 
  high <- as.data.frame(results_lin_iv[[3]]) %>% 
    gather("id", "high", 1:30)
  
  all <- left_join(mean, low, by="id") %>% 
    left_join(high, by="id") %>% 
    mutate(month = seq(1:30))
  
  ggplot(data = all, aes(month, mean)) +
    geom_ribbon(aes(ymin=low*100, ymax=high*100), fill="grey70", alpha=0.4) +
    geom_line(aes(month, mean*100)) +
    geom_line()+
    geom_point(aes(x=month, y=mean*100)) +
    scale_x_continuous(name = "Months since shock", breaks=seq(1:30)) +
    scale_y_continuous(name = "% change")
  }



#geom_ribbon( ymin = as.numeric(mean)-1, ymax = as.numeric(mean) +1 , fill = "grey70")



# All use 30 horizon periods 
# Trend is not clear?


# 1 trend
png('./Output/BRW_CPI_T1.png')
IRF(FREDData$CPI,BRWShocks$BRW_monthly, 1, 1.96)
dev.off()  

IRF(BRWShocks$BRW_monthly,BRWShocks$BRW_monthly, 1, 1.96)


png('./Output/BRW_INDProd_T1.png')
IRF(FREDData$IndProd,BRWShocks$BRW_monthly, 1, 1.96)
dev.off()  


#2 trend
png('./Output/BRW_CPI_T2.png')
IRF(FREDData$CPI,BRWShocks$BRW_monthly, 2, 1.96)
dev.off()  



png('./Output/BRW_INDProd_T2.png')
IRF(FREDData$IndProd,BRWShocks$BRW_monthly, 2, 1.96)
dev.off() 

