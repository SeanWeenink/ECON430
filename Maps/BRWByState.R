###########################################################################
###########################################################################
###                                                                     ###
###                          BRW REPLICATION:                           ###
###                    DATA INPUT AND INITIALIZATION                    ###
###                                                                     ###
###########################################################################
###########################################################################
setwd('C:/Users/seanw/Dropbox/ECON430SeanWeenink/R/ECON430/Maps')


# Importing the BRW shock data
BRWShocks <- read.csv('./ShocksData/brw-shock-series.csv')
BRWShocks <- BRWShocks %>% 
  filter(month < "2018m1") %>% 
  dplyr::select("month", "BRW_monthly") 
BRWShocks <- as.data.frame(BRWShocks)
BRWShocks <- na.omit(BRWShocks)
#sd <- sd(BRWShocks$BRW_monthly) For adjusting the shock size

# Define the function that produces IRF graphs
IRFGraph <- function(endog_data, shock, trend, conf){
  # Endogenous data
  endog_data <- as.data.frame(endog_data)
  # Shock variable
  shock <- as.data.frame(shock)
  # Estimate linear model
  results_lin_iv <- lp_lin_iv(endog_data = endog_data, lags_endog_lin = 12,
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
  # Create a data frame with the mean, lower cf and upper cf
  all <- left_join(mean, low, by="id") %>% 
    left_join(high, by="id") %>% 
    mutate(month = seq(1:30))
  # Plot the graph 
  ggplot(data = all, aes(month, mean)) +
    geom_ribbon(aes(ymin=low, ymax=high), fill="grey70", alpha=0.4) +
    geom_line(aes(month, mean)) +
    geom_line()+
    geom_point(aes(x=month, y=mean)) + # points for each month 
    scale_x_continuous(name = "Months since shock", breaks=seq(1:30)) +
    scale_y_continuous(name = "% change in unemployment") +
    theme_minimal() +
    geom_hline(yintercept=0, linetype="dashed", color="red", size=1)
  
}
# Creating and saving the IRF graphs for each state
# create a vector of column names for reference in the for loop
colnames <- colnames(FREDData)
for(i in 2:ncol(FREDData)-1){ # -1 to not include the date column
  print(i)  # printing i as a check
  CSVFileName <- paste("./Output/",colnames[i],".png",sep="") # This names the the saved file for each state
  png(CSVFileName)
  print(IRFGraph(FREDData[i],BRWShocks$BRW_monthly, 1, 1.96))# to save the file it needs to display in the plot viewer. print() does this within a for loop or function
  dev.off() 
}



# Define the function that will calculate the max change in unemployment from each IRF for each state
# This is for use in the map as a color based measure
IRFMax <- function(endog_data, shock, trend, conf){
  # Endogenous data
  endog_data <- as.data.frame(endog_data)
  # Shock variable
  shock <- as.data.frame(shock)
  # Estimate linear model
  results_lin_iv <- lp_lin_iv(endog_data = endog_data, lags_endog_lin = 12,
                              shock = shock, trend = trend,
                              confint = conf, hor = 30)
 
  # Calculate the IRF 
  mean <- as.data.frame(results_lin_iv[[1]]) %>% 
    gather("id", "mean", 1:30)
  # Determine the max change in mean
  max <- max(mean$mean)
  max
}

IRFMax(FREDData[1],BRWShocks$BRW_monthly, 1, 1.96)


colnames <- colnames(FREDData)


test <- data.frame(c("a"), c("b"))
colnames(test) <- c("state", "max")

for(i in 1:(ncol(FREDData)-1)){ # -1 to not include the date column
  result <- c(colnames[i], IRFMax(FREDData[i],BRWShocks$BRW_monthly, 1, 1.96))
  test <- test %>% 
    rbind(result)
}

test <- test[-1,]


colnames(test) <- c("NAME", "max")







