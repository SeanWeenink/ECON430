#############################################################
# demo.R
# This program demonstrates how to use the smooth local
# projection R package lproj.R by estimating the 
# impulse response of GDP to a monetary shock
# (instrumented by contemporaneous inflation and lagged GDP, 
# inflation, and interest rate)
##############################################################

# Clear the environment
rm(list=ls())

# Bring in the smooth local projection function
source("lproj.R")

# Load the macro data
load("us_macro.RData")
cols <- us_macro[,2:ncol(us_macro)]

# Plot the raw series
head(us_macro)
matplot(us_macro,t='l')
legend("topright", colnames(us_macro),col=seq_len(ncol(us_macro)),cex=0.8,fill=seq_len(ncol(us_macro)))

P <- 4 # number of lags of rhs variables
h1<- 1 # Set to 0 to have the shock hit in period 0, 1 to have the shock hit in period 1
H <- 20 # Number of horizons

y  <- us_macro$yg # Response variable
x  <- us_macro$ir # Endogenous variable related to the shock
# Other RHS variables (used to instrument for the impulse variable)
w  <- cbind( us_macro$yg , us_macro$pi , lagmatrix( cbind(us_macro$yg,us_macro$pi,us_macro$ir) , P ) ) 

# Create lambda options
lambda <- c(0.0001,0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0,9,10)/1000

# Remove NA rows due to lags
LPdata <- cbind(y,x,w)
LPdata <- na.omit(LPdata)
colnames(LPdata)[1] <- "yg"
colnames(LPdata)[2] <- "ir"
w <- LPdata[,3:ncol(LPdata)]
y <- LPdata[,1]
x <- LPdata[,2]

# Run a regular local projection (Jorda 2005).
ir.regular  <- lproj( y=y , x=x , w=w , const=TRUE , type='reg' , H=H , h1=h1 )

# Run a smooth local projection
ir.smooth   <- lproj( y=y , x=x , w=w , const=TRUE , type='smooth' , H=H , h1=h1 , r=2 , lambda=lambda )
ir.smooth   <- lproj.cv( ir.smooth , 5 ) # Pick the optimal coefficients as result of minimum RSS from choice of lambda

# Plot the regular and smooth local projection coefficients as impulse response functions to compare them
plot(ir.regular$ir, type='l', col='blue', xlab='time', ylab='response')
lines(ir.smooth$ir.opt, type='l', col='red')
abline(h=0)
legend('bottomright', inset=.05, legend=c("regular", "smooth"), col=c('blue','red'), lty=1:2)

# Add confidence intervals to the smooth local projections and plot the impulse response functions
ir.smooth <- lproj.conf(ir.smooth, ir.smooth$idx.opt)

plot(ir.smooth$ir.opt, type='l', col='black', xlab='time', ylab='response', ylim=range(min(na.omit(ir.smooth$irc[,1])), max(na.omit(ir.smooth$irc[,2]))))
lines(ir.smooth$irc[,1], type='l', col='red', lty='dashed')
lines(ir.smooth$irc[,2], type='l', col='red', lty='dashed')
abline(h=0)

