setwd('C:/Users/seanw/Dropbox/ECON430SeanWeenink/R')


library("XML")
library("methods")

data <- read.csv("StateData.csv")


StateExportsTotal <- read.csv("C:/Users/seanw/Dropbox/ECON430SeanWeenink/R/ECON430/StateDataTotal.csv")



# Importing total exports by state
TotalExportsByState <- read_excel("ECON430/TotalExportsByState.xlsx")
# Find the mean exports for regression
ExportMeans <- as.data.frame(colMeans(TotalExportsByState))
ExportMeans <- ExportMeans %>% 
  mutate(state = colnames(TotalExportsByState))



colnames(ExportMeans) <- c("avg", "state")
ExportMeans <- ExportMeans[-1,]



a <- cbind(ExportMeans, test, by="state")


b <- a[!duplicated(as.list(a))]


ggplot(, aes(x=avg, y=max))+
  geom_point()
