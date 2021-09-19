###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 1:                             ###
###                    DATA INPUT AND INITIALIZATION                    ###
###                                                                     ###
###########################################################################
###########################################################################

setwd('C:/Users/seanw/Dropbox/ECON430SeanWeenink/R/ECON430/Maps')

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
FREDCodes <- c("FLUR", "CAUR", "TXUR", "OHUR","NYUR", "MIUR", "PAUR", "NCURN", "WIUR", "ILURN", "MOUR", "NJURN", "ORUR", "TNUR", "MNURN", "ALUR", "SCUR", "WAUR",
               "KYURN", "GAUR", "WVUR", "INUR", "COUR", "AZUR", "OKUR", "LAUR", "MAUR", "ARUR", "NMUR", "MTUR", "HIURN", "VAUR", "IAUR", "NVUR", "KSURN",
               "IDURN", "MSUR", "MEUR", "CTURN", "UTUR", "AKUR", "MDUR", "SDURN", "NDUR", "NHURN", "NEUR", "WYUR", "DEUR", "VTUR", "RIUR", "PRUR"
               )



FREDData <- as.data.frame(lapply(FREDCodes, fredr, observation_start = StartDate ,observation_end = EndDate ))




value <- rep("value.", 50)
nums <- seq(1, 50, 1)
names <- as.data.frame(paste(value, nums, sep=""))
names <- rbind(names, c("value"))
colnames(names) <- "names"
names <- as.data.frame(sort(names$names))
colnames(names) <- "names"

names <- as.vector(t(names))


date <- FREDData %>% 
  dplyr::select(date)
Rates <- FREDData %>% 
  dplyr::select(one_of(names))


FREDData <- Rates %>% 
  mutate(date = date)
colnames(FREDData) <- c("Florida", "California", "Texas", "Ohio","New York", "Michigan", "Pennsylvania", "North Carolina", "Wisconsin", "Illinois", "Missouri", "New Jersey", "Oregon", "Tennessee", "Minnesota", "Alabama", "South Carolina", "Washington",
                        "Kentucky", "Georgia", "West Virginia", "Indiana", "Colorado", "Arizona", "Oklahoma", "Louisiana", "Massachusetts", "Arkansas", "New Mexico", "Montana", "Hawaii", "Virginia", "Iowa", "Nevada", "Kansas",
                        "Idaho", "Mississippi", "Maine", "Connecticut", "Utah", "Alaska", "Maryland", "South Dakota", "North Dakota", "New Hampshire", "Nebraska", "Wyoming", "Delaware", "Vermont", "Rhode Island", "Puerto Rico", "date"
)




averages <- as.data.frame(colMeans(FREDData[1:51][sapply(FREDData[1:51], is.numeric)])) 

averages <- averages %>% 
  mutate(NAME = c("Florida", "California", "Texas", "Ohio","New York", "Michigan", "Pennsylvania", "North Carolina", "Wisconsin", "Illinois", "Missouri", "New Jersey", "Oregon", "Tennessee", "Minnesota", "Alabama", "South Carolina", "Washington",
                  "Kentucky", "Georgia", "West Virginia", "Indiana", "Colorado", "Arizona", "Oklahoma", "Louisiana", "Massachusetts", "Arkansas", "New Mexico", "Montana", "Hawaii", "Virginia", "Iowa", "Nevada", "Kansas",
                  "Idaho", "Mississippi", "Maine", "Connecticut", "Utah", "Alaska", "Maryland", "South Dakota", "North Dakota", "New Hampshire", "Nebraska", "Wyoming", "Delaware", "Vermont", "Rhode Island", "Puerto Rico"
  ))

colnames(averages) <- c("avg", "NAME")
  
ShortStateNames <- ShortStateNames %>% 
  gather("id", "name", 1:51)

states <- left_join(states, averages, by="NAME")


############################################################################
############################################################################
###                                                                      ###
###                              SECTION 2:                              ###
###                                 MAPS                                 ###
###                                                                      ###
############################################################################
############################################################################

library(leaflet)
library(htmlwidgets)
library(sf)
library(maps)
library(maptools)
library(rgdal)
library(tigris)


# Remove us territories that are not in the unemployment dataset
states <- states(cb = TRUE)
states <- left_join(states, averages, by="NAME")
states <- na.omit(states)


# Select color bins for map scale
bins <- c(0, 0.5, 1, 1.5, 2, 3, 4, 5, Inf)
pal <- colorBin("YlOrRd", domain = states$avg, bins = bins)


# Creating the maps
leaflet(states) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(avg),
              color = "black",
              weight = 0.5) %>%
  setView(-98.5795, 39.8282, zoom=3) %>% 
  addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                                                   position = "bottomright")






# Remove us territories that are not in the unemployment dataset
states <- states(cb = TRUE)
states <- left_join(states, test, by="NAME")

states <- na.omit(states)

states$max <- as.numeric(states$max)

# Select color bins for map scale
bins <- c(-Inf, -2, -1, 0, 1, 2, 3, 4, Inf)
pal <- colorBin("YlOrRd", domain = states$max, bins = bins)


# Creating the maps
leaflet(states) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(max),
              color = "black",
              weight = 0.5) %>%
  setView(-98.5795, 39.8282, zoom=3) %>% 
  addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
            position = "bottomright")

