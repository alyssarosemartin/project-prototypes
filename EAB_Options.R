library(raster)
library(sp)
library(lubridate)
library(rnpn)
library(leaflet)
library(maps)
library(mapdata)

rm(list=ls())

#https://uspest.org/CAPS/EAB_cohorts/
#https://uspest.org/CAPS/EAB_cohorts/Misc_output/Earliest_PEMp0Excl1_20221231.tif
#https://uspest.org/CAPS/DDRP_user_guide_and_platform_requirements.pdf

Rasters <- "~/Documents/My Files/USA-NPN/Data/Analysis/R_default/NIFA/EAB"
setwd(Rasters)

EAB_adult <- raster("Earliest_PEMp0Excl1_20221231.tif")
EAB_egg <- raster("Earliest_PEMe1Excl1_20221231.tif")
#EAB_stage <- raster("EAB_StageCount_Excl1_20220725.tif") #not using bc hard to figure out how to plot with the decimal system

plot(EAB_adult)
plot(EAB_egg)
hist(EAB_adult)
plot(EAB_adult, colNA="blue")
crs(EAB_adult)
crs(EAB_egg)

#set CRS - need to double check this with Brittany
proj4string(EAB_adult) <- CRS("+init=epsg:4269")
proj4string(EAB_egg) <- CRS("+init=epsg:4269")

#Don't run - was just for figuring out how climate exclusion is coded in raster - answer: -2
location = data.frame(x = -75.4007,
                      y = 39.9869)
coordinates(location) <- ~x+y
mypoints = SpatialPoints(location,proj4string = CRS("+init=epsg:4269"))
myvalues = extract(EAB, mypoints)
data <- data.frame(myvalues)


#OPTION 2 - more popular according to our survey (past dates are grey, far future dates by month name, then 2 months, 1 month, 2 weeks, 1 week)
  
#Create raster for option 2

#set -2 values to NA, check map looks ok
EAB_adult2 <- reclassify(EAB_adult, cbind(-2, NA)) 
plot(EAB_adult2) 

EAB_egg2 <- reclassify(EAB_egg, cbind(-2, NA)) 
plot(EAB_egg2) 


#set up weeks and months and prior months
#today = yday(today())
today = 120
pm3 = today - 90
pm2 = today - 60
pm1 = today - 30
w1 = today + 7
w2 = today + 14
w3 = today + 21
w4 = today + 28
m1 = today + 60
m2 = today + 90

#create function to bin dates into the weeks/months - using numbers 1-11, where 1 = more than 3 months ago etc
fun <- function(x) {
  ifelse(x<pm3, 1,
         ifelse(x>=pm3 & x<pm2, 2,
                ifelse(x>=pm2 & x<pm1, 3,
                       ifelse(x>=pm1 & x<today, 4,
                              ifelse(x>=today & x<w1, 5,
                                     ifelse(x>=w1 & x<w2, 6,
                                            ifelse(x>=w2 & x<w3, 7,
                                                   ifelse(x>=w3 & x<w4, 8,
                                                          ifelse(x>=w4 & x<m1, 9,
                                                                 ifelse(x>=m1 & x<m2, 10,
                                                                        ifelse(x>=m2, 11,
            NA)))))))))))
}


#apply function to the raster layer(s)
EAB_adult3 <- calc(EAB_adult2, fun)
plot(EAB_adult3)
hist(EAB_adult3)

EAB_egg3 <- calc(EAB_egg2, fun)
plot(EAB_egg3)
hist(EAB_egg3)


#create palette for binned weeks/months
bins <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
cb <- c("#E8E8E8", "#C8C8C8", "#808080", "#000000",
  "#fde725",  "#b5de2b", "#6ece58", "#35b779", "#1f9e89", "#26828e", "#31688e")
pal5 <- colorBin(palette = cb, bins = bins, domain = EAB_adult3,  na.color = "transparent")

#get state outlines
states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
class(states)
plot(states)

#create map for EAB adult layer
leaflet() %>% addTiles() %>%
  addRasterImage(EAB_adult3, colors = pal5, opacity = 1) %>%
  addPolygons(data = states, opacity = 1, weight = 1, color = "black", fillColor = "transparent") %>%
  addLegend("bottomright", 
            colors = c("#E8E8E8", "#C8C8C8", "#808080", "#000000","#fde725",  "#b5de2b", "#6ece58", "#35b779", "#1f9e89", "#26828e", "#31688e"),
            labels= c("More than 3 Months Ago", "3 Months Ago", "2 Months Ago","Last Month", "This Week", "Next Week","In 2 weeks", "In 3 weeks", "Next month", "In 2 months", "In more than 2 months"),
            title= "EAB Adults Expected",
            opacity = 1)  %>%
  setView(lng = -93,lat = 34, zoom = 4) 

#create map for EAB EGG layer
leaflet() %>% addTiles() %>%
  addRasterImage(EAB_egg3, colors = pal5, opacity = 1) %>%
  addPolygons(data = states, opacity = 1, weight = 1, color = "black", fillColor = "transparent") %>%
  addLegend("bottomright", 
            colors = c("#E8E8E8", "#C8C8C8", "#808080", "#000000","#fde725",  "#b5de2b", "#6ece58", "#35b779", "#1f9e89", "#26828e", "#31688e"),
            labels= c("More than 3 Months Ago", "3 Months Ago", "2 Months Ago","Last Month", "This Week", "Next Week","In 2 weeks", "In 3 weeks", "Next month", "In 2 months", "In more than 2 months"),
            title= "EAB Eggs Expected",
            opacity = 1)  %>%
  setView(lng = -93,lat = 34, zoom = 4) 
