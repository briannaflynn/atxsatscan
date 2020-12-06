library(tidyverse)
library(rgdal)
library(rgeos)
library(maptools)
library(ggalt)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)

# Prepare the zip poly data for US
mydata <- readOGR(dsn = "/Users/brie/Desktop/Mol290C Project/cb_2016_us_zcta510_500k", layer = "cb_2016_us_zcta510_500k")
class(mydata)
head(mydata@data)

# Austin Texas zip code data
zip <- read_csv("/Users/brie/Desktop/Mol290C Project/zip_codes.csv")
zip
summary(zip)

zipVec <- pull(zip)
zipVec

# Get polygon data for TX only
# subset the data by zip

austinMap<- mydata[mydata@data$ZCTA5CE10 %in% zipVec, ]
austinMap@data

set.seed(111)
austinMap$value <- sample.int(n = 10000, size = nrow(austinMap), replace = TRUE)
austinMap@data

zips <- tibble(
  x = austinMap$ZCTA5CE10
)
zips


plot(austinMap)

atxMap <- fortify(austinMap)
atxMap
austinMap.df <- as(austinMap, "data.frame")
centers <- data.frame(gCentroid(spgeom = austinMap, byid = TRUE))
head(centers)
centers$zip <- rownames(centers)

ggplot() + geom_cartogram(data = atxMap, aes(x = long, y = lat, map_id = id), map = atxMap) 

ggplot() + geom_cartogram(data = atxMap, aes(x = long, y = lat, map_id = id), map = atxMap) + coord_map() + theme_map() 





