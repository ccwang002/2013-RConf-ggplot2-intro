library(plyr)
library(ggmap)
df.air <- read.csv("play_air_map/air_nothern_2012.csv", as.is=TRUE)
df.loc <- data.frame(loc=unique(df.air$loc), stringsAsFactors=FALSE)

# retreive GEO location (lon, lat)
locator <- function(loc) {
  geo_loc <- geocode(location=loc$loc)
  df_temp <- data.frame(loc=loc, lon=geo_loc$lon, lat=geo_loc$lat)
  return(df_temp)
}
df.loc <- ddply(df.loc, .(loc), locator)

# combined with the air data
df.air_loc <- merge(df.air, df.loc, by="loc", all.y=TRUE)
df.air_loc$pollutant <- factor(df.air_loc$pollutant)

g <- ggmap(
  get_googlemap(
    center=c(121.49, 25.04), #Long/lat of centre, or "Edinburgh" city place
    zoom=10, maptype='satellite', #also hybrid/terrain/roadmap
    scale=2,  #resolution scaling, 1 (low) or 2 (high)
    language='zh-TW' # currently will fail, new ggmap solved this problem
  ),
  extent='device', #can also be "normal" etc
  darken = 0 #you can dim the map when plotting on top
)

g + geom_point(
  data=df.air_loc[df.air_loc$hour == 10 & df.air_loc$pollutant == "PM10", ], 
  aes(x=lon, y=lat, size=value, alpha=value), 
  color="yellow") + 
  scale_size_continuous(range=c(5, 30), name="PM10 (ug/m3)") + 
  scale_alpha_continuous(range=c(0.2, 1), name="PM10 (ug/m3)")

