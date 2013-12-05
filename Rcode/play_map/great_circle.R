# code from https://gist.github.com/cengel/2165374
# idea from http://www.stanford.edu/~cengel/cgi-bin/anthrospace/great-circles-on-a-recentered-worldmap-in-ggplot

library(maps)
library(geosphere)
library(plyr)
library(ggplot2)
library(sp)

airports <- read.csv("play_map/airports.csv", as.is=TRUE, header=TRUE)
flights <- read.csv("play_map/openflights-export-TPE-2013-12-05.csv", as.is=TRUE, header=TRUE)

# aggregate nunber of flights
flights.ag <- ddply(flights, .(From, To), summarise, freq = length(To))

# add latlons
flights.ll <- merge(flights.ag, airports, all.x=T, by.x="To", by.y="IATA")
taipei.ll <- c(airports$longitude[airports["IATA"]=="TPE"],airports$latitude[airports["IATA"]=="TPE"])

# calculate routes -- Dateline Break FALSE, otherwise we get a bump in the shifted ggplots
rts <- gcIntermediate(taipei.ll, flights.ll[,c('longitude', 'latitude')], 100, breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)
rts.ff <- ggplot2:::fortify.SpatialLinesDataFrame(rts) # convert into something ggplot can plot

flights.ll$id <-as.character(c(1:nrow(flights.ll))) # that rts.ff$id is a char
gcircles <- merge(rts.ff, flights.ll, all.x=T, by="id") # join attributes, we keep them all, just in case


### Recenter ####

center <- 0 # positive values only - US centered view is 260

# shift coordinates to recenter great circles
gcircles$long.recenter <- ifelse(gcircles$long < center - 180 , gcircles$long + 360, gcircles$long)

# shift coordinates to recenter worldmap
worldmap <- map_data ("world")
worldmap$long.recenter <- ifelse(worldmap$long < center - 180 , worldmap$long + 360, worldmap$long)

### Function to regroup split lines and polygons
# takes dataframe, column with long and unique group variable, returns df with added column named group.regroup
RegroupElements <- function(df, longcol, idcol){
  g <- rep(1, length(df[,longcol]))
  if (diff(range(df[,longcol])) > 300) { # check if longitude within group differs more than 300 deg, ie if element was split
    d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
    g[!d] <- 1 # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
    g[d] <- 2 # parts that are moved
  }
  g <- paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
  df$group.regroup <- g
  df
}

### Function to close regrouped polygons
# takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable
ClosePolygons <- function(df, longcol, ordercol){
  if (df[1,longcol] != df[nrow(df),longcol]) {
    tmp <- df[1,]
    df <- rbind(df,tmp)
  }
  o <- c(1: nrow(df)) # rassign the order variable
  df[,ordercol] <- o
  df
}

# now regroup
gcircles.rg <- ddply(gcircles, .(id), RegroupElements, "long.recenter", "id")
worldmap.rg <- ddply(worldmap, .(group), RegroupElements, "long.recenter", "group")

# close polys
worldmap.cp <- ddply(worldmap.rg, .(group.regroup), ClosePolygons, "long.recenter", "order") # use the new grouping var

# Earth image from NASA
# http://visibleearth.nasa.gov/view.php?id=55167
library(jpeg)
library(reshape2)
library(grid)
img <- readJPEG("play_map/earth_lights_lrg.jpg")
#for high res
#img <- readTIFF("play_map/earth_lights_4800.tif")

# plot
g <- ggplot(worldmap.cp) + 
  # geom_polygon(aes(long.recenter, lat , group=group.regroup), size = 0.2, fill="#f9f9f9", colour = "grey65", data=worldmap.cp) +
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                    -180, 180, -90, 90) + 
  geom_line(aes(long.recenter,lat,group=group.regroup, color=freq, alpha=freq), size=5, data=gcircles.rg) + 
  scale_colour_gradient(low="yellow", high="red") +   # set color gradient here
  scale_alpha_continuous(range=c(0.2, 1)) + # set transparency here
  ylim(-90, 90) +
  coord_equal()

theme_bare <- theme(
  axis.line = element_blank(), 
  axis.text.x = element_blank(), 
  axis.text.y = element_blank(),
  axis.ticks = element_blank(), 
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(), 
  axis.ticks.length = unit(0.001, "lines"), 
  axis.ticks.margin = unit(0.001, "lines"), 
  legend.position = "none", 
  panel.background = element_blank(), 
  panel.border = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.margin = unit(c(0, 0, 0, 0), "lines"), 
  plot.background = element_blank(),
  plot.margin = unit(c(0, 0, 0, 0), "lines"),
  line = element_blank(),
  text= element_blank()
)

g + theme_bare + labs(x=NULL, y=NULL)

