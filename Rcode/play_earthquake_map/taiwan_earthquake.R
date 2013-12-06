library(plyr)
library(ggmap)
# adapted from http://xccds1977.blogspot.tw/2012/06/ggmap.html
# raw data from http://www.cwb.gov.tw/V7/earthquake/rtd_eq.htm
df.earthquake <- read.csv("play_earthquake_map/earthquake_month_parsed.csv", as.is=TRUE)

g <- ggmap(
  get_googlemap(
    center="Taiwan", #Long/lat of centre, or "Edinburgh" city place
    zoom=8, maptype='terrain', #also hybrid/terrain/roadmap
    scale=2,  #resolution scaling, 1 (low) or 2 (high)
    language='zh-TW' # currently will fail, new ggmap solved this problem
  ),
  extent='device', #can also be "normal" etc
  darken = 0 #you can dim the map when plotting on top
)

show_over_scale <- function(scale) {
  df_temp <- df.earthquake[df.earthquake$scale > scale, ]
  g_layered <- g + geom_point(data=df_temp, aes(x=lon, y=lat), color="red") +
    stat_density2d(
      aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
      bins = 12, geom = "polygon",
      data = df_temp
    ) + scale_alpha_continuous(range=c(0.3, 1), guide=FALSE) +
    scale_fill_continuous(low="yellow", high="red", guide=FALSE)
  ggsave(filename=paste0(scale*10, ".png"), scale=2, width=2, height=2)
}

for(s in seq(from=2, to=5, by=0.5)) 
  show_over_scale(s)
