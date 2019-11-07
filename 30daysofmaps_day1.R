#cta map

library(ctar)
library(ggmap)
library(ggplot2)
library(dplyr)




train_positions <- get_arrivals(key = Sys.getenv("ctar_train_api"))

train_latlona <- train_positions %>%
  dplyr::select(lon, lat, rt) %>%
  mutate(lon = as.numeric(as.character(lon)),
         lat = as.numeric(as.character(lat)),
         rt = as.factor(rt))

pal <- c("blue")

cols <- c()

#map <- get_map(location = c(lon = -87.6298, lat = 41.8781), zoom = 12, maptype = "roadmap")

map <- ggmap::get_map(location = c(lon = -87.6298, lat = 41.8781), zoom = 12, maptype="terrain-lines", source="stamen")

map2 <- ggmap::get_map(location = c(lon = -87.6298, lat = 41.8781), zoom = 11, maptype="terrain-lines", source="stamen")

g <- ggmap(map, extennt = "device") +
  geom_point(data = train_latlona, aes(x = lon, y = lat, group = rt, colour = rt)) +
  scale_colour_manual(aesthetics = c("colour", "fill"), values = c("blue", "brown", "green", "orange", "purple", "pink", "red", "yellow")) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")
  
g
