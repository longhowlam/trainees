############### maps and openstreetmaps in R ################################################

library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(leaflet)
# need latest dev version because of jpeg issues
#devtools::install_github("dkahle/ggmap") 

######  restaurant data ####################################################

restaurants <- readr::read_csv("data/Restaurants.csv")

## restaurants in amsterdam
amsterdam = restaurants %>% 
  filter(
    plaats == "Amsterdam",
    !is.na(aantalreviews),
    !is.na(LONGs),
    !is.na(LATs)
  )

## transform data to a sf object (Simple Features object: open standard in data format for GIS 
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
amsterdam <- st_as_sf(
  x = amsterdam,                         
  coords = c("LONGs", "LATs"),
  crs = projcrs
)

amsterdam_map <- get_map(
  getbb("Amsterdam"), 
  maptype = "toner-background", 
  source = "stamen"
)

# duurt erg lang
ggmap(amsterdam_map) +
  geom_sf(
    data = amsterdam,
    inherit.aes =FALSE,
    aes(colour = aantalreviews, fill = aantalreviews),
    alpha = .65,
    size = 1,
    shape = 21
  )+
  labs(title = "restaurants in Amsterdam", x="",y="")


######## Heel nederland ###################################################

# eerste argument is een bounding box van NL in long lat coordinaten
mad_map <- get_map(
  c(left = 3.3, bottom = 50.5, right = 7.3, top = 53.6902),
  maptype = "toner-background", 
  source = "stamen"
  )

df <- st_as_sf(
  x = restaurants,                         
  coords = c("LONGs", "LATs"),
  crs = projcrs
)

# duurt erg lang
ggmap(mad_map) +
  geom_sf(data = df,
          inherit.aes =FALSE,
          aes(colour = aantalreviews),
          fill="#004529",
          alpha = .65,
          size = 2,
          shape = 21)+
  labs(title = "restaurants in Amsterdam", x="",y="")

########### POI ####################################################

# Er is meer. Je kan PoI opvragen met osm: zie ook https://wiki.openstreetmap.org/wiki/Map_Features


## aanwezige features
available_features()

# Een feature is bijvoorbeeld amenities (voorzieningen / faciliteiten)
# en deze heeft weer tags. Die kan je tonen
available_tags("amenity")

# shops
available_tags("shop")

#### Querying PoI's ###########################

## Speed Cameras in Amsterdam
q <- getbb("Amsterdam")%>%
  opq()%>%
  add_osm_feature("highway",	"speed_camera")
out = osmdata_sf(q)
speed = out$osm_points 

## Kaartje met PoI's
ggmap(amsterdam_map) +
  geom_sf(data = speed,
          inherit.aes =FALSE,
          colour="#238443",
          fill="#004529",
          alpha = .95,
          size = 4,
          shape=21)+
  labs(title = "fixed speed cameras")



#### Cheese shops ############################

out =  getbb("Amsterdam")%>%
  opq() %>%
  add_osm_feature("shop") %>% 
  osmdata_sf()

out2 = out$osm_points 
out2 = out2 %>%  select(name, addr.city, opening_hours, shop )

# list type of shops
typewinkels = out2 %>% group_by(shop) %>%  summarise(n=n())

cheesewinkels = out2 %>% filter(shop == "cheese")

# Plaats cheese shops op een leaflet kaart
cheeseIcons <- iconList(
  cheese = makeIcon("cheese.png",  20, 20)
)

leaflet(cheesewinkels) %>%
  addTiles() %>%
  addMarkers(icon = ~cheeseIcons, popup = ~name)


## Een gewone ggmap kaartje
ggmap(amsterdam_map) +
  geom_sf(data = cheesewinkels,
          inherit.aes =FALSE,
          colour="#238443",
          fill="#004529",
          alpha = .95,
          size = 2,
          shape=21)+
  labs(title = "cheese shops in Amsterdam")
