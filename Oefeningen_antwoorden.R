#### SESSIE 1 #####################################################################################

####  * Oefening 2 #######################################################

boodschappen <- readRDS("data/boodschappen.RDs")

library(readr)
Restaurants <- read_csv("data/Restaurants.csv")

library(dplyr)
Restaurants = Restaurants %>% mutate(arvLog = log(aantalreviews))

Restaurants = Restaurants %>% select(-straat)


## * Oefening 3 ###############################################################

Restaurants = Restaurants %>% filter(!is.na(aantalreviews))

Zwolle = Restaurants %>% filter(plaats == "Zwolle")

keuken = Restaurants %>% 
  group_by(keuken) %>% 
  summarise(gem = mean(aantalreviews))


## * Oefening 4 ###############################################################

library(stringr)
test = Restaurants %>% mutate(lengte = str_length(restNamen)) %>%  arrange(desc(lengte))
patroon = glob2rx("Q*")
Restaurants %>% filter( str_detect(restNamen, patroon))

Restaurants %>%
  filter( 
    !str_detect(restNamen, patroon),
    str_detect(restNamen, "Q")
  )
           
           

#### Sessie 2 #####################################################################################


## * Oefening 1 ###############################################################







