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
           

## Oefening 5 #################################################################

library(lubridate)
library(anytime)

x = anydate("1991-02-15")
y = anydate("2004-05-03")
y-x

as.integer(y-x) %/% 7
interval(x,y) %/% weeks()

library(tidyverse)
Restaurants <- read_csv("data/Restaurants.csv")

Restaurants = Restaurants %>% 
  mutate(
    keuken = fct_lump(keuken, n = 10)
  )

table(Restaurants$keuken)  


## oefening 6 #################################################################

library(plotly)

Restaurants <- read_csv("data/Restaurants.csv")
Restaurants %>% 
  group_by(keuken) %>% 
  summarise(gem_prijs = mean(prijs, na.rm = TRUE)) %>% 
  plot_ly(x = ~keuken, y = ~gem_prijs)

Restaurants %>% 
  group_by(keuken) %>% 
  summarise(gem_prijs = mean(prijs, na.rm = TRUE)) %>% 
  arrange(desc(gem_prijs)) %>% 
  slice(1:25) %>% 
  mutate(
    keuken = fct_reorder(keuken, gem_prijs, mean)
  ) %>% 
  plot_ly(x = ~keuken, y = ~gem_prijs)



#### Sessie 2 #####################################################################################


## * Oefening 1 ###############################################################







