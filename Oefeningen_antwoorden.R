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

AllCarsGasPedaal <- readRDS("data/AllCarsGasPedaal.Rds")

outlm1 = lm(VraagPrijs ~ KMStand + OuderdomMaanden, data = AllCarsGasPedaal)
outlm2 = lm(VraagPrijs ~ KMStand + OuderdomMaanden + Merk, data = AllCarsGasPedaal)

AllCarsGasPedaal = AllCarsGasPedaal %>% mutate(
  Merk = fct_lump(Merk, 32)
)

outlm2a = lm(VraagPrijs ~ KMStand + OuderdomMaanden + Merk, data = AllCarsGasPedaal)

outlm3 = lm(VraagPrijs ~ KMStand + OuderdomMaanden + Merk + Merk*KMStand, data = AllCarsGasPedaal)
outlm4 = lm(VraagPrijs ~ KMStand + OuderdomMaanden + Merk + Merk*KMStand + Motor + Transmissie, data = AllCarsGasPedaal)

summary(outlm1)
summary(outlm2)
summary(outlm2a)
summary(outlm3)
summary(outlm4)


## * Oefening 2 ###############################################################

library(splines)
linmod = lm(VraagPrijs ~ KMStand + OuderdomMaanden , data = AllCarsGasPedaal)
splinemod = lm(VraagPrijs ~ ns(KMStand,6) + ns(OuderdomMaanden,6) , data = AllCarsGasPedaal)

summary(linmod)
summary(splinemod)


## * Oefening 3 #################################################################

library(rpart)
library(visNetwork)

tree.out = rpart(VraagPrijs ~ KMStand + Merk, data = AllCarsGasPedaal)
visTree(tree.out, height = "800px", nodesPopSize = TRUE, minNodeSize = 10, maxNodeSize = 30)

tree.out = rpart(VraagPrijs ~ KMStand + Merk, data = AllCarsGasPedaal, control = list(cp=0.001))
visTree(tree.out, height = "800px", nodesPopSize = TRUE, minNodeSize = 10, maxNodeSize = 30)


## * Oefening 4 ##################################################################

library(h2o)
library(dplyr)

Jaap <- readRDS("data/Jaap.RDs")

Jaap = Jaap %>% 
  mutate(
    Duur = if_else(prijs > 800000, "Y", "N") %>% as.factor
  ) %>% 
  mutate_if(is.character, as.factor)

h2o.init()

jaap.h2o = as.h2o(Jaap)

JaapTT = h2o.splitFrame(jaap.h2o, ratios = 0.8)


RFmodel = h2o.randomForest(
  x = c(2,4,5,6,7),
  y = "Duur",
  training_frame  = JaapTT[[1]],
  validation_frame = JaapTT[[2]]
)
RFmodel

h2o.varimp_plot(RFmodel)

h2o.gainsLift(RFmodel, JaapTT[[2]])

