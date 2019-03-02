################################################
###
### voorbeeld van reticulate met top2000 data

#### libraries ############
library(dplyr)
library(plotly)
library(reticulate)
use_condaenv("my_py36")

## top2000 data
top2000 <- readRDS("top2000dataset.RDs")


plot_ly(
  top2000, 
  text = ~paste(
    artist, "<br>", song, " ", "<br> top 2000 positie", positie, "<br> spotify popularity", popularity),
  x = ~popularity,
  y = ~positie) 

ggplot(top2000, aes(popularity,positie)) +
  geom_point() + 
  geom_smooth(method = lm) + 
  labs(title = "Top2000 positie versus Spotify popularity")


##### extract sound features
featurematrix = top2000[, 8:18] %>%  as.matrix

#### apply python umap ########################
umap = import("umap")

embedding = umap$UMAP(
  n_neighbors = 5L,
  n_components = 3L,
  min_dist = 0.15,
  metric='euclidean'
)

## compute UMAP with 3 components
embedding_out = embedding$fit_transform(featurematrix)

top2000Set = data.frame(embedding_out)
top2000Set$trackid = top2000$trackid
top2000Set = top2000Set %>% 
  left_join(top2000) %>% 
  mutate(duur_min = as.numeric(duration)/1000/60)

text = paste(top2000Set$artist, "<br>", top2000Set$song, " ", top2000Set$positie)

plot_ly(
  top2000Set, 
  x = ~X1,
  y = ~X2, 
  z = ~X3,
  color=~positie,
  text = ~paste(
    artist, "<br>", song, " ", "<br>", positie),
  size =.1 , sizes = c(4,5)
) %>% 
  layout(title = '3D umap of Radio2 top 2000 songs 2018')

