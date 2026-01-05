# Creating a map of popular names
library(tidyverse)

# package for creating maps similar to ggplot()
library(tmap) # install.packages("tmap")

# package contains spacial data for US states (and more)
library(spData) # install.packages("spData")

library(leaflet) # for interactive maps


# Add fill layer to nz shape
tm_shape(us_states) +
  tm_fill() 
# Add border layer to nz shape
tm_shape(us_states) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(us_states) +
  tm_fill() +
  tm_borders() 

map_us <- tm_shape(us_states) + tm_polygons()

