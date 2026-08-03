# Demo terra::viewshed().

library(tidyverse)
library(terra)
library(tidyterra)
library(viridis)

f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)
x <- project(r, "EPSG:2169")
p <- cbind(70300, 96982)
v <- viewshed(x, p, 0, 0, 0.85714)

v_0 = ifel(v == 1, NA, v)
v_1 = ifel(v == 0, NA, v)

p_vect = vect(p, crs = "EPSG:2169")

ggplot() + 
  geom_spatraster(data = r) + 
  scale_fill_viridis(option = "A") +
  scale_fill_viridis(option = "A", na.value = NA) +
  theme_void() +
  theme(legend.position = "none")

ggplot() + 
  geom_spatraster(data = r) +
  geom_spatvector(data = p_vect, shape = 21, size = 3, fill = "white") +
  geom_spatraster(data = v_1, alpha = 0.33) +
  scale_fill_viridis(option = "A", na.value = NA) +
  theme_void() +
  theme(legend.position = "none")
