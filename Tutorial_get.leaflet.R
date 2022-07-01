# load function
source("https://raw.githubusercontent.com/scrameri/PopGen/main/src/get.leaflet.R")

# simulate data
dd <- data.frame(x = c(rnorm(n = 50, mean =  50, sd = 0.1),rnorm(n = 50, mean =  45, sd = 0.2)),
                 y = c(rnorm(n = 50, mean = -14, sd = 0.1),rnorm(n = 50, mean = -20, sd = 0.2)),
                 ID = paste0("IND_", 1:100),
                 LAYER = rep("1", 100),
                 LAYER2 = rep(c(1,2), times = 50),
                 Species = c(rep("A", 50), rep("B", 50)),
                 SpecimenID = 100001:100100)


# get spatial object
# by default, expects geovars = c("x","y") or Tropicos geovars
# by default, expects WGS84 lat/long projection (epsg:4326)
sp <- get.sp(df = dd, proj = "epsg:4326")

# some ways to use get.leaflet()
get.leaflet(sp)
get.leaflet(sp, colorfac = "Species", layerfac = "LAYER2") # any factor for layering may be used
get.leaflet(sp, colorfac = "Species") # by default, layers and color factor levels are the same
get.leaflet(sp, colorfac = "Species", link = "SpecimenID") # click on LINK in popup.
get.leaflet(sp, draw.polygons = TRUE, color.palette = "brown") # change colors 
get.leaflet(sp, draw.polygons = TRUE, color.palette = "brown", alpha = 5) # change smoothness of alpha hull
get.leaflet(sp, draw.polygons = TRUE, color.palette = "brown", alpha = 5, buff.alpha = 0.5) # change buffer
get.leaflet(sp, draw.polygons = TRUE, method.range = "convex.hull")
get.leaflet(sp, draw.polygons = TRUE, colorfac = "Species", method.range = "convex.hull") # convex hull

# assign the returned leaflet map and continue to modify the map using %>%
M <- get.leaflet(sp, draw.polygons = TRUE, colorfac = "Species")
M %>% hideGroup("A")

# in base leaflet, you would need a lot of code to get a map close to the ones above
leaflet() %>% addTiles %>% 
  addCircleMarkers(data = sp,
                   radius = 4,
                   fillOpacity = 0.1,
                   opacity = 1,
                   color = colorFactor(palette = c("red","blue"), 
                                       domain = dd$Species)(dd$Species))

