## ----echo=FALSE, out.width="100%"--------------------------------------------
knitr::include_graphics("figs/raster_concept.png")


## ----echo=FALSE, out.width="100%"--------------------------------------------
knitr::include_graphics("figs/SpatRaster.png")


## ----libraries, warning = FALSE,message=FALSE--------------------------------
library(sf)
library(terra)
library(tidyterra)
library(tidyverse)
library(malariaAtlas)
library(RColorBrewer)


## ----load, message = FALSE---------------------------------------------------

#lets load a population raster in R
population <- rast("data/rasters/tza_ppp_2020_constrained.tif")
population


## ----plot1-------------------------------------------------------------------
plot(population)


## ----download, warning=FALSE, message=FALSE,echo = -2------------------------
#first we'll load the tanzania shapefile from the package
tz_districts <- getShp(country = "Tanzania", admin_level = c("admin2")) #this is an sf

#next we'll load the dataset of PfPR for the year 2022
pfpr_2022 <- getRaster(dataset_id = "Malaria__202406_Global_Pf_Parasite_Rate", year = 2022, shp = tz_districts)



## ----plot pfpr, warning=FALSE, message=FALSE---------------------------------
autoplot(pfpr_2022)
plot(pfpr_2022, main = "PfPR 2-10 in 2022")


## ----pfpr--------------------------------------------------------------------
pfpr_2022
pfpr_2022 <- pfpr_2022[[1]]

#the name of the raster is super long, so we'll fix that
names(pfpr_2022) <- "pfpr_2022"


## ----project-alternative-----------------------------------------------------
r <- rast(xmin=-110, xmax=-90, ymin=40, ymax=60, ncols=40, nrows=40)
values(r) <- 1:ncell(r)
r
plot(r)
newcrs <- "+proj=robin +datum=WGS84"
pr1 <- terra::project(r, newcrs)
crs(pr1)
plot(pr1)


## ----project-----------------------------------------------------------------
# Define target CRS
#we're going to use the Universal Mercator Projection (which makes the world flat)
target_crs <- "EPSG:3857"

# Reproject raster
projected_population <- project(population, target_crs, method = "bilinear") #bilinear because we assume population is continuous

plot(projected_population)


## ----ggplot------------------------------------------------------------------
ggplot()+
  geom_raster(data = pfpr_2022, mapping = aes(x=x, y =y, fill = pfpr_2022))+
  coord_equal()


## ----ggplot1-----------------------------------------------------------------
ggplot(tz_districts)+
  geom_sf()+
  geom_spatraster(data = pfpr_2022, mapping = aes(fill = pfpr_2022))+
  geom_sf(fill = NA)+
  scale_fill_distiller(palette = "RdYlGn", na.value = 'transparent')+
  theme_void()+
  labs(title = "Plasmodium falciparum 2-10 for 2022", fill = "PfPR")


## ----ggplot2-----------------------------------------------------------------
# Define color palette (5 bins = 5 colors)
pfpr_pal <- brewer.pal(n = 5, name = "RdYlGn")
pfpr_pal <- rev(pfpr_pal) #reverse it to make low green and high red

# Define break points
pfpr_breaks <- c(0, 0.05, 0.1, 0.2, 0.3, 1)

ggplot() +
  geom_spatraster(data = pfpr_2022, aes(fill = pfpr_2022)) +
  geom_sf(data = tz_districts, fill = NA) +
  scale_fill_stepsn(colours = pfpr_pal, breaks = pfpr_breaks, na.value = 0) +
  theme_void() +
  labs(title = "Plasmodium falciparum 2-10 for 2022", fill = "PfPR")

 


## ----sol1--------------------------------------------------------------------

ggplot(tz_districts)+
  geom_sf()+
  geom_spatraster(data = population, mapping = aes(fill = tza_ppp_2020_constrained))+
  geom_sf(fill = NA)+
  scale_fill_viridis_c(option = "D", na.value = "transparent", trans = "log10", direction = -1)+
  theme_void()+
  labs(title = "Population count for 2022", fill = "All age")



## ----crop--------------------------------------------------------------------
ext(pfpr_2022)
pfpr1 <- crop(pfpr_2022, c(-8,35,-6,25))   # c(xmin, xmax, ymin, ymax)
plot(pfpr1)

# crop malaria prevalence to just kilimanjaro
mtwara <- filter(tz_districts, name_1 == 'Mtwara')
mtwara_pfpr <- crop(pfpr_2022, mtwara)
plot(mtwara_pfpr)


## ----------------------------------------------------------------------------

# mask malaria prevalence to just kilimanjaro
mtwara <- filter(tz_districts, name_1 == 'Mtwara')
mtwara_pfpr <- crop(pfpr_2022, mtwara) %>% mask(mtwara) #reccomend to crop to set new extents
plot(mtwara_pfpr)



## ----aggregate, message = FALSE----------------------------------------------
population_1km <- aggregate(population, fact = 10, fun = "sum", na.rm=TRUE)
plot(population_1km)


## ----pop_risk, message = FALSE-----------------------------------------------
#check extent matches
ext(population_1km) == ext(pfpr_2022)

#if they don't match use resample to get them to match
population_1km_resamp <- resample(population_1km, pfpr_2022)

#Now we can multiply the population and prevalence information to get population at risk
pop_at_risk <- population_1km_resamp * pfpr_2022
names(pop_at_risk) = "population_at_risk"

ggplot()+
  geom_raster(pop_at_risk, mapping = aes(x = x, y = y, fill = population_at_risk))+
  geom_sf(tz_districts, mapping = aes(geometry = geometry), fill=NA)+
  scale_fill_viridis_c(option = "B", trans = "log10", na.value = "transparent")+
  theme_void()+
  coord_sf()+
  labs(title = "Population at risk in 2022", fill = "Population")


## ----sol2--------------------------------------------------------------------
# Define color palette (5 bins = 5 colors)
pfpr_pal <- brewer.pal(n = 5, name = "RdYlGn")
pfpr_pal <- rev(pfpr_pal) #reverse it to make low green and high red

# Define break points
pfpr_breaks <- c(0, 5, 10, 20, 30,100)

ggplot(tz_districts)+
  geom_sf()+
  geom_spatraster(data = pfpr_2022*100, mapping = aes(fill = pfpr_2022))+
  geom_sf(fill = NA)+
  scale_fill_stepsn(colours = pfpr_pal, breaks = pfpr_breaks, na.value = 0) +
  theme_void()+
  labs(title = "Plasmodium falciparum 2-10 for 2022", fill = "PfPR")



## ----extract-----------------------------------------------------------------
pop_risk <- terra::extract(pop_at_risk, vect(tz_districts), sum, na.rm=TRUE, ID = FALSE)

tz_districts <- bind_cols(tz_districts, pop_risk)

ggplot(tz_districts)+
  geom_sf(mapping = aes(fill = population_at_risk))+
  scale_fill_distiller(palette = "Reds", direction = 1, trans = 'log10', na.value = "lightblue")+
  theme_void()


## ----sol3--------------------------------------------------------------------

pfpr <- terra::extract(pfpr_2022, vect(tz_districts), mean, na.rm=TRUE, ID = FALSE)
pop <- terra::extract(population, vect(tz_districts), sum, na.rm=TRUE, ID = FALSE)

tz_districts <- bind_cols(tz_districts, pfpr, pop)

ggplot(tz_districts)+
  geom_sf(mapping = aes(fill = pfpr_2022))+
  scale_fill_distiller(palette = "RdYlGn", na.value = "lightblue")+
  theme_void()



## ----multiband---------------------------------------------------------------

#first we create a list of all the rasters for chirps
rainfall_rasters <- list.files(path = "data/rasters/", pattern = "chirps", full.names = TRUE)

#then we'll load it into R the same way we do a single band
rainfall_2022 <- rast(rainfall_rasters)
rainfall_2022


## ----plot multi--------------------------------------------------------------
plot(rainfall_2022)


## ----ggplot multi------------------------------------------------------------
#let's maybe first clean up the names, turn them into dates
names(rainfall_2022) <- seq(ym("2022-01"), ym("2022-12"), by = "months") %>% format("%b %Y")

ggplot()+
  geom_spatraster(data = rainfall_2022)+
facet_wrap(~lyr, ncol = 4)+
  scale_fill_distiller(palette = "Blues", direction = 1, na.value = "transparent", trans = 'sqrt')+
  theme_void()+
  labs(fill = "mm", title = "Rainfall")


## ----sol4--------------------------------------------------------------------

rainfall <- terra::extract(rainfall_2022, tz_districts, sum, na.rm=TRUE, ID = FALSE)

tz_rainfall <- tz_districts %>% 
  bind_cols(rainfall)

tz_rainfall %>% 
  pivot_longer(cols = `Jan 2022`:`Dec 2022`, names_to = "date", values_to ="rain") %>% 
ggplot()+
  geom_sf(mapping = aes(fill = rain/1000))+
  facet_wrap(~date)+
  scale_fill_distiller(palette = "Blues", direction = 1, na.value = "transparent", trans = 'sqrt')+
  theme_void()+
  labs(fill = "m", title = "Rainfall")




## ----rasterize---------------------------------------------------------------
rasterise_pop_risk <- rasterize(tz_districts, field = "population_at_risk", pop_at_risk)
plot(rasterise_pop_risk)


## ----export, eval=FALSE------------------------------------------------------
## writeRaster(pop_at_risk, "data/rasters/population_risk_2022.tif")

