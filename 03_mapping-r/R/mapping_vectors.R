## ----echo=FALSE, out.width="100%"-------------------------------------------------------------
knitr::include_graphics("figs/points-lines-polygons-vector-data-types.png")


## ----echo=FALSE, out.width="100%"-------------------------------------------------------------
knitr::include_graphics("figs/raster_concept.png")


## ----echo=FALSE, out.width="100%"-------------------------------------------------------------
knitr::include_graphics("figs/Spatial_Layers.png")


## ----echo=FALSE, out.width="100%"-------------------------------------------------------------
knitr::include_graphics("figs/crs.png")


## ----echo=FALSE, out.width="100%"-------------------------------------------------------------
knitr::include_graphics("figs/spatial-projection-transformations-crs.png")


## ----libraries, warning = FALSE,message=FALSE-------------------------------------------------
library(sf)
library(tmap)
library(ggspatial)
library(ggrepel)
library(tidyverse)
library(malariaAtlas)


## ----echo=FALSE, out.width="100%"-------------------------------------------------------------
knitr::include_graphics("figs/shapefiles_in_r.png")


## ----read_dat, message=FALSE------------------------------------------------------------------
tz_admin1 <- st_read("data/shapefiles/TZ_admin1.shp")


## ----sf1, results ='hide',message=FALSE, warning = FALSE--------------------------------------
tz_admin1


## ----figurename3, echo=FALSE, fig.cap="", out.width = '90%'-----------------------------------
  knitr::include_graphics("figs/sf_features.png")


## ----make points------------------------------------------------------------------------------
tz_pr <- read_csv("data/pfpr-mis-tanzania-clean.csv")

# Make the gps data into a point simple feature
tz_pr_points <- st_as_sf(tz_pr, coords = c("longitude", "latitude"), crs = 4326)


## ----plot1------------------------------------------------------------------------------------
tz_region <- ggplot(tz_admin1)+
  geom_sf()+
  theme_bw()+
  labs(title = "Tanzania Regions")


## ----plot2------------------------------------------------------------------------------------
ggplot()+
  geom_sf(tz_admin1, mapping = aes(geometry = geometry))+
  geom_point(tz_pr, mapping = aes(x = longitude, y = latitude, color = pf_pr))+
  scale_color_distiller(palette = "Spectral")+
  theme_bw()


## ----join, message=FALSE----------------------------------------------------------------------
tz_pop_adm1 = read_csv("data/tza_admpop_adm1_2020_v2.csv") %>% 
  #Change the characters from upper to title style
  mutate(name_1 = str_to_title(ADM1_EN)) %>% 
  #Some names completely don't match so manually change them
  mutate(name_1 = case_when(name_1 == "Dar Es Salaam" ~ "Dar-es-salaam",
                            name_1 == "Pemba North" ~ "Kaskazini Pemba",
                            name_1 == "Pemba South" ~ "Kusini Pemba",
                            name_1 == "Zanzibar North" ~ "Kaskazini Unguja",
                            name_1 == "Zanzibar Central/South" ~ "Kusini Unguja",
                            name_1 == "Zanzibar Urban/West" ~ "Mjini Magharibi",
                            name_1 == "Coast" ~ "Pwani",
                            TRUE ~ as.character(name_1) #be sure to include this or it will turn name_1 NA
                            ))

#check if names in the columns match
table(tz_admin1$name_1 %in% tz_pop_adm1$name_1)


## ----join2------------------------------------------------------------------------------------
tz_pop_admin1 <- tz_admin1 %>% left_join(tz_pop_adm1, by = "name_1") 

ggplot(tz_pop_admin1)+
  geom_sf(mapping = aes(fill = T_TL))+
  #use na.value to make the lakes appear as lightblue
  scale_fill_viridis_c(option = "B", na.value = "lightblue", trans = 'sqrt')+
  theme_bw()+
  labs(fill = "Total Population")



## ----sf_join, message=FALSE-------------------------------------------------------------------
sf_use_s2(FALSE)
tz_pr_point_region <- st_join(tz_pr_points, tz_admin1)


## ----sf_join_2, message = FALSE---------------------------------------------------------------

tz_region_map <- tz_pr_point_region %>% 
  ungroup() %>% #run this to remove any previous groupings that occured
  group_by(name_1) %>% 
  summarise(mean_pr = mean(pf_pr, na.rm=TRUE)) %>% 
  st_drop_geometry() %>% 
   #we put a "." to indicate where the data we've been working with goes for left join
  left_join(tz_admin1, .) %>% 
  ggplot()+
  geom_sf(mapping = aes(fill = mean_pr))+
  scale_fill_distiller(palette = "Spectral", na.value = "lightblue")+
  labs(fill = "Mean PfPR 0-5 years", title = "Tanzania Regions", subtitle = "MIS 2017")+
  theme_bw()

tz_region_map


## ----buffer1----------------------------------------------------------------------------------
tz_pf_buffer_5km <- st_buffer(tz_pr_points, dist = 0.2) #20km is approx 0.2 decimal degree close to the equator

tz_region+
  geom_sf(tz_pf_buffer_5km, mapping = aes(geometry = geometry))+
  geom_point(tz_pr, mapping = aes(x = longitude, y = latitude, color = pf_pr), size = 0.5)+
  scale_color_distiller(palette = "Spectral")+
  labs(color = "PfPR 0-5 years", subtitle = "MIS 2017")



## ---------------------------------------------------------------------------------------------
st_crs(tz_admin1)
st_crs(tz_pr_points)


## ----out.width='50%', fig.show='hold', fig.align='default'------------------------------------
# Change the projection to UTM zone 35S
tz_admin1_utm <- st_transform(tz_admin1, 32735)
tz_pr_points_utm <- st_transform(tz_pr_points, 32735)


## ----north arrow and scale bar----------------------------------------------------------------
publicaton_pr_map <- tz_region_map+
  annotation_north_arrow(
    location = 'tr', #put top right
    height = unit(0.5, "cm"), 
    width = unit(0.5, "cm")
  )+
  annotation_scale(
    location = 'bl', #bottom left
  )+
  theme_void()

publicaton_pr_map


## ----labels-----------------------------------------------------------------------------------
publicaton_pr_map+
  geom_sf_text(mapping = aes(label = name_1), size = 1.5)


## ----save outputs-----------------------------------------------------------------------------
ggsave(filename = "tanzania_pr_map_2017.png")


## ----save, eval=FALSE-------------------------------------------------------------------------
# st_write(tz_pop_admin1, "data/shapefiles/tz_population_admin1.shp")


## ----tmap1------------------------------------------------------------------------------------
tm_shape(tz_admin1) +
  tm_polygons()


## ----tmap-pop---------------------------------------------------------------------------------
tm_shape(tz_pop_admin1) +
  tm_polygons("T_TL", palette = "viridis", title = "Population",
               style = 'pretty', n = 4,
              colorNA = 'lightblue', textNA = "lakes")


## ----tmap-pop1--------------------------------------------------------------------------------
tm_shape(tz_pop_admin1) +
  tm_polygons("T_TL", palette = "viridis", title = "Population",
               style = 'pretty', n = 4,
              colorNA = 'lightblue', textNA = "lakes")+
  tm_text(text = "name_1", size = 0.5)+
  tm_layout(legend.outside = TRUE)


## ----interactive------------------------------------------------------------------------------
tmap_mode("view")

 tm_shape(tz_pop_admin1) +
  tm_polygons("T_TL", 
              id="name_1", #added for the labels in interactive to show region
              palette = "viridis", title = "Population",
               style = 'pretty', n = 4,
              colorNA = 'lightblue', textNA = "lakes")+
  #tm_text(text = "name_1", size = 0.5)+
  tm_layout(legend.outside = TRUE)


