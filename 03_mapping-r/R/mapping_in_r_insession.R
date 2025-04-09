#welcome to mapping in R

#install.packages("tidyverse", "sf","malariaAtlas","tmap", "ggspatial", "ggrepel")
install.packages("ggspatial")

#load packages we want to use
library(sf)
library(tmap)
library(ggspatial)
library(ggrepel)
library(tidyverse)
library(malariaAtlas)

#import shapefile in R
tz_admin1 <- st_read("data/shapefiles/TZ_admin1.shp")
tz_admin1

#load in point data
tz_pr <- read_csv("data/pfpr-mis-tanzania-clean.csv")

#convert the point data to spatial point data
tz_pr_points <- st_as_sf(tz_pr, coords = c("longitude", "latitude"), crs = 4326)
tz_pr_points
names(tz_pr_points)

#coordinate reference system
st_crs(tz_admin1)

#plot tanzania as a country
ggplot(tz_admin1)+
  geom_sf()+
  theme_bw()+
  labs(title = "Tanzania Regions")

#plot tanzania and the prevalence points on a map
ggplot()+
  geom_sf(tz_admin1, mapping = aes(geometry = geometry))+
  geom_point(tz_pr, mapping = aes(x = longitude, y = latitude, color = pf_pr))+
  scale_color_distiller(palette = "Spectral")+
  theme_bw()

#plot tanzania and prevalence points using only shapefiles
ggplot()+
  geom_sf(tz_admin1, mapping = aes(geometry = geometry))+
  geom_sf(tz_pr_points, mapping = aes(geometry = geometry, color = pf_pr))+
  scale_color_distiller(palette = "Spectral")+
  theme_minimal()+
  labs(title = "Malaria Prevalence U5 - MIS Tanzania 2017",
       color = "PfPR")

#Joining data to shapefile
tz_pop_admin1 <- read_csv("data/tza_admpop_adm1_2020_v2.csv")
tz_pop_admin1
tz_admin1
unique(tz_admin1$name_1)
unique(tz_pop_admin1$ADM1_EN)

#first we are going to clean the population data to match the shapefile names for regions
tz_pop_admin1 <- tz_pop_admin1 %>% 
                #change the character from upper case to Proper/Title case
                mutate(name_1 = str_to_title(ADM1_EN)) %>% 
  # fix manually some of the names that don't match
  mutate(name_1 = case_when(name_1 == "Dar Es Salaam" ~ "Dar-es-salaam",
                            name_1 == "Pemba North" ~ "Kaskazini Pemba",
                            name_1 == "Pemba South" ~ "Kusini Pemba",
                            name_1 == "Zanzibar North" ~ "Kaskazini Unguja",
                            name_1 == "Zanzibar Central/South" ~ "Kusini Unguja",
                            name_1 == "Zanzibar Urban/West" ~ "Mjini Magharibi",
                            name_1 == "Coast" ~ "Pwani",
                            TRUE~ as.character(name_1)
                            ))

unique(tz_pop_admin1$name_1)
unique(tz_admin1$name_1)

table(tz_admin1$name_1 %in% tz_pop_admin1$name_1)

tz_region <- filter(tz_admin1, type_1=="Region")
table(tz_region$name_1 %in% tz_pop_admin1$name_1)

#now that the population is clean, i can join the data to the polygon shapefile

tz_pop_admin1_shp <- tz_admin1 %>% 
                    left_join(tz_pop_admin1, by = "name_1")
tz_pop_admin1_shp

ggplot(tz_pop_admin1_shp)+
  geom_sf(mapping = aes(fill = T_TL))+
  scale_fill_viridis_c(option = "B", na.value = "lightblue",trans = 'sqrt')+
  theme_minimal()+
  labs(title = "Population in Tanzania in 2020",
       fill = "Total Population")

?scale_fill_viridis_c

#challenge: can you make the plot of population into bins

#extract region names for prevalence points in Tanzania
sf_use_s2(FALSE)
tz_pr_point_regions <- st_join(tz_pr_points, tz_admin1)
tz_pr_point_regions
?st_join

names(tz_pr_point_regions)

tz_region_map_pr <- tz_pr_point_regions %>% 
  ungroup() %>% #just incase there are any groups
  group_by(name_1) %>% 
  summarise(mean_pr = mean(pf_pr, na.rm = TRUE)) %>% 
  st_drop_geometry() %>% 
  left_join(tz_admin1, ., by = "name_1")
  
#joined_data <-  left_join(tz_admin1, tz_region_map_pr)

#plot the data by region for prevalence
pr_region_plot <- ggplot(tz_region_map_pr)+
  geom_sf(mapping = aes(fill = mean_pr))+
  scale_fill_distiller(palette = "Spectral", na.value = "lightblue")+
  theme_bw()+
  labs(fill = "Mean PfPR 0-5 years",
       Title = "Prevalence in Tanzania Regions",
       subtitle = "MIS 2017")

#interactive maps using tmap
library(tmap)
#plot the regions of tanzania
tm_shape(tz_admin1)+
  tm_polygons()

#plot population in tanzania
tm_shape(tz_pop_admin1_shp)+
  tm_polygons("T_TL",palette = 'inferno', title = "Population", colorNA = "lightblue", style = "pretty", textNA = "lakes", n= 3)+
  tm_layout(legend.outside = TRUE)

#making this interactive
tmap_mode("view")

tm_shape(tz_pop_admin1_shp)+
  tm_polygons("T_TL", palette = "magma", title = "Total Population", colorNA = "lightblue", style = "pretty", n= 4)+
  tm_layout(legend.outside = TRUE)

#export shapefile
st_write(tz_region, "data/shapefiles/tz_regions_only.shp",append = TRUE)

pr_region_plot
ggsave("tz_pr_map.png",pr_region_plot,  width = 8, height = 8)
