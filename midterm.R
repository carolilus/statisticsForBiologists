setwd("C:/Users/lu1280ca/Work Folders/Desktop/R_LAG/statisticsForBiologists/")
dat = read.csv("exam2023_data-2.csv")
str(dat)
dat$Season = as.factor(dat$Season)
dat$Property=as.factor(dat$Property)
dat$Landscape_position=as.factor(dat$Landscape.position)
head(dat)


#### CREATE A DECENT MAP ####
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
#translate coordinates to Lat-Lon
spatial_data=st_as_sf(dat, 
                         coords = c("Easting", "Northing"), 
                         crs = 28355) 
geographic_data=st_transform(spatial_data, crs = 4326)
coords=st_coordinates(geographic_data)
dat$Longitude=coords[, "X"]
dat$Latitude=coords[, "Y"]

# adding some features to the map (lakes, cities, state lines)
world_map <- ne_countries(scale = "large", returnclass = "sf")
lakes = ne_download(scale = 10, type = "lakes", category = "physical")
aus_states <- ne_states(country = "Australia", returnclass = "sf")
world_cities <- maps::world.cities %>%
  filter(country.etc == "Australia")
cities_to_label <- world_cities %>%
  filter(name %in% c("Melbourne", "Sydney", "Adelaide", "Canberra", "Ballarat", "Geelong")) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)
australia_map <- world_map[world_map$name == "Australia", ]
points_sf <- st_as_sf(dat, 
                      coords = c("Longitude", "Latitude"), 
                      crs = 4326)
# crop the map to sampling area
vic_lon_min <- 143 
vic_lon_max <- 147.5
vic_lat_min <- -39.2
vic_lat_max <- -35.5
# plot sampling area
ggplot() +
  geom_sf(data = australia_map, 
          linewidth = 0.5) +
  geom_sf(data = aus_states, 
          fill = "#FFFFB3",  
          color = "grey50",  
          linewidth = 0.5) +
  geom_sf(data = aus_states %>% filter(name == "Victoria"), 
          fill="#FFFFB3",
          color = "grey50",
          linewidth = 0.9)+ 
  geom_sf(data = lakes, 
          fill = "lightblue", 
          color = "grey50", 
          linewidth = 0.5) +
  #geom_sf(data = points_sf, 
  #        color = "green4", 
  #        size = 2, 
  #        alpha = 0.7) +
  geom_point(data = dat, aes(x=Longitude, y=Latitude, color=Landscape.position),
             size = 2, 
             alpha = 0.7) +
  geom_sf(data = cities_to_label,
          color="black",
          size= 1)+
  geom_sf_text(data = cities_to_label,
               aes(label = name),
               size = 3.5,
               color = "black",
               fontface = "bold",
               nudge_y = -0.1,
               check_overlap = TRUE) +
  coord_sf(xlim = c(vic_lon_min, vic_lon_max), 
           ylim = c(vic_lat_min, vic_lat_max), 
           expand = FALSE) +
  labs(title = "survey sites in Victoria",
       x = "Longitude", 
       y = "Latitude") +
  theme(panel.grid.major = element_line(color = "grey80", linetype = "dashed", linewidth = 0.3),
        panel.background = element_rect(fill = "white"))


# crop the map to victoria state 
vic_lon_min <- 138 
vic_lon_max <- 152
vic_lat_min <- -39.7
vic_lat_max <- -33
#plot south-east australia
ggplot() +
  geom_sf(data = australia_map, 
          linewidth = 0.5) +
  geom_sf(data = aus_states, 
          fill = "#FFFFB3",  
          color = "grey50",  
          linewidth = 0.5) +
  geom_sf(data = aus_states %>% filter(name == "Victoria"), 
          fill="#FFFFB3",
          color = "grey50",
          linewidth = 0.9)+ 
  geom_sf(data = lakes, 
          fill = "lightblue", 
          color = "grey50", 
          linewidth = 0.5) +
  geom_point(data = dat, aes(x=Longitude, y=Latitude, color=Season),
          size = 2, 
          alpha = 0.7) +
 # geom_sf(data = cities_to_label,
          #color="black",
          #size= 1)+
  #geom_sf_text(data = cities_to_label,
               #aes(label = name),
               #size = 3.5,
               #color = "black",
               #fontface = "bold",
               #nudge_y = -0.1,
               #check_overlap = TRUE) +
  coord_sf(xlim = c(vic_lon_min, vic_lon_max), 
           ylim = c(vic_lat_min, vic_lat_max), 
           expand = FALSE) +
  labs(title = "survey sites",
       x = "Longitude", 
       y = "Latitude") +
  theme(panel.grid.major = element_line(color = "grey80", linetype = "dashed", linewidth = 0.3),
        panel.background = element_rect(fill = "lightblue"))


#### MODEL FITTING ####
library(lme4)
# Young trees: dat$euc_sdlgs0_50cm, small seedlings < 50 cm

# Old trees: dat$euc_sdlgs.2m + dat$euc_sdlgs50_2m, saplings from 50 cm onwards
dat$euc_sdlgs50plus = dat$euc_sdlgs50cm.2m + dat$euc_sdlgs.2m

# See that the distributions of the counts are very skewed
hist(dat$euc_sdlgs0_50cm)
hist(dat$euc_sdlgs50plus)

# check that the variance is much bigger than the mean:
# young trees
mean_seedlings <- mean(dat$euc_sdlgs0_50cm, na.rm = TRUE)
variance_seedlings <- var(dat$euc_sdlgs0_50cm, na.rm = TRUE)
dispersion_ratio_raw <- variance_seedlings / mean_seedlings
print(paste("Mean Total Seedlings:", round(mean_seedlings, 2)))
print(paste("Variance Total Seedlings:", round(variance_seedlings, 2)))
print(paste("Variance to Mean Ratio:", round(dispersion_ratio_raw, 2)))
# old trees
mean_seedlings <- mean(dat$euc_sdlgs50plus, na.rm = TRUE)
variance_seedlings <- var(dat$euc_sdlgs50plus, na.rm = TRUE)
dispersion_ratio_raw <- variance_seedlings / mean_seedlings
print(paste("Mean Total Seedlings:", round(mean_seedlings, 2)))
print(paste("Variance Total Seedlings:", round(variance_seedlings, 2)))
print(paste("Variance to Mean Ratio:", round(dispersion_ratio_raw, 2)))

library(lme4)
dat$BareGround_scaled=scale(dat$BareGround_cover)
dat$Grass_scaled= scale(dat$NativePerennialGrass_cover)
dat$Fern_scaled= scale(dat$NativePerennialFern_cover)
dat$Dist_scaled=scale(dat$Distance_to_Eucalypt_canopy.m.)
dat$Precip_scaled =scale(dat$annual_precipitation)
dat$K_perc_scaled = scale(dat$K_perc)
dat$SRad_Jan_scaled = scale(dat$SRad_Jan)

#run the Model using the SCALED variables, because the algorithm otherwise does not converge
m_young_nb_scaled <- glmer.nb(euc_sdlgs0_50cm ~
                                BareGround_scaled +
                                Grass_scaled +
                                Dist_scaled +
                                Precip_scaled +
                                K_perc_scaled+
                                SRad_Jan_scaled+
                                (1 | Property),
                              data = dat)
summary(m_young_nb_scaled)

m_old_nb_scaled=glmer.nb(euc_sdlgs50plus ~
                           BareGround_scaled +
                           Grass_scaled +
                           Dist_scaled +
                           Precip_scaled +
                           K_perc_scaled+
                           SRad_Jan_scaled+
                           (1 | Property),
                         data = dat)
summary(m_old_nb_scaled)

# calculating effects of most significant predictors (cannot extract them from the model object)
exp(-0.63)
exp(-0.52)


#### PLOT MOST SIGNIFICANT EFFECT ####
library(ggeffects)
library(ggplot2)

pred_dist <- ggpredict(m_young_nb_scaled, terms = "Dist_scaled [all]", type = "fixed") # 'fe' for fixed effects

ggplot(pred_dist, aes(x = x, y = predicted)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
  labs(title = "Predicted Seedling Count vs. Distance to Existing Canopy",
       x = "Distance to Canopy (Standard Deviations from Mean)",
       y = "Expected Number of Young Seedlings") +
  theme_minimal()

pred_dist <- ggpredict(m_old_nb_scaled, terms = "Dist_scaled [all]", type = "fixed") # 'fe' for fixed effects

ggplot(pred_dist, aes(x = x, y = predicted)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
  labs(title = "Predicted Sapling Count vs. Distance to Existing Canopy",
       x = "Distance to Canopy (Standard Deviations from Mean)",
       y = "Expected Number of Old Saplings") +
  theme_minimal()
