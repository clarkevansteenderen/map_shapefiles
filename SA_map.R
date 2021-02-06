###########################################################
# Using shape files of South Africa to plot a map with 
# biomes and GPS points
###########################################################

library(pacman)

pacman::p_load(tidyverse, 
               spocc, 
               ggmap, 
               maptools, 
               maps, 
               ggplot2,
               scrubr, 
               mapr, 
               tidyr, 
               stringr,
               rnaturalearth, 
               rnaturalearthdata, 
               rlang, 
               sf, 
               ggspatial,
               raster,
               stars,
               here)

# set the ggplot theme

theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", 
                                              fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), 
                                                            "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), 
                                                            "mm")),
                  legend.position = "none"))

# import the shape file
biome_shp <- st_read("C:/Users/s1000334/Documents/Guy/R code/vegm2006_biomes_withforests/vegm2006_biomes_withforests.shp")

# Plot basic biome map
ggplot(data = biome_shp) +
  # This will plot a map
  geom_sf()

str(biome_shp)

# convert shapefile into a dataframe (i.e. tibble)
biome_df <- as_tibble(biome_shp)

biome_df %>%
  distinct(BIOME)

# What levels are present within BIOMENAME?
biome_df %>%
  distinct(BIOMENAME)

###########################################################
# Plot SA map coloured by biome  
###########################################################

# Plot basic biome map coloured by biome 
ggplot(data = biome_shp) +
  # Colours by biome, alpha makes colour a bit transparent
  geom_sf(aes(fill = BIOMENAME),
          alpha = 0.3) +
  # We want a legend on the right side of the graph
  theme(legend.position = "right") +
  # Add a colour palette of yellow, orange and brown
  scale_fill_brewer(palette = "Accent") +
  # Add x and y-axis labels, and change legend title 
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "Biome") + 
  # Limit y and x axis limits 
  coord_sf(xlim = c(15.5, 33.5), 
           ylim = c(-35, -21.75), 
           expand = FALSE) +
  # Limit y and x axis limits (western cape only)
  # coord_sf(xlim = c(17, 25.5), 
  #         ylim = c(-35, -31), 
  #         expand = FALSE) +
  # Add a scale bar
  annotation_scale(location = "br",  
                   style = "ticks", 
                   width_hint = 0.150) +
  # Add a north arrow 
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.175, "in"), 
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

###########################################################
# Plot SA map coloured by biome, with province borders
###########################################################

# read in shape file for provincial borders
province_shp <- st_read("C:/Users/s1000334/Documents/Guy/R code/ZAF_adm/ZAF_adm1.shp") 
province_shp$NAME_1

# predefine the colours for each biome

cols <- c("Albany Thicket" = "darkgreen", 
          "Desert" = "orange", 
          "Forests" = "black", 
          "Fynbos" = "steelblue2",
          "Grassland" = "grey80",
          "Indian Ocean Coastal Belt" = "blue",
          "Nama-Karoo" = "pink",
          "Savanna" = "khaki",
          "Succulent Karoo" = "darkolivegreen3")

# plot the new map:

ggplot(data = biome_shp) +
  geom_sf(aes(fill = BIOMENAME),
          alpha = 1) +
  ###############
# Only new bit
geom_sf(data = province_shp, aes(fill = NA),
        # Colour of the outline
        colour = "black",
        # Width of the province border lines
        size = 0.5) +
  # ###########
theme(legend.position = "right") +
  #scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = cols) +
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "Biome") + 
  coord_sf(xlim = c(15.5, 33.5), 
           ylim = c(-35, -21.75), 
           expand = FALSE) +
  annotation_scale(location = "br",  
                   style = "ticks", 
                   width_hint = 0.150) +
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.175, "in"), 
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

###########################################################
# Plot GPS coordinates onto the map
###########################################################

# Import GPS points from excel file on GitHub
sample_gps <- readr::read_csv("https://raw.githubusercontent.com/CJMvS/map_shapefiles/main/tetramesa_sample_info.csv")

# Check data import 
head(sample_gps)

# Clean the column names
sample_gps <- sample_gps %>%
  janitor::clean_names()

head(sample_gps)

# find the distinct host plants (ie. factors)
sample_gps %>% distinct(host)

# plot the map with the added GPS coordinates as points
ggplot() +
  geom_sf(data = biome_shp,
          aes(fill = BIOMENAME),
          alpha = 0.8) +
  geom_sf(data = province_shp, 
          aes(fill = NA),
          colour = "black",
          size = 0.35) +
  # Add the points onto the map
  geom_point(data = sample_gps, aes(x = longitude, 
                                    y = latitude,
                                    colour = host
  ),
  size = 6, shape = 16) +
  # Colour the points manually according to host plant:
  # E. curvula = red, S. pyramidalis = blue and H. hirta = green
  scale_colour_manual(values = c("red", "blue", "green")) +
  theme(legend.position = "right") +
  # Manually specifcy fill colours for different biomes
  scale_fill_manual(values = cols) + 
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "Biome",
       colour = "Host Plant") + 
  coord_sf(xlim = c(15.5, 33.5), 
           ylim = c(-35, -21.75), 
           expand = FALSE) +
  annotation_scale(location = "br",  
                   style = "ticks", 
                   width_hint = 0.150) +
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.325, "in"), 
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)
