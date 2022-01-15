# Animation libraries
library(gganimate)
library(gifski)
library(transformr)

# Create sf points of ztrax in buffered convex hull of perimeter
pts <- ztrax.res.hull %>% 
  dplyr::mutate(BuildingAreaSqFt = as.double(BuildingAreaSqFt)) %>%
  dplyr::select(RowID, YearBuilt, BuildingAreaSqFt)
min(pts$YearBuilt)
max(pts$YearBuilt)

# Create static map
static <- ggplot() +
  geom_sf(data = evt, fill="grey90", size = 0.8, col = NA) +
  geom_sf(data = pts, aes(color=YearBuilt), size=0.2) +
  scale_color_viridis_c(option = "plasma", limits = c(1864,2019), 
                        breaks = c(1864, 1900, 1950, 1990, 2000, 2019),) +
  guides(color = guide_colourbar(position="top", barwidth = 0.5, barheight = 12.5, ticks=F,
                                 label.position = "right", title.position = "left",
                                 label.theme = element_text(angle = -25, size=12))) +
  geom_sf(data = evt, fill=NA, size = 1, col = "#8c2d04") +
  ggspatial::annotation_scale(height=unit(1.2, "mm")) +
  labs(title="Residential Housing Development (Marshall Fire Area)",
       subtitle="Data Source: Zillow Transaction and Assessment Database (ZTRAX)",
       color="Year Built") +
  theme_void() +
  theme(plot.title = element_text(size=20),
        plot.subtitle = element_text(size=16),
        legend.title = element_text(angle = 90, size=14))
static

# Set the environment
setwd('figures')

# Animate it
animap <- static +
  transition_time(YearBuilt) +
  labs(caption = "Year: {frame_time}\nFrame {frame} of {nframes}") +
  gganimate::enter_recolor(fill = "#f0f5f9") +
  gganimate::shadow_mark(past = TRUE, alpha = 1) +
  theme(plot.caption = element_text(size=18))
# Grab number of frames
num_years <- max(pts$YearBuilt) - min(pts$YearBuilt) + 1
gganimate::animate(
  animap, nframes = num_years, renderer = gifski_renderer(),
  width = 850, height=600)
# Save GIF
anim_save("MarshallFire_Development_Animation.gif")




##############################################################
# For Boulder County
boco <- boco %>% st_transform(st_crs(ztrax.res))
pts.boco <- ztrax.res %>% 
  dplyr::select(RowID, YearBuilt) %>%
  mutate(YearBuilt = as.integer(YearBuilt)) %>%
  filter(!is.na(YearBuilt)) %>%
  st_intersection(., boco)
min(pts.boco$YearBuilt)
max(pts.boco$YearBuilt)

# Create static map
static <- ggplot() +
  geom_sf(data = boco, fill="grey90", size = 0.8, col = NA) +
  geom_sf(data = pts.boco, aes(color=YearBuilt), size=0.2) +
  scale_color_viridis_c(option = "plasma", limits = c(1860,2019), 
                        breaks = c(1860, 1900, 1950, 1990, 2000, 2019)) +
  guides(color = guide_colourbar(position="top", barwidth = 0.5, barheight = 12.5, ticks=F,
                                 label.position = "right", title.position = "left",
                                 label.theme = element_text(angle = -25, size=12))) +
  geom_sf(data = boco, fill=NA, size = 1, col = "#8c2d04") +
  geom_sf(data = boco.cities, size=2.25, shape=10) +
  # geom_sf_text(
  #   data = boco.cities, aes(label = NAME), fontface = "bold", size=6) +
  # coord_sf(crs = st_crs(boco)) +
  ggspatial::annotation_scale(height=unit(1.2, "mm")) +
  labs(title="Residential Housing Development  (Boulder County, CO)",
       subtitle="Data Source: Zillow Transaction and Assessment Database (ZTRAX)",
       color="Year Built") +
  theme_void() +
  theme(plot.title = element_text(size=16, color="black"),
        plot.subtitle = element_text(size=16, color="black"),
        legend.title = element_text(angle = 90, size=14, color="black"),
        legend.position = "left")
static

# Animate it
animap <- static +
  transition_time(YearBuilt) +
  labs(caption = "Year: {frame_time}\nFrame {frame} of {nframes}") +
  gganimate::enter_recolor(fill = "#f0f5f9") +
  gganimate::shadow_mark(past = TRUE, alpha = 1) +
  theme(plot.caption = element_text(size=18))
# Grab number of frames
num_years <- max(pts$YearBuilt) - min(pts$YearBuilt) + 1
# Animate
gganimate::animate(
  animap, nframes = num_years, renderer = gifski_renderer(),
  width = 800, height=650)
# Save GIF
anim_save("BoulderCounty_Development_Animation.gif")