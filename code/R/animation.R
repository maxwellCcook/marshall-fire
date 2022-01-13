# Animation
library(gganimate)
library(gifski)
library(transformr)

# Create sf points
pts <- ztrax.res.hull %>% 
  dplyr::mutate(XY = st_coordinates(.)[,2]) %>%
  dplyr::select(RowID, YearBuilt, XY)

# Create static map
static <- ggplot() +
  geom_sf(data = pts, size=0.75) +
  geom_sf(data = evt, fill=NA, size = 1.2, col = "#3a6589") +
  theme_void()
static

# Set the environment
setwd('figures')
# Animate it
animap <- static +
  transition_time(YearBuilt) +
  ggtitle('Year: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}') +
  gganimate::enter_recolor(fill = "#f0f5f9") +
  gganimate::shadow_mark(past = TRUE, alpha = 1)
num_years <- max(pts$YearBuilt) - min(pts$YearBuilt) + 1
gganimate::animate(
  animap, nframes = num_years, renderer = gifski_renderer(),
  height = 450, width = 700)
# Save GIF
anim_save("MarshallFire_Development_Animation.gif")

