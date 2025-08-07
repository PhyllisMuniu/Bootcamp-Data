rm(list=ls())
#Loading the Libraries
library(tmap)
library(RColorBrewer)
library(lubridate)
library(spdep)
library(tmap)
library(gganimate)
library(gifski)
library(viridis)
library(transformr)
library(ggspatial)
library(mapview)
library(ggplot2)
library(dplyr)
library(sf)
library(tidyverse)
library(maptiles)
library(sf)

#Loading the data
data <- read.csv("D:/Workshop/covid_2020.csv")
head(data)
# Read the shapefile
shp <- st_read("D:/Workshop/shp_africa/Africa_Countries.shp")
#Check column names to find the correct name field
names(shp)
head(shp,20)
view(data)
# Plotting the Shapefile
ggplot() +
  geom_sf(data = shp,
          fill = "lightblue",
          color = "darkblue",
          lwd = 0.5) +
  theme_minimal()
#Adding the names
ggplot() +
  geom_sf(data = shp,
          fill = "lightblue",
          color = "darkblue",
          lwd = 0.5) +
  theme_minimal()+
geom_sf_text(data = shp,  
             aes(label = NAME),  
             color = "black",
             size = 3)

# View unique country names to identify exact spelling or case
unique(shp$NAME)  # or shp$COUNTRY, shp$ADMIN, or similar column depending on your data

# Subset Kenya (assuming the country name column is 'NAME')
kenya <- shp[shp$NAME == "Kenya", ]

# Or using dplyr for clarity

kenya <- shp %>% filter(NAME == "Kenya")

# Plot to check
plot(st_geometry(kenya))

# If missing CRS, assign WGS84
if (is.na(st_crs(shp))) {
  st_crs(shp) <- 4326
}
shp_web <- st_transform(shp, 3857)
basemap <- get_tiles(shp_web, provider = "Esri.WorldStreetMap")

names(shp_web)
sum(is.na(st_geometry(shp_web)))



# Get a street basemap (can use other styles like "osm", "cartolight", "streets")
basemap <- get_tiles(shp_web, provider = "Esri.WorldStreetMap")

# Plot
ggplot() +
  layer_spatial(basemap) +
  geom_sf(data = shp_web, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf_text(data = shp_web, aes(label = NAME), size = 3, color = "black") +  # Replace NAME with your country name column
  theme_minimal()

#colnames(data)[colnames(data) == "country"] <- "NAME" 
# Renaming the columns
dzata <- data %>%
  rename("Date" = "date", "NAME" = "country")
head(dzata)  


colnames(dzata)
gg_data <- dzata %>% select(c("Date","NAME","lat","long","cases","months","population"))
shpp <- shp_web %>% select(c("NAME","geometry"))
#Joining the csv and shp data
df <- left_join(gg_data,shpp, by="NAME")
#the nas
sum(is.na(df))
head(df)

# Make sure `df` is an sf object
df <- st_as_sf(df)


# Step 1: Create case category
df$case_category <- cut(df$cases,
                        breaks = c(-Inf, 100, 200, 300, 400, Inf),
                        labels = c("<100", "100-200", "200-300", "300-400", ">400"),
                        right = TRUE)

# Step 2: Loop through each month and save individual maps
unique_months <- unique(df$months)

for (month in unique_months) {
  df_month <- df %>% filter(months == month)
  
  p <- ggplot(data = df_month) +
    geom_sf(aes(fill = case_category), color = "grey40") +
    scale_fill_brewer(palette = "YlOrRd", name = "Cases", drop = FALSE) +
    theme_minimal() +
    labs(title = paste("COVID-19 Cases in Africa -", month),
         caption = "Data Source: Your COVID Dataset") +
    theme(
      strip.text = element_text(size = 10),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()  # ??? Remove grid lines
    )

  
  # Save each plot
  ggsave(filename = paste0("covid_cases_", gsub(" ", "_", month), ".png"),
         plot = p,
         width = 8, height = 6, dpi = 300)
}





#Animated 

# Convert Date column properly
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")

# Replace zeros or negatives for log scale
df$cases <- as.numeric(df$cases)
df$cases[df$cases <= 0] <- 1e-3

# Animated plot
anim <- ggplot(df) +
  geom_sf(aes(geometry = geometry, fill = case_category), color = "grey60") +
  scale_fill_brewer(palette = "YlOrRd", name = "Cases", drop = FALSE) +
  labs(
    title = "COVID-19 Cases in Africa: {frame_time}",
    fill = "Cases",
    caption = "Source: Your Dataset"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  transition_time(Date) +
  ease_aes('linear') +
  transition_time(Date) +    # Use corrected Date column
  ease_aes('linear')

animate(anim, renderer = gifski_renderer(loop = TRUE), nframes = 100, fps = 10)

anim_save("covid_africa.gif", animation = last_animation())




