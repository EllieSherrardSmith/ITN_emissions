###########################################
##
## Mapping port routes
##
##############################
install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "ggrepel", "dplyr"))

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(dplyr)

library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(grid)  # for arrow()

# Load African countries
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# African Ports
ports <- data.frame(
  port = c("Lagos", "Mombasa", "Durban", "Dakar", "Abidjan", "Cotonou", "Dar es Salaam"),
  country = c("Nigeria", "Kenya", "South Africa", "Senegal", "Cote d'Ivoire", "Benin", "Tanzania"),
  lon = c(3.377, 39.67, 31.02, -17.44, -4.01, 2.42, 39.29),
  lat = c(6.45, -4.04, -29.87, 14.69, 5.33, 6.36, -6.82)
)

# Inland African capitals
capitals <- data.frame(
  city = c("Kigali", "Bujumbura", "Lusaka", "Lilongwe", "Ouagadougou", "Bamako", "Niamey"),
  country = c("Rwanda", "Burundi", "Zambia", "Malawi", "Burkina Faso", "Mali", "Niger"),
  lon = c(30.05, 29.36, 28.32, 33.79, -1.52, -7.99, 2.11),
  lat = c(-1.95, -3.37, -15.41, -13.97, 12.37, 12.65, 13.51)
)

# Inland Routes
routes <- data.frame(
  port = c("Dar es Salaam", "Dar es Salaam", "Durban", "Beira", "Abidjan", "Cotonou", "Lagos"),
  city = c("Kigali", "Bujumbura", "Lusaka", "Lilongwe", "Ouagadougou", "Niamey", "Bamako"),
  port_lon = c(39.29, 39.29, 31.02, 34.83, -4.01, 2.42, 3.377),
  port_lat = c(-6.82, -6.82, -29.87, -19.82, 5.33, 6.36, 6.45),
  city_lon = c(30.05, 29.36, 28.32, 33.79, -1.52, 2.11, -7.99),
  city_lat = c(-1.95, -3.37, -15.41, -13.97, 12.37, 13.51, 12.65)
)

# International shipping origins (China & Vietnam)
international_ports <- data.frame(
  city = c("Shanghai", "Ho Chi Minh City"),
  country = c("China", "Vietnam"),
  lon = c(121.47, 106.7),
  lat = c(31.23, 10.77),
  dest_lon = c(39.29, 39.29),  # Dar es Salaam
  dest_lat = c(-6.82, -6.82)
)

# Combine for plotting
ggplot(data = africa) +
  geom_sf(fill = "antiquewhite") +
  
  # African ports
  geom_point(data = ports, aes(x = lon, y = lat), color = "navy", size = 3) +
  geom_text_repel(data = ports, aes(x = lon, y = lat, label = port), size = 3, color = "navy") +
  
  # Inland capitals
  geom_point(data = capitals, aes(x = lon, y = lat), color = "red", size = 3) +
  geom_text_repel(data = capitals, aes(x = lon, y = lat, label = city), size = 3, color = "darkred") +
  
  # Inland routes
  geom_curve(data = routes,
             aes(x = port_lon, y = port_lat, xend = city_lon, yend = city_lat),
             color = "darkgreen", curvature = 0.3, arrow = arrow(length = unit(0.15, "cm"))) +
  
  # International shipping routes
  geom_point(data = international_ports, aes(x = lon, y = lat), color = "purple", size = 3) +
  geom_text_repel(data = international_ports, aes(x = lon, y = lat, label = city), size = 3, color = "purple") +
  geom_curve(data = international_ports,
             aes(x = lon, y = lat, xend = dest_lon, yend = dest_lat),
             color = "purple", curvature = -0.4, arrow = arrow(length = unit(0.2, "cm"))) +
  
  theme_minimal() +
  labs(title = "Shipping Routes from China and Vietnam to Dar es Salaam and Inland Africa") +
  coord_sf(xlim = c(30, 130), ylim = c(-40, 40)) +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

##############################################
## World map for ports to place... 
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(grid)  # for arrow()

# Load ALL countries to include Asia
# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Load data
data_emissionsB <- read_excel("data/data_emissions.xlsx", sheet = "Table 1b")
unique(data_emissionsB$`arrival port`)

# Asian Origin Locations
locations <- data.frame(
  location = c(
    "Qingdao", "Qingdao", 
    "Qinhuangdao", "Qinhuangdao",
    "Nanjing", "Nanjing", "Nanjing", "Nanjing",
    "Xiamen", 
    "Ratchaburi", "Ratchaburi", "Ratchaburi", 
    rep("Ho Chi Minh City",12), 
    "Kochi", "Kochi", "Kochi", "Kochi", 
    "Karachi", "Karachi", "Karachi", 
    rep("Arusha",7), 
    "Kampala", "Kampala"
  ),
  country = c(
    rep("China", 9),
    rep("Thailand", 3),
    rep("Vietnam", 12),
    rep("India", 4),
    rep("Pakistan", 3),
    rep("Tanzania", 7),
    rep("Uganda", 2)
  ),
  lon = c(
    120.3719, 120.3719,
    119.6176, 119.6176,
    118.7674, 118.7674, 118.7674, 118.7674,
    118.0894,
    rep(99.8171, 3),
    rep(106.6602, 12),
    rep(76.2596, 4),
    rep(67.001, 3),
    rep(36.6828, 7),
    rep(32.5825, 2)
  ),
  lat = c(
    36.0986, 36.0986,
    39.9292, 39.9292,
    32.0811, 32.0811, 32.0811, 32.0811,
    24.4798,
    rep(13.5362, 3),
    rep(10.7626, 12),
    rep(9.9664, 4),
    rep(24.8607, 3),
    rep(-3.3869, 7),
    rep(0.3476, 2)
  ),
  type = rep("Asian Origin", 40),
  dest_lon = rep(c(
    34.8500, 39.2083,
    34.8500, 39.2083,
    -4.0031, -4.0031, 34.8500, 39.2083,
    39.2083,
    -4.0031, -4.0031, 39.2083,
    -4.0031, -4.0031, -4.0031, 2.3912, 2.3912, 39.2083, 39.2083, 39.2083, 39.2083, 39.8119, 49.4023, 39.2026,
    -4.0031, 39.2083, -10.7969, -10.7969,
    49.4023, 49.4023, 49.4023,
    36.6828, 36.6828, 36.6828, 36.6828, 2.3912, 2.3912, 39.8119,
    32.5825, 32.5825
  )[1:40]),  # Ensure same length
  dest_lat = rep(c(
    -19.833, -6.7924,
    -19.833, -6.7924,
    5.3480, 5.3480, -19.833, -6.7924,
    -6.7924,
    5.3480, 5.3480, -6.7924,
    5.3480, 5.3480, 5.3480, 6.3703, 6.3703, -6.7924, -6.7924, -6.7924, -6.7924, -12.9787, -18.1492, -6.1659,
    5.3480, -6.7924, 6.3005, 6.3005,
    -18.1492, -18.1492, -18.1492,
    -3.3869, -3.3869, -3.3869, -3.3869, 6.3703, 6.3703, -12.9787,
    0.3476, 0.3476
  )[1:40])
) %>% distinct()

# African inland routes
routes <- data.frame(
  port = c("Dar es Salaam", "Dar es Salaam", "Durban", "Beira", "Abidjan", "Cotonou", "Lagos"),
  city = c("Kigali", "Bujumbura", "Lusaka", "Lilongwe", "Ouagadougou", "Niamey", "Bamako"),
  port_lon = c(39.29, 39.29, 31.02, 34.83, -4.01, 2.42, 3.377),
  port_lat = c(-6.82, -6.82, -29.87, -19.82, 5.33, 6.36, 6.45),
  city_lon = c(30.05, 29.36, 28.32, 33.79, -1.52, 2.11, -7.99),
  city_lat = c(-1.95, -3.37, -15.41, -13.97, 12.37, 13.51, 12.65)
)

# African ports
ports <- data.frame(
  port = c("Lagos", "Mombasa", "Durban", "Dakar", "Abidjan", "Cotonou", "Dar es Salaam"),
  country = c("Nigeria", "Kenya", "South Africa", "Senegal", "Cote d'Ivoire", "Benin", "Tanzania"),
  lon = c(3.377, 39.67, 31.02, -17.44, -4.01, 2.42, 39.29),
  lat = c(6.45, -4.04, -29.87, 14.69, 5.33, 6.36, -6.82)
)

locations_filtered <- locations %>%
  filter(!(lon == dest_lon & lat == dest_lat))

# Build map
ggplot(data = world) +
  geom_sf(fill = "antiquewhite") +
  
  # Ports
  geom_point(data = ports, aes(x = lon, y = lat), color = "navy", size = 3) +
  geom_text_repel(data = ports, aes(x = lon, y = lat, label = port), size = 3, color = "navy") +
  
  # Inland routes
  geom_curve(data = routes,
             aes(x = port_lon, y = port_lat, xend = city_lon, yend = city_lat),
             color = "darkgreen", curvature = 0.3, arrow = arrow(length = unit(0.15, "cm"))) +
  
  # Asian Origins and East African Destinations
  geom_point(data = locations, aes(x = lon, y = lat, color = type), size = 2.5) +
  geom_text_repel(data = locations, aes(x = lon, y = lat, label = location), size = 3) +
  geom_curve(data = locations_filtered,
             aes(x = lon, y = lat, xend = dest_lon, yend = dest_lat, color = type),
             curvature = -0.4, arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.8) +
  
  scale_color_manual(values = c("Asian Origin" = adegenet::transp("purple",0.4))) +
  
  theme_minimal() +
  labs(title = "Routes for ITNs into Africa",
       color = "Location Type") +
  coord_sf(xlim = c(-10, 130), ylim = c(-40, 40)) +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )


###########
##
##
##---Diagram for transport

data_emissionsB <- read_excel("data/data_emissions.xlsx", sheet = "Table 1b")


ggplot(data_emissionsB, aes(x = `Freightos_method mtCO2e`)) +
  geom_histogram(
    bins = 10,
    fill = "#69b3a2",   # soft green color
    color = "white",    # clean white borders
    alpha = 0.8
  ) +
  labs(
    title = "Shipping emissions for each route",
    x = expression("mt CO"[2]*"e"),
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90")
  )


ggplot(data_emissionsB, aes(x = GHGProtocolRoadEstimate_mtCO2e_weight)) +
  geom_histogram(
    bins = 10,
    fill = "#69b3a2",   # soft green color
    color = "white",    # clean white borders
    alpha = 0.8
  ) +
  labs(
    title = "Trucking emissions for each route (weight)",
    x = expression("mt CO"[2]*"e"),
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90")
  )
par(mar = c(3,3,1,1))
par(mfrow = c(1,1))

xx = seq(0,100,1)
yy = seq(0,100,1)

## Create an empty plot
# Install if necessary
library(ggplot2)
library(ggalluvial)
library(dplyr)

# Define the flows and values at each stage
flow_data <- data.frame(
  id     = rep(1:3, each = 4),
  stage  = rep(c("A", "B", "C", "D"), times = 3),
  node   = c("A1", "B1", "C1", "D1",   # flow 1
             "A1", "B2", "C2", "D2",   # flow 2
             "A2", "B3", "C3", "D3"),  # flow 3
  value  = c(100, 80, 60, 50,           # values decreasing per stage
             60, 55, 40, 30,
             90, 90, 85, 70)
)

# Create plot
ggplot(flow_data,
       aes(x = stage, stratum = node, alluvium = id, y = value)) +
  geom_flow(aes(fill = as.factor(id)), stat = "alluvium", lode.guidance = "frontback") +
  geom_stratum(width = 1/10, fill = "grey90", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(expand = c(.05, .05)) +
  theme_minimal() +
  labs(title = "Flow with Different Values at Each Stage",
       x = "Stage", y = "Value", fill = "Flow ID")
