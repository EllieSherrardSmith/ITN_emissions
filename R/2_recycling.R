### load in data for nets as it stands
###
library(readxl)
library(dplyr)
library(ggplot2)

## plot the association between distance travelled by ship or road
dp <- read_excel("data/Supplement 2 Emissions.xlsx", sheet = "Table 1 update")

dp$Road_fuel = 1000*as.numeric(dp$GHGProtocolRoadEstimate_mtCO2e_fuel)/dp$`ITN number required`
dp$Road_weight = 1000*as.numeric(dp$GHGProtocolRoadEstimate_mtCO2e_weight)/dp$`ITN number required`

dp$ship_freightos = 1000*as.numeric(dp$`Freightos_method mtCO2e`)/dp$`ITN number required`
dp$ship_CCWG = 1000*as.numeric(dp$CCWGmethod)/dp$`ITN number required`



library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

##----------------------------------------------------------------------------
## Calculate emissions (kg CO2e per ITN)
##----------------------------------------------------------------------------

dp <- dp %>%
  mutate(
    Road_fuel   = 1000 * as.numeric(GHGProtocolRoadEstimate_mtCO2e_fuel) /
      `ITN number required`,
    Road_weight = 1000 * as.numeric(GHGProtocolRoadEstimate_mtCO2e_weight) /
      `ITN number required`,
    ship_freightos = 1000 * as.numeric(`Freightos_method mtCO2e`) /
      `ITN number required`,
    ship_CCWG = 1000 * as.numeric(CCWGmethod) /
      `ITN number required`
  )

##----------------------------------------------------------------------------
## Panel 1 data
##----------------------------------------------------------------------------

road <- dp %>%
  transmute(
    distance = `total LAND journeys (n 20TEU) x distance`,
    lower = pmin(Road_fuel, Road_weight),
    upper = pmax(Road_fuel, Road_weight),
    estimate = Road_fuel,
    mode = "Road"
  )

ship <- dp %>%
  transmute(
    distance = `shipping distance`,
    lower = pmin(ship_freightos, ship_CCWG),
    upper = pmax(ship_freightos, ship_CCWG),
    estimate = ship_freightos,
    mode = "Ship"
  )

line_dat <- bind_rows(road, ship)

shipping_limit <- 20360

##----------------------------------------------------------------------------
## Panel 2 data
##----------------------------------------------------------------------------

compare_emissions <- dp %>%
  filter(`shipping distance` < 20360)

box_dat <- bind_rows(
  tibble(
    Method = "Freightos",
    Mode = "Ship",
    Emissions = compare_emissions$ship_freightos
  ),
  tibble(
    Method = "CCWG",
    Mode = "Ship",
    Emissions = compare_emissions$ship_CCWG
  ),
  tibble(
    Method = "Fuel",
    Mode = "Road",
    Emissions = compare_emissions$Road_fuel
  ),
  tibble(
    Method = "Tonne-km",
    Mode = "Road",
    Emissions = compare_emissions$Road_weight
  )
)

##----------------------------------------------------------------------------
## Colours
##----------------------------------------------------------------------------

cols <- c(
  Road = "#0072B2",
  Ship = "#E69F00"
)

fills <- c(
  Freightos = "#E69F00",
  CCWG = "#E69F00",
  Fuel = "#0072B2",
  `Tonne-km` = "#0072B2"
)

##----------------------------------------------------------------------------
## Panel 1
##----------------------------------------------------------------------------

p1 <- ggplot(line_dat,
             aes(distance, estimate, colour = mode)) +
  
  geom_linerange(aes(ymin = lower,
                     ymax = upper),
                 linewidth = 0.6,
                 alpha = 0.9) +
  
  # geom_point(size = 1.5) +
  
  scale_colour_manual(values = cols) +
  
  scale_y_continuous(
    limits = c(0, 0.8),
    breaks = seq(0, 0.8, 0.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    x = "Total Distance covered for delivery (km)",
    y = expression(kg~CO[2]*"e per ITN"),
    colour = NULL
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    legend.position = c(0.88, 0.88),
    legend.background = element_blank(),
    legend.key = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black")
  ) +
  geom_vline(
    xintercept = shipping_limit,
    linetype = "dashed",
    linewidth = 0.6,
    colour = "grey40"
  ) +
  annotate(
    "text",
    x = shipping_limit,
    y = 0.78,
    label = "Maximum shipping distance \n for Panel B comparison",
    hjust = 0,
    vjust = 1,
    size = 3.5,
    colour = "grey30"
  )

##----------------------------------------------------------------------------
## Panel 2
##----------------------------------------------------------------------------

p2 <- ggplot(box_dat,
             aes(Method, Emissions, fill = Method)) +
  
  geom_boxplot(width = 0.65,
               linewidth = 0.6,
               outlier.size = 1) +
  
  scale_fill_manual(values = fills, guide = "none") +
  
  scale_y_continuous(
    limits = c(0, 0.4),
    breaks = seq(0, 0.4, 0.1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    x = "Method of estimation",
    y = expression(kg~CO[2]*"e per ITN")
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(colour = "black")
  )

##----------------------------------------------------------------------------
## Combine (first panel larger)
##----------------------------------------------------------------------------

(p1 | p2) +
  plot_layout(widths = c(2.5, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold", size = 16),
    plot.tag.position = c(0.02, 0.98)
  )

##
##
##
## RECYCLING POSSIBILITIES
##
##
##
df <- read_excel("data/Supplement 2 Emissions.xlsx", sheet = "recycling")
names(df)


df$Combined <- interaction(df$Net, df$Size, sep = " ")

df <- df %>%
  mutate(
    Scope = factor(
      Scope,
      levels = c(1, 2, 3),
      labels = c("Scope 1", "Scope 2", "Scope 3")
    )
  )

df$Product = df$Net

plot_df <- df %>%
  group_by(Combined, Scope, Item,`Activity Type`) %>%
  summarise(
    Kg_CO2e = sum(`kgCO2e per ITN`),
    .groups = "drop"
  )

tapply(plot_df$Kg_CO2e[plot_df$Scope == "Scope 3"],
       plot_df$Combined[plot_df$Scope == "Scope 3"],sum)

totals <- plot_df %>%
  group_by(Combined) %>%
  summarise(
    Total = sum(Kg_CO2e),
    .groups = "drop"
  )

library(ggplot2)

panA = ggplot(plot_df,
       aes(x = Combined,
           y = Kg_CO2e,
           fill = Scope,
           # colour = Country,
           group = interaction(`Activity Type`, 
                               Scope))) +
  
  geom_col(width = 0.8,
           linewidth = 0.6,
           position = "stack") +
  
  geom_text(
    data = totals,
    aes(x = Combined, y = Total, 
        label = round(Total, 1)),
    inherit.aes = FALSE,
    vjust = -0.4,
    fontface = "bold",
    size = 3.5
  ) + scale_y_continuous(
    expand = expansion(mult = c(0, 0.08))
  ) + 
  
  # geom_text(aes(label = Type),
  #           position = position_stack(vjust = 0.5),
  #           size = 3,
  #           colour = "black") +
  
  scale_fill_brewer(palette = "Set2") +
  
  theme_classic() + 
  
  theme(
    axis.title.y = element_text(
      margin = margin(r = 10)
    ),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 30,
      hjust = 0.75,
      vjust = 0.8
    )
  ) +
  labs(
    y = expression("kg "~CO[2]*"e per ITN")
  )
  

plot_df$`Activity Type` <- factor(
  plot_df$`Activity Type`,
  levels = c(
    
    
    "Accessories",
    "Packaging",
    "Process",
    "Raw Material",
    "Freight"
  )
)

tapply(plot_df$Kg_CO2e[plot_df$`Activity Type` == "Raw Material"],
       plot_df$Combined[plot_df$`Activity Type` == "Raw Material"],sum)

panB = ggplot(plot_df,
       aes(x = Combined,
           y = Kg_CO2e,
           fill = `Activity Type`)) +
  geom_col(width = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  
  geom_col(width = 0.8,
           linewidth = 0.6,
           position = "stack") +
  
  # geom_text(aes(label = Type),
  #           position = position_stack(vjust = 0.5),
  #           size = 3,
  #           colour = "black") +
  
  scale_fill_brewer(palette = "Set2") +
  
  theme_classic() + 
  
  theme(
    axis.title.y = element_text(
      margin = margin(r = 10)
    ),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 30,
      hjust = 0.75,
      vjust = 0.8
    )
  ) +
  labs(
    y = expression("kg "~CO[2]*"e per ITN")
  )


plot2_df <- df %>%
  group_by(Combined, Country, `Activity Type`, Item) %>%
  summarise(
    Kg_CO2e = sum(`kgCO2e per ITN`),
    .groups = "drop"
  )

plot2_df = subset(plot2_df,
                  plot2_df$`Activity Type` == "Raw Material")

totals2 <- plot2_df %>%
  group_by(Combined) %>%
  summarise(
    Total = sum(Kg_CO2e),
    .groups = "drop"
  )

panC = ggplot(plot2_df,
              aes(x = Combined,
                  y = Kg_CO2e,
                  fill = Item)) +
  geom_col(width = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  
  geom_col(width = 0.8,
           linewidth = 0.6,
           position = "stack") +
  
  geom_text(
    data = totals2,
    aes(x = Combined, y = Total, label = round(Total, 1)),
    inherit.aes = FALSE,
    vjust = -0.4,
    fontface = "bold",
    size = 3.5
  ) + scale_y_continuous(
    expand = expansion(mult = c(0, 0.08))
  ) + 
  # geom_text(aes(label = Type),
  #           position = position_stack(vjust = 0.5),
  #           size = 3,
  #           colour = "black") +
  
  theme_classic() + 
  
  theme(
    axis.title.y = element_text(
      margin = margin(r = 10)
    ),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 30,
      hjust = 0.75,
      vjust = 0.8
    )
  ) +
  labs(
    y = expression("kg "~CO[2]*"e per ITN")
  )


############################
##
## And let us look at bottle-to-net recycling
##
############################

bot = read_excel("data/Supplement 2 Emissions.xlsx", sheet = "bottle to net")

panD = ggplot(
  bot,
  aes(
    x = percentage_recycled_content,
    y = kgCO2e_per_net,
    colour = factor(Emissions_factor_conversion),
    linetype = `ITN size`,
    group = interaction(
      Emissions_factor_conversion,
      `ITN size`
    )
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = "Percentage recycled content",
    y = expression("kg "~CO[2]*"e per net"),
    colour = "Emissions factor conversion",
    linetype = "ITN size"
  ) +
  scale_linetype_manual(
    values = c(
      "largest" = "solid",
      "smaller" = "dashed"
    )
  ) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() + 
  labs(
    y = expression("kg "~CO[2]*"e per ITN")
  )



(panA | panB) /
  (panC | panD) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold", size = 16),
    plot.tag.position = c(0.02, 0.98)
  )

(panA | panB) /
  (panC | panD) +
  plot_layout(widths = c(1.6, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold", size = 16),
    plot.tag.position = c(0.02, 0.98)
  )
