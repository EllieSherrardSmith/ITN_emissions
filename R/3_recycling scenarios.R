# recycling part 2
# theoretical costs explorations

# Install if needed:
# install.packages(c("plotly", "dplyr", "tidyr"))

library(plotly)
library(dplyr)

# # 1. Define the three theoretical PET systems
# 
scenarios <- data.frame(
  scenario = c(
    "Virgin PET\nno waste collection",
    "Open-loop rPET\nno waste collection",
    "Closed-loop rPET\nwaste fully managed"
  ),

  # Illustrative values -- replace with your own data
  emissions = c(3.713, 2.528, 3.00),       # kg CO2e / functional unit
  cost = c(1.00, 1.15, 1.5),    # $ / functional unit
  waste = c(1.00, 1.00, 0.10),        # kg waste / functional unit

  colour = c("#D55E00", "#0072B2", "#009E73")
)
# 
# # ---------------------------------------------------------
# # 2. Define oil-price scenarios
# # ---------------------------------------------------------
# 
# oil_multipliers <- c(
#   "Low oil price"      = 0.70,
#   "Reference oil price" = 1.00,
#   "High oil price"     = 1.30,
#   "Very high oil price" = 1.60
# )
# 
# # Assume a proportion of virgin PET's cost is oil-linked.
# oil_cost_share <- 0.50
# 
# # Calculate cost under each oil-price scenario
# cost_sensitivity <- expand.grid(
#   scenario = scenarios$scenario,
#   oil_scenario = names(oil_multipliers)
# ) %>%
#   left_join(scenarios, by = "scenario") %>%
#   mutate(
#     oil_multiplier = oil_multipliers[oil_scenario],
#     
#     cost = case_when(
#       grepl("Virgin", scenario) ~
#         cost_base * (
#           (1 - oil_cost_share) +
#             oil_cost_share * oil_multiplier
#         ),
#       
#       TRUE ~ cost_base
#     )
#   )
# 
# # ---------------------------------------------------------
# # 3. Plot the reference scenario
# # ---------------------------------------------------------
# 
# reference <- cost_sensitivity %>%
#   filter(oil_scenario == "Reference oil price")
# 
# plot_ly(
#   reference,
#   x = ~emissions,
#   y = ~cost,
#   z = ~waste,
#   type = "scatter3d",
#   mode = "markers+text",
#   text = ~scenario,
#   textposition = "top center",
#   marker = list(
#     size = 14,
#     color = ~colour,
#     opacity = 0.8
#   )
# ) %>%
#   layout(
#     title = "PET systems: emissions, cost and waste",
#     scene = list(
#       xaxis = list(
#         title = "Emissions produced\n(kg CO2e / functional unit)"
#       ),
#       yaxis = list(
#         title = "Cost\n($ / functional unit)"
#       ),
#       zaxis = list(
#         title = "Waste volume generated\n(kg / functional unit)"
#       )
#     )
#   )
# 
# 
# # ---------------------------------------------------------
# # 4. Find the oil price at which cost rankings change
# # ---------------------------------------------------------
# 
# oil_grid <- seq(0.4, 2.5, by = 0.01)
# 
# sensitivity <- data.frame(
#   oil_multiplier = oil_grid
# ) %>%
#   mutate(
#     virgin_cost =
#       scenarios$cost_base[1] *
#       ((1 - oil_cost_share) +
#          oil_cost_share * oil_multiplier),
#     
#     open_loop_cost = scenarios$cost_base[2],
#     
#     closed_loop_cost = scenarios$cost_base[3]
#   )
# 
# # Plot cost as a function of oil price
# plot(
#   sensitivity$oil_multiplier,
#   sensitivity$virgin_cost,
#   type = "l",
#   lwd = 3,
#   col = "#D55E00",
#   xlab = "Oil price relative to reference",
#   ylab = "PET cost ($ / functional unit)",
#   ylim = range(
#     sensitivity[, c(
#       "virgin_cost",
#       "open_loop_cost",
#       "closed_loop_cost"
#     )]
#   )
# )
# 
# lines(
#   sensitivity$oil_multiplier,
#   sensitivity$open_loop_cost,
#   lwd = 3,
#   col = "#0072B2"
# )
# 
# lines(
#   sensitivity$oil_multiplier,
#   sensitivity$closed_loop_cost,
#   lwd = 3,
#   col = "#009E73"
# )
# 
# legend(
#   "topleft",
#   legend = c(
#     "Virgin PET",
#     "Open-loop rPET",
#     "Closed-loop rPET"
#   ),
#   col = c("#D55E00", "#0072B2", "#009E73"),
#   lwd = 3,
#   bty = "n"
# )


# ---------------------------------------------------------
# 5. Explore different rPET / virgin PET cost ratios
# ---------------------------------------------------------

# cost_ratios <- c(
#   0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.5
# )
# 
# ratio_data <- expand.grid(
#   ratio = cost_ratios,
#   scenario = scenarios$scenario
# ) %>%
#   left_join(scenarios, by = "scenario") %>%
#   mutate(
#     cost = case_when(
#       grepl("Virgin", scenario) ~ 1,
#       grepl("Open-loop", scenario) ~ ratio,
#       grepl("Closed-loop", scenario) ~ ratio * 1.1
#     )
#   )
# 
# # Plot one selected ratio
# selected_ratio <- ratio_data %>%
#   filter(ratio == 1.0)
# 
# plot_ly(
#   selected_ratio,
#   x = ~emissions,
#   y = ~cost,
#   z = ~waste,
#   type = "scatter3d",
#   mode = "markers+text",
#   text = ~scenario,
#   textposition = "top center",
#   marker = list(
#     size = 15,
#     color = ~colour,
#     opacity = 0.8
#   )
# ) %>%
#   layout(
#     title = "PET system comparison at rPET/virgin cost ratio = 1.0",
#     scene = list(
#       xaxis = list(title = "Emissions"),
#       yaxis = list(title = "Relative cost"),
#       zaxis = list(title = "Waste generated")
#     )
#   )

#####################################
##
## Theoretical demo of oil volatility in prices
##                     waste production
##                     costs ratios

# Install packages if required
# install.packages(c(
#   "tidyverse",
#   "plotly",
#   "scales",
#   "patchwork"
# ))

library(tidyverse)
library(plotly)
library(scales)
library(patchwork)

## THEORETICAL PET SYSTEMS

pet <- tibble(
  system = c(
    "Virgin PET",
    "Open-loop rPET",
    "Closed-loop rPET"
  ),
  
  collection = c(
    "No waste collection",
    "No waste collection",
    "Waste fully managed"
  ),
  
  emissions = c(
    3.713,  ## highest emissions (Fig 3D)
    2.528,  ## lowest emissions (Fig 3D)
    3.000   ## lower emissions (anticipate higher)
  ),
  
  waste = c(
    1.00,
    1.00,
    0.10   ## vastly reduced with closed loop
  ),
  
  cost = c(
    1.00,  ## slightly cheaper?
    1.15,
    1.5
  )
)

pet

pet_cols <- c(
  "Virgin PET" = "#D55E00",
  "Open-loop rPET" = "#0072B2",
  "Closed-loop rPET" = "#009E73"
)

####
##
## Figure 1. Conceptual positioning of PET management pathways
## across emissions, economic cost and waste generation.
fig_3d <- plot_ly(
  data = pet,
  x = ~emissions,
  y = ~cost,
  z = ~waste,
  type = "scatter3d",
  mode = "markers+text",
  
  text = ~system,
  textposition = "top center",
  
  marker = list(
    size = 16,
    color = unname(pet_cols[pet$system]),
    opacity = 0.85,
    line = list(
      color = "white",
      width = 2
    )
  ),
  
  hovertemplate = paste(
    "<b>%{text}</b><br>",
    "Emissions: %{x:.2f}<br>",
    "Cost: %{y:.2f}<br>",
    "Waste: %{z:.2f}",
    "<extra></extra>"
  )
) %>%
  
  layout(
    scene = list(
      
      xaxis = list(
        title = "Emissions produced<br>(kg CO2-eq / functional unit)",
        titlefont = list(size = 14),
        tickfont = list(size = 11),
        gridcolor = "grey85",
        zerolinecolor = "grey70"
      ),
      
      yaxis = list(
        title = "Cost<br>(relative cost)",
        titlefont = list(size = 14),
        tickfont = list(size = 11),
        gridcolor = "grey85",
        zerolinecolor = "grey70"
      ),
      
      zaxis = list(
        title = "Waste generated<br>(kg / functional unit)",
        titlefont = list(size = 14),
        tickfont = list(size = 11),
        gridcolor = "grey85",
        zerolinecolor = "grey70"
      ),
      
      camera = list(
        eye = list(
          x = 1.6,
          y = 1.6,
          z = 1.3
        )
      ),
      
      bgcolor = "white"
    ),
    
    paper_bgcolor = "white",
    plot_bgcolor = "white",
    
    margin = list(
      l = 0,
      r = 0,
      b = 0,
      t = 40
    )
  )

# fig_3d

oil_share <- 0.70

oil_price <- seq(
  0.5,
  2.0,
  by = 0.01
)

cost_sensitivity <- tibble(
  oil_multiplier = oil_price
) %>%
  mutate(
    
    virgin_cost =
      (1 - oil_share) +
      oil_share * oil_multiplier,
    
    open_loop_cost = as.numeric(pet[2,5]),
    
    closed_loop_cost = as.numeric(pet[3,5])
    
  )

cost_long <- cost_sensitivity %>%
  
  pivot_longer(
    cols = c(
      virgin_cost,
      open_loop_cost,
      closed_loop_cost
    ),
    names_to = "system",
    values_to = "cost"
  ) %>%
  
  mutate(
    system = recode(
      system,
      virgin_cost = "Virgin PET",
      open_loop_cost = "Open-loop rPET",
      closed_loop_cost = "Closed-loop rPET"
    )
  )

p_cost <- ggplot(
  cost_long,
  aes(
    x = oil_multiplier,
    y = cost,
    colour = system
  )
) +
  
  geom_line(
    linewidth = 1.2
  ) +
  
  scale_colour_manual(
    values = pet_cols
  ) +
  
  scale_x_continuous(
    name = "Oil price relative to reference",
    breaks = c(0.5, 1.0, 1.5, 2.0),
    labels = c(
      "50%",
      "100%",
      "150%",
      "200%"
    )
  ) +
  
  scale_y_continuous(
    name = "Relative PET cost",
    labels = number_format(
      accuracy = 0.1
    )
  ) +
  
  theme_classic(
    base_size = 12
  ) +
  
  theme(
    legend.position = "bottom",
    
    legend.title = element_blank(),
    
    axis.title = element_text(
      size = 12
    ),
    
    axis.text = element_text(
      size = 10,
      colour = "black"
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 13
    ),
    
    panel.grid = element_blank()
  ) +
  
  labs(
    title = "Sensitivity of PET cost to oil-price volatility"
  )

p_cost


oil_grid <- seq(
  0.5,
  2.0,
  by = 0.01
)

ratio_grid <- seq(
  0.5,
  1.8,
  by = 0.01
)

sensitivity <- expand_grid(
  oil_multiplier = oil_grid,
  rPET_ratio = ratio_grid
) %>%
  
  mutate(
    
    # Virgin PET cost
    virgin_cost =
      (1 - oil_share) +
      oil_share * oil_multiplier,
    
    # rPET cost
    rPET_cost =
      rPET_ratio * virgin_cost,
    
    # Determine cheapest option
    preferred =
      case_when(
        
        rPET_cost < virgin_cost ~
          "rPET",
        
        rPET_cost > virgin_cost ~
          "Virgin PET",
        
        TRUE ~
          "Equal cost"
      )
  )


p_heatmap <- ggplot(
  sensitivity,
  aes(
    x = oil_multiplier,
    y = rPET_ratio,
    fill = preferred
  )
) +
  
  geom_raster() +
  
  scale_fill_manual(
    values = c(
      "Virgin PET" = "#D55E00",
      "rPET" = "#009E73",
      "Equal cost" = "white"
    )
  ) +
  
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    colour = "grey20",
    linewidth = 0.6
  ) +
  
  scale_x_continuous(
    name = "Oil price relative to reference",
    breaks = c(
      0.5,
      1.0,
      1.5,
      2.0
    ),
    labels = c(
      "50%",
      "100%",
      "150%",
      "200%"
    )
  ) +
  
  scale_y_continuous(
    name = "rPET / virgin PET cost ratio",
    breaks = seq(
      0.5,
      1.8,
      0.2
    )
  ) +
  
  theme_classic(
    base_size = 12
  ) +
  
  theme(
    legend.position = "bottom",
    
    legend.title = element_blank(),
    
    axis.text = element_text(
      colour = "black"
    ),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.5
    )
  ) +
  
  labs(
    title = "Economic decision space under oil-price volatility"
  )

p_heatmap


sensitivity_2 <- expand_grid(
  oil_multiplier = seq(0.5, 2.0, by = 0.01),
  rPET_ratio = seq(0.5, 1.8, by = 0.01)
) %>%
  
  mutate(
    
    virgin_cost =
      (1 - oil_share) +
      oil_share * oil_multiplier,
    
    rPET_cost =
      rPET_ratio * virgin_cost,
    
    cost_difference =
      rPET_cost - virgin_cost
  )


p_heatmap = ggplot(
  sensitivity_2,
  aes(
    x = oil_multiplier,
    y = rPET_ratio,
    z = cost_difference
  )
) +
  
  geom_raster(
    aes(
      fill = cost_difference
    )
  ) +
  
  geom_contour(
    colour = "black",
    linewidth = 0.8,
    breaks = 0
  ) +
  
  scale_fill_gradient2(
    low = "#009E73",
    mid = "white",
    high = "#D55E00",
    midpoint = 0,
    name = "rPET cost\nminus virgin cost"
  ) +
  
  labs(
    x = "Oil price relative to reference",
    y = "rPET / virgin PET cost ratio"
  ) +
  
  theme_classic(
    base_size = 12
  )



# ---------------------------------------------------------
# MULTI-CRITERIA DECISION SCORE
# ---------------------------------------------------------

pet_mca <- pet %>%
  
  mutate(
    
    emissions_norm =
      emissions / max(emissions),
    
    cost_norm =
      cost / max(cost),
    
    waste_norm =
      waste / max(waste)
    
  )

# Example stakeholder weighting
w_emissions <- 0.20
w_cost      <- 0.60 ## 
w_waste     <- 0.20

pet_mca <- pet_mca %>%
  
  mutate(
    
    decision_score =
      w_emissions * emissions_norm +
      w_cost * cost_norm +
      w_waste * waste_norm
    
  )

pet_mca %>%
  select(
    system,
    emissions_norm,
    cost_norm,
    waste_norm,
    decision_score
  )

p_mca <- ggplot(
  pet_mca,
  aes(
    x = reorder(system, decision_score),
    y = decision_score,
    fill = system
  )
) +
  
  geom_col(
    width = 0.65,
    colour = "white",
    linewidth = 0.3
  ) +
  
  geom_text(
    aes(
      label = round(decision_score, 2)
    ),
    hjust = -0.15,
    size = 3.5
  ) +
  
  scale_fill_manual(
    values = pet_cols
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.12)
    )
  ) +
  
  labs(
    x = NULL,
    y = "Multi-criteria decision score",
    title = "Across emissions, cost and waste"
  ) +
  
  theme_classic(
    base_size = 12
  ) +
  
  theme(
    legend.position = "none",
    
    axis.text = element_text(
      colour = "black"
    ),
    
    axis.title = element_text(
      size = 12
    ),
    
    plot.title = element_text(
      size = 13,
      face = "bold"
    )
  )

combined_figure <- (p_cost | p_heatmap) / p_mca  +
  
  plot_annotation(
    tag_levels = "A"
  )

combined_figure


library(tidyverse)

# ---------------------------------------------------------
# 1. Normalise the three criteria
# ---------------------------------------------------------

pet_mca <- pet %>%
  mutate(
    emissions_norm = emissions / max(emissions),
    cost_norm      = cost / max(cost),
    waste_norm     = waste / max(waste)
  )


# ---------------------------------------------------------
# 2. Create a grid of emissions and waste weights
#
# Cost receives whatever weight remains:
#
# cost weight = 1 - emissions weight - waste weight
# ---------------------------------------------------------

weight_grid <- expand_grid(
  w_emissions = seq(0, 1, by = 0.02),
  w_waste     = seq(0, 1, by = 0.02)
) %>%
  
  mutate(
    w_cost = 1 - w_emissions - w_waste
  ) %>%
  
  # Remove combinations where the cost weight is negative
  filter(w_cost >= 0)


# ---------------------------------------------------------
# 3. Calculate the multi-criteria score
# ---------------------------------------------------------

mca_heatmap <- weight_grid %>%
  
  crossing(
    pet_mca %>%
      select(
        system,
        emissions_norm,
        cost_norm,
        waste_norm
      )
  ) %>%
  
  mutate(
    
    score =
      w_emissions * emissions_norm +
      w_cost      * cost_norm +
      w_waste     * waste_norm
    
  ) %>%
  
  group_by(
    w_emissions,
    w_waste,
    w_cost
  ) %>%
  
  slice_min(
    score,
    n = 1,
    with_ties = FALSE
  ) %>%
  
  ungroup()


mca_heatmap_plot <- ggplot(
  mca_heatmap,
  aes(
    x = w_emissions,
    y = w_waste,
    fill = system
  )
) +
  
  geom_tile(
    width = 0.021,
    height = 0.021
  ) +
  
  scale_fill_manual(
    values = pet_cols,
    name = "Preferred pathway"
  ) +
  
  scale_x_continuous(
    name = "Weight assigned to emissions",
    labels = scales::percent,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    name = "Weight assigned to waste",
    labels = scales::percent,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Multi-criteria decision space",
    subtitle = paste0(
      "Cost weight = 100% − emissions weight − waste weight"
    )
  ) +
  
  theme_classic(
    base_size = 12
  ) +
  
  theme(
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    axis.title = element_text(
      size = 12
    ),
    
    axis.text = element_text(
      colour = "black"
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    
    plot.subtitle = element_text(
      size = 10,
      colour = "grey30"
    )
  )

mca_heatmap_plot


# ---------------------------------------------------------
# 5. Calculate the MCA score across the entire weight space
# ---------------------------------------------------------

mca_scores <- weight_grid %>%
  
  crossing(
    pet_mca %>%
      select(
        system,
        emissions_norm,
        cost_norm,
        waste_norm
      )
  ) %>%
  
  mutate(
    
    score =
      w_emissions * emissions_norm +
      w_cost      * cost_norm +
      w_waste     * waste_norm
    
  )


closed_loop_heatmap <- mca_scores %>%
  
  filter(
    system == "Closed-loop rPET"
  ) %>%
  
  ggplot(
    aes(
      x = w_emissions,
      y = w_waste,
      fill = score
    )
  ) +
  
  geom_tile(
    width = 0.021,
    height = 0.021
  ) +
  
  scale_fill_viridis_c(
    option = "C",
    name = "MCA score"
  ) +
  
  scale_x_continuous(
    name = "Emissions weight",
    labels = scales::percent,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    name = "Waste weight",
    labels = scales::percent,
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Closed-loop rPET: multi-criteria score",
    subtitle = "Cost receives the remaining decision weight"
  ) +
  
  theme_classic(
    base_size = 12
  )

###########################
##
##
##
#############################

library(tidyverse)
library(scales)

pet <- tibble(
  system = c(
    "Virgin PET",
    "Open-loop rPET",
    "Closed-loop rPET"
  ),

  emissions = c(
    3.713,
    2.528,
    3.00
  ),

  waste = c(
    1.00,
    1.00,
    0.10
  ),

  cost_base = c(
    1.00,
    1.15,
    1.5
  )
)

pet_cols <- c(
  "Virgin PET" = "#D55E00",
  "Open-loop rPET" = "#0072B2",
  "Closed-loop rPET" = "#009E73"
)

# ---------------------------------------------------------
# OIL PRICE ASSUMPTION
# ---------------------------------------------------------

oil_share <- 0.70

oil_grid <- seq(
  0.50,
  2.00,
  by = 0.02
)

# ---------------------------------------------------------
# MCA WEIGHT SPACE
# ---------------------------------------------------------

weight_grid <- expand_grid(
  
  w_emissions = seq(
    0,
    1,
    by = 0.02
  ),
  
  w_waste = seq(
    0,
    1,
    by = 0.02
  )
  
) %>%
  
  mutate(
    
    w_cost =
      1 -
      w_emissions -
      w_waste
    
  ) %>%
  
  filter(
    w_cost >= 0
  )

# ---------------------------------------------------------
# COMBINE OIL PRICE AND MCA WEIGHTS
# ---------------------------------------------------------

scenario_grid <- expand_grid(
  
  oil_multiplier = oil_grid,
  
  weight_grid
  
)

# ---------------------------------------------------------
# OIL-ADJUSTED COSTS
# ---------------------------------------------------------

mca_data <- scenario_grid %>%
  
  crossing(
    pet
  ) %>%
  
  mutate(
    
    cost = case_when(
      
      system == "Virgin PET" ~
        
        cost_base *
        (
          (1 - oil_share) +
            oil_share * oil_multiplier
        ),
      
      TRUE ~
        
        cost_base
      
    )
    
  )

# ---------------------------------------------------------
# NORMALISE EACH CRITERION
# ---------------------------------------------------------

mca_data <- mca_data %>%
  
  group_by(oil_multiplier) %>%
  
  mutate(
    
    emissions_norm =
      (emissions - min(emissions)) /
      (max(emissions) - min(emissions)),
    
    cost_norm =
      (cost - min(cost)) /
      (max(cost) - min(cost)),
    
    waste_norm =
      (waste - min(waste)) /
      (max(waste) - min(waste))
    
  ) %>%
  
  ungroup()

# ---------------------------------------------------------
# MULTI-CRITERIA DECISION SCORE
# ---------------------------------------------------------

mca_data <- mca_data %>%
  
  mutate(
    
    decision_score =
      
      w_emissions * emissions_norm +
      
      w_cost * cost_norm +
      
      w_waste * waste_norm
    
  )

# ---------------------------------------------------------
# FIND THE PREFERRED PATHWAY
# ---------------------------------------------------------

mca_preference <- mca_data %>%
  
  group_by(
    oil_multiplier,
    w_emissions,
    w_waste,
    w_cost
  ) %>%
  
  slice_min(
    decision_score,
    n = 1,
    with_ties = FALSE
  ) %>%
  
  ungroup()

# ---------------------------------------------------------
# SELECT OIL PRICE SCENARIOS
# ---------------------------------------------------------

selected_oil <- c(
  0.50,
  1.00,
  1.10,
  1.30
)

mca_selected <- mca_preference %>%
  
  filter(
    oil_multiplier %in% selected_oil
  ) %>%
  
  mutate(
    
    oil_label = factor(
      oil_multiplier,
      levels = selected_oil,
      labels = c(
        "Low oil price\n(50%)",
        "Reference\n(100%)",
        "Higher oil price\n(110%)",
        # "Higher oil price\n(120%)",
        "Higher oil price\n(130%)"
        # "High oil price\n(160%)"
        # "Very high\n(200%)"
      )
    )
    
  )

# ---------------------------------------------------------
# MCA DECISION HEATMAP
# ---------------------------------------------------------

p_mca_oil <- ggplot(
  mca_selected,
  aes(
    x = w_emissions,
    y = w_waste,
    fill = system
  )
) +
  
  geom_tile(
    width = 0.021,
    height = 0.021
  ) +
  
  facet_wrap(
    ~ oil_label,
    nrow = 1
  ) +
  
  scale_fill_manual(
    values = pet_cols,
    name = "Preferred pathway"
  ) +
  
  scale_x_continuous(
    name = "Emissions weight",
    labels = percent,
    breaks = seq(
      0,
      1,
      by = 0.2
    ),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    name = "Waste weight",
    labels = percent,
    breaks = seq(
      0,
      1,
      by = 0.2
    ),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  
  labs(
    title =
      "Multi-criteria PET preference under oil-price scenarios",
    
    subtitle =
      "Cost weight = 100% − emissions weight − waste weight"
  ) +
  
  theme_classic(
    base_size = 11
  ) +
  
  theme(
    
    strip.background =
      element_rect(
        fill = "grey95",
        colour = "black"
      ),
    
    strip.text =
      element_text(
        face = "bold"
      ),
    
    legend.position =
      "bottom",
    
    legend.title =
      element_text(
        face = "bold"
      ),
    
    axis.text =
      element_text(
        colour = "black"
      ),
    
    plot.title =
      element_text(
        face = "bold",
        size = 14
      ),
    
    plot.subtitle =
      element_text(
        colour = "grey30"
      )
  )

p_mca_oil
# Each panel represents a different oil-price environment.

# Within each panel:

# left/right = importance of emissions
# bottom/top = importance of waste
# the remaining weight goes to cost
# colour = pathway with the lowest MCA score

# So you can see whether increasing oil prices causes 
# the Virgin PET region to shrink, because virgin PET 
# becomes economically less attractive.

combo_figure = (p_cost | p_heatmap) /
  (p_mca | closed_loop_heatmap) /
  p_mca_oil +
  
  plot_annotation(
    tag_levels = "A"
  )

combo_figure
