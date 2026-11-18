

library(readxl)
library(ggplot2)
library(patchwork)

### First load in the full data from the 91 countries
## --- Data loading
distance   <- read_excel("data/Supplement 2 Emissions.xlsx", sheet = "A3 Shipping to location")
names(distance)

distance$distance_km = distance$Ship = distance$Road = numeric(nrow(distance))
for(i in 1:nrow(distance)){
  distance$distance_km[i] = sum(distance[i,7:9],na.rm = TRUE)
  distance$Ship[i] = sum(distance[i,8],na.rm = TRUE)
  distance$Road[i] = sum(distance[i,7],distance[i,9],na.rm = TRUE)
}


p_main <- ggplot(distance, aes(distance_km)) +
  geom_histogram(
    bins = 40,
    fill = "grey70",
    colour = "white"
  ) +
  labs(
    x = "Distance travelled (km)",
    y = "Frequency",
    title = "Total", tag = "A"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_ship <- ggplot(distance, aes(Ship)) +
  geom_histogram(
    bins = 20,
    fill = "#2C7218",
    colour = "white"
  ) +
  labs(
    x = "",
    y = "",
    title = "Ship"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_road <- ggplot(distance, aes(Road)) +
  geom_histogram(
    bins = 20,
    fill = "#DEB887",
    colour = "white"
  ) +
  labs(
    x = "",
    y = "",
    title = "Road"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



panel1 = p_main | (p_ship / p_road) +
  plot_layout(widths = c(2, 1))



###########
###
## Panel B is the distribution of kg CO2e emissions estimated per ITN delivered, with insets for ITN class

co2e   <- read_excel("data/Supplement 2 Emissions.xlsx", sheet = "Table 1 update")
names(co2e)

co2e$co2e_estimate = co2e$`Mid point`

x_lim <- range(co2e$co2e_estimate, na.rm = TRUE)

y_lim <- c(-0.45,6.45)
# y_lim <- ggplot_build(
#   ggplot(co2e, aes(co2e_estimate)) +
#     geom_histogram(bins = 10)
# )$layout$panel_params[[1]]$y.range

p_itns <- ggplot(co2e, aes(co2e_estimate)) +
  geom_histogram(
    bins = 40,
    fill = "grey70",
    colour = "white"
  ) +
  labs(
    x = expression("kg CO"[2]*"e per ITN delivered"),
    y = "Frequency",
    title = "Total", tag = "B"
  ) +
  theme_minimal(base_size = 12) +
  coord_cartesian(xlim = x_lim, ylim = y_lim)

p_pyr = ggplot(subset(co2e, class == "Pyrethroid only"),
                 aes(co2e_estimate)) +
  geom_histogram(
    bins = 20,
    fill = "#2C7FB8",
    colour = "white"
  ) +
  labs(x = expression("Estimated kg CO"[2]*"e per ITN delivered"),
       title = "Pyrethroid-only ITN") +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank()
  ) +
  coord_cartesian(xlim = x_lim, ylim = y_lim)

p_pbo <- ggplot(subset(co2e, class == "Pyrethroid-PBO"),
                 aes(co2e_estimate)) +
  geom_histogram(
    bins = 20,
    fill = "#D95F0E",
    colour = "white"
  ) +
  labs(x = expression("Estimated kg CO"[2]*"e per ITN delivered"),
       title = "Pyrethroid PBO") +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank()
  ) +
  coord_cartesian(xlim = x_lim, ylim = y_lim)

p_pyrrole <- ggplot(subset(co2e, class == "Pyrethroid-Pyrrole"),
                 aes(co2e_estimate)) +
  geom_histogram(
    bins = 20,
    fill = "#B23AEE",
    colour = "white"
  ) +
  labs(x = expression("kg CO"[2]*"e per ITN delivered"),
       title = "Pyrethroid-Pyrrole ITN") +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank()
  ) +
  coord_cartesian(xlim = x_lim, ylim = y_lim)


panel2 = p_itns | (p_pyr / p_pbo / p_pyrrole) +
  plot_layout(widths = c(3, 1))






########################################
##
summary_per_ITN_f = function(i){
  aa = unique(co2e$class)[i]
  bb = quantile(co2e$`Mid point`[co2e$class == unique(co2e$class)[i]],
           c(0.05,0.5,0.95),na.rm = TRUE)
  cc = quantile(co2e$`kg CO2e due to transport per ITN delivered max`[co2e$class == unique(co2e$class)[i]],
                c(0.05,0.5,0.95),na.rm = TRUE)
  dd = quantile(co2e$`kg CO2e due to transport per ITN delivered min`[co2e$class == unique(co2e$class)[i]],
                c(0.05,0.5,0.95),na.rm = TRUE)
  return(list(aa,bb,cc,dd))
}
summary_per_ITN_f(1)
summary_per_ITN_f(2)
summary_per_ITN_f(3)

## manufacturer cost plus bale contribution per ITN)
1 - mean(co2e$`Mid point`,na.rm = TRUE)/((2.33 + 0.0153) + mean(co2e$`Mid point`,na.rm = TRUE))
1 - min(co2e$`Mid point`,na.rm = TRUE)/((2.33 + 0.0153) + mean(co2e$`Mid point`,na.rm = TRUE))
1 - max(co2e$`Mid point`,na.rm = TRUE)/((2.33 + 0.0153) + mean(co2e$`Mid point`,na.rm = TRUE))

                                    ###############################################
#----Data for all ITNs delivered 2016 - 2025 of these classes
nets_distrib <- read_excel("data/data_emissions.xlsx", sheet = "nets distributed")


pyr_only = sum(nets_distrib$ITNs)
pbo = sum(nets_distrib$pboITNs,na.rm=TRUE)
pyrrole = sum(nets_distrib$dualITNs,na.rm=TRUE)

mtCO2e10yearsPYR = as.numeric(summary_per_ITN_f(1)[[2]][2])*pyr_only
mtCO2e10yearsPBO = as.numeric(summary_per_ITN_f(2)[[2]][2])*pbo
mtCO2e10yearsDUAL = as.numeric(summary_per_ITN_f(3)[[2]][2])*pyrrole

mtCO2e10yearsPYR+mtCO2e10yearsPBO+mtCO2e10yearsDUAL

pyrethroidITN_yr = as.numeric(summary_per_ITN_f(1)[[2]][2])*nets_distrib$ITNs
pyrPBOITN_yr = as.numeric(summary_per_ITN_f(1)[[2]][2])*nets_distrib$pboITNs
dualITN_yr = as.numeric(summary_per_ITN_f(1)[[2]][2])*nets_distrib$dualITNs
TOTAL = c(pyrethroidITN_yr[1:4],pyrethroidITN_yr[5:10]+pyrPBOITN_yr[5:10]+dualITN_yr[5:10])

item3 <- data.frame(
  Year = 2015:2024,
  "Pyrethroid ITNs" = round(cumsum(pyrethroidITN_yr),2),
  "Pyrethroid-PBO ITNs" = round(cumsum(c(0,0,0,pyrPBOITN_yr[4:10])),2),
  "Pyrethroid-pyrrole ITNs" = round(cumsum(c(0,0,0,0,dualITN_yr[5:10])),2),
  "Total" = round(cumsum(TOTAL),2)
)

pyrethroidITN_yr = as.numeric(summary_per_ITN_f(1)[[4]][1])*nets_distrib$ITNs
pyrPBOITN_yr = as.numeric(summary_per_ITN_f(1)[[4]][1])*nets_distrib$pboITNs
dualITN_yr = as.numeric(summary_per_ITN_f(1)[[4]][1])*nets_distrib$dualITNs
TOTAL = c(pyrethroidITN_yr[1:4],pyrethroidITN_yr[5:10]+pyrPBOITN_yr[5:10]+dualITN_yr[5:10])

mtCO2e10yearsPYR = as.numeric(summary_per_ITN_f(1)[[4]][1])*pyr_only
mtCO2e10yearsPBO = as.numeric(summary_per_ITN_f(2)[[4]][1])*pbo
mtCO2e10yearsDUAL = as.numeric(summary_per_ITN_f(3)[[4]][1])*pyrrole

mtCO2e10yearsPYR+mtCO2e10yearsPBO+mtCO2e10yearsDUAL

item3_5crI <- data.frame(
  Year = 2015:2024,
  "Pyrethroid ITNs" = round(cumsum(pyrethroidITN_yr),2),
  "Pyrethroid-PBO ITNs" = round(cumsum(c(0,0,0,pyrPBOITN_yr[4:10])),2),
  "Pyrethroid-pyrrole ITNs" = round(cumsum(c(0,0,0,0,dualITN_yr[5:10])),2),
  "Total" = round(cumsum(TOTAL),2)
)

pyrethroidITN_yr = as.numeric(summary_per_ITN_f(1)[[3]][3])*nets_distrib$ITNs
pyrPBOITN_yr = as.numeric(summary_per_ITN_f(1)[[3]][3])*nets_distrib$pboITNs
dualITN_yr = as.numeric(summary_per_ITN_f(1)[[3]][3])*nets_distrib$dualITNs
TOTAL = c(pyrethroidITN_yr[1:4],pyrethroidITN_yr[5:10]+pyrPBOITN_yr[5:10]+dualITN_yr[5:10])

mtCO2e10yearsPYR = as.numeric(summary_per_ITN_f(1)[[3]][3])*pyr_only
mtCO2e10yearsPBO = as.numeric(summary_per_ITN_f(2)[[3]][3])*pbo
mtCO2e10yearsDUAL = as.numeric(summary_per_ITN_f(3)[[3]][3])*pyrrole

mtCO2e10yearsPYR+mtCO2e10yearsPBO+mtCO2e10yearsDUAL

item3_95crI <- data.frame(
  Year = 2015:2024,
  "Pyrethroid ITNs" = round(cumsum(pyrethroidITN_yr),2),
  "Pyrethroid-PBO ITNs" = round(cumsum(c(0,0,0,pyrPBOITN_yr[4:10])),2),
  "Pyrethroid-pyrrole ITNs" = round(cumsum(c(0,0,0,0,dualITN_yr[5:10])),2),
  "Total" = round(cumsum(TOTAL),2)
)

# 
# plot(item3$Total ~ item3$Year,lty = 2, col = "black",
#      ylab = expression("Cumulative ktC0"[2]*"e emissions"),
#      xlab = "Year",yaxt="n",
#      ylim = c(0,max(item3$Total)),pch=20)
# axis(2, las = 2, at = seq(0,80000,20000),labels = seq(0,80,20))
# 
# lines(item3$Total ~ item3_95crI$Year,col = "black",lwd=2)
# polygon(c(item3$Year,rev(item3$Year)),
#         c(item3$Pyrethroid.ITNs,
#           rep(0,10)),
#         col = adegenet::transp("blue",0.2),border=NA)
# 
# item3$stand_and_pbo = item3$Pyrethroid.ITNs+item3$Pyrethroid.PBO.ITNs
# polygon(c(item3$Year,rev(item3$Year)),
#         c(item3$stand_and_pbo,
#           rep(0,10)),
#         col = adegenet::transp("blue",0.2),border=NA)
# text(2022, 20000,"Pyrethroid-ITNs")
# text(2022, 70000,"Pyrethroid-PBO ITNs")
# text(2020, 86000,"Pyrethroid-Pyrrole ITNs")
# arrows(x0 = 2022, y0 = 86000, x1 = 2023.8, y1 = 86000,
#        length = 0.1, angle = 30,
#        code = 2, lty = 1,
#        lwd = 2)


library(dplyr)

item3$log_pyr = log(item3$Pyrethroid.ITNs+1)
item3$log_pob = log(item3$Pyrethroid.PBO.ITNs+1)
item3$log_pyrrole = log(item3$Pyrethroid.pyrrole.ITNs+1)
# item3$log_total = log(item3$Total+1)

df_plot <- item3 %>%
  rename(
    pyrA = "Pyrethroid.ITNs",
    pbo_incA = "Pyrethroid.PBO.ITNs",
    pyrrole_incA = "Pyrethroid.pyrrole.ITNs",
    logpyrA = log_pyr,
    logpbo_incA = log_pob,
    logpyrrole_incA = log_pyrrole
  ) %>%
  mutate(
    pbo = pyrA + pbo_incA,
    pyrrole = pbo + pyrrole_incA,
    logpbo = logpyrA + logpbo_incA,
    logpyrrole = logpbo + logpyrrole_incA,
  )

item3_5crI$log_pyr = log(item3_5crI$Pyrethroid.ITNs+1)
item3_5crI$log_pob = log(item3_5crI$Pyrethroid.PBO.ITNs+1)
item3_5crI$log_pyrrole = log(item3_5crI$Pyrethroid.pyrrole.ITNs+1)


df_plot_unc05 <- item3_5crI %>%
  rename(pyrA = "Pyrethroid.ITNs",
         pbo_incA = "Pyrethroid.PBO.ITNs",
         pyrrole_incA = "Pyrethroid.pyrrole.ITNs",
         logpyrA = log_pyr,
         logpbo_incA = log_pob,
         logpyrrole_incA = log_pyrrole
  ) %>%
  mutate(
    pbo = pyrA + pbo_incA,
    pyrrole = pbo + pyrrole_incA,
    logpbo = logpyrA + logpbo_incA,
    logpyrrole = logpbo + logpyrrole_incA,
  )


item3_95crI$log_pyr = log(item3_95crI$Pyrethroid.ITNs+1)
item3_95crI$log_pob = log(item3_95crI$Pyrethroid.PBO.ITNs+1)
item3_95crI$log_pyrrole = log(item3_95crI$Pyrethroid.pyrrole.ITNs+1)
item3_95crI$log_total = log(item3_95crI$Total+1)


df_plot_unc95 <- item3_95crI %>%
  rename(pyrA = "Pyrethroid.ITNs",
         pbo_incA = "Pyrethroid.PBO.ITNs",
         pyrrole_incA = "Pyrethroid.pyrrole.ITNs",
         logpyrA = log_pyr,
         logpbo_incA = log_pob,
         logpyrrole_incA = log_pyrrole
  ) %>%
  mutate(
    pbo = pyrA + pbo_incA,
    pyrrole = pbo + pyrrole_incA,
    logpbo = logpyrA + logpbo_incA,
    logpyrrole = logpbo + logpyrrole_incA,
  )

# cONVERSION 1000000 kg is 1 kt

panel3 = ggplot(df_plot, aes(x = Year)) +
  
  # ## Total line
  # geom_line(
  #   aes(y = log_total),
  #   linetype = "dashed",
  #   colour = "black",
  #   linewidth = 1
  # ) +
  
  ## Pyrethroid only
  geom_ribbon(
    aes(ymin = 0, ymax = pyrA),
    fill = "#2C7FB8",
    alpha = 0.25
  ) +
  
  ## lower uncertainty
  geom_line(
    data = df_plot_unc05,
    aes(x = Year, y = pyrA),
    linetype = "dashed",
    colour = "#2C7FB8",
    linewidth = 0.8
  ) +
  
  ## upper uncertainty
  geom_line(
    data = df_plot_unc95,
    aes(x = Year, y = pyrA),
    linetype = "dashed",
    colour = "#2C7FB8",
    linewidth = 0.8
  )  +
  
  ## Pyrethroid + PBO
  geom_ribbon(
    aes(ymin = pyrA, ymax = pbo),
    fill = "#D95F0E",
    alpha = 0.25
  ) +
  
  ## lower uncertainty
  geom_line(
    data = df_plot_unc05,
    aes(x = Year, y = pbo),
    linetype = "dashed",
    colour = "#D95F0E",
    linewidth = 0.8
  ) +
  
  ## upper uncertainty
  geom_line(
    data = df_plot_unc95,
    aes(x = Year, y = pbo),
    linetype = "dashed",
    colour = "#D95F0E",
    linewidth = 0.8
  )  +
  
  ## Pyrethroid + PBO + Pyrrole
  geom_ribbon(
    aes(ymin = pbo, ymax = pyrrole),
    fill = "#B23AEE",
    alpha = 0.25
  ) +
  
  ## lower uncertainty
  geom_line(
    data = df_plot_unc05,
    aes(x = Year, y = pyrrole),
    linetype = "dashed",
    colour = "#B23AEE",
    linewidth = 0.8
  ) +
  
  ## upper uncertainty
  geom_line(
    data = df_plot_unc95,
    aes(x = Year, y = pyrrole),
    linetype = "dashed",
    colour = "#B23AEE",
    linewidth = 0.8
  )  +
  
  ## Labels
  labs(
    x = "Year",
    y = expression("Cumulative ktCO"[2]*"e emissions"), tag = "C"
  ) +
  
  ## Axis styling
  scale_x_continuous(
    limits = c(2015, 2024),
    breaks = seq(2015, 2024, 2),
    labels = seq(2015, 2024, 2)
  ) +
  # +
  # scale_y_log10(
  #   limits = c(1, 1.7e8),
  #   breaks = scales::log_breaks(n = 6),
  #   labels = scales::label_number(scale = 1e-3, suffix = "k")
  # )
  ## Axis styling
  scale_y_continuous(
    limits = c(0, 680000000),
    breaks = seq(0, 680000000, by=40000000),
    labels = seq(0, 680, 40)
  ) 

library(patchwork)

wrap_plots(panel1, panel2, panel3, widths = c(1, 1, 1))
#1280-480