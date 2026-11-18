########################################
##
## Data shipping analysis
## from https://mrv.emsa.europa.eu/#public/emission-report

## read 2024 data
## --- Data loading
data_ships   <- read_excel("data/data_shipping.xlsx", sheet = "ships2024")

summary(data_ships$`CO₂eq emissions per distance_kg CO₂eq per n mile`)

plot(sort(data_ships$`CO₂eq emissions per distance_kg CO₂eq per n mile`) ~
       c(1:nrow(data_ships)),
     ylab = expression("CO"[2]*"eq emissions / nautical mile"),
     yaxt = "n",xaxt = "n",xlab = "Ordered EU shipping data")
axis(2, las = 2, at = seq(0,1600,200))
quantile(data_ships$`CO₂eq emissions per distance_kg CO₂eq per n mile`,c(0.05,0.25,0.5,0.75,0.95))


########################################
##
## --- Data loading
data_emissionsB   <- read_excel("data/data_emissions.xlsx", sheet = "Table 1 manuscript")
data_emissionsB   <- read_excel("data/data_emissions.xlsx", sheet = "Table 1b")

plot(sort(data_emissionsB$TOTALmtCO2e_max) ~
       c(1:nrow(data_emissionsB)),
     ylab = expression("mt CO"[2]*"eq emissions"),
     yaxt = "n",xaxt = "n",xlab = "Ordered trips")
axis(2, las = 2, at = seq(0,50,10))
points(sort(data_emissionsB$TOTALmtCO2e_min) ~
         c(1:nrow(data_emissionsB)))

boxplot(data_emissionsB$TOTALmtCO2e_min ~ 
          data_emissionsB$Key_for_subclass)

# install.packages("ggplot2")
library(ggplot2)

ggplot(data_emissionsB, aes(x = Key_for_subclass, y = TOTALmtCO2e_max)) +
  geom_boxplot(outlier.shape = NA) +  # Don't show outliers (they'll be in jitter)
  geom_jitter(aes(shape = Route, color = place),
              width = 0.2, size = 2, alpha = 0.7) + # Adjust size and transparency
  scale_color_brewer(palette = "Dark2") +  # Optional: Nice color palette
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
  , 
  legend.position = "right",                      # Move legend down
  legend.box = "horizontal",                       # Arrange items in a row
  legend.box.margin = margin(t = 10),              # Space between plot and legend
  legend.spacing.x = unit(0.5, "cm")               # Space between legend items
  ) +
  labs(title = "ITN product transportation emissions",
       x = "", y = expression("mt CO"[2]*"eq emissions"), color = "Country", shape = "Route option")

## now estimate the max and min
data_emissionsB$mid <- rowMeans(data_emissionsB[,c('TOTALmtCO2e_min', 'TOTALmtCO2e_max')], na.rm=TRUE)

ggplot(data_emissionsB, aes(x = Key_for_subclass)) +
  
  # Draw min–max segment per row
  geom_segment(aes(
    x = Key_for_subclass, xend = Key_for_subclass,
    y = TOTALmtCO2e_min, yend = TOTALmtCO2e_max,
    color = place
  ),
  alpha = 0.5, linewidth = 0.8) +
  
  # Draw boxplot for context (optional)
  geom_boxplot(aes(y = mid), outlier.shape = NA, alpha = 0.2) +
  
  # Plot aligned points at TOTALmtCO2e_max (no horizontal jitter)
  geom_point(aes(
    y = mid,
    shape = Route,
    color = place
  ),
  size = 2.5, alpha = 0.8, position = position_nudge(x = 0)) +
  
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10),
    legend.position = "right",
    legend.box = "horizontal",
    legend.box.margin = margin(t = 10),
    legend.spacing.x = unit(0.5, "cm")
  ) +
  labs(
    title = "ITN product transportation emissions (min–max with aligned points)",
    x = "", y = expression("mt CO"[2]*"eq emissions"),
    color = "Country", shape = "Route option"
  )

###############################################
## for each ITN class - we can estimate the "mean" and range
## per ITN delivered
summary_per_ITN_f = function(i){
  aa = unique(data_emissionsB$class)[i]
  bb = quantile(data_emissionsB$mid[data_emissionsB$class == unique(data_emissionsB$class)[i]]/
             data_emissionsB$`ITN number required`[data_emissionsB$class == unique(data_emissionsB$class)[i]],
           c(0.05,0.5,0.95))
  return(list(aa,bb))
}
summary_per_ITN_f(1)
summary_per_ITN_f(2)
summary_per_ITN_f(3)

###############################################
#----Data for all ITNs delivered 2016 - 2025 of these classes
nets_distrib <- read_excel("data/data_emissions.xlsx", sheet = "nets distributed")


pyr_only = sum(nets_distrib$ITNs)
pbo = sum(nets_distrib$pboITNs,na.rm=TRUE)
pyrrole = sum(nets_distrib$dualITNs,na.rm=TRUE)

mtCO2e10yearsPYR = as.numeric(summary_per_ITN_f(1)[[2]])*pyr_only
mtCO2e10yearsPBO = as.numeric(summary_per_ITN_f(2)[[2]])*pbo
mtCO2e10yearsDUAL = as.numeric(summary_per_ITN_f(3)[[2]])*pyrrole

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

pyrethroidITN_yr = as.numeric(summary_per_ITN_f(1)[[2]][1])*nets_distrib$ITNs
pyrPBOITN_yr = as.numeric(summary_per_ITN_f(1)[[2]][1])*nets_distrib$pboITNs
dualITN_yr = as.numeric(summary_per_ITN_f(1)[[2]][1])*nets_distrib$dualITNs
TOTAL = c(pyrethroidITN_yr[1:4],pyrethroidITN_yr[5:10]+pyrPBOITN_yr[5:10]+dualITN_yr[5:10])

item3_5crI <- data.frame(
  Year = 2015:2024,
  "Pyrethroid ITNs" = round(cumsum(pyrethroidITN_yr),2),
  "Pyrethroid-PBO ITNs" = round(cumsum(c(0,0,0,pyrPBOITN_yr[4:10])),2),
  "Pyrethroid-pyrrole ITNs" = round(cumsum(c(0,0,0,0,dualITN_yr[5:10])),2),
  "Total" = round(cumsum(TOTAL),2)
)

pyrethroidITN_yr = as.numeric(summary_per_ITN_f(1)[[2]][3])*nets_distrib$ITNs
pyrPBOITN_yr = as.numeric(summary_per_ITN_f(1)[[2]][3])*nets_distrib$pboITNs
dualITN_yr = as.numeric(summary_per_ITN_f(1)[[2]][3])*nets_distrib$dualITNs
TOTAL = c(pyrethroidITN_yr[1:4],pyrethroidITN_yr[5:10]+pyrPBOITN_yr[5:10]+dualITN_yr[5:10])

item3_95crI <- data.frame(
  Year = 2015:2024,
  "Pyrethroid ITNs" = round(cumsum(pyrethroidITN_yr),2),
  "Pyrethroid-PBO ITNs" = round(cumsum(c(0,0,0,pyrPBOITN_yr[4:10])),2),
  "Pyrethroid-pyrrole ITNs" = round(cumsum(c(0,0,0,0,dualITN_yr[5:10])),2),
  "Total" = round(cumsum(TOTAL),2)
)

plot(item3$Total ~ item3$Year,lty = 2, col = "black",
     ylab = expression("Cumulative ktC0"[2]*"e emissions"),
     xlab = "Year",yaxt="n",
     ylim = c(0,max(item3$Total)),pch=20)
axis(2, las = 2, at = seq(0,80000,20000),labels = seq(0,80,20))

lines(item3$Total ~ item3_95crI$Year,col = "black",lwd=2)
polygon(c(item3$Year,rev(item3$Year)),
        c(item3$Pyrethroid.ITNs,
          rep(0,10)),
        col = adegenet::transp("blue",0.2),border=NA)

item3$stand_and_pbo = item3$Pyrethroid.ITNs+item3$Pyrethroid.PBO.ITNs
polygon(c(item3$Year,rev(item3$Year)),
        c(item3$stand_and_pbo,
          rep(0,10)),
        col = adegenet::transp("blue",0.2),border=NA)
text(2022, 20000,"Pyrethroid-ITNs")
text(2022, 70000,"Pyrethroid-PBO ITNs")
text(2020, 86000,"Pyrethroid-Pyrrole ITNs")
arrows(x0 = 2022, y0 = 86000, x1 = 2023.8, y1 = 86000, 
       length = 0.1, angle = 30,
       code = 2, lty = 1,
       lwd = 2)

