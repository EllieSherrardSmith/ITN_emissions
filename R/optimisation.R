######################
##
## Optimisation

library(dplyr)
library(tidyr)
library(rPref)      # for Pareto frontier
library(ggplot2)
library(GGally)

# Example objectives (edit these)
obj_max <- c("efficacy", "durability", "impact")                    # want these HIGH
obj_min <- c("cost", "env_impact", "risk") # want these LOW

## first up we detext correlations:
detect_correlations <- function(df, objectives, threshold = 0.8) {
  m <- df[, objectives, drop = FALSE]
  cmat <- cor(m, use = "complete.obs")
  
  # return long table of correlations above threshold (absolute)
  highcorr <- which(abs(cmat) >= threshold & abs(cmat) < 1, arr.ind = TRUE)
  
  if (nrow(highcorr) == 0) return(NULL)
  
  # remove duplicates i,j and j,i
  highcorr <- highcorr[highcorr[,1] < highcorr[,2], , drop = FALSE]
  
  data.frame(
    var1 = rownames(cmat)[highcorr[,1]],
    var2 = colnames(cmat)[highcorr[,2]],
    corr = cmat[highcorr]
  )
}

## and adjust the weights
# adjust_weights_for_correlation <- function(weights, corr_table) {
# 
#   if (is.null(corr_table)) return(weights)
# 
#   w <- weights
# 
#   for (i in seq_len(nrow(corr_table))) {
#     v1 <- corr_table$var1[i]
#     v2 <- corr_table$var2[i]
#     r  <- abs(corr_table$corr[i])
# 
#     # reduce weights according to correlation
#     w[v1] <- w[v1] * (1 - r)
#     w[v2] <- w[v2] * (1 - r)
#   }
# 
#   # renormalise
#   w <- w / sum(w)
#   return(w)
# }

## or
merge_correlated_objectives <- function(df, corr_table) {
  
  if (is.null(corr_table)) return(df)
  
  df_new <- df
  
  for (i in seq_len(nrow(corr_table))) {
    v1 <- corr_table$var1[i]
    v2 <- corr_table$var2[i]
    new_name <- paste0(v1, "_", v2, "_comp")
    
    df_new[[new_name]] <- rowMeans(df_new[, c(v1, v2)])
    
    # drop originals
    df_new[[v1]] <- NULL
    df_new[[v2]] <- NULL
  }
  
  return(df_new)
}

## create a dataframe with the lists of info for the objectives above
normalise_df <- function(df, obj_max, obj_min) {
  
  df_norm <- df %>%
    mutate(
      across(all_of(obj_max),
             ~ (.-min(.))/(max(.)-min(.))),
      across(all_of(obj_min),
             ~ (max(.) - .)/(max(.)-min(.)))
    )
  
  return(df_norm)
}

obj_max <- c("efficacy", "durability", "impact")                    # want these HIGH
obj_min <- c("cost", "env_impact") # want these LOW


df <- data.frame(
  strategy   = paste0("S", 1:6),
  cost       = c(120, 150, 90, 200, 130, 160),
  durability  = c(50, 40, 70, 30, 45, 55),
  efficacy   = c(0.65, 0.80, 0.55, 0.90, 0.72, 0.78),
  impact     = c(0.30, 0.45, 0.60, 0.25, 0.40, 0.35),
  env_impact = c(1,1,3,2,1,3),
  risk = c(1,1,2,2,3,3)
)
df

df_norm <- normalise_df(df, obj_max, obj_min)

build_preferences <- function(df_norm, obj_max, obj_min) {
  
  prefs <- list()
  
  for (col in colnames(df_norm)) {
    if (col %in% obj_max) {
      prefs[[col]] <- do.call(high, list(as.name(col)))
    } else if (col %in% obj_min) {
      prefs[[col]] <- do.call(low, list(as.name(col)))
    }
  }
  
  # multiply them together sequentially
  pref_combined <- prefs[[1]]
  if (length(prefs) > 1) {
    for (k in 2:length(prefs)) {
      pref_combined <- pref_combined * prefs[[k]]
    }
  }
  
  return(pref_combined)
}


compute_pareto <- function(df_norm, obj_max, obj_min) {
  
  # Combine into a single preference expression
  pref_combined <- build_preferences(df_norm,
                                     obj_max = c(obj_max),
                                     obj_min = c(obj_min))
  
  # Pareto selection
  pareto <- rPref::psel(df_norm[,2:6], pref_combined)
  
  return(pareto)
}

pareto_set <- compute_pareto(df_norm, obj_max, obj_min)
pareto_set

# Example weights (edit later or let user set via sliders)
weights <- c(
  cost      = 0.25,
  durability = 0.25,
  efficacy  = 0.40,
  impact    = 0.05,
  env_impact = 0.05
)

score_weighted <- function(df_norm, weights) {
  
  # ensure correct order
  df_use <- df_norm[, names(weights)]
  
  df_norm$score <- rowSums(df_use * weights)
  
  df_norm %>% arrange(desc(score))
}

ranked_pareto <- score_weighted(pareto_set, weights)
ranked_pareto

run_multiobjective_corr <- function(
    df,
    obj_max,
    obj_min,
    weights,
    corr_threshold = 0.8,
    corr_method = c("weights", "merge", "none")
) {
  corr_method <- match.arg(corr_method)
  
  # ---- STEP 1: Detect correlations ----
  all_objectives <- c(obj_max, obj_min)
  
  corr_table <- detect_correlations(df, all_objectives, threshold = corr_threshold)
  message("Detected correlations:")
  print(corr_table)
  
  # ---- STEP 2: Modify data or weights depending on method ----
  if (corr_method == "merge" && !is.null(corr_table)) {
    
    df <- merge_correlated_objectives(df, corr_table)
    
    # rebuild objective lists (dropping merged originals)
    all_new_names <- names(df)
    new_max <- obj_max[obj_max %in% all_new_names]
    new_min <- obj_min[obj_min %in% all_new_names]
    
    obj_max <- new_max
    obj_min <- new_min
    
  }
  
  if (corr_method == "weights" && !is.null(corr_table)) {
    
    weights <- adjust_weights_for_correlation(weights, corr_table)
    message("Adjusted weights:")
    print(weights)
    
  }
  
  # ---- STEP 3: Normalise ----
  df_norm <- normalise_df(df, obj_max, obj_min)
  
  # ---- STEP 4: Pareto ----
  pareto_set <- compute_pareto(df_norm, obj_max, obj_min)
  
  # ---- STEP 5: Weighted scoring ----
  ranked <- score_weighted(pareto_set, weights)
  
  return(list(
    normalised = df_norm,
    pareto_set = pareto_set,
    ranked_pareto = ranked,
    correlations = corr_table,
    adjusted_weights = weights
  ))
}

## Visualise the Pareto Set
GGally::ggparcoord(
  data = pareto_set,
  columns = match(c(obj_max, obj_min), names(pareto_set)),
  scale = "globalminmax",
  alphaLines = 0.7
) +
  theme_minimal() +
  ggtitle("Pareto-optimal strategies")

ggplot(pareto_set, aes(x = cost, y = efficacy)) +
  geom_point(size = 3) +
  theme_minimal()
# correlation matrix
obj_cols <- c("cost", "durability", "efficacy", "impact", "env_impact", "risk")
cor_mat <- cor(df[, obj_cols])

# plot heatmap
library(ggplot2)
library(reshape2)
melted <- melt(cor_mat)
ggplot(melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color="white") +
  scale_fill_gradient2(low="blue", high="red", mid="white",
                       midpoint=0, limit=c(-1,1), space="Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  ggtitle("Correlation Heatmap of Objectives")

# distance = 1 - abs(correlation)
dist_mat <- as.dist(1 - abs(cor_mat))
hc <- hclust(dist_mat, method="average")
plot(hc, main="Hierarchical Clustering of Objectives", xlab="", sub="", ylab="1 - |correlation|")

# cluster cut (e.g., threshold 0.7)
clusters <- cutree(hc, h = 1 - 0.7)

weight_table <- data.frame(
  objective = names(clusters),
  cluster = clusters
) %>%
  group_by(cluster) %>%
  mutate(weight = 1/n()) %>%
  ungroup()

weight_table


# Normalize 0-1 (higher = better for max, 1 - value for min)
df_norm <- df %>%
  mutate(
    cost = (max(cost) - cost)/(max(cost)-min(cost)),
    durability = (durability - min(durability))/(max(durability)-min(durability)),
    efficacy = (efficacy - min(efficacy))/(max(efficacy)-min(efficacy)),
    impact = (max(impact) - impact)/(max(impact)-min(impact)),
    env_impact = (max(env_impact) - env_impact)/(max(env_impact)-min(env_impact)),
    risk = (max(risk) - risk)/(max(risk)-min(risk))
  )

# Preference: weighted by correlation-adjusted weights
library(rPref)

# identify correlated clusters (threshold 0.7)
cor_mat <- cor(df[, obj_cols])
dist_mat <- as.dist(1 - abs(cor_mat))
hc <- hclust(dist_mat, method="average")
clusters <- cutree(hc, h = 1 - 0.7)

# For each cluster with more than 1 variable, create a composite
df_adj <- df
for (cl in unique(clusters)) {
  vars <- names(clusters)[clusters == cl]
  if (length(vars) > 1) {
    comp_name <- paste(vars, collapse="_comp")
    df_adj[[comp_name]] <- rowMeans(df[, vars])
    # remove original correlated columns
    df_adj <- df_adj[, !(names(df_adj) %in% vars)]
  }
}

obj_max <- c("cost_compdurability_compefficacy_compimpact")  # adjust after merging
obj_min <- c("env_impact", "risk")

df_norm <- df_adj %>%
  mutate(across(all_of(obj_max), ~ (. - min(.)) / (max(.) - min(.)))) %>%
  mutate(across(all_of(obj_min), ~ (max(.) - .) / (max(.) - min(.))))

prefs <- high("cost_compdurability_compefficacy_compimpact") * low("env_impact") * low("risk") 
pareto_set <- psel(df_norm, prefs)
pareto_set

ggplot(df_norm, aes(x=env_impact, y=cost_compdurability_compefficacy_compimpact, label=strategy)) +
  geom_point(size=3) +
  geom_point(data=pareto_set, color="red", size=3) +
  geom_text(vjust=-0.5) +
  labs(x="Normalized environmental impact (lower better)", y="Composite Efficacy+Durability (higher better)") +
  ggtitle("2D Pareto Front")

plot_ly(df_norm, x=~env_impact, y=~cost_compdurability_compefficacy_compimpact, z=~risk,
        text=~strategy, type='scatter3d', mode='markers',
        marker=list(size=5,color='blue')) %>%
  add_markers(data=pareto_set, x=~env_impact, y=~cost_compdurability_compefficacy_compimpact, z=~risk,
              marker=list(size=6,color='red'), name='Pareto') %>%
  layout(scene=list(xaxis=list(title='Environmental impact'),
                    yaxis=list(title='Efficacy+Durability'),
                    zaxis=list(title='Risk')),
         title="3D Pareto Front")

