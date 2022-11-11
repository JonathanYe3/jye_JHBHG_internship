#' Objectives:
#' Create actual lollipop plots of the individual loadings, then side by side display
#' Make correlation plots/heatmaps for the components themselves
#' Add identifying lables to the points

source("Uganda/R/setup.R")

# Format data for PCA - get rid of cols that have NA - scale
facetask_num <- facetask_num[ , colSums(is.na(facetask_num)) == 0] %>% scale()
flanker_num <- flanker_num[ , colSums(is.na(flanker_num)) == 0] %>% scale()

# Facetask PCA
face_pca <- psych::principal(facetask_num, nfactors = 6)

# Facetask Lollipop
face_loadings <- face_pca[["loadings"]] %>% matrix_to_df() %>% tibble::rownames_to_column()
face_loadings <- pivot_longer(face_loadings, cols = 2:7)

face_pop <- ggplot(face_loadings, aes(x=rowname, y=value)) +
      geom_segment( aes(x=rowname, xend=rowname, y=0, yend=value), color="skyblue") +
      geom_point( color="blue", size=3, alpha=0.6) +
      theme_light() +
      coord_flip()
face_pop + facet_grid(cols = vars(name))

# Facetask component heatmaps
face_loadings <- face_pca[["loadings"]]
face_cor <- cor(face_loadings, use = "pairwise.complete.obs") 

