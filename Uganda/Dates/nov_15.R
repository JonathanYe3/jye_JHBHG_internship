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
face_cor <- data.frame(Variables = rownames(face_cor), face_cor) %>% 
      pivot_longer(cols = !Variables)

ggplot(data = face_cor, aes(x=Variables,y=name, fill=value))+geom_tile()+
      scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 10, hjust = 1))+
      coord_fixed()

# Facetask scatters with ID
face_scores <- face_pca[["scores"]] %>% matrix_to_df()

face_scatter <- plotly::plot_ly(x=face_scores[,"RC1"], y = face_scores[,"RC3"], 
                                type = "scatter", mode="markers", text=rownames(face_scores)) %>% 
      layout(title = 'Facetask Scores - RC1 vs. RC3', 
             scene = list(xaxis=list(title = 'RC1'),yaxis=list(title = 'RC3')))

# Flanker PCA
flank_pca <- psych::principal(flanker_num, nfactors = 10)

# Flanker Lollipop
flank_loadings <- flank_pca[["loadings"]] %>% matrix_to_df() %>% tibble::rownames_to_column()
flank_loadings <- pivot_longer(flank_loadings, cols = 2:11)

flank_pop <- ggplot(flank_loadings, aes(x=rowname, y=value)) +
      geom_segment( aes(x=rowname, xend=rowname, y=0, yend=value), color="skyblue") +
      geom_point( color="blue", size=3, alpha=0.6) +
      theme_light() +
      coord_flip()
flank_pop + facet_grid(cols = vars(name))

# Flanker component heatmaps
flank_loadings <- flank_pca[["loadings"]]
flank_cor <- cor(flank_loadings, use = "pairwise.complete.obs")
flank_cor <- data.frame(Variables = rownames(flank_cor), flank_cor) %>% 
      pivot_longer(cols = !Variables)

ggplot(data = flank_cor, aes(x=Variables,y=name, fill=value))+geom_tile()+
      scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 10, hjust = 1))+
      coord_fixed()

# Flanker scatters with ID
flank_scores <- flank_pca[["scores"]] %>% matrix_to_df()

flank_scatter <- plotly::plot_ly(x=flank_scores[,"RC1"], y = flank_scores[,"RC3"], 
                                type = "scatter", mode="markers", text=rownames(flank_scores)) %>% 
      layout(title = 'Flanker Scores - RC1 vs. RC3', 
             scene = list(xaxis=list(title = 'RC1'),yaxis=list(title = 'RC3')))

