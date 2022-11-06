source("Uganda/R/setup.R")

# Format data for PCA - get rid of cols that have NA - scale
facetask_num <- facetask_num[ , colSums(is.na(facetask_num)) == 0] %>% scale()
flanker_num <- flanker_num[ , colSums(is.na(flanker_num)) == 0] %>% scale()

# Facetask PCA
face_pca <- psych::principal(facetask_num, nfactors = 6)

      # Scree Plot
face_vaccounted <- face_pca[["Vaccounted"]] %>% t() %>% matrix_to_df()

face_s1 <- ggplot(data = face_vaccounted, aes(x= reorder(rownames(face_vaccounted), desc(Proportion.Var)), y=Proportion.Var))+
      geom_bar(stat="identity", width=0.5)

face_s2 <- ggplot(data = face_vaccounted, aes(x= reorder(rownames(face_vaccounted), desc(Proportion.Var)), y=Cumulative.Proportion, group=1))+
      geom_line() + geom_point()

      # Lollipop Plot
face_loadings <- face_pca[["loadings"]] %>% matrix_to_df()

face_lollipop <- plotly::plot_ly(x=face_loadings[,"RC1"], y=face_loadings[,"RC2"], z=face_loadings[,"RC3"],
                color = rownames(face_loadings), type="scatter3d", mode="markers")
face_lollipop <- face_lollipop %>% 
      layout(title = 'Facetask Lollipop - first 3 components', 
             scene = list(xaxis=list(title = 'RC1'),yaxis=list(title = 'RC2'),zaxis=list(title = 'RC3')))

      # Scatterplot of scores - for 2 components
face_scores <- face_pca[["scores"]] %>% matrix_to_df()

face_scatter <- plotly::plot_ly(x=face_scores[,"RC1"], y = face_scores[,"RC3"], 
                                type = "scatter", mode="markers") %>% 
      layout(title = 'Facetask Scores - RC1 vs. RC3', 
             scene = list(xaxis=list(title = 'RC1'),yaxis=list(title = 'RC3')))

      #Results
face_s1
face_s2
face_lollipop
face_scatter

# Flanker PCA
flank_pca <- psych::principal(flanker_num, nfactors = 10)

      # Scree Plot
flank_vaccounted <- flank_pca[["Vaccounted"]] %>% t() %>% matrix_to_df()

flank_s1 <- ggplot(data = flank_vaccounted, aes(x= reorder(rownames(flank_vaccounted), desc(Proportion.Var)), y=Proportion.Var))+
      geom_bar(stat="identity", width=0.5)

flank_s2 <- ggplot(data = flank_vaccounted, aes(x= reorder(rownames(flank_vaccounted), desc(Proportion.Var)), y=Cumulative.Proportion, group=1))+
      geom_line() + geom_point()

      # Lollipop Plot
flank_loadings <- flank_pca[["loadings"]] %>% matrix_to_df()

flank_lollipop <- plotly::plot_ly(x=flank_loadings[,"RC1"], y=flank_loadings[,"RC2"], z=flank_loadings[,"RC3"],
                                 color = rownames(flank_loadings), type="scatter3d", mode="markers")
flank_lollipop <- flank_lollipop %>% 
      layout(title = 'Flanker Lollipop - first 3 components', 
             scene = list(xaxis=list(title = 'RC1'),yaxis=list(title = 'RC2'),zaxis=list(title = 'RC3')))

      # Scatterplot of scores - for 2 components
flank_scores <- flank_pca[["scores"]] %>% matrix_to_df()

flank_scatter <- plotly::plot_ly(x=flank_scores[,"RC1"], y = flank_scores[,"RC3"], 
                                type = "scatter", mode="markers") %>% 
      layout(title = 'Flanker Scores - RC1 vs. RC3', 
             scene = list(xaxis=list(title = 'RC1'),yaxis=list(title = 'RC3')))

      #Results
flank_s1
flank_s2
flank_lollipop
flank_scatter
