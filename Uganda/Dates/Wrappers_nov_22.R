pacman::p_load("dplyr", "tidyr", "ggplot2", "ggstatsplot", "readxl", "corrplot", 
               "reshape2", "psych", "patchwork", "plotly")

#' Sets up Uganda tibbles
#' @param face_null_cols the columns of facetask to set as null
#' @param flank_null_cols the columns of flanker to set as null

setup <- function(face_null_cols = c("NumCompletions", "n", "go_n", "nogo_n", "session", "block"),
                  flank_null_cols = c("NumCompletions", "n", "congruent_n", "incongruent_n", "session", "block"))
      {
      # Load data
      Uganda_TwoTests <- "Uganda/Data/Uganda_TwoTests.xlsx"
      facetask_wide <- read_excel(path = Uganda_TwoTests, sheet = 3)
      flanker_wide <- read_excel(path = Uganda_TwoTests, sheet = 4)
      
      facetask_num <- facetask_wide[,sapply(facetask_wide, is.numeric)]
      facetask_num[,face_null_cols] <- NULL
      
      flanker_num <- flanker_wide[,sapply(facetask_wide, is.numeric)]
      flanker_num[,flank_null_cols] <- NULL
      
      my_list <- list(facetask_wide, facetask_num, flanker_wide, flanker_num)
      names(my_list) <- c("facetask_wide", "facetask_num", "flanker_wide", "flanker_num")
      return(my_list)
}

# Matrix to dataframe function
#' @param mat a numeric matrix
matrix_to_df <- function(mat){
      df <- data.frame(matrix(as.numeric(mat), attributes(mat)$dim, 
                              dimnames=attributes(mat)$dimnames))
      return(df)
}


#' Function that performs pca on a uganda dataset
#' @param df a uganda tibble/dataframe, must be the numerical one
#' @param num_factors nfactors for the principal function (4 for facetask, 3 for flanker)
my_pca <- function(df, num_factors){
      df <- df[ , colSums(is.na(df)) == 0] %>% scale()
      face_pca <- psych::principal(df, nfactors = num_factors)

      return(face_pca)
}

#' Function that returns scree plot, lollipop, heatmap
#' @param pca output from my_pca()
#' 
pca_plots <- function(pca, comp_1 = "RC1", comp_2 = "RC2"){
      # lollilop plot
      loadings <- pca[["loadings"]] %>% matrix_to_df() %>% tibble::rownames_to_column()
      loadings <- pivot_longer(loadings, cols = 2:ncol(loadings))
      
      pop <- ggplot(loadings, aes(x=rowname, y=value)) +
            geom_segment( aes(x=rowname, xend=rowname, y=0, yend=value), color="skyblue") +
            geom_point( color="blue", size=3, alpha=0.6) +
            theme_light() +
            coord_flip()
      pop <- pop + facet_grid(cols = vars(name))
      
      # Scree
      vaccounted <- pca[["Vaccounted"]] %>% t() %>% matrix_to_df()
      
      scree <- ggplot(data = vaccounted, aes(x= reorder(rownames(vaccounted), desc(Proportion.Var)))) + 
            geom_bar(stat="identity", width=0.5, aes(y=Proportion.Var)) +
            geom_line(aes(y= Cumulative.Proportion, group=1)) + geom_point(aes(y= Cumulative.Proportion, group=1))
      
      # Scatter
      scores <- pca[["scores"]] %>% matrix_to_df()
      
      scatter <- plotly::plot_ly(x=scores[,comp_1], y = scores[,comp_2], 
                                      type = "scatter", mode="markers", text=rownames(scores)) %>% 
            layout(title = paste("Scores - ", comp_1, " vs. ", comp_2), 
                   scene = list(xaxis=list(title = comp_1),yaxis=list(title = comp_2)))
      
      plots_list <- list(pop, scree, scatter)
      names(plots_list) <- c("pop", "scree", "scatter")
      return(plots_list)
}

#' Outputs correlation heatmap for the components of a pca
#' @param pca output from my_pca
#' 
pca_heatmap <- function(pca){
      loadings <- pca[["loadings"]]
      cor <- cor(loadings, use = "pairwise.complete.obs")
      cor <- data.frame(Variables = rownames(cor), cor) %>% 
            pivot_longer(cols = !Variables)
      
      ggplot(data = cor, aes(x=Variables,y=name, fill=value))+geom_tile()+
            scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                                 midpoint = 0, limit = c(-1,1), space = "Lab", 
                                 name="Pearson\nCorrelation") +
            theme_minimal()+ 
            theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                             size = 10, hjust = 1))+
            coord_fixed()+
            geom_signif(comparisons = list(c("RC2","RC3"))) #### help
}
