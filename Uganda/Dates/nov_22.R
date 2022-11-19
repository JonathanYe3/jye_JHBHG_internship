source("Uganda/dates/Wrappers_nov_22.R")

uganda_list <- setup()

# facetask
face_num <- uganda_list[["facetask_num"]]
face_pca <- my_pca(face_num, 4)
face_plots <- pca_plots(face_pca)
face_heatmap <- pca_heatmap(face_pca)
