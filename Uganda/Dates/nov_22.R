source("Uganda/dates/Wrappers_nov_22.R")

uganda_list <- setup()
face_num <- uganda_list[["facetask_num"]]
face_pca <- my_pca(face_num, 4)
face_plots <- pca_plots(face_pca)
