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
