pacman::p_load("dplyr", "tidyr", "ggplot2", "ggstatsplot", "readxl", "corrplot", 
               "reshape2", "psych", "patchwork", "plotly")

# Load data
Uganda_TwoTests <- "Uganda/Data/Uganda_TwoTests.xlsx"
facetask_wide <- read_excel(path = Uganda_TwoTests, sheet = 3)
flanker_wide <- read_excel(path = Uganda_TwoTests, sheet = 4)

facetask_num <- facetask_wide[,sapply(facetask_wide, is.numeric)]
facetask_num[,c("NumCompletions", "n", "go_n", "nogo_n", "session", "block")] <- NULL

flanker_num <- flanker_wide[,sapply(facetask_wide, is.numeric)]
flanker_num[,c("NumCompletions", "n", "congruent_n", "incongruent_n", "session", "block")] <- NULL

# Matrix to dataframe function
#' @param mat a numeric matrix
matrix_to_df <- function(mat){
      df <- data.frame(matrix(as.numeric(mat), attributes(mat)$dim, 
                        dimnames=attributes(mat)$dimnames))
      return(df)
}