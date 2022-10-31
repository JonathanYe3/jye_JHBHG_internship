pacman::p_load("dplyr", "tidyr", "ggplot2", "ggstatsplot", "readxl", "corrplot", "reshape2", "psych")

# Load data
Uganda_TwoTests <- "Uganda/Data/Uganda_TwoTests.xlsx"
facetask_wide <- read_excel(path = Uganda_TwoTests, sheet = 3)
flanker_wide <- read_excel(path = Uganda_TwoTests, sheet = 4)

facetask_num <- facetask_wide[,sapply(facetask_wide, is.numeric)]
flanker_num <- flanker_wide[,sapply(facetask_wide, is.numeric)]
