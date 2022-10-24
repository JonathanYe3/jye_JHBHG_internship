pacman::p_load("dplyr", "tidyr", "ggplot2", "ggstatsplot", "readxl", "corrplot", "reshape", "psych")

# Load data
Uganda_TwoTests <- "Uganda/Data/Uganda_TwoTests.xlsx"
flanker_wide <- read_excel(path = Uganda_TwoTests, sheet = 4)
facetask_wide <- read_excel(path = Uganda_TwoTests, sheet = 3)

# New heat map
# Facetask
facetask <- facetask_wide[,sapply(facetask_wide, is.numeric)]
facetask[,c("NumCompletions", "n", "go_n", "nogo_n", "session", "block")] <- NULL
face_cor <- cor(facetask, use = "pairwise.complete.obs")
