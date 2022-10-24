pacman::p_load("dplyr", "tidyr", "ggplot2", "ggstatsplot", "readxl", "corrplot", "reshape", "psych")

# Load data
Uganda_TwoTests <- "Uganda/Data/Uganda_TwoTests.xlsx"
flanker_wide <- read_excel(path = Uganda_TwoTests, sheet = 4)
facetask_wide <- read_excel(path = Uganda_TwoTests, sheet = 3)

# Facetask
facetask <- facetask_wide[,sapply(facetask_wide, is.numeric)]
facetask[,c("NumCompletions", "n", "go_n", "nogo_n")] <- NULL
face_cor <- cor(facetask, use = "pairwise.complete.obs") %>% scale()

face_pca <- psych::principal(facetask, nfactors = 10)
base_face_pca <- prcomp(face_cor)

ggbiplot(base_face_pca)

# Flanker
flanker <- flanker_wide[,sapply(facetask_wide, is.numeric)]
flanker[,c("NumCompletions", "n", "congruent_n", "incongruent_n")] <- NULL
flank_cor <- cor(flanker, use = "pairwise.complete.obs") %>% scale()

flank_pca <- psych::principal(flank_cor)
base_flank_pca <- prcomp(flank_cor)

ggbiplot(base_flank_pca)

