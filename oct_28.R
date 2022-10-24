pacman::p_load("dplyr", "tidyr", "ggplot2", "ggstatsplot", "readxl", "corrplot", "reshape2", "psych")

# Load data
Uganda_TwoTests <- "Uganda/Data/Uganda_TwoTests.xlsx"
flanker_wide <- read_excel(path = Uganda_TwoTests, sheet = 4)
facetask_wide <- read_excel(path = Uganda_TwoTests, sheet = 3)

# New heat map
# Facetask
facetask <- facetask_wide[,sapply(facetask_wide, is.numeric)]
facetask[,c("NumCompletions", "n", "go_n", "nogo_n", "session", "block")] <- NULL
face_cor <- cor(facetask, use = "pairwise.complete.obs")
face_cor <- reshape2::melt(face_cor)

ggplot(data = face_cor, aes(x=Var1,y=Var2, fill=value))+geom_tile()+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 10, hjust = 1))+
      coord_fixed()

# Flanker
flanker <- flanker_wide[,sapply(facetask_wide, is.numeric)]
flanker[,c("NumCompletions", "n", "congruent_n", "incongruent_n", "session", "block")] <- NULL
flank_cor <- cor(flanker, use = "pairwise.complete.obs")
flank_cor <- reshape2::melt(flank_cor)

ggplot(data = flank_cor, aes(x=Var1,y=Var2, fill=value))+geom_tile()+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 10, hjust = 1))+
      coord_fixed()

########################################################
# Moving on to the PCA - objective is to scale the data first, not the correlation matrix
flanker_wide <- read_excel(path = Uganda_TwoTests, sheet = 4)
facetask_wide <- read_excel(path = Uganda_TwoTests, sheet = 3)

# Facetask
facetask <- facetask_wide[,sapply(facetask_wide, is.numeric)]
facetask[,c("NumCompletions", "n", "go_n", "nogo_n", "session", "block")] <- NULL
facetask <- scale(facetask)

face_pca <- psych::principal(facetask, nfactors = 6)
