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

temp <- corr.test(as.matrix(facetask)) 
# use these p values to make the stars

face_cor <- reshape2::melt(face_cor) # use pivot longer instead

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
plot.psych(face_pca)

face_loadings <- face_pca[["loadings"]]
face_loadings <- data.frame(matrix(as.numeric(face_loadings), attributes(face_loadings)$dim, 
                  dimnames=attributes(face_loadings)$dimnames))
# RC2 vs RC5
ggplot(face_loadings, aes(x=RC2, y=RC5)) + 
      geom_point(size = 2)+
      ggtitle("Facetask: RC2 vs. RC5")

ggscatterstats(face_loadings, x=RC2, y= RC5)+
      ggtitle("Facetask: RC2 vs. RC5")

# Flanker
flanker <- flanker_wide[,sapply(flanker_wide, is.numeric)]
flanker[,c("NumCompletions", "n", "congruent_n", "incongruent_n", "session", "block")] <- NULL
flanker <- scale(flanker)

flank_pca <- psych::principal(flanker, nfactors = 10)
plot.psych(flank_pca)

flank_loadings <- flank_pca[["loadings"]]
flank_loadings <- data.frame(matrix(as.numeric(flank_loadings), attributes(flank_loadings)$dim, 
                                   dimnames=attributes(flank_loadings)$dimnames))
# RC1 vs RC2
ggplot(flank_loadings, aes(x=RC1, y=RC2)) + 
      geom_point(size = 2)+
      ggtitle("Flanker: RC1 vs. RC2")

ggscatterstats(flank_loadings, x=RC1, y= RC2)+
      ggtitle("Flanker: RC1 vs. RC2")
