pacman::p_load("dplyr", "tidyr", "ggplot2", "ggstatsplot", "readxl", "corrplot", "reshape")

# Load data
Uganda_TwoTests <- "Uganda/Data/Uganda_TwoTests.xlsx"
flanker_wide <- read_excel(path = Uganda_TwoTests, sheet = 4)
facetask_wide <- read_excel(path = Uganda_TwoTests, sheet = 3)

# Scatterplots
ggscatterstats(facetask_wide, x=n_Accurate, y= PercentAccurate_Overall)+
      ggtitle("Facetask: Number Accurate vs. Percent Accurate Overall")

ggscatterstats(facetask_wide, x=n_Accurate, y=responseTime_Overall)+
      ggtitle("Facetask: Number Accurate vs. Response Time Overall")
ggscatterstats(flanker_wide, x=n_Accurate, y=responseTime_Overall)+
      ggtitle("Flanker: Number Accurate vs. Response Time Overall")

ggscatterstats(facetask_wide, x=n_Accurate, y=responseTime_std_Overall)+
      ggtitle("Facetask: Number Accurate vs. Response Time STD Overall")
ggscatterstats(flanker_wide, x=n_Accurate, y=responseTime_std_Overall)+
      ggtitle("Flanker: Number Accurate vs. Response Time STD Overall")

# Correlation Matrices

#Facetask
temp <- facetask_wide[,sapply(facetask_wide, is.numeric)]
temp[,c("NumCompletions", "n", "go_n", "nogo_n")] <- NULL
face_cor <- cor(temp, use = "pairwise.complete.obs")
corrplot(face_cor, method = 'circle', order = 'AOE', tl.cex = 0.5, diag = FALSE)

#Flanker
temp <- flanker_wide[,sapply(facetask_wide, is.numeric)]
temp[,c("NumCompletions", "n", "congruent_n", "incongruent_n")] <- NULL
flank_cor <- cor(temp, use = "pairwise.complete.obs")
corrplot(flank_cor, method = 'circle', order = 'AOE',tl.cex = 0.5, diag = FALSE)
