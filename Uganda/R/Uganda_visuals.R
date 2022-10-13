pacman::p_load("dplyr", "tidyr", "ggplot2", "ggstatsplot", "readxl", "corrplot", "reshape")

# Load data
Uganda_TwoTests <- "Uganda/Data/Uganda_TwoTests.xlsx"
flanker_wide <- read_excel(path = Uganda_TwoTests, sheet = 4)
facetask_wide <- read_excel(path = Uganda_TwoTests, sheet = 3)

# Scatterplots
ggplot(facetask_wide, aes(x=n_Accurate, y=PercentAccurate_Overall)) + 
      geom_point(size = 2)+
      geom_smooth()+
      ggtitle("Facetask: Number Accurate vs. Percent Accurate Overall")

ggplot(facetask_wide, aes(x=n_Accurate, y=responseTime_Overall)) + 
      geom_point(size = 2)+
      geom_smooth()+
      ggtitle("Facetask: Number Accurate vs. Response Time Overall")

ggplot(flanker_wide, aes(x=n_Accurate, y=responseTime_Overall)) + 
      geom_point(size = 2)+
      geom_smooth()+
      ggtitle("Flanker: Number Accurate vs. Response Time Overall")

ggplot(facetask_wide, aes(x=n_Accurate, y=responseTime_std_Overall)) + 
      geom_point(size = 2)+
      geom_smooth()+
      ggtitle("Facetask: Number Accurate vs. Response Time STD Overall")

ggplot(flanker_wide, aes(x=n_Accurate, y=responseTime_std_Overall)) + 
      geom_point(size = 2)+
      geom_smooth()+
      ggtitle("Flanker: Number Accurate vs. Response Time STD Overall")

# Correlation Matrices

# Facetask corrplot
temp <- facetask_wide[ , colSums(is.na(facetask_wide)) == 0]
temp <- temp[,-c(1:6, 8, 16, 24)]
dropped_cols_face <- setdiff(names(facetask_wide),names(temp))
face_cor <- cor(temp)
corrplot(face_cor, method = 'number', order = 'AOE', diag = FALSE)
corrplot(face_cor, method = 'shade', order = 'AOE', diag = FALSE)

# ggplot Facetask

face_cor <- round(cor(temp),2)
face_cor <- melt(face_cor)
ggplot(data = face_cor, aes(X1, X2, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 10, hjust = 1))+
      coord_fixed()

# Flanker
temp <- flanker_wide[ , colSums(is.na(flanker_wide)) == 0]
temp <- temp[,-c(1:6, 8, 16, 24)]
dropped_cols_flank <- setdiff(names(flanker_wide),names(temp))
flank_cor <- cor(temp)
corrplot(flank_cor, method = 'number', order = 'AOE', diag = FALSE)
corrplot(flank_cor, method = 'shade', order = 'AOE', diag = FALSE)

# ggplot Flanker

flank_cor <- round(cor(temp),2)
flank_cor <- melt(face_cor)
ggplot(data = flank_cor, aes(X1, X2, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 10, hjust = 1))+
      coord_fixed()

