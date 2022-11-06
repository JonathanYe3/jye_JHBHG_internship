library(ggsignif)
source("Uganda/R/setup.R")

# Facetask correlation matrix
face_cor <- cor(facetask_num, use = "pairwise.complete.obs") 

stars <- corr.test(as.matrix(facetask_num))

face_cor <- data.frame(Variables = rownames(face_cor), face_cor) %>% 
      pivot_longer(cols = !Variables)

ggplot(data = face_cor, aes(x=Variables,y=name, fill=value))+geom_tile()+
      scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 10, hjust = 1))+
      coord_fixed() + 
      
      geom_signif(comparisons = list(c("Variables", "name")),
                  data = as.data.frame(stars[["stars"]]))

# Flanker correlation matrix
flank_cor <- cor(flanker_num, use = "pairwise.complete.obs")
