source("Uganda/R/setup.R")

# Format data for PCA - get rid of cols that have NA - scale
facetask_num <- facetask_num[ , colSums(is.na(facetask_num)) == 0] %>% scale()
flanker_num <- flanker_num[ , colSums(is.na(flanker_num)) == 0] %>% scale()

# Facetask PCA
face_pca <- psych::principal(facetask_num, nfactors = 6)

      # Scree Plot
face_vaccounted <- face_pca[["Vaccounted"]] %>% t()
face_vaccounted <- data.frame(matrix(as.numeric(face_vaccounted), attributes(face_vaccounted)$dim, 
                  dimnames=attributes(face_vaccounted)$dimnames))

p1 <- ggplot(data = face_vaccounted, aes(x= reorder(rownames(face_vaccounted), desc(Proportion.Var)), y=Proportion.Var))+
      geom_bar(stat="identity", width=0.5)

p2 <- ggplot(data = face_vaccounted, aes(x= reorder(rownames(face_vaccounted), desc(Proportion.Var)), y=Cumulative.Proportion, group=1))+
      geom_line() + geom_point()
p1+p2

      