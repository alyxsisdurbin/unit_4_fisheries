# Haley Durbin
# 2023-03-23

library(palmerpenguins)
library(tidyverse)

pen_drop_na = penguins %>%
  drop_na()

pen_num = pen_drop_na %>%
  select(ends_with("mm"), body_mass_g)
head(pen_num)
pen_meta = pen_drop_na %>%
  select(species, sex, island, year)
head(pen_meta)


#run the pca
pen_pca = prcomp(pen_num, scale.=TRUE, center=TRUE) #scale normalizes all of our numerical values, center: our pc's are centered over zero.

pen_pca
summary(pen_pca) #proportion of variance determines how much of the pcs tell you about the variance
dim(pen_pca$x)

str(summary(pen_pca))
summary(pen_pca)$importance
summary(pen_pca)$importance[2,]

## calculate proportion of variance from SDevs

(pen_pca$sdev)^2/sum(pen_pca$sdev^2)

# scree plot

plot(pen_pca)

pca_scree = data.frame(pc = c(1:4),
                       var = summary(pen_pca)$importance[2,])    
pca_scree

ggplot(data=pca_scree, aes(x=pc, y=var)) +
  geom_col() +
  geom_point() +
  geom_line() +
  theme_bw() +
  ggtitle("Scree Plot") +
  ylab("Proportion of variance expained")

# create a biplot

head(pen_pca$x)
pen_pca_meta = cbind(pen_pca$x, pen_meta)
head(pen_pca_meta)

ggplot()+
  geom_point(aes(x=PC1, y=PC2, color=species, shape=sex), 
             data=pen_pca_meta) +
  coord_fixed(ratio=1)

 # library(devtools)
 # install_github("vqv/ggbiplot")
library(ggbiplot)

biplot(pen_pca) # base r's biplots

ggbiplot(pen_pca, scale=1, groups=pen_meta$species, ellipse=T, alpha=0) + #alpha 0 makes the points disappear, but leaves ellipses 
  geom_point(aes(color=pen_meta$species, shape=pen_meta$sex)) +
  xlim(-3,3)

pen_pca
