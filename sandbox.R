library(tidyverse)
library(maps)
library(ggplot2)
library(skimr)
library(rgl)
library(MASS)

bridges = read.csv("NTAD_National_Bridge_Inventory_5655614151403109778.csv")

unique(bridges$STATE_CODE_001)
ne_bridges = bridges %>% 
  filter(STATE_CODE_001 == 31, DECK_COND_058 != "N",
         SUBSTRUCTURE_COND_060 != "N", SUPERSTRUCTURE_COND_059 != "N",
         SERVICE_UND_042B %in% c("5","6","7","8","9")) %>%
  select(COUNTY_CODE_003, RECORD_TYPE_005A,YEAR_BUILT_027,TRAFFIC_LANES_ON_028A,
         TRAFFIC_LANES_UND_028B,ADT_029,YEAR_ADT_030,SERVICE_UND_042B,
         MAX_SPAN_LEN_MT_048,STRUCTURE_LEN_MT_049,ROADWAY_WIDTH_MT_051,
         DECK_WIDTH_MT_052,VERT_CLR_OVER_MT_053,DECK_COND_058,
         SUPERSTRUCTURE_COND_059,SUBSTRUCTURE_COND_060,LATDD,LONGDD) %>%
  rename(YearBuilt = YEAR_BUILT_027, MaxSpan = MAX_SPAN_LEN_MT_048, 
         ADT = ADT_029, DeckCond = DECK_COND_058, 
         SuperstructureCond = SUPERSTRUCTURE_COND_059, 
         SubstructureCond = SUBSTRUCTURE_COND_060) %>%
  mutate(IsOmaha = (COUNTY_CODE_003 == 55), DeckCond = as.numeric(DeckCond), 
         SuperstructureCond = as.numeric(SuperstructureCond), SubstructureCond = as.numeric(SubstructureCond)) 

omaha_bridges = ne_bridges %>%
  filter(COUNTY_CODE_003 == 55) %>%
  dplyr::select(-c(IsOmaha, COUNTY_CODE_003, RECORD_TYPE_005A))

dim(ne_bridges)
head(ne_bridges)

min(bridges$LONGDD)

nebraska = map_data("state") %>%
  filter(region == "nebraska")

ggplot() +
  geom_polygon(data = nebraska,aes(x = long, y = lat, group = group),
               fill = "white", color = "black", linewidth = 1) +
  geom_point(data = ne_bridges,aes(x = LONGDD, y = LATDD, color = IsOmaha)
             , size = 1) +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(x = "Longitude",y = "Latitude",title = "Bridges over Waterways in Nebraska", color = "Is in Douglas County?")

scottkey = bridges %>%
  filter(STATE_CODE_001 == 24, ROUTE_PREFIX_005B == 1, 
         MAX_SPAN_LEN_MT_048 > 350, YEAR_BUILT_027 == 1976) %>%
  dplyr::select(YEAR_BUILT_027,TRAFFIC_LANES_ON_028A,
         TRAFFIC_LANES_UND_028B,ADT_029,YEAR_ADT_030,SERVICE_UND_042B,
         MAX_SPAN_LEN_MT_048,STRUCTURE_LEN_MT_049,ROADWAY_WIDTH_MT_051,
         DECK_WIDTH_MT_052,VERT_CLR_OVER_MT_053,DECK_COND_058,
         SUPERSTRUCTURE_COND_059,SUBSTRUCTURE_COND_060,LATDD,LONGDD) %>%
  rename(YearBuilt = YEAR_BUILT_027, MaxSpan = MAX_SPAN_LEN_MT_048, 
         ADT = ADT_029, DeckCond = DECK_COND_058, 
         SuperstructureCond = SUPERSTRUCTURE_COND_059, 
         SubstructureCond = SUBSTRUCTURE_COND_060) %>%
  mutate(DeckCond = as.numeric(DeckCond), 
         SuperstructureCond = as.numeric(SuperstructureCond), SubstructureCond = as.numeric(SubstructureCond)) 

scottkey

mn = bridges %>%
  filter(STATE_CODE_001 == 27, ROUTE_PREFIX_005B == 1, LATDD > 44.97, LATDD < 44.99, LONGDD > -93.246, STRUCTURE_TYPE_043B == 21)

mn

d = dist(omaha_bridges)

hier_bridges = hclust(d)
plot(hier_bridges)

cor(omaha_bridges)
pairs(omaha_bridges)
pca = princomp(omaha_bridges)
skim(omaha_bridges)
skim(scottkey)

summary(pca)
plot(pca)

km = kmeans(omaha_bridges, centers = 3)
km

summary(km)

#clustering

#Note that the colors used in the plots correspond to the cluster numbers. The function palette() shows the
#  colors corresponding to the numbers. There are ONLY 8 colors listed. If the number of clusters is > 8,
#  the colors will REPEAT, which would not be desirable for a plot of this type!
PCA.CA.plot <- function(data.set, cluster.results, numb.clust, plot.title,
                        cor.use = TRUE, inches = 0.5, label.obs = FALSE, adjust3Dlabel = 0.25) {
  
  clusters <- cutree(tree = cluster.results, k = numb.clust)
  
  #PC scores
  pca.cor <- princomp(x = data.set, cor = cor.use, scores = FALSE)
  pca.cor$scale <- apply(X = data.set, MARGIN = 2, FUN = sd)
  score.cor <- predict(pca.cor, newdata = data.set)
  
  #Scatter plot of first two PCs
  dev.new()
  par(pty = "s")
  common.limits <- c(min(score.cor[,1:2]), max(score.cor[,1:2]))
  plot(x = score.cor[,1], y = score.cor[,2], xlab = "PC #1", ylab = "PC #2",
       main = paste("PCs with", plot.title, "and", numb.clust, "clusters"),
       xlim = common.limits, ylim = common.limits, panel.first = grid(col = "lightgray", lty = "dotted"),
       col = clusters, pch = clusters)
  abline(h = 0)
  abline(v = 0)
  text(x = score.cor[,1], y = score.cor[,2]+0.2)
  
  #Bubble plot of first three PCs
  dev.new()
  par(pty = "s")
  PC3.positive <- score.cor[,3] - min(score.cor[,3])  #Bubble needs to contain all values > 0
  col.symbol <- ifelse(test = score.cor[,3]>0, yes = "red", no = "blue")
  symbols(x = score.cor[,1], y = score.cor[,2], circles = PC3.positive,
          xlab = "PC #1", ylab = "PC #2", main = paste("PCs with", plot.title, "and", numb.clust, "clusters"), inches = inches,
          xlim = common.limits, ylim = common.limits, panel.first = grid(col = "lightgray", lty = "dotted"),
          fg = col.symbol)
  text(x = score.cor[,1], y = score.cor[,2], col = clusters)
  abline(h = 0)
  abline(v = 0)
  
  #3D plot - Note: I used common limits for all three dimensions here because the distance between points is important
  plot3d(x = score.cor[,1], y = score.cor[,2], z = score.cor[,3], xlab = "PC #1", ylab = "PC #2",
         zlab = "PC #3", type = "h", xlim = common.limits, ylim = common.limits, zlim = common.limits)
  plot3d(x = score.cor[,1], y = score.cor[,2], z = score.cor[,3], add = TRUE, col = clusters, size = 6)
  persp3d(x = common.limits, y = common.limits, z = matrix(data = c(0,0,0,0), nrow = 2, ncol = 2),
          add = TRUE, col = "green") #Put a plane on the plot
  grid3d(side = c("x", "y", "z"), col = "lightgray")
  if (label.obs) {
    text3d(x = score.cor[,1], y = score.cor[,2], z = score.cor[,3]+adjust3Dlabel, text = 1:nrow(data.set))
  }
  invisible()
}

dist.mat2 <- dist(x = omaha_bridges, method = "euclidean")
clust.nn2 <- hclust(d = dist.mat2, method = "single")
data.frame(obs.cluster = clust.nn2$merge, clust.nn2$height)

plot(clust.nn2)
abline(h = 36000, lty = "dashed", lwd = 2)
abline(h = 30000, lty = "dashed", lwd = 2)
abline(h = 20000, lty = "dashed", lwd = 2)
abline(h = 15000, lty = "dashed", lwd = 2)

PCA.CA.plot(data.set = omaha_bridges, cluster.results = clust.nn2, numb.clust = 2,
            plot.title = "nearest neighbor CA method", cor.use = FALSE)
PCA.CA.plot(data.set = omaha_bridges, cluster.results = clust.nn2, numb.clust = 3,
            plot.title = "nearest neighbor CA method", cor.use = FALSE)
PCA.CA.plot(data.set = omaha_bridges, cluster.results = clust.nn2, numb.clust = 4,
            plot.title = "nearest neighbor CA method", cor.use = FALSE)
clusters2 <- cutree(tree = clust.nn2, k = 3)
par(pty = "m")
parcoord(x = omaha_bridges, col = clusters2, main = "Omaha parallel coordinate plot",
         lwd = clusters2)

kmeans_result <- kmeans(scale(omaha_bridges), centers = 3, nstart = 25)
omaha_bridges$Cluster <- as.factor(kmeans_result$cluster)

pre_proc <- preProcess(omaha_bridges %>% 
                         dplyr::select(-Cluster), method = c("center", "scale"))
omaha_scaled <- predict(pre_proc, omaha_bridges 
                        %>% dplyr::select(-Cluster))

pca_result <- prcomp(omaha_scaled, scale. = FALSE)  # already scaled above
pca_data <- as.data.frame(pca_result$x[, 1:2])

# Add cluster assignments
pca_data$Cluster <- omaha_bridges$Cluster
# Plot PCA results with clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering with PCA",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

print(head(sort(abs(kmeans_result$centers[1,]), decreasing = TRUE)))
print(head(sort(abs(kmeans_result$centers[2,]), decreasing = TRUE)))
print(head(sort(abs(kmeans_result$centers[3,]), decreasing = TRUE)))


library(caret)
library(FNN)

set.seed(123)

# Split into training and test sets
trainIndex <- createDataPartition(omaha_bridges[,1], p = 0.8, list = FALSE)
trainData <- omaha_bridges[trainIndex, ]  # only numeric features
testData  <- omaha_bridges[-trainIndex, ]

k <- 5
nn_result <- get.knnx(data = trainData, query = scottkey, k = k)

nn_result$nn.index
nn_result$nn.dist

neighbors <- trainData[nn_result$nn.index[1, ], ]
print(neighbors[,15:16])

library(tigris)
library(sf)

ne_counties <- counties(state = "NE", cb = TRUE, progress_bar = FALSE)
douglas <- ne_counties %>% filter(NAME == "Douglas")

ggplot() +
  geom_sf(data = douglas) +
  geom_point(data = neighbors, aes(x = LONGDD, y = LATDD), 
             color = "navyblue", size = 2, alpha = 0.6) +
  theme_void() + 
  labs(x = "Longitude",y = "Latitude",title = "Bridges over Waterways in Nebraska")

ggplot() +
  geom_sf(data = douglas) +
  geom_point(data = neighbors, aes(x = LONGDD, y = LATDD),
             color = "steelblue", size = 2, alpha = 0.6) +
  coord_sf(xlim = c(-96.6, -95.9), ylim = c(41.1, 41.5)) +
  theme_void() + 
  labs(x = "Longitude",y = "Latitude",title = "Bridges over Waterways in Nebraska")


# Try getting more complete river data from the NHD
library(nhdplusTools)

big_pap_nhd <- get_nhdplus(AOI = st_transform(douglas, 4326), 
                           realization = "flowline") %>%
  filter(str_detect(gnis_name, "Papillion"))

ggplot() +
  geom_sf(data = douglas) +
  geom_sf(data = big_pap_nhd, color = "blue", linewidth = 0.7) +
  geom_point(data = neighbors, aes(x = LONGDD, y = LATDD),
             color = "red", size = 2, alpha = 0.6) +
  theme_minimal() +
  labs(x = "Longitude",y = "Latitude",title = "Top 5 Most Similar Bridges in Omaha")
