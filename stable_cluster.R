

stable_cluster <- function(data, k, m){##k is number of clusters, m is sample proportion
  
  size <- m*nrow(data)
  data$id <- 1:nrow(data)
  
  ##subsample
  sample1_id <- sample(1:nrow(data), size, replace = FALSE)
  sample2_id <- sample(1:nrow(data), size, replace = FALSE)
  
  sub_1 <- data[sample1_id, ]
  sub_2 <- data[sample2_id, ]
  
  ##k-means
  ##use all features in data for clustering
  result_1 <- data.frame(kmeans(sub_1, centers = k,iter.max = 100000)$cluster) ##try to alleviate the impact of random starting value
  result_2 <- data.frame(kmeans(sub_2, centers = k,iter.max = 100000)$cluster)
  
  ##cluster result
  sub_1_clust <- data.frame(cbind(sub_1$id,result_1))
  names(sub_1_clust)[1] <- "id"
  names(sub_1_clust)[2] <- "cluster"
  sub_2_clust <- data.frame(cbind(sub_2$id,result_2))
  names(sub_2_clust)[1] <- "id"
  names(sub_2_clust)[2] <- "cluster"
  
  ##find intersected obs and its clustering membership, ensure 1-1 match
  intersect <- merge(sub_1_clust,sub_2_clust,by = c("id"))
  clust_1 <- as.integer(as.character(intersect$cluster.x))
  clust_2 <- as.integer(as.character(intersect$cluster.y))
  
  
  ##construct C_ij
  inter.dim <- dim(intersect)[1]
  C_1 <- matrix(clust_1, nr = inter.dim, nc = inter.dim) == matrix(clust_1, nr = inter.dim, nc = inter.dim, byrow = TRUE)
  C_2 <- matrix(clust_2, nr = inter.dim, nc = inter.dim) == matrix(clust_2, nr = inter.dim, nc = inter.dim, byrow = TRUE)
  diag(C_1) <- 0
  diag(C_2) <- 0
  
  ##compute similarity measure
  jaccard <- sum(C_1 * C_2)/(sum(C_1) + sum(C_2) - sum(C_1 * C_2))
  matching <- (sum(C_1 * C_2) + sum((1-C_1) * (1-C_2)))/(sum(C_1 * C_2) + sum((1-C_1) * (1-C_2)) + sum((1-C_1)*C_2) + sum((1-C_2)*C_1))
  corr <- sum(C_1 * C_2)/sqrt(sum(C_1)*sum(C_2))
#   print(jaccard)
#   print(matching)
#   print(corr)
  return(c(jaccard, matching, corr))
}


####EXAMPLE

setwd("/Users/chenglulu1127/Dlab/Stability/Stability-based-clustering")
airport <- read.csv("airports.csv",header=T)
set.seed(166)

##initialize
stable_result_jac <- matrix(0, nrow = 14, ncol = 100)
stable_result_mat <- matrix(0, nrow = 14, ncol = 100)
stable_result_cor <- matrix(0, nrow = 14, ncol = 100)


###Repeat 100 times for each scenario
for (k in 2:15){
  for (i in 1:100){
  stable_result_jac[k-1,i] <- stable_cluster(airport, k = k, m = 0.8)[1]
  stable_result_mat[k-1,i] <- stable_cluster(airport, k = k, m = 0.8)[2]
  stable_result_cor[k-1,i] <- stable_cluster(airport, k = k, m = 0.8)[3]
  }
}

jac <- data.frame(t(stable_result_jac))
mat <- data.frame(t(stable_result_mat))
cor <- data.frame(t(stable_result_cor))
