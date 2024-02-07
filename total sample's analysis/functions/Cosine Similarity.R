#Found at https://stats.stackexchange.com/questions/31565/compute-a-cosine-dissimilarity-matrix-in-r
#Accessed at 05.12.2023 12.37


#Takes in a data.frame and returns either a matrix of cosine similarities or dissimilarities.

cos_sim <- function(DF){
  Matrix <- as.matrix(DF)
  sim <- Matrix / sqrt(rowSums(Matrix * Matrix))
  sim <- sim %*% t(sim)
}


cos_dissim <- function(DF){
  Matrix <- as.matrix(DF)
  sim <- Matrix / sqrt(rowSums(Matrix * Matrix))
  sim <- sim %*% t(sim)
  as.dist(1 - sim)
}