# test_dtwSatPatterns.R

#library(testthat)
#source("/home/alber/Documents/ghProjects/dtwSatPatterns/R/util.R")




n <- 3
up <- 0
down <- 1
test <- matrix(0, ncol = n, nrow = n)
for(i in 1:ncol(test)){
  for(j in 1:nrow(test)){
    test[i, j] <- up
    if(i > j){test[i, j] <- down}
  }
}
res <- .triangularMatrix(n, up, down)
expect_equal(test, res)
