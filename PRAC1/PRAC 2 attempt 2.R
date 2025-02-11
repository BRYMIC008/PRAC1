set.seed(1)
x <- c(1:100)
y <- sin(x/10) + rnorm(1,0,0.2)
f <- 0.1
y_final <- rep(0,100)
  n <- length(x)
  k <- f*n
  weights <- matrix(0, nrow = k+1 ,ncol = n)
  
  for (i in 1:n){
    vector_distance = vector()
    w <- matrix(0,nrow = k+1,ncol = k+1)
    for (j in 1:n){
      vector_distance[j] <- (abs(x[j]-x[i]))
    }
    vector_indicies <- order(vector_distance)[1:(k+1)]
    vector_distance <- sort(vector_distance)
    neighbours <- vector_distance[1:(k+1)]
    for (l in 1:k+1){
      weights[l,i] <- (1-(neighbours[l]/max(neighbours)^3)^3)
    }
    w <- diag(weights[,i],nrow = k+1,ncol = k+1)
    x_neighbours <- as.matrix(x[vector_indicies])
    y_neighbours <- as.matrix(y[vector_indicies])
    xt_neighbours <- t(x_neighbours)
    b <- solve((xt_neighbours %*% w %*% x_neighbours))
    b <- b %*% xt_neighbours %*% w %*% y_neighbours
    y_final[i] <- b*x[i]

  }
plot(x,y_final)
