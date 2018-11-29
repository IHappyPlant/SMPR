distance <- function(u, v) sqrt(sum((u - v)^2))
get_distances <- function(xl, z) apply(xl[,1:(ncol(xl)-1)], 1, distance, z)
sort_objects_by_dist <- function(xl, z) xl[order(get_distances(xl, z)),]

kNN <- function(xl, z, k) names(which.max(table(sort_objects_by_dist(xl, z)[1:k, ncol(xl)])))

w.kwnn <- function(i, k, q) (i <= k) * q^i
kwNN <- function(xl, z, k, q) {
  ordered_xl <- sort_objects_by_dist(xl, z)
  weights <- w.kwnn(1:nrow(ordered_xl), k, q)
  names(weights) <- ordered_xl[, ncol(ordered_xl)]
  sum_by_class <- sapply(unique(sort(names(weights))), function(class, weights) sum(weights[names(weights) == class]), weights)
  names(which.max(sum_by_class))
}

classifyZ <- function(xl, k, q) { 
  z <- c(5, 5)
  classKNN <- kNN(xl, z, k)
  classKWNN <- kwNN(xl, z, k, q)
  classifiedZ <- rbind(c(z, classKNN), c(z, classKWNN))
  return (classifiedZ)
}

drawPlot <- function(xl, k, q, classifiedObjects) {
  n <- ncol(classifiedObjects)
  colors = c("plus" = "red", "minus" = "blue")
  q10 = q * 10
  par(mfrow=c(1, 2))
  plot(xl[, 1:2], pch = 21, bg = colors[xl$Class], col = colors[xl$Class], main="Классификация методом kNN", xlab = "X", ylab = "Y", asp = 1)
  points(classifiedObjects[1, 1], classifiedObjects[1, 2], pch = 22, col = colors[classifiedObjects[1, n]])
  
  plot(xl[, 1:2], pch = 21, bg = colors[xl$Class], col = colors[xl$Class], main="Классификация методом kwNN", xlab = "X", ylab = "Y", asp = 1)
  points(classifiedObjects[2, 1], classifiedObjects[2, 2], pch = 22, col = colors[classifiedObjects[2, n]])
}

setModelData <- function() {
  xl <- data.frame(matrix(NA, 6, 3))
  colnames(xl) <- c("X", "Y", "Class")
  xl[1, 1:2] <- c(1, 1)
  xl[2, 1:2] <- c(4, 5)
  xl[3, 1:2] <- c(5, 4)
  xl[4, 1:2] <- c(5, 6)
  xl[5, 1:2] <- c(7, 6)
  xl[6, 1:2] <- c(8, 5)
  xl[1:3, 3] <- "plus"
  xl[4:6, 3] <- "minus"
  return(xl)
}

main <- function() {
  xl <- setModelData()
  k <- 5
  q <- 0.1
  classifiedObjects <- classifyZ(xl, k, q)
  drawPlot(xl, k, q, classifiedObjects)
}

main()
