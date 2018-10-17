dist = function(u, v) { # Метрика
	sqrt(sum((u - v)^2))
}

sortObj <- function(xl, z, metricFunction = dist) { 
# Сортировка объектов выборки по расстоянию до классифицируемого
	l <- nrow(xl)
	n <- ncol(xl) - 1
	distances <- rep(0, l)
	for (i in 1:l)
		distances[i] <- c(metricFunction(xl[i, 1:n], z))
	orderedXL <- xl[order(distances), ]
	return (orderedXL)
}

oneNN <- function(xl, z) {
	orderedXL <- sortObj(xl, z);
	n <- ncol(orderedXL)
	class <- orderedXL[1, n]
	return (class)
}

buildIrisClassMap <- function(xl) {
  ox <- seq(0, 7, 0.1)
  oy <- seq(0, 2.5, 0.1)
  classifiedObjects <- matrix(NA, length(ox) * length(oy), ncol(xl))
  cnt <- 1
  for (i in ox)
    for (j in oy) {
      z <- c(i, j)
      class <- oneNN(xl, z)
      classifiedObjects[cnt, ] <- c(i, j, class)
      cnt <- cnt + 1
    }
  return (classifiedObjects)
}

drawPlot <- function(xl, classifiedObjects) {
  n <- ncol(classifiedObjects)
  l <- nrow(classifiedObjects)
  colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], main="Классификация ирисов Фишера методом 1NN", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  points(classifiedObjects[, 1:(n-1)], pch = 22, col = colors[classifiedObjects[, n]])
}

main <- function() {
  xl <- iris[, 3:5]
  classifiedObjects <- buildIrisClassMap(xl)
  drawPlot(xl, classifiedObjects)
}

main()