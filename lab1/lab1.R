dist = function(u, v) { #Метрика
	sqrt(sum((u - v)^2))
}

sortObj <- function(xl, z, metricFunction = dist) { 
#Сортировка объектов выборки по расстоянию до классифицируемого
	l <- dim(xl)[1]
	n <- dim(xl)[2] - 1
	distances <- rep(0, l)
	for (i in 1:l) {
		distances[i] <- c(metricFunction(xl[i, 1:n], z))
  }
	orderedXL <- xl[order(distances), ]
	return (orderedXL)
}

oneNN <- function(xl, z) #1NN
{
	orderedXL <- sortObj(xl, z);
	n <- dim(orderedXL)[2]
	class <- orderedXL[1, n]
	return (class)
}

xl <- iris[, 3:5]
colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], main=" Классификаци¤ ирисов Фишера методом 1NN", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)


for (i in seq(0, 7, 0.1)) {
  for (j in seq(0, 2.5, 0.1)) {
    z <- c(i, j)
    class <- oneNN(xl, z)
    points(z[1], z[2], pch = 22, col = colors[class])
  }
}