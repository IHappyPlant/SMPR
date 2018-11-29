distance <- function(u, v) sqrt(sum((u - v)^2))
get_distances <- function(xl, z) apply(xl[,1:(ncol(xl)-1)], 1, distance, z)
sort_objects_by_dist <- function(xl, z) xl[order(get_distances(xl, z)),]

w.kwnn <- function(i, k, q) (i <= k) * q^i
kwNN_onSortedXl <- function(ordered_xl, k, q) {
  weights <- w.kwnn(1:nrow(ordered_xl), k, q)
  names(weights) <- ordered_xl[, ncol(ordered_xl)]
  sum_by_class <- sapply(unique(sort(names(weights))), function(class, weights) sum(weights[names(weights) == class]), weights)
  names(which.max(sum_by_class))
}

kwNN <- function(xl, z, k, q) {
  ordered_xl <- sort_objects_by_dist(xl, z)
  weights <- w.kwnn(1:nrow(ordered_xl), k, q)
  names(weights) <- ordered_xl[, ncol(ordered_xl)]
  sum_by_class <- sapply(unique(sort(names(weights))), function(class, weights) sum(weights[names(weights) == class]), weights)
  names(which.max(sum_by_class))
}

lOO <- function(xl) { 
  # Метод скользящего контроля для подбора оптимального k
  l <- nrow(xl)
  n <- ncol(xl)
  qRange <- seq(0.1, 1, 0.1)
  lOOForK <- matrix(0, l-1, length(qRange))
  for (i in 1:l) {
    orderedXL <- sort_objects_by_dist(xl[-i, ], xl[i, 1:(n-1)])  # Выборка без i-го объекта, отсортированная относительно него
    for (k in 1:(l-1)) lOOForK[k,] <- lOOForK[k,] + sapply(qRange, function(q) (kwNN_onSortedXl(orderedXL, k, q) != xl[i, n]) / l)
  }
  return (lOOForK) # Матрица зависимости LOO от k и q
}

getOptimalPar <- function(lOOForK) arrayInd(which.min(lOOForK), dim(lOOForK)) # Получить оптимальные k и q

getIrisClassMap <- function(xl, k, q) { 
  # Построим карту классификации на основе ирисов Фишера, и запишем её в матрицу
  classifiedObjects <- c()
  for (i in seq(0, 7, 0.1))
    for (j in seq(0, 2.5, 0.1)) 
      classifiedObjects <- rbind(classifiedObjects, c(i, j, kwNN(xl, c(i, j), k, q)))
  classifiedObjects
}

drawPlots <- function(k, q, lOOForK, classifiedObjects) {
  l <- nrow(classifiedObjects)
  n <- ncol(classifiedObjects)
  colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  q10 = q * 10
  par(mfrow=c(1, 2))
  # Карта классификации
  plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], main="Классификация ирисов Фишера методом kwNN", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  points(classifiedObjects[, 1:(n-1)], pch = 22, col = colors[classifiedObjects[, n]])
  # График LOO
  plot(lOOForK[1:nrow(lOOForK), q10], type = "l", bg = "red", col = "red", main = "Оценка оптимальности различных k по LOO", xlab = "Значения k", ylab = "Значения LOO")
  points(k, lOOForK[k, q10], pch = 21, bg = "blue", col = "blue")
  label = paste("k = ", k, "\n", "LOO = ", round(lOOForK[k, q10], 3))
  text(k, lOOForK[k, q10], labels = label, pos = 3)
  lines(lOOForK, col = "red")  
}

#main <- function() {
  xl <- iris[, 3:5]
  lOOForK <- lOO(xl)
  opt_par <- getOptimalPar(lOOForK)
  k <- opt_par[1]
  q <- opt_par[2] / 10
  classifiedObjects <- getIrisClassMap(xl, k, q)
  drawPlots(k, q, lOOForK, classifiedObjects)
#}

#main()
