distance <- function(u, v) sqrt(sum((u - v)^2))
get_distances <- function(xl, z) apply(xl[,1:(ncol(xl)-1)], 1, distance, z)
sort_objects_by_dist <- function(xl, z) xl[order(get_distances(xl, z)),]

kNN <- function(xl, z, k) names(which.max(table(sort_objects_by_dist(xl, z)[1:k, ncol(xl)])))

kNN_OnSortedXl <- function(orderedXl, k) names(which.max(table(orderedXl[1:k, ncol(orderedXl)])))
  

lOO <- function(xl) { # Метод скользящего контроля для подбора оптимального k
  l <- nrow(xl)
  n <- ncol(xl)
  lOOForK <- rep.int(0, l-1)
  for (i in 1:l) {
    print(i)
    orderedXl <- sort_objects_by_dist(xl[-i, ], xl[i, 1:(n-1)]) # Выборка без i-го объекта, отсортированная относительно него
    for (k in 1:(l-1)) lOOForK[k] <- lOOForK[k] + (kNN_OnSortedXl(orderedXl, k) != xl[i, n]) / l
  }
  lOOForK # Вектор зависимости LOO от k
}

getOptimalK <- function(lOOForK) which.min(lOOForK)

buildIrisClassMap <- function(xl, k) { 
  # Классифицируем объекты на основе выборки ирисов Фишера, и запишем их в матрицу
  classifiedObjects <- c()
  for (i in seq(0, 7, 0.1))
    for (j in seq(0, 2.5, 0.1))
      classifiedObjects <- rbind(classifiedObjects, c(i, j, kNN(xl, c(i, j), k)))
  classifiedObjects
}

drawPlots <- function(xl, k, lOOForK, classifiedObjects) {
  n <- ncol(classifiedObjects)
  colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  par(mfrow=c(1, 2))
  # Карта классификации
  plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], xlim = c(0, 7), main="Классификация ирисов Фишера методом kNN", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  points(classifiedObjects[, 1:(n-1)], pch = 22, col = colors[classifiedObjects[, n]])
  # График LOO
  plot(lOOForK, type = "l", bg = "red", col = "red", main = "Оценка оптимальности различных k по LOO", xlab = "Значения k", ylab = "Значения LOO")
  points(k, lOOForK[which.min(lOOForK)], pch = 21, bg = "blue", col = "blue")
  label = paste("k = ", k, "\n", "LOO = ", round(lOOForK[which.min(lOOForK)], 3))
  text(k, lOOForK[which.min(lOOForK)], labels = label, pos = 3, xpd = T)
}

main <- function() {
  xl <- iris[, 3:5]
  lOOForK <- lOO(xl)
  k <- getOptimalK(lOOForK)
  classifiedObjects <- buildIrisClassMap(xl, k)
  drawPlots(xl, k, lOOForK, classifiedObjects)
}

main()
