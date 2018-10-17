dist = function(u, v) sqrt(sum((u - v)^2)) # Евклидова метрика

sortObj <- function(xl, z, metricFunction = dist) { # Сортировка объектов по возрастанию расстояния до классифицируемого
  l <- nrow(xl)
  n <- ncol(xl) - 1
  distances <- rep(0, l)
  for (i in 1:l)
    distances[i] <- metricFunction(xl[i, 1:n], z)
  orderedXL <- xl[order(distances), ]
  return (orderedXL)
}

kNN <- function(xl, z, k) {
  orderedXl <- sortObj(xl, z)
  n <- ncol(orderedXl)
  classes <- orderedXl[1:k, n] 
  counts <- table(classes) # Таблица встречаемости каждого класса среди k ближайших соседей объекта
  class <- names(which.max(counts)) # Наиболее часто встречаемый класс
  return (class)
}

kNN_OnSortedXl <- function(orderedXl, k) {
  n <- ncol(orderedXl)
  classes <- orderedXl[1:k, n] 
  counts <- table(classes) # Таблица встречаемости каждого класса среди k ближайших соседей объекта
  class <- names(which.max(counts)) # Наиболее часто встречаемый класс
  return (class)
}

lOO <- function(xl) { # Метод скользящего контроля для подбора оптимального k
  l <- nrow(xl)
  n <- ncol(xl)
  lOOForK <- rep.int(0, l)
  for (i in 1:l) {
    xi <- xl[i, 1:(n-1)] # i-й объект выборки
    orderedXl <- sortObj(xl[-i, ], xi) # Выборка без i-го объекта
    for (k in 1:l) {
      class <- kNN_OnSortedXl(orderedXl, k)
      if (class != xl[i, n])
        lOOForK[k] <- lOOForK[k] + 1 / l
    }
  }
  return (lOOForK) # Вектор зависимости LOO от k
}

getOptimalK <- function(lOOForK) which.min(lOOForK)

buildIrisClassMap <- function(xl, k) { 
  # Классифицируем объекты на основе выборки ирисов Фишера, и запишем их в матрицу
  n <- ncol(xl)
  ox <- seq(0, 7, 0.1)
  oy <- seq(0, 2.5, 0.1)
  classifiedObjects <- matrix(NA, length(ox)*length(oy), n)
  cnt <- 1
  for (i in ox)
    for (j in oy) {
      z <- c(i, j)
      class <- kNN(xl, z, k)
      classifiedObjects[cnt, ] <- c(i, j, class)
      cnt <- cnt + 1
    }
  return (classifiedObjects)
}

drawPlots <- function(k, lOOForK, classifiedObjects) {
  l <- nrow(classifiedObjects)
  n <- ncol(classifiedObjects)
  colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  par(mfrow=c(1, 2))
  # Карта классификации
  plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], main="Классификация ирисов Фишера методом kNN", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  points(classifiedObjects[, 1:(n-1)], pch = 22, col = colors[classifiedObjects[, n]])
  # График LOO
  plot(lOOForK, type = "l", bg = "red", col = "red", main = "Оценка оптимальности различных k по LOO", xlab = "Значения k", ylab = "Значения LOO")
  points(k, lOOForK[which.min(lOOForK)], pch = 21, bg = "blue", col = "blue")
  label = paste("k = ", k, "\n", "LOO = ", round(lOOForK[which.min(lOOForK)], 3))
  text(k, lOOForK[which.min(lOOForK)], labels = label, pos = 3)
}

main <- function() {
  xl <- iris[, 3:5]
  lOOForK <- lOO(xl)
  k <- getOptimalK(lOOForK)
  classifiedObjects <- buildIrisClassMap(xl, k)
  drawPlots(k, lOOForK, classifiedObjects)
}

main()
