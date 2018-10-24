dist <- function(u, v) sqrt(sum(u-v)^2)

kernel.G <- function(r) (2*pi)^(-0.5)*exp(-0.5*(r^2)) # Гауссовское ядро
kernel.E <- function(r) (3/4)*((1-r^2)^2)*(abs(r) <= 1) # Ядро Епанечникова
kernel.Q <- function(r) (15/16)*((1 - r^2)^2)*(abs(r) <= 1) # Квартическое ядро
kernel.T <- function(r) (1 - abs(r))*(abs(r) <= 1) # Треугольное ядро
kernel.P <- function(r) (0.5)*(abs(r) <= 1) # Прямоугольное ядро

getDistances <- function(xl, z, metricFunction = dist) {
  # Получить вектор расстояний от объекта z до каждого объекта выборки
  l <- nrow(xl)
  n <- ncol(xl)
  distances <- rep(0, l)
  for (i in 1:l)
    distances[i] <- metricFunction(xl[i, 1:(n-1)], z)
  return (distances)
}

parzen <- function(xl, h, distances, kernelFunction = kernel.G) {
  # xl - выборка
  # h - ширина окна
  # distances - расстояния от объекта z до каждого объекта из xl 
  # Расстояния считаются заранее, поэтому сам объект z здесь не нужен
  # kernelFunction - используемая функция ядра. По умолчанию Гауссовское
  l <- nrow(xl)
  n <- ncol(xl)
  classes <- xl[1:l, n] # Классы объектов выборки
  weights <- table(classes) # Таблица для весов классов
  weights[1:length(weights)] <- 0
  for (i in 1:l) { # Для каждого объекта выборки
    class <- xl[i, n] # Берём его класс
    r <- distances[i] / h
    weights[class] <- weights[class] + kernelFunction(r) # И прибавляем его вес к общему весу его класса
  }
  if (max(weights) == 0) # Если точка не попала в окно (не проклассифицировалась)
    return (0) # То вернуть 0
  class <- names(which.max(weights))
  return (class) # Иначе вернуть класс с максимальным весом
}


lOO <- function(xl, kernelFunction = kernel.G) {
  l <- nrow(xl)
  n <- ncol(xl)
  #hvalues <- seq(0.1, 2, 0.1)
  hvalues <- seq(1, 150, 1) # чисто поорать
  sum <- rep(0, length(hvalues))
  for (i in 1:l) {
    cnt <- 1
    xi <- xl[i, 1:(n-1)]
    xl1 <- xl[-i, ]
    print(i)
    distances <- getDistances(xl1, xi)
    for (h in hvalues) {
      class <- parzen(xl1, h, distances, kernelFunction)
      if (class != xl[i, n] || class == 0) {
        sum[cnt] = sum[cnt] + 1/l
      }
      cnt <- cnt + 1
    }
  }
  return (sum)
}

#getOptimalH <- function(looForH) which.min(looForH) / 10
getOptimalH <- function(looForH) which.min(looForH)

buildClassMap <- function(xl, h, kernelFunction = kernel.G) {
  l <- nrow(xl)
  n <- ncol(xl)
  ox <- seq(0, 7, 0.1)
  oy <- seq(0, 2.5, 0.1)
  classifiedObjects <- matrix(NA, length(ox)*length(oy), n)
  cnt <- 1
  for (i in ox)
    for (j in oy) {
      z <- c(i, j)
      distances <- getDistances(xl, z)
      class <- parzen(xl, h, distances, kernelFunction)
      if (class != 0) {
        classifiedObjects[cnt, ] <- c(i, j, class)
        cnt <- cnt + 1
      }
    }
  return (classifiedObjects)
}

buildPlots <- function(xl, classifiedObjects, looForH, h) {
  l <- nrow(classifiedObjects)
  n <- ncol(classifiedObjects)
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  h10 <- h*10
  par(mfrow=c(1, 2))
  # Карта классификации
  plot(xl[,1:(n-1)], pch = 21, bg = colors[xl[, n]], col = colors[xl[, n]], main = "Классификация ирисов Фишера методом парзеновского окна", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  points(classifiedObjects[, 1:(n-1)], pch = 22, col = colors[classifiedObjects[, n]])
  # График lOO
  #plot(seq(0.1, 2, 0.1), looForH[1:length(looForH)], type = "l", bg = "red", col = "red", main = "Оценка оптимальности различных h по LOO", xlab = "Значения h", ylab = "Значения LOO")
  plot(seq(1, 150, 1), looForH[1:length(looForH)], type = "l", bg = "red", col = "red", main = "Оценка оптимальности различных h по LOO", xlab = "Значения h", ylab = "Значения LOO")
  #points(h, looForH[h10], pch = 21, bg = "blue", col = "blue")
  points(h, looForH[h], pch = 21, bg = "blue", col = "blue")
  #label <- paste("h = ", h, "\n", "LOO = ", round(looForH[h10], 3))
  label <- paste("h = ", h, "\n", "LOO = ", round(looForH[h], 3))
  #text(h, looForH[h10], labels = label, pos = 3)
  text(h, looForH[h], labels = label, pos = 3)
}

main <- function(kernelFunction = kernel.G) {
  xl <- iris[, 3:5]
  lOOForH <- lOO(xl, kernelFunction)
  h <- getOptimalH(lOOForH)
  classifiedObjects <- buildClassMap(xl, h, kernelFunction)
  buildPlots(xl, classifiedObjects, lOOForH, h)
}

main(kernel.G)