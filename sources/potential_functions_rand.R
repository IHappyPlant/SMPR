require("plotrix")
dist <- function(u, v) sqrt(sum((u-v)^2)) # Евклидова метрика

kernel.G <- function(r) (2*pi)^(-0.5)*exp(-0.5*(r^2)) # Гауссовское ядро
kernel.E <- function(r) (3/4)*(1-r^2)*(abs(r) <= 1) # Ядро Епанечникова
kernel.Q <- function(r) (15/16)*((1 - r^2)^2)*(abs(r) <= 1) # Квартическое ядро
kernel.T <- function(r) (1 - abs(r))*(abs(r) <= 1) # Треугольное ядро
kernel.P <- function(r) (0.5)*(abs(r) <= 1) # Прямоугольное ядро

getSubsetByPotentials <- function(xl, potentials, res) {
  # Получить новый массив со значениями, соответствующими ненулевым значениям потенциалов
  if(!is.null(ncol(res))) return (res[as.numeric(rownames(xl[which(potentials != 0),])), ]) # Если не вектор
  return (res[as.numeric(rownames(xl[which(potentials != 0),]))]) # Если вектор
}

getDistances <- function(xl, z, metricFunction = dist) {
  # Посчитать расстояния от объекта z до каждого объекта выборки
  l <- nrow(xl)
  n <- ncol(xl)
  distances <- rep(0, l)
  for (i in 1:l) distances[i] <- metricFunction(xl[i, 1:(n-1)], z)
  return (distances)
}

getHVector <- function(xl) {
  # Задать ширину окна для каждого объекта выборки
  l <- nrow(xl)
  h <- rep(0, l)
  for(i in 1:l) {
    if (xl[i, ncol(xl)] == "setosa") h[i] <- 1
    else h[i] <- 0.4
  }
  return (h)
}

getPotentials <- function(xl, h, eps, kernelFunction = kernel.G) {
  # Получить потенциалы всех объектов выборки
  l <- nrow(xl)
  n <- ncol(xl)
  potentials <- rep(0, l)
  err <- eps + 1
  # Посчитаем расстояния от каждого объекта выборки до остальных
  distances <- matrix(0, l, l)
  for (i in 1:l)
    distances[i,] <- getDistances(xl, c(xl[i, 1], xl[i, 2]))
  # Пока число ошибок больше заданного
  while (err > eps) {
    while (TRUE) {
      # Пока не получим несоответствие классов, чтобы обновить потенциалы
      rand <- sample(1:l, 1)
      class <- pF(distances[rand,], potentials, h, xl, kernelFunction)
      if (class != xl[rand, n]) {
        potentials[rand] = potentials[rand] + 1
        break
      }
    }
    # Подсчет числа ошибок
    err <- 0
    for (i in 1:l) {
      class <- pF(distances[i,], potentials, h, xl, kernelFunction)
      err <- err + (class != xl[i, n])
    }
    print(err)
    print(potentials)
  }
  return (potentials)
}

pF <- function(distances, potentials, h, xl, kernelFunction = kernel.G) {
  l <- nrow(xl)
  n <- ncol(xl)
  classes <- xl[, n]
  weights <- table(classes) # Таблица для весов классов
  weights[1:length(weights)] <- 0 # По умолчанию все веса равны нулю
  for (i in 1:l) { # Для каждого объекта выборки
    class <- xl[i, n] # Берется его класс
    r <- distances[i] / h[i]
    weights[class] <- weights[class] + potentials[i] * kernelFunction(r) # Считается его вес, и прибавляется к общему ввесу его класса
  }
  if (max(weights) != 0) return (names(which.max(weights))) # Если есть веса больше нуля, то вернуть класс с наибольшим весом
  return ("") # Если точка не проклассифицировалась, то вернуть пустую строку
}

buildClassificationMap <- function(xl, h, potentials, kernelFunction = kernel.G) {
  # Проклассифицируем объекты на основе обучающей выборки, и запишем их в матрицу
  ox <- seq(0, 7, 0.1)
  oy <- seq(0, 2.5, 0.1)
  classifiedObjects <- matrix(NA, length(ox)*length(oy), ncol(xl))
  cnt <- 1
  for (i in ox)
    for (j in oy) {
      z <- c(i, j)
      distances <- getDistances(xl, z)
      class <- pF(distances, potentials, h, xl, kernelFunction)
      if (class != "") {
        classifiedObjects[cnt, ] <- c(z[1], z[2], class)
        cnt <- cnt + 1
      }
    }
  return (classifiedObjects)
}

drawPlots <- function(xl, classifiedObjects, potentials, h) {
  l <- nrow(xl)
  n <- ncol(xl)
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  # Полупрозрачные цвета для потенциалов
  redTrans <- col2rgb("red")
  redTrans <- rgb(redTrans[1], redTrans[2], redTrans[3], alpha = 255/5, max = 255)
  green3Trans <- col2rgb("green3")
  green3Trans <- rgb(green3Trans[1], green3Trans[2], green3Trans[3], alpha = 255/5, max = 255)
  blueTrans <- col2rgb("blue")
  blueTrans <- rgb(blueTrans[1], blueTrans[2], blueTrans[3], alpha = 255/5, max = 255)
  colorsTrans <- c("setosa" = redTrans, "versicolor" = green3Trans, "virginica" = blueTrans)
  par(mfrow=c(1,2))
  # Карта потенциалов
  plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], main = "Карта потенциалов", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  for (i in 1:l)
    if (potentials[i] != 0)
      draw.circle(xl[i, 1], xl[i, 2], radius = potentials[i], border = colorsTrans[xl[i, n]], col = colorsTrans[xl[i, n]])
  # Карта классификации
  plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], main = "Классификация ирисов Фишера методом потенциальных функций", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  points(classifiedObjects[, 1:(n-1)], pch = 22, col = colors[classifiedObjects[, n]])
}

main <- function(kernelFunction = kernel.G) {
  xl <- iris[, 3:5]
  h <- getHVector(xl)
  potentials <- getPotentials(xl, h, 5, kernelFunction)
  newXl <- getSubsetByPotentials(xl, potentials, xl) # новая выборка с теми элементами, у которых потенциалы ненулевые
  newH <- getSubsetByPotentials(xl, potentials, h) # Соответствующий вектор с ширинами окон этих элементов
  newPotentials <- getSubsetByPotentials(xl, potentials, potentials) # Соответствующие ненулевые потенциалы
  classifiedObjects <- buildClassificationMap(newXl, newH, newPotentials, kernelFunction)
  drawPlots(xl, classifiedObjects, potentials, h)
}

main(kernel.G)