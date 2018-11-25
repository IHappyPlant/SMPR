dist = function(u, v) sqrt(sum((u - v)^2)) # Евклидова метрика

sortObj <- function(xl, z, metricFunction = dist) {
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- rep(0, l)
  for (i in 1:l)
    distances[i] <- metricFunction(xl[i, 1:n], z)
  orderedXL <- xl[order(distances), ]
  return (orderedXL)
}

kwNN_onSortedXl <- function(orderedXl, k, q) {
  n <- ncol(orderedXl)
  classes <- orderedXl[1:k, n]        # Берём k ближайших соседей
  classes <- table(classes)           # Делаем для них таблицу
  classes[1:length(classes)] <- 0     # Обнуляем все значения в таблице
  for (i in names(classes))           # Для каждого класса
    for (j in 1:k)                    # Проходим по k ближайшим соседям
      if (orderedXl[j, n] == i)       # И суммируем веса всех объектов одинаковых классов
        classes[i] = classes[i] + q^j
  class <- names(which.max(classes))  # Вернём класс с самым большим весом
  return (class)
}

kwNN <- function(xl, z, k, q) {
  orderedXl <- sortObj(xl, z)
  n <- ncol(orderedXl)
  classes <- orderedXl[1:k, n]        # Берём k ближайших соседей
  classes <- table(classes)           # Делаем для них таблицу
  classes[1:length(classes)] <- 0     # Обнуляем все значения в таблице
  for (i in names(classes))           # Для каждого класса
    for (j in 1:k)                    # Проходим по k ближайшим соседям
      if (orderedXl[j, n] == i)       # И суммируем веса всех объектов одинаковых классов
        classes[i] = classes[i] + q^j
  class <- names(which.max(classes))  # Вернём класс с самым большим весом
  return (class)
}

lOO <- function(xl) { 
  # Метод скользящего контроля для подбора оптимального k
  l <- nrow(xl)
  n <- ncol(xl)
  qRange <- seq(0.1, 1, 0.1)
  lOOForK <- matrix(0, l-1, length(qRange))
  for (i in 1:l) {
    print(i)
    xi <- xl[i, 1:(n-1)]                # i-й объект выборки
    orderedXL <- sortObj(xl[-i, ], xi)  # Выборка без i-го объект
    for (k in 1:(l-1)) {
      q_cnt <- 1
      for (q in qRange) {
        lOOForK[k, q_cnt] <- lOOForK[k, q_cnt] + (kwNN_onSortedXl(orderedXL, k, q) != xl[i, n]) / l
        q_cnt <- q_cnt + 1
      }
    }
  }
  return (lOOForK) # Матрица зависимости LOO от k и q
}

getOptimalK <- function(lOOForK) {
  optimalIndex <- 1
  optimalVal <- lOOForK[1, 1]
  for (i in 1:ncol(lOOForK)) {
    minIndex <- which.min(lOOForK[, i])
    minVal <- lOOForK[minIndex, i]
    #print(paste("i = ", i, "; tmp = ", minIndex))
    if (optimalVal > minVal) {
      optimalIndex <- minIndex
      optimalVal <- minVal
    }
  }
  return (optimalIndex)
}

getOptimalQ <- function(k, lOOForK) {
  optimalVal <- lOOForK[k, 1]
  optimalIndex <- 1
  for (i in 2:ncol(lOOForK)) {
    minValue <- lOOForK[k, i]
    if (optimalVal > minValue) {
      optimalVal <- minValue
      optimalIndex <- i
    }
  }
  optimalIndex <- optimalIndex
  return (optimalIndex / 10)
}

getIrisClassMap <- function(xl, k, q) { 
  # Построим карту классификации на основе ирисов Фишера, и запишем её в матрицу
  n <- ncol(xl)
  ox <- seq(0, 7, 0.1)
  oy <- seq(0, 2.5, 0.1)
  classifiedObjects <- matrix(NA, length(ox)*length(oy), n)
  cnt <- 1
  for (i in ox) {
    for (j in oy) {
      z <- c(i, j)
      class <- kwNN(xl, z, k, q)
      classifiedObjects[cnt, ] <- c(i, j, class)
      cnt <- cnt + 1
    }
  }
  return (classifiedObjects)
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
  #k <- getOptimalK(lOOForK)
  #q <- getOptimalQ(k, lOOForK)
  #classifiedObjects <- getIrisClassMap(xl, k, q)
  #drawPlots(k, q, lOOForK, classifiedObjects)
#}

#main()