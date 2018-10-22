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

kNN <- function(xl, z, k) {
  orderedXl <- sortObj(xl, z)
  n <- ncol(orderedXl)
  classes <- orderedXl[1:k, n] 
  counts <- table(classes) # Таблица встречаемости каждого класса среди k ближайших соседей объекта
  class <- names(which.max(counts)) # Наиболее часто встречаемый класс
  return (class)
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
