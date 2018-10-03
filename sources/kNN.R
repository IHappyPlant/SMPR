dist = function(u, v) {
  #Евклидова метрика
  sqrt(sum((u - v)^2))
}

sortObj <- function(xl, z, metricFunction = dist) {
  #Сортировка объектов по возрастанию расстояния до классифицируемого
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- rep(0, l)
  for (i in 1:l)
    distances[i] <- metricFunction(xl[i, 1:n], z)
  orderedXL <- xl[order(distances), ]
  return (orderedXL)
}

kNN <- function(xl, z, k) {
  orderedXL <- sortObj(xl, z);
  n <- dim(orderedXL)[2]
  classes <- orderedXL[1:k, n] 
  counts <- table(classes) # Таблица встречаемости каждого класса среди k ближайших соседей объекта
  class <- names(which.max(counts)) # Наиболее часто встречаемый класс
  return (class)
}

lOO <- function(k, xl) {
  # Метод скользящего контроля для подбора оптимального k
  sum = 0
  for (i in 1:dim(xl)[1]) { 
    tmpXl <- rbind(xl[1:i-1, ], xl[i+1:dim(xl)[1],]) # Удаление i-го объекта из выборки
    xi <- c(xl[i, 1], xl[i, 2])
    class <- kNN(tmpXl, xi, k)
    if (class != xl[i, 3])
      sum = sum + 1
  }
  sum = sum / dim(xl)[1] 
  return (sum)
}


xl <- iris[, 3:5]
minErr <- 1
k <- 1
lOOForK <- matrix(NA, 1, 2)
lOOForK[1, ] <- c(k, lOO(1, xl))
tmp <- matrix(NA, 1, 2)

for (i in 2:150) { # Выбор по LOO оптимального k среди последовательности от 2 до 60
  curErr <- lOO(i, xl)
  tmp[1, ] <- c(i, curErr)
  lOOForK <- rbind(lOOForK, tmp)
  if (curErr < minErr) {
    minErr = curErr
    k = i
  }
}

colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], main="Классификация ирисов Фишера методом kNN", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)

# Карта классификации
for (i in seq(0, 7, 0.1)) {
  for (j in seq(0, 2.5, 0.1)) {
    z <- c(i, j)
    class <- kNN(xl, z, k)
    points(z[1], z[2], pch = 22, col = colors[class])
  }
}

# График LOO
plot(lOOForK, type = "l", bg = "red", col = "red", main = "Оценка оптимальности различных k по LOO", xlab = "Значения k", ylab = "Значения LOO")
points(k, minErr, pch = 21, bg = "blue", col = "blue")
label = paste("k = ", k, "\n", "LOO = ", round(minErr, 3))
text(k, minErr, labels = label, pos = 3)
lines(lOOForK, col = "red")
