dist = function(u, v) { # Евклидова метрика
  sqrt(sum((u - v)^2))
}

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
  orderedXL <- sortObj(xl, z);
  n <- dim(orderedXL)[2]
  classes <- orderedXL[1:k, n]  # Берём k ближайших соседей
  classes <- table(classes) # Делаем для них таблицу
  classes[1:length(classes)] <- 0 # Обнуляем все значения в таблице
  for (i in names(classes)) { # Для каждого класса
    for (j in 1:k) { # Проходим по всей таблице
      if (orderedXL[j, n] == i) # И суммируем веса всех объектов одинаковых классов
        classes[i] = classes[i] + (k - j + 1) / (q*q)
    }
  }
  class <- names(which.max(classes)) # Вернём самый большой вес
  return (class)
}

lOO <- function(k, q, xl) {
  sum = 0
  for (i in 1:dim(xl)[1]) { 
    tmpXl <- rbind(xl[1:i-1, ], xl[i+1:dim(xl)[1],]) # Временная выборка, с удалённым i-м объектом
    xi <- c(xl[i, 1], xl[i, 2]) # i-й объект, для которого будем запускать LOO
    class <- kwNN(tmpXl, xi, k, q)
    if (class != xl[i, 3]) # Если классы не совпали, увеличим сумму ошибки
      sum = sum + 1
  }
  sum = sum / dim(xl)[1]  # sum / l
  return (sum)
}


xl <- iris[, 3:5]

k <- 1
q <- 1
lOOForK <- matrix(NA, 1, 2) # Для графика зависимости LOO от k
lOOForK[1, ] <- c(k, lOO(k, q, xl))
tmp <- matrix(NA, 1, 2)
minErr <- 999999999

for (i in 2:150) { # Подбор по LOO оптимальных k среди чисел от 2 до 150
  for (j in 1:10) { # и q среди чисел от 1 до 10
    curErr <- lOO(i, j, xl) # Значение LOO на текущей итерации
    tmp[1, ] <- c(i, curErr) 
    lOOForK <- rbind(lOOForK, tmp)
    if (curErr < minErr) { # Если текущее значение LOO меньше минимального встреченного
      minErr <- curErr
      k <- i
      q <- j
    }
  }
}

colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], main="Классификация ирисов Фишера методом kwNN", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)

for (i in seq(0, 7, 0.1)) {
  for (j in seq(0, 2.5, 0.1)) {
    z <- c(i, j)
    class <- kwNN(xl, z, k, q)
    points(z[1], z[2], pch = 22, col = colors[class])
  }
}

# График LOO
plot(lOOForK, type = "l", bg = "red", col = "red", main = "Оценка оптимальности различных k по LOO", xlab = "Значения k", ylab = "Значения LOO")
label = paste("k = ", k, "\n", "LOO = ", round(minErr,3))
text(k, minErr, labels = label, pos = 3)