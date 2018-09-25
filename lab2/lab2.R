dist = function(u, v) { # Метрика
  sqrt(sum((u - v)^2))
}

sortObj <- function(xl, z, metricFunction = dist) { # Сортировка объектов выборки по расстоянию до классифицируемого
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- rep(0, l)
  for (i in 1:l) {
    distances[i] <- c(metricFunction(xl[i, 1:n], z))
  }
  orderedXL <- xl[order(distances), ]
  return (orderedXL)
}

kNN <- function(xl, z, k) {
  orderedXL <- sortObj(xl, z);
  n <- dim(orderedXL)[2]
  classes <- orderedXL[1:k, n] 
  counts <- table(classes) # Таблица встречаемости каждого класса среди k ближайших соседей
  class <- names(which.max(counts)) # Наиболее часто встречаемый класс среди k ближайших соседей
  return (class)
}

lOO <- function(k, xl) { # Метод скользящего контроля для выбора оптимального k
  #Первую и последнюю итерацию проведём вне цикла, чтобы метод не сыпался на крайних k
  tmpXl <- xl[i+1:dim(xl)[1], ] # Удаляем объект из выборки
  xi <- c(xl[1, 1], xl[1, 2]) 
  class <- kNN(tmpXl, xi, k) # Классифицируем его методом kNN
  if (class == xl[1, 3]) # Класс совпал с реальным
    sum = 0
  else # Классы не совпали
    sum = 1
  
  for (i in 1:(dim(xl)[1] - 1)) { 
    tmpXl <- rbind(xl[1:i-1, ], xl[i+1:dim(xl)[1],])
    xi <- c(xl[i, 1], xl[i, 2])
    class <- kNN(tmpXl, xi, k)
    if (class != xl[i, 3])
      sum = sum + 1
  }
  
  tmpXl <- xl[1:(dim(xl)[1] - 1), ]
  xi <- c(xl[dim(xl)[1], 1], xl[dim(xl)[1], 2])
  class <- kNN(tmpXl, xi, k)
  if (class != xl[dim(xl)[1], 3])
    sum = sum + 1
  
  sum = sum / dim(xl)[1] # Усреднение полученной ошибки
  return (sum)
}


xl <- iris[, 3:5]
colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], main="Классификация ирисов Фишера методом kNN", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)

minErr = 99999
curErr = 0
k = 1
for (i in 1:10) { # Выбор оптимального k среди последовательности от 1 до 10
  curErr <- lOO(i, xl)
  if (curErr < minErr) {
    minErr = curErr
    k = i
  }
}

for (i in seq(0, 7, 0.1)) {
  for (j in seq(0, 2.5, 0.1)) {
    z <- c(i, j)
    class <- kNN(xl, z, k)
    points(z[1], z[2], pch = 22, col = colors[class])
  }
}