dist <- function(u, v) sqrt(sum((u-v)^2))

sort_obj <- function(xl, z, metricFunction = dist) {
  l <- nrow(xl)
  n <- ncol(xl)
  distances <- rep(0, l)
  for (i in 1:l) distances[i] <- metricFunction(xl[i, 1:(n-1)], z)
  orderedXL <- xl[order(distances), ]
  return (orderedXL)
}

kwNN <- function(xl, z, k, q) {
  orderedXl <- sort_obj(xl, z)
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

get_margin <- function(xl, z, k, q, target_class) {
  # Получить отступ объекта z для класса target_class
  orderedXl <- sort_obj(xl, z)
  n <- ncol(orderedXl)
  w <- 0
  for (i in 1:k) {
      class <- orderedXl[i, n]
      if (class == target_class) w <- w + q^i
      else w <- w - q^i
  }
  return (w)
}

get_margin_for_all <- function(xl, k, q) {
  # Для всех объектов выборки получить их отступы
  l <- nrow(xl)
  n <- ncol(xl)
  w <- data.frame(matrix(0, l, 2))
  colnames(w) <- c("w", "class")
  for (i in 1:l) {
    z <- xl[i, 1:(n-1)]
    target_class <- xl[i, n]
    w[i, ] <- c(get_margin(xl, z, k, q, target_class), toString(target_class))
  }
  return (w)
}

stolp <- function(xl, k, q, eps, eps_noise) {
  l <- nrow(xl)
  n <- ncol(xl)
  omega <- data.frame()
  w <- get_margin_for_all(xl, k, q)
  new_xl <- xl[which(w[1:l,1] > eps_noise),] # Удалить выбросы
  l <- nrow(new_xl) # Пересчитать длину выборки
  rownames(new_xl) <- c(1:l) # Перенумеровать выборку
  w <- get_margin_for_all(new_xl, k, q) # Пересчитать отступы
  # Взять по одному эталону из каждого класса
  for (i in unique(new_xl[, n])) {
      indexes <- as.numeric(rownames(w[which(new_xl[, n] == i),]))
      etalon <- new_xl[indexes[which.max(w[indexes,1])],]
      omega <- rbind(omega, etalon)
  }
  print(omega)
  l_omega <- nrow(omega)
  while (l_omega != l) {
    margins <- rep(0, l)
    margins_indexes <- c(1:l)
    errors <- 0
    for (i in 1:l) {
      z <- new_xl[i, 1:(n-1)]
      if (k > l_omega) tmp_k <- l_omega
      else tmp_k <- k
      margins[i] <- c(get_margin(omega, z, tmp_k, q, new_xl[i, n]))
      errors <- errors + (margins[i] < 0)
    }
    if (errors <= eps) break
    #print(margins)
    #print(new_xl[margins_indexes[which.min(margins[margins_indexes])],])
    omega <- rbind(omega, new_xl[margins_indexes[which.min(margins[margins_indexes])],])
    l_omega <- nrow(omega)
  }
  return (omega)
}

build_classification_map <- function(xl, k, q) {
  l <- nrow(xl)
  n <- ncol(xl)
  ox <- seq(0, 7, 0.1)
  oy <- seq(0, 2.5, 0.1)
  classified_objects <- matrix(NA, length(ox)*length(oy), n)
  cnt <- 1
  for (i in ox)
    for (j in oy) {
      z <- c(i, j)
      class <- kwNN(xl, z, k, q)
      classified_objects[cnt, ] <- c(i, j, class)
      cnt <- cnt + 1
    }
  return (classified_objects)
}

draw_plots <- function(xl, omega, classified_objects, k, q) {
  n <- ncol(xl)
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  par(mfrow=c(1,2))
  plot(xl[, 1:(n-1)], pch = 21, col = colors[xl[,n]], main = "Эталонные объекты", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  points(omega[, 1:(n-1)], pch = 21, bg = colors[omega[,n]], col = "black")
  plot(omega[, 1:(n-1)], pch = 21, bg = colors[omega[,n]], col = colors[omega[,n]], main = "Классификация ирисов Фишера методом kwNN", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  points(classified_objects[, 1:(n-1)], pch = 22, col = colors[classified_objects[,n]])
}

#main <- function() {
  xl <- iris[, 3:5]
  k <- 10
  q <- 0.1
  omega <- stolp(xl, k, q, 3, -2)
  omega <- omega[order(as.numeric(rownames(omega))),]
  classified_objects <- build_classification_map(omega, k, q)
  draw_plots(xl, omega, classified_objects, k, q)
#}

#main()