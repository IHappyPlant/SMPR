dist <- function(u, v) sqrt(sum((u-v)^2))

get_distances <- function(xl, z, metric_function = dist) apply(xl[,1:(ncol(xl)-1)], 1, metric_function, z)

sort_obj <- function(xl, z, metric_function = dist) {
  distances <- get_distances(xl, z)
  return (xl[order(distances),])
}

kNN <- function(orderedXl, k) {
  n <- ncol(orderedXl)
  classes <- orderedXl[1:k, n] 
  counts <- table(classes) # Таблица встречаемости каждого класса среди k ближайших соседей объекта
  class <- names(which.max(counts)) # Наиболее часто встречаемый класс
  return (class)
}

get_margin <- function(xl, z, k, target_class) {
  # Получить отступ объекта z для класса target_class
  orderedXl <- sort_obj(xl, z)
  n <- ncol(orderedXl)
  classes <- orderedXl[1:k, n]
  weights <- table(classes)
  margin <- weights[target_class] - sum(weights[which(names(weights) != target_class)])
  return (margin)
}

get_margin_for_all <- function(xl, k) {
  l <- nrow(xl)
  n <- ncol(xl)
  margins <- rep(0, l)
  for (i in 1:l) {
    xi <- xl[i , 1:(n-1)]
    target_class <- xl[i, n]
    margins[i] <- get_margin(xl[-i,], xi, k, target_class)
  }
  return (margins)
}

lOO <- function(xl) { # Метод скользящего контроля для подбора оптимального k
  l <- nrow(xl)
  n <- ncol(xl)
  lOOForK <- rep.int(0, l)
  for (i in 1:l) {
    xi <- xl[i, 1:(n-1)] # i-й объект выборки
    orderedXl <- sort_obj(xl[-i, ], xi) # Выборка без i-го объекта
    for (k in 1:l) {
      class <- kNN(orderedXl, k)
      if (class != xl[i, n])
        lOOForK[k] <- lOOForK[k] + 1 / l
    }
  }
  return (lOOForK) # Вектор зависимости LOO от k
}

loo_mod <- function(xl, omega) {
  l <- nrow(xl)
  n <- ncol(xl)
  l_omega <- nrow(omega)
  loo <- rep(0, l_omega)
  for (i in 1:l) {
    xi <- xl[i, 1:(n-1)]
    ordered_omega <- sort_obj(omega, xi)
    for (k in 1:l_omega) {
      class <- kNN(ordered_omega, k)
      loo[k] <- loo[k] + (class != xl[i, n]) / l
    }
  }
  return (loo)
}

get_optimal_k <- function(loo) which.min(loo)

stolp <- function(xl, errors_lim, noises_lim) {
  l <- nrow(xl)
  n <- ncol(xl)
  k <- 6
  margins <- get_margin_for_all(xl, k)
  new_xl <- xl[which(margins > noises_lim), ]
  new_l <- nrow(new_xl)
  margins <- get_margin_for_all(new_xl, k)
  margins <- margins[as.numeric(rownames(new_xl))]
  names(margins) <- rownames(new_xl)
  omega <- data.frame()
  for (i in unique(new_xl[, n])) {
    indexes_for_class <- rownames(new_xl[which(new_xl[, n] == i),])
    etalon <- new_xl[names(which.max(margins[indexes_for_class])),]
    omega <- rbind(omega, etalon)
  }
  l_omega <- nrow(omega)
  # До сюда пока всё правильно
  while (l_omega != l) {
    margins <- rep(0, l)
    loo <- loo_mod(xl, omega)
    k <- get_optimal_k(loo) # Оптимальное k для omega
    errors <- data.frame()
    for (i in 1:l) { # Посчитать количество ошибок на выборке
      xi <- xl[i, 1:(n-1)]
      margins[i] <- get_margin(omega, xi, k, xl[i, n])
      if (margins[i] < 0) errors <- rbind(errors, xl[i,])
    }
    
    if(nrow(errors) <= errors_lim) break
    
    names(margins) <- rownames(xl)
    omega <- rbind(omega, errors[names(which.min(margins)),])
    l_omega <- nrow(omega)
    print(omega)
  }
  return (omega)
}

build_classification_map <- function(xl, k) {
  l <- nrow(xl)
  n <- ncol(xl)
  ox <- seq(0, 7, 0.1)
  oy <- seq(0, 2.5, 0.1)
  classified_objects <- matrix(NA, length(ox) * length(oy), n)
  cnt <- 1
  for (i in ox)
    for (j in oy) {
      z <- c(i, j)
      ordered_xl <- sort_obj(xl, z)
      class <- kNN(ordered_xl, k)
      classified_objects[cnt, ] <- c(z, class)
      cnt <- cnt + 1
    }
  return (classified_objects)
}

draw_plots <- function(xl, omega, classified_objects) {
  n <- ncol(xl)
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  par(mfrow=c(1,2))
  plot(xl[, 1:(n-1)], pch = 21, col = colors[xl[,n]], main = "Эталонные объекты", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  points(omega[, 1:(n-1)], pch = 21, bg = colors[omega[,n]], col = "black")
  plot(omega[, 1:(n-1)], pch = 21, bg = colors[omega[,n]], col = colors[omega[,n]], xlim = c(0, 7), main = "Классификация ирисов Фишера методом kNN", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  points(classified_objects[, 1:(n-1)], pch = 22, col = colors[classified_objects[,n]])
}

xl <- iris[, 3:5]
omega <- stolp(xl, 3, -2)
loo <- lOO(omega)
k <- get_optimal_k(loo)
classified_objects <- build_classification_map(omega, k)
draw_plots(xl, omega, classified_objects)