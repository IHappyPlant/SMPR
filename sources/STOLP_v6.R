distance <- function(u, v) sqrt(sum((u - v)^2))
get_distances <- function(xl, z) apply(xl[,1:(ncol(xl)-1)], 1, distance, z)
sort_objects_by_dist <- function(xl, z) xl[order(get_distances(xl, z)),]

w.kwnn <- function(i, k, q) (i <= k) * q^i
kwnn <- function(xl, z, k, q) {
  ordered_xl <- sort_objects_by_dist(xl, z)
  weights <- w.kwnn(1:nrow(ordered_xl), k, q)
  names(weights) <- ordered_xl[, ncol(ordered_xl)]
  sum_by_class <- sapply(unique(sort(names(weights))), function(class, weights) sum(weights[names(weights) == class]), weights)
  names(which.max(sum_by_class))
}

margin <- function(xl, z, k, q, target_class) {
  ordered_xl <- sort_objects_by_dist(xl, z)
  weights <- w.kwnn(1:nrow(ordered_xl), k, q)
  names(weights) <- ordered_xl[, ncol(ordered_xl)]
  sum(weights[names(weights) == target_class]) - sum(weights[names(weights) != target_class])
}

stolp <- function(xl, err_lim, noise_lim) {
  l <- nrow(xl)
  n <- ncol(xl)
  omega <- data.frame()
  margins = rep(0, l)
  k <- 20
  q <- 0.1
  for (i in 1:l) margins[i] <- margin(xl[-i,], xl[i, 1:(n-1)], k, q, xl[i,n])
  new_xl <- xl[which(margins > noise_lim),]
  margins <- margins[which(margins > noise_lim)]
  to_out <- c()
  for (i in unique(new_xl[,n])) {
    indexes <- which(new_xl[,n] == i)
    omega <- rbind(omega, new_xl[indexes[which.max(margins[indexes])],])
    to_out <- c(to_out, indexes[which.max(margins[indexes])])
  }
  new_xl <- new_xl[-to_out,]
  l_new_xl <- nrow(new_xl)
  l_omega <- nrow(omega)
  print(omega)
  while (l > l_omega) {
    rownames(new_xl) <- c(1:l_new_xl)
    errors <- 0
    for (i in 1:l) errors <- errors + (kwnn(omega, xl[i, 1:(n-1)], k, q) != xl[i, n])
    print(paste("errors = ", errors))
    if (errors <= err_lim) break
    margins <- rep(0, l_new_xl)
    for (i in 1:l_new_xl) margins[i] <- margin(omega, new_xl[i, 1:(n-1)], k, q, new_xl[i, n])
    to_out <- c()
    errors <- new_xl[which(margins < 0),]
    margins <- margins[which(margins < 0)]
    print(errors)
    for (i in unique(errors[,n])) {
      indexes <- which(errors[,n] == i)
      omega <- rbind(omega, errors[indexes[which.min(margins[indexes])],])
      to_out <- c(to_out, as.numeric(rownames(new_xl[which.min(margins[indexes]),])))
    }
    new_xl <- new_xl[-to_out,]
    l_new_xl <- nrow(new_xl)
    print(omega)
  }
  omega
}

build_classification_map <- function(xl, k, q) {
  classified_objects <- c()
  for (i in seq(0, 7, 0.1))
    for (j in seq(0, 2.5, 0.1))
      classified_objects <- rbind(classified_objects, c(i, j, kwnn(xl, c(i, j), k, q)))
  classified_objects
}

draw_plots <- function(xl, omega, classified_objects) {
  n <- ncol(xl)
  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  par(mfrow=c(1,2))
  plot(xl[, 1:(n-1)], pch = 21, col = colors[xl[,n]], main = "Эталонные объекты", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  points(omega[, 1:(n-1)], pch = 21, bg = colors[omega[,n]], col = "black")
  plot(omega[, 1:(n-1)], pch = 21, bg = colors[omega[,n]], col = colors[omega[,n]], xlim = c(0, 7), main = "Классификация ирисов Фишера методом kwNN", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  points(classified_objects[, 1:(n-1)], pch = 22, col = colors[classified_objects[,n]])
}

main <- function() {
  xl <- iris[, 3:5]
  omega <- stolp(xl, 4, -0.01)
  k <- 20
  q <- 0.1
  classified_objects <- build_classification_map(omega, k, q)
  draw_plots(xl, omega, classified_objects)
}

main()