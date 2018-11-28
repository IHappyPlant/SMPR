distance = function(u, v) sqrt(sum((u - v)^2))
get_distances <- function(xl, z) apply(xl[,1:(ncol(xl)-1)], 1, distance, z)
sort_objects_by_dist <- function(xl, z) xl[order(get_distances(xl, z)),]

onn <- function(xl, z) sort_objects_by_dist(xl, z)[1, ncol(xl)]

build_class_map <- function(xl) {
  classifiedObjects <- c()
  for (i in seq(0, 7, 0.1))
    for (j in seq(0, 2.5, 0.1))
      classifiedObjects <- rbind(classifiedObjects, c(i, j, onn(xl, c(i, j))))
  classifiedObjects
}

draw_plot <- function(xl, classifiedObjects) {
  n <- ncol(classifiedObjects)
  colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  plot(xl[, 1:(n-1)], pch = 21, bg = colors[xl[,n]], col = colors[xl[,n]], xlim = c(0, 7), main="Классификация ирисов Фишера методом 1NN", xlab = "Длина лепестка", ylab = "Ширина лепестка", asp = 1)
  points(classifiedObjects[, 1:(n-1)], pch = 22, col = colors[classifiedObjects[, n]])
}

main <- function() {
  xl <- iris[, 3:5]
  classified_objects <- build_class_map(xl)
  draw_plot(xl, classified_objects)
}

main()
