library(shiny)

ui <- fluidPage(
  
  titlePanel("Байесовские классификаторы"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, sliderInput("n", "Число наблюдений", 10, 50, 50, 10)),
        column(12, "Класс Red", style = "color: red; text-align: center; font-size: 24px"),
        column(6, sliderInput("mu11", "Мат. ожидание First", 10, 20, 10, 1)), column(6, sliderInput("mu21", "Мат. ожидание Second", 1, 10, 5, 1)),
        column(6, sliderInput("sigma11", "Стандартное отклонение First", 0.1, 1, 0.5, 0.1)), column(6, sliderInput("sigma21", "Стандартное отклонение Second", 0.1, 1, 1, 0.1)),
        
        column(12, "Класс Blue", style = "color: blue; text-align: center; font-size: 24px"),
        column(6, sliderInput("mu12", "Мат. ожидание First", 10, 20, 11, 1)), column(6, sliderInput("mu22", "Мат. ожидание Second", 1, 10, 6, 1)),
        column(6, sliderInput("sigma12", "Стандартное отклонение First", 0.1, 1, 0.5, 0.1)), column(6, sliderInput("sigma22", "Стандартное отклонение Second", 0.1, 1, 0.7, 0.1)),
        
        column(12, radioButtons("classifiers", "Классификатор", c("Оптимальный" = 0, "Наивный" = 1, "Plug-in" = 2), selected = 1, inline = T))
      )
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

optimal_bayes <- function(xm, input) {
  p <- function(ksi, mu, sigma) (1 / (sigma * sqrt(2 * pi))) * exp(-(ksi - mu) ^ 2 / (2 * sigma ^ 2))
  
  classifier <- function(x, classes, mu, sigma, Py, lambda = c(1, 1)) {
    sum_by_class <- rep(0, length(classes))
    names(sum_by_class) <- classes
    for (i in 1:length(sum_by_class)) {
      sum <- 0
      for (j in 1:length(x)) sum <- sum + log(p(x[j], mu[i,j], sigma[i,j]))
      sum_by_class[i] <- log(lambda[i] * Py[i]) + sum
    }
    names(which.max(sum_by_class))
  }
  
  build_classification_map <- function(classes, mu, sigma, Py, limits) {
    classifiedObjects <- c()
    for (i in seq(limits[1,1] - 5, limits[1,2] + 5, 0.1))
      for (j in seq(limits[2,1] - 5, limits[2,1] + 5, 0.1)) 
        classifiedObjects <- rbind(classifiedObjects, c(i, j, classifier(c(i, j), classes, mu, sigma, Py)))
      classifiedObjects
  }
  
  draw_plot <- function(xm, classified_objects) {
    n <- ncol(xm)
    colors <- c("blue" = "blue", "red" = "red")
    plot(xm[,1:(n-1)], pch = 21, bg = colors[xm[,n]], col = colors[xm[,n]], main = "Карта классификации нормального распределения", asp = 1)
    points(classified_objects[,1:(n-1)], pch = 21, col = colors[classified_objects[,n]])
  }
  
  get_mu <- function(xm) sum(xm) / length(xm)
  
  get_sigma <- function(xm, mu) sum((xm - nu)^2) / (m-1)
  
  main <- function(xm) {
    Py <- c(0.5, 0.5)
    mu <- rbind(c(input$mu11, input$mu21), c(input$mu12, input$mu22))
    sigma <- rbind(c(input$sigma11, input$sigma21), c(input$sigma12, input$sigma22))
    classes <- unique(xm[,ncol(xm)])
    limits <- matrix(c(min(mu[,1]), min(mu[,2]), max(mu[,1]), max(mu[,2])), 2, 2)
    
    classified_objects <- build_classification_map(classes, mu, sigma, Py, limits)
    draw_plot(xm, classified_objects)
  }
  
  main(xm)
}


naive_bayes <- function(xm) {
  p <- function(ksi, mu, sigma) (1 / (sigma * sqrt(2 * pi))) * exp(-(ksi - mu) ^ 2 / (2 * sigma ^ 2))
  
  classifier <- function(x, classes, mu, sigma, Py, lambda = c(1, 1)) {
    sum_by_class <- rep(0, length(classes))
    names(sum_by_class) <- classes
    for (i in 1:length(sum_by_class)) {
      sum <- 0
      for (j in 1:length(x)) sum <- sum + log(p(x[j], mu[i,j], sigma[i,j]))
      sum_by_class[i] <- log(lambda[i] * Py[i]) + sum
    }
    names(which.max(sum_by_class))
  }
  
  build_classification_map <- function(classes, mu, sigma, Py, limits) {
    classifiedObjects <- c()
    for (i in seq(limits[1,1] - 5, limits[1,2] + 5, 0.1))
      for (j in seq(limits[2,1] - 5, limits[2,1] + 5, 0.1)) 
        classifiedObjects <- rbind(classifiedObjects, c(i, j, classifier(c(i, j), classes, mu, sigma, Py)))
      classifiedObjects
  }
  
  draw_plot <- function(xm, classified_objects) {
    n <- ncol(xm)
    colors <- c("blue" = "blue", "red" = "red")
    plot(xm[,1:(n-1)], pch = 21, bg = colors[xm[,n]], col = colors[xm[,n]], main = "Карта классификации нормального распределения", asp = 1)
    points(classified_objects[,1:(n-1)], pch = 21, col = colors[classified_objects[,n]])
  }
  
  get_mu <- function(xm) sum(xm) / length(xm)
  
  get_sigma <- function(xm, mu) sum((xm - mu)^2) / (length(xm)-1)
  
  main <- function(xm) {
    Py <- c(0.5, 0.5)
    m <- nrow(xm)
    tmp11 <- xm[1:(m/2),1]
    tmp21 <- xm[1:(m/2),2]
    tmp12 <- xm[(m/2+1):m,1]
    tmp22 <- xm[(m/2+1):m,2]
    mu <- rbind(c(get_mu(tmp11), get_mu(tmp21)), c(get_mu(tmp12), get_mu(tmp22)))
    sigma <- rbind(c(get_sigma(tmp11, mu[1,1]), get_sigma(tmp21, mu[1,2])), c(get_sigma(tmp12, mu[2,1]), get_sigma(tmp22, mu[2,2])))
    classes <- unique(xm[,ncol(xm)])
    limits <- matrix(c(min(mu[,1]), min(mu[,2]), max(mu[,1]), max(mu[,2])), 2, 2)
    
    classified_objects <- build_classification_map(classes, mu, sigma, Py, limits)
    draw_plot(xm, classified_objects)
  }
  
  main(xm)
}


plug_in <- function(xm) {
  get_mu <- function(xm) {
    sum <- 0
    for (i in nrow(xm)) sum <- sum + xm[i,1:2]
    sum / nrow(xm)
  }
  
  get_sigma <- function(xm, mu) {
    sum <- 0
    for (i in 1:nrow(xm)) {
      xi <- matrix(c(xm[i,1], xm[i,2]), 1, 2)
      sum <- sum + t(xi - mu) %*% (xi - mu)
    }
    sum / (nrow(xm)-1)
  }
  
  p <- function(x, mu, sigma) (1 / (2*pi * sqrt(det(sigma)))) * exp(-0.5*(t(x-mu) %*% solve(sigma) %*% (x-mu)))
  
  classifier <- function(x, classes, mu, sigma, Py = c(0.5, 0.5), lambda = c(1,1)) {
    sum_by_class <- rep(0, length(classes))
    names(sum_by_class) <- classes
    sum_by_class[1] <- lambda[1] * Py[1] * p(x, mu[1,], sigma[1:2,])
    sum_by_class[2] <- lambda[2] * Py[2] * p(x, mu[2,], sigma[3:4,])
    return (sum_by_class)
  }
  
  draw_discriminant_line <- function(xm, classes, mu, sigma, Py, limits) {
    x <- seq(limits[1,1] - 5, limits[1,2] + 5, length.out = 100)
    y <- seq(limits[2,1] - 5, limits[2,1] + 5, length.out = 100)
    for (i in x)
      for (j in y) {
        sum_by_class <- classifier(c(i, j), classes, mu, sigma, Py)
        if (sum_by_class[1] == 0 && sum_by_class[2] == 0)
          print(sum_by_class)
      }
    print(1)
        
  }
  
  main <- function(xm) {
    Py <- c(0.5, 0.5)
    m <- nrow(xm)
    tmp1 <- xm[1:(m/2),1:2]
    tmp2 <- xm[(m/2+1):m,1:2]
    mu <- rbind(get_mu(tmp1), get_mu(tmp2))
    mu <- matrix(c(mu[1,1], mu[2,1], mu[1,2], mu[2,2]), 2, 2)
    sigma <- rbind(get_sigma(tmp1, mu[1,]), get_sigma(tmp2, mu[2,]))
    limits <- matrix(c(min(mu[,1]), min(mu[,2]), max(mu[,1]), max(mu[,2])), 2, 2)
    classes <- unique(xm[,ncol(xm)])
    draw_discriminant_line(xm, classes, mu, sigma, Py, limits)
  }
  main(xm)
}

server <- function(input, output) {
  
  output$plot <- renderPlot({
    n <- input$n
    xm11 <- rnorm(n/2, input$mu11, input$sigma11)
    xm12 <- rnorm(n/2, input$mu21, input$sigma21)
    xm21 <- rnorm(n/2, input$mu12, input$sigma12)
    xm22 <- rnorm(n/2, input$mu22, input$sigma22)
    tmp1 <- cbind(xm11, xm12)
    tmp2 <- cbind(xm21, xm22)
    colnames(tmp1) <- c()
    colnames(tmp2) <- c()
    xm <- data.frame()
    xm <- rbind(xm, tmp1)
    xm <- rbind(xm, tmp2)
    classes <- 1:n
    classes[1:(n/2)] <- "red"
    classes[(n/2+1):n] <- "blue"
    xm <- cbind(xm, classes)
    colnames(xm) <- c("First", "Second", "Class")
    if (input$classifiers == 0) optimal_bayes(xm, input)
    else if (input$classifiers == 1) naive_bayes(xm)
    #else if (input$classifiers == 2) plug_in(xm)
  })
}

shinyApp(ui = ui, server = server)

