library(shiny)
library(MASS)

ui <- fluidPage(
  
  titlePanel("Байесовские классификаторы"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, radioButtons("classifiers", "Классификатор", c("Оптимальный" = 0, "Наивный" = 1, "Plug-in" = 2, "ЛДФ" = 3), selected = 3, inline = T)),
        column(12, sliderInput("n", "Число наблюдений", 100, 500, 500, 100)),
        
        column(12, "Класс Red", style = "color: red; text-align: center; font-size: 24px"),
        column(6, sliderInput("mu11", "Мат. ожидание First", 10, 20, 10, 1)), column(6, sliderInput("mu21", "Мат. ожидание Second", 1, 10, 5, 1)),
        column(6, sliderInput("sigma11", "Стандартное отклонение First", 0.1, 1, 0.5, 0.1)), column(6, sliderInput("sigma21", "Стандартное отклонение Second", 0.1, 1, 0.5, 0.1)),
        
        column(12, "Класс Blue", style = "color: blue; text-align: center; font-size: 24px"),
        column(6, sliderInput("mu12", "Мат. ожидание First", 10, 20, 15, 1)), column(6, sliderInput("mu22", "Мат. ожидание Second", 1, 10, 5, 1)),
        column(6, sliderInput("sigma12", "Стандартное отклонение First", 0.1, 1, 0.5, 0.1)), column(6, sliderInput("sigma22", "Стандартное отклонение Second", 0.1, 1, 0.5, 0.1))
 
      )
    ),
    
    mainPanel(
      plotOutput("plot"),
      textOutput("error")
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
  
  get_mu <- function(xm) colMeans(xm)

  get_sigma <- function(xm, mu) {
    sum <- 0
    for (i in 1:nrow(xm)) {
      xi <- matrix(c(xm[i,1], xm[i,2]), 1, 2)
      sum <- sum + t(xi - mu) %*% (xi - mu)
    }
    sum / (nrow(xm)-1)
  }
  
  get_discriminant_coeffs <- function(mu, sigma, Py = 0.5) {
    mu1 <- matrix(c(mu[1,1], mu[1,2]),1,2)
    mu2 <- matrix(c(mu[2,1], mu[2,2]),1,2)
    sigma1 <- sigma[1:2,]
    sigma2 <- sigma[3:4,]
    invSigma1 <- solve(sigma1)
    invSigma2 <- solve(sigma2)
    a1 <- invSigma1[1,1]
    b1 <- invSigma1[1,2]
    c1 <- invSigma1[2,2]
    a2 <- invSigma2[1,1]
    b2 <- invSigma2[1,2]
    c2 <- invSigma2[2,2]
    x2 <- a1 - a2
    y2 <- c1 - c2
    xy <- 2 * b1 - 2 * b2
    x <- 2 * b2 * mu2[2] - 2 * b1 * mu1[2] - 2 * a1 * mu1[1] + 2 * a2 * mu2[1]
    y <- 2 * b2 * mu2[1] + 2 * c2 * mu2[2] - 2 * b1 * mu1[1] - 2 * c1 * mu1[2]
    f <- -a2 * mu2[1]^2 - 2 * b2 * mu2[1] * mu2[2] - c2 * mu2[2]^2 + a1 * mu1[1]^2 + 2 * b1 * mu1[1] * mu1[2] + c1 * mu1[2]^2 - log(det(sigma1)) + log(det(sigma2))
    #print(c("x^2" = x2, "y^2" = y2, "xy" = xy, "x" = x, "y" = y, "1" = f))
    return (c("x^2" = x2, "y^2" = y2, "xy" = xy, "x" = x, "y" = y, "1" = f))
  }
  
  draw_discriminant_line <- function(xm, mu, sigma, Py, limits) {
    x <- seq(limits[1,1] - 5, limits[1,2] + 5, length.out = 100)
    y <- seq(limits[2,1] - 5, limits[2,1] + 5, length.out = 100)
    coeffs <- get_discriminant_coeffs(mu, sigma, Py)
    #print(coeffs)
    z <- outer(x, y, function(x, y) coeffs["x^2"]*x^2 + coeffs["xy"]*x*y + coeffs["y^2"]*y^2 + coeffs["x"]*x + coeffs["y"]*y + coeffs["1"])
    n <- ncol(xm)
    colors <- c("blue" = "blue", "red" = "red")
    plot(xm[,1:(n-1)], pch = 21, bg = colors[xm[,n]], col = colors[xm[,n]], main = "Карта классификации нормального распределения", asp = 1)
    contour(x, y, z, lwd = 3, col = "black", levels = 0, drawlabels = F,add = T)
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
    #print(mu)
    #print(sigma)
    draw_discriminant_line(xm, mu, sigma, Py, limits)
  }
  main(xm)
}

fisher <- function(xm) {
  
  get_mu <- function(xm) colMeans(xm)
  
  get_sigma <- function(xm, mu) {
    sum <- 0
    m <- nrow(xm)
    for (i in 1:(m/2)) {
      xi <- matrix(c(xm[i,1], xm[i,2]), 1, 2)
      sum <- sum + t(xi - mu[1,]) %*% (xi - mu[1,])
    }
    for (i in (m/2+1):m) {
      xi <- matrix(c(xm[i,1], xm[i,2]), 1, 2)
      sum <- sum + t(xi - mu[2,]) %*% (xi - mu[2,])
    }
    sum / (m-2)
  }
  
  get_discriminant_coeffs <- function(mu, sigma) {
    mu1 <- matrix(c(mu[1,1], mu[1,2]),1,2)
    mu2 <- matrix(c(mu[2,1], mu[2,2]),1,2)
    invSigma <- solve(sigma)
    a <- invSigma[1,1]
    b <- invSigma[1,2]
    c <- invSigma[2,2]
    x <- 2 * b * mu2[2] - 2 * b * mu1[2] - 2 * a * mu1[1] + 2 * a * mu2[1]
    y <- 2 * b * mu2[1] + 2 * c * mu2[2] - 2 * b * mu1[1] - 2 * c * mu1[2]
    f <- -a * mu2[1]^2 - 2 * b * mu2[1] * mu2[2] - c * mu2[2]^2 + a * mu1[1]^2 + 2 * b * mu1[1] * mu1[2] + c * mu1[2]^2
    return (c("x" = x, "y" = y, "1" = f))
  }
  
  draw_discriminant_line <- function(xm, mu, sigma, limits) {
    x <- seq(limits[1,1] - 5, limits[1,2] + 5, length.out = 100)
    y <- seq(limits[2,1] - 5, limits[2,1] + 5, length.out = 100)
    coeffs <- get_discriminant_coeffs(mu, sigma)
    z <- outer(x, y, function(x, y) coeffs["x"]*x + coeffs["y"]*y + coeffs["1"])
    n <- ncol(xm)
    colors <- c("blue" = "blue", "red" = "red")
    plot(xm[,1:(n-1)], pch = 21, bg = colors[xm[,n]], col = colors[xm[,n]], main = "Карта классификации нормального распределения", asp = 1)
    contour(x, y, z, lwd = 3, levels = 0, col = "black", drawlabels = F,add = T)
  }
  
  main <- function(xm) {
    Py <- c(0.5, 0.5)
    m <- nrow(xm)
    tmp1 <- xm[1:(m/2),1:2]
    tmp2 <- xm[(m/2+1):m,1:2]
    mu <- rbind(get_mu(tmp1), get_mu(tmp2))
    mu <- matrix(c(mu[1,1], mu[2,1], mu[1,2], mu[2,2]), 2, 2)
    sigma <- (get_sigma(xm, mu))
    limits <- matrix(c(min(mu[,1]), min(mu[,2]), max(mu[,1]), max(mu[,2])), 2, 2)
    classes <- unique(xm[,ncol(xm)])
    draw_discriminant_line(xm, mu, sigma, limits)
  }
  main(xm)
}

server <- function(input, output) {
  
  output$plot <- renderPlot({
    output$error = renderText("")
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
    else if (input$classifiers == 2) {
      sigma1 <- matrix(c(input$sigma11, 0, 0, input$sigma21), 2, 2)
      sigma2 <- matrix(c(input$sigma12, 0, 0, input$sigma22), 2, 2)
      mu1 <- c(input$mu11, input$mu21)
      mu2 <- c(input$mu12, input$mu22)
      xm1 <- mvrnorm(n = n/2, mu1, sigma1)
      xm2 <- mvrnorm(n = n/2, mu2, sigma2)
      colnames(xm1) <- c()
      colnames(xm2) <- c()
      xm <- data.frame()
      xm <- rbind(xm, xm1)
      xm <- rbind(xm, xm2)
      classes <- 1:n
      classes[1:(n/2)] <- "red"
      classes[(n/2+1):n] <- "blue"
      xm <- cbind(xm, classes)
      colnames(xm) <- c("First", "Second", "Class")
      plug_in(xm)
    }
    else if (input$classifiers == 3) {
      sigma1 <- matrix(c(input$sigma11, 0, 0, input$sigma21), 2, 2)
      if (sigma1[1,1] != input$sigma12 || sigma1[2,2] != input$sigma22) {
        output$error = renderText("Стандартные отклонения соответствующих признаков всех классов должны быть равны")
        return ()
      }
      mu1 <- c(input$mu11, input$mu21)
      mu2 <- c(input$mu12, input$mu22)
      xm1 <- mvrnorm(n = n/2, mu1, sigma1)
      xm2 <- mvrnorm(n = n/2, mu2, sigma1)
      colnames(xm1) <- c()
      colnames(xm2) <- c()
      xm <- data.frame()
      xm <- rbind(xm, xm1)
      xm <- rbind(xm, xm2)
      classes <- 1:n
      classes[1:(n/2)] <- "red"
      classes[(n/2+1):n] <- "blue"
      xm <- cbind(xm, classes)
      colnames(xm) <- c("First", "Second", "Class")
      fisher(xm)
    }
  })
}

shinyApp(ui = ui, server = server)

