library(shiny)
library(MASS)

ui <- fluidPage(
  titlePanel("EM-алгоритм"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, "Параметры алгоритма", style = "color: black; text-align: center; font-size: 24px"),
        column(6, sliderInput("r", "R", 0.1, 1, 0.3, 0.1)), column(6, selectInput("delta", "delta", c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10), 1e-6)),
        column(12, "Класс Red", style = "color: red; text-align: center; font-size: 24px"),
        column(12, sliderInput("n_comp_1", "Количество компонент", 3, 5, 1, 1)),
        column(12, sliderInput("n_el_1", "Количество объектов в каждой компоненте", 50, 100, 50, 10)),
        column(6, sliderInput("mu11", "Мат. ожидание First", 0, 20, 0, 1)), column(6, sliderInput("mu12", "Мат. ожидание Second", 0, 10, 0, 1)),
        column(6, sliderInput("sigma11", "Стандартное отклонение First", 0.1, 1, 0.5, 0.1)), column(6, sliderInput("sigma12", "Стандартное отклонение Second", 0.1, 1, 0.5, 0.1)),
        column(6, sliderInput("step11", "Шаг новой компоненты от центра старой First", 0, 5, 1, 1)), column(6, sliderInput("step12", "Шаг новой компоненты от центра старой Second", 0, 5, 0, 1)),
        
        column(12, "Класс Blue", style = "color: blue; text-align: center; font-size: 24px"),
        column(12, sliderInput("n_comp_2", "Количество компонент", 3, 5, 1, 1)),
        column(12, sliderInput("n_el_2", "Количество объектов в каждой компоненте", 50, 500, 50, 50)),
        column(6, sliderInput("mu21", "Мат. ожидание First", 0, 20, 20, 1)), column(6, sliderInput("mu22", "Мат. ожидание Second", 0, 10, 0, 1)),
        column(6, sliderInput("sigma21", "Стандартное отклонение First", 0.1, 1, 0.5, 0.1)), column(6, sliderInput("sigma22", "Стандартное отклонение Second", 0.1, 1, 0.5, 0.1)),
        column(6, sliderInput("step21", "Шаг новой компоненты от центра старой First", 0, 5, 1, 1)), column(6, sliderInput("step22", "Шаг новой компоненты от центра старой Second", 0, 5, 0, 1))
      )
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

generate_xl <- function(n, n_comp, mu, sigma1, sigma2, step) {
  xl <- c()
  mu_res <- mu[1,]
  stp <- step[1,]
  for (i in 1:n_comp[1]) {
    xl <- rbind(xl, cbind(mvrnorm(n[1], mu_res, sigma1), 1))
    mu_res <- mu_res + (-1)^i * stp
    stp <- stp + step[1,]
  }
  mu_res <- mu[2,]
  stp <- step[2,]
  for (i in 1:n_comp[2]) {
    xl <- rbind(xl, cbind(mvrnorm(n[2], mu_res, sigma2), 2))
    mu_res <- mu_res + (-1)^i * stp
    stp <- stp + step[2,]
  }
  xl
}



phi <- function(x, mu, sigma) prod(sapply(1:length(x), function(i) ((1 / (sqrt(sigma[i,i]) * sqrt(2 * pi))) * exp(-0.5 * ((x[i] - mu[i]) / sqrt(sigma[i,i]))^2))))
get_mu <- function(xl) colMeans(xl)
get_sigma <- function(xl, mu) {
  sum <- 0
  for (i in 1:nrow(xl)) {
    xi <- matrix(c(xl[i, 1], xl[i, 2]), 1, 2)
    sum <- sum + t(xi - mu) %*% (xi - mu)
  }
  sum / (nrow(xl) - 1)
}

get_phi_all <- function(objects, theta, w) {
  phi_all <- c()
  k <- nrow(theta) / 2
  for (i in 1:nrow(objects)) phi_all <- c(phi_all, sum(sapply(1:k, function(j) w[j] * phi(c(objects[i, ]), theta[(2*j-1):(2*j),3], theta[(2*j-1):(2*j), (1:2)]))))
  return (phi_all)
}

rho2 <- function(x1, x2, sigma) {
  sum <- 0
  n <- length(x1)
  for (d in 1:n) {
    tmp0 <- sqrt(sigma[d, d])^(-2)
    tmp1 <- abs(x1[d] - x2[d])^2
    sum <- sum + tmp0 * tmp1
  }
  return (sum)
}

p <- function(x, mu, sigma) {
  n <- length(x)
  tmp <- 1
  for (i in 1:n) tmp <- tmp * sqrt(sigma[i,i])
  nj <- (2 * pi)^(-n/2)* tmp^(-1)
  return (nj * exp(-0.5 * rho2(x, mu, sigma)))
}

EM_seq <- function(xl, m0, r, delta) {
  # EM - алгоритм с последовательным добавлением компонент
  l <- nrow(xl)
  objects <- xl[,-ncol(xl)]
  mu <- matrix(get_mu(objects), 1, 2)
  sigma <- get_sigma(objects, mu)
  theta <- cbind(sigma, t(mu))
  k <- 1
  w <- 1
  # print(theta)
  max_iter <- 7 # максимум компонент, чтобы не зацикливался
  for (iter in 1:max_iter) {
    phi_all <- get_phi_all(objects, theta, w)
    max_r <- max(phi_all) * r
    u <- which(phi_all < max_r)
    u_capacity <- length(u)
    
    if (u_capacity < m0) break
    
    k <- k + 1
    wk <- u_capacity / l
    # print(u_capacity)
    w <- sapply(1:length(w), function(i) w[i] * (1 - wk))
    w <- c(w, wk)
    mu <- matrix(get_mu(objects[u,]), 1, 2)
    sigma <- get_sigma(objects[u,], mu)
    theta <- rbind(theta, cbind(sigma, t(mu)))
    theta <- EM(xl, k, theta, delta, w)
    w <- c(theta[1:k,4]) # Уберём w из полученного theta
    theta <- theta[,-4]
    # print(k)
    # print(theta)
    # print(w)
  }
  return (cbind(theta, as.matrix(c(w, rep(0, k)))))
}

to_stop <- function(g, g0) {
  g <- abs(g - g0)
  max_ind <- arrayInd(which.max(g), dim(g))
  return (g[max_ind[1], max_ind[2]])
}

EM <- function(xl, k, theta, delta, w) {
  # EM - алгоритм с фиксированным числом компонент
  l <- nrow(xl)
  objects <- xl[,-ncol(xl)]
  n <- ncol(objects)
  g <- matrix(0, l, k)
  g0 <- matrix(0, l, k)
  max_iter <- 100 # максимум итераций, чтобы не зацикливался
  for (iter in 1:max_iter) {
    # E - шаг
    for (i in 1:l)
      for (j in 1:k) {
        g0[i, j] <- g[i, j]
        tmp <- sum(sapply(1:k, function(s) w[s] * phi(objects[i,], theta[(2*s-1):(2*s), 3], theta[(2*s-1):(2*s), (1:2)])))
        tmp1 <- w[j] * phi(objects[i,], theta[(2*j-1):(2*j), 3], theta[(2*j-1):(2*j), (1:2)])
        g[i, j] <- tmp1 / tmp 
      }
    # Жуткий костыль: если в последнем столбце все нули (почему то иногда плотность становится нулевой), тогда вернём, как было раньше
    if (sum(g[,k]) < 1e-50) g[,k] = g0[,k]
    # M - шаг
    for (j in 1:k) {
      w[j] <- sum(g[,j]) / l
      mu <- sapply(1:n, function(i) sum(g[,j] * objects[,i]) / (l * w[j])) # Оптимальное мат ожидание
      sigma <- matrix(0, 2, 2)
      sigma[1,1] <- sum(g[,j] * (objects[,1] - mu[1])^2) / (l * w[j]) # Оптимальная матрица ковариации (диагональная)
      sigma[2,2] <- sum(g[,j] * (objects[,2] - mu[2])^2) / (l * w[j])
      theta[(2*j-1),] <- c(sigma[1,], mu[1])
      theta[(2*j),] <- c(sigma[2,], mu[2])
    }
    if (to_stop(g, g0) <= delta) break
  }
  return (cbind(theta, as.matrix(c(w, rep(0, k)))))
}

solve_the_problem <- function(xl, r = 0.3, delta = 1e-8, n) {
  l <- nrow(xl)
  first_class <- xl[which(xl[,3] == 1),]
  second_class <- xl[which(xl[,3] == 2),]
  theta1 <- EM_seq(first_class, round(nrow(first_class) / 3), r, delta)
  theta2 <- EM_seq(second_class, round(nrow(second_class) / 3), r, delta)
  k1 <- nrow(theta1) / 2
  k2 <- nrow(theta2) / 2
  w1 <- c(theta1[,4])
  w1 <- w1[1:k1]
  w2 <- c(theta2[,4])
  w2 <- w2[1:k2]
  theta1 <- theta1[,-4]
  theta2 <- theta2[,-4]
  P1 <- nrow(first_class) / l
  P2 <- nrow(second_class) / l
  divide(theta1, theta2, w1, w2, k1, k2, P1, P2)
}

divide <- function(theta1, theta2, w1, w2, k1, k2, P1, P2) {
  tmp1 <- 2*k1
  tmp2 <- 2*k2
  x <- seq(min(theta1[which(1:tmp1 %% 2 == 1), 3], theta2[which(1:tmp2 %% 2 == 1), 3]) - 5, max(theta1[which(1:tmp1 %% 2 == 1), 3], theta2[which(1:tmp2 %% 2 == 1), 3]) + 5, length.out = 100)
  y <- seq(min(theta1[which(1:tmp1 %% 2 == 0), 3], theta2[which(1:tmp2 %% 2 == 0), 3]) - 5, max(theta1[which(1:tmp1 %% 2 == 0), 3], theta2[which(1:tmp2 %% 2 == 0), 3]) + 5, length.out = 100)
  z <- matrix(NA, length(x), length(y))
  for (i in 1:length(x))
    for (j in 1:length(y)) {
      p1 <- sapply(1:k1, function(k) p(c(x[i], y[j]), theta1[(2*k-1):(2*k), 3], theta1[(2*k-1):(2*k), (1:2)]))
      p2 <- sapply(1:k2, function(k) p(c(x[i], y[j]), theta2[(2*k-1):(2*k), 3], theta2[(2*k-1):(2*k), (1:2)]))
      p1 <- sum(p1 * w1) * P1
      p2 <- sum(p2 * w2) * P2
      z[i, j] <- p1 - p2
    }
  contour(x, y, z, lwd = 3, col = "black", add = T, drawlabels = F, levels = 0)
}

server <- function(input, output) {
  output$plot <- renderPlot({
    n_comp <- c(input$n_comp_1, input$n_comp_2)
    n <- c(input$n_el_1, input$n_el_2)
    mu <- matrix(c(input$mu11, input$mu21, input$mu12, input$mu22), 2, 2)
    sigma1 <- matrix(c(input$sigma11, 0, 0, input$sigma12), 2, 2)
    sigma2 <- matrix(c(input$sigma21, 0, 0, input$sigma22), 2, 2)
    step <- matrix(c(input$step11, input$step21, input$step12, input$step22), 2, 2)
    r <- input$r
    delta <- as.numeric(input$delta)

    
    xl <- generate_xl(n, n_comp, mu, sigma1, sigma2, step)
    colors <- c("red", "blue")
    plot(xl[,-ncol(xl)], pch = 21, col = colors[xl[,ncol(xl)]], bg = colors[xl[,ncol(xl)]], asp = 1)
    solve_the_problem(xl, r, delta, n)
  })
}

shinyApp(ui, server)
