library(shiny)

ui <- fluidPage(
  
  titlePanel("Линейные классификаторы"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, radioButtons("classifiers", "Классификатор", c("ADALINE" = 0, "Правило Хебба" = 1, "Логистич. регрессия" = 2, "SVM" = 3, "Все сразу" = 4), selected = 0, inline = T)),
        column(12, sliderInput("n", "Число наблюдений", 100, 500, 100, 100)),
        
        column(12, "Класс Red", style = "color: red; text-align: center; font-size: 24px"),
        column(6, sliderInput("mu11", "Мат. ожидание First", 0, 20, 0, 1)), column(6, sliderInput("mu21", "Мат. ожидание Second", 0, 10, 0, 1)),
        column(6, sliderInput("sigma11", "Стандартное отклонение First", 0.1, 1, 0.5, 0.1)), column(6, sliderInput("sigma21", "Стандартное отклонение Second", 0.1, 1, 0.5, 0.1)),
        
        column(12, "Класс Blue", style = "color: blue; text-align: center; font-size: 24px"),
        column(6, sliderInput("mu12", "Мат. ожидание First", 0, 20, 6, 1)), column(6, sliderInput("mu22", "Мат. ожидание Second", 0, 10, 0, 1)),
        column(6, sliderInput("sigma12", "Стандартное отклонение First", 0.1, 1, 0.5, 0.1)), column(6, sliderInput("sigma22", "Стандартное отклонение Second", 0.1, 1, 0.5, 0.1))
        
      )
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

normalize <- function(xl) {
  for (i in 1:(ncol(xl)-1)) xl[,i] <- (xl[,i] - mean(xl[,i])) / sd(xl[,i])
  xl
}

add_col_for_w0 <- function(xl) cbind(xl[,1:(ncol(xl)-1)], -1, xl[, ncol(xl)])

loss.Q <- function(m) (1-m)^2
loss.L <- function(m) max(-m,0)
loss.Log <- function(m) log2(1 + exp(-m))
sigmoid <- function(z) 1 / (1 + exp(-z))

adaline.get_w <- function(w, object, class, eta)  w - c(eta) * (w %*% object - class) %*% object
hebb.get_w <- function(w, object, class, eta) w + eta * object * class
logistic.get_w <- function(w, object, class, eta) w + eta * object * class * sigmoid(c(w %*% object) * class)

gradient <- function(xl, eta, lambda, rule, loss_function, method) {
  l <- nrow(xl)
  n <- ncol(xl)
  w <- runif(n-1, -1/(2*(n-1)), 1/(2*(n-1)))
  objects <- xl[,-n]
  classes <- xl[, n]
  q <- 0
  for (i in 1:l) q <- q + loss_function(w %*% objects[i,] * classes[i])
  
  cnt <- 0
  while (T) {
    cnt <- cnt + 1
    
    rand <- sample(1:l, 1)
    eps <- loss_function(w %*% objects[rand,] * classes[rand])
    
    eta <- 1 / sqrt(cnt)
    if (method != "hebb")
      w <- rule(w, objects[rand,], classes[rand], eta)
    else if (w %*% objects[rand,] * classes[rand] < 0) {
      w <- rule(w, objects[rand,], classes[rand], eta)
    }
    q_prev <- q
    q <- (1 - lambda) * q + lambda * eps
    if (abs(q_prev - q) <= 1e-5) break
    else if (cnt == 30000) { print("exit by cnt"); break; }
  }
  w
}


adaline <- function(xl, new_xl) {
  w <- gradient(new_xl, 1, 1/6, adaline.get_w, loss.Q, "adaline")
  n <- ncol(xl)
  colors <- c("1" <- "red", "2" = "blue")
  plot(xl[,1:(n-1)], pch = 21, col = colors[xl[,n]], bg = colors[xl[,n]], asp = 1)
  x <- seq(-20, 20, length.out = 100)
  y <- seq(-20, 20, length.out = 100)
  z <- outer(x, y, function(x, y) w[1] * x + w[2] * y + w[3])
  contour(x, y, z, levels = 0, add = T, drawlabels = F, lwd = 3, col = "navy")
}

perceptron <- function(xl, new_xl) {
  w <- gradient(new_xl, 1, 1/6, hebb.get_w, loss.L, "hebb")
  n <- ncol(xl)
  colors <- c("1" <- "red", "2" = "blue")
  plot(xl[,1:(n-1)], pch = 21, col = colors[xl[,n]], bg = colors[xl[,n]], asp = 1)
  x <- seq(-20, 20, length.out = 100)
  y <- seq(-20, 20, length.out = 100)
  z <- outer(x, y, function(x, y) w[1] * x + w[2] * y + w[3])
  contour(x, y, z, levels = 0, add = T, drawlabels = F, lwd = 3, col = "gold")
}

logistic <- function(xl, new_xl) {
  w <- gradient(new_xl, 1, 1/6, logistic.get_w, loss.Log, "logistic")
  n <- ncol(xl)
  colors <- c("1" <- "red", "2" = "blue")
  plot(xl[,1:(n-1)], pch = 21, col = colors[xl[,n]], bg = colors[xl[,n]], asp = 1)
  x <- seq(-20, 20, length.out = 100)
  y <- seq(-20, 20, length.out = 100)
  z <- outer(x, y, function(x, y) w[1] * x + w[2] * y + w[3])
  contour(x, y, z, levels = 0, add = T, drawlabels = F, lwd = 3, col = "orchid")
}

compare <- function(xl, new_xl) {
  n <- ncol(xl)
  x <- seq(-20, 20, length.out = 100)
  y <- seq(-20, 20, length.out = 100)
  w <- gradient(new_xl, 1, 1/6, adaline.get_w, loss.Q, "adaline")
  z1 <- outer(x, y, function(x, y) w[1] * x + w[2] * y + w[3])
  w <- gradient(new_xl, 1, 1/6, hebb.get_w, loss.L, "hebb")
  z2 <- outer(x, y, function(x, y) w[1] * x + w[2] * y + w[3])
  w <- gradient(new_xl, 1, 1/6, logistic.get_w, loss.Log, "logistic")
  z3 <- outer(x, y, function(x, y) w[1] * x + w[2] * y + w[3])
  colors <- c("1" <- "red", "2" = "blue")
  plot(xl[,1:(n-1)], pch = 21, col = colors[xl[,n]], bg = colors[xl[,n]], asp = 1)
  contour(x, y, z1, levels = 0, add = T, drawlabels = F, lwd = 3, col = "navy")
  contour(x, y, z2, levels = 0, add = T, drawlabels = F, lwd = 3, col = "gold")
  contour(x, y, z3, levels = 0, add = T, drawlabels = F, lwd = 3, col = "orchid")
  legend("bottomright", c("ADALINE", "Персептрон", "Логистич. регрессия"), pch = c("l","l","l"), col = c("navy", "gold", "orchid"))
}

server <- function(input, output) {
  output$plot = renderPlot({
    n <- input$n
    xl11 <- rnorm(n/2, input$mu11, input$sigma11)
    xl12 <- rnorm(n/2, input$mu21, input$sigma21)
    xl21 <- rnorm(n/2, input$mu12, input$sigma12)
    xl22 <- rnorm(n/2, input$mu22, input$sigma22)
    tmp1 <- cbind(xl11, xl12)
    tmp2 <- cbind(xl21, xl22)
    xl <- rbind(cbind(tmp1, 1), cbind(tmp2, 2))
    colnames(xl) <- c("First", "Second", "Class")
    
    n_xl <- normalize(xl)
    n_xl <- add_col_for_w0(n_xl)
   
    if (input$classifiers == 0) adaline(xl, n_xl)
    else if (input$classifiers == 1) perceptron(xl, n_xl)
    else if (input$classifiers == 2) logistic(xl, n_xl)
    else if (input$classifiers == 4) compare(xl, n_xl)
  })
  
}

shinyApp(ui, server)
