library(shiny)
library(kernlab)

ui <- fluidPage(
  
  titlePanel("Линейные классификаторы"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, radioButtons("classifiers", "Классификатор", c("ADALINE" = 0, "Правило Хебба" = 1, "Логистич. регрессия" = 2, "SVM" = 3, "Все сразу" = 4), selected = 0, inline = T), style = "text-align: center"),
        column(6, checkboxInput("show_iter", "Показывать итерации", T)), column(6, checkboxInput("show_q", "Показывать изменение Q", T)),
        column(12, selectInput("eps_q", "Критерий стабилизации Q", c(0.00001, 0.0001, 0.001, 0.01, 0.1), 0.0001)),
        column(12, "Особые критерии останова", style = "text-align: center;"),
        column(6, checkboxInput("adaline_stop", "Adaline", F)), column(6, checkboxInput("logistic_stop", "Логистич. регрессия", F)),
        column(12, sliderInput("n", "Число наблюдений", 100, 500, 100, 100)),
        
        column(12, "Класс Red", style = "color: red; text-align: center; font-size: 24px"),
        column(6, sliderInput("mu11", "Мат. ожидание First", 0, 20, 0, 1)), column(6, sliderInput("mu21", "Мат. ожидание Second", 0, 10, 0, 1)),
        column(6, sliderInput("sigma11", "Стандартное отклонение First", 0.1, 1, 0.5, 0.1)), column(6, sliderInput("sigma21", "Стандартное отклонение Second", 0.1, 1, 0.5, 0.1)),
        
        column(12, "Класс Blue", style = "color: blue; text-align: center; font-size: 24px"),
        column(6, sliderInput("mu12", "Мат. ожидание First", 0, 20, 6, 1)), column(6, sliderInput("mu22", "Мат. ожидание Second", 0, 10, 0, 1)),
        column(6, sliderInput("sigma12", "Стандартное отклонение First", 0.1, 1, 0.5, 0.1)), column(6, sliderInput("sigma22", "Стандартное отклонение Second", 0.1, 1, 0.5, 0.1)),
        
        column(12, uiOutput("ui"))
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

adaline.get_w <- function(w, object, class, eta) w - c(eta) * (w %*% object - class) %*% object
hebb.get_w <- function(w, object, class, eta) w + c(eta) * object * class
logistic.get_w <- function(w, object, class, eta) w + c(eta) * object * class * sigmoid(c(w %*% object) * class)

gradient <- function(xl, eta, lambda, rule, loss_function, eps_q, stop_by_margins) {
  l <- nrow(xl)
  n <- ncol(xl)
  w <- matrix(c(runif(n-1, -1/(2*(n-1)), 1/(2*(n-1)))), 1, 3) # Инициализация w случайными значениями
  objects <- xl[,-n]
  classes <- xl[, n]
  q <- sum(sapply(1:l, function(i) loss_function(margin(w, objects[i,], classes[i])))) # Начальная инициализация q
  q_full <- matrix(q, 1, 1)
  cnt <- 0
  while (T) {
    cnt <- cnt + 1 # Счётчик итераций
    margins <- sapply(1:l, function(i) margin(w[cnt,], objects[i,], classes[i]))
    errors <- which(margins < 0)
    
    if (length(errors) == 0 && stop_by_margins == T) break;
    
    if (length(errors) > 0) rand <- sample(errors, 1)
    else rand <- sample(1:l, 1)
    
    eps <- loss_function(margin(w[cnt,], objects[rand,], classes[rand])) # Ошибка алгоритма на объекте
    
    eta <- 1 / (objects[rand,] %*% objects[rand,])^2 # Пересчёт темпа обучения
    if (length(errors) == 0) eta <- eta / 2
    
    # Обновление весов
    w <- rbind(w, rule(w[cnt,], objects[rand,], classes[rand], eta))
    
    # Пересчёт q
    q_prev <- q
    q <- (1 - lambda) * q + lambda * eps
    q_full <- rbind(q_full, q)
    
    if (abs(q_prev - q) / max(q_prev, q) <= eps_q) { print("exit by q"); break; }
    else if (cnt == 20000) { print("exit by cnt"); break; }
    
  }
  w <- cbind(w, q_full)
  w
}

margin <- function(w, object, class) w %*% object * class

adaline <- function(xl, show_iter, show_q, eps_q, stop_by_margins) {
  w <- gradient(xl, 1, 1/6, adaline.get_w, loss.Q, eps_q, stop_by_margins)
  q <- w[,ncol(w)]
  n <- ncol(xl)
  l <- nrow(w)
  if (show_q == T) {
    par(mfrow=c(1, 2))
    plot(q, type = "l", bg = "red", col = "red", main = "График изменения Q", xlab = "Итерации", ylab = "Значения Q")
  }
  colors <- c("blue", "white", "red")
  plot(xl[,1:(n-2)], type="n", asp = 1, main = "Классификатор")
  if (show_iter == T) for (i in 1:(l-1)) abline(a = w[i,3]/w[i,2], b = -w[i,1]/w[i,2], lwd = 1, col = "black")
  abline(a = w[l,3]/w[l,2], b = -w[l,1]/w[l,2], lwd = 3, col = "navy")
  points(xl[,1:(n-2)], pch = 21, col = colors[xl[,n]+2], bg = colors[xl[,n]+2])
  
}

perceptron <- function(xl, show_iter, show_q, eps_q, stop_by_margins) {
  w <- gradient(xl, 1, 1/6, hebb.get_w, loss.L, eps_q, stop_by_margins)
  q <- w[,ncol(w)]
  n <- ncol(xl)
  l <- nrow(w)
  if (show_q == T) {
    par(mfrow=c(1, 2))
    plot(q, type = "l", bg = "red", col = "red", main = "График изменения Q", xlab = "Итерации", ylab = "Значения Q")
  }
  colors <- c("blue", "white", "red")
  plot(xl[,1:(n-2)], type="n", asp = 1, main = "Классификатор")
  if (show_iter == T) for (i in 1:(l-1)) abline(a = w[i,3]/w[i,2], b = -w[i,1]/w[i,2], lwd = 1, col = "black")
  abline(a = w[l,3]/w[l,2], b = -w[l,1]/w[l,2], lwd = 3, col = "gold")
  points(xl[,1:(n-2)], pch = 21, col = colors[xl[,n]+2], bg = colors[xl[,n]+2])
}

logistic <- function(xl, show_iter, show_q, eps_q, stop_by_margins, map) {
  w <- gradient(xl, 1, 1/6, logistic.get_w, loss.Log, eps_q, stop_by_margins)
  q <- w[,ncol(w)]
  n <- ncol(xl)
  l <- nrow(w)
  if (show_q == T) {
    par(mfrow=c(1, 2))
    plot(q, type = "l", bg = "red", col = "red", main = "График изменения Q", xlab = "Итерации", ylab = "Значения Q")
  }
  colors <- c("blue", "white", "red")
  plot(xl[,1:(n-2)], type="n", asp = 1, main = "Классификатор")
  if (show_iter == T) for (i in 1:(l-1)) abline(a = w[i,3]/w[i,2], b = -w[i,1]/w[i,2], lwd = 1, col = "black")
  abline(a = w[l,3]/w[l,2], b = -w[l,1]/w[l,2], lwd = 3, col = "orchid")
  points(xl[,1:(n-2)], pch = 21, col = colors[xl[,n]+2], bg = colors[xl[,n]+2])
}

svm <- function(xl, c = 1) {
  n <- ncol(xl)
  colnames(xl) <- c("First", "Second", "Const", "Class")
  
  objects <- xl[,1:(n-2)]
  classes <- xl[,n]
  svp <- ksvm(x = objects,y = classes, type="C-svc", kernel = "vanilladot", C = c)
  print(svp)
  plot(svp, data = objects)
}

compare <- function(xl, show_iter, eps_q, stop_by_margins) {
  n <- ncol(xl)
  w1 <- gradient(xl, 1, 1/6, adaline.get_w, loss.Q, eps_q, stop_by_margins["Adaline"])
  w2 <- gradient(xl, 1, 1/6, hebb.get_w, loss.L, eps_q, stop_by_margins["Hebb"])
  w3 <- gradient(xl, 1, 1/6, logistic.get_w, loss.Log, eps_q, stop_by_margins["Logistic"])
  l1 <- nrow(w1)
  l2 <- nrow(w2)
  l3 <- nrow(w3)
  colors <- c("blue", "white", "red")
  plot(xl[,1:(n-2)], type="n", asp = 1, main = "Классификаторы")
  if (show_iter == T) {
    for (i in 1:(l1-1)) abline(a = w1[i,3]/w1[i,2], b = -w1[i,1]/w1[i,2], lwd = 1, col = "black")
    for (i in 1:(l2-1)) abline(a = w2[i,3]/w2[i,2], b = -w2[i,1]/w2[i,2], lwd = 1, col = "black")
    for (i in 1:(l3-1)) abline(a = w3[i,3]/w3[i,2], b = -w3[i,1]/w3[i,2], lwd = 1, col = "black")
  }
  abline(a = w1[l1,3]/w1[l1,2], b = -w1[l1,1]/w1[l1,2], lwd = 3, col = "navy")
  abline(a = w2[l2,3]/w2[l2,2], b = -w2[l2,1]/w2[l2,2], lwd = 3, col = "gold")
  abline(a = w3[l3,3]/w3[l3,2], b = -w3[l3,1]/w3[l3,2], lwd = 3, col = "orchid")
  points(xl[,1:(n-2)], pch = 21, col = colors[xl[,n]+2], bg = colors[xl[,n]+2])
  legend("bottomright", c("ADALINE", "Персептрон", "Логистич. регрессия"), pch = c("l","l","l"), col = c("navy", "gold", "orchid"))
}

server <- function(input, output) {
  output$ui <- renderUI({
    if (input$classifiers == 3) sliderInput("c", "C", 1, 10, 1, 1)
    else column(12)
  })
  
  output$plot = renderPlot({
    n <- input$n
    xl11 <- rnorm(n/2, input$mu11, input$sigma11)
    xl12 <- rnorm(n/2, input$mu21, input$sigma21)
    xl21 <- rnorm(n/2, input$mu12, input$sigma12)
    xl22 <- rnorm(n/2, input$mu22, input$sigma22)
    tmp1 <- cbind(xl11, xl12)
    tmp2 <- cbind(xl21, xl22)
    xl <- rbind(cbind(tmp1, 1), cbind(tmp2, -1))
    colnames(xl) <- c("First", "Second", "Class")
    
    n_xl <- normalize(xl)
    n_xl <- add_col_for_w0(n_xl)
    
    show_iter <- input$show_iter
    stop_by_margins <- c("Adaline" = input$adaline_stop, "Hebb" = T, "Logistic" = input$logistic_stop)
    show_q <- input$show_q
    eps_q <- as.numeric(input$eps_q)
    
    if (input$classifiers == 0) adaline(n_xl, show_iter, show_q, eps_q, stop_by_margins["Adaline"])
    else if (input$classifiers == 1) perceptron(n_xl, show_iter, show_q, eps_q, stop_by_margins["Hebb"])
    else if (input$classifiers == 2) logistic(n_xl, show_iter, show_q, eps_q, stop_by_margins["Logistic"])
    else if (input$classifiers == 3) { 
      if (!is.null(input$c)) c <- input$c
      else c <- 1
      svm(n_xl, c) 
    }
    else if (input$classifiers == 4) compare(n_xl, show_iter, eps_q, stop_by_margins)
  })
  
}

shinyApp(ui, server)
