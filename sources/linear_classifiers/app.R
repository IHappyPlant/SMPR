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
      plotOutput("plot"),
      textOutput("error")
    )
  )
)

normalize <- function(xl) {
  for (i in 1:(ncol(xl)-1)) xl[,i] <- (xl[,i] - mean(xl[,i])) / sd(xl[,i])
  xl
}

add_col_for_w0 <- function(xl) cbind(xl[,1:(ncol(xl)-1)], -1, xl[, ncol(xl)])

loss.S <- function(m) 2 / (1 + exp(m))
loss.S.d <- function(m) (-2 * exp(m)) / (1 + exp(m))^2
loss.Q <- function(m) (1-m)^2
loss.Q.d <- function(m) 2*m-2

adaline.get_w <- function(w, object, class, eta)  w - c(eta) * (w %*% object - class) %*% object

gradient <- function(xl, eta, lambda, method, loss_function) {
  l <- nrow(xl)
  n <- ncol(xl)
  w <- runif(n-1, -1/(2*n), 1/(2*n))
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
    w <- method(w, objects[rand,], classes[rand], eta)
    q_prev <- q
    q <- (1 - lambda) * q + lambda * eps
    if (abs(q_prev - q) <= 1e-5) break
    
    else if (cnt == 30000) { print("exit by cnt"); break; }
  }
  w
}

sg.ADALINE <- function(xl, eta = 1, lambda = 1/6)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)
  iterCount <- 0
  ## initialize Q
  Q <- 0
  for (i in 1:l)
  {
    ## calculate the scalar product <w,x>
    wx <- sum(w * xl[i, 1:n])
    ## calculate a margin
    margin <- wx * xl[i, n + 1]
    Q <- Q + loss.Q(margin)
  }
  repeat
  {
    ## calculate the margins for all objects of the
    margins <- array(dim = l)
    for (i in 1:l)
    {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      margins[i] <- crossprod(w, xi) * yi
    }
    ## select the error objects
    errorIndexes <- which(margins <= 0)
    if (length(errorIndexes) > 0)
    {
      # select the random index from the errors
      i <- sample(errorIndexes, 1)
      iterCount <- iterCount + 1
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      ## calculate the scalar product <w,xi>
      wx <- sum(w * xi)
      ## make a gradient step
      margin <- wx * yi
      ## calculate an error
      ex <- loss.Q(margin)
      eta <- 1 / sqrt(sum(xi * xi))
      w <- w - eta * (wx - yi) * xi
      ## Calculate a new Q
      Qprev <- Q
      Q <- (1 - lambda) * Q + lambda * ex
    }
    else
    {
      break
    }
  }
  return (w)
}

adaline <- function(xl, new_xl) {
  w <- gradient(new_xl, 1, 1/6, adaline.get_w, loss.Q)
  n <- ncol(xl)
  l <- nrow(xl)
  colors <- c("1" <- "red", "2" = "blue")
  plot(xl[,1:(n-1)], pch = 21, col = colors[xl[,n]], bg = colors[xl[,n]], asp = 1)
  x <- seq(-20, 20, length.out = 100)
  y <- seq(-20, 20, length.out = 100)
  z <- outer(x, y, function(x, y) w[1] * x + w[2] * y + w[3])
  contour(x, y, z, levels = 0, add = T, drawlabels = F, lwd = 3, col = "navy")
}

server <- function(input, output) {
  output$plot = renderPlot({
    output$error = renderText("")
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
  })
  
}

shinyApp(ui, server)
