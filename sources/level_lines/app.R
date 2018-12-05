library(shiny)

ui <- fluidPage(
  titlePanel("Линии уровня"),
  
  sidebarLayout(
    sidebarPanel(
      
      "Ковариационная матрица",
      fluidRow(
        column(6, textInput("a11", "a11", 1)),
        column(6, textInput("a12", "a12, a21", 0)),
        column(6, NULL),
        column(6, textInput("a22", "a22", 1))
      ),
        
      "Мат. ожидание",
      fluidRow(
        column(6, textInput("nu1", "nu1", 0)),
        column(6, textInput("nu2", "nu2", 0))
      ),
      
      fluidRow(
        column(12, sliderInput("step", "Шаг плотности", 0.001, 0.1, 0.01, 0.01))
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "plot", height = "600px"),
      textOutput("err")
    )
  )
)

point <- function(x, mu, sigma) (1 / (2*pi * sqrt(det(sigma)))) * exp(-0.5*((x-mu) %*% solve(sigma) %*% t(x-mu)))

server <- function(input, output, session) {
  output$plot <- renderPlot({
    sigma <- matrix(c(as.numeric(input$a11), as.numeric(input$a12), as.numeric(input$a12), as.numeric(input$a22)), 2, 2)
    mu <- matrix(c(as.numeric(input$nu1), as.numeric(input$nu2)), 1, 2)
    
    if (is.nan(det(sigma))) {
      output$err <- renderText("Заполните матрицу ковариации")
      return()
    }
    else if (det(sigma) <= 0) {
      output$err <- renderText("Определитель матрицы <= 0")
      return ()
    }
    if(is.na(mu[1,1]) || is.na(mu[1,2])) {
      output$err <- renderText("Заполните вектор мат. ожиданий")
      return ()
    }
    output$err <- renderText("")
    
    max_x <- sigma[1, 1] + 2
    min_x <- -sigma[1, 1] - 2
    max_y <- sigma[2, 2] + 2
    min_y <- -sigma[2, 2] - 2
    
    x <- seq(min_x, max_x, length.out = 100)
    y <- seq(min_y, max_y, length.out = 100)
    
    z <- matrix(NA, length(x), length(y))
    for (i in 1:length(x))
      for (j in 1:length(y))
        z[i, j] <- point(matrix(c(x[i], y[j]), 1, 2), mu, sigma)
    
    add = F
    for (i in seq(0, 0.2, input$step)) {
      contour(x, y, z, levels = i, add = add, asp = 1)
      add = T
    }
  }
  )
}

shinyApp(ui = ui, server = server)
