library(shiny)

ui <- fluidPage(
  titlePanel("Линии уровня"),
  
  sidebarLayout(
    sidebarPanel(
      
      "Ковариационная матрица",
      fluidRow(
        column(6, sliderInput("a11", "a11", 1, 10, 1)),
        column(6, sliderInput("a12", "a12, a21", 1, 10, 1)),
        column(6, NULL),
        column(6, sliderInput("a22", "a22", 1, 10, 1))
      )
        

    ),
    
    mainPanel(
      plotOutput(outputId = "plot", height = "600px")
    )
  )
)

density <- function(x, mu, sigma) {
  
}

server <- function(input, output, session) {
  output$plot <- renderPlot(
    cov_matrix <- matrix(c(input$a11, input$a12, input$a12, input$a22), 2, 2)
    
  )
}

shinyApp(ui = ui, server = server)
