# shiny-functions.R
#
# Show a function plot in Shiny.
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Function Plot"),
  
  sidebarLayout(
    sidebarPanel(
      # =======================================================================
      # define parameters here
      sliderInput(
        "a",
        label = "a",
        min = -30,
        max = 30,
        value = -15,
        step = 0.1
      ),
      sliderInput(
        "b",
        label = "b",
        min = 0.01,
        max = 50,
        value = 20,
        step = 0.1
      ),
      sliderInput(
        "c",
        label = "c",
        min = -10,
        max = 10,
        value = 3,
        step = 0.1
      ),
      sliderInput(
        "d",
        label = "d",
        min = -50,
        max = 0,
        value = -3.5,
        step = 0.1
      ),
      # =======================================================================
      
      # define coordinates
      checkboxInput("useConstraints", "use x/y constraints", value = FALSE),
      numericInput("xmin", label = "x minimum", value = 0),
      numericInput("xmax", label = "x maximum", value = 15),
      numericInput("ymin", label = "y minimum", value = 0),
      numericInput("ymax", label = "y maximum", value = 10)
    ),
    
    mainPanel(
      plotOutput("plot", height = "600px")
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({

    fun <- function(x) {
      return(
        # =======================================================================
        # define function here
        input$a * log10(log10(log10(input$b * x + input$c))) + input$d
        # =======================================================================
      )
    }

    xmin = ifelse(input$useConstraints, input$xmin, 0)
    xmax = ifelse(input$useConstraints, input$xmax, 15)

    p = ggplot(
      data.frame(x = c(xmin, xmax)), aes(x)
    ) +
      stat_function(fun = fun, geom = "line") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
      
    
    if (input$useConstraints) {
      return(p + ylim(input$ymin, input$ymax))
    } else {
      return(p)
    }

  })
  

}

shinyApp(ui = ui, server = server)
