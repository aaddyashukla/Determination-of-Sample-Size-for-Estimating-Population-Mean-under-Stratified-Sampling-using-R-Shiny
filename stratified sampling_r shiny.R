install.packages("shiny")
library(shiny)

ui <- fluidPage(
  
  titlePanel("Sampling–2 : Stratified Sampling Allocation (R Shiny)"),
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput("L", "Number of Strata (L):", value = 3, min = 1),
      numericInput("Z", "Z value (e.g. 1.96 for 95% CI):", value = 1.96),
      numericInput("B", "Allowable Sampling Bias (B):", value = 2),
      
      tags$hr(),
      h4("Enter Strata Details"),
      
      tableInput <- tableOutput("inputTable"),
      actionButton("calculate", "Calculate Sample Size")
    ),
    
    mainPanel(
      h4("Sample Size Allocation Results"),
      tableOutput("resultTable")
    )
  )
)

server <- function(input, output, session) {
  
  # Create default input table
  strata_data <- reactive({
    data.frame(
      Stratum = paste0("S", 1:input$L),
      Nh = rep(100, input$L),
      Sh = rep(10, input$L),
      Cost = rep(1, input$L),
      Time = rep(1, input$L)
    )
  })
  
  output$inputTable <- renderTable({
    strata_data()
  })
  
  observeEvent(input$calculate, {
    
    df <- strata_data()
    
    N <- sum(df$Nh)
    Wh <- df$Nh / N
    
    # Total sample size formula
    n <- (input$Z^2 * (sum(Wh * df$Sh))^2) / (input$B^2)
    n <- ceiling(n)
    
    # Proportional Allocation
    df$n_prop <- round(n * df$Nh / N)
    
    # Neyman Allocation
    df$n_neyman <- round(n * (df$Nh * df$Sh) / sum(df$Nh * df$Sh))
    
    # Optimised Allocation
    df$n_optimised <- round(
      n * ((df$Nh * df$Sh) / sqrt(df$Cost * df$Time)) /
        sum((df$Nh * df$Sh) / sqrt(df$Cost * df$Time))
    )
    
    df$Total_Sample_Size <- n
    
    output$resultTable <- renderTable({
      df
    })
  })
}

shinyApp(ui = ui, server = server)
