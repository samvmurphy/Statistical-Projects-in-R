# ===============================================
# STAT133 Project 02 - Shiny Apps
# ===============================================
# Title: Retirement Withdrawl Simulator
# Description: Shiny app designed to simulate 
# various scenarios for the withdrawl of funds from 
# a retirement account, under different contexts of 
# inflation and rates of return
# Author: Andreas Maass
# Due Date: 11/05/2021


# ===============================================
# Required packages
# ===============================================
library(tidyverse)
library(shiny)

# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Retirement Withdrawl Simulator"),
  fluidRow(
    
    # Inputs for initial portfolio, retirement age, and withdrawal rate
    column(3,
           h4('General Settings'),
           numericInput(inputId = "p", 
                      label = "Initial Portfolio Amount", 
                       value = 1000000,
                       min = 1),
           numericInput(inputId = "age", 
                       label = "Retirement Age",
                       value = 60,
                       min = 1),
           numericInput(inputId = "wd", 
                       label = "Withdrawl Rate (%)", 
                       value = 4,
                       min = 1)
    ),
    
    # Inputs for mean and standard deviation of annual return rates
    column(3,
           h4('Growth Settings'),
           numericInput(inputId = "retr", 
                        label = "Average Annual Return (%)", 
                        value = 10,
                        min = 1),
           numericInput(inputId = "volr", 
                        label = "Average Return Volatility (%)", 
                        value = 18,
                        min = 1)
    ),
    
    # Inputs for mean and standard deviation of annual inflation rates
    column(3,
           h4('Inflation Settings'),
           numericInput(inputId = "reti", 
                        label = "Average Annual Inflation Rate (%)", 
                        value = 3,
                        min = 1),
           numericInput(inputId = "voli", 
                        label = "Average Inflation Volitility (%)", 
                        value = 3.5,
                        min = 1)
    ),
    
    # Inputs for number of simulations, and random seed
    column(3,
           h4('Simlulation Settings'),
           numericInput(inputId = "sims", 
                        label = "Number of Simulations", 
                        value = 50,
                        min = 1),
           numericInput(inputId = "seed", 
                        label = "Random Seed", 
                        value = 12345),
           numericInput(inputId = 'Agg',
                        label = 'Aggregation Count',
                        value = 1)
    )
  ),
  
  hr(),
  h4('Simulated Timelines'),
  plotOutput('plot'),
  
  hr(),
  h4('Output Statistics'),
  verbatimTextOutput('table')
)



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {

  # you may need to create reactive objects
  # (e.g. data frame to be used for graphing purposes)
  dat <- reactive({
    set.seed(input$seed)
    start = input$p # in dollars
    age = 100 - input$age # in years
    wd = input$wd/100 # in dollars
    amount_every_year = start * wd # in dollars
    sims = input$sims # count
    retr = input$retr/100
    volr = input$volr/100
    reti = input$reti/100
    voli = input$voli/100
    df = data.frame()
    for(j in 1:sims){
      new_lis = c()
      for(i in 1:age){
        var_inf = rnorm(1, mean = reti, sd = voli)
        var_ret = rnorm(1, mean = retr, sd = volr)
        if(i == 1){
          new_lis = c(new_lis, start - (start * wd))
        }
        else{
          new_lis = c(new_lis, new_lis[i-1] * (1+var_ret) - amount_every_year * (1+var_inf))
        }
      }
      if(j == 1){
        df = new_lis
      }
      else{
        df = cbind(df, new_lis)
      }
    }
    names = c()
    for(p in 1:sims){
      names = c(names, paste('year', p, sep = ' '))
    }
    colnames(df) = c(names)
    library(reshape2)
    data_long1 = melt(df, id = 'x')
    data.frame(data_long1)
    }
  )
  

  # code for graph
  # (e.g. reactive data frame used for graphing purposes)
  output$plot <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = dat(), aes(x = Var1, y = value, color = Var2)) +
      geom_line(size = .5,show.legend = FALSE)+
      geom_hline(yintercept = 0, color = 'red')+
      xlab('Years after retirement')+
      ylab('Portfolio balance')
  })
  
  
  # code for statistics
  output$table <- renderPrint({
    # replace the code below with your code!!!
    summary(dat())
  })
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

