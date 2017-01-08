library(shiny)

shinyUI(fluidPage(
  titlePanel("Impact of Apple Ad Expenses on iMac Sales"),
  sidebarLayout(position = "right",
    sidebarPanel(
      radioButtons('modelType', "Choose Individual-level Model", choices = c('Weibull' = 2, 'Exponential' = 1)),
      checkboxInput('mixture', "Add Gamma Distribution (Mixture Model)"),
      checkboxGroupInput('covariates', "More Covariates", choices = c('New iMac Releases' = 'ni', 'Steve Jobs Death' = 'jd'), selected = c('ni','jd')),
      sliderInput('startQuarter', 'Choose the starting Quarter (1: Q1-06, 44: Q4-16)', min = 1, max = 44, value = 1),
      sliderInput('endQuarter', 'Choose the ending Quarter (1: Q1-06, 44: Q4-16)', min = 1, max = 44, value = 44)
    ),
    mainPanel(
      plotOutput('plot'),
      tableOutput('table')
      )
  )
))