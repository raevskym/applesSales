library(shiny)
shinyUI(pageWithSidebar(
  headerPanel('Impact of Apple Ad Expenses on iMac Sales'),
  sidebarPanel(
  sliderInput('startQuarter', 'Choose the starting Quarter (1: Q1-06, 40: Q4-15)', min = 1, max = 40, value = 1),
    sliderInput('endQuarter', 'Choose the ending Quarter (1: Q1-06, 40: Q4-15)', min = 1, max = 40, value = 40),
    radioButtons('modelType', "Choose Individual-level Model", choices = c('Weibull' = 2, 'Exponential' = 1)),
    checkboxInput('mixture', "Add Gamma Distribution (Mixture Model)"),
    checkboxGroupInput('covariates', "More Covariates", choices = c('New iMac Releases' = 'ni', 'Steve Jobs Death' = 'jd'), selected = c('ni','jd'))
    ),
  mainPanel(
    plotOutput('taxiPlot')
  )
))