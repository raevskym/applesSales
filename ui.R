library(shiny)

shinyUI(fluidPage(
  titlePanel("Impact of Apple Ad Expenditures on iMac Sales"),
  sidebarLayout(position = "right",
    sidebarPanel(
      radioButtons('modelType', "Choose Individual-level Model", choices = c('Weibull' = 2, 'Exponential' = 1)),
      checkboxGroupInput('covariates', "More Covariates", choices = c('New iMac Releases' = 'ni', 'Steve Jobs Death' = 'jd'), selected = c('ni','jd')),
      sliderInput('startQuarter', 'Choose Date Range (1: Q1 2006, 43: Q3 2016)', min = 1, max = 43, value = c(1,43)),
      sliderInput('num', 'Input 2016 Ad Expenditures (in $bn)', min = 1, max = 2.6, value = 1.8),
      strong("More Options"),
      checkboxInput('mixture', "Add Gamma Distribution (Mixture Model)"),
      checkboxInput('benchmark', "Add Linear Regression for Comparison")
    ),
    mainPanel(
      plotOutput('plot'),
      tableOutput('table')
      )
  ),
  fluidRow(
    column(12,
      textOutput('description')
))))