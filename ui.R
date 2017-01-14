library(shiny)

shinyUI(fluidPage(
  titlePanel("Projecting impact of Apple ad expenditures on iMac sales using probability models"),
  fluidRow(
    column(12,
           "This project employs a modelling aproach I picked up while I was", withTags({a(href="https://www.linkedin.com/in/peterfader", "Peter Fader")}), "'s student and research assistant. Fader pioneered an approach for modeling customer retention that is fundamentally different from traditional curve fitting / regression methods. Combining probability theory with non-linear optimization allows us to model customer behavior in a much more nuanced matter. This customer-centric modeling approach is rapidly gaining traction in industry, and is the fundamental building block behind statistical marketting consulting startup", withTags({a(href="http://www.zodiacmetrics.com/", "Zodiac Metrics")}),"."
    ),
    column(12, 
          "
          "
           ),
    column(12, 
           "
           "
    ),
    column(12,
           "A full pdf report of this investigation and the R code are both available on my website. This app was built with the Shiny R library."
          ),
  sidebarLayout(position = "right",
    sidebarPanel(
      radioButtons('modelType', "Choose Individual-level Model", choices = c('Weibull' = 2, 'Exponential' = 1)),
      checkboxInput('mixture', "Add Gamma Distribution (Mixture Model)"),
      checkboxGroupInput('covariates', "More Covariates", choices = c('New iMac Releases' = 'ni', 'Steve Jobs Death' = 'jd'), selected = c('ni','jd')),
      sliderInput('startQuarter', 'Choose Date Range (1: Q1 2006, 43: Q3 2016)', min = 1, max = 43, value = c(1,43)),
      sliderInput('num', 'Input 2016 Ad Expenditures (in $bn)', min = 1, max = 2.6, value = 1.8),
      sliderInput('benchmark', "Compare to regression/curve-fitting (pick # polynomials, default linear)", min = 1, max = 20, value = 1)
    ),
    mainPanel(
      plotOutput('plot'),
      tableOutput('table'),
      textOutput('description')
      )
  )
)))