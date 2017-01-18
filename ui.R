library(shiny)

shinyUI(fluidPage(
  titlePanel("Projecting impact of Apple ad expenditures on iMac sales using probability models"),
  fluidRow(
    column(12,
           "This project employs a modelling technique I picked up while I was", withTags({a(href="https://www.linkedin.com/in/peterfader", "Peter Fader")}), "'s student and research assistant. Fader pioneered an approach for modeling customer retention that is fundamentally more accurate than traditional curve-fitting / regression methods. Combining probability theory with non-linear optimization allows us to model customer behavior in a much more nuanced matter. This customer-centric modeling approach is rapidly gaining traction in industry, and is the fundamental building block behind statistical marketing consulting startup", withTags({a(href="http://www.zodiacmetrics.com/", "Zodiac Metrics")}),"."
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
      radioButtons('modelType', "Choose Individual-level Model", choices = c('Exponential' = 1, 'Weibull' = 2)),
      checkboxGroupInput('covariates', "More Covariates", choices = c('Model X Release' = 'mx', 'Model 3 Release' = 'm3')),
      sliderInput('startQuarter', 'Choose Date Range (1: Q3 2012, 16: Q2 2016)', min = 1, max = 16, value = c(1,16)),
      sliderInput('benchmark', "Compare to regression/curve-fitting (pick # polynomials, default linear)", min = 1, max = 10, value = 1)
    ),
    mainPanel(
      plotOutput('plot'),
      tableOutput('table'),
      textOutput('description')
      )
  )
)))