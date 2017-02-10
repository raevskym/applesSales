library(shiny)

#Data Source, m3 sales: 
#Data Source, oil: http://www.macrotrends.net/1369/crude-oil-price-history-chart

salesmatrix <- read.csv("teslasales.csv")[1:16,]
range.ex <- function(x, fac=1.2) { xrg <- range(x);  m <- mean(xrg);  (xrg - m)*fac + m }

TeslaPlot <- function(startQuarter, modelType, covariates, benchmark) {
#total number of Model S sales, N = 129377
  
        if (modelType == 1) {
          #Exponential Model
          eCov <- function(x) { ## function to optimize
            x1 <- x[1] #lambda
            x2 <- x[2] # beta
            salesmatrix$exb=exp(salesmatrix[,7]*x2)
            salesmatrix$A=cumsum(salesmatrix$exb)
            salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
            salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
            salesmatrix$dP[1]=salesmatrix$Pt[1]
            salesmatrix$LL <- c(salesmatrix$sales*log(salesmatrix$dP))
            -sum(salesmatrix$LL)
          }
          initial_guess=c(2.5, -4)
          res <- optim(initial_guess, eCov)
          x1 <- res$par[1] #lambda
          x2 <- res$par[2] # beta
          salesmatrix$exb=exp(salesmatrix[,7]*x2)
          salesmatrix$A=cumsum(salesmatrix$exb)
          salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
          salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
          salesmatrix$dP[1]=salesmatrix$Pt[1]
          print(salesmatrix$dP[1:16] * 129377)
          salesmatrix$LL <- c(salesmatrix$sales*log(salesmatrix$dP))
          LL <- -sum(salesmatrix$LL)
          salesmatrix$exp <- salesmatrix$dP * 129377
          MAPE <- mean(abs((salesmatrix$sales[1:16]-salesmatrix$exp[1:16])/salesmatrix$sales[1:16]))
        
          z <- cbind(lambda = x1, b_ads = x2, LL = -LL, MAPE = MAPE)
          rownames(z) <- "parameters"
        }
  
              
  #Exponential, iMac Covar
            if (modelType == 1 && length(covariates) == 1 && covariates[1] == 'mx') {
              e2Cov <- function(x) { ## function to optimize
                x1 <- x[1] #lambda
                x2 <- x[2] # beta
                x4 <- x[3] #b_iMac
                salesmatrix$exb=exp(salesmatrix$adv..bn.*x2 + salesmatrix$iMac*x4)
                salesmatrix$A=cumsum(salesmatrix$exb)
                salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
                salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
                salesmatrix$dP[1]=salesmatrix$Pt[1]
                salesmatrix$LL <- c(salesmatrix$sales*log(salesmatrix$dP))
                -sum(salesmatrix$LL)
              }
                initial_guess=c(.005, 1.5, 0.5)
                x <- optim(initial_guess, e2Cov)$par
                x1 <- x[1] # lambda
                x2 <- x[2] # beta
                x4 <- x[3] # b_iMac
                salesmatrix$exb=exp(salesmatrix$adv..bn.*x2 + salesmatrix$iMac*x4)
                salesmatrix$A=cumsum(salesmatrix$exb)
                salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
                salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
                salesmatrix$dP[1]=salesmatrix$Pt[1]
                salesmatrix$LL <- c(salesmatrix$sales*log(salesmatrix$dP))
                LL <- -sum(salesmatrix$LL)
                salesmatrix$exp <- salesmatrix$dP * 129377
                MAPE <- mean(abs((salesmatrix$sales[1:16]-salesmatrix$exp[1:16])/salesmatrix$sales[1:16]))
               
                 
                z <- cbind(lambda = x1, b_ads = x2, b_iMac = x4, LL = -LL, MAPE = MAPE)
                rownames(z) <- "parameters"
                
                
                }
      
  #Exponential, Steve Covar
  if (modelType == 1 && length(covariates) == 1 && covariates[1] == 'm3') {
    e2CovJobs <- function(x) { ## function to optimize
      x1 <- x[1] #lambda
      x2 <- x[2] # beta
      x5 <- x[3] #b_steve
      x6 <- x[4] #gamma
      x7 <- x[5] #delta
      salesmatrix$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(salesmatrix$t-24)))[24:44])
      salesmatrix$exb=exp(salesmatrix$adv..bn.*x2 + salesmatrix$sJobs*x5)
      salesmatrix$A=cumsum(salesmatrix$exb)
      salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
      salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
      salesmatrix$dP[1]=salesmatrix$Pt[1]
      salesmatrix$LL <- c(salesmatrix$sales*log(salesmatrix$dP))
      LL <- -sum(salesmatrix$LL)
    }
    initial_guess=c(0.005, 1.5, 0.5, 0.2, 14)
    x <- optim(initial_guess, e2CovJobs)$par
    x1 <- x[1] #lambda
    x2 <- x[2] # beta
    x5 <- x[3] #b_steve
    x6 <- x[4] #gamma
    x7 <- x[5] #delta
    salesmatrix$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(salesmatrix$t-24)))[24:44])
    salesmatrix$exb=exp(salesmatrix$adv..bn.*x2 + salesmatrix$sJobs*x5)
    salesmatrix$A=cumsum(salesmatrix$exb)
    salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
    salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
    salesmatrix$dP[1]=salesmatrix$Pt[1]
    salesmatrix$LL <- c(salesmatrix$sales*log(salesmatrix$dP))
    LL <- -sum(salesmatrix$LL)
    salesmatrix$exp <- salesmatrix$dP * 129377
    MAPE <- mean(abs((salesmatrix$sales[1:16]-salesmatrix$exp[1:16])/salesmatrix$sales[1:16]))
 
    
    z <- cbind(lambda = x1, b_ads = x2, b_jobs = x5, gamma = x6, delta = x7, LL = -LL, MAPE = MAPE)
    rownames(z) <- "parameters"
    
    
    }
  
  #Exponential, Both Covar
  if (modelType == 1 && length(covariates) == 2) {
      e3Cov <- function(x) { ## function to optimize
        x1 <- x[1] #lambda
        x2 <- x[2] # beta
        x4 <- x[3] #b_iMac
        x5 <- x[4] #b_steve
        x6 <- x[5] #gamma
        x7 <- x[6] #delta
        salesmatrix$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(salesmatrix$t-24)))[24:44])
        salesmatrix$exb=exp(salesmatrix$adv..bn.*x2 + salesmatrix$iMac*x4 + salesmatrix$sJobs*x5)
        salesmatrix$A=cumsum(salesmatrix$exb)
        salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
        salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
        salesmatrix$dP[1]=salesmatrix$Pt[1]
        salesmatrix$LL <- c((salesmatrix$sales[1:16])*(log(salesmatrix$dP)[1:16]), (129377-sum(salesmatrix[1:16,5]))*log(1-salesmatrix$Pt[16]), NA, NA, NA)
        -sum(salesmatrix$LL)
      }
      initial_guess=c(0.005, 1.5, 0.5, 0.5, 0.2, 14)
      optim(initial_guess, e3Cov)
      x1 <- optim(initial_guess, e3Cov)$par[1] #lamda
      x2 <- optim(initial_guess, e3Cov)$par[2] #b_ads
      x4 <- optim(initial_guess, e3Cov)$par[3] #b_iMac
      x5 <- optim(initial_guess, e3Cov)$par[4] #b_steve
      x6 <- optim(initial_guess, e3Cov)$par[5] #gamma
      x7 <- optim(initial_guess, e3Cov)$par[6] #delta
      salesmatrix$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(salesmatrix$t-24)))[24:44])
      salesmatrix$exb=exp(salesmatrix$adv..bn.*x2 + salesmatrix$iMac*x4 + salesmatrix$sJobs*x5)
      salesmatrix$A=cumsum(salesmatrix$exb)
      salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
      salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
      salesmatrix$dP[1]=salesmatrix$Pt[1]
      salesmatrix$LL <- c(salesmatrix$sales*log(salesmatrix$dP))
      LL <- -sum(salesmatrix$LL)
      salesmatrix$exp <- salesmatrix$dP * 129377
      MAPE <- mean(abs((salesmatrix$sales[1:16]-salesmatrix$exp[1:16])/salesmatrix$sales[1:16]))
      
      z <- cbind(lambda = x1, b_ads = x2, b_iMac = x4, b_jobs = x5, gamma = x6, delta = x7, LL = -LL, MAPE = MAPE)
      rownames(z) <- "parameters"
      
      
      }

  #Weibull
        if (modelType == 2 && length(covariates) == 0) {
          wCov <- function(x) { ## function to optimize
            x1 <- x[1] #lambda
            x2 <- x[2] #beta
            x3 <- x[3] #c
            salesmatrix$exb=exp(salesmatrix[,7]*x2)
            salesmatrix$A=cumsum((salesmatrix$t^x3)*salesmatrix$exb)
            salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
            salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
            salesmatrix$dP[1]=salesmatrix$Pt[1]
            salesmatrix$LL <- c(salesmatrix$sales*log(salesmatrix$dP))
            -sum(salesmatrix$LL)
          }
          initial_guess=c(.001, -2, 2)
          x <- optim(initial_guess, wCov)$par
          x1 <- x[1] #lambda
          x2 <- x[2] # beta
          x3 <- x[3] # c
          salesmatrix$exb=exp(salesmatrix[,7]*x2)
          salesmatrix$A=cumsum((salesmatrix$t^x3)*salesmatrix$exb)
          salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
          salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
          salesmatrix$dP[1]=salesmatrix$Pt[1]
          salesmatrix$LL <- c(salesmatrix$sales*log(salesmatrix$dP))
          LL <- -sum(salesmatrix$LL)
          salesmatrix$exp <- salesmatrix$dP * 129377
          MAPE <- mean(abs((salesmatrix$sales[1:16]-salesmatrix$exp[1:16])/salesmatrix$sales[1:16]))
         
          z <- cbind(lambda = x1, b_ads = x2, c = x3, LL = -LL, MAPE = MAPE)
          rownames(z) <- "parameters"
          
          
        }
  
  #Weibull, Model X covar
  if (modelType == 2 && length(covariates) == 1 && covariates[1] == 'mx') {
    w2CovJobs <- function(x) { ## function to optimize
      x1 <- x[1] #lambda
      x2 <- x[2] # beta
      x3 <- x[3] # c
      x5 <- x[4] #b_mx
      x6 <- x[5] #gamma
      x7 <- x[6] #delta
      salesmatrix$mx <- c(array(0, c(1,12)), 1-x6*(1-exp(-x7*abs(salesmatrix$t-13)))[13:16])
      salesmatrix$exb=exp(salesmatrix[,7]*x2 + salesmatrix$mx*x5)
      salesmatrix$A=cumsum((salesmatrix$t^x3)*salesmatrix$exb)
      salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
      salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
      salesmatrix$dP[1]=salesmatrix$Pt[1]
      salesmatrix$LL <- c(salesmatrix$sales*log(salesmatrix$dP))
      -sum(salesmatrix$LL)
    }
    initial_guess=c(.01, -.5, 2, 0.3, -83, .03)
    optim(initial_guess, w2CovJobs)
    x1 <- optim(initial_guess, w2CovJobs)$par[1] #lamda
    x2 <- optim(initial_guess, w2CovJobs)$par[2] #b_ads
    x3 <- optim(initial_guess, w2CovJobs)$par[3] #c
    x5 <- optim(initial_guess, w2CovJobs)$par[4] #b_steve
    x6 <- optim(initial_guess, w2CovJobs)$par[5] #gamma
    x7 <- optim(initial_guess, w2CovJobs)$par[6] #delta
    salesmatrix$mx <- c(array(0, c(1,12)), 1-x6*(1-exp(-x7*abs(salesmatrix$t-13)))[13:16])
    salesmatrix$exb=exp(salesmatrix[,7]*x2 + salesmatrix$mx*x5)
    salesmatrix$A=cumsum(salesmatrix$t^x3*salesmatrix$exb)
    salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
    salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
    salesmatrix$dP[1]=salesmatrix$Pt[1]
    salesmatrix$LL <- c(salesmatrix$sales*log(salesmatrix$dP))
    LL <- -sum(salesmatrix$LL)
    salesmatrix$exp <- salesmatrix$dP * 129377
    MAPE <- mean(abs((salesmatrix$sales[1:16]-salesmatrix$exp[1:16])/salesmatrix$sales[1:16]))
    
    
    z <- cbind(lambda = x1, b_ads = x2, c = x3, b_jobs = x5, gamma = x6, delta = x7, LL = -LL, MAPE = MAPE)
    rownames(z) <- "parameters"
    
    
  }
  
        #Weibull, Model 3 Covar
        if (modelType == 2 && length(covariates) == 1 && covariates[1] == 'm3') {
          w2Cov <- function(x) { ## function to optimize
            x1 <- x[1] #lambda
            x2 <- x[2] # beta
            x3 <- x[3] # c
            x4 <- x[4] #b_m3
            salesmatrix$exb=exp(salesmatrix$adv..bn.*x2 + salesmatrix$iMac*x4)
            salesmatrix$A=cumsum(salesmatrix$t^x3)
            salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
            salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
            salesmatrix$dP[1]=salesmatrix$Pt[1]
            salesmatrix$LL <- salesmatrix$sales*log(salesmatrix$dP)
            -sum(salesmatrix$LL)
          }
          initial_guess=c(0.005, -.5, 1.5, 0.5)
          x <- optim(initial_guess, w2Cov)$par
          x1 <- x[1] #lamda
          x2 <- x[2] #b_ads
          x3 <- x[3] #c
          x4 <- x[4] #b_m3
          salesmatrix$exb=exp(salesmatrix$adv..bn.*x2 + salesmatrix$iMac*x4)
          salesmatrix$A=cumsum(salesmatrix$t^x3)
          salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
          salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
          salesmatrix$dP[1]=salesmatrix$Pt[1]
          salesmatrix$LL <- salesmatrix$sales*log(salesmatrix$dP)
          LL <- -sum(salesmatrix$LL)
          salesmatrix$exp <- salesmatrix$dP * 129377
          MAPE <- mean(abs((salesmatrix$sales[1:16]-salesmatrix$exp[1:16])/salesmatrix$sales[1:16]))
        
          z <- cbind(lambda = x1, b_ads = x2, c = x3, b_iMac = x4, LL = -LL, MAPE = MAPE)
          rownames(z) <- "parameters"
          
          
        }
  
  
  #Weibull, iMac & Steve Covars
  if (modelType == 2 && length(covariates) == 2) {
    w3Cov <- function(x) { ## function to optimize
      x1 <- x[1] #lambda
      x2 <- x[2] # beta
      x3 <- x[3] # c
      x4 <- x[4] #b_iMac
      x5 <- x[5] #b_steve
      x6 <- x[6] #gamma
      x7 <- x[7] #delta
      salesmatrix$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(salesmatrix$t-24)))[24:44])
      salesmatrix$exb=exp(salesmatrix$adv..bn.*x2 + salesmatrix$iMac*x4 + salesmatrix$sJobs*x5)
      salesmatrix$A=cumsum(((salesmatrix$t^x3) - (c(0, salesmatrix$t[1:16]))^x3)*salesmatrix$exb)
      salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
      salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
      salesmatrix$dP[1]=salesmatrix$Pt[1]
      salesmatrix$LL <- c((salesmatrix$sales[1:16])*(log(salesmatrix$dP)[1:16]), (129377-sum(salesmatrix[1:16,5]))*log(1-salesmatrix$Pt[16]), NA, NA, NA)
      -sum(salesmatrix$LL)
    }
    initial_guess=c(0.005, 1.5, 1.5, 0.5, 0.2, 0.2, 13.5)
    optim(initial_guess, w3Cov)
    x1 <- optim(initial_guess, w3Cov)$par[1] #lamda
    x2 <- optim(initial_guess, w3Cov)$par[2] #b_ads
    x3 <- optim(initial_guess, w3Cov)$par[3] #c
    x4 <- optim(initial_guess, w3Cov)$par[4] #b_iMac
    x5 <- optim(initial_guess, w3Cov)$par[5] #b_steve
    x6 <- optim(initial_guess, w3Cov)$par[6] #gamma
    x7 <- optim(initial_guess, w3Cov)$par[7] #delta
    salesmatrix$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(salesmatrix$t-24)))[24:44])
    salesmatrix$exb=exp(salesmatrix$adv..bn.*x2 + salesmatrix$iMac*x4 + salesmatrix$sJobs*x5)
    salesmatrix$A=cumsum(((salesmatrix$t^x3) - (c(0, salesmatrix$t[1:16]))^x3)*salesmatrix$exb)
    salesmatrix$Pt<-1-exp(-x1*salesmatrix$A)
    salesmatrix$dP[2:nrow(salesmatrix)]=diff(salesmatrix$Pt)
    salesmatrix$dP[1]=salesmatrix$Pt[1]
    salesmatrix$LL <- c((salesmatrix$sales[1:16])*(log(salesmatrix$dP)[1:16]), (129377-sum(salesmatrix[1:16,5]))*log(1-salesmatrix$Pt[16]), NA, NA, NA)
    LL <- -sum(salesmatrix$LL)
    salesmatrix$exp <- salesmatrix$dP * 129377
    MAPE <- mean(abs((salesmatrix$sales[1:16]-salesmatrix$exp[1:16])/salesmatrix$sales[1:16]))
    
    
    z <- cbind(lambda = x1, b_ads = x2, c = x3, b_iMac = x4, b_jobs = x5, gamma = x6, delta = x7, LL = -LL, MAPE = MAPE)
    rownames(z) <- "parameters"
  }
  
  
  plotMin <- min((salesmatrix$dP[startQuarter[1]:startQuarter[2]] * 129377), (salesmatrix[startQuarter[1]:(startQuarter[2]-1),5]), (salesmatrix[startQuarter[1]:startQuarter[2],6]*100))
  plotMax <- max((salesmatrix$dP[startQuarter[1]:startQuarter[2]] * 129377), (salesmatrix[startQuarter[1]:(startQuarter[2]-1),5]), (salesmatrix[startQuarter[1]:startQuarter[2],6]*100))
  
  salesPlot <- plot (c(1:16), salesmatrix$dP[1:16] * 129377, type="l", col="blue", xlab="Quarter", xlim = c(startQuarter[1], startQuarter[2]), ylab="iMac Sales (in 1,000s)", pch=16, cex=1.2,
                    lwd=2, ylim = c(0, plotMax))
  print (salesmatrix$dP[1:16] * 129377)
  lines (c(1:16), salesmatrix[1:16,5], pch=16, cex=1.2, lwd=2, col="red")
  lines (c(1:16), salesmatrix[1:16,6]*100, pch=16, cex=1.2, lwd=2, lty=2)
  lines (within(data.frame(c(1:16)), ypred <- predict(lm((salesmatrix$dP[1:16] * 129377)~poly(c(1:16),benchmark,raw=TRUE)), data.frame(c(1:16)))), col = "grey", pch=16, cex=1.2, lwd=2, lty=2)
  legend (startQuarter[1], plotMax, c("Projected Sales (model)", "Actual Sales", "Ad Expenditures (covariate)","Polynomial Regression (benchmark)"),
            lty = c(1,1,2,2), lwd = c(2,2,2,2),
            col = c("blue","red","black","grey"))
    
  z
}

description <- function(startQuarter, modelType, covariates) {
    if (modelType == 1) {
      "Exponential distributions are essentially Weibull distributions that don't account for duration dependence, c, which measures how the time since most recent purchase impacts repurchase likelihood. Thus for any c significantly greater than 1, Weibull will have better fit (lower MAPE)."
    }
    else {
      if (modelType == 2 && length(covariates) == 2) {
        "With a Mean Average Percent Error (MAPE) of less than .13, the Weibull (with all 3 covariates) demonstrates better fit than all other probability models and traditional regression methods.

Note: At time of analysis (Q4 2016), Apple disclosed Q1-Q3 2016 sales but had not disclosed 2016 ad expenditures. The model initially assumes that ad expenditures in 2016 equal those in 2015 ($1.8bn); Apple could build the most accurate model by inputting their true 2016 ad exenditures.
          Unfortunately this analysis makes the implicit assumptiont that each iMac purchase is a novel customer; the author is working to correct this."
      }
        else {
        "Unlike tradional regression methods (curve fitting), probability models predict sales by analyzing purchase behaviors of individual customers. Lambda measures the average customer's purchase propensity in a given period, betas measure the covariance between purchase likelihood and their given co-variable, Log Likelihood is the parameter we seek to maximize, and MAPE is a goodness-of-fit measure for comparing models."
        }
      }
    }


  
shinyServer(
  function(input, output) {
    output$plot <- renderPlot({TeslaPlot(input$startQuarter, input$modelType, input$covariates, input$benchmark)})
    output$table <- renderTable({TeslaPlot(input$startQuarter, input$modelType, input$covariates, input$benchmark)}, digits = 4)
    output$description <- renderText({description(input$startQuarter, input$modelType, input$covariates)})
    }
  )