library(shiny)
library(dplyr)
library(lubridate)
library(BH)


#ALTERNATE MIXTURES NOT RESOLVED (i.e. WG and 1Cov/2Cov)
#ADD PREDICTION INTO 2016

#data input and clear
iData <- read.csv("iData.csv")
imatr <- iData
imatr[,3] <- as.numeric(gsub(",","",imatr[,3]))
range.ex <- function(x, fac=1.2) { xrg <- range(x);  m <- mean(xrg);  (xrg - m)*fac + m }

tripsPlot <- function(startQuarter, endQuarter, modelType, covariates, mixture) {
        if (modelType == 1 && !mixture) {
          #Exponential Model
          eCov <- function(x) { ## function to optimize
            x1 <- x[1] #lambda
            x2 <- x[2] # beta
            imatr$exb=exp(imatr$adv..bn.*x2)
            imatr$A=cumsum(imatr$exb)
            imatr$Pt<-1-exp(-x1*imatr$A)
            imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
            imatr$dP[1]=imatr$Pt[1]
            imatr$LL=imatr$sales*log(imatr$dP)
            -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
          }
          initial_guess=c(.005, 2)
          x <- optim(initial_guess, eCov)$par
          x1 <- x[1] #lambda
          x2 <- x[2] # beta
          imatr$exb=exp(imatr$adv..bn.*x2)
          imatr$A=cumsum(imatr$exb)
          imatr$Pt<-1-exp(-x1*imatr$A)
          imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
          imatr$dP[1]=imatr$Pt[1]
          imatr$LL=imatr$sales*log(imatr$dP)
          LL <- -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
          
          plotMin <- min((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
          plotMax <- max((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
          
          taxiPlot <- plot (imatr[,1], imatr$dP * 150452, type="l", col="blue", xlab="Quarter", xlim = c(startQuarter, endQuarter), ylab="iMac Sales (in 1,000s)", pch=16, cex=1.2,
                            lwd=2, ylim = c(plotMin, plotMax))
          
          lines (imatr[,1], imatr[,3], pch=16, cex=1.2, lwd=2, col="purple")
          lines (imatr[,1], imatr[,4]*5000, pch=16, cex=1.2, lwd=1)
          legend (startQuarter, plotMax, c("Projected Sales (model)", "Actual Sales", "Ad Expenses (covariate)"),
                                          
                                           lty = c(1,1,1), lwd = c(2,2,1), col = c("blue","purple","black"))
        
          z <- cbind(lambda = x1, b_ads = x2, LL = -LL)
          rownames(z) <- "parameters"
          
          
          }
  
        if (modelType == 1 && mixture) {
          #Exponential Gamma EG
          EG <- function(x) { ## function to optimize
            x1 <- x[1] #r
            x2 <- x[2] #alpha
            x3 <- x[3] #b
            imatr$exb=exp(imatr$adv..bn.*x3)
            imatr$A=cumsum(imatr$exb)
            imatr$Pt<-1-((x2/(x2+imatr$A)))^x1
            imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
            imatr$dP[1]=imatr$Pt[1]
            imatr$LL=imatr$sales*log(imatr$dP)
            -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
          }
          initial_guess=c(300, 50000, 1.5)
          optim(initial_guess, EG)
          x1 <- optim(initial_guess, EG)$par[1] #r
          x2 <- optim(initial_guess, EG)$par[2] #alpha
          x3 <- optim(initial_guess, EG)$par[3] #b
          imatr$exb=exp(imatr$adv..bn.*x3)
          imatr$A=cumsum(imatr$exb)
          imatr$Pt<-1-((x2/(x2+imatr$A)))^x1
          imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
          imatr$dP[1]=imatr$Pt[1]
          imatr$LL=imatr$sales*log(imatr$dP)
          LL <- -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
          
          plotMin <- min((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
          plotMax <- max((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
          
          taxiPlot <- plot (imatr[,1], imatr$dP * 150452, type="l", col="blue", xlab="Quarter", xlim = c(startQuarter, endQuarter), ylab="iMac Sales (in 1,000s)", pch=16, cex=1.2,
                            lwd=2, ylim = c(plotMin, plotMax))
          
          lines (imatr[,1], imatr[,3], pch=16, cex=1.2, lwd=2, col="purple")
          lines (imatr[,1], imatr[,4]*5000, pch=16, cex=1.2, lwd=1)
          legend (startQuarter, plotMax, c("Projected Sales (model)", "Actual Sales", "Ad Expenses (covariate)"),
                                           
                  lty = c(1,1,1), lwd = c(2,2,1),
                  col = c("blue","purple","black"))
        
          z <- cbind(r = x1, alpha = x2, b_ads = x3, LL = -LL)
          rownames(z) <- "parameters"
          
          
          }
              
  #Exponential, iMac Covar
            if (modelType == 1 && length(covariates) == 1 && covariates[1] == 'ni') {
              e2Cov <- function(x) { ## function to optimize
                x1 <- x[1] #lambda
                x2 <- x[2] # beta
                x4 <- x[3] #b_iMac
                imatr$exb=exp(imatr$adv..bn.*x2 + imatr$iMac*x4)
                imatr$A=cumsum(imatr$exb)
                imatr$Pt<-1-exp(-x1*imatr$A)
                imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
                imatr$dP[1]=imatr$Pt[1]
                imatr$LL=imatr$sales*log(imatr$dP)
                -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
              }
                initial_guess=c(.005, 1.5, 0.5)
                x <- optim(initial_guess, e2Cov)$par
                x1 <- x[1] # lambda
                x2 <- x[2] # beta
                x4 <- x[3] # b_iMac
                imatr$exb=exp(imatr$adv..bn.*x2 + imatr$iMac*x4)
                imatr$A=cumsum(imatr$exb)
                imatr$Pt<-1-exp(-x1*imatr$A)
                imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
                imatr$dP[1]=imatr$Pt[1]
                imatr$LL=imatr$sales*log(imatr$dP)
                LL <- -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
                
                plotMin <- min((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
                plotMax <- max((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
                
                taxiPlot <- plot (imatr[,1], imatr$dP * 150452, type="l", col="blue", xlab="Quarter", xlim = c(startQuarter, endQuarter), ylab="iMac Sales (in 1,000s)", pch=16, cex=1.2,
                                  lwd=2, ylim = c(plotMin, plotMax))
                
                lines (imatr[,1], imatr[,3], pch=16, cex=1.2, lwd=2, col="purple")
                lines (imatr[,1], imatr[,4]*5000, pch=16, cex=1.2, lwd=1)
                legend (startQuarter, plotMax, c("Projected Sales (model)", "Actual Sales", "Ad Expenses (covariate)"),
                                                
                        lty = c(1,1,1), lwd = c(2,2,1),
                        col = c("blue","purple","black"))
              
                z <- cbind(lambda = x1, b_ads = x2, b_iMac = x4, LL = -LL)
                rownames(z) <- "parameters"
                
                
                }
      
  #Exponential, Steve Covar
  if (modelType == 1 && length(covariates) == 1 && covariates[1] == 'jd') {
    e2CovJobs <- function(x) { ## function to optimize
      x1 <- x[1] #lambda
      x2 <- x[2] # beta
      x5 <- x[3] #b_steve
      x6 <- x[4] #gamma
      x7 <- x[5] #delta
      imatr$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(imatr$t-24)))[24:40])
      imatr$exb=exp(imatr$adv..bn.*x2 + imatr$sJobs*x5)
      imatr$A=cumsum(imatr$exb)
      imatr$Pt<-1-exp(-x1*imatr$A)
      imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
      imatr$dP[1]=imatr$Pt[1]
      imatr$LL=imatr$sales*log(imatr$dP)
      -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
    }
    initial_guess=c(0.005, 1.5, 0.5, 0.2, 14)
    x <- optim(initial_guess, e2CovJobs)$par
    x1 <- x[1] #lambda
    x2 <- x[2] # beta
    x5 <- x[3] #b_steve
    x6 <- x[4] #gamma
    x7 <- x[5] #delta
    imatr$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(imatr$t-24)))[24:40])
    imatr$exb=exp(imatr$adv..bn.*x2 + imatr$sJobs*x5)
    imatr$A=cumsum(imatr$exb)
    imatr$Pt<-1-exp(-x1*imatr$A)
    imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
    imatr$dP[1]=imatr$Pt[1]
    imatr$LL=imatr$sales*log(imatr$dP)
    LL <- -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
    
    plotMin <- min((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
    plotMax <- max((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
    
    taxiPlot <- plot (imatr[,1], imatr$dP * 150452, type="l", col="blue", xlab="Quarter", xlim = c(startQuarter, endQuarter), ylab="iMac Sales (in 1,000s)", pch=16, cex=1.2,
                      lwd=2, ylim = c(plotMin, plotMax))
    
    lines (imatr[,1], imatr[,3], pch=16, cex=1.2, lwd=2, col="purple")
    lines (imatr[,1], imatr[,4]*5000, pch=16, cex=1.2, lwd=1)
    legend (startQuarter, plotMax, c("Projected Sales (model)", "Actual Sales", "Ad Expenses (covariate)"),
                                  
            lty = c(1,1,1), lwd = c(2,2,1),
            col = c("blue","purple","black"))
  
    z <- cbind(lambda = x1, b_ads = x2, b_jobs = x5, gamma = x6, delta = x7, LL = -LL)
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
        imatr$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(imatr$t-24)))[24:40])
        imatr$exb=exp(imatr$adv..bn.*x2 + imatr$iMac*x4 + imatr$sJobs*x5)
        imatr$A=cumsum(imatr$exb)
        imatr$Pt<-1-exp(-x1*imatr$A)
        imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
        imatr$dP[1]=imatr$Pt[1]
        imatr$LL=imatr$sales*log(imatr$dP)
        -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
      }
      initial_guess=c(0.005, 1.5, 0.5, 0.5, 0.2, 14)
      optim(initial_guess, e3Cov)
      x1 <- optim(initial_guess, e3Cov)$par[1] #lamda
      x2 <- optim(initial_guess, e3Cov)$par[2] #b_ads
      x4 <- optim(initial_guess, e3Cov)$par[3] #b_iMac
      x5 <- optim(initial_guess, e3Cov)$par[4] #b_steve
      x6 <- optim(initial_guess, e3Cov)$par[5] #gamma
      x7 <- optim(initial_guess, e3Cov)$par[6] #delta
      imatr$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(imatr$t-24)))[24:40])
      imatr$exb=exp(imatr$adv..bn.*x2 + imatr$iMac*x4 + imatr$sJobs*x5)
      imatr$A=cumsum(imatr$exb)
      imatr$Pt<-1-exp(-x1*imatr$A)
      imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
      imatr$dP[1]=imatr$Pt[1]
      imatr$LL=imatr$sales*log(imatr$dP)
      LL <- -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
      
      plotMin <- min((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
      plotMax <- max((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
      
      taxiPlot <- plot (imatr[,1], imatr$dP * 150452, type="l", col="blue", xlab="Quarter", xlim = c(startQuarter, endQuarter), ylab="iMac Sales (in 1,000s)", pch=16, cex=1.2,
                        lwd=2, ylim = c(plotMin, plotMax))
      
      lines (imatr[,1], imatr[,3], pch=16, cex=1.2, lwd=2, col="purple")
      lines (imatr[,1], imatr[,4]*5000, pch=16, cex=1.2, lwd=1)
      legend (startQuarter, plotMax, c("Projected Sales (model)", "Actual Sales", "Ad Expenses (covariate)"),
                                       
              lty = c(1,1,1), lwd = c(2,2,1),
              col = c("blue","purple","black"))
  
      z <- cbind(lambda = x1, b_ads = x2, b_iMac = x4, b_jobs = x5, gamma = x6, delta = x7, LL = -LL)
      rownames(z) <- "parameters"
      
      
      }
  
  #Weibull Model
        if (modelType == 2 && length(covariates) == 0) {
          wCov <- function(x) { ## function to optimize
            x1 <- x[1] #lambda
            x2 <- x[2] # beta
            x3 <- x[3] # c
            imatr$exb=exp(imatr$adv..bn.*x2)
            imatr$A=cumsum(((imatr$t^x3) - (c(0, imatr$t[1:39]))^x3)*imatr$exb)
            imatr$Pt<-1-exp(-x1*imatr$A)
            imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
            imatr$dP[1]=imatr$Pt[1]
            imatr$LL=imatr$sales*log(imatr$dP)
            -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
          }
          initial_guess=c(.001, 1.5, 1.5)
          x <- optim(initial_guess, wCov)$par
          x1 <- x[1] #lambda
          x2 <- x[2] # beta
          x3 <- x[3] # c
          imatr$exb=exp(imatr$adv..bn.*x2)
          imatr$A=cumsum(((imatr$t^x3) - (c(0, imatr$t[1:39]))^x3)*imatr$exb)
          imatr$Pt<-1-exp(-x1*imatr$A)
          imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
          imatr$dP[1]=imatr$Pt[1]
          imatr$LL=imatr$sales*log(imatr$dP)
          LL <- -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
          
          plotMin <- min((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
          plotMax <- max((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
          
          taxiPlot <- plot (imatr[,1], imatr$dP * 150452, type="l", col="blue", xlab="Quarter", xlim = c(startQuarter, endQuarter), ylab="iMac Sales (in 1,000s)", pch=16, cex=1.2,
                            lwd=2, ylim = c(plotMin, plotMax))
          
          lines (imatr[,1], imatr[,3], pch=16, cex=1.2, lwd=2, col="purple")
          lines (imatr[,1], imatr[,4]*5000, pch=16, cex=1.2, lwd=1)
          legend (startQuarter, plotMax, c("Projected Sales (model)", "Actual Sales", "Ad Expenses (covariate)"),
                                           lty = c(1,1,1), lwd = c(2,2,1),
                                           col = c("blue","purple","black"))
       
          z <- cbind(lambda = x1, b_ads = x2, c = x3, LL = -LL)
          rownames(z) <- "parameters"
          
          
           }
  
  
    if (modelType == 2 && mixture) {
          #WG Weibull Gamma
          WG <- function(x) { ## function to optimize
            x1 <- x[1] # r
            x2 <- x[2] # beta
            x3 <- x[3] # c
            x4 <- x[4] #alpha
            imatr$exb=exp(imatr$adv..bn.*x2)
            imatr$A=cumsum(((imatr$t^x3) - (c(0, imatr$t[1:39]))^x3)*imatr$exb)
            imatr$Pt<-1-((x4/(x4+imatr$A)))^x1
            imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
            imatr$dP[1]=imatr$Pt[1]
            imatr$LL=imatr$sales*log(imatr$dP)
            -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
          }
          initial_guess=c(300, 1.5, 1.5, 30000)
          x <- optim(initial_guess, WG)$par
          x1 <- x[1] #r
          x2 <- x[2] # beta
          x3 <- x[3] # c
          x4 <- x[4] #alpha
          imatr$exb=exp(imatr$adv..bn.*x2)
          imatr$A=cumsum(((imatr$t^x3) - (c(0, imatr$t[1:39]))^x3)*imatr$exb)
          imatr$Pt<-1-((x4/(x4+imatr$A)))^x1
          imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
          imatr$dP[1]=imatr$Pt[1]
          imatr$LL=imatr$sales*log(imatr$dP)
          LL <- -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
          
          plotMin <- min((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
          plotMax <- max((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
          
          taxiPlot <- plot (imatr[,1], imatr$dP * 150452, type="l", col="blue", xlab="Quarter", xlim = c(startQuarter, endQuarter), ylab="iMac Sales (in 1,000s)", pch=16, cex=1.2,
                            lwd=2, ylim = c(plotMin, plotMax))
          
          lines (imatr[,1], imatr[,3], pch=16, cex=1.2, lwd=2, col="purple")
          lines (imatr[,1], imatr[,4]*5000, pch=16, cex=1.2, lwd=1)
          legend (startQuarter, plotMax, c("Projected Sales (model)", "Actual Sales", "Ad Expenses (covariate)"),
                                            lty = c(1,1,1), lwd = c(2,2,1),
                                            col = c("blue","purple","black"))
   
          z <- cbind(r = x1, alpha = x4, b_ads = x2, c = x3, LL = -LL)
          rownames(z) <- "parameters"
          
          
           }
          
        #Weibull, iMac Covar
        if (modelType == 2 && length(covariates) == 1 && covariates[1] == 'ni') {
          w2Cov <- function(x) { ## function to optimize
            x1 <- x[1] #lambda
            x2 <- x[2] # beta
            x3 <- x[3] # c
            x4 <- x[4] #b_iMac
            imatr$exb=exp(imatr$adv..bn.*x2 + imatr$iMac*x4)
            imatr$A=cumsum(((imatr$t^x3) - (c(0, imatr$t[1:39]))^x3)*imatr$exb)
            imatr$Pt<-1-exp(-x1*imatr$A)
            imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
            imatr$dP[1]=imatr$Pt[1]
            imatr$LL=imatr$sales*log(imatr$dP)
            -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
          }
          initial_guess=c(0.005, 1.5, 1.5, 0.5)
          x <- optim(initial_guess, w2Cov)$par
          x1 <- x[1] #lamda
          x2 <- x[2] #b_ads
          x3 <- x[3] #c
          x4 <- x[4] #b_iMac
          imatr$exb=exp(imatr$adv..bn.*x2 + imatr$iMac*x4)
          imatr$A=cumsum(((imatr$t^x3) - (c(0, imatr$t[1:39]))^x3)*imatr$exb)
          imatr$Pt<-1-exp(-x1*imatr$A)
          imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
          imatr$dP[1]=imatr$Pt[1]
          imatr$LL=imatr$sales*log(imatr$dP)
          LL <- -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
          
          plotMin <- min((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
          plotMax <- max((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
          
          taxiPlot <- plot (imatr[,1], imatr$dP * 150452, type="l", col="blue", xlab="Quarter", xlim = c(startQuarter, endQuarter), ylab="iMac Sales (in 1,000s)", pch=16, cex=1.2,
                            lwd=2, ylim = c(plotMin, plotMax))
          
          lines (imatr[,1], imatr[,3], pch=16, cex=1.2, lwd=2, col="purple")
          lines (imatr[,1], imatr[,4]*5000, pch=16, cex=1.2, lwd=1)
          legend (startQuarter, plotMax, c("Projected Sales (model)", "Actual Sales", "Ad Expenses (covariate)"),
                                            lty = c(1,1,1), lwd = c(2,2,1),
                                            col = c("blue","purple","black"))
       
          z <- cbind(lambda = x1, b_ads = x2, c = x3, b_iMac = x4, LL = -LL)
          rownames(z) <- "parameters"
          
          
        }
  #Weibull, Steve Covar
  if (modelType == 2 && length(covariates) == 1 && covariates[1] == 'jd') {
    w2CovJobs <- function(x) { ## function to optimize
      x1 <- x[1] #lambda
      x2 <- x[2] # beta
      x3 <- x[3] # c
      x5 <- x[4] #b_steve
      x6 <- x[5] #gamma
      x7 <- x[6] #delta
      imatr$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(imatr$t-24)))[24:40])
      imatr$exb=exp(imatr$adv..bn.*x2 + imatr$sJobs*x5)
      imatr$A=cumsum(((imatr$t^x3) - (c(0, imatr$t[1:39]))^x3)*imatr$exb)
      imatr$Pt<-1-exp(-x1*imatr$A)
      imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
      imatr$dP[1]=imatr$Pt[1]
      imatr$LL=imatr$sales*log(imatr$dP)
      -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
    }
    initial_guess=c(0.005, 1.5, 1.5, 0.5, 0.2, 14)
    optim(initial_guess, w2CovJobs)
    x1 <- optim(initial_guess, w2CovJobs)$par[1] #lamda
    x2 <- optim(initial_guess, w2CovJobs)$par[2] #b_ads
    x3 <- optim(initial_guess, w2CovJobs)$par[3] #c
    x5 <- optim(initial_guess, w2CovJobs)$par[4] #b_steve
    x6 <- optim(initial_guess, w2CovJobs)$par[5] #gamma
    x7 <- optim(initial_guess, w2CovJobs)$par[6] #delta
    imatr$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(imatr$t-24)))[24:40])
    imatr$exb=exp(imatr$adv..bn.*x2 + imatr$sJobs*x5)
    imatr$A=cumsum(((imatr$t^x3) - (c(0, imatr$t[1:39]))^x3)*imatr$exb)
    imatr$Pt<-1-exp(-x1*imatr$A)
    imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
    imatr$dP[1]=imatr$Pt[1]
    imatr$LL=imatr$sales*log(imatr$dP)
    LL <- -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
    
    plotMin <- min((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
    plotMax <- max((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
    
    taxiPlot <- plot (imatr[,1], imatr$dP * 150452, type="l", col="blue", xlab="Quarter", xlim = c(startQuarter, endQuarter), ylab="iMac Sales (in 1,000s)", pch=16, cex=1.2,
                      lwd=2, ylim = c(plotMin, plotMax))
    
    lines (imatr[,1], imatr[,3], pch=16, cex=1.2, lwd=2, col="purple")
    lines (imatr[,1], imatr[,4]*5000, pch=16, cex=1.2, lwd=1)
    legend (startQuarter, plotMax, c("Projected Sales (model)", "Actual Sales", "Ad Expenses (covariate)"),
            lty = c(1,1,1), lwd = c(2,2,1),
            col = c("blue","purple","black"))
    
  
    z <- cbind(lambda = x1, b_ads = x2, c = x3, b_jobs = x5, gamma = x6, delta = x7, LL = -LL)
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
      imatr$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(imatr$t-24)))[24:40])
      imatr$exb=exp(imatr$adv..bn.*x2 + imatr$iMac*x4 + imatr$sJobs*x5)
      imatr$A=cumsum(((imatr$t^x3) - (c(0, imatr$t[1:39]))^x3)*imatr$exb)
      imatr$Pt<-1-exp(-x1*imatr$A)
      imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
      imatr$dP[1]=imatr$Pt[1]
      imatr$LL=imatr$sales*log(imatr$dP)
      -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
    }
    initial_guess=c(0.005, 1.5, 1.5, 0.5, 0.5, 0.2, 14)
    optim(initial_guess, w3Cov)
    x1 <- optim(initial_guess, w3Cov)$par[1] #lamda
    x2 <- optim(initial_guess, w3Cov)$par[2] #b_ads
    x3 <- optim(initial_guess, w3Cov)$par[3] #c
    x4 <- optim(initial_guess, w3Cov)$par[4] #b_iMac
    x5 <- optim(initial_guess, w3Cov)$par[5] #b_steve
    x6 <- optim(initial_guess, w3Cov)$par[6] #gamma
    x7 <- optim(initial_guess, w3Cov)$par[7] #delta
    imatr$sJobs <- c(array(0, c(1,23)), 1-x6*(1-exp(-x7*abs(imatr$t-24)))[24:40])
    imatr$exb=exp(imatr$adv..bn.*x2 + imatr$iMac*x4 + imatr$sJobs*x5)
    imatr$A=cumsum(((imatr$t^x3) - (c(0, imatr$t[1:39]))^x3)*imatr$exb)
    imatr$Pt<-1-exp(-x1*imatr$A)
    imatr$dP[2:nrow(imatr)]=diff(imatr$Pt)
    imatr$dP[1]=imatr$Pt[1]
    imatr$LL=imatr$sales*log(imatr$dP)
    LL <- -sum(imatr$LL, (150452-sum(imatr[,3]))*log(1-imatr$Pt[40]))
    
    plotMin <- min((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
    plotMax <- max((imatr$dP[startQuarter:endQuarter] * 150452), (imatr[startQuarter:endQuarter,3]), (imatr[startQuarter:endQuarter,4]*5000))
    
    taxiPlot <- plot (imatr[,1], imatr$dP * 150452, type="l", col="blue", xlab="Quarter", xlim = c(startQuarter, endQuarter), ylab="iMac Sales (in 1,000s)", pch=16, cex=1.2,
                      lwd=2, ylim = c(plotMin, plotMax))
    
    lines (imatr[,1], imatr[,3], pch=16, cex=1.2, lwd=2, col="purple")
    lines (imatr[,1], imatr[,4]*5000, pch=16, cex=1.2, lwd=1)
    legend (startQuarter, plotMax, c("Projected Sales (model)", "Actual Sales", "Ad Expenses (covariate)"),
            lty = c(1,1,1), lwd = c(2,2,1),
            col = c("blue","purple","black"))
    
    
    z <- cbind(lambda = x1, b_ads = x2, c = x3, b_iMac = x4, b_jobs = x5, gamma = x6, delta = x7, LL = -LL)
    rownames(z) <- "parameters"
  }
  z
}

shinyServer(
  function(input, output) {
    output$plot <- renderPlot({tripsPlot(input$startQuarter, input$endQuarter, input$modelType, input$covariates, input$mixture)})
    output$table <- renderTable({tripsPlot(input$startQuarter, input$endQuarter, input$modelType, input$covariates, input$mixture)}, digits = 4)
    }
  )