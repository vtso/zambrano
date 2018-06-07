# econ project server
# gse 544, zambrano


# Stuff left to do:
# add in kelly criterion
# figure out how to upload app online


rm(list=ls())

library(GLDEX)
library(quantmod)
library(lubridate)
library(dplyr)


# functions ############################### ############################### ###############################  ###############################

# this function takes a finance ticker symbol and year as input parameters and returns a time series data set that is made up of 
## daily returns of the given ticker symbol. The daily returns given go back to the number of years indicated, from today's date.
## It uses the getSymbols() function from the library quantmod. This function was written specifically for this project.
getData <- function(ticker, yr){
  currentDate <- Sys.Date() # get current date
  defaultDate <- currentDate - years(yr)
  df <- getSymbols(ticker, from= defaultDate, env= NULL, src= "yahoo")
  # dfTwo <- getSymbols(coTwo, from= defaultDate, env= NULL, src= "yahoo")
}

# this function takes shares, dpf, maxshare as input parameters. It is plugged into the optim function to calculates
## kelly criterion to maximize log returns; this function was created from open source code for this project
opt_portfolio <-function(shares, dpf, maxshare) {
  
  # calculate portfolios return vector
  exp_returns <- dpf%*%shares
  
  obj = -sum(log(1+exp_returns))
  weight.penalty = 1000*(1-sum(shares))^2
  
  # max share penalty:
  maxpen <- sum(shares[shares>maxshare])*1000
  return(obj + weight.penalty + maxpen)
}

# this function takes in stock data and expected returns and provides a recomemndation of investment distribution
## this function uses the opt_portfolio function and was modified from open source code for this project
opt_portfolio_wrapper <- function(stocks,r=rep(0.03,length(stocks)),maxshare=1,daily=FALSE,short=FALSE) {
  
  defaultDate <- Sys.Date()- years(20)
  # test which stocks are already in workspace, else download data
  
  df1 <- getSymbols(stocks[1],from=defaultDate,env=NULL, src= "yahoo")[,6]
  df2 <- getSymbols(stocks[2],from=defaultDate,env=NULL, src= "yahoo")[,6]
  
  # merge all stocks together
  portfolio <- cbind(df1, df2)
  
  # build returns by diff()/lag()
  d.portfolio <- data.frame(na.omit(diff(portfolio)/stats::lag(portfolio)))
  
  # center around the mean to eliminate past performance as an information for portfolio selection
  d.portfolio.future <- scale(d.portfolio, scale=F)
  
  # add the (personally) expected return for each stock
  for(i in 1:ncol(d.portfolio.future)) {
    d.portfolio.future[,i] <- d.portfolio.future[,i] + r[i]
  }
  
  # define lower und upper bounds for the parameters. 0 to 1 or -1 to 1 if short positions are allowed
  lower <- rep(0,length(stocks))
  upper <- rep(1,length(stocks))
  
  # starting values for optimizer
  start <- rep(1/length(stocks),length(stocks))
  
  res <- optim(start, opt_portfolio, dpf=d.portfolio.future, maxshare=1, method="Nelder-Mead")
  
  Portfolio <- data.frame("Stock"= stocks, "Share"=round(res$par, digits= 3))
  return(Portfolio)
}


# this is the server function that will feed into the R Shiny app
server <- function(input, output, session){
  observeEvent(input$refresh,{
    coOne <- as.character(input$coOne)
    coTwo <- "SPY"
    one.vector <- as.vector(getData(coOne, 20)[,6])
    two.vector <- as.vector(getData(coTwo, 20)[,6])
    one.returns <- diff(log(one.vector), lag= 1)
    two.returns <- diff(log(two.vector), lag= 1)
    currentDate <- Sys.Date()
    defaultDate <- currentDate - years(20)
    spLambdaDist <- fun.data.fit.mm(one.returns); lamb1 <- spLambdaDist[,2]
    lamb1_1 <- lamb1[1]; lamb1_2 <- lamb1[2]; lamb1_3 <- lamb1[3]; lamb1_4 <- lamb1[4] # fmkl for given co.
    spLambdaDist2 <- fun.data.fit.mm(two.returns); lamb2 <- spLambdaDist2[,2]
    lamb2_1 <- lamb2[1]; lamb2_2 <- lamb2[2]; lamb2_3 <- lamb2[3]; lamb2_4 <- lamb2[4] # fmkl for spy
    fmkl1 <- rgl(n= 10000000, lambda1= lamb1_1, lambda2= lamb1_2, lambda3= lamb1_3,
                 lambda4= lamb1_4, param= "fmkl")
    fmkl2 <- rgl(n= 10000000, lambda1= lamb2_1, lambda2= lamb2_2, lambda3= lamb2_3,
                 lambda4= lamb2_4, param= "fmkl")
    mkt1 <- fun.moments.r(one.returns, normalise = "Y")
    mkt2 <- fun.moments.r(two.returns, normalise = "Y")
    # calculate sharp ratio for coOne
    mean1 <- as.numeric(mkt1[1]); sd1 <- as.numeric(sqrt(mkt1[2])); quant1 <- qnorm(0.01)
    skew1 <- as.numeric(mkt1[3]); kurt1 <- as.numeric(mkt1[4]);
    Zcf1 <- (quant1 + (quant1^2-1)*skew1/6 + (quant1^3 - 3*quant1)*kurt1/24 - (2*quant1^3 - 5*quant1)*skew1^2/36)
    Mvar1 <- mean1 + sd1*Zcf1; rf1 <- (log(1+0.007))/365
    Mod_Sharpe1 <- (mean1 - rf1)/(-Mvar1)
    # calculate sharp ratio for SPY
    mean2 <- as.numeric(mkt2[1]); sd2 <- as.numeric(sqrt(mkt2[2])); quant2 <- qnorm(0.01)
    skew2 <- as.numeric(mkt2[3]); kurt2 <- as.numeric(mkt2[4]);
    Zcf2 <- (quant2 + (quant2^2-1)*skew2/6 + (quant2^3 - 3*quant2)*kurt2/24 - (2*quant2^3 - 5*quant2)*skew2^2/36)
    Mvar2 <- mean2 + sd2*Zcf2; rf2 <- (log(1+0.007))/365
    Mod_Sharpe2 <- (mean2-rf2)/(-Mvar2)
    # calculate kelly criterion
    #rstocks <- c(coTwo, coOne) # input and SP500
    kellyResult <- opt_portfolio_wrapper(c(coTwo, coOne), r= c(mean1, mean2), maxshare= 1, daily= TRUE, short= FALSE)
    
    # generates outputs displayed on dashboard
    output$plot1 <- renderPlot({
      ts.plot(ts(cbind(one.returns, two.returns), frequency= 364, start= year(defaultDate), end= year(currentDate)),
              main= paste("Fig 1. Adjusted Returns:", coOne, "vs.", coTwo), 
              col= c("navy blue", "maroon"), ylab= "Returns")
      legend("bottomright", legend= c(coOne, coTwo), col= c("navy blue", "maroon"), lty= 1)
    })
    output$text2 <- renderTable({ # calculates returns without mc simulations
      mkt1 <- c(mkt1, "Adj.Sharpe"= Mod_Sharpe1)
      mkt2 <- c(mkt2, "Adj.Sharpe"= Mod_Sharpe2)
      moments <- rbind(mkt1, mkt2)
      rownames(moments) <- c(coOne, coTwo)
      moments
    }, rownames= TRUE, width= "75%", digits= 6)
    output$textMC <- renderTable({ # calculates returns with mc simulations
      momentsMC <- rbind(fun.moments.r(fmkl1, normalise = "Y"), 
                       fun.moments.r(fmkl2, normalise = "Y"))
      rownames(momentsMC) <- c(coOne, coTwo)
      momentsMC
    }, rownames= TRUE, width= "75%", digits= 6)
    output$plot2 <- renderPlot({
      fun.plot.fit(fit.obj = spLambdaDist2, data = two.returns, 
                   nclass = 100, param = c("rs", "fmkl"), xlab = "Returns",
                   main= "Fig 2.1 RS vs FMK Model Fit for SP500")

    })
    output$plot3 <- renderPlot({
      fun.plot.fit(fit.obj = spLambdaDist, data= one.returns,
                   nclass= 100, param= c("rs", "fmkl"), xlab= "Returns",
                   main= paste("Fig 2.2 RS vs FMK Model Fit for", coOne))
      
    })
    output$kelly1 <- renderValueBox({
      valueBox(kellyResult[1,2], coOne, icon= icon("check-circle"),
               color= "purple")
    })
    output$kelly2 <- renderValueBox({
      valueBox(kellyResult[2,2], coTwo, icon= icon("check-circle"),
               color= "yellow")
    })
    #output$kellycrit <- renderTable({
    #  kellyResult
    #}, width = "75%", digits= 3)
  })
  

}

