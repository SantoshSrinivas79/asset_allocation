##################################################################
### This app visulizes chooses portfolio optimization strategy ###
### Written by: Dongpeng Liu                                   ###
### This version: 2017-10-07                                   ###
##################################################################

# load necessary packages
library(shiny)
library(shinydashboard)
library(quantmod)
library(PortfolioAnalytics)
library(dplyr)
library(xts)
library(ggplot2)

rm(list = ls())

######################
### user interface ###
######################

ui = dashboardPage(
  
  # header of the dash board 
  dashboardHeader(title = "Numerical inputs"),
  
  # side bar
  dashboardSidebar(
    
    # slider to select proportion of total assets invested
    # full investment by default
    sliderInput(inputId = "prisky",
                label = "Proportion of assets to be invested (%):",
                min = 0,
                max = 100,
                value = 100), # end of sliderInput
    
    # slider to select min and max proportion of total assets allowed in us stock
    # 0 - 100 by default
    sliderInput(inputId = "puss",
                label = "Min & max proportion of assets allowed in US stock (%):",
                min = -20,
                max = 120,
                value = c(0, 100)), # end of sliderInput
    
    # slider to select min and max proportion of total assets allowed in us bonds
    # 0 - 100 by default
    sliderInput(inputId = "pusb",
                label = "Min & max proportion of assets allowed in US bonds (%):",
                min = -20,
                max = 120,
                value = c(0, 100)), # end of sliderInput
    
    # slider to select min and max proportion of total assets allowed in commodities
    # 0 - 100 by default
    sliderInput(inputId = "pcom",
                label = "Min & max proportion of assets allowed in commodities (%):",
                min = -20,
                max = 120,
                value = c(0, 100)), # end of sliderInput
    
    # slider to select min and max proportion of total assets allowed in ca stock
    # 0 - 100 by default
    sliderInput(inputId = "pcas",
                label = "Min & max proportion of assets allowed in CA stock (%):",
                min = -20,
                max = 120,
                value = c(0, 100)), # end of sliderInput
    
    # slider to select amount of risk tolerated
    # 20 by default
    sliderInput(inputId = "rtol",
                label = "Weekly risk tolerance (%):",
                min = 0,
                max = 20,
                value = 5,
                step = 1), # end of sliderInput
    
    # slider to select the training period
    # 60 by default 
    sliderInput(inputId = "tp",
                label = "Training period (weeks)",
                min = 60,
                max = 100,
                value = 60,
                step = 5) # end of sliderInput
    
  ), # end of dashboardSidebar
  
  # main body
  dashboardBody(
    
    ## non-numerical inputs ##
    fluidRow(
      
      box(title = "Non-numerical inputs",
          solidHeader = T,
          status = "primary",
          collapsible = T,
          width = 4,
          height = 312,
          
          # input menu to select the starting and ending dates
          # the most recent 731 days is selected by default
          dateRangeInput(inputId = "dates",
                         label = "Select the time period:",
                         format = "yyyy-mm-dd",
                         start = Sys.Date() - 731,
                         end = Sys.Date() - 1,
                         min = "2011-07-14",
                         max = Sys.Date() - 1), # end of dateRangeInput
          
          # select box to choose moment calculation methods
          # sample by default
          selectInput(inputId = "moments",
                      label = "Moments calculation method:",
                      choices = list("Sample" = "sample",
                                     "Boudt" = "boudt",
                                     "Black Litterman" = "black_litterman",
                                     "Meucci" = "meucci")), #end of selectInput
          
          # select box to choose rebalancing frequency
          # monthly by default
          selectInput(inputId = "rebal",
                      label = "Rebalancing frequency:",
                      choices = list("Monthly" = "months",
                                     "Quarterly" = "quarters",
                                     "Yearly" = "years")) # end of selectInput
          ), # end of box
    
      ## weight chart
      box(title = "Optimal Allocation",
          solidHeader = T,
          status = "primary",
          collapsible = T,
          width = 4,
          
          plotOutput("chartweights", height = 250)
          ), # end of box
      
      ## risk contribution
      box(title = "Risk Contribution",
          solidHeader = T,
          status = "primary",
          collapsible = T,
          width = 4,
          
          plotOutput("riskcon", height = 250)
          ) # end of box
      
    ), # end of row 1
  
    fluidRow(
      
      infoBoxOutput("wksup"),
      infoBoxOutput("wksdown"),
      infoBoxOutput("totreturn")
    ), # end of row2
    
    fluidRow(
      
      ## performance chart
      box(title = "Performance of asset allocation strategy",
          solidHeader = T,
          status = "primary",
          collapsible = T,
          width = 12,
          
          plotOutput("performance", height = 500)
          ) # end of box
      
    ), # end of row 3
    
    fluidRow(
      
      ## histogram
      box(title = "Distribution of weekly portfolio returns",
          solidHeader = T,
          status = "primary",
          collapsible = T,
          width = 6,
          
          plotOutput("histogram", height = 400)
      ), # end of box
      
      ## notes
      box(title = "Notes",
          solidHeader = T,
          status = "primary",
          collapsible = T,
          width = 6,
          height = 462,
          
          "1. Results are based on portfolio performances on the test set.",
          br(),
          "2. Differences of portfolio performances do not imply quality differences of allocation strategies if original data is collected from different periods."
      ) # end of box
      
    ) # end of row 4
      
  ) # end of dashboardBody
) # end of ui


############################
### interactive contents ###
############################

server = function(input, output){
  
  rv = reactiveValues()
  
  # data values reactive to input values
  observeEvent(
    
    # triggers
    {
      input$dates
      input$prisky
      input$puss
      input$pusb
      input$pcom
      input$pcas
      input$rtol
      input$moments
      input$rebal
      input$tp
    }, # end of triggers,
    
    # reactive output
    {
      ## download price data and calculate returns ##
      # us stock
      returns = "^GSPC" %>%
        getSymbols(src = "yahoo",
                   auto.assign = F,
                   from = input$dates[1],
                   to = input$dates[2]) %>%
        to.weekly() %>%
        Ad() %>%
        log() %>%
        diff()
      
      # other data: us bond, commodities, ca stock
      assets = c("AGG", "USCI", "^GSPTSE")
      for (i in 1:length(assets)){
        returns_temp = assets[i] %>%
          getSymbols(src = "yahoo",
                     auto.assign = F,
                     from = input$dates[1],
                     to = input$dates[2]) %>%
          to.weekly() %>%
          Ad() %>%
          log() %>%
          diff()
        returns = merge(returns, returns_temp, fill = 0)
      } # end of downloading data
      
      returns = returns[-1, ]
      colnames(returns) = c("us_stock", "us_bond", "commodities", "ca_stock")
      
      ## create portfolio ##
      # define portfolio
      p_spec = portfolio.spec(colnames(returns))
      
      # adding constraints
      p_spec = add.constraint(portfolio = p_spec,
                              type = "weight_sum",
                              min_sum = input$prisky[1] / 100,
                              max_sum = input$prisky[2] / 100)
      p_spec = add.constraint(portfolio = p_spec,
                              type = "box",
                              min = c(input$puss[1] / 100, input$pusb[1] / 100,
                                      input$pcom[1] / 100, input$pcas[1] / 100),
                              max = c(input$puss[2] / 100, input$pusb[2] / 100,
                                      input$pcom[2] / 100, input$pcas[2] / 100))
      
      # adding objectives
      p_spec = add.objective(portfolio = p_spec,
                             type = "return",
                             name = "mean")
      p_spec = add.objective(portfolio = p_spec,
                             type = "risk_budget",
                             name = "StdDev",
                             min_prisk = 0, max_prisk = input$rtol / 100)
      
      # finding optimal asset allocation
      opt = suppressWarnings(optimize.portfolio.rebalancing(R = returns,
                                                            portfolio = p_spec,
                                                            optimize_method = "random",
                                                            method = input$moments,
                                                            training_period = input$tp,
                                                            rolling_window = input$tp,
                                                            rebalance_on = input$rebal))
      
      rv$opt = opt
      
      # extract weights
      opt_weight = extractWeights(opt)
      
      # calculate portfolio returns
      p_returns = Return.portfolio(R = returns, 
                                   weights = opt_weight)
      rv$p_returns = p_returns
      
      # number of positive and negative returns
      pos_ret = sum(p_returns > 0)
      neg_ret = sum(p_returns < 0)
      rv$counts = c(pos_ret, neg_ret)
      
      # calculate cumulative returns and total rate of return
      c_returns = cumprod(p_returns + 1) - 1
      t_return = tail(c_returns, 1) 
      rv$t_return = t_return
      
    } # end of reactive output
    
  ) # end of observeEvent
  
  
  # draw asset allocation figure
  output$chartweights = renderPlot(chart.Weights(rv$opt,
                                                 main = "Allocation",
                                                 ylab = "Weights",
                                                 cex.axis = 0.6))
  
  # draw the performance figure
  output$performance = renderPlot(charts.PerformanceSummary(rv$p_returns,
                                                            main = "",
                                                            cex.axis = 0.9,
                                                            Rf = 0.0025 / 52))
  # draw the histogram
  output$histogram = renderPlot(ggplot(data = data.frame(rv$p_returns), aes(x = rv$p_returns[, 1])) +
                                  geom_histogram(aes(y = ..density.., fill = ..count..), 
                                                 alpha = 0.7, bins = 30, xlab = "weekly return") + 
                                  geom_density(col = "red", width = 1))
  
  # draw risk contribution chart
  output$riskcon = renderPlot(chart.RiskBudget(rv$opt, 
                                               match.col = "StdDev", 
                                               risk.type = "percentage",
                                               cex.axis = 0.6))
  
  # create info boxes
  output$wksup = renderInfoBox({
    infoBox("Positive returns", paste(rv$counts[1], "weeks"),
            icon = icon("arrow-up"), fill = T, col = "green")
  })
  
  output$wksdown = renderInfoBox({
    infoBox("Negative returns", paste(rv$counts[2], "weeks"),
            icon = icon("arrow-down"), fill = T, color = "red")
  })
  
  output$totreturn = renderInfoBox({
    infoBox("Total rate of returns", paste(round(rv$t_return * 100, 2), "%", sep = ""),
            icon = icon("money"), fill = T, color = "yellow")
  })
} # end of server


###################
### run the app ###
###################

shinyApp(ui = ui, server = server)