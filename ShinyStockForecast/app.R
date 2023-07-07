library(shiny)
library(quantmod)
library(dygraphs)
library(zoo)
library(forecast)
library(tseries)

nameVector <- c("Open","High","Low","Close","Volume","Adjusted")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Forecasting Stocks"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            style = "position:fixed;width:inherit;",
            # Input: Slider for the number of bins ----
            textInput(inputId = "ticker",
                        label = "Enter Stock Symbol:",placeholder = "GOOG"),
            sliderInput(inputId = "forecastPeriods",label = "# Periods Forecast",
                        min = 5,max = 100,value = 10),
            radioButtons(inputId = "SelectStockChannel",label = "Select Channel",
                         choices = nameVector,selected = "Close"),
            uiOutput("StartDate"),
            textOutput("startDate")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            #fluidRow(column(12,dygraphOutput(outputId = "CandlestickPlot"))),
            fluidRow(column(12,dygraphOutput(outputId = "SeriesPlot"))),
            fluidRow(column(12,plotOutput(outputId = "ForecastPlot"))),
            fluidRow(column(12,plotOutput(outputId = "ACFPlot"))),
            fluidRow(column(12,plotOutput(outputId = "PACFPlot"))),
            fluidRow(column(12,textOutput(outputId = "Stationary"))),
            
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    output$startDate <- renderText({
        as.character(input$dateRangeSelection2[1])
    })
    
    output$StartDate <- renderUI({
        sliderInput(inputId = "StartDate",label = "Select Forecast Start Date",
                    min = min(index(thisStock())),
                    max = max(index(thisStock())),value = min(index(thisStock())))
    })
    
    output$Stationary <- renderText({
        thisForecastStationary()
    })
    
    thisStock <- eventReactive(input$ticker,{
        
        req(input$ticker)
        
        cat("\n ","ticker:",input$ticker)
        
        #q <- getSymbols(Symbols = "CRK",auto.assign = FALSE)
        
        #q <- getSymbols(Symbols = input$ticker,auto.assign = FALSE)
        
        #saveRDS(q,"CRK.RData")
        q <- readRDS("CRK.RData")
        
        #input <- list("ticker"="CRK")
        names(q) <- gsub(pattern = paste0(input$ticker,"\\."),x = names(q),replacement = "")
        print(names(q))
        print("FINISHED READING DATA")
        q
        
    })
    
    thisStockFiltered <- eventReactive({input$ticker;input$StartDate},{
        
        req(input$ticker)
        req(input$StartDate)

        if (!is.null(input$StartDate)) {
            print("not null")
            q <- window(thisStock(), start = input$StartDate,
                   end = Sys.Date()-1)
        } else {
            print("is null")
            q <- thisStock()
        }
        q
    })
    
    thisStockFilteredChannel <- eventReactive({input$ticker;input$StartDate;input$SelectStockChannel},{
        #input$StockChannel <- "Close"
        #q[,which(names(q) == input$StockChannel)]
        req(input$ticker)
        req(input$StartDate)
        req(input$SelectStockChannel)
        print("Entering select stock channel")
        thisStockFiltered()[,which(names(thisStockFiltered()) == input$SelectStockChannel)]
        
    })
    
    output$CandlestickPlot <- renderDygraph({
        #[,1:4]
        dygraphs::dygraph(thisStock()) %>% dyCandlestick()
        # quantmod::candleChart(thisStock(),multi.col=TRUE)
    })
    
    
    output$SeriesPlot <- renderDygraph({

        dygraphs::dygraph(thisStockFiltered()[,1:4]) %>% 
            dySeries(name = "High",label = "High") %>%
            dySeries(name = "Close",label = "Close") %>%
            dySeries(name = "Low",label = "Low")
    })
    
    output$ForecastPlot <- renderPlot({
        print("Inside PlotForecast")
        plot(thisForecast())
        
    })
    
    output$ACFPlot <- renderPlot({
        print("Inside ACF")
        acf(thisStockFilteredChannel(),lag.max = length(thisStockFilteredChannel()),
            xlab = "lag #", ylab = 'ACF', main=' ')
        
    })
    
    output$PACFPlot <- renderPlot({
        print("Inside PACF")
        pacf(thisStockFilteredChannel(),lag.max = length(thisStockFilteredChannel()),
            xlab = "lag #", ylab = 'ACF', main=' ')
        
    })

    
    thisForecast <- eventReactive({input$ticker;input$StartDate;input$forecastPeriods;input$SelectStockChannel},{

        # input$StartDate;input$forecastPeriods

        print("entering thisForecast")

        req(input$StartDate)
        req(input$forecastPeriods)

        q <- thisStockFilteredChannel()
        #print(q)
        model <- auto.arima(q, trace=TRUE, test="kpss", ic="bic")
        print(model)
        f <- forecast(model,h = input$forecastPeriods)
        print(f)
        f
    })
    
    
    thisForecastStationary <- eventReactive({input$ticker;input$StartDate;input$forecastPeriods;input$SelectStockChannel},{
        
        # input$StartDate;input$forecastPeriods
        
        req(input$ticker)
        req(input$SelectStockChannel)
        req(input$StartDate)
        req(input$forecastPeriods)
        
        print("entering Stationary Test for thisForecast")
        
        q <- thisStockFilteredChannel()
        
        w <- adf.test(log(q), alternative="stationary", k=0)

        print(w$p.value)
        
        if (w$p.value < 0.05) {
            print("less than 0.05")
            result <- paste0("P-Value: ",round(w$p.value,digits = 2),
                         ", Cannot accept Null Hypothesis: Series is likely Stationary")
        } else {
            result <- paste0("P-Value: ",round(w$p.value,digits = 2),
                         ", Cannot reject Null Hypothesis: Series may be Non-Stationary")
        }
        print(result)
        
        result
        
    })
    
    
}

shinyApp(ui = ui, server = server)
# 
# library(tseries)
# q <- readRDS("CRK.RData")
#  
# q1 <- tail(q$CRK.Adjusted,100)
# 
# adf.test(log(q1))
# 
# w <- adf.test(log(q1), alternative="stationary", k=0)
# 
# w <- adf.test(rnorm(100), alternative="stationary", k=0)
# 
# w
# 
# if (w$p.value < 0.05) {
#     print(paste0("P-Value: ",round(w$p.value,digits = 2),
#                  ", Cannot accept Null Hypothesis: Series is likely Stationary"))
# } else {
#     print(paste0("P-Value: ",round(w$p.value,digits = 2),
#                  ", Cannot reject Null Hypothesis: Series may be Non-Stationary"))
# }
# acf(q1)
# 
# model <- auto.arima(tail(q1), trace=TRUE, test="kpss", ic="bic")
# 
# f <- forecast(model,h = 20)
# plot(f)

# ?forecast
