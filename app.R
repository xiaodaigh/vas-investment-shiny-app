library(shiny)
library(quantmod)
library(data.table)
library(magrittr)
library(fst)
library(withr)
library(DT)

asx_index <- fst::read_fst("AXJO.fst",as.data.table = T)
  
calc_ratio <- function(p, d, a1) {
  a1c = copy(a1[date < d])
  a1c[,yrs := as.integer(d-date)/365.25+1]
  a1c[,ratio:=exp(log(p/price)/yrs)-1]
  a1c
}

calc_price <- function(d, a1) {
  a1b = a1[date == d,]
  res = optim(a1b$price, function(p) {
    a1d = calc_ratio(p, a1b$date, a1)
    a1d[,sum((ratio - 0.0482)^2*yrs)]
  }, method="Brent", lower =0, upper =200)
  
  data.table(date=d, pricenow = a1b$price, targetprice = res$par)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("VAS Investment"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         actionButton("run_model_btn", label = "Update the model")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        dataTableOutput("latestprice")
        , DT::dataTableOutput("targetprice")
        , plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   # returns the shares price of VAS imputed with XJO data for longer history
   shares_hist <- reactive({
     # get vas data ------------------------------------------------------------
     getSymbols("VAS.AX",from=as.Date("1970-01-01"))
     a=as.data.table(VAS.AX)
     a1 = a[!is.na(VAS.AX.Close),.(date=index,price=VAS.AX.Close)]
     # browser()
     b1 = asx_index
     # concatenate with indexing result ----------------------------------------
     b2=rbindlist(
       list(
         data.table(date=as.Date("1980-01-01"), price =500), 
         b1[date<=a1[1,date]]
       )
     )
     
     platest =b2[.N,price]
     b2[,priceratio:=price/platest]
     add_to_a1 = a1[1,.(date=b2$date, price=price*b2$priceratio)]
     a1 = rbindlist(list(add_to_a1[-.N,],a1))
     
     a1
   })
   
   output$latestprice <- renderDataTable({
     input$run_model_btn
     a1 = shares_hist()
     d = a1[(.N-6):.N,date]
     rbindlist(lapply(d, function(dd) calc_price(dd,a1)))
   })
   
   target_price_dt <- reactive({
     # browser()
     a1 = shares_hist()
     # calc_price(a1[.N,date])
     # calc_price(as.Date("2007-11-01"))
     # calc_price(as.Date("2009-01-05"))
     
     bb_hist <- fst::read_fst("target_price_hist.fst",as.data.table = T)
     
     n = a1[date > bb_hist[,max(date)],.N]
     # browser()
     # if(n == 0) {
     #   return(bb_hist)
     # } else {
       # go tiger ----------------------------------------------------------------
       withProgress(message="Calculating target price",value = 0, {
         # system.time(bb <- lapply(a1[date > bb_hist[,max(date)],date], function(d) {
         #   res = calc_price(d)
         #   incProgress(1/n, detail =paste("Finished ", d))
         #   res
         # }) %>% rbindlist)
         system.time(bb <- lapply(a1[(.N-30):.N,date], function(d) {
           res = calc_price(d, a1)
           incProgress(1/n, detail =paste("Finished ", d))
           res
         }) %>% rbindlist)
       })
       fst::write_fst(bb,"target_price_hist.fst")
       return(bb)
     # }
   })
   
   output$distPlot <- renderPlot({
     bb = target_price_dt()
     # browser()
     ylim=c(min(bb$targetprice, bb$pricenow), max(bb$targetprice, bb$pricenow))
     plot(bb[, .(date,targetprice)],type='l',col="blue", ylim = c(20,100))
     lines(bb[, .(date, pricenow)],type='l',col="black")
     legend("bottomright",lty=1,
            c("Long term trend", "VAS Historical Price (with Imputation pre-2009)"),
            col=c("blue","black"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

