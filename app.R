#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
ui <- fluidPage(

    # Application title
    titlePanel("BatteryMon Log Viewer"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Choose the year",
                        min = 2023,
                        max = 2033,
                        value = 2023),
            sliderInput("month",
                        "Choose the month",
                        min = 1,
                        max = 12,
                        value = 7),
            sliderInput("date",
                        "Choose the date",
                        min = 1,
                        max = 31,
                        value = 1),
        ),

        mainPanel(
           plotOutput("BatteryPlot")
        )
    )
)


server <- function(input, output) {

    output$BatteryPlot <- renderPlot({
      setwd("C:\\Users\\nisha\\Documents\\PassMark\\BatteryMon")
      if(input$month < 10) m <- paste0("0", as.character(input$month)) else m = as.character(input$month)
      if(input$date < 10) d <- paste0("0", as.character(input$date)) else d = as.character(input$date)
      y <- as.character(input$year)
      thedate <- paste0(y, "-", m, "-",  d)
      battery <- tryCatch(read.table(paste0("./batterymonlog ", thedate, ".txt"), header = TRUE, sep =",", skip = 3), 
                          error = function(e) {cat("Error: no log in this date. \n")},
                          warning = function(w) {cat("Warning: no log in this date. \n")})
      if(is.null(battery)) {
        n=10000;
        r=0.7;r_e=(1-r*r)^.5;
        X=rnorm(n);
        Y=X*r+r_e*rnorm(n);
        Y=ifelse(X>0,Y,-Y);
        plot(X,Y,col="pink")
      }else{
        tem = battery$Bat1..Charge
        bat1.Charge = rep(0, length(tem))
        
        for(i in seq_len(length(bat1.Charge))) {bat1.Charge[i] <- as.numeric(str_sub(tem[i], 2, -2))}
        
        tem <- battery$Time
        tem = str_split(str_sub(tem, 2, -1), ":")
        bat1.time <- c()
        for(i in seq_len(length(bat1.Charge))) {bat1.time[i] <- sum(as.numeric(tem[[i]]) * c(3600, 60, 1)) / 3600}
        
        battery <- data.frame(charge = bat1.Charge, time = bat1.time)
        
        plot(bat1.time, bat1.Charge, type = "o", pch = 20, axes = F, xlab = paste0(thedate, " hours"))
        box()
        axis(side = 1, at = 0:24)
        axis(side = 2) 
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
