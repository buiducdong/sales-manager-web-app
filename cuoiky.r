
library(ggplot2)
library(shiny)
library(shinydashboard)
library(MASS)
library(haven)
library(ggthemes)
library(zoo)
library(lubridate)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggrepel)
library(plotly)
library(DT)
library(readxl)
library(scales)
df <- read.csv('D://DataAnalysis/cuoiky/sales_data.csv')

team <- factor(df$Team)

#doi dinh dang dat ten de hien thi len giao dien 
colName1 <- c("January" = "01", 
              "February" = "02",
              "March" = "03",
              "April" = "04",
              "May" = "05",
              "June" = "06",
              "July" = "07",
              "August" = "08",
              "September" = "09",
              "October" = "10",
              "November" = "11",
              "December" = "12")

colName2 <- c("Quarter 1" = "1",
              "Quarter 2" = "2",
              "Quarter 3" = "3",
              "Quarter 4" = "4")

#tao danh sach cac team
teamNames <- c("SB IB","MM IB","ENT IB","SB ACQ","MM ACQ","ENT ACQ")

#formatting to display correctly on front end 
col_alias <- function(x) {switch(x,
                                 "01" = "January",
                                 "02" = "February",
                                 "03" = "March",
                                 "04" = "April",
                                 "05" = "May",
                                 "06" = "June",
                                 "07" = "July",
                                 "08" = "August",
                                 "09" = "September",
                                 "10" = "October",
                                 "11" = "November",
                                 "12" = "December")}

col_alias2 <- function(x) {switch(x,
                                  '1' = "Quarter 1",
                                  '2' = "Quarter 2",
                                  '3' = "Quarter 3",
                                  '4' = "Quarter 4")}


#start of dashboard 
ui = dashboardPage( skin = "blue",
                    dashboardHeader( title = "Sales Analytics"), 
                    dashboardSidebar(
                      sidebarMenu(
                        #upload file option
                        fileInput("myFile",
                                  "Upload Excel Data:",
                                  accept = ".xlsx"),
                        #lua chon che do xem
                        radioButtons("timeView", "Select View:", choices = 
                                       c("Monthly", "Quarterly", "Yearly")),
                        #lua chon nam
                        radioButtons("yearOption1", "Select Year 1:", choices = 
                                       c("2011","2012","2013","2014","2015","2016","2017","2018"), selected = "2018"),
                        selectInput("team1", "Select Team 1:", choices = teamNames),
                        #cho lua chon thang neu o input o tren = monthly
                        conditionalPanel(condition = 'input.timeView == "Monthly"',
                                         selectInput("month1", "Select Month 1:", choices = 
                                                       colName1, selected = "01")),
                        #cho lua chon quy neu o input o tren = quarterly
                        conditionalPanel(condition = 'input.timeView == "Quarterly"',
                                         selectInput("quarter1", "Select Quarter 1:", choices = 
                                                       colName2, selected = "1")),
                        #lua chon team 2
                        conditionalPanel(condition = 'input.check1 == "1"',
                                         selectInput("team2", "Select Team 2:", choices = levels(team))),
                        #cac tuy chon cua nam team 2
                        conditionalPanel(condition = 'input.check1 == "1"',
                                         radioButtons("yearOption2", "Select Year 2:", choices = 
                                                        c("2011","2012","2013","2014","2015","2016","2017","2018"), selected = "2017")),
                        #lua chon thang cho team 2
                        conditionalPanel(condition = 'input.check1 == "1" & input.timeView == "Monthly"',
                                         selectInput("month2", "Select Month 2:", choices = 
                                                       colName1,selected = "02")),
                        #lua chon quy cho team 2
                        conditionalPanel(condition = 'input.check1 == "1" & input.timeView == "Quarterly"',
                                         selectInput("quarter2", "Select Quarter 2:", choices = 
                                                       colName2, selected = "2")),
                        #tao checkbox de kiem tra hanh dong so sanh
                        checkboxInput("check1","Make a comparison", value = FALSE)
                      )
                    ),
                    #body layout of dashboard 
                    dashboardBody(
                      fluidRow(
                        #bieu do cot
                        box(width = 6, plotlyOutput("hist1")),
                        #bieu do donut
                        box(width = 6, plotlyOutput("donut1")),
                        #sum of 0-99% 
                        valueBoxOutput("productivityBox1"),
                        #tong doanh so
                        valueBoxOutput("productivityBox1.1"),
                        #sum of 100% 
                        valueBoxOutput("productivityBox1.2"),
                        #bang du lieu
                        box(width = 12,dataTableOutput("table1"))
                        
                      )
                    ))


#hien thi
server = function(input, output) {
  
  #uploading file
  a1 = reactive({
    req(input$myFile)
    read_excel(input$myFile$datapath)
  })
  
  #chon du lieu cho dau vao theo thang/name/quy
  newdata <- reactive({
    a1 <- a1()
    x <- as.Date(a1$Month)
    
    a1$mo <- strftime(x, "%m")
    a1$yr <- strftime(x, "%Y")
    a1$qrt <- quarter(x, with_year = FALSE, fiscal_start = 01)
    
    #tap hop du lieu de hien thi
    newdata <- a1[grepl("Y", a1$`Quota Held Flag`),]
    
    #converting the participation column to  categorical for donut chart
    newdata$Participation[is.na(newdata$Participation)] <- 0
    newdata$Participation <- factor(newdata$Participation, labels =
                                      c("0-99%","100%"))
    
    #grouping data
    newdata %>%
      group_by(yr, mo, qrt)
    
    
  })
  
  #tao tam du lieu khi phan tich cho 1 nhom
  newdata2 <- reactive({
    newdata2 <- newdata ()
    
    if(input$timeView == 'Monthly'){
      return(newdata2 %>%
               filter(yr == input$yearOption1 & mo == input$month1 & Team == input$team1))
    }
    
    if(input$timeView == 'Quarterly'){
      return(newdata2 %>%
               filter(yr == input$yearOption1 & qrt == input$quarter1 & Team == input$team1))
    }
    else{
      return(newdata2 %>%
               filter(yr == input$yearOption1 & Team == input$team1))
    }
  })
  
  #tao tap du lieu neu so sanh 2 team 
  newdata3 <- reactive({
    newdata3 <- newdata ()
    
    if(input$timeView == 'Monthly'){
      return(newdata3 %>%
               filter(yr == input$yearOption2 & mo == input$month2 & Team == input$team2))
    }
    
    if(input$timeView == 'Quarterly'){
      return(newdata3 %>%
               filter(yr == input$yearOption2 & qrt == input$quarter2 & Team == input$team2))
    }
    else{
      return(newdata3 %>%
               filter(yr == input$yearOption2 & Team == input$team2))
    }
  })
  
  #start of histogram
  p3 <- reactive({
    newdata2 <- newdata2()
    newdata3 <- newdata3()
    
    if(input$check1 == 'FALSE'){
      return(
        #for single view 
        ggplot() +
          geom_histogram(data = newdata2, aes(x =`Attainment Bucket`, 
                                              text = paste("Count:",..count..)), 
                         fill = "#3486f9", color = "#635f5f", stat = "count")+
          scale_x_discrete(limits=c("0-29%","30-69%","70-89%","90-99%",
                                    "100-200%","200-300%","+300%")) +
          geom_text(stat = "count", aes(label = ..count..,y = ..count..), 
                    vjust = 1.75,  
                    size = 5,
                    font = 3,
                    color = "white") +
          theme_bw() +
          labs(x="Attainment Buckets",
               title = paste(input$team1))
      ) 
    }
    else{
      return(
        #for comparing teams
        ggplot() +
          geom_histogram(data = newdata2, aes(x =`Attainment Bucket`,
                                              text = paste(input$team1,
                                                           "count:", ..count..)), 
                         fill = "#3486f9", stat = "count", position="identity", alpha = .8)+
          geom_histogram(data = newdata3, aes(x =`Attainment Bucket`, 
                                              text = paste(input$team2,
                                                           "count:", ..count..)), 
                         fill = "#635f5f", stat = "count", position="identity", alpha = .8)+
          scale_x_discrete(limits=c("0-29%","30-69%","70-89%","90-99%",
                                    "100-200%","200-300%","+300%"))+
          scale_fill_manual(name="Legend", values = c("#635f5f","#3486f9")) +   
          theme_bw() +
          labs(x="Attainment Buckets",
               title = paste(input$team1, "-",input$team2))
      )
    }
  })
  
  #key data output box 1 
  output$productivityBox1 <- renderValueBox({
    
    Sum <- sum(newdata2()$'Bookings'[which(newdata2()$'Participation' == '0-99%')], na.rm = T)
    
    valueBox(prettyNum(paste0("$", sprintf("%.2f",Sum)), big.mark = ","),
             subtitle = "Sum of 0-99% Participation:",
             icon = icon("dollar-sign", lib = "font-awesome"),
             color = "blue")
  })
  
  #key data output box 2
  output$productivityBox1.1 <- renderValueBox({
    
    avg <- mean(newdata2()$'Bookings', na.rm = T)
    
    valueBox(prettyNum(paste0("$", sprintf("%.2f",avg)), big.mark = ","),
             subtitle = "Average Productivity:",
             icon = icon("money-bill-wave", lib = "font-awesome"),
             color = "blue")
  })
  
  #key data output box 3
  output$productivityBox1.2 <- renderValueBox({
    
    Sum <- sum(newdata2()$'Bookings'[which(newdata2()$'Participation' == '100%')], na.rm = T)
    
    valueBox(prettyNum(paste0("$", sprintf("%.2f",Sum)), big.mark = ","),
             subtitle = "Sum of 100% Participation:",
             icon = icon("hand-holding-usd", lib = "font-awesome"),
             color = "blue")
  })
  
  
  output$hist1 <- renderPlotly({
    
    #friendly message to instruct user of how the app works
    text <- paste("chon file cua ban")
    
    validate(
      #only show above message when there is not a file uploaded 
      need(!is.null(input$myFile), text)
    )
    
    ggplotly(p3(), tooltip = "text") %>% config(displayModeBar = F) %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE)) %>%
      layout(showlegend = T)
    
  })
  
  #start of donut plot
  output$donut1 <- renderPlotly ({
    
    #grouping data by the categorical variable Participation 
    newdata2 <- newdata2() %>%
      group_by(Participation) %>%
      summarize(count = n())
    
    newdata3 <- newdata3() %>%
      group_by(Participation) %>%
      summarize(count = n())
    #for team single view 
    if(input$check1 == 'FALSE'){
      
      return(plot_ly(newdata2, labels = ~Participation, values = ~ count, type = "pie", hole = 0.6,
                     marker = list(colors = c('#635f5f','#3486f9'),line = list(color = '#635f5f', width = 2)))%>%
               layout(title = "Participation",  showlegend = T,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                   showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                   showticklabels = FALSE)) %>%
               config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% 
               layout(yaxis=list(fixedrange=TRUE)))
    }
    else{
      
      #for team comparison 
      return(plot_ly(newdata2, labels = ~Participation, values = ~ count, type = "pie", hole = 0.6,
                     marker = list(colors = c('#635f5f','#3486f9'),line = list(color = '#635f5f', width = 2)),
                     name = paste(input$team1,col_alias(input$month1),input$yearOption1),
                     domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
               add_trace(data = newdata3, labels = ~Participation, values = ~ count, type = "pie", hole = 0.6,
                         name = paste(input$team2,col_alias(input$month2),input$yearOption2),
                         domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
               layout(title = "Participation",  showlegend = T,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                   showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                   showticklabels = FALSE)) %>%
               config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% 
               layout(yaxis=list(fixedrange=TRUE)))
    }
    
    
  })
  
  #raw data table start 
  output$table1 <- renderDataTable ({
    
    datatable(newdata2()[,c(2:5,8:9)], options = list(pageLength = 5)) %>% 
      formatCurrency(c(2:3),'$') %>%
      formatPercentage(4,2)
    
  })
  
  
}

shinyApp(ui = ui, server = server)

