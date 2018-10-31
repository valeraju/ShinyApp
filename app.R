library(shiny)
library(ggplot2)
library(anytime)
library(shinyalert)
library(shinythemes)
library(stringr)
library(scales)
library(lubridate)
library(devtools)
library(shinydashboard)
library(dplyr)
library(plotly)
library(ygdashboard)
library(DT)
library(leaflet)

# moduleUI <- function(id,state){
#   ns <- NS(id)
#   tagList("OK1","OK2","OK3")
# }

module <- function(input, output, session){}

ui <- dashboardPage(
  skin = "black",
  title = "DataDisney",
  collapse_sidebar = TRUE,
  
  dashboardHeader(title="DataDisney Project"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tags$script('
                // Bind function to the toggle sidebar button
                $(".sidebar-toggle").on("click",function(){
                $(window).trigger("resize"); // Trigger resize event
                })'
    ),
    tabBox(
      title = "",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabs", height = "100%", width="100%", selected = "Custom",
      tabPanel("Live"),
      tabPanel("Last 7 days"),
      tabPanel("Last 12 months"),
      tabPanel("Last 5 years"),
      tabPanel("Custom",
               fluidRow(
                 column(7,
                        tags$head(tags$style(HTML(".small-box {height: 60px}"))),
                        valueBox(value = tags$p("- min", style = "font-size: 1vw;"),"Max", width = 3, color = "green"),
                        valueBox(value = tags$p("- min", style = "font-size: 1vw;"),"Min", width = 3, color = "green"),
                        valueBox(value = tags$p("- min", style = "font-size: 1vw;"),"Mean", width = 2, color = "green"),
                        valueBox(value = tags$p("- min", style = "font-size: 1vw;"),"Mode", width = 2, color = "green"),
                        valueBox(value = tags$p("- min", style = "font-size: 1vw;"),"Median", width = 2, color = "green"),
                        fluidRow(
                          box(width = 12, title = "Evolution of the waiting time over time for one or more rides",
                              fluidRow(
                                column(4,
                                       tags$style(type='text/css', "#showButton { width:100%; margin-top: 25px;}"),
                                       tags$style(type = "text/css", "#plotWT {height: calc(50vh - 80px) !important;}"),
                                       dateRangeInput(inputId = "dateRange",
                                                      label = 'Select a date range',
                                                      start = min(df$lastUpdate),
                                                      end = min(df$lastUpdate),
                                                      language = "us",
                                                      format = "yyyy-mm-dd",
                                                      min = min(df$lastUpdate),
                                                      max = max(df$lastUpdate),
                                                      startview = "decade",
                                                      width = 250)),
                                column(4,selectInput(inputId = "nameAtt",label = "Select a theme park or a ride",choices = park_themes, width = 250)),
                                column(4,actionButton("showButton","Show Analysis"))),
                              plotlyOutput("plotWT")
                          )),
                        fluidRow(box(width = 12, title = "Evolution of the number of observations over time for one or more rides",
                                     tags$style(type = "text/css", "#plotOcc {height: calc(50vh - 80px) !important;}"),
                                     plotlyOutput("plotOcc"))),
                        fluidRow(box(width = 12, title = "Waiting Times in real time",
                                     tags$style(type = "text/css", "#realTimeTable {height: calc(50vh - 80px) !important;}"),
                                     DT::dataTableOutput("realTimeTable")))
                 ),
                 column(5,
                        fluidRow(
                          tags$style(type = "text/css", "#map {height: calc(70vh - 80px) !important;}"),
                          tags$style(type = "text/css", "#plotPC {height: calc(50vh - 80px) !important;}"),
                          tags$style(type = "text/css", "#minTable {height: calc(40vh - 80px) !important;}"),
                          tags$style(type = "text/css", "#maxTable {height: calc(40vh - 80px) !important;}"),
                          box(width = 12, title = "Map of rides with their waiting time",
                              leafletOutput("map", width = "100%", height = "100%"))),
                        fluidRow(box(title="Percentage of availability of attractions", width=12, plotlyOutput("plotPC"))),
                        fluidRow(
                          column(6, box(width = NULL,title="Rides with the shortest waiting time", tableOutput("minTable"))),
                          column(6, box(width = NULL,title="Rides with the longest waiting time", tableOutput("maxTable")))
                        )
                 )
               )
      )
    )),
  
  dashboardFooter(mainText = "My footer", subText = "2018"),
  
  dashboardControlbar()
  )

server <- function(input,output) {
  
  plotWaitTimes <- reactive({
    plot <- head(dfPlotInit,50)
    ggplotly(ggplot(data=plot, aes(x=lastUpdate, y=waitTime, fill = name)) +
               # geom_smooth(method = "auto", se = F, color="#FF4B2B", size = 0.3) +
               scale_x_datetime(
                 breaks = seq(min(plot$lastUpdate),max(plot$lastUpdate),by="4 hours"),
                 labels = date_format("%H:%M"),
                 limits = c(min(plot$lastUpdate)-hours(1),max(plot$lastUpdate)+hours(1))
               ) +
               # scale_fill_gradient(low = "#6be585", high = "#dd3e54") +
               geom_bar(stat="identity", na.rm = TRUE) +
               labs(x="Time", y="Waiting Time (min)") +
               theme_bw())
    # plot_ly(data = plot, x = ~lastUpdate, y = ~waitTime, color = ~name, type = "bar")
  })
  
  plotPieChart <- reactive({
    plot <- head(df,500)
    plot_ly(df, labels = ~status, type = 'pie')
  })
  
  plotOcc <- reactive({
    plot_ly(data = dfOcc, x = ~lastUpdate, y = ~nbOcc, color = ~name, type = "bar")
  })
  
  output$plotWT <- renderPlotly({
    plotWaitTimes()
  })
  
  output$plotPC <- renderPlotly({
    plotPieChart()
  })
  
  output$plotOcc <- renderPlotly({
    plotOcc()
  })
  
  output$maxTable <- renderTable({
    test_max <- aggregate(waitTime ~ name, head(dfPlotInit,50), max)
    head(test_max[order(-test_max$waitTime),],3)
  }, width = "100%", rownames = F, colnames = T,spacing = 'l',striped = TRUE,align = 'c')
  
  output$minTable <- renderTable({
    test_min <- aggregate(waitTime ~ name, head(dfPlotInit,50), min)
    head(test_min[order(test_min$waitTime),],3)
  }, width = "100%", rownames = F, colnames = T,spacing = 'l',striped = TRUE,align = 'c')
  
  output$realTimeTable <- DT::renderDataTable({
    cols = c("id","name","status","waitTime")
    DT::datatable(dfRealTime[,cols], options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  map <- reactive({
    leaflet() %>%
      setView(lat=28.388727, lng=-81.57713339999997, zoom=13.3) %>%
      addTiles() %>%
      addCircleMarkers(lng = dfCoordAgg$longitude, lat = dfCoordAgg$latitude, popup =  dfCoordAgg$legend, clusterOptions = markerClusterOptions())
    #      addCircleMarkers(lng = dfTest2$lng, lat = dfTest2$lat,popup = df$legend,fillOpacity = 0.5)
  })
  
  output$map <- renderLeaflet({
    map()
  })
  
}

shinyApp(ui=ui,server=server)