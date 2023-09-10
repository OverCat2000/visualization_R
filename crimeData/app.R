#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggmap)
library(ggplot2)
library(dplyr)
library(plotly)
library(TSstudio)
library(xts)
ggmap::register_google(key = "AIzaSyCKb1WLoI5A3Lme4MveEOSujqPl4E5LfPw", write = TRUE)


act = unique(myCrime$crimeType)
choicheAct = setNames(1:9, act)
area = unique(myCrime$`AREA NAME`)

ui <- fluidPage(
# ui <- navbarPage(

    titlePanel("Crime data"),
# 
#     sidebarLayout(
#         sidebarPanel(
#             selectInput("select",
#                         h3("crime"),
#                         choice = act,
#                         selected = 1),
# 
#         sliderInput("slider",
#                    h3("sample slider"),
#                    min = 0, max = 100, value = 10)
#             ),
#         
#         mainPanel(
#           tabsetPanel(type = "tab",
#                       tabPanel("mapping",
#                                plotOutput("map")
#                                ),
#                       tabPanel("time series",
#                                plotlyOutput("ts")
#                       )
#           )
#         )
#     )
# )

  tabsetPanel(type = "tab",
              tabPanel("mapping",
                       sidebarPanel(
                         selectInput("select",
                                     h3("crime"),
                                     choice = act,
                                     selected = 1),
                         sliderInput("slider",
                                     h3("sample slider"),
                                       min = 0, max = 100, value = 5)
                       ),
                       mainPanel(
                         plotOutput("map")
                       )
                ),
              tabPanel("time series",
                       sidebarPanel(
                         checkboxGroupInput("check",
                                            h3("crime"),
                                            choices = choicheAct,
                                            selected = 1)
                       ),
                       mainPanel(
                         plotlyOutput("ts")
                         # textOutput("test")
                       )
              ),
              tabPanel("area map",
                       sidebarPanel(
                         selectInput("select2",
                                     h3("crime"),
                                     choice = act,
                                     selected = 1),
                         selectInput("select3",
                                     h3("area"),
                                     choice = area,
                                     selected = 1)
                       ),
                       mainPanel(
                         plotOutput("areaMap")
                         # renderText("test")
                       )
              )
  )
           
)


server <- function(input, output) {
  
    crimeReact <- reactive({
      myCrime %>%
        filter(crimeType == input$select) %>%
        filter(LON != 0 & LAT != 0) %>%
        sample_frac(input$slider / 100)
    })
    
    crimeReact2 <- reactive({
      myCrime %>%
        filter(crimeType == input$select2) %>%
        filter(`AREA NAME` == input$select3) %>%
        filter(LON != 0 & LAT != 0) 
        # sample_frac(input$slider / 100)
    })
    
    # coordReact <- reactive({
    #   coord %>%
    #     filter(`AREA NAME` == input$select3)
    # })
    
    output$map <- renderPlot({
      get_googlemap(center = "los Angeles") %>%
        ggmap() +
        geom_point(data = crimeReact(),
                   aes(x = LON, y = LAT), color = "tomato") +
        labs(title = "arson crime map of los angeles")
    },
    {width = 900},
    {height = 800})
    
    output$ts <- renderPlotly({
      ts_plot(mer[, as.integer(input$check)],
              slider = T,
              Xgrid = T)
      
    })
    
    output$areaMap <- renderPlot({
      gmap = get_googlemap(center = c(x = as.numeric(coord[coord$`AREA NAME` == input$select3, ][2]),
                               y = as.numeric(coord[coord$`AREA NAME` == input$select3, ][3]))
                               , zoom = 12) %>%
        ggmap()
      gmap +
        geom_point(data = crimeReact2(),
                   aes(x = LON, y = LAT),
                   color = "tomato")
    },
      {width = 900},
      {height = 800})
                   
    
    # output$test <- renderText({
    #   temp = coordReact()
    #   print(temp)
    # })

    }






# Run the application 
shinyApp(ui = ui, server = server)
