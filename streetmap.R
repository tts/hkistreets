library(shiny)
library(leaflet)
library(dplyr)

coord <- readRDS("coord.RDS")
len <- readRDS("length.RDS")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(bottom = 40, right = 20,
                draggable = TRUE,
                checkboxGroupInput(inputId = "range",
                                   label = "Length of the street",
                                   choices = list("Tiny (0 m)" = "Zero",
                                                  "Short (-500 m)" = "Under 500 m",
                                                  "Medium (500-1000 m)" = "500-1000 m",
                                                  "Long (1000-3000 m)" = "1-3 km",
                                                  "Extra long (3000- m)" = "Over 3 km"),
                                   selected = NULL),
                selectizeInput(inputId = 'streets', 
                               label = 'Streets', 
                               choices = NULL, 
                               options = list(maxItems = 10))
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    leaflet(coord) %>%
      addTiles() %>%
      fitBounds(~min(e), ~min(n), ~max(e), ~max(n))
  })
  
  
  filteredData <- reactive({
    if ( is.null(input$range) )  
      return(coord)
    semi_join(coord, len[len$range == input$range,], by = "katunimi")
  })
  
 
  streetData <- reactive({
    if ( is.null(input$streets) )
      return(filteredData())
    
    isolate(filteredData()[filteredData()$katunimi %in% input$streets, ])
    
  })
  
  
  observe(
    updateSelectizeInput(session, 
                         inputId = 'streets', 
                         choices = filteredData()$katunimi 
    )
  )
  
  
  icon.ion <- makeAwesomeIcon(icon = 'android-walk', 
                              markerColor = 'green',
                              library='ion')
  
  
  observeEvent(c(input$range,input$streets),{
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(data = streetData(),
                        lng  = ~e,
                        lat  = ~n,
                        icon = icon.ion,
                        popup = ~paste0(katunimi, " ", osoitenumero),
                        clusterOptions = markerClusterOptions()) %>%
      fitBounds(.,
                min(streetData()$e),min(streetData()$n),
                max(streetData()$e),max(streetData()$n))
  })
  
  
}

shinyApp(ui, server)