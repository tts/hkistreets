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
                selectInput(inputId = "range",
                            label = "Street length",
                            choices = c("Short, i.e. only 0 to 1 addresses",
                                        "Under 500 m","500-1000 m", "1-3 km", "Over 3 km",
                                        "All"),
                            selected = "All"),
                selectInput(inputId = "lang", 
                            label = "Choose language", 
                            choices = c("Finnish", "Swedish"),
                            selected = "Finnish"),
                selectizeInput(inputId = "streets", 
                               label = "Select up to 10 streets by name", 
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
    if ( (input$range) == "All" )  
      return(coord)
    semi_join(coord, len[len$range == input$range,], by = "katunimi")
  })
  

  observe(
    updateSelectizeInput(session, 
                         inputId = 'streets', 
                         choices = if ( input$lang == 'Finnish' ) filteredData()$katunimi else filteredData()$gatan
    )
  )
 
   
  streetData <- reactive({
    if ( is.null(input$streets) )
      return(filteredData())
    
    isolate(filteredData()[if ( input$lang == 'Finnish' ) filteredData()$katunimi %in% input$streets
                           else filteredData()$gatan %in% input$streets, ])
    
  })
  
  
  icon.ion <- makeAwesomeIcon(icon = 'android-walk', 
                              markerColor = 'yellow',
                              library='ion')
  

  observeEvent(c(input$range,input$streets, input$lang),{
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(data = streetData(),
                        lng  = ~e,
                        lat  = ~n,
                        icon = icon.ion,
                        popup = ~paste0(if ( input$lang == 'Finnish' ) katunimi else gatan, ' ',osoitenumero, '<br/>',
                                        if ( input$lang == 'Finnish' ) gatan else katunimi, ' ',osoitenumero, '<br/>',
                                      #  'Length (approx): m<br/>',
                                       '<a href="', gviewurl, '">Google Street View</a>'),
                        clusterOptions = markerClusterOptions()) %>%
      fitBounds(.,
                min(streetData()$e), min(streetData()$n),
                max(streetData()$e), max(streetData()$n))
  })
  

}

shinyApp(ui, server)