library(shiny)
library(leaflet)
library(dplyr)

data <- readRDS("strdata.RDS")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(bottom = 40, right = 20,
                draggable = TRUE,
                selectInput(inputId = "range",
                            label = "Street length",
                            choices = c("Short, i.e. only 0 to 1 addresses",
                                        "Under 500 m", "500-1000 m", "1-3 km", "Over 3 km",
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
    leaflet(data) %>%
      addTiles() %>%
      fitBounds(~min(e), ~min(n), ~max(e), ~max(n))
  })
  
  filteredRange <- reactive({
    if ( (input$range) == "All" )  
      return(data)
    data %>% 
      filter(range == input$range)
  })
  
  observe(
    updateSelectizeInput(session, 
                         inputId = 'streets', 
                         choices = filteredRange()[[input$lang]]
    )
  )
 
   
  filteredStreets <- reactive({
    if ( is.null(input$streets) )
      return(filteredRange())
    filteredRange()[filteredRange()[[isolate(input$lang)]] == input$streets, ]
    
  })
  
  
  icon.ion <- makeAwesomeIcon(icon = 'android-walk', 
                              markerColor = 'yellow',
                              library='ion')
  

  observeEvent(c(input$range, input$streets),{
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(data = filteredStreets(),
                        lng  = ~e,
                        lat  = ~n,
                        icon = icon.ion,
                        popup = ~paste0('<b>', if ( input$lang == 'Finnish' ) Finnish else Swedish, ' ', osoitenumero, '</b>', '<br/>',
                                        if ( input$lang == 'Finnish' ) Swedish else Finnish, ' ', osoitenumero, '<br/>',
                                       'Length (approx): ', m, 'm<br/>',
                                       '<a href="', gviewurl, '">Google Street View (could be missing)</a>'),
                        clusterOptions = markerClusterOptions()) %>%
      fitBounds(.,
                min(filteredStreets()$e), min(filteredStreets()$n),
                max(filteredStreets()$e), max(filteredStreets()$n))
  })
  

}

shinyApp(ui, server)