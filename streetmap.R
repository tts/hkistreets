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
                            choices = c("Short, only 1 address",
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
                               options = list(maxItems = 10)),
                downloadButton("data_hki_streets", "Download"),
                HTML("<div><br/><a target='blank' href='http://tuijasonkkila.fi/blog/2018/01/streets-of-helsinki/'>About</a></div>")
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(data) %>%
      addTiles(urlTemplate = "https://cdn.digitransit.fi/map/v1/hsl-map/{z}/{x}/{y}.png",
               attribution = 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors 
                       <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>. 
                       Data source: <a href="http://www.hri.fi/fi/dataset/helsingin-osoiteluettelo">Addresses of the city of Helsinki</a> 
                       <a href="http://creativecommons.org/licenses/by/4.0/deed.fi">CC BY 4.0</a>') %>% 
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
    filteredRange()[filteredRange()[[isolate(input$lang)]] %in% input$streets, ]
    
  })
  
  
  icon.ion <- makeAwesomeIcon(icon = 'android-walk', 
                              markerColor = 'orange',
                              library='ion')
  

  observeEvent(c(input$range, input$streets),{
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(data = filteredStreets(),
                        lng  = ~e,
                        lat  = ~n,
                        icon = icon.ion,
                        popup = ~paste0('<b>', Finnish, ' ' , osoitenumero, '</b><br/>',
                                        '<b>', Swedish, ' ', osoitenumero, '</b><br/>',
                                        'Length, approx: ', m, ' m<br/>',
                                        '<i>0 m</i> means that there is only one address on that street', '<br/>',
                                        '<a target="blank" href="', gviewurl, '">Google Street View (if available)</a>'),
                        clusterOptions = markerClusterOptions()) %>%
      fitBounds(.,
                min(filteredStreets()$e), min(filteredStreets()$n),
                max(filteredStreets()$e), max(filteredStreets()$n))
  })
  
  
  output$data_hki_streets = downloadHandler(
    filename = function() {
      filename = 'data_hki_streets.csv'
    },
    content = function(file) {
     {
        write.csv(filteredStreets(), 'temp.csv', row.names = FALSE)
        file.rename('temp.csv', file)    
      } 
    }
  )

}

shinyApp(ui, server)