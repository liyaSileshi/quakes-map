#Liya Tilahun

#importing libraries
library(shiny)
library(leaflet)
library(RColorBrewer)
library(RCurl)
library(RJSONIO)
library(rsconnect)

#reads in the JSON that has earthquake information depending on the selection
#of "TimeFrame" in the UI of the app 
web<- function(name)
{
  if (name == "Past Hour") 
    source <- getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson")
  else if (name == "Past Day")
    source <- getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_day.geojson")
  else if (name == "Past Week") 
    source <-getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_week.geojson")
  else if (name == "Past 30 Days") 
    source <- getURL("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/4.5_month.geojson")
  
  
  #convert from a JSON data to R object
  myQuakes = fromJSON(source,simplify=TRUE, nullValue=NA)
  
  #cleaning the data
  quakesList = myQuakes[["features"]] #use subsetting to extract an element
  numRows = length(quakesList)
  #change it to a dataframe
  quakesdf = data.frame(matrix(unlist(quakesList), nrow=numRows, byrow=T),
                        stringsAsFactors = FALSE)
  quakesdf <- quakesdf[,-1]
  quakesdf <- quakesdf[,-2:-27]
  quakesdf <- quakesdf[,-5]
  colnames(quakesdf) = c("mag", "long", "lat", "depth")
  
  #change it to a numeric data type
  quakesdf[] <- lapply(quakesdf, function(x) as.numeric(as.character(x)))
  return(quakesdf)
}

quakesdf<-web("Past Week")
quakesdf


#user interface
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(quakesdf$mag), max(quakesdf$mag),
                            value = range(quakesdf$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                selectInput("TimeFrame", "TimeFrame",c("Past Day", "Past Week", "Past 30 Days", "Past Hour"),"Past 30 Days"),
                checkboxInput("legend", "Show legend", TRUE)
                
  )
)



#server
server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  
  
  filteredData <- reactive({
    quakesdf <- web(input$TimeFrame)
    quakesdf[quakesdf$mag >= input$range[1] & quakesdf$mag <= input$range[2],]
    
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakesdf$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakesdf) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakesdf)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)


