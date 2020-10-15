#
# This is a Shiny web application that provides a coyote reporting form.
#

library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)


# Define UI for application that draws a clickable map
ui <- fluidPage(

    "Clevelandmetroparks coyote report form",

    fluidRow(leafletOutput("map1", "50%", 500)),
    fluidRow(verbatimTextOutput("Click_text")),
    fluidRow(textInput("first_name", "First name"))

)

# Define map and layer controls
map = leaflet() %>%
    addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>',
        group = "Mapbox") %>%
#           addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI WorldImagery") %>%
    setView(lng = -81.65, lat = 41.38, zoom = 10) %>%
    addLayersControl(baseGroups = c("Mapbox",
#                                    "Open Topo Map",
                                    "ESRI WorldImagery"),
                     position = "bottomright") %>%
    leaflet.extras::addSearchOSM(options = searchOptions(collapsed = FALSE))


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map1 = renderLeaflet(map)
    
    observe({
        click = input$map1_click
        if(is.null(click))
            return()
        text<-paste("Latitude: ", click$lat, ", Longtitude: ", click$lng)
        text2<-paste("You've selected point ", text)
# This works in Rstudio, but not on server since websockets are not working
        # map1_proxy = leafletProxy("map1") %>%
        #     clearPopups() %>%
        #     addPopups(click$lng, click$lat, text)
        output$Click_text<-renderText({
            text2
        })
    
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
