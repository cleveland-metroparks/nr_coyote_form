#
# This is a Shiny web application that provides a coyote reporting form.
#
# It uses tricks from here:
#  - https://deanattali.com/2015/06/14/mimicking-google-form-shiny/


library(shiny)
library(shinyTime)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(pool)


# Some global variables and functions
#  These need to be updated as form changes
fieldsAll <- c("first_name", "last_name", "phone", "email", 
               "incident_date", "i_time", "ampm", 
               "interaction", "interaction_details", 
               "sighting_information", "sighting_details", 
               "activity_during_sighting", "activity_details", 
               "reservation", "zip", "location_details", 
               "additional_details")
responsesDir <- file.path("responses")

source("loginparams.R")

pool <- dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = DBname,
    host = Host,
    username = User,
    password =  Password,
    port=Port
)
onStop(function() {
    poolClose(pool)
})

epochTime <- function() {
    as.integer(Sys.time())
}

# Set up marking for mandatory fields
fieldsMandatory = c("first_name", "interaction")
labelMandatory = function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}
appCSS <-
    ".mandatory_star { color: red; }
   #error { color: red; }"

# Define UI for application that draws a clickable map
ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    
    titlePanel("Clevelandmetroparks coyote report form"),
    "Mandatory information marked with ",
           labelMandatory(" "),
           " must be entered before you can submit.",
    div(
        id = "form",
        h3("Contact information"),
        textInput("first_name", labelMandatory("First name")),
        textInput("last_name", "Last name"),
        textInput("phone", "Phone number"),
        textInput("email", "Email"),
        h3("Incident information"),
        "ATTENTION: If a coyote came in contact with a human or pet, 
        the visitor should seek medical attention from a 
        doctor/veterinarian immediately!",
        dateInput("incident_date", "Incident date"),
        timeInput("incident_time", "Incident time", seconds = F),
        radioButtons("ampm", "AM or PM", choices = c("AM", "PM"),
                     inline = T,
                     selected = character(0)),
        checkboxGroupInput("interaction", 
                           labelMandatory("Coyote Interaction"),
                           choices = c("Coyote howling",
                                       "Coyote sighting",
                                       "Human attack",
                                       "Pet attack",
                                       "Other"),
                           inline = T,
                           selected = character(0)),
        textAreaInput("interaction_details", 
                      "Additional information",
                      placeholder = "Additional details"),
        checkboxGroupInput("sighting_information", 
                           "Sighting information",
                           choices = c("Single coyote",
                                       "2 or more coyotes",
                                       "Coyote came in contact with humans",
                                       "Coyote came in contact with humans",
                                       "Shy/Cautious",
                                       "Aggressive/Angry",
                                       "Other"),
                           inline = F,
                           selected = character(0)),
        textAreaInput("sighting_details",
                      "Explain choices above",
                      placeholder = "Please give details about any interaction"),
        checkboxGroupInput("activity_during_sighting",
                           "What were you doing at time of sighting?",
                           choices = c("Alone",
                                       "With children",
                                       "with other adults",
                                       "Bicycling",
                                       "On horseback",
                                       "Tending horses",
                                       "Walking dog(s) on leash",
                                       "Walking dog(s) off leash",
                                       "Other"),
                           inline = F,
                           selected = character(0)),
        textAreaInput("activity_details",
                      "What were you doing at time of sighting?",
                      placeholder = "Please give details about your actions when sighting occurred."),

        h3("Incident location"),
        selectInput("reservation",
                    "Reservation (if applicable)",
                    choices = c("None chosen",
                                "Acacia",
                                "Bedford",
                                "Big Creek",
                                "Bradley Woods",
                                "Brecksville",
                                "Brookside",
                                "Euclid Creek",
                                "Garfield Park",
                                "Hinckley",
                                "Huntington",
                                "Lakefront",
                                "Mill Stream Run",
                                "North Chagrin",
                                "Ohio and Erie Canal",
                                "Rocky River",
                                "South Chagrin",
                                "Washington")),
        numericInput("zip", "Sighting zip code (if known)",
                     value = NULL, min = 10000, max = 99999),
        
        "Instructions:  Either", br(),
        "  * click, drag, and zoom map to location", br(),
        "or", br(),
        "  * type address with city into search bar,", br(),
        "then click location on map to record coordinates of sighting.", br(),
        "Click layers button toggle between map and image backgrounds.",
        
        leafletOutput("map1", "50%", 500),

        verbatimTextOutput("Click_text"),
        textAreaInput("location_details", 
                      "Additional location information",
                      placeholder = "Please add any additional location details."),
        
        h3("Additional information"),
        "Describe the coyote(s) including size, color, 
        coat condition, tail position. What did the coyote do? Where 
        did it go? What did you do? If an attack or confrontation 
        occurred, describe the number of victims (human or pet), 
        age, and sex of victims(s), the activity of the coyote(s) 
        prior to the attack, the activity of the victim(s) prior 
        to the attack, defensive actions taken by victim(s) or 
        bystanders, and any injuries sustained.",
        textAreaInput("additional_details", 
                      "Any additional information",
                      placeholder = "Additional details"),
        
        actionButton("submit", "Submit", class = "btn-primary"),
        shinyjs::hidden(
            span(id = "submit_msg", "Submitting..."),
            div(id = "error",
                div(br(), tags$b("Error: "), span(id = "error_msg"))
            )
        )
    ),
    shinyjs::hidden(
        div(
            id = "thankyou_msg",
            h3("Thanks, your response was submitted successfully! You may be contacted for further information."),
            actionLink("submit_another", "Submit another response")
        )
    )
    

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
server = function(input, output) {

    output$map1 = renderLeaflet(map)
    output$i_time = renderText(strftime(input$incident_time, "%R"))
    Latitude = reactiveVal()
    Longitude = reactiveVal()

    observe({
        mandatoryFilled <-
            vapply(fieldsMandatory,
                   function(x) {
                       !is.null(input[[x]]) && input[[x]] != ""
                   },
                   logical(1))
        mandatoryFilled <- all(mandatoryFilled)
        
        shinyjs::toggleState(id = "submit", 
                             condition = mandatoryFilled)
    })    
    
    observe({
        click = input$map1_click
        if(is.null(click))
            return()
        cl_lat = reactiveVal(click$lat)
        cl_lng = reactiveVal(click$lng)
        text<-paste("Latitude: ", cl_lat(), 
                    ", Longitude: ", cl_lng())
        text2<-paste("You've selected point ", text)
        map1_proxy = leafletProxy("map1") %>%
            clearPopups() %>%
            addPopups(cl_lng(), 
                      cl_lat(), 
                      text)
# This is needed to get a value out of a reactive
        Latitude(cl_lat())
        Longitude(cl_lng())

        output$Click_text<-renderText({
            text2
        })
    
        
    })

    formData <- reactive({
        data <- sapply(fieldsAll, function(x) input[[x]])
        data <- c(data, 
                  Latitude = Latitude(),
                  Longitude = Longitude(),
                  timestamp = epochTime())
        data <- t(data)
        data
    })
    
    humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
    
    saveData <- function(data) {
        fileName <- sprintf("%s_%s_%s.csv", #"%s_%s_%s_%s.csv"
                            input$first_name,
                            input$last_name,
                            humanTime()#, 
# use line below if you worry about same username/same second collisions
                            # digest::digest(data)
                            )
        
        write.csv(x = data, file = file.path(responsesDir, fileName),
                  row.names = FALSE, quote = TRUE)
    }
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
        shinyjs::disable("submit")
        shinyjs::show("submit_msg")
        shinyjs::hide("error")
        
        tryCatch({
            saveData(formData())
            shinyjs::reset("form")
            shinyjs::hide("form")
            shinyjs::show("thankyou_msg")
        },
        error = function(err) {
            shinyjs::html("error_msg", err$message)
            shinyjs::show(id = "error", anim = TRUE, animType = "fade")
        },
        finally = {
            shinyjs::enable("submit")
            shinyjs::hide("submit_msg")
        })
    })
    
    observeEvent(input$submit_another, {
        shinyjs::show("form")
        shinyjs::hide("thankyou_msg")
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
