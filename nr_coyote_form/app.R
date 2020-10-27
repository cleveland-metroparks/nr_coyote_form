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
library(dbplyr)
# library(pool)
library(DBI)

# Some global variables and functions
#  These need to be updated as form changes

#  We may need to eliminate this method 
#    if all lists need to be reformatted like date etc.
fieldsAll <- c("first_name", "last_name", "phone", "email",
               "ampm", "concern", "concern_type", 
               "concern_details", "in_metroparks", "zip",
               "location_details",
               "activity_during_sighting", "activity_details", 
               "trail",
               "sighting_information", "sighting_details", 
               "additional_details")
responsesDir <- file.path("responses")
db_table_out = "rshiny_test_form_2"

# To run in the console, use this one.
# source("nr_coyote_form/loginparams_shiny.R")
# This is where app expects it when running locally or on the server
source("loginparams_shiny.R")

con = dbConnect(
    drv = RPostgres::Postgres(),
    # drv = RPostgreSQL::PostgreSQL(),
    dbname = DBname,
    host = Host,
    user = User,
    password = Password,
    port=Port
)
onStop(function() {
    dbDisconnect(con)
    rm(User, Password, pos = ".GlobalEnv")
})

epochTime <- function() {
    as.integer(Sys.time())
}

# Set up marking for mandatory fields
fieldsMandatory = c("first_name", "concern")
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
    "Mandatory information marked with red star",
           labelMandatory(" "),
           " must be entered before you can submit.", br(),
    div(
        id = "form",
        h3("Contact information"),
        "If you would like to be contacted by wildlife staff,
        please enter a phone number or email.", br(),
        textInput("first_name", labelMandatory("First name")),
        textInput("last_name", "Last name"),
        textInput("phone", "Phone number"),
        textInput("email", "Email"),
        
        h3("Incident information"),
        tags$span(style="color:red", "ATTENTION: If a coyote came in contact with 
        a human or pet, the visitor should seek medical 
        attention from a doctor/veterinarian immediately!"), br(),
        dateInput("inc_date", "Incident date"),
        timeInput("inc_time", "Incident time", seconds = F),
        radioButtons("ampm", "AM or PM", choices = c("AM", "PM"),
                     inline = T,
                     selected = character(0)),
        selectInput("concern", labelMandatory("Why are you reporting a coyote?"),
                    choices = c("Choose one option" = "",
                                "I saw or heard a coyote (no concerns)" =
                                "no concern",
                                "I have a concern about a coyote (please check concerns below)" =
                                "concerned"),
                    selected = character(0)),
        selectInput("concern_type", 
                           "If you have concerns check as many as apply (skip this if you have no concerns):",
                           choices = c("Coyote made physical contact with human",
                                       "Coyote made physical contact with pet",
                                       "Coyote was following",
                                       "Coyote was barking",
                                       "Coyote was growling",
                                       "Coyote approached without fear",
                                       "Coyote stood its ground and would not move",
                                       "Coyote looked sick or injured",
                                       "Other (explain below)"),
                           multiple = T,
                           selected = character(0)),
        textAreaInput("concern_details", 
                      "Additional information about concern:",
                      placeholder = "Additional details about encounter"),

        h3("Location of encounter"),
        tags$em("Please use the online mapping function"),
        "and provide 
        any additional location information below.", br(),
        selectInput("in_metroparks",
                    "Was this coyote within, adjacent to, or outside of Cleveland Metroparks?",
                    choices = c("Choose one option" = "",
                                "Within" = "within",
                                "Adjacent to (next to Cleveland Metroparks)" = 
                                    "adjacent",
                                "Outside of Cleveland Metroparks" = "outside"),
                    selected = character(0)),
        numericInput("zip", "Sighting zip code (if known)",
                     value = NULL, min = 10000, max = 99999),
        
        "Instructions:  Either", br(),
        "  * click, drag, and zoom map to location", br(),
        "or", br(),
        "  * type address with city into search bar,", br(),
        "then click location on map to record coordinates of sighting.", br(),
        "Click layers button toggle between map and image backgrounds.", br(),
        "NOTE: coordinates are not recorded unless you click on the map.",
        br(),
        
        leafletOutput("map1", "50%", 500),

        verbatimTextOutput("Click_text"),
        textAreaInput("location_details", 
                      "Additional location information",
                      placeholder = "Possible additional information:  
                      address, reservation, trail segment or picnic 
                      area name, be as specific as possible, note 
                      any landmarks"),
        
        h3("Additional information"),
        selectInput("activity_during_sighting",
                    "Person reporting this coyote was: (check as many as apply)",
                    choices = c("Walking", "Running",
                     "In a building",
                     "In a vehicle",
                     "Bicycling",
                     "On horseback",
                     "Tending horses",
                     "Walking dog(s) on leash",
                     "Walking dog(s) off leash",
                     "Other (explain below)"),
                    multiple = T,
                    selected = character(0)),
        textAreaInput("activity_details",
                      "What were you doing at time of sighting?",
                      placeholder = "Please give details about your actions when sighting occurred."),
        selectInput("trail",
                    "If oudoors in Cleveland Metroparks were you",
                    choices = c("Choose one option" = "",
                                "On a trail or in parking lot" =
                                    "trail",
                                "Off of a trail" = "off trail",
                                "Other (explain below)" =
                                    "other"),
                    selected = character(0)),
        textAreaInput("activity_details",
                      "What were you doing at time of sighting?",
                      placeholder = "Please give details about your actions when sighting occurred."),
        selectInput("sighting_information", 
                       "Sighting information",
                       choices = c("Single coyote",
                                   "2 or more coyotes ",
                                   "Coyote came in contact with humans",
                                   "Coyote came in contact with humans",
                                   "Shy/Wary/Cautious",
                                   "Bold/Unafraid",
                                   "Other (explain below)"),
                       multiple = T,
                       selected = character(0)),
        textAreaInput("sighting_details",
                  "Explain choices above",
                  placeholder = "Please give details about any interaction"),
    
        
        "Optional:  Describe the coyote(s) including coat color, 
        coat condition, tail position. Feel free to add additional 
        details about the encounter here.", br(),
        textAreaInput("additional_details", 
                      "Any additional information",
                      placeholder = "Additional details"),
        
        "Mandatory information marked with red star",
        labelMandatory(" "),
        " must be entered before you can submit.", br(),
        
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

# testing to see if pool is working
    # output$tbl = renderTable({
    #     pool %>% 
    #         tbl(in_schema(Schema, "coyote_report_form_view")) %>% 
    #         head()
    # })
    
    output$map1 = renderLeaflet(map)
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

    fileName = reactive({sprintf("%s_%s_%s.csv", #"%s_%s_%s_%s.csv"
                        input$first_name,
                        input$last_name,
                        humanTime()#, 
# use line below if you worry about same username/same second 
#  collisions or want a nice unique key.
                        # digest::digest(data)
    )
})
    
    formData <- reactive({
        data <- sapply(fieldsAll, function(x) input[[x]])
        data <- c(filename_key = fileName(),
                  data, 
                  incident_date = as.character(input$inc_date),
                  incident_time = strftime(input$inc_time, "%R"),
                  Latitude = Latitude(),
                  Longitude = Longitude(),
                  timestamp = epochTime())
        data <- t(data)
        data
    })
    
    humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
    table_id = Id(schema = Schema, 
                  table = db_table_out)
    
    saveData <- function(data) {
        write.csv(x = data, file = file.path(responsesDir, fileName()),
                  row.names = FALSE, quote = TRUE)
        dbAppendTable(con, table_id, value = data.frame(data))
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
            shinyjs::show(id = "error", 
                          anim = TRUE, 
                          animType = "fade")
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
