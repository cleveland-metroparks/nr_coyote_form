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
library(DBI)

responsesDir <- file.path("responses")
db_table_out = "rshiny_test_form_2"
# formData reactive in server block must be changed whenever form is modified

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
fieldsMandatory = c("last_name", "concern", "in_metroparks")
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
        textInput("first_name", "First name"),
        textInput("last_name", labelMandatory("Last name")),
        textInput("phone", "Phone number"),
        textInput("email", "Email"),
        
        h3("Incident information"),
        tags$span(style="color:red", "ATTENTION: If a coyote came in contact with 
        a human or pet, that person should seek medical 
        attention from a doctor/veterinarian immediately!"), br(), br(),
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
        conditionalPanel(condition = "input.concern == 'concerned'",
            selectInput("concern_type", 
                    "What are your concerns?",
                           choices = c("Choose one or more options" = "",
                                       "Coyote made physical contact with human",
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
          conditionalPanel(condition = "input.concern_type.includes('Other (explain below)')",
            textAreaInput("concern_details", 
                      "Additional information about concern:",
                      placeholder = "Why did you select Other above?")
          )
        ),
        h3("Location of encounter"),
        tags$em("Please use the online mapping function"),
        "and provide any additional location information below.", br(), br(),
        selectInput("in_metroparks",
                    labelMandatory("Was this coyote within, adjacent to, 
                                   or outside of Cleveland Metroparks?"),
                    choices = c("Choose one option" = "",
                                "Within Cleveland Metroparks" = "within",
                                "Adjacent to or next to Cleveland Metroparks" = 
                                    "adjacent",
                                "Outside of Cleveland Metroparks" = "outside"),
                    selected = character(0)),
        numericInput("zip", "Sighting zip code (if known)",
                     value = NULL, min = 10000, max = 99999),
        hr(),
        h4("Map instructions:"),
        "Either", br(),
        "  1. click, drag, and zoom map to location", br(),
        "or", br(),
        "  2. type address with city into search bar,", br(),
        "then click location on map to record coordinates of sighting.", br(), br(),
        "To toggle between map and image backgrounds, click layers button.", br(), br(),
        strong("NOTE: coordinates are not recorded unless you click on the map."),
        br(), br(),
        leafletOutput("map1", "80%", 500),
        verbatimTextOutput("Click_text"),
        hr(),
        textAreaInput("location_details", 
                      "Additional location information (optional)",
                      placeholder = "Address, trail or picnic area name, be as specific as possible, note any landmarks"),
        
        h3("Additional information"),
        selectInput("outdoors", 
                    "Person reporting this coyote was:",
                    choices = c("Choose one option" = "",
                                "Outdoors",
                                "In a building",
                                "In a vehicle")),
        conditionalPanel(condition = 
                     "input.outdoors == 'Outdoors'",
            selectInput("activity_outdoors",
                "Activity while outdoors:",
                choices = c("Choose one or more options" = "",
                        "Walking",
                        "Walking dog(s) on leash",
                        "Walking dog(s) off leash",
                        "Running",
                        "Bicycling",
                        "On horseback",
                        "Tending horses",
                        "Other (explain below)"),
                multiple = T,
                selected = character(0)),
            conditionalPanel(condition = 
                             "input.activity_outdoors.includes('Other (explain below)')",
                textAreaInput("activity_details",
                      "What were you doing at time of sighting?",
                      placeholder = "Why did you select Other above?")
            ),
            selectInput("trail",
                    "If oudoors in Cleveland Metroparks were you",
                    choices = c("Choose one option" = "",
                                "On a trail" = "trail",
                                "Parking lot" = "parking lot",
                                "Off of a trail" = "off trail",
                                "Other (explain below)" =
                                    "other"),
                    selected = character(0))
        ),
        selectInput("sighting_information", 
                       "Sighting information",
                       choices = c("Choose one or more options" = "",
                                   "Single coyote",
                                   "2 or more coyotes ",
                                   "Shy/Wary/Cautious",
                                   "Bold/Unafraid",
                                   "Other (explain below)"),
                       multiple = T,
                       selected = character(0)),
        conditionalPanel(condition = 
                     "((input.trail == 'other') || (input.sighting_information.includes('Other (explain below)')))",
            textAreaInput("sighting_details",
                  "Explain choices above",
                  placeholder = "Why did you select Other in either of your last two above?")
        ),
        
        "Optional:  Describe the coyote(s) including coat color, 
        coat condition, tail position. Feel free to add additional 
        details about the encounter here.", br(), br(),
        textAreaInput("additional_details", 
                      "Any additional information",
                      placeholder = "Additional details"),
        
        "Mandatory information marked with red star",
        labelMandatory(" "),
        " must be entered before you can submit.", br(), br(),
        
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
    addProviderTiles(providers$Stamen.Terrain, group = "Stamen Terrain") %>%
    # addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI WorldImagery") %>%
    setView(lng = -81.65, lat = 41.38, zoom = 10) %>%
    addLayersControl(baseGroups = c("Mapbox",
                                    "Stamen.Terrain",
                                    # "Open Topo Map",
                                    "ESRI WorldImagery"),
                     position = "bottomright") %>%
    leaflet.extras::addSearchOSM(options = searchOptions(collapsed = FALSE))


# Define server logic required to draw a histogram
server = function(input, output) {

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

    formData = reactive({
        data = c(filename_key = fileName(),
                   first_name = input$first_name,
                   last_name = input$last_name,
                   phone = input$phone,
                   email = input$email,
                   concern = input$concern,
                   concern_type = paste(input$concern_type,
                                        collapse = "|"),
                   concern_details = input$concern_details,
                   in_metroparks = input$in_metroparks,
                   zip = input$zip,
                   location_details = input$location_details,
                   outdoors = input$outdoors,
                   activity_outdoors =
                       paste(input$activity_outdoors,
                             collapse = "|"),
                   activity_details = input$activity_details,
                   trail = input$trail,
                   sighting_information =
                       paste(input$sighting_information,
                             collapse = "|"),
                   sighting_details = input$sighting_details,
                   additional_details = input$additional_details,
                   incident_date = as.character(input$inc_date),
                   incident_time = strftime(input$inc_time, "%R"),
                   ampm = input$ampm,
                   Latitude = Latitude(),
                   Longitude = Longitude(),
                   timestamp = epochTime()
                   )
        data = t(data)
        data
    })

    # This approach would be nice, but fails for some reason related to data2
    # formData <- reactive({
    #     data1 <- sapply(fieldsSimple, function(x) input[[x]])
    #     data2 = sapply(fieldsCombine, function(x) paste(input[[x]],
    #                                                     collapse = "|"))
    #     data <- c(filename_key = fileName(),
    #               data1, data2,
    #               incident_date = as.character(input$inc_date),
    #               incident_time = strftime(input$inc_time, "%R"),
    #               Latitude = Latitude(),
    #               Longitude = Longitude(),
    #               timestamp = epochTime())
    #     data <- t(data)
    #     data
    # })
    
    humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
    table_id = Id(schema = Schema, 
                  table = db_table_out)
    
    saveData <- function(data) {
        write.csv(x = data, file = file.path(responsesDir, fileName()),
                  row.names = FALSE, quote = TRUE)
        dbAppendTable(con, table_id, value = data.frame(data))
        # dbAppendTable(con, table_id, value = data)
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
