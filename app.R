library(shiny)
library(leaflet)
library(sf)
library(bslib)
library(rnaturalearth)
library(shinyWidgets)
library(shinydashboard)

kenya_shapefile <- st_read("C:/Users/anitt/Downloads/ken_adm_iebc_20191031_shp (1)/ken_admbnda_adm1_iebc_20191031.shp")

ui <- page_navbar(
  id = "navbar",
  title = "Screening rates for Cervical and Breast Cancers in Kenya",
  nav_spacer(),
  nav_panel("Landing Page", value = "landing_page",
            div(
              style = "position: relative; height: 100%; overflow: hidden;",
              leafletOutput("kenya_map", height = "100%"),
              fluidRow(
                column(12,
                       absolutePanel(id = "controls", class = "panel panel-default",
                                     bottom = 20, right = 20, width = 300, fixed = TRUE,
                                     draggable = TRUE, height = "auto",
                                     tags$div(
                                       style = "background-image: url('https://www.example.com/user_image.jpg'); 
                                                background-size: cover; 
                                                background-position: center; 
                                                color: white; 
                                                border: 2px solid grey; 
                                                padding: 10px; 
                                                border-radius: 5px; height: 120px; margin-bottom: 10px;",
                                       valueBox(
                                         value = "44,726", 
                                         subtitle = "Total Cancer Cases", 
                                         icon = icon("cancer", lib = "font-awesome"),
                                         width = 12
                                       )
                                     ),
                                     tags$div(
                                       style = "background-image: url('https://www.example.com/user_image.jpg'); 
                                                background-size: cover; 
                                                background-position: center; 
                                                color: white; 
                                                border: 2px solid grey; 
                                                padding: 10px; 
                                                border-radius: 5px; height: 120px; margin-bottom: 10px;",
                                       valueBox(
                                         value = "7,243", 
                                         subtitle = "Breast Cancer Cases", 
                                         icon = icon("female", lib = "font-awesome"),
                                         width = 12
                                       )
                                     ),
                                     tags$div(
                                       style = "background-image: url('https://www.example.com/user_image.jpg'); 
                                                background-size: cover; 
                                                background-position: center; 
                                                color: white; 
                                                border: 2px solid grey; 
                                                padding: 10px; 
                                                border-radius: 5px; height: 120px;",
                                       valueBox(
                                         value = "5,845", 
                                         subtitle = "Cervical Cancer Cases", 
                                         icon = icon("female", lib = "font-awesome"),
                                         width = 12
                                       )
                                     )
                       )
                )
              )
            )
  ),
  nav_panel("Screening Rates", value = "screening_page",
            fluidRow(
              column(3, 
                     box(title = "Select Years", status = "primary", width = 12, solidHeader = TRUE,
                         checkboxGroupInput("year_select", label = NULL, 
                                            choices = list("2014" = 2014, "2022" = 2022), 
                                            selected = c(2014, 2022))
                     ),
                     box(title = "Select Cancer Type", status = "primary", width = 12, solidHeader = TRUE,
                         checkboxGroupInput("cancer_type", label = NULL, 
                                            choices = list("Cervical Cancer" = "cervical", "Breast Cancer" = "breast"), 
                                            selected = c("cervical", "breast"))
                     ),
                     box(title = "Select Age Range", status = "primary", width = 12, solidHeader = TRUE,
                         sliderInput("age_range", label = NULL, min = 30, max = 49, value = c(30, 49))
                     )
              ),
              column(6, 
                     h3("Kenya Counties Map"),
                     leafletOutput("shapefile_map", height = "600px")  
              ),
              column(3,
                     box(title = "County Details", status = "info", width = 12, solidHeader = TRUE,
                         uiOutput("county_info"),
                         actionButton("go_to_dashboard", "View Full Report", class = "btn-primary")
                     )
              )
            )
  ),
  nav_panel("County Dashboard", value = "county_dashboard",
            fluidRow(
              column(12,
                     h3(textOutput("county_dashboard_title")),
                     uiOutput("county_dashboard_content"),
                     actionButton("back_to_screening_rates", "Back to Screening Rates", class = "btn-secondary")
              )
            )
  ),
  nav_panel("About This Site", value = "about_page"),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light"))
)

server <- function(input, output, session) {
  
  output$kenya_map <- renderLeaflet({
    map_provider <- if (input$dark_mode == "dark") {
      providers$CartoDB.DarkMatter
    } else {
      providers$CartoDB.Positron
    }
    
    kenya_data <- ne_countries(scale = "medium", returnclass = "sf") %>%
      subset(name == "Kenya")
    
    leaflet(kenya_data) %>%
      addProviderTiles(map_provider) %>%
      addPolygons(
        weight = 1,
        color = "black", 
        fillColor = "#2a7f62",  
        fillOpacity = 0.4
      ) %>%
      setView(lng = 37.90, lat = -0.0236, zoom = 6)
  })
  
  observeEvent(input$dark_mode, {
    if (input$dark_mode == "dark") {
      showNotification("Welcome to the dark side!")
    }
  })
  
  selected_county <- reactiveValues(name = NULL, data = NULL)
  
  screening_rates <- data.frame(
    county = rep(kenya_shapefile$ADM1_EN, each = 20),
    age = rep(30:49, times = nrow(kenya_shapefile)),
    cervical_2014 = sample(10:60, nrow(kenya_shapefile)*20, replace = TRUE),
    cervical_2022 = sample(20:70, nrow(kenya_shapefile)*20, replace = TRUE),
    breast_2014 = sample(5:50, nrow(kenya_shapefile)*20, replace = TRUE),
    breast_2022 = sample(15:65, nrow(kenya_shapefile)*20, replace = TRUE)
  )
  
  output$shapefile_map <- renderLeaflet({
    leaflet(kenya_shapefile) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(
        fillOpacity = 0.5,  
        color = "black",    
        weight = 1,
        layerId = ~ADM1_EN,  
        popup = ~paste0("<b>County:</b> ", ADM1_EN)  
      ) %>% 
      setView(lng = 37.90, lat = -0.0236, zoom = 6)  
  })
  
  observeEvent(input$shapefile_map_shape_click, {
    selected_county$name <- input$shapefile_map_shape_click$id
    
    county_data <- screening_rates[screening_rates$county == selected_county$name, ]
    
    county_data_filtered <- county_data %>%
      filter(age >= input$age_range[1] & age <= input$age_range[2])
    
    selected_county$data <- county_data_filtered
  })
  
  output$county_info <- renderUI({
    if (is.null(selected_county$name)) {
      return(h4("Click on a county to see details."))
    }
    
    county_data <- selected_county$data
    if (nrow(county_data) == 0) {
      return(h4("No data available for this county."))
    }
    
    selected_cancer_types <- input$cancer_type
    cancer_info <- list()  
    
    if ("cervical" %in% selected_cancer_types) {
      cancer_info <- append(cancer_info, 
                            list(
                              tags$li(strong("Cervical Cancer (2022): "), county_data$cervical_2022, "%")
                            ))
    }
    
    if ("breast" %in% selected_cancer_types) {
      cancer_info <- append(cancer_info, 
                            list(
                              tags$li(strong("Breast Cancer (2022): "), county_data$breast_2022, "%")
                            ))
    }
    
    if (length(cancer_info) > 0) {
      return(tagList(
        h4(strong(selected_county$name)),
        p("Cancer Screening Rates:"),
        tags$ul(cancer_info)
      ))
    } else {
      return(h4("No data available for the selected cancer types."))
    }
  })
  
  observeEvent(input$go_to_dashboard, {
    updateNavbarPage(session, inputId = "navbar", selected = "county_dashboard")
  })
  
  output$county_dashboard_title <- renderText({
    if (is.null(selected_county$name)) {
      return("Detailed Cancer Screening Data")
    } else {
      return(paste("Detailed Cancer Screening Data of", selected_county$name))
    }
  })
  
  output$county_dashboard_content <- renderUI({
    if (is.null(selected_county$name)) {
      return(h4("No county selected. Click on a county from the Screening Rates panel."))
    }
    
    county_data <- selected_county$data
    if (nrow(county_data) == 0) {
      return(h4("No data available for this county."))
    }
    
    tagList(
      h2(strong(selected_county$name), " - Full Screening Report"),
      h4("Cancer Screening Rates"),
      tableOutput("county_table")
    )
  })
  
  output$county_table <- renderTable({
    if (is.null(selected_county$data)) return(NULL)
    selected_county$data
  })
  
  observeEvent(input$back_to_screening_rates, {
    updateNavbarPage(session, inputId = "navbar", selected = "screening_page")
  })
}

shinyApp(ui, server)
