library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(DT)
library(ggplot2)
library(here)
library(leaflet)
library(lubridate)
library(plotly)
library(readr)
library(reactlog)
library(sf)
library(stringr)
library(writexl)

options(scipen = 999999)

# reactlog::reactlog_enable()

# data ----
most_recent_data_update_dt <- "2021-12-05"

lookup <- read_csv(here("Data", "lookup.csv"))
records <- read_csv(here("Data", "records.csv"))

records_min_dt <- min(
  c(min(records$complaint_date, na.rm = T),
    min(records$inspection_date, na.rm = T))
)

records_max_dt <- max(
  c(max(records$complaint_date, na.rm = T),
    max(records$inspection_date, na.rm = T))
)


# ui elements ----
# TODO: Move to ui.R and source when finished
explore_sidebar <- function() {
  conditionalPanel(
    "input.sidebarid == 'explore'",
    pickerInput(inputId = "cca_filter_explore",
                label = "Use this dropdown select a community area: ",
                choices = records %>% distinct(community) %>% arrange(community),
                selected = records$community,
                options = pickerOptions(
                  actionsBox = T
                ),
                multiple = T),
    dateRangeInput("date_filter",
                   label = "Select the date range for the data: ",
                   start = records_max_dt %m-% years(5),
                   end = records_max_dt,
                   min = records_min_dt,
                   max = records_max_dt,
                   format = "yyyy-mm",
                   startview = "year")
  )
}

explore_body <- function (){
  tabItem(tabName = "explore",
          
          fluidRow(
            uiOutput("ibox1"),
            uiOutput("ibox2"),
            uiOutput("ibox3")
          ),
          fluidRow(
            withSpinner(plotlyOutput("explore_plot"))
          ),
          fluidRow(
            withSpinner(dataTableOutput("explore_table"))
          )
  )
}

map_sidebar <- function() {
  conditionalPanel(
    "input.sidebarid == 'map'",
    HTML("Fill out the filters as desired and press <br> 'Generate Map' to view the results.<br>"),
    textInput(inputId = "address_filter",
              label = "Use this to search by address: ", 
              value = "",
              placeholder = "e.g. Halsted or 914 Halsted St"),
    
    pickerInput(inputId = "cca_filter_map",
                label = "Use this dropdown select a community area: ",
                choices = lookup %>% distinct(community) %>% arrange(community),
                selected = lookup$community,
                options = pickerOptions(
                  actionsBox = T
                ),
                multiple = T),
    
    actionButton(inputId = "do",
                 label = "Generate Map")
  )
}

map_body <- function() {
  tabItem(tabName = "map",
          # Ensures the map takes up the whole page
          tags$style(type = "text/css", "#lookup_map {height: calc(100vh - 80px) !important;}"),
          
          fluidRow(
            withSpinner(leafletOutput("lookup_map"))
          ),
  )
}

about_sidebar <- function () {
  conditionalPanel(
    "input.sidebarid == 'about'",
    br(),
    downloadButton(outputId = "download_data",
                   label = "Download dashboard data")
  )
}

about_body <- function() {
  tabItem(tabName = "about",
          # in-line CSS to format the text
          style = "
          white-space: normal;
          margin-left: 10px;
          margin-right: 10px;
          ",
          
          fluidRow(
        HTML(paste0("<br>This shiny app was built and deployed by <a href='github.com/ethanjantz'>Ethan Jantz</a> to explore how the Chicago Department of Public Health is handling environmental complaints.<br>
                
                <br>In the spirit of participatory government, this dashboard provides general overviews for communities and organizers to understand how their neighborhoods are faring in terms of environmental issues and compare them to other neighborhoods in the city.<p>

Data is pulled from the <a href='https://data.cityofchicago.org/Environment-Sustainable-Development/CDPH-Environmental-Records-Lookup-Table/a9u4-3dwb'>Chicago Open Data Portal</a>. This project was developed using R and Shiny as the final course project for Coding for Civic Applications.<p>

Data was last updated on ", most_recent_data_update_dt,".")
        )
      )
  )
}

# ui ----
ui <- dashboardPage(
  
  dashboardHeader(title = HTML("<h4>CDPH Environmental Response<br /> Dashboard</h4>"),
                  titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebarid",
      # in-line CSS to format the text
      style = "white-space: normal;
      margin-top: 10px;
      margin-left: 10px;
      margin-right: 10px;
      margin-bottom: 10px;",
      
      menuItem("Explore",
               tabName = "explore",
               icon = icon("chart-line")),
      explore_sidebar(),
      
      menuItem("Map",
               tabName = "map",
               icon = icon("map")),
      map_sidebar(),
      
      menuItem("About",
               tabName = "about",
               icon = icon("question")),
      about_sidebar()
    )
  ),
  dashboardBody(
    tabItems(
      explore_body(),
      map_body(),
      about_body()
    )
  )
)

# server ----
server <- function(input, output) {
  
  # explore ----
  ### reactive 
  filtered_records <- reactive({
    records %>%
      filter(
        community %in% input$cca_filter_explore,
        complaint_date >= input$date_filter[1] | inspection_date >= input$date_filter[1],
        complaint_date <= input$date_filter[2] | inspection_date <= input$date_filter[2]
      ) %>%
      mutate(address = coalesce(address, map_address)) %>%
      select(type, everything(), -c(latitude, longitude, location, map_address, street_number:street_type))
  })
  
  summarized_records <- reactive({
    filtered_records() %>%
      mutate(
        across(c(complaint_date, inspection_date),
               ~lubridate::floor_date(.x, unit = "quarters")),
        type = ifelse(is.na(inspection_date), "complaint", "inspection"),
        date = coalesce(complaint_date, inspection_date)
      ) %>%
      count(type, date)
    
  })
  
  community_with_most_complaints <- reactive({
    records %>%
      filter(
        community %in% input$cca_filter_explore,
        complaint_date >= input$date_filter[1] | inspection_date >= input$date_filter[1],
        complaint_date <= input$date_filter[2] | inspection_date <= input$date_filter[2],
        type == "complaint"
      ) %>%
      count(community) %>%
      mutate(prop = (n / sum(n)) * 100)  %>%
      arrange(desc(n))
  })
  
  most_complaint_types <- reactive({
    records %>%
      filter(
        community %in% input$cca_filter_explore,
        complaint_date >= input$date_filter[1] | inspection_date >= input$date_filter[1],
        complaint_date <= input$date_filter[2] | inspection_date <= input$date_filter[2],
        type == "complaint"
      ) %>%
      count(complaint_type) %>%
      mutate(prop = (n / sum(n)) * 100)  %>%
      arrange(desc(n))
  })
  
  inspect_complaint_ratio <- reactive({
    df <- records %>%
      filter(
        community %in% input$cca_filter_explore,
        complaint_date >= input$date_filter[1] | inspection_date >= input$date_filter[1],
        complaint_date <= input$date_filter[2] | inspection_date <= input$date_filter[2],
      ) %>%
      count(type)
    
    ratio <- (df %>% filter(type == "inspection") %>% pull(n)) / 
      (df %>% filter(type == "complaint") %>% pull(n))
    
    ratio
  })
  
  ### render
  
  output$ibox1 <- renderUI({
    infoBox("Neighborhood with most complaints based on filtered data", 
            value = community_with_most_complaints()[1,1],
            subtitle = paste0(round(community_with_most_complaints()[1,3], 1), "%"),
            icon = icon("map-pin"),
            color = "purple")
  })
  
  output$ibox2 <- renderUI({
    
    infoBox("Most common type of complaint based on filtered data",
            value = most_complaint_types()[1,1],
            subtitle = paste0(round(most_complaint_types()[1,3], 1), "%"),
            icon = icon("exclamation-triangle"),
            color = "yellow")
  })
  
  output$ibox3 <- renderUI({
    
    infoBox("Inspection to complaint ratio based on filtered data",
            value = round(inspect_complaint_ratio(), 2),
            icon = icon("percent"))
  })
  
  output$explore_plot <- renderPlotly({
    # req(input$cca_filter_explore)
    plotly::ggplotly(
      ggplot(summarized_records(), aes(x = date, y = n, color = type)) +
        geom_line() +
        geom_point() +
        labs(title = "Complaints and Inspections by Quarter",
             x = "Date",
             y = "Complaints/Inspections Received") +
        ylim(0, NA) +
        scale_x_date(date_breaks = "1 year", 
                     date_labels = "%Y",
                     date_minor_breaks = "3 months")
    )
    
  })
  
  output$explore_table <- DT::renderDataTable(
    filtered_records(),
    rownames = F,
    options = list(
      pageLength = 5,
      scrollX = T,
      scrollY = T,
      search.return = T,
      # This collapses the `target` column values so row heights are shorter
      # but values remain
      # values refer to the complaint_detail and inspection_log columns
      columnDefs = list(list(
        targets = c(6, 7),
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data.length > 6 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
          "}")
      )
      )
    )
  )
  
  # map ----
  ### reactive
  filtered_lookup <- eventReactive(input$do, {
    df <- lookup %>%
      filter(
        stringr::str_detect(address, 
                            str_to_upper(input$address_filter))
      )
    df
    
  })
  
  ### render
  output$lookup_map <-  renderLeaflet({
    
    filtered_lookup() %>%
      # Having the community filter in here works more simply
      # than having it in the eventReactive above
      filter(community %in% input$cca_filter_map) %>%
      leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        clusterOptions = markerClusterOptions(),
        popup = ~ paste0("<b>", str_to_title(address), "</b><br>",
                         "Complaints: ", complaint_url, 
                         "<br> Inspections: ", inspection_url,
                         "<br> Permits: ", permit_url)
      )
    
  })
  
  # Opting out of this one, since if the filter isn't set it tries to load
  # too many rows at once and crashes the DT
  # output$lookup_table <- DT::renderDataTable(
  #   filtered_lookup(),
  #   rownames = F,
  #   options = list(
  #     pageLength = 10,
  #     scrollX = T,
  #     scrollY = T,
  #     search.return = T
  #   )
  # )
  
  # about ----
  output$download_data <- downloadHandler(
    filename = function() {
      paste("environmental_dashboard_data.xlsx")
    },
    content = function(file) {
      write_xlsx(
        list("Lookup" = lookup, "Records" = records),
        path = file
      )
    }    
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
