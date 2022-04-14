#loading library
library(shiny)
library(tidyverse)
library(DT)
library(shinythemes)
library(leaflet)
# library(rgdal)
library(shinydashboard)

#loading data
data <- read.csv("lgNepal.csv")
#setting lat col as numeric
data$lat <- as.numeric(data$lat)
# shapes <- readOGR("local_level.shp",GDAL1_integer64_policy = TRUE)

#renaming data
data$name_of_province <- as.character(data$name_of_province)
class(data$name_of_province)
for(i in 1:7){
  names<- c("Pradesh 1","Madhesh Pradesh","Bagmati Pradesh","Gandaki Pradesh","Lumbini Pradesh","Karnali Pradesh","Sudurpashchim Pradesh")
  data$name_of_province[data$name_of_province ==i] <-names[i]
}

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Local Government"),
  dashboardSidebar(
    selectInput("province","Select Province",unique(data$name_of_province)),
    selectizeInput("district","Select District",choices = NULL),
    selectizeInput("level","Local Level Type",choices = NULL)
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML('
        .content {
          padding-left: 20px;
          height:100vh;
        }
        ')
      )
    ),
   fluidRow(

    #theme 
    # theme = shinytheme("simplex"),
    # Application title
    titlePanel("Local Levels of Nepal   नेपालको स्थानीय तह"),

    # Sidebar for input  filter
    # sidebarLayout(
    #     sidebarPanel(
    #         selectInput("province","Select Province",unique(data$name_of_province)),
    #         selectizeInput("district","Select District",choices = NULL),
    #         selectizeInput("level","Local Level Type",choices = NULL)
    #     ),

        # Show a table
        # mainPanel(
          tabsetPanel(
            tabPanel("Table",
              DT::DTOutput("maintable"),
              fluidRow(
                valueBoxOutput("total_pop"),
                valueBoxOutput("total_households"),
                valueBoxOutput("total_area")
              ),
            ),
            tabPanel("Map", leafletOutput("map")),
          )
        # )
    )
)
)
# Define server logic required
server <- function(input, output,session) {
  
  #----reactive calculations
  district_sel <- reactive({
      data %>% filter(name_of_province == input$province)
  })
  
  observeEvent(district_sel(),{
    updateSelectizeInput(session,"district", choices = district_sel()$district_name_english)
  
  level_sel <- reactive({
      data %>% filter(name_of_province == input$province,
                      district_name_english == input$district)
    
  })
  observeEvent(level_sel(),{
    updateSelectizeInput(session,"level",choices = level_sel()$palika_type)
  
  
  #logic for all selection
  # if(input$name_of_province != "All"){
  #   data <- subset(data, name_of_province ==input$name_of_province)
  # }
  
  filter_data <- reactive({
      data %>% filter(name_of_province == input$province,
                      district_name_english == input$district,
                      palika_type == input$level)
  })
  
  #-------------------------Table render--------
    output$maintable <- DT::renderDT({
      filter_data() %>% 
        select("Local Level Name" = palika_name_english, District = district_name_english, 
                        Type = palika_type, Province = name_of_province, Website = website)
    })
    
  
  #--------------------------Map Render--------------
    output$map <- renderLeaflet({
      map_data <- filter_data() %>% 
        mutate(pop_info = paste("<h4><b>",palika_name_english,"</b></h4>",
                                "<br/>","<b>Area (in sq.km):</b>",area_sq_km,
                                "<br/>","<b>Total Population:</b>",total_population,
                                "<br/>","<b>Total Households:</b>",total_households,
                                "<br/>","<b>Municapal Center:</b>",municipal_center_english,
                                "<br/>","<b>Total Wards:</b>",total_wards
                                ))
      
      colors <- c("red","blue")
      pal <- colorFactor(colors, map_data$total_population)
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        setView(map_data$long[1], map_data$lat[1], zoom = 9) %>% 
        addCircleMarkers(data = map_data, lng= ~long, lat= ~lat, radius = ~sqrt(map_data$area_sq_km), 
                         popup = ~pop_info, color = ~pal(total_population))
      
    })
  
  
  #------total pop
  
  
  #---------------------- Value box Extra Render---------------
  output$total_pop <- renderValueBox({
    valueBox(
      paste0(sum(filter_data()$total_population)), "Total Population", icon = icon("users"),
      color = "purple"
    )
  })
  
  output$total_households <- renderValueBox({
    valueBox(
      paste0(sum(filter_data()$total_households)), "Total Households", icon = icon("home"),
      color = "blue"
    )
  })
  
  output$total_area <- renderValueBox({
    valueBox(
      paste(sum(filter_data()$area_sq_km)), "Total Area (in sq.km)", icon = icon("chart-area"),
      color = "orange"
    )
  })
  
  
  })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
