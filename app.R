library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(RColorBrewer)

covid <- read.csv("data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("COVID-19 DI JAWA BARAT"),
    
    sidebarLayout(
        sidebarPanel(
            h5("Sebaran kasus terkonfirmasi positif kumulatif dihitung sejak 1 Agustus 2020"),
            tags$a(h6(href="https://pikobar.jabarprov.go.id/table-case", "Sumber Data", target="_blank")),
            selectInput("date",
                        "Pilih tanggal:",
                        choices = unique(covid$tanggal))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Peta Sebaran", leafletOutput("kasus")),
                tabPanel("Grafik")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    jabar_map <- reactive({
        map <- st_read("petajabar27/Peta Jabar 27.shp")
        newcases <- covid %>% 
            filter(tanggal == input$date) %>%
            select(IDKAB2, confirmation)
        newcases$IDKAB2 <- as.character(newcases$IDKAB2)
        value <- left_join(map, newcases, by = "IDKAB2")
        return(value)
    })
    
    output$kasus <- renderLeaflet({
        
        pal <- colorBin(
            palette = "OrRd", 5,
            domain = jabar_map()$confirmation)
        
        jabar_map() %>%
            leaflet() %>%
            addProviderTiles(provider = "CartoDB.Positron") %>%
            addPolygons(
                color = ~pal(jabar_map()$confirmation),
                weight = 1,
                opacity = 1,
                dashArray = "2",
                fillOpacity = 0.7,
                
                highlight = highlightOptions(
                    weight = 1,
                    color = "#000000",
                    dashArray = "1",
                    fillOpacity = 1,
                    bringToFront = TRUE),
                
                label = paste0(jabar_map()$KABKOT, " ", jabar_map()$confirmation),
                
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
            
            addLegend(
                position = "bottomright",
                pal = pal,
                values = ~jabar_map()$confirmation,
                title = "Kasus Terkonfirmasi Positif Kumulatif")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
