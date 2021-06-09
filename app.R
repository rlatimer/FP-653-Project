#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# This app relies on 'urbnmapr'. Users may need to install this first by running the following line:
# devtools::install_github("UrbanInstitute/urbnmapr")

library(shiny)
library(plotly)
library(scales)
library(tidyverse)
library(shinythemes)
library(viridis)
library(stringr)
library(glue)
library(reactable)
library(tmap)
library(tmaptools)
library(sf)

tmap_mode("view")
options(scipen = 999)
county_data <- read_csv("app_data.csv")
geo_data <- read_sf("wst_cst.shp")

county_data <- geo_data %>%
    left_join(county_data, by = "fips") %>%
    mutate(county_name = str_to_title(county_name))


# 
# -# tm_shape(test) +
# #     tm_polygons("number_schools")
# # 
# # ggplot(test) +
# #     geom_sf(aes(fill = number_schools, color = number_schools))
# # 
# 
# tm_shape(test) +
#     tm_polygons("number_schools")
# 
# ggplot(test) +
#     geom_sf(aes(fill = number_schools, color = number_schools))
# 
# centroids <- st_centroid(test)
# 
# centroids <- centroids %>% 
#     mutate(county = str_replace_all(county_name, " County", ""))
# 
# tm_shape(test) +
#     tm_polygons("number_schools",
#                 style = "cont") +
#     tm_shape(centroids) +
#     tm_text("county", size = 0.5) +
#     tm_layout(legend.outside = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("Attendance Change App"),
               h5("Compares the year-over-year change in school attendance during 2020"),
                h6("Chris Ives, Tess Sameshima, Rachael Latimer"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                    sidebarPanel(
                        sliderInput("month", label = "Month", min = 1, max = 12, value = 1, ticks = F, animate = T),
                        radioButtons("grade", label = "School Level", choices = c("All" = "all", "Elementary" = "elem", "Middle-High" = "middlehigh")),
                        selectInput("share_closed", "Variable of Interest", c("% of Schools With >25% decline in visitors" = "closed_25",
                                                                              "% of Schools With >50% decline in visitors" = "closed_50",
                                                                              "% of Schools With >75% decline in visitors" = "closed_75",
                                                                              "Mean % Change in School Visitors" = "mean_change")),
                        radioButtons("state", label = "State", choices = c("California" = "CA", "Oregon" = "OR", "Washington" = "WA")),
                        width = 3
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        tabsetPanel(type = "tabs",
                        tabPanel("TMaps",
                                 tmapOutput("tmap2", width = "100%", height = 700),
                                 reactableOutput("table"))                    )
                ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    month <- reactive({input$month})
    grade <- reactive({input$grade})
    share_closed <- reactive({input$share_closed})
    state <- reactive({input$state})
    
    pct <- reactive({
        str_sub(share_closed(), -2, -1)
    })

    variable <- reactive({
        if (input$share_closed == "mean_change") {
            glue('mean_{input$grade}_change')
        }
        else {
            glue('share_{input$grade}_{input$share_closed}')
        }
    })
    
    
    # labrel <- observe({
    #     if (input$condition==1){
    #         isolate({
    #             writingMarks(input)
    #         })
    #     }
    #     return()
    # })

data <- reactive({
    county_data %>% 
        filter(month == month())
})

state_all <- reactive({
    county_data %>% 
        filter(state_abb == state())
})

state_data <- reactive({
    county_data %>% 
        filter(month == month(),
               state_abb == state())
})

output$tmap2 <- renderTmap({
    tm_shape(state_data()) +
        if (input$share_closed == "mean_change") {
            tm_polygons(variable(), 
                        popup.vars = c(
                            "County Name" = "county_name",
                            "Covid Cases per 100,000" = "capita_covid",
                            "Share of Population" = "share_pop"),
                        palette = "viridis",
                        breaks = c(seq(-100, 100, 20)),
                        midpoint = 0) +
                tm_text("county_name", size = 0.7) +
                tm_bubbles(size = "capita_covid", col = "red", alpha = 0.5)
        }
    else {
        tm_polygons(variable(), 
                    popup.vars = c(
                        "County Name" = "county_name",
                        "Covid Cases per 100,000" = "capita_covid",
                        "Share of Population" = "share_pop"),
                    palette = "viridis",
                    breaks = c(seq(0, 100, 10)),
                    midpoint = 50) +
            tm_text("county_name", size = 0.7) +
            tm_bubbles(size = "capita_covid", col = "red", alpha = 0.5)
    }
})

output$table <- renderReactable({
    as.data.frame(state_all()) %>% 
        select(county_name, month, variable(), -geometry) %>% 
        arrange(county_name) %>% 
        reactable(filterable = TRUE,
              defaultPageSize = 25, groupBy = "county_name"
    )
    
})
}

# Run the application 
shinyApp(ui = ui, server = server)

