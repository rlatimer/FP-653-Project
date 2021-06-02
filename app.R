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

needs(tidyverse, sf, tmaptools, tmap)
library(shiny)
library(plotly)
library(scales)
library(tidyverse)
library(shinythemes)
library(usmap)
library(urbnmapr)
library(viridis)
library(rjson)
library(stringr)
library(glue)
library(RSocrata)

tmap_mode("view")
g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
)

geo_data <- read_sf("us_county_geom.shp")

covid_cases <- read_csv("county_case_counts.csv")

county_data <- read_csv("app_attendance_data.csv")

county_data <- left_join(geo_data, county_data)

tm_shape(test) +
    tm_polygons("number_schools")

ggplot(test) +
    geom_sf(aes(fill = number_schools, color = number_schools))

centroids <- st_centroid(test)

centroids <- centroids %>% 
    mutate(county = str_replace_all(county_name, " County", ""))

tm_shape(test) +
    tm_polygons("number_schools",
                style = "cont") +
    tm_shape(centroids) +
    tm_text("county", size = 0.5) +
    tm_layout(legend.outside = TRUE)
# 
# county_map <- counties %>% 
#     mutate(county_fips = as.character(county_fips))

# 
# test %>%
#     ggplot(aes(long, lat, group = group, fill = mean_all_change)) +
#     geom_polygon(color = NA) +
#     coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
#     labs(fill = "Median Household Income")

# county_sum <- county_data %>% 
#     group_by(county_fips, month, county_name) %>% 
#     summarise(mean_all = mean(mean_all_change),
#               mean_elem = mean(mean_elem_change),
#               mean_high = mean(mean_middlehigh_change)) %>%
#     rename(fips = county_fips)

# test %>%
#     ggplot(aes(long, lat, group = group, fill = mean_all)) +
#     geom_polygon(color = NA) +
#     coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
#     labs(fill = "Mean All Change")


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
                        textInput("state", label = "State"),
                        width = 3
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        tabsetPanel(type = "tabs",
                        tabPanel("GGPlots", plotOutput("map"),
                                 tmapOutput("tmap")),
                        tabPanel("Plotly", plotlyOutput("plotly"), h5("Rendering takes some time."))
                    )
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
    
data <- reactive({
    county_data %>% 
        filter(month == month())
})

state_data <- reactive({
    county_data %>% 
        filter(month == month(),
               state_abb == state())
})

    # output$map <- renderPlot({
    #     data() %>%
    #         ggplot(aes(long, lat, group = group, fill = mean_all)) +
    #         geom_polygon(color = NA) +
    #         coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    #         scale_fill_viridis(limits = c(-1, 1)) +
    #         labs(fill = "Mean All Change")
    # })
    #     

# output$map <- renderPlot({
#     plot_usmap(regions = "counties", data = data(), values = variable()) + 
#     labs(title = "Schools experiencing a year-over-year decline of at least 50 percent for month:",
#          subtitle = "subtitle here") + 
#     theme(panel.background = element_rect(color = "black", fill = "lightblue"))
# })

output$map <- renderPlot({
    plot_usmap(regions = "counties",
               data = data(),
               values = variable()) +
        labs(title = ifelse(
            input$share_closed == "mean_change",
            glue("Average Year-Over-Year % Change in School Visitors"),
            glue(
                "Percent of Schools Experiencing A Year-Over-Year Decline of at least {pct()} percent for month"
            )
        )) +
        scale_fill_continuous(type = "viridis", limits = c(-100, 100))
}) %>%
    bindCache(input$month, input$share_closed, input$grade)

output$tmap <- renderTmap({
    tm_shape(state_data()) +
    tm_polygons(variable()) +
    tm_shape(centroids) +
    tm_text("county", size = 0.5)
})


#adding state boundaries
states <- plot_usmap(
    "states", 
    color = "black",
    fill = alpha(0.01)
) 
    output$plotly <- renderPlotly({
        p <- plotly::plot_ly() %>%
            add_trace(
                type = "choropleth",
                geojson = counties_json,
                locations = data()$fips,
                z = data()$mean_all_change,
                text = data()$county_name,
                colorscale = "Viridis",
                zmin = -100,
                zmax = 100,
                marker = list(line = list(width = 0))
            ) %>% colorbar(title = "Mean Attendance Change (%)") %>%
            layout(title = "2020 US School Attendance Change by County") %>% layout(geo = g)
    }) %>% 
        shiny::bindCache(input$month)
}
# Run the application 
shinyApp(ui = ui, server = server)
