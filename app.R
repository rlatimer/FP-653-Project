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
library(usmap)
library(urbnmapr)
library(viridis)
library(rjson)
library(stringr)
library(glue)
library(RSocrata)
library(reactable)
library(sf)
library(tmap)
library(tmaptools)


tmap_mode("view")
options(scipen = 999)

geo_data <- read_sf("wst_cst.shp")
county_data <- read_csv("app_data.csv")

county_data <- geo_data %>% 
    left_join(county_data, by = "fips") %>% 
    mutate(county_name = str_to_title(county_name))


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("Attendance Change App"),
                h5("Compares the year-over-year change in school attendance during 2020"),
                h6("Chris Ives, Tess Sameshima, Rachael Latimer"),
                
                # Sidebar with a slider input for number of bins
                sidebarLayout(
                    sidebarPanel(
                        sliderInput(
                            "month",
                            label = "Month",
                            min = 1,
                            max = 12,
                            value = 1,
                            ticks = F,
                            animate = T
                        ),
                        radioButtons(
                            "grade",
                            label = "School Level",
                            choices = c(
                                "All" = "all",
                                "Elementary" = "elem",
                                "Middle-High" = "middlehigh"
                            )
                        ),
                        selectInput(
                            "share_closed",
                            "Variable of Interest",
                            c(
                                "Mean % Change in School Visitors" = "mean_change",
                                "% of Schools With >25% decline in visitors" = "closed_25",
                                "% of Schools With >50% decline in visitors" = "closed_50",
                                "% of Schools With >75% decline in visitors" = "closed_75"
                            )
                        ),
                        radioButtons(
                            "state",
                            label = "State",
                            choices = c(
                                "California" = "CA",
                                "Oregon" = "OR",
                                "Washington" = "WA"
                            )
                        ),
                        width = 3
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(tabsetPanel(
                        type = "tabs",
                        tabPanel(
                            "TMaps",
                            tmapOutput("tmap2", width = "100%", height = 700),
                            textOutput("narr"),
                            reactableOutput("test"),
                            reactableOutput("table")
                        )
                    ))
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    month <- reactive({
        input$month
    })
    grade <- reactive({
        input$grade
    })
    share_closed <- reactive({
        input$share_closed
    })
    state <- reactive({
        input$state
    })
    
    pct <- reactive({
        str_sub(share_closed(),-2,-1)
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

state_all <- reactive({
    county_data %>% 
        filter(state_abb == state())
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


output$tmap2 <- renderTmap({
    tm_shape(state_data()) +
        tm_polygons(variable(), 
                    popup.vars = c(
            "County Name" = "county_name",
            "Covid Cases per 100,000" = "capita_covid",
            "Share of Population" = "share_pop"), palette = "-inferno") +
        tm_bubbles(size = "capita_covid", col = "red", style = "cont",
                   palette = "red", alpha = 0.5,
                   size.max = max(state_all()$capita_covid)) +
        tm_text("county_name", size = 0.4)
        if (input$share_closed == "mean_change") {
            tm_polygons(
                variable(),
                popup.vars = c(
                    "County Name" = "county_name",
                    "Covid Cases per 100,000" = "capita_covid",
                    "Share of Population" = "share_pop"
                ),
                palette = "viridis",
                breaks = c(seq(-100, 100, 20)),
                midpoint = 0
            ) +
                tm_text("county_name", size = 0.7) +
                tm_bubbles(
                    size = "capita_covid",
                    col = "red",
                    alpha = 0.5,
                    popup.vars = c(
                        "County Name" = "county_name",
                        "Covid Cases per 100,000" = "capita_covid",
                        "Share of Population" = "share_pop"
                    )
                )
        }
    else {
        tm_polygons(
            variable(),
            popup.vars = c(
                "County Name" = "county_name",
                "Covid Cases per 100,000" = "capita_covid",
                "Share of Population" = "share_pop"
            ),
            palette = "viridis",
            breaks = c(seq(0, 100, 10)),
            midpoint = 50
        ) +
            tm_text("county_name", size = 0.7) +
            tm_bubbles(
                size = "capita_covid",
                col = "red",
                alpha = 0.5,
                popup.vars =
                    c(
                        "County Name" = "county_name",
                        "Covid Cases per 100,000" = "capita_covid",
                        "Share of Population" = "share_pop"
                    )
            )
    }
})

rv_map <-reactiveValues()

observeEvent(input$tmap2_shape_click, {
    click <- input$tmap2_shape_click
    print(input$tmap2_shape_click$id)
    print(substr(click$id, 2, 6))
    rv_map$county_click <- (substr(click$id, 2, 6))
    print(rv_map$county)
})

narr_data <- reactive({
    county_data %>%
        filter(month == input$month,
               state_abb == input$state,
               fips == rv_map$county)
})



# output$narr <- renderText({
#     glue("In {isolate(narr_data()$county_name)} cases per 100,000")
# })

output$test <- renderReactable({
    as.data.frame(narr_data()) %>% 
        reactable(
            defaultPageSize = 25,
        )
    
})


output$table <- renderReactable({
    as.data.frame(state_all()) %>%
        select(county_name, month, variable(),-geometry) %>%
        arrange(county_name) %>%
        reactable(
            filterable = TRUE,
            defaultPageSize = 25,
            groupBy = "county_name"
        )
    
})

}
# Run the application 
shinyApp(ui = ui, server = server)
