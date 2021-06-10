#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(scales)
library(tidyverse)
library(shinythemes)
library(viridis)
library(stringr)
library(glue)
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
                titlePanel("School Attendance Patterns During the COVID-19 Pandemic"),
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
                        h5(strong("Interpretive Narrative")),
                        textOutput("narr"),
                        width = 3
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        h5(strong("This dashboard visualizes the change in school attendance for each month in 2020 compared to 2019 for each 
                           county in the western United States. Data is also provided for the total COVID-19 cases by county, when available.
                                  Attendance patterns are represented using anonymized cell phone foot-traffic data at school sites.")),
                            tmapOutput("tmap2", width = "100%", height = 700),
                            reactableOutput("test"),
                            h3("School Attendance Data", align = "center"),
                            reactableOutput("table"),
                            h3("CDC County Case Data", align = "center"),
                            reactableOutput("cdc_table"),
                        h5("Data source: https://www.nature.com/articles/s41562-021-01087-8")
                        )
                    ))

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

state_data <- reactive({
    county_data %>% 
        filter(month == month(),
               state_abb == state())
})

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
        if (input$share_closed == "mean_change") {
            tm_polygons(
                variable(),
                title = "Percent Change",
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
            title = "Percent of Schools",
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

rv_map <- reactiveValues()

county <- eventReactive(input$tmap2_shape_click, {
    click <- input$tmap2_shape_click
    print(input$tmap2_shape_click$id)
    print(substr(click$id, 2, 6))
    rv_map$county_click <- (substr(click$id, 2, 6))
    print(rv_map$county_click)
    return(rv_map$county_click)
})

narr_data <- reactive({
    as.data.frame(county_data) %>%
        filter(month == input$month,
               state_abb == input$state,
               fips == county()) %>% 
        select(county_name, month_names, capita_covid, variable())
})

output$narr <- renderText({
    if (is.null(county())) {
        text <- paste("")
        return(text)
}
    if (input$share_closed == "mean_change") {
    text <- glue("In {paste(narr_data()$county_name)} County, schools saw a 
         {paste(narr_data()[,4])} percent change in visitors during {paste(narr_data()$month_names)} of 2020, compared to the previous year. 
                 Reported cases in {paste(narr_data()$county_name)} County during {paste(narr_data()$month_names)} 2020 were {paste(narr_data()$capita_covid)} per 100,000 residents.")
    return(text)
    }
    else {
        text <- glue("In {paste(narr_data()$county_name)} County, approximately {paste(narr_data()[,4])} percent of schools saw at least a 
          {paste(pct())} percent decline in visitors during {paste(narr_data()$month_names)} of 2020, compared to the previous year. 
                 Reported cases in {paste(narr_data()$county_name)} County during {paste(narr_data()$month_names)} 2020 were {paste(narr_data()$capita_covid)} per 100,000 residents.")
        return(text)
    }
})


output$table <- renderReactable({
    school_table <- as.data.frame(county_data) %>% #change data to master dataframe
        select(month_names, county_name, state_abb, total_students, number_schools, share_all_closed_25, share_all_closed_50, share_all_closed_75, mean_all_change) %>%
        arrange(county_name, state_abb) %>%
        rename("% of Schools with at least 25% visitor decline from 2019" = share_all_closed_25,
               "% of Schools with at least 50% visitor decline from 2019" = share_all_closed_50,
               "% of Schools with at least 75% visitor decline from 2019" = share_all_closed_75,
               "Mean % change of in person visits from 2019" = mean_all_change,
               "Month" = month_names,
               "County Name" = county_name,
               "State" = state_abb,
               "Total Students" = total_students,
               "Number of Schools" = number_schools) %>%
        reactable(filterable = TRUE,
                  defaultPageSize = 11,
                  defaultColDef = colDef(align = "center")
        )
    return(school_table)
    
})

output$cdc_table <- renderReactable({
as.data.frame(county_data) %>% 
    select(month_names, county_name, state_abb, all_cases, capita_covid, share_pop) %>% 
    reactable(
        searchable = TRUE,
        filterable = TRUE,
        columns = list(
            month_names = colDef(name="Month"),
            county_name = colDef(name="County"),
            state_abb = colDef(name="State"),
            all_cases = colDef(name="Total COVID-19 cases"),
            capita_covid = colDef(name="COVID-19 cases per 100,000"),
            share_pop = colDef(name="Cases as Share of Population")),
        defaultPageSize = 11,
        defaultColDef = colDef(align = "center"))
})
}
# Run the application 
shinyApp(ui = ui, server = server)
