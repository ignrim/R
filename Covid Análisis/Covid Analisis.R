# COVID-19 Mobility trends
# pkgs --------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(plotly)
library(tsibble)
library(feasts)
library(fable)


# Apple data --------------------------------------------------------------

apple_df <- read_csv("applemobilitytrends-2020-06-22.csv")
apple <- apple_df %>% 
    pivot_longer(cols = -c(geo_type:country),
                 names_to = "date") %>% 
    select(-c(alternative_name)) %>% 
    mutate(country = if_else(is.na(country),region,country),
           date = ymd(date)
           ) %>% 
    mutate_if(is.character,as_factor) %>% 
    as_tsibble(index = date, key = c(region, transportation_type,`sub-region`,country))
apple <- apple %>% 
    group_by_key() %>% 
    mutate( change = value / first(value) - 1,
        change_label = sprintf("%.2f%%", change * 100)) %>% 
    ungroup()


# Google data -------------------------------------------------------------

google_df <- read_csv("Global_Mobility_Report.csv", 
                      col_types = cols(sub_region_2 = 
                                           col_character(),
                                       census_fips_code = 
                                           col_character()))

google <- google_df %>%
    select(-c(iso_3166_2_code:census_fips_code)) %>%
    pivot_longer(cols = -c(country_region_code:date)) %>%
    mutate(name = str_replace(name, 
                              pattern = 
                                  "_percent_change_from_baseline",
                              ""),
           sub_region_1 = if_else(is.na(sub_region_1),
                                  country_region,
                                  sub_region_1),
           value = value / 100,
           date = dmy(date)) %>%
    mutate_if(is.character,as_factor) %>% 
    mutate(name_label = sprintf("%.2f%%", value * 100)) %>% 
    as_tsibble(index = date, key = c(country_region, 
                                     sub_region_1,
                                     sub_region_2,
                                     name))


# User Interface ----------------------------------------------------------
# Here we define how the app is showing for the end user (if we want tabs,
# select, slider or checkbox inputs, ...) and if we want to show plots, 
# tables, forecasts ...
ui <- fluidPage(theme = shinytheme("united"),

    # Application title
    titlePanel(tags$div(HTML('Tendencias de movilidad por COVID-19 
                             <i class="fa fa-head-side-cough" style = "color:#069C27;"></i>')),
               windowTitle = "Tendencias de movilidad por COVID-19"),
    
    navbarPage(title = tags$div(HTML('Fuente de datos 
                             <i class="fa fa-biohazard";"></i>')),
               position = "static-top",
               tabPanel(icon = icon("apple"), title = "Apple",
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput(inputId = "pais",
                                             label = "Selecciona el país",
                                             choices = levels(apple$country),
                                             selected = "Mexico"),
                                 uiOutput("select2"),
                                 checkboxGroupInput(inputId = "transport_type",
                                                    label = "Tipo de transporte",
                                                    choices = levels(apple$transportation_type), inline = TRUE,
                                                    selected = "driving")
                             ),
                             
                             # Show a plot of the generated distribution
                             mainPanel(
                                 plotlyOutput("covid_apple")
                             )
                         )
                   
               ),
               tabPanel(icon = icon("google"), title =  "Google",
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput(inputId = "pais2",
                                             label = "Selecciona el país",
                                             choices = levels(apple$country),
                                             selected = "Mexico"),
                                 uiOutput("region1"),
                                 # conditionalPanel()
                                 uiOutput("region2"),
                                 checkboxGroupInput(inputId = "sector",
                                                    label = "Sector",
                                                    choices = levels(google$name), 
                                                    inline = TRUE,
                                                    selected = levels(google$name))
                             ),
                             
                             # Show a plot of the generated distribution
                             mainPanel(
                                 plotlyOutput("covid_google")
                             )
                         )
                   
               )
        
    )

    
)


# Server logic ------------------------------------------------------------

server <- function(input, output) {

# Apple -------------------------------------------------------------------

    output$select2 <- renderUI({
        selectInput(inputId = "regiones",
                    label = "Selecciona la región",
                    choices = apple %>% filter(country == input$pais) %>% distinct(region) %>% pull())
    })
    
    df <- reactive({
        apple %>% 
            filter(country == input$pais,
                   transportation_type %in% c(input$transport_type),
                   region == input$regiones
                   
            )
    })
    
    df_season_adj <- reactive({
        df() %>% 
            model(STL(change)) %>% 
            components()
    })

    output$covid_apple <- renderPlotly({
        p <- df() %>% 
            ggplot(aes(x = date, y = change, 
                       color = transportation_type,
                       label = change_label)) +
            geom_line() + 
            geom_hline(yintercept = 0,
                       linetype = "dashed", 
                       color = "firebrick") +
            annotate("text", label = "Baseline", 
                     x = last(apple$date)-5, y = 0.05, 
                     size = 3, color = "firebrick") + 
            guides(color = guide_legend(title = NULL)) +
            scale_y_continuous(labels = scales::percent)+
            ggtitle(paste("Mobility trends in ",input$regiones, ",", input$pais))
        
        ggplotly(p, tooltip = c("x","label","color"))
    })
    
    
    output$region1 <- renderUI({
        selectInput(inputId = "subregion1",
                    label = "Selecciona la región",
                    choices = google %>% filter(country_region == input$pais2) %>% 
                        distinct(sub_region_1) %>% pull(),
                    selected = input$pais2)
    })

# Google ------------------------------------------------------------------

    
    dfg <- reactive({
        google %>% 
            filter(country_region == input$pais2,
                   name %in% c(input$sector),
                   sub_region_1 == input$subregion1
                   
            )
    })
    
    output$covid_google <- renderPlotly({
        p <- dfg() %>% 
            ggplot(aes(x = date, y = value, 
                       color = name,
                       label = name_label)) +
            geom_line() + 
            geom_hline(yintercept = 0,
                       linetype = "dashed", 
                       color = "firebrick") +
            annotate("text", label = "Baseline", 
                     x = last(google$date)-5, y = 0.05, 
                     size = 3, color = "firebrick") + 
            guides(color = guide_legend(title = NULL)) +
            scale_y_continuous(labels = scales::percent)+
            ggtitle(paste("Mobility trends in ",input$subregion1,
                          ",", input$pais2)) +
            facet_wrap(~ name) +
            theme(legend.position = "none")
        
        ggplotly(p, tooltip = c("x","label","color"))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

