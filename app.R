library(tidyverse)
library(readxl)
library(httr)
library(wesanderson)
library(scales)

ui <- navbarPage("CoVID cases by country and state",
    
    ## Countries
    tabPanel("Countries",
             titlePanel(paste("Country data collected from ecdc.europa.eu on", Sys.Date())),
             br(),
             br(),
             sidebarLayout(
                 sidebarPanel(
                     #Number of days on X axis
                     sliderInput(inputId = "previous_days_country",
                                 label = "Number of Days:",
                                 min = 1,
                                 max = 70,
                                 v = 50),
                     
                     ## Input country selector
                     selectInput(inputId = "country_select",
                                 label = "Countries:",
                                 choices = str_sort(c("United States of America", "China", "United Kingdom", "Sweden", "Germany",
                                                      "Italy", "France", "Spain", "South Korea", "Switzerland", "New Zealand", "Israel",
                                                      "Thailand", "Vietnam", "Singapore", "Netherlands", "Mexico", "Japan", "Australia",
                                                      "Palestine", "Curacao", "South Africa", "Colombia", "Argentina", "Costa Rica", "Ecuador",
                                                      "Brazil")),
                                 selected = c("United States of America", "China", "Germany", "Italy"),
                                 multiple = TRUE),
                     
                     #Data to be plotted
                     selectInput(inputId = "data_type_country",
                                 label = "Plotted Data:",
                                 choices = c("Cases per day",
                                             "Total Confirmed Cases",
                                             "Deaths"),
                                 selected = "Cases per day")),
             mainPanel(
                 #Output of country
                 plotOutput(outputId = "country_plot")
             ))),
    
    ## States
    tabPanel("US states and counties",
             titlePanel(paste("State and county data collected from github.com/nytimes/covid-19-data on", Sys.Date())),
             br(),
             br(),
    sidebarLayout(
        sidebarPanel(
            #Day Slider
            sliderInput(inputId = "previous_days_state",
                        label = "Number of Days:",
                        min = 1,
                        max = 70,
                        v = 50),
            #State Selector
            selectInput(inputId = "state_select",
                        label = "States:",
                        choices = c(state.name%>% as.vector()),
                        selected = c("Colorado", "California", "Washington", "Utah", "New Jersey"),
                        multiple = TRUE),
            #Data to be plotted
            selectInput(inputId = "data_type_state",
                        label = "Plotted Data:",
                        choices = c("Cases per day",
                                    "Total Confirmed Cases",
                                    "Deaths"),
                        selected = "Cases per day")
        ),
        mainPanel(
            #Output of state
            plotOutput(outputId = "state_plot"
            )
        )
    ),
    
    #County
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "statecounty_select",
                        label = "State:",
                        choices = c(state.name%>% as.vector()),
                        selected = "California",
                        multiple = FALSE
                
            ),
            uiOutput("secondSelection"),
        ),
        mainPanel(
            plotOutput(outputId = "county_plot")
        )
    )))

server <- function(input, output) {
    
    
    
    
    output$country_plot <- renderPlot( {
        #plotting
        ## Download country data
        GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
        
        ## read in dfs
        countries_raw <-read_csv(tf)%>%
            rename(date = dateRep,
                   country = countriesAndTerritories,
                   `Cases per day` = `cases`,
                   Deaths = deaths)%>%
            group_by(country)%>%
            arrange(date)%>%
            mutate(`Total Confirmed Cases` = cumsum(`Cases per day`))%>%
            ungroup()%>%
            select(date, country, input$data_type_country)%>%
            rename(plot = 3)%>%
            mutate(date = as.Date(date, "%d/%m/%y"),
                   country = gsub("_", " ", country),
                   country = case_when(country == "CuraÃ§ao" ~ "Curacao",
                                       TRUE ~ as.character(country)))%>%
            filter(date != "2020-12-31")
        
        
        #Pulls in the input countries
        countries_filtered <- countries_raw%>%
            filter(country %in% input$country_select)
        
        countries_filtered%>%
            ggplot(aes(date, plot, color = country)) +
            geom_line() +
            geom_point() +
            scale_x_date(date_breaks = "4 days", limits = c(Sys.Date() - input$previous_days_country, NA), date_labels = "%b %d") +
            xlab(paste("Date (past", input$previous_days_country, "days)")) +
            ylab(input$data_type_country) +
            scale_y_log10(limits = c(1, max(countries_filtered$plot + 5000)),
                          breaks = trans_breaks("log10", function(x) 10^x)) +
            # scale_color_manual(values = wes_palette("Darjeeling1", 5, type = c('discrete'))) +
            ggtitle("New daily CoVID-19 cases by country") +
            theme(
                axis.text.x = element_text(angle = 60, size = 10, hjust = 1),
                panel.background = element_rect(fill = "transparent"), 
                plot.background = element_rect(fill = "transparent", color = NA), 
                panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',colour = "gray"),
                panel.grid.minor.y = element_line(size = 0.2, linetype = 'solid',colour = "gray"), 
                panel.grid.major.x = element_line(size = 0.2, linetype = 'solid',colour = "gray"),
                legend.background = element_rect(fill = "transparent"), 
                legend.box.background = element_rect(fill = "transparent"),
                plot.title = element_text(hjust = 0.5, size = 25)) 
    })
    
    output$state_plot <- renderPlot({
        # Downlaod state data
        state_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
        
        #read and clean the state data
        state_raw <- read_csv(url(state_url))%>%
            select(-fips)%>%
            mutate(date = as.Date(date))%>%
            group_by(state)%>%
            arrange(date)%>%
            mutate(`Cases per day` = cases - lag(cases, default = first(date)),
                   `Cases per day` = case_when(`Cases per day` < 0 ~ 0,
                                         TRUE ~ as.numeric(`Cases per day`)))%>%
            rename(`Total Confirmed Cases` = cases,
                   Deaths = deaths)%>%
            ungroup()%>%
            select(date, state, input$data_type_state)%>%
            rename(plot = 3)
        
        #Filter out states selected
        states_filtered <- state_raw%>%
            filter(state %in% input$state_select)
        
        states_filtered%>%
            ggplot(aes(date, plot, color = state)) +
            geom_line() +
            geom_point() +
            scale_x_date(date_breaks = "4 days", limits = c(Sys.Date() - input$previous_days_state, NA), date_labels = "%b %d") +
            xlab(paste("Date (past", input$previous_days_state, "days)")) +
            ylab(input$data_type_state) +
            scale_y_log10() + 
            # scale_color_manual(values = wes_palette("Darjeeling1", 5, type = c('discrete'))) +
            ggtitle("New daily CoVID-19 cases by state") +
            theme(
                axis.text.x = element_text(angle = 60, size = 10, hjust = 1),
                panel.background = element_rect(fill = "transparent"), 
                plot.background = element_rect(fill = "transparent", color = NA), 
                panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',colour = "gray"), 
                panel.grid.minor.y = element_line(size = 0.2, linetype = 'solid',colour = "gray"), 
                panel.grid.major.x = element_line(size = 0.2, linetype = 'solid',colour = "gray"),
                legend.background = element_rect(fill = "transparent"), 
                legend.box.background = element_rect(fill = "transparent"),
                plot.title = element_text(hjust = 0.5, size = 30)) 
    })
    output$secondSelection <- renderUI({
        counties_filter_df <- read_csv("https://raw.githubusercontent.com/Zquinlan/ShinyApp_CoVID19/master/counties_filter_df.csv")
        
            selectInput(inputId = "county_select",
                        label = "Counties:",
                        choices = c((counties_filter_df%>%
                                        filter(state == input$statecounty_select))$county%>%
                                        as.factor()%>%
                                        levels()),
                        selected = c("San Diego", "Los Angeles", "San Francisco", "Humboldt", "Riverside"),
                        multiple = TRUE)
        })
    output$county_plot <- renderPlot({
        county_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
        
        counties_raw <- read_csv(url(county_url))%>%
            select(-fips)%>%
            mutate(date = as.Date(date))%>%
            group_by(state, county)%>%
            arrange(date)%>%
            mutate(`Cases per day` = cases - lag(cases, default = first(date)),
                   `Cases per day` = case_when(`Cases per day` < 0 ~ 0,
                                         TRUE ~ as.numeric(`Cases per day`)))%>%
            rename(`Total Confirmed Cases` = cases,
                   Deaths = deaths)%>%
            ungroup()%>%
            select(date, state, county, input$data_type_state)%>%
            rename(plot = 4)
        
        ## Plotting
        counties_filtered <- counties_raw%>%
            filter(state %in% input$statecounty_select)%>%
            filter(county %in% input$county_select)
        
        counties_filtered%>%
            ggplot(aes(date, plot, color = county)) +
            geom_line() +
            geom_point() +
            scale_x_date(date_breaks = "4 days", limits = c(Sys.Date() - input$previous_days_state, NA), date_labels = "%b %d") +
            xlab(paste("Date (past", input$previous_days_state, "days)")) +
            ylab(input$data_type_state) +
            scale_y_log10() + 
            # scale_color_manual(values = wes_palette("Darjeeling1", 5, type = c('discrete'))) +
            ggtitle(paste("New daily CoVID-19 cases by county in",
                          input$statecounty_select)) +
            theme(
                axis.text.x = element_text(angle = 60, size = 10, hjust = 1),
                panel.background = element_rect(fill = "transparent"), 
                plot.background = element_rect(fill = "transparent", color = NA), 
                panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',colour = "gray"), 
                panel.grid.minor.y = element_line(size = 0.2, linetype = 'solid',colour = "gray"), 
                panel.grid.major.x = element_line(size = 0.2, linetype = 'solid',colour = "gray"),
                legend.background = element_rect(fill = "transparent"), 
                legend.box.background = element_rect(fill = "transparent"),
                plot.title = element_text(hjust = 0.5, size = 25)) 
        
    })
}




shinyApp(ui = ui, server = server)


