library(tidyverse)
library(readxl)
library(httr)
library(wesanderson)
library(scales)

ui <- navbarPage("CoVID cases by country and state",
    
    ## Countries
    tabPanel("Countries",
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
                                                      "Brazil", "Egypt", "India")),
                                 selected = c("United States of America", "China", "Germany", "Italy"),
                                 multiple = TRUE),
                     
                     #Data to be plotted
                     selectInput(inputId = "data_type_country",
                                 label = "Plotted Data:",
                                 choices = c("Cases per day",
                                             "Total Confirmed Cases",
                                             "Deaths"),
                                 selected = "Cases per day"),
                     radioButtons(inputId = "pop_divider_country",
                                  label = "Data relativization:",
                                  choices = c("Not relativized", 
                                              "Percent of Population",
                                              "Cases per 100,000 people"),
                                  selected = "Not relativized"
                                  )),
             mainPanel(
                 #Output of country
                 plotOutput(outputId = "country_plot")
             )),
             hr(),
             print("For up to date information on CoVID-19 visit https://viralization.org/covid19.php"),
             br(),
             print(paste("Country data collected from ecdc.europa.eu on", Sys.Date())),
             br(),
             br(),
             print("Shiny App built by Zach Quinlan. Code can be found at https://github.com/Zquinlan/ShinyApp_CoVID19"),
             br(),
             print("Suggestions or comments please email Zquinlan@ucsd.edu"),
             br()),
    
    ## States
    tabPanel("US states and counties",
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
                        selected = "Cases per day"),
            radioButtons(inputId = "pop_divider_state",
                         label = "Data relativization:",
                         choices = c("Not relativized", 
                                     "Percent of Population",
                                     "Cases per 100,000 people"),
                         selected = "Not relativized")
        ),
        mainPanel(
            #Output of state
            plotOutput(outputId = "state_plot"
            )
        )
    ),
    br(),
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
    ),
    hr(),
    print("For up to date information on CoVID-19 visit https://viralization.org/covid19.php"),
    br(),
    print(paste("State and county data collected from github.com/nytimes/covid-19-data on", Sys.Date())),
    br(),
    br(),
    print("\n Shiny App built by Zach Quinlan. Code can be found at https://github.com/Zquinlan/ShinyApp_CoVID19"),
    br()),
    tabPanel("Information",
             h1("Why did I make this?"),
             print("There are a lot of really great web apps and plot already available to the public. 
                   I specifically made this one because I was not finding an addequate way to compare state and county data
                   how I wanted to. Moreover, a lot of my friends, family, and commuity members have been feeling like they 
                   have zero control over this whole situation or the data which they are presented. I built this in the hopes 
                   that it would give people who are not data-oriented or maybe just didn't have the time a quick way to look 
                   at the datamthemselves without having to read more news about the situation. I am always looking for ways
                   to improve this or give people more data to look at. If you do have any suggestions, do not hesitate to
                   contact me directly (email is below)."),
             br(),
             br(),
             print("I hope that it helps you during this time; stay safe and healthy <3"),
             br(),
             br(),
             h3("CoVID-19 Resources:"),
             print("- More information on CoVID-19 and rational responses to the pandemic, can be found at 
             The Viral Information Institute (VII; https://viralization.org/covid19.php)"),
             br(),
             print("- Another great shiny app to compare countries trejectories in terms of days post infection
                   can be found at https://robinhweide.shinyapps.io/Covid-19_shifter/"),
             br(),
             br(),
             h3("Caveats and Acknowledgements"),
             hr(),
             print("US Census data for states and counties is from 2018 as the new census data has not yet been reported.
                   Country population data is from 2018. As such, any relativizations done by population have inherent error."),
             br(),
             br(),
             h5("Data is not my own and was downloaded directly from:"),
             print("The New York Times (Github.com/nytimes/covid-19-data)"),
             br(),
             print("The European Centre for Disease Control (ecdc.europa.eu)"),
             br(),
             print("The 2010-2018 US Census report"),
             br(),
             br(),
             br(),
             print("\n Shiny App built by Zach Quinlan. Code can be found at https://github.com/Zquinlan/ShinyApp_CoVID19"),
             br(),
             print("Suggestions or comments please email Zquinlan@ucsd.edu"))
    )

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
            mutate(`Total Confirmed Cases` = cumsum(`Cases per day`),
                   )%>%
            ungroup()%>%
            select(date, country, input$data_type_country, popData2018)%>%
            rename(plot = 3)%>%
            mutate(date = as.Date(date, "%d/%m/%y"),
                   country = gsub("_", " ", country),
                   country = case_when(country == "CuraÃ§ao" ~ "Curacao",
                                       TRUE ~ as.character(country)),
                   plot = case_when(input$pop_divider_country == "Not relativized" ~ plot,
                                    input$pop_divider_country == "Percent of Population" ~ plot/popData2018*100,
                                    input$pop_divider_country == "Cases per 100,000 people" ~ plot/popData2018*100000))%>%
            filter(date != "2020-12-31")
        
        
        #Pulls in the input countries
        countries_filtered <- countries_raw%>%
            filter(country %in% input$country_select)
        
        number_countries_country <- length(input$country_select)
        
        countries_filtered%>%
            ggplot(aes(date, plot, color = country)) +
            geom_line() +
            geom_point() +
            scale_x_date(date_breaks = "4 days", limits = c(Sys.Date() - input$previous_days_country, NA), date_labels = "%b %d") +
            xlab(paste("Date (past", input$previous_days_country, "days)")) +
            ylab(paste(input$data_type_country, " (", input$pop_divider_country, ")", sep = "")) +
            scale_y_log10(limits = c(min(countries_filtered$plot - 1), max(countries_filtered$plot + .05*max(countries_filtered$plot))),
                          breaks = trans_breaks("log10", function(x) 10^x)) +
            scale_color_manual(values = wes_palette("Darjeeling1", number_countries_country, type = c('continuous'))) +
            ggtitle("CoVID-19 cases by country") +
            theme(
                axis.text.x = element_text(angle = 60, size = 15, hjust = 1),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 17),
                panel.background = element_rect(fill = "transparent"), 
                plot.background = element_rect(fill = "transparent", color = NA), 
                panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',colour = "gray"),
                panel.grid.minor.y = element_line(size = 0.2, linetype = 'solid',colour = "gray"), 
                panel.grid.major.x = element_line(size = 0.2, linetype = 'solid',colour = "gray"),
                legend.background = element_rect(fill = "transparent"), 
                legend.box.background = element_rect(fill = "transparent"),
                legend.text = element_text(size = 15),
                legend.title = element_blank(),
                legend.position = "top",
                plot.title = element_text(hjust = 0.5, size = 25)) 
    })
    output$state_plot <- renderPlot({
        # Downlaod state data
        state_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
        census_url <- "https://raw.githubusercontent.com/Zquinlan/ShinyApp_CoVID19/master/us_census_data.csv"
        
        #read and clean the state data
        county_census <- read_csv(url(census_url))%>%
            mutate(population = as.numeric(population))
        
        state_census <- county_census%>%
            group_by(state)%>%
            summarize_if(is.numeric, sum)
        
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
            left_join(state_census, by = "state")%>%
            select(date, state, input$data_type_state, population)%>%
            rename(plot = 3)%>%
            mutate(plot = case_when(input$pop_divider_state == "Not relativized" ~ plot,
                                    input$pop_divider_state == "Percent of Population" ~ plot/population*100,
                                    input$pop_divider_state == "Cases per 100,000 people" ~ plot/population*100000))
        
        #Filter out states selected
        states_filtered <- state_raw%>%
            filter(state %in% input$state_select)
        
        number_countries_state <- length(input$state_select)
        
        states_filtered%>%
            ggplot(aes(date, plot, color = state)) +
            geom_line() +
            geom_point() +
            scale_x_date(date_breaks = "4 days", limits = c(Sys.Date() - input$previous_days_state, NA), date_labels = "%b %d") +
            xlab(paste("Date (past", input$previous_days_state, "days)")) +
            ylab(paste(input$data_type_state, " (", input$pop_divider_state, ")", sep = "")) +
            scale_y_log10() + 
            scale_color_manual(values = wes_palette("Darjeeling1", number_countries_state, type = c('continuous'))) +
            ggtitle("CoVID-19 cases by state") +
            theme(
                axis.text.x = element_text(angle = 60, size = 15, hjust = 1),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 17),
                panel.background = element_rect(fill = "transparent"), 
                plot.background = element_rect(fill = "transparent", color = NA), 
                panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',colour = "gray"), 
                panel.grid.minor.y = element_line(size = 0.2, linetype = 'solid',colour = "gray"), 
                panel.grid.major.x = element_line(size = 0.2, linetype = 'solid',colour = "gray"),
                legend.background = element_rect(fill = "transparent"), 
                legend.box.background = element_rect(fill = "transparent"),
                legend.text = element_text(size = 15),
                legend.title = element_blank(),
                legend.position = "top",
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
        # Defining URL for county data
        county_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
        census_url <- "https://raw.githubusercontent.com/Zquinlan/ShinyApp_CoVID19/master/us_census_data.csv"
        
        #read and clean the state data
        county_census <- read_csv(url(census_url))%>%
            mutate(population = as.numeric(population))
        
        #Reading in and cleaning dataframes
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
            left_join(county_census, by = c("state", "county"))%>%
            select(date, state, county, input$data_type_state, population)%>%
            rename(plot = 4)%>%
            mutate(plot = case_when(input$pop_divider_state == "Not relativized" ~ plot,
                             input$pop_divider_state == "Percent of Population" ~ plot/population*100,
                             input$pop_divider_state == "Cases per 100,000 people" ~ plot/population*100000))
        
        ## Plotting
        counties_filtered <- counties_raw%>%
            filter(state %in% input$statecounty_select)%>%
            filter(county %in% input$county_select)
        
        number_countries_county <- length(input$county_select)
        
        counties_filtered%>%
            ggplot(aes(date, plot, color = county)) +
            geom_line() +
            geom_point() +
            scale_x_date(date_breaks = "4 days", limits = c(Sys.Date() - input$previous_days_state, NA), date_labels = "%b %d") +
            xlab(paste("Date (past", input$previous_days_state, "days)")) +
            ylab(paste(input$data_type_state, " (", input$pop_divider_state, ")", sep = "")) +
            scale_y_log10() + 
            scale_color_manual(values = wes_palette("Darjeeling1", number_countries_county, type = c('continuous'))) +
            ggtitle(paste("CoVID-19 cases by county in",
                          input$statecounty_select)) +
            theme(
                axis.text.x = element_text(angle = 60, size = 15, hjust = 1),
                axis.text.y = element_text(size = 15),
                axis.title = element_text(size = 17),
                panel.background = element_rect(fill = "transparent"), 
                plot.background = element_rect(fill = "transparent", color = NA), 
                panel.grid.major.y = element_line(size = 0.2, linetype = 'solid',colour = "gray"), 
                panel.grid.minor.y = element_line(size = 0.2, linetype = 'solid',colour = "gray"), 
                panel.grid.major.x = element_line(size = 0.2, linetype = 'solid',colour = "gray"),
                legend.background = element_rect(fill = "transparent"), 
                legend.box.background = element_rect(fill = "transparent"),
                legend.text = element_text(size = 15),
                legend.title = element_blank(),
                legend.position = "top",
                plot.title = element_text(hjust = 0.5, size = 25)) 
        
    })
}




shinyApp(ui = ui, server = server)


