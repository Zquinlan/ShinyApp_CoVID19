library(tidyverse)
library(readxl)
library(httr)
library(wesanderson)
library(scales)



ui <- fluidPage(
    #title
    titlePanel("CoVID cases by country and state"),
    
    ## Countries
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "previous_days",
                        label = "Number of Days",
                        min = 1,
                        max = 70,
                        v = 50
            ),
            ## Input country selector
            selectInput(inputId = "country_select",
                        label = "Countries",
                        choices = str_sort(c("United States of America", "China", "United Kingdom", "Sweden", "Germany",
                                    "Italy", "France", "Spain", "South Korea", "Switzerland", "New Zealand", "Israel",
                                    "Thailand", "Vietnam", "Singapore", "Netherlands", "Mexico", "Japan", "Australia",
                                    "Palestine", "Curacao", "South Africa")),
                        selected = c("United States of America", "China", "Germany", "Italy"),
                        multiple = TRUE)
        ),
        mainPanel(
            #Output of country
            plotOutput(outputId = "country_plot"
                       
            ))),
    
    ## States
    sidebarLayout(
        
        sidebarPanel(
            selectInput(inputId = "state_select",
                        label = "States",
                        choices = c(state.name%>% as.vector()),
                        selected = c("Colorado", "California", "Washington", "Utah", "New Jersey"),
                        multiple = TRUE)
        ),
        mainPanel(
            #Output of state
            plotOutput(outputId = "state_plot"
            )
        )
    ))

server <- function(input, output) {
    
    
    
    
    output$country_plot <- renderPlot( {
        #plotting
        ## Download country data
        url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
        GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
        
        ## read in dfs
        countries_raw <-read_excel(tf)%>%
            rename(date = dateRep,
                   country = countriesAndTerritories)%>%
            select(date, cases, deaths, country)%>%
            mutate(date = as.Date(date),
                   location_type = "country")%>%
            mutate(country = gsub("_", " ", country),
                   country = case_when(country == "CuraÃ§ao" ~ "Curacao",
                                       TRUE ~ as.character(country)))
        
        #Pulls in the input countries
        countries_filtered <- countries_raw%>%
            filter(country %in% input$country_select)
        
        countries_filtered%>%
            ggplot(aes(date, cases, color = country)) +
            geom_line() +
            geom_point() +
            scale_x_date(date_breaks = "4 days", limits = c(Sys.Date() - input$previous_days, NA), date_labels = "%b %d") +
            xlab(paste("Date (past", input$previous_days, "days)")) +
            ylab("New cases") +
            scale_y_log10(limits = c(1, max(countries_filtered$cases + 5000)),
                          breaks = trans_breaks("log10", function(x) 10^x)) +
            # scale_color_manual(values = wes_palette("Darjeeling1", 5, type = c('discrete'))) +
            ggtitle(paste("New daily CoVID-19 cases by country \n (data downloaded from ecdc.europa.eu on", Sys.Date(), ")")) +
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
    
    output$state_plot <- renderPlot({
        # Downlaod state data
        state_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
        
        #read and clean the state data
        state_raw <- read_csv(url(state_url))%>%
            select(-fips)%>%
            mutate(date = as.Date(date),
                   location_type = "state")%>%
            group_by(state)%>%
            arrange(date)%>%
            mutate(new_cases = cases - lag(cases, default = first(date)),
                   new_cases = case_when(new_cases < 0 ~ 0,
                                         TRUE ~ as.numeric(new_cases)))%>%
            ungroup()
        
        #Filter out states selected
        
        states_filtered <- state_raw%>%
            filter(state %in% input$state_select)
        
        states_filtered%>%
            ggplot(aes(date, new_cases, color = state)) +
            geom_line() +
            geom_point() +
            scale_x_date(date_breaks = "4 days", limits = c(Sys.Date() - input$previous_days, NA), date_labels = "%b %d") +
            xlab(paste("Date (past", input$previous_days, "days)")) +
            ylab("New Cases") +
            scale_y_log10() + 
            # scale_color_manual(values = wes_palette("Darjeeling1", 5, type = c('discrete'))) +
            ggtitle(paste("New daily CoVID-19 cases by state \n (data downloaded from github.com/nytimes/covid-19-data on", Sys.Date(), ")")) +
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
}




shinyApp(ui = ui, server = server)


