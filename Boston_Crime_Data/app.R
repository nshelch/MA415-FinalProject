library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggmap)
library(DT)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Boston Crime Data"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Introduction", tabName = "intro", icon = icon("chevron-right")),
                        menuItem("Year", tabName = "year", icon = icon("chevron-right"),
                                 menuSubItem("2015", tabName = "year_2015", icon = icon("chevron-right")),
                                 menuSubItem("2016", tabName = "year_2016", icon = icon("chevron-right")),
                                 menuSubItem("2017", tabName = "year_2017", icon = icon("chevron-right")),
                                 menuSubItem("2018", tabName = "year_2018", icon = icon("chevron-right"))),
                        menuItem("Cumulative", tabName = "cumulative", icon = icon("chevron-right"))
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(
                          tabName = "intro",
                          h2("Introduction", align = "center"),
                          p("In order to analyze the trend of crime in Boston data was gathered from 
                            Analyze Boston*, which contains the date, location, and crime that was committed between June 15th, 2015 and May 2nd, 2018 
                            amongst other things. The original data file contained a number of variables, some of which were changed or 
                            excluded for the analysis. 
                            A breakdown of the variables can be seen below:",
                            style = "font-family: 'arial'"),
                          br(),
                          p("-",strong("Incident Number:"), "Internal BPD report number",
                            style = "font-family: 'arial'"),
                          p("-",strong("Offense Code:"), "Numerical code of offense description",
                            style = "font-family: 'arial'"),
                          p("-",strong("Offense Code Group:"), "Internal categorization of offense description",
                            style = "font-family: 'arial'"),
                          p("-",strong("Offense Description:"), "Primary descriptor of incident",
                            style = "font-family: 'arial'"),
                          p("-",strong("District:"), "District the crime was reported in",
                            style = "font-family: 'arial'"),
                          p("-",strong("Reporting Area:"), "Reporting area number associated with where the crime was reported from",
                            style = "font-family: 'arial'"),
                          p("-",strong("Shooting:"), " Indicating a shooting took place",
                            style = "font-family: 'arial'"),
                          p("-",strong("Occurred on Date:"), " Earliest date and time the incident could have taken place",
                            style = "font-family: 'arial'"),
                          p("-",strong("URC Part:"), "Universal Crime Reporting number",
                            style = "font-family: 'arial'"),
                          p("-",strong("Street:"), "Street name the incident took place",
                            style = "font-family: 'arial'"),
                          br(),
                          p("* Data was gathered from:", a("https://data.boston.gov/dataset/crime-incident-reports-august-2015-to-date-source-new-system"),
                            style = "font-family: 'arial'")
                        ),
                        
                        tabItem(
                          
                          tabName = "year_2015",
                          h2("Crime Data: 2015"),
                          p("First lets look at the distribution of different types of crime that occurred in 2015:",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_type_2015", height = 600, width = 800)
                          ),
                          br(),
                          p("Now lets look at the trend of crime rates over the course of the year",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_year_2015", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_year_2015"),
                                   br(),
                                   DT::dataTableOutput("table_year_2015", width = 700)
                          ),
                          br(),
                          p("The steep drop in crimes in June is due to the data set starting from June 15. Due to the lack
                            of data for the first two weeks of June, it is understandable that it has a smaller number of 
                            crimes than the months to follow, which is also why it was excluded from the chi-squared test. 
                            As we look at the crime rates from July onward, we see a fairly steady
                            rate of crime occuring regardless of the month. Despite that, the small p-value we obtained after running a 
                            chi-squared test indicates that month and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("Now lets look at the trend of crime rates over the course of a month",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_month_2015", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_month_2015"),
                                   br(),
                                   DT::dataTableOutput("table_month_2015", width = 700)
                          ),
                          br(),
                          p("The small p-value we obtained after running a 
                            chi-squared test indicates that day of month and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("Now let's look into how the day and hour of a week affects crime rates",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_week_2015", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_week_2015"),
                                   br(),
                                   DT::dataTableOutput("table_week_2015", width = 700)
                          ),
                          br(),
                          p("As we can see, the most amount of crimes occur between the hours of 9am and midnight, with 
                            a decrease in crime between midnight and 8am. Furthermore, we can see that the number of crimes
                            that occur late in the evening (between midnight and 8am) increases on the weekends. This can be confirmed
                            by looking at the graph below. The small p-value we obtained after running a 
                            chi-squared test indicates that day of week and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("And now if we look at the hours alone",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_hour_2015", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_hour_2015"),
                                   br(),
                                   DT::dataTableOutput("table_hour_2015", width = 700)
                          ),
                          br(),
                          p("As you can see, there is a dip in crime between the hours of midnight and 10am, when the rate
                            of crime returns to what it was at midnight. The small p-value we obtained after running a 
                            chi-squared test indicates that hour and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("Now lets look at the distribution of crime based on the location:",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_loc_2015", height = 500, width = 700)   
                          ),
                          br(),
                          p("As we can see, most crime is centered around downtown Boston.",
                            style = "font-family: 'arial'")
                          
                        ),
                        
                        tabItem(
                          
                          tabName = "year_2016",
                          h2("Crime Data: 2016", 
                             align = "center"),
                          p("First lets look at the distribution of different types of crime that occurred in 2016:",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_type_2016", height = 600, width = 800)   
                          ),
                          br(),
                          p("Now lets look at the trend of crime rates over the course of the year",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_year_2016", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_year_2016"),
                                   br(),
                                   DT::dataTableOutput("table_year_2016", width = 700)
                          ),
                          br(),
                          p("We can see a steep drop in crimes in February, perhaps due to the shortened length of the month,
                            and that the majority of crimes occur during the summer. This may be due to the affect that heat
                            has on peoples' temperment, along with the end of school and summer break for many children and young
                            adults. The small p-value we obtained after running a 
                            chi-squared test indicates that month and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("Now lets look at the trend of crime rates over the course of a month",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_month_2016", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_month_2016"),
                                   br(),
                                   DT::dataTableOutput("table_month_2016", width = 700)
                          ),
                          p("The small p-value we obtained after running a 
                            chi-squared test indicates that day of month and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("Now let's look into how the day and hour of a week affects crime rates",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_week_2016", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_week_2016"),
                                   br(),
                                   DT::dataTableOutput("table_week_2016", width = 700)
                          ),
                          br(),
                          p("As we can see, the most amount of crimes occur between the hours of 9am and midnight, with 
                            a decrease in crime between midnight and 8am. Furthermore, we can see that the number of crimes
                            that occur late in the evening (between midnight and 8am) increases on the weekends. This can be confirmed
                            by looking at the graph below. The small p-value we obtained after running a 
                            chi-squared test indicates that day of week and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("And now if we look at the hours alone",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_hour_2016", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_hour_2016"),
                                   br(),
                                   DT::dataTableOutput("table_hour_2016", width = 700)
                          ),
                          br(),
                          p("As you can see, there is a dip in crime between the hours of 1am and 9am, when the rate
                            of crime returns to what it was at midnight. The small p-value we obtained after running a 
                            chi-squared test indicates that hour and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("Now lets look at the distribution of crime based on the location:",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_loc_2016", height = 500, width = 700)   
                          ),
                          br(),
                          p("As we can see, most crime is centered around downtown Boston.",
                            style = "font-family: 'arial'")
                        ),
                        
                        tabItem(
                          
                          tabName = "year_2017",
                          h2("Crime Data: 2017", 
                             align = "center"),
                          p("First lets look at the distribution of different types of crime that occurred in 2017:",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_type_2017", height = 600, width = 800)   
                          ),
                          br(),
                          p("Now lets look at the trend of crime rates over the course of the year",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_year_2017", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_year_2017"),
                                   br(),
                                   DT::dataTableOutput("table_year_2017", width = 700)
                          ),
                          br(),
                          p("We can see a steep drop in crimes in February, perhaps due to the shortened length of the month,
                            and that the majority of crimes occur during the summer. This may be due to the affect that heat
                            has on peoples' temperment, along with the end of school and summer break for many children and young
                            adults. The small p-value we obtained after running a 
                            chi-squared test indicates that month and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("Now lets look at the trend of crime rates over the course of a month",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_month_2017", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_month_2017"),
                                   br(),
                                   DT::dataTableOutput("table_month_2017", width = 700)
                          ),
                          br(),
                          p("The small p-value we obtained after running a 
                            chi-squared test indicates that day of month and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("Now let's look into how the day and hour of a week affects crime rates",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_week_2017", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_week_2017"),
                                   br(),
                                   DT::dataTableOutput("table_week_2017", width = 700)
                          ),
                          br(),
                          p("As we can see, the most amount of crimes occur between the hours of 9am and midnight, with 
                            a decrease in crime between midnight and 8am. Furthermore, we can see that the number of crimes
                            that occur late in the evening (between midnight and 8am) increases on the weekends. This can be confirmed
                            by looking at the graph below. The small p-value we obtained after running a 
                            chi-squared test indicates that day of week and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("And now if we look at the hours alone",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_hour_2017", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_hour_2017"),
                                   br(),
                                   DT::dataTableOutput("table_hour_2017", width = 700)
                          ),
                          br(),
                          p("As you can see, there is a dip in crime between the hours of midnight and 10am, when the rate
                            of crime returns to what it was at midnight. The small p-value we obtained after running a 
                            chi-squared test indicates that hour and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("Now lets look at the distribution of crime based on the location:",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_loc_2017", height = 500, width = 700)   
                          ),
                          br(),
                          p("As we can see, most crime is centered around downtown Boston.",
                            style = "font-family: 'arial'")
                          
                          ),
                        
                        tabItem(
                          tabName = "year_2018",
                          h2("Crime Data: 2018",
                             align = "center"),
                          p("First lets look at the distribution of different types of crime that occurred in 2018:",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_type_2018", height = 600, width = 800)   
                          ),
                          br(),
                          p("Now lets look at the trend of crime rates over the course of the year",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_year_2018", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_year_2018"),
                                   br(),
                                   DT::dataTableOutput("table_year_2018", width = 700)
                                   
                          ),
                          br(),
                          p("The steep drop in crimes in May is due to the fact that the current year is not yet over, which is why May crime rates
                            were excluded from the chi-squared test. Despite the prior months 
                            showing a steady rate in crime the p-value indicates that month and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("Now lets look at the trend of crime rates over the course of a month",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_month_2018", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_month_2018"),
                                   br(),
                                   DT::dataTableOutput("table_month_2018", width = 700)
                                   
                          ),
                          br(),
                          p("The graph shows that the crime rates have their peaks and troughs, and the p-value obtained from running
                            a chi-squared test indicates that the day of month and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("Now let's look into how the day and hour of a week affects crime rates",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_week_2018", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_week_2018"),
                                   br(),
                                   DT::dataTableOutput("table_week_2018", width = 700)   
                          ),
                          br(),
                          p("As we can see, the most amount of crimes occur between the hours of 9am and midnight, with 
                            a decrease in crime between midnight and 8am. Furthermore, we can see that the number of crimes
                            that occur late in the evening (between midnight and 8am) increases on the weekends. This can be confirmed
                            by looking at the graph below. The small p-value we obtained after running a 
                            chi-squared test indicates that day of the week and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("And now if we look at the hours alone",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_hour_2018", height = 500, width = 700),
                                   br(),
                                   textOutput("stat_hour_2018"),
                                   br(),
                                   DT::dataTableOutput("table_hour_2018", width = 700)   
                          ),
                          br(),
                          p("As you can see, there is a dip in crime between the hours of midnight and 10am, when the rate
                            of crime returns to what it was at midnight. The small p-value we obtained after running a 
                            chi-squared test indicates that hour and crime rate are not independent.",
                            style = "font-family: 'arial'"),
                          br(),
                          p("Now lets look at the distribution of crime based on the location:",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_loc_2018", height = 500, width = 700)   
                          ),
                          br(),
                          p("As we can see, most crime is centered around downtown Boston.",
                            style = "font-family: 'arial'")
                          
                        ),
                        
                        tabItem(
                          tabName = "cumulative",
                          h2("Looking at the Data From 2015 - 2018"),
                          p("Lets look at the cumulative distribution of crimes per month from the year 2015-2018:",
                            style = "font-family: 'arial'"),
                          fluidRow(align = "center",
                                   plotOutput("plot_cum_year", height = 500, width = 700)   
                          ),
                          p("As we can see, despite the dips and peaks when looking at the yearly crime trends for each 
                            year, the overall distribution throughout the years very similar."),
                          br(),
                          p("Furthermore, we can look if the total number of crimes that have occurred each year is the same. 
                            Given the lack of data for 2015 and 2018, we will only compare the crime totals from 2016 and 2017.",
                            style = "font-family: 'arial'"),
                          br(),
                          textOutput("stat_cumulative"),
                          br(),
                          DT::dataTableOutput("table_cumulative"),
                          br(),
                          p("As we can see, the diffrence in the total number of crimes that occurred in 2016 and 2017 is 
                            significant due to the small p-value. This means that total crime rate and year are not independent.")
                        
                        )
                        
                        )
                        )
                        )

server <- function(input, output) {
  month_colors <- c("#4424D6", "blue", "#347C98", "#66B032", "#B2D732", "#FEFE33", "#FCCC1A", "#FB9902","#FC600A","#FE2712",
                    "#C21460", "#8601AF", "#4424D6")
  
  boston_map <- get_map(location = "Boston", maptype = "satellite", zoom = 12)
  
  crime <- readRDS("filtered_crime.RDS")

  output$plot_type_2015 <- renderPlot({
    cur_crime <- crime %>% 
      filter(YEAR == 2015)
    
    # Crime Type
    ggplot(cur_crime, aes(x=OFFENSE_CODE_GROUP)) + 
      geom_bar( stat="count", color = "black", fill = "#C21460") +
      theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.1),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(face = "bold", hjust = -1)) +
      labs(title="Crimes in 2015", 
           x="Crime Type",
           y = "# of Times")
  })
  
  output$plot_year_2015 <- renderPlot({
    
    crimes_year <- crime %>% 
      filter(YEAR == 2015) %>% 
      select(MONTH) %>% 
      group_by(MONTH) %>% 
      summarize(`CRIME TOTALS` = n()) %>% 
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    # Year
    ggplot(crimes_year, aes(x = 6:12, y = `CRIME TOTALS`)) +
      geom_line(size = 1.2)+
      geom_point(size = 5, colour = month_colors[6:12]) + 
      theme(axis.text.x = element_text(size = 10),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10)) +
      labs(title="Crimes in 2015", 
           x= "Month",
           y = "# of Crimes") + 
      scale_x_continuous(breaks = 6:12,
                         labels= month.name[6:12])
  })
  
  output$plot_month_2015 <- renderPlot({

    crime_month <- crime %>%
      filter(YEAR == 2015) %>% 
      select(DAY) %>%
      group_by(DAY) %>%
        summarize(`CRIME TOTALS` = n()) %>%
        mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))

    # Day of Month
    ggplot(crime_month, aes(x = 1:31, y = `CRIME TOTALS`)) +
      geom_line(size = 1.2)+
      geom_point(size = 3) +
      theme(axis.text.x = element_text(size = 10),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10)) +
      labs(title="Crimes Per Day of Month in 2015",
           x= "Day",
           y = "# of Crimes")
  })

  output$plot_week_2015 <- renderPlot({
    
    crime_week <- crime %>% 
      filter(YEAR == 2015) %>% 
      select(DAY_OF_WEEK, HOUR) %>% 
      group_by(DAY_OF_WEEK, HOUR) %>% 
      summarize(`CRIME TOTALS` = n()) %>% 
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    
    # Day of Week
    ggplot(crime_week, aes(x = DAY_OF_WEEK, y = Proportion, fill = HOUR)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient2(low = "#000333", mid = "yellow", midpoint = 12, high = "#000066",
                           labels = 0:23,
                           breaks = 0:23,
                           guide = guide_legend(
                             title.position = "top",
                             title = "Hour",
                             label.position = "right",
                             label.hjust = 0.5,
                             label.vjust = 1,
                             title.vjust = 1,
                             title.hjust = .5,
                             title.theme = element_text(face = "bold", angle = 0),
                             label.theme = element_text(angle = 0)
                           )
      ) +
      labs(title="Distribution of Crimes per Hour in 2015", 
           x= "Day of Week",
           y = "Proportion") + 
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12), 
            axis.title.x = element_text(face = "bold", size = 15),
            axis.title.y = element_text(face = "bold", size = 15)) +
      scale_x_discrete(limits=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  })
  
  output$plot_hour_2015 <- renderPlot({
    
    crime_hour <- crime %>% 
      filter(YEAR == 2015) %>% 
      select(HOUR) %>% 
      group_by(HOUR) %>% 
      summarize(`CRIME TOTALS` = n()) %>% 
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    # Time
    ggplot(crime_hour, aes(x = 1:24, y = `CRIME TOTALS`)) +
      geom_line(size = 1.2)+
      geom_point(size = 3) + 
      theme(axis.text.x = element_text(size = 10),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10)) +
      labs(title="Crimes Per Hour", 
           x= "Hour",
           y = "# of Crimes")
  })
  
  output$plot_loc_2015 <- renderPlot({
    crime_2015 <- crime %>%
      filter(YEAR == 2015)

    # Location
    ggmap(boston_map, extent = "device") +
      geom_density2d(data = crime_2015,
                     aes(x = Long, y = Lat), size = 0.3) +
      stat_density2d(data = crime_2015, aes(x = Long, y = Lat,
                                            fill = ..level.., alpha = ..level..),
                     size = 0.01, bins = 16, geom = "polygon") +
      scale_fill_gradient(low = "green", high = "red",
                          guide = FALSE) +
      scale_alpha(range = c(0, 0.3), guide = FALSE) +
      labs(title="Crime Distribution in 2015")
  })

  
  output$stat_year_2015 <- renderPrint({
    crime_2015 <- crime %>% filter(YEAR == 2015)
    year_2015 <- crime_2015 %>% filter(MONTH != 6) %>% select(MONTH) %>% group_by(MONTH) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Month_in_2015 <- year_2015["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Month_in_2015)
  })
  
  output$table_year_2015 = DT::renderDataTable({
    crime_2015 <- crime %>% filter(YEAR == 2015)
    year_2015 <- crime_2015 %>% filter(MONTH != 6) %>% select(MONTH) %>% group_by(MONTH) %>% summarize(TOTAL = n())
    year_chisq <- chisq.test(year_2015["TOTAL"])
    
    # make table with expected, observed, and residual values
    year_table <- cbind(year_chisq$observed, year_chisq$expected, year_chisq$residuals)
    colnames(year_table) <- c("Obs.","Exp.","Res.")
    rownames(year_table) <- month.name[7:12]
    
    # display table
    datatable(year_table, options = list(
      searching = FALSE,
      pageLength = 7,
      lengthMenu = c(7)
    ))
  })
  
  output$stat_month_2015 <- renderPrint({
    crime_2015 <- crime %>% filter(YEAR == 2015)
    month_2015 <- crime_2015 %>% select(DAY) %>% group_by(DAY) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Day_of_the_Month_in_2015 <- month_2015["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Day_of_the_Month_in_2015)
  })
  
  output$table_month_2015 = DT::renderDataTable({
    crime_2015 <- crime %>% filter(YEAR == 2015)
    month_2015 <- crime_2015 %>% select(DAY) %>% group_by(DAY) %>% summarize(TOTAL = n())
    month_chisq <- chisq.test(month_2015["TOTAL"])
    
    # make table with expected, observed, and residual values
    month_table <- cbind(month_chisq$observed, month_chisq$expected, month_chisq$residuals)
    colnames(month_table) <- c("Obs.","Exp.","Res.")
    rownames(month_table) <- 1:31
    
    # display table
    datatable(month_table, options = list(
      searching = FALSE,
      pageLength = 7,
      lengthMenu = c(7,14,21,31)
    ))
  })
  
  output$stat_week_2015 <- renderPrint({
    crime_2015 <- crime %>% filter(YEAR == 2015)
    week_2015 <- crime_2015 %>% select(DAY_OF_WEEK) %>% group_by(DAY_OF_WEEK) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Day_of_the_Week_in_2015 <- week_2015["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Day_of_the_Week_in_2015)
  })
  
  output$table_week_2015 = DT::renderDataTable({
    crime_2015 <- crime %>% filter(YEAR == 2015)
    week_2015 <- crime_2015 %>% select(DAY_OF_WEEK) %>% group_by(DAY_OF_WEEK) %>% summarize(TOTAL = n())
    week_chisq <- chisq.test(week_2015["TOTAL"])
    
    # make table with expected, observed, and residual values
    week_table <- cbind(week_chisq$observed, week_chisq$expected, week_chisq$residuals)
    colnames(week_table) <- c("Obs.","Exp.","Res.")
    rownames(week_table) <- c("Friday","Monday","Saturday","Sunday","Thursday","Tuesday","Wednesday")
    
    # display table
    datatable(week_table, options = list(
      searching = FALSE,
      pageLength = 7,
      lengthMenu = 0
    ))
  })
  
  output$stat_hour_2015 <- renderPrint({
    crime_2015 <- crime %>% filter(YEAR == 2015)
    hour_2015 <- crime_2015 %>% select(HOUR) %>% group_by(HOUR) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Hour_in_2015 <- hour_2015["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Hour_in_2015)
  })
  
  output$table_hour_2015 = DT::renderDataTable({
    crime_2015 <- crime %>% filter(YEAR == 2015)
    hour_2015 <- crime_2015 %>% select(HOUR) %>% group_by(HOUR) %>% summarize(TOTAL = n())
    hour_chisq <- chisq.test(hour_2015["TOTAL"])
    
    # make table with expected, observed, and residual values
    hour_table <- cbind(hour_chisq$observed, hour_chisq$expected, hour_chisq$residuals)
    colnames(hour_table) <- c("Obs.","Exp.","Res.")
    rownames(hour_table) <- 0:23
    
    # display table
    datatable(hour_table, options = list(
      searching = FALSE,
      pageLength = 6,
      lengthMenu = c(6,12,18,24)
    ))
  })
  
  output$plot_type_2016 <- renderPlot({
    cur_crime <- crime %>% 
      filter(YEAR == 2016)
    
    # Crime Type
    ggplot(cur_crime, aes(x=OFFENSE_CODE_GROUP)) + 
      geom_bar( stat="count", color = "black", fill = "#C21460") +
      theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.1),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(face = "bold", hjust = -1)) +
      labs(title="Crimes in 2016", 
           x="Crime Type",
           y = "# of Times")
  })
  
  output$plot_year_2016 <- renderPlot({
    
    crimes_year <- crime %>% 
      filter(YEAR == 2016) %>% 
      select(MONTH) %>% 
      group_by(MONTH) %>% 
      summarize(`CRIME TOTALS` = n()) %>% 
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    # Year
    ggplot(crimes_year, aes(x = 1:12, y = `CRIME TOTALS`)) +
      geom_line(size = 1.2)+
      geom_point(size = 5, colour = month_colors[1:12]) + 
      theme(axis.text.x = element_text(size = 10),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10)) +
      labs(title="Crimes in 2016", 
           x= "Month",
           y = "# of Crimes") + 
      scale_x_continuous(breaks = 1:12,
                         labels= month.name)
  })
  
  output$plot_month_2016 <- renderPlot({
    
    crime_month <- crime %>%
      filter(YEAR == 2016) %>% 
      select(DAY) %>%
      group_by(DAY) %>%
      summarize(`CRIME TOTALS` = n()) %>%
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    # Day of Month
    ggplot(crime_month, aes(x = 1:31, y = `CRIME TOTALS`)) +
      geom_line(size = 1.2)+
      geom_point(size = 3) +
      theme(axis.text.x = element_text(size = 10),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10)) +
      labs(title="Crimes Per Day of Month in 2016",
           x= "Day",
           y = "# of Crimes")
  })
  
  output$plot_week_2016 <- renderPlot({
    
    crime_week <- crime %>% 
      filter(YEAR == 2016) %>% 
      select(DAY_OF_WEEK, HOUR) %>% 
      group_by(DAY_OF_WEEK, HOUR) %>% 
      summarize(`CRIME TOTALS` = n()) %>% 
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    
    # Day of Week
    ggplot(crime_week, aes(x = DAY_OF_WEEK, y = Proportion, fill = HOUR)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient2(low = "#000333", mid = "yellow", midpoint = 12, high = "#000066",
                           labels = 0:23,
                           breaks = 0:23,
                           guide = guide_legend(
                             title.position = "top",
                             title = "Hour",
                             label.position = "right",
                             label.hjust = 0.5,
                             label.vjust = 1,
                             title.vjust = 1,
                             title.hjust = .5,
                             title.theme = element_text(face = "bold", angle = 0),
                             label.theme = element_text(angle = 0)
                           )
      ) +
      labs(title="Distribution of Crimes per Hour in 2016", 
           x= "Day of Week",
           y = "Proportion") + 
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12), 
            axis.title.x = element_text(face = "bold", size = 15),
            axis.title.y = element_text(face = "bold", size = 15)) +
      scale_x_discrete(limits=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  })
  
  output$plot_hour_2016 <- renderPlot({
    
    crime_hour <- crime %>% 
      filter(YEAR == 2016) %>% 
      select(HOUR) %>% 
      group_by(HOUR) %>% 
      summarize(`CRIME TOTALS` = n()) %>% 
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    # Time
    ggplot(crime_hour, aes(x = 1:24, y = `CRIME TOTALS`)) +
      geom_line(size = 1.2)+
      geom_point(size = 3) + 
      theme(axis.text.x = element_text(size = 10),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10)) +
      labs(title="Crimes Per Hour", 
           x= "Hour",
           y = "# of Crimes")
  })
  
  output$plot_loc_2016 <- renderPlot({
    crime_2016 <- crime %>%
      filter(YEAR == 2016)

    # Location
    ggmap(boston_map, extent = "device") +
      geom_density2d(data = crime_2016,
                     aes(x = Long, y = Lat), size = 0.3) +
      stat_density2d(data = crime_2016, aes(x = Long, y = Lat,
                                            fill = ..level.., alpha = ..level..),
                     size = 0.01, bins = 16, geom = "polygon") +
      scale_fill_gradient(low = "green", high = "red",
                          guide = FALSE) +
      scale_alpha(range = c(0, 0.3), guide = FALSE) +
      labs(title="Crime Distribution in 2016")
  })

  
  output$stat_year_2016 <- renderPrint({
    crime_2016 <- crime %>% filter(YEAR == 2016)
    year_2016 <- crime_2016 %>% select(MONTH) %>% group_by(MONTH) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Month_in_2016 <- year_2016["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Month_in_2016)
  })
  
  output$table_year_2016 = DT::renderDataTable({
    crime_2016 <- crime %>% filter(YEAR == 2016)
    year_2016 <- crime_2016 %>% select(MONTH) %>% group_by(MONTH) %>% summarize(TOTAL = n())
    year_chisq <- chisq.test(year_2016["TOTAL"])
    
    # make table with expected, observed, and residual values
    year_table <- cbind(year_chisq$observed, year_chisq$expected, year_chisq$residuals)
    colnames(year_table) <- c("Obs.","Exp.","Res.")
    rownames(year_table) <- month.name[1:12]
    
    # display table
    datatable(year_table, options = list(
      searching = FALSE,
      pageLength = 4,
      lengthMenu = c(4,8,12)
    ))
  })
  
  output$stat_month_2016 <- renderPrint({
    crime_2016 <- crime %>% filter(YEAR == 2016)
    month_2016 <- crime_2016 %>% select(DAY) %>% group_by(DAY) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Day_of_the_Month_in_2016 <- month_2016["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Day_of_the_Month_in_2016)
  })
  
  output$table_month_2016 = DT::renderDataTable({
    crime_2016 <- crime %>% filter(YEAR == 2016)
    month_2016 <- crime_2016 %>% select(DAY) %>% group_by(DAY) %>% summarize(TOTAL = n())
    month_chisq <- chisq.test(month_2016["TOTAL"])
    
    # make table with expected, observed, and residual values
    month_table <- cbind(month_chisq$observed, month_chisq$expected, month_chisq$residuals)
    colnames(month_table) <- c("Obs.","Exp.","Res.")
    rownames(month_table) <- 1:31
    
    # display table
    datatable(month_table, options = list(
      searching = FALSE,
      pageLength = 7,
      lengthMenu = c(7,14,21,31)
    ))
  })
  
  output$stat_week_2016 <- renderPrint({
    crime_2016 <- crime %>% filter(YEAR == 2016)
    week_2016 <- crime_2016 %>% select(DAY_OF_WEEK) %>% group_by(DAY_OF_WEEK) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Day_of_the_Week_in_2016 <- week_2016["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Day_of_the_Week_in_2016)
  })
  
  output$table_week_2016 = DT::renderDataTable({
    crime_2016 <- crime %>% filter(YEAR == 2016)
    week_2016 <- crime_2016 %>% select(DAY_OF_WEEK) %>% group_by(DAY_OF_WEEK) %>% summarize(TOTAL = n())
    week_chisq <- chisq.test(week_2016["TOTAL"])
    
    # make table with expected, observed, and residual values
    week_table <- cbind(week_chisq$observed, week_chisq$expected, week_chisq$residuals)
    colnames(week_table) <- c("Obs.","Exp.","Res.")
    rownames(week_table) <- c("Friday","Monday","Saturday","Sunday","Thursday","Tuesday","Wednesday")
    
    # display table
    datatable(week_table, options = list(
      searching = FALSE,
      pageLength = 7,
      lengthMenu = 0
    ))
  })
  
  output$stat_hour_2016 <- renderPrint({
    crime_2016 <- crime %>% filter(YEAR == 2016)
    hour_2016 <- crime_2016 %>% select(HOUR) %>% group_by(HOUR) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Hour_in_2016 <- hour_2016["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Hour_in_2016)
  })
  
  output$table_hour_2016 = DT::renderDataTable({
    crime_2016 <- crime %>% filter(YEAR == 2016)
    hour_2016 <- crime_2016 %>% select(HOUR) %>% group_by(HOUR) %>% summarize(TOTAL = n())
    hour_chisq <- chisq.test(hour_2016["TOTAL"])
    
    # make table with expected, observed, and residual values
    hour_table <- cbind(hour_chisq$observed, hour_chisq$expected, hour_chisq$residuals)
    colnames(hour_table) <- c("Obs.","Exp.","Res.")
    rownames(hour_table) <- 0:23
    
    # display table
    datatable(hour_table, options = list(
      searching = FALSE,
      pageLength = 6,
      lengthMenu = c(6,12,18,24)
    ))
  })
  
  output$plot_type_2017 <- renderPlot({
    cur_crime <- crime %>% 
      filter(YEAR == 2017)
    
    # Crime Type
    ggplot(cur_crime, aes(x=OFFENSE_CODE_GROUP)) + 
      geom_bar( stat="count", color = "black", fill = "#C21460") +
      theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.1),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(face = "bold", hjust = -1)) +
      labs(title="Crimes in 2017", 
           x="Crime Type",
           y = "# of Times")
  })
  
  output$plot_year_2017 <- renderPlot({
    
    crimes_year <- crime %>% 
      filter(YEAR == 2017) %>% 
      select(MONTH) %>% 
      group_by(MONTH) %>% 
      summarize(`CRIME TOTALS` = n()) %>% 
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    # Year
    ggplot(crimes_year, aes(x = 1:12, y = `CRIME TOTALS`)) +
      geom_line(size = 1.2)+
      geom_point(size = 5, colour = month_colors[1:12]) + 
      theme(axis.text.x = element_text(size = 10),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10)) +
      labs(title="Crimes in 2017", 
           x= "Month",
           y = "# of Crimes") + 
      scale_x_continuous(breaks = 1:12,
                         labels= month.name)
  })
  
  output$plot_month_2017 <- renderPlot({
    
    crime_month <- crime %>%
      filter(YEAR == 2017) %>% 
      select(DAY) %>%
      group_by(DAY) %>%
      summarize(`CRIME TOTALS` = n()) %>%
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    # Day of Month
    ggplot(crime_month, aes(x = 1:31, y = `CRIME TOTALS`)) +
      geom_line(size = 1.2)+
      geom_point(size = 3) +
      theme(axis.text.x = element_text(size = 10),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10)) +
      labs(title="Crimes Per Day of Month in 2017",
           x= "Day",
           y = "# of Crimes")
  })
  
  output$plot_week_2017 <- renderPlot({
    
    crime_week <- crime %>% 
      filter(YEAR == 2017) %>% 
      select(DAY_OF_WEEK, HOUR) %>% 
      group_by(DAY_OF_WEEK, HOUR) %>% 
      summarize(`CRIME TOTALS` = n()) %>% 
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    
    # Day of Week
    ggplot(crime_week, aes(x = DAY_OF_WEEK, y = Proportion, fill = HOUR)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient2(low = "#000333", mid = "yellow", midpoint = 12, high = "#000066",
                           labels = 0:23,
                           breaks = 0:23,
                           guide = guide_legend(
                             title.position = "top",
                             title = "Hour",
                             label.position = "right",
                             label.hjust = 0.5,
                             label.vjust = 1,
                             title.vjust = 1,
                             title.hjust = .5,
                             title.theme = element_text(face = "bold", angle = 0),
                             label.theme = element_text(angle = 0)
                           )
      ) +
      labs(title="Distribution of Crimes per Hour in 2017", 
           x= "Day of Week",
           y = "Proportion") + 
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12), 
            axis.title.x = element_text(face = "bold", size = 15),
            axis.title.y = element_text(face = "bold", size = 15)) +
      scale_x_discrete(limits=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  })
  
  output$plot_hour_2017 <- renderPlot({
    
    crime_hour <- crime %>% 
      filter(YEAR == 2017) %>% 
      select(HOUR) %>% 
      group_by(HOUR) %>% 
      summarize(`CRIME TOTALS` = n()) %>% 
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    # Time
    ggplot(crime_hour, aes(x = 1:24, y = `CRIME TOTALS`)) +
      geom_line(size = 1.2)+
      geom_point(size = 3) + 
      theme(axis.text.x = element_text(size = 10),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10)) +
      labs(title="Crimes Per Hour", 
           x= "Hour",
           y = "# of Crimes")
  })
  
  output$plot_loc_2017 <- renderPlot({
    crime_2017 <- crime %>%
      filter(YEAR == 2017)

    # Location
    ggmap(boston_map, extent = "device") +
      geom_density2d(data = crime_2017,
                     aes(x = Long, y = Lat), size = 0.3) +
      stat_density2d(data = crime_2017, aes(x = Long, y = Lat,
                                            fill = ..level.., alpha = ..level..),
                     size = 0.01, bins = 16, geom = "polygon") +
      scale_fill_gradient(low = "green", high = "red",
                          guide = FALSE) +
      scale_alpha(range = c(0, 0.3), guide = FALSE) +
      labs(title="Crime Distribution in 2017")
  })

  
  output$stat_year_2017 <- renderPrint({
    crime_2017 <- crime %>% filter(YEAR == 2017)
    year_2017 <- crime_2017 %>% select(MONTH) %>% group_by(MONTH) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Month_in_2017 <- year_2017["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Month_in_2017)
  })
  
  output$table_year_2017 = DT::renderDataTable({
    crime_2017 <- crime %>% filter(YEAR == 2017)
    year_2017 <- crime_2017 %>% select(MONTH) %>% group_by(MONTH) %>% summarize(TOTAL = n())
    year_chisq <- chisq.test(year_2017["TOTAL"])
    
    # make table with expected, observed, and residual values
    year_table <- cbind(year_chisq$observed, year_chisq$expected, year_chisq$residuals)
    colnames(year_table) <- c("Obs.","Exp.","Res.")
    rownames(year_table) <- month.name[1:12]
    
    # display table
    datatable(year_table, options = list(
      searching = FALSE,
      pageLength = 4,
      lengthMenu = c(4,8,12)
    ))
  })
  
  output$stat_month_2017 <- renderPrint({
    crime_2017 <- crime %>% filter(YEAR == 2017)
    month_2017 <- crime_2017 %>% select(DAY) %>% group_by(DAY) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Day_of_the_Month_in_2017 <- month_2017["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Day_of_the_Month_in_2017)
  })
  
  output$table_month_2017 = DT::renderDataTable({
    crime_2017 <- crime %>% filter(YEAR == 2017)
    month_2017 <- crime_2017 %>% select(DAY) %>% group_by(DAY) %>% summarize(TOTAL = n())
    month_chisq <- chisq.test(month_2017["TOTAL"])
    
    # make table with expected, observed, and residual values
    month_table <- cbind(month_chisq$observed, month_chisq$expected, month_chisq$residuals)
    colnames(month_table) <- c("Obs.","Exp.","Res.")
    rownames(month_table) <- 1:31
    
    # display table
    datatable(month_table, options = list(
      searching = FALSE,
      pageLength = 7,
      lengthMenu = c(7,14,21,31)
    ))
  })
  
  output$stat_week_2017 <- renderPrint({
    crime_2017 <- crime %>% filter(YEAR == 2017)
    week_2017 <- crime_2017 %>% select(DAY_OF_WEEK) %>% group_by(DAY_OF_WEEK) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Day_of_the_Week_in_2017 <- week_2017["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Day_of_the_Week_in_2017)
  })
  
  output$table_week_2017 = DT::renderDataTable({
    crime_2017 <- crime %>% filter(YEAR == 2017)
    week_2017 <- crime_2017 %>% select(DAY_OF_WEEK) %>% group_by(DAY_OF_WEEK) %>% summarize(TOTAL = n())
    week_chisq <- chisq.test(week_2017["TOTAL"])
    
    # make table with expected, observed, and residual values
    week_table <- cbind(week_chisq$observed, week_chisq$expected, week_chisq$residuals)
    colnames(week_table) <- c("Obs.","Exp.","Res.")
    rownames(week_table) <- c("Friday","Monday","Saturday","Sunday","Thursday","Tuesday","Wednesday")
    
    # display table
    datatable(week_table, options = list(
      searching = FALSE,
      pageLength = 7,
      lengthMenu = 0
    ))
  })
  
  output$stat_hour_2017 <- renderPrint({
    crime_2017 <- crime %>% filter(YEAR == 2017)
    hour_2017 <- crime_2017 %>% select(HOUR) %>% group_by(HOUR) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Hour_in_2017 <- hour_2017["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Hour_in_2017)
  })
  
  output$table_hour_2017 = DT::renderDataTable({
    crime_2017 <- crime %>% filter(YEAR == 2017)
    hour_2017 <- crime_2017 %>% select(HOUR) %>% group_by(HOUR) %>% summarize(TOTAL = n())
    hour_chisq <- chisq.test(hour_2017["TOTAL"])
    
    # make table with expected, observed, and residual values
    hour_table <- cbind(hour_chisq$observed, hour_chisq$expected, hour_chisq$residuals)
    colnames(hour_table) <- c("Obs.","Exp.","Res.")
    rownames(hour_table) <- 0:23
    
    # display table
    datatable(hour_table, options = list(
      searching = FALSE,
      pageLength = 6,
      lengthMenu = c(6,12,18,24)
    ))
  })
  
  output$plot_type_2018 <- renderPlot({
    cur_crime <- crime %>% 
      filter(YEAR == 2018)
    
    # Crime Type
    ggplot(cur_crime, aes(x=OFFENSE_CODE_GROUP)) + 
      geom_bar( stat="count", color = "black", fill = "#C21460") +
      theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.1),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10),
            legend.title = element_text(face = "bold", hjust = -1)) +
      labs(title="Crimes in 2018", 
           x="Crime Type",
           y = "# of Times")
  })
  
  output$plot_year_2018 <- renderPlot({
    
    crimes_year <- crime %>% 
      filter(YEAR == 2018) %>% 
      select(MONTH) %>% 
      group_by(MONTH) %>% 
      summarize(`CRIME TOTALS` = n()) %>% 
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    # Year
    ggplot(crimes_year, aes(x = 1:5, y = `CRIME TOTALS`)) +
      geom_line(size = 1.2)+
      geom_point(size = 5, colour = month_colors[1:5]) + 
      theme(axis.text.x = element_text(size = 10),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10)) +
      labs(title="Crimes in 2018", 
           x= "Month",
           y = "# of Crimes") + 
      scale_x_continuous(breaks = 1:5,
                         labels= month.name[1:5])
  })
  
  output$plot_month_2018 <- renderPlot({
    
    crime_month <- crime %>%
      filter(YEAR == 2018) %>% 
      select(DAY) %>%
      group_by(DAY) %>%
      summarize(`CRIME TOTALS` = n()) %>%
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    # Day of Month
    ggplot(crime_month, aes(x = 1:31, y = `CRIME TOTALS`)) +
      geom_line(size = 1.2)+
      geom_point(size = 3) +
      theme(axis.text.x = element_text(size = 10),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10)) +
      labs(title="Crimes Per Day of Month in 2018",
           x= "Day",
           y = "# of Crimes")
  })
  
  output$plot_week_2018 <- renderPlot({
    
    crime_week <- crime %>% 
      filter(YEAR == 2018) %>% 
      select(DAY_OF_WEEK, HOUR) %>% 
      group_by(DAY_OF_WEEK, HOUR) %>% 
      summarize(`CRIME TOTALS` = n()) %>% 
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    
    # Day of Week
    ggplot(crime_week, aes(x = DAY_OF_WEEK, y = Proportion, fill = HOUR)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient2(low = "#000333", mid = "yellow", midpoint = 12, high = "#000066",
                           labels = 0:23,
                           breaks = 0:23,
                           guide = guide_legend(
                             title.position = "top",
                             title = "Hour",
                             label.position = "right",
                             label.hjust = 0.5,
                             label.vjust = 1,
                             title.vjust = 1,
                             title.hjust = .5,
                             title.theme = element_text(face = "bold", angle = 0),
                             label.theme = element_text(angle = 0)
                           )
      ) +
      labs(title="Distribution of Crimes per Hour in 2018", 
           x= "Day of Week",
           y = "Proportion") + 
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12), 
            axis.title.x = element_text(face = "bold", size = 15),
            axis.title.y = element_text(face = "bold", size = 15)) +
      scale_x_discrete(limits=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  })
  
  output$plot_hour_2018 <- renderPlot({
    
    crime_hour <- crime %>% 
      filter(YEAR == 2018) %>% 
      select(HOUR) %>% 
      group_by(HOUR) %>% 
      summarize(`CRIME TOTALS` = n()) %>% 
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    # Time
    ggplot(crime_hour, aes(x = 1:24, y = `CRIME TOTALS`)) +
      geom_line(size = 1.2)+
      geom_point(size = 3) + 
      theme(axis.text.x = element_text(size = 10),
            axis.title.x = element_text(face = "bold"),
            axis.title.y = element_text(face = "bold"),
            legend.text = element_text(size = 10)) +
      labs(title="Crimes Per Hour", 
           x= "Hour",
           y = "# of Crimes")
  })
  
  output$plot_loc_2018 <- renderPlot({
    crime_2018 <- crime %>%
      filter(YEAR == 2018)

    # Location
    ggmap(boston_map, extent = "device") +
      geom_density2d(data = crime_2018,
                     aes(x = Long, y = Lat), size = 0.3) +
      stat_density2d(data = crime_2018, aes(x = Long, y = Lat,
                                            fill = ..level.., alpha = ..level..),
                     size = 0.01, bins = 16, geom = "polygon") +
      scale_fill_gradient(low = "green", high = "red",
                          guide = FALSE) +
      scale_alpha(range = c(0, 0.3), guide = FALSE) +
      labs(title="Crime Distribution in 2018")
  })

  
  output$stat_year_2018 <- renderPrint({
    crime_2018 <- crime %>% filter(YEAR == 2018)
    year_2018 <- crime_2018 %>% filter(MONTH != 5) %>% select(MONTH) %>% group_by(MONTH) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Month_in_2018 <- year_2018["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Month_in_2018)
  })
  
  output$table_year_2018 = DT::renderDataTable({
    crime_2018 <- crime %>% filter(YEAR == 2018)
    year_2018 <- crime_2018 %>% filter(MONTH != 5) %>% select(MONTH) %>% group_by(MONTH) %>% summarize(TOTAL = n())
    year_chisq <- chisq.test(year_2018["TOTAL"])
    
    # make table with expected, observed, and residual values
    year_table <- cbind(year_chisq$observed, year_chisq$expected, year_chisq$residuals)
    colnames(year_table) <- c("Obs.","Exp.","Res.")
    rownames(year_table) <- month.name[1:4]
    
    # display table
    datatable(year_table, options = list(
      searching = FALSE,
      pageLength = 4,
      lengthMenu = 0
    ))
  })
  
  output$stat_month_2018 <- renderPrint({
    crime_2018 <- crime %>% filter(YEAR == 2018)
    month_2018 <- crime_2018 %>% select(DAY) %>% group_by(DAY) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Day_of_the_Month_in_2018 <- month_2018["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Day_of_the_Month_in_2018)
  })
  
  output$table_month_2018 = DT::renderDataTable({
    crime_2018 <- crime %>% filter(YEAR == 2018)
    month_2018 <- crime_2018 %>% select(DAY) %>% group_by(DAY) %>% summarize(TOTAL = n())
    month_chisq <- chisq.test(month_2018["TOTAL"])
    
    # make table with expected, observed, and residual values
    month_table <- cbind(month_chisq$observed, month_chisq$expected, month_chisq$residuals)
    colnames(month_table) <- c("Obs.","Exp.","Res.")
    rownames(month_table) <- 1:31
    
    # display table
    datatable(month_table, options = list(
      searching = FALSE,
      pageLength = 7,
      lengthMenu = c(7,14,21,31)
    ))
  })
  
  output$stat_week_2018 <- renderPrint({
    crime_2018 <- crime %>% filter(YEAR == 2018)
    week_2018 <- crime_2018 %>% select(DAY_OF_WEEK) %>% group_by(DAY_OF_WEEK) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Day_of_the_Week_in_2018 <- week_2018["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Day_of_the_Week_in_2018)
  })
  
  output$table_week_2018 = DT::renderDataTable({
    crime_2018 <- crime %>% filter(YEAR == 2018)
    week_2018 <- crime_2018 %>% select(DAY_OF_WEEK) %>% group_by(DAY_OF_WEEK) %>% summarize(TOTAL = n())
    week_chisq <- chisq.test(week_2018["TOTAL"])
    
    # make table with expected, observed, and residual values
    week_table <- cbind(week_chisq$observed, week_chisq$expected, week_chisq$residuals)
    colnames(week_table) <- c("Obs.","Exp.","Res.")
    rownames(week_table) <- c("Friday","Monday","Saturday","Sunday","Thursday","Tuesday","Wednesday")
    
    # display table
    datatable(week_table, options = list(
      searching = FALSE,
      pageLength = 7,
      lengthMenu = 0
    ))
  })
  
  output$stat_hour_2018 <- renderPrint({
    crime_2018 <- crime %>% filter(YEAR == 2018)
    hour_2018 <- crime_2018 %>% select(HOUR) %>% group_by(HOUR) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Hour_in_2018 <- hour_2018["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Hour_in_2018)
  })
  
  output$table_hour_2018 = DT::renderDataTable({
    crime_2018 <- crime %>% filter(YEAR == 2018)
    hour_2018 <- crime_2018 %>% select(HOUR) %>% group_by(HOUR) %>% summarize(TOTAL = n())
    hour_chisq <- chisq.test(hour_2018["TOTAL"])
    
    # make table with expected, observed, and residual values
    hour_table <- cbind(hour_chisq$observed, hour_chisq$expected, hour_chisq$residuals)
    colnames(hour_table) <- c("Obs.","Exp.","Res.")
    rownames(hour_table) <- 0:23
    
    # display table
    datatable(hour_table, options = list(
      searching = FALSE,
      pageLength = 6,
      lengthMenu = c(6,12,18,24)
    ))
  })
  
  output$plot_cum_year <- renderPlot({
    crimes_per_month_year <- crime %>% 
      select(MONTH, YEAR) %>% 
      group_by(YEAR, MONTH) %>% 
      summarize(`CRIME TOTALS` = n()) %>% 
      mutate(Proportion = `CRIME TOTALS` / sum(`CRIME TOTALS`))
    
    ggplot(crimes_per_month_year, aes(x = YEAR, y = Proportion, fill = factor(MONTH))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = month_colors[1:12],
                        labels = month.name,
                        guide = guide_legend(
                          title.position = "top",
                          title = "Month",
                          label.position = "right",
                          label.hjust = 0.5,
                          label.vjust = 1,
                          title.vjust = 1,
                          title.hjust = .5,
                          title.theme = element_text(face = "bold", angle = 0),
                          label.theme = element_text(angle = 0)
                        )
      ) +
      labs(title="Distribution of Crimes per Month", 
           x= "Year",
           y = "Proportion") + 
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12), 
            axis.title.x = element_text(face = "bold", size = 15),
            axis.title.y = element_text(face = "bold", size = 15))
  })
  
  output$stat_cumulative <- renderPrint({
    crime_cum <- crime %>% filter(YEAR == 2016 | YEAR == 2017)
    crime_tot <- crime_cum %>% select(YEAR) %>% group_by(YEAR) %>% summarize(TOTAL = n())
    Number_of_Crimes_Committed_Each_Year <- crime_tot["TOTAL"]
    chisq.test(Number_of_Crimes_Committed_Each_Year)
  })
  
  output$table_cumulative = DT::renderDataTable({
    crime_cum <- crime %>% filter(YEAR == 2016 | YEAR == 2017)
    crime_tot <- crime_cum %>% select(YEAR) %>% group_by(YEAR) %>% summarize(TOTAL = n())
    crime_chisq <- chisq.test(crime_tot["TOTAL"])
    
    # make table with expected, observed, and residual values
    crime_table <- cbind(crime_chisq$observed, crime_chisq$expected, crime_chisq$residuals)
    colnames(crime_table) <- c("Obs.","Exp.","Res.")
    rownames(crime_table) <- 2016:2017
    
    # display table
    datatable(crime_table, options = list(
      searching = FALSE,
      pageLength = 2,
      lengthMenu = 0
    ))
  })
  
}

shinyApp(ui, server)
