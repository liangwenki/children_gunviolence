library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(leaflet)
library(DT)
library(plotly)
library(reshape2)
library(shinycssloaders)
library(lubridate)
library(wesanderson)
library(RColorBrewer)
# options(spinner.color="#006272",spinner.color.background="#ffffff")

# Reading data
data_c = read_csv("Children_data.csv")
data_c$Incident_Date <-
  as.Date(data_c$Incident_Date, format = "%m/%d/%y")
data_s = read_csv("School_data.csv")
data_s$Incident_Date <-
  as.Date(data_s$Incident_Date, format = "%m/%d/%y")
data_class_c = read_csv("Children_class_data.csv")
data_class_s = read_csv("School_class_data.csv")
#Prepare Intro Data
data_intents = read_csv("Gun deaths by intents.csv")
data_intents$Year <- as.integer(data_intents$Year)
data_intents$Intents <-
  factor(
    data_intents$Intents,
    levels = c(
      "Homicide",
      "Suicide",
      "Unintentional",
      "Undetermined",
      "Legal Intervention"
    )
  )
data_causes = read_csv("Underlying Cause of Death.csv")

# Prepare plot data
data_s$year <- as.integer(format(data_s$Incident_Date, "%Y"))
line1 = data_s %>%
  group_by(year) %>%
  summarise(N = n())
data_c$year <- as.integer(format(data_c$Incident_Date, "%Y"))
line2 = data_c %>%
  group_by(year) %>%
  summarise(N = n())
line_join = left_join(line1, line2, by = "year") %>%
  rename(
    "Year" = "year",
    "School Incidents" = "N.x",
    "Child Involved Incidents" = "N.y"
  )
row_join = melt(line_join, id.vars = c("Year")) %>%
  rename("Category" = "variable", "Incidents" = "value")
# Trend by Type Data Prep
child_shoot_class <- data_class_c %>% select(2, 8, 9, 11, 12)
colnames(child_shoot_class) <-
  c("Incident_Date",
    "Killed",
    "Injured",
    "label_col",
    "Classification")
child_shoot_class$Incident_Date <-
  dmy(child_shoot_class$Incident_Date)
child_shoot_class$year <-
  as.integer(format(child_shoot_class$Incident_Date, "%Y"))
child_shoot_class$month <-
  as.integer(format(child_shoot_class$Incident_Date, "%m"))
child_shoot_class$Killed_or_Injured <-
  recode(child_shoot_class$Killed, `1` = "Killed", `0` = "Injured")

#ui
ui <- fluidPage(theme = "style.css",
                shinyUI(
                  dashboardPage(
                    skin = "red",
                    dashboardHeader(title = "Child Involved Gun Violence",
                                    titleWidth = 300),
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(
                        menuItem("Introduction", tabName = "Intro", icon = icon("info")),
                        menuItem(
                          "Incidents Map",
                          tabName = "Map",
                          icon = icon("map-pin")
                        ),
                        menuItem(
                          "Incidents Analysis",
                          tabName = "Ana",
                          icon = icon("desktop")
                        ),
                        menuItem(
                          "Gun Violence Tracking",
                          tabName = "Trend",
                          icon = icon("fas fa-chart-line")
                        ),
                        menuItem(
                          "About Us & Data",
                          tabName = "About",
                          icon = icon("pushpin", lib = "glyphicon")
                        )
                      )
                    ),
                    
                    dashboardBody(tabItems(
                      tabItem(tabName = "Intro",
                              fluidRow(column(
                                width = 12,
                                box(
                                  title = tags$h3("Introduction"),
                                  solidHeader = T,
                                  width = NULL,
                                  #status = "primary",
                                  id = "intro",
                                  tags$h5(tags$strong("Project Description")),
                                  tags$h5(
                                    "The combination of widespread gun ownership and lax gun safety rules creates a very perilous environment for youngsters in the United States. The consequences of this gun violence epidemic are catastrophic. Over 8,000 adolescents are murdered or badly injured by weapons in the United States each year. In the meanwhile, survivors and their loved ones are dealing with loss, anguish, and dread. The consequences of this gun violence epidemic are catastrophic. Over 8,000 adolescents are murdered or badly injured by weapons in the United States each year. In the meanwhile, survivors and their loved ones are dealing with loss, anguish, and dread."
                                  ),
                                  tags$h5(
                                    "This visual dashboard gives insights into plausible explanations and patterns associated to children safety, with the goal of providing viable analysis and solutions for dealing with gun violence involving children."
                                  ),
                                  tags$h5(tags$strong("Research Questions")),
                                  tags$h5(
                                    "Where and when did Children-involved incidents and School incidents happen between 2014-2021?"
                                  ),
                                  tags$h5(
                                    "Are the shootings related to macro factors (Unemployment rate, education rate, GDP, Personal Income) in each state?"
                                  ),
                                  tags$h5(
                                    "What is the trend of these incidents and how can we improve the current situation?"
                                  )
                                )
                              )),
                              fluidRow(column(
                                width = 12,
                                box(
                                  title = h3("The Facts", align = "center"),
                                  solidHeader = T,
                                  width = NULL,
                                  id = 'facts',
                                  fluidRow(column(
                                    width = 12,
                                    box(
                                      title = tags$h4("Guns Are the Leading Cause of Death for Children under 18 in 2020"),
                                      solidHeader = F,
                                      width = NULL,
                                      plotlyOutput("leading_cause")
                                    )
                                  )),
                                  fluidRow(column(
                                    width = 12,
                                    box(
                                      title = "Gun Deaths of Children Are Rising in the USA",
                                      solidHeader = F,
                                      width = NULL,
                                      plotlyOutput("gun_deaths")
                                    )
                                  ))
                                )
                              ))),
                      
                      tabItem(tabName = "Map",
                              fluidRow(
                                column(
                                  width = 6,
                                  box(
                                    title = "Select to Plot",
                                    solidHeader = F,
                                    width = NULL,
                                    selectInput(
                                      "select",
                                      label = NULL,
                                      choices = list(
                                        "Child Involved Incidents" = 1,
                                        "School Incidents" = 2
                                      )
                                    )
                                  )
                                ),
                                column(
                                  width = 6,
                                  box(
                                    title = "Select Time",
                                    solidHeader = F,
                                    collapsible = T,
                                    collapsed = FALSE,
                                    width = NULL,
                                    sliderInput(
                                      "year",
                                      "Year:",
                                      min = 2014,
                                      max = 2021,
                                      value = c(2020, 2021),
                                      animate = animationOptions(interval = 2000, loop = FALSE)
                                    )
                                  )
                                )
                              ),
                              fluidRow(column(
                                12,
                                box(
                                  title = "Map",
                                  solidHeader = F,
                                  #status = "info",
                                  leafletOutput("map", height = 800),
                                  width = NULL,
                                  height = "auto"
                                )
                              ))),
                      
                      tabItem(tabName = "Ana",
                              fluidRow(column(
                                6,
                                box(
                                  title = "Select Time",
                                  solidHeader = F,
                                  collapsible = T,
                                  collapsed = FALSE,
                                  width = NULL,
                                  selectInput(
                                    "select2",
                                    label = NULL,
                                    choices = line_join$Year,
                                    selected = 2021
                                  )
                                )
                              )),
                              fluidRow(column(
                                12,
                                box(
                                  title = "Map",
                                  solidHeader = F,
                                  #status = "info",
                                  leafletOutput("map2", height = 800),
                                  width = NULL,
                                  height = "auto"
                                )
                              ))),
                      
                      tabItem(tabName = "Trend",
                              fluidRow(
                                column(
                                  width = 12,
                                  box(
                                    title = "Gun Violence Tracking",
                                    solidHeader = F,
                                    width = NULL,
                                    height = 500,
                                    plotlyOutput("trend")
                                  )
                                ),
                                fluidRow(column(
                                  width = 6,
                                  box(
                                    title = "Trend by Type",
                                    solidHeader = F,
                                    width = NULL,
                                    selectInput(
                                      "select_class",
                                      label = "Select Type",
                                      selected = c("Children injured", "Children killed"),
                                      choices = unique(child_shoot_class$Classification),
                                      multiple = TRUE
                                    ),
                                    plotlyOutput("trend_by_type")
                                  )
                                  
                                ),
                                
                                column(
                                  width = 6,
                                  box(
                                    title = "Trend by Month",
                                    solidHeader = F,
                                    width = NULL,
                                    selectInput(
                                      "select_month",
                                      label = "Select Month",
                                      selected = unique(child_shoot_class$month),
                                      choices = unique(child_shoot_class$month),
                                      multiple = TRUE
                                    ),
                                    plotlyOutput("trend_by_month")
                                  )
                                ))
                              )),
                      
                      
                      tabItem(tabName = "About",
                              fluidRow (column(
                                width = 12,
                                box(
                                  title = "Team Members",
                                  solidHeader = T,
                                  width = NULL,
                                  
                                  userBox(
                                    title = userDescription(
                                      title = "Wenqi Liang",
                                      subtitle = "Developer",
                                      type = 2,
                                      image = "1.jpeg"
                                      #backgroundImage = "https://www.freecodecamp.org/news/content/images/size/w2000/2021/06/w-qjCHPZbeXCQ-unsplash.jpg"
                                    ),
                                    # status = "maroon",
                                    p(
                                      tags$h5(
                                        "Master of Science in Business Analytics and Risk Management",
                                        br(),
                                        "Carey Business School, Johns Hopkins University 	"
                                      )
                                      
                                    )
                                  ),
                                  
                                  userBox(
                                    title = userDescription(
                                      title = "Xiaowen Fan",
                                      subtitle = "Developer",
                                      type = 2,
                                      image = "2.png"
                                      #   backgroundImage = "https://www.freecodecamp.org/news/content/images/size/w2000/2021/06/w-qjCHPZbeXCQ-unsplash.jpg"
                                    ),
                                    # status = "warning",
                                    p(
                                      tags$h5(
                                        "Master of Science in Business Analytics and Risk Management",
                                        br(),
                                        "Carey Business School, Johns Hopkins University 	"
                                      )
                                      
                                    )
                                  ),
                                  
                                  userBox(
                                    title = userDescription(
                                      title = "Xinyi Chen",
                                      subtitle = "Developer",
                                      type = 2,
                                      image = "3.jpeg"
                                      #backgroundImage = "https://www.freecodecamp.org/news/content/images/size/w2000/2021/06/w-qjCHPZbeXCQ-unsplash.jpg"
                                    ),
                                    # status = "maroon",
                                    p(
                                      tags$h5(
                                        "Master of Science in Business Analytics and Risk Management",
                                        br(),
                                        "Carey Business School, Johns Hopkins University 	"
                                      )
                                    )
                                  ),
                                  userBox(
                                    title = userDescription(
                                      title = "Xiaoxiao Ge",
                                      subtitle = "Developer",
                                      type = 2,
                                      image = "4.jpeg"
                                      #   backgroundImage = "https://www.freecodecamp.org/news/content/images/size/w2000/2021/06/w-qjCHPZbeXCQ-unsplash.jpg"
                                    ),
                                    # status = "warning",
                                    p(
                                      tags$h5(
                                        "Master of Science in Business Analytics and Risk Management",
                                        br(),
                                        "Carey Business School, Johns Hopkins University 	"
                                      )
                                      
                                    )
                                  )
                                ),
                                fluidRow (column(
                                  width = 12,
                                  box(
                                    title = "Data Source",
                                    solidHeader = T,
                                    width = NULL,
                                    tags$h5(
                                      "1. GUN VIOLENCE ARCHIVE" ,
                                      br(),
                                      "https://www.gunviolencearchive.org/",
                                      br(),
                                      br(),
                                      "2. GDP and Personal Income Data from 2014-2011 in each state of USA" ,
                                      br(),
                                      "https://www.bea.gov/",
                                      br(),
                                      br(),
                                      "3. State Unemployment Rate" ,
                                      br(),
                                      "https://www.bls.gov/charts/state-employment-and-unemployment/state-unemployment-rates-animated.htm",
                                      br(),
                                      br(),
                                      "4. State Education Rate" ,
                                      br(),
                                      "https://wallethub.com/edu/e/most-educated-states/31075#cinda-klickna",
                                      br(),
                                      br(),
                                      "5. CDC: Death Rate by Incidents and Causes" ,
                                      br(),
                                      "https://wonder.cdc.gov/controller/datarequest/D76;jsessionid=D61432F03AFC472DD26C8DE6D399?stage=results&action=sort&direction=MEASURE_DESCEND&measure=D76.M1",
                                      
                                      
                                    )
                                  )
                                ))
                              )))
                    ))
                  )
                ))

server <- function(input, output, session) {
  # Tab1 Intro
  output$leading_cause = renderPlotly({
    Cause = reorder(data_causes$Causes, -data_causes$Deaths)
    leadingcauses = ggplot(data_causes,
                           aes(
                             x = Cause,
                             y = Deaths,
                             fill = Cause,
                             text = paste("Cause of Death:", Causes, "\nNumber of Deaths:", Deaths)
                           )) +
      geom_bar(stat = "identity", width = 0.6) +
      scale_fill_manual(
        values = c(
          "darkred",
          "lightgrey",
          "lightgrey",
          "lightgrey",
          "lightgrey",
          "lightgrey",
          "lightgrey",
          "lightgrey",
          "lightgrey",
          "lightgrey"
        )
      ) +
      labs(
        y = "Total Deaths in 2020",
        x = " ",
        title = "",
        caption = "Source: CDC WONDER. Includes children ages 1-17"
      ) +
      theme_minimal() +
      guides(x = guide_axis(n.dodge = 1)) +
      theme(
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    
    ggplotly(leadingcauses, tooltip = "text")
  })
  
  output$gun_deaths = renderPlotly({
    
    intents = data_intents %>%
      filter(Year >= 2010)%>%
      ggplot(
      aes(
        x = Year,
        y = Deaths,
        color = Intents,
        group = Intents,
        text = paste(
          "Year:",
          Year,
          "\nNumber of Deaths:",
          Deaths,
          "\nIntent of Deaths:",
          Intents
        )
      )
    ) +
      geom_line(size = 1) +
      geom_point() +
      scale_color_brewer(type = "seq",
                         palette = 'Reds',
                         direction = -1) +
      scale_x_continuous(breaks = seq(from = 2010, to = 2020, by = 1)) +
      labs(
        y = "Gun Deaths",
        x = " ",
        title = "",
        caption = "Source: CDC WONDER. Includes children ages 1-17"
      ) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank())
    
    ggplotly(intents, tooltip = "text")
  })
  
  # Tab2 Map
  output$map = renderLeaflet({
    c <-
      leaflet(data = data_c %>% filter(as.numeric(format(
        Incident_Date, '%Y'
      )) == input$year)) %>%
      addTiles() %>%
      setView(lng = -93.85,
              lat = 37.45,
              zoom = 4) %>%
      addMarkers(
        lng = ~ Longitude,
        lat = ~ Latitude,
        popup = ~ label_col ,
        clusterOptions = markerClusterOptions()
      )
    
    s <-
      leaflet(data = data_s %>% filter(as.numeric(format(
        Incident_Date, '%Y'
      )) == input$year)) %>%
      addTiles() %>%
      setView(lng = -93.85,
              lat = 37.45,
              zoom = 4) %>%
      addMarkers(
        lng = ~ Longitude,
        lat = ~ Latitude,
        popup = ~ label_col ,
        clusterOptions = markerClusterOptions()
      )
    
    if (input$select == 1)
      return(c)
    else
      return(s)
  })
  
  
  # Tab4 Trend
  ## Trend plot
  output$trend = renderPlotly({
    trend <-
      ggplot(data = row_join,
             mapping = aes(x = Year, y = Incidents, color = Category)) +
      geom_line(size = 1) +
      geom_point() +
      scale_color_manual(values = c("navy", "dark red")) +
      labs(y = "Number of incidents",
           x = "Year",
           color = "Category") +
      scale_x_continuous(limits = c(2014, 2021),
                         breaks = seq(2013, 2021, 1)) +
      theme(
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      ) +
      geom_vline(aes(xintercept = 2020), linetype = "dashed") +
      annotate(
        "text",
        x = 2020.5,
        y = 700,
        label = "Covid-19 Outbreak",
        color = "darkgrey",
        size = 3
      )
    
    ggplotly(trend)
  })
  ## Trend by type plot
  output$trend_by_type = renderPlotly({
    trend_by_type <- child_shoot_class %>%
      filter(Classification == input$select_class) %>%
      group_by(year, Classification) %>%
      summarise(count = n()) %>%
      drop_na()  %>%
      ggplot(aes(
        x = year,
        y = count,
        group = Classification,
        color = Classification
      )) +
      geom_line(size = 1) +
      geom_point() +
      scale_color_manual(values = wes_palette("BottleRocket1")) +
      labs(x = "Year", y = "Number of Accidents", title = "Total Incidents Trend by Type") +
      theme(
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      )
    ggplotly(trend_by_type)
  })
  
  ##Trend by Month Plot
  output$trend_by_month = renderPlotly({
    shoot_by_month <- child_shoot_class %>%
      filter(month == input$select_month) %>%
      group_by(month, Killed_or_Injured) %>%
      summarise(count = n()) %>%
      drop_na()  %>%
      ggplot(aes(month)) +
      geom_bar(aes(weight = count, fill = Killed_or_Injured)) +
      scale_x_continuous(breaks = seq(1, 12, by = 1)) +
      scale_fill_manual(values = wes_palette("BottleRocket1")) +
      facet_wrap(~ Killed_or_Injured) +
      labs(x = "Month", y = "Number of Incidents by Month", title = "Total Incidents by Month")
    ggplotly(shoot_by_month)
  })
}

shinyApp(ui = ui, server = server)