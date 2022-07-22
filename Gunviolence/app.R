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
library(maps)
library(dplyr)
library(scales)
library(ggmap)
library(readxl)

# Reading data
data_c = read_csv("Children_data.csv")
data_c$Incident_Date <-
  as.Date(data_c$Incident_Date, format = "%m/%d/%y")
data_s = read_csv("School_data.csv")
data_s$Incident_Date <-
  as.Date(data_s$Incident_Date, format = "%m/%d/%y")
data_class_c = read_csv("Children_class_data.csv")
data_class_s = read_csv("School_class_data.csv")

MainStates <- map_data("state")

Child <- read.csv("Children_data3.csv")
School <- read.csv("School_data3.csv")
Child$Incident_Date = as.Date(Child$Incident_Date, "%m/%d/%Y")
School$Incident_Date = as.Date(School$Incident_Date, "%m/%d/%Y")
Child <- rename(Child, "lat" = "Latitude", "long" = "Longitude")
School <- rename(School, "lat" = "Latitude", "long" = "Longitude")


##############Read State Data
MainStates <- map_data("state")

##############Read GDP Data
StateGDP <-
  read.csv("Gross domestic product (GDP) by state.csv", as.is = TRUE)
StateGDP <- gather(StateGDP, key = "Year", value = "GDP", 2:9)
StateGDP$Year = gsub('X', '', StateGDP$Year, fixed = TRUE)
StateGDP$region <- tolower(StateGDP$region)

#############Read Unemployment Data
StateUnemploy <- read.csv("Unemployment_rate.csv", as.is = TRUE)
StateUnemploy <-
  gather(StateUnemploy, key = "Year", value = "Unemployment_rate", 2:9)
StateUnemploy$Year = gsub('X', '', StateUnemploy$Year, fixed = TRUE)
StateUnemploy$region <- tolower(StateUnemploy$region)

#############Read Personal Income Data
StatePersonalIn <-
  read.csv("Personal Income, Population, Per Capita Personal Income.csv",
           as.is = TRUE)
StatePersonalIncome <- StatePersonalIn[-c(1)]
StatePersonalIncome <-
  gather(StatePersonalIncome, key = "Year", value = "Personal_Income", 2:9)
StatePersonalIncome <-
  rename(StatePersonalIncome, "region" = "GeoName")
StatePersonalIncome$region <- tolower(StatePersonalIncome$region)
StatePersonalIncome$Year = gsub('X', '', StatePersonalIncome$Year, fixed =
                                  TRUE)

#############Read Educational Attainment Data
StateEducational <- read_excel("Educational Attainment.xlsx")
StateEducational <-
  gather(StateEducational, key = "Year", value = "Educational_Attainment", 2:8)
StateEducational$region <- tolower(StateEducational$region)
#StateEducational$Educational_Attainment <- paste(StateEducational$Educational_Attainment,"%")


############Merge Data
MergeStates1 <-
  full_join(StateUnemploy, StateGDP, by = c("region", "Year"))
MergeStates1 <-
  full_join(MergeStates1, StatePersonalIncome, by = c("region", "Year"))
MergeStates1 <-
  full_join(MergeStates1, StateEducational, by = c("region", "Year"))
MergedStates <- left_join(MainStates, MergeStates1, by = "region")


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

########################
######################UI
########################

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
                          "School Gun Violence Tracking",
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
                    
                    dashboardBody(
                      tags$head(tags$style(
                        HTML(
                          '
        ### dashboard color
        /* navbar - logo */
       .skin-red .main-header .logo:hover {
                              background-color: #222d32;
                              }
       .skin-red .main-header .logo {
                              background-color: #9b2222;
                              }
       /* navbar - top */
       .skin-red .main-header .navbar {
                              background-color: #9b2222;
                              }
       /* other links in the sidebarmenu when hovered */
       .skin-red .main-sidebar .sidebar .sidebar-menu a:hover {
                              background-color: #1b2428;
                              }
       .skin-red .main-sidebar .sidebar .sidebar-menu .active a {
                              background-color: #9b2222;
                              }
                              '
                        )
                      )),
                      
                      tabItems(
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
                                      "This visual dashboard provides insights into possible causes and trends related to children involved incidents, aiming to provide feasible analysis and solutions in dealing with the gun violence associated with children."
                                    ),
                                    tags$h5(tags$strong("Research Questions")),
                                    tags$h5(
                                      "Where and when did Children-involved incidents and School incidents happen?"
                                    ),
                                    tags$h5(
                                      "Are the shootings related to macro factors (Unemployment rate, education rate, GDP, Personal Income) in each state?"
                                    ),
                                    tags$h5(
                                      "What is the trend of incidents happened in schools and how can we improve the current situation?"
                                    ),
                                    img(src = "kids.webp", height = 500 ,style="display: block; margin-left: auto; margin-right: auto;")
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
                        
                        tabItem(
                          tabName = "Ana",
                          ####CXY
                          fluidRow(
                            column(
                              width = 4,
                              box(
                                title = "Select Time",
                                solidHeader = F,
                                collapsible = T,
                                collapsed = FALSE,
                                width = NULL,
                                selectInput(
                                  "select2",
                                  label = NULL,
                                  choices = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"),
                                  selected = 2014
                                )
                              )
                            ),
                            column(
                              width = 4,
                              box(
                                title = "Select Analysis Factor",
                                solidHeader = F,
                                collapsible = T,
                                collapsed = FALSE,
                                width = NULL,
                                selectInput(
                                  "select3",
                                  label = NULL,
                                  choices = c(
                                    "GDP",
                                    "Personal_Income",
                                    "Unemployment_rate",
                                    "Educational_Attainment"
                                  ),
                                  selected = "GDP"
                                )
                              )
                            ),
                            column(
                              width = 4,
                              box(
                                title = "Select Incident Types",
                                solidHeader = F,
                                collapsible = T,
                                collapsed = FALSE,
                                width = NULL,
                                selectInput(
                                  "select4",
                                  label = NULL,
                                  choices = c("School Incidents", "Children Incidents", "None"),
                                  selected = "None"
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
                              plotlyOutput("map2", height = 800),
                              width = NULL,
                              height = "auto",
                              plotlyOutput("m")
                            )
                            
                          ))
                        ),
                        
                        tabItem(
                          tabName = "Trend",
                          
                          fluidRow(column(
                            width = 12,
                            box(
                              title = "School Incidents are Boucing Back",
                              solidHeader = F,
                              width = NULL,
                              height = 500,
                              plotlyOutput("trend")
                              
                            )
                          )),
                          
                          fluidRow(column(
                            width = 6,
                            box(
                              title = "Children Shooters are Worsening the Situations",
                              solidHeader = F,
                              width = NULL,
                              selectInput(
                                "select_class",
                                label = "Select Incidents Type",
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
                              title = "Summer Breaks Bring More Gun Shots",
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
                          )),
                          
                          
                          
                          fluidRow(box(
                            title = "Feasible Solutions", width = 12,
                            column(12,
                                   fluidRow(tags$div(
                                     column(
                                       6,
                                       # br(),
                                       tags$h5(
                                         tags$strong("Enact Child Access Prevention Laws:"),
                                         "Child and teen gun deaths are preventable and child access prevention laws can reduce accidental shootings of children by as much as 23 percent. We must require that guns be stored safely so children and teens cannot access them unsupervised."
                                       ),
                                       tags$h5(
                                         tags$strong("Implement Universal Background Checks:"),
                                         "Current federal law does not require background checks for gun sales at gun shows, on the internet or between private individuals. Background checks do not prevent legal gun purchases but they could prevent child and teen gun deaths. We must extend background check requirements to cover all gun sales."
                                       ),
                                       tags$h5(
                                         tags$strong("Prohibit Firearm Access for High-Risk Groups:"),
                                         "We must keep guns out of the hands of those who would use them to harm children, families and communities. People convicted of domestic abuse or other violent crimes should have restricted gun access."
                                       ),
                                       tags$h5(
                                         tags$strong("Raising the minimum age to purchase a firearm:"),
                                         "This would help prevent young people from owning guns until their brains are more fully developed. Studies have found this policy decreases suicides and unintentional shootings."
                                       ),
                                       tags$h5(
                                         tags$strong("Fund Gun Violence Prevention Research:"),
                                         "To tackle the gun violence epidemic in America, citizens need more information, not more guns. We must increase federal funding for research on gun violence and its causes."
                                       )
                                     ),
                                     
                                     column(6, br(),
                                            tags$img(
                                       img(
                                         src = "school.webp",
                                         height = 350,
                                         style="display: block; margin-left: auto; margin-right: auto;"
                                       )
                                     ))
                                   )))
                          ))
                        ),
                        
                        
                        
                        
                        
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
                                          "Master of Science in Business Analytic and Risk Management",
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
                                          "Master of Science in Business Analytic and Risk Management",
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
                                          "Master of Science in Business Analytic and Risk Management",
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
                                          "Master of Science in Business Analytic and Risk Management",
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
                      )
                    )
                  )
                ))

server <- function(input, output, session) {
  # Tab1 Intro
  output$leading_cause = renderPlotly({
    Cause = reorder(data_causes$Causes,-data_causes$Deaths)
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
    filteredintents = data_intents %>%
      filter(Year >= 2010)
    
    intents = ggplot(
      filteredintents,
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
  
  #Tab 3 Map
  output$map2 = renderPlotly({
    m <- filter(MergedStates, as.numeric(Year) == input$select2)
    
    if (input$select3 == 'GDP') {
      p <-
        ggplot() + geom_polygon(
          data = m,
          mapping = aes(
            x = long,
            y = lat,
            group = group,
            fill = GDP,
            text = paste0("State: ", region)
          )
        )
    }
    
    if (input$select3 == 'Personal_Income') {
      p <-
        ggplot() + geom_polygon(
          data = m,
          mapping = aes(
            x = long,
            y = lat,
            group = group,
            fill = Personal_Income,
            text = paste0("State: ", region)
          )
        ) +
        
        #scale_fill_distiller(name="Personal Income",palette = "Greens",na.value = "grey50")
        scale_fill_continuous(
          name = "Personal Income",
          low = "light green",
          high = "dark green",
          na.value = "grey50"
        )
    }
    
    if (input$select3 == 'Unemployment_rate') {
      p <-
        ggplot() + geom_polygon(
          data = m,
          mapping = aes(
            x = long,
            y = lat,
            group = group,
            fill = Unemployment_rate,
            text = paste0("State: ", region)
          )
        ) +
        scale_fill_continuous(
          name = "Unemployment_rate",
          low = "light blue",
          high = "dark blue",
          na.value = "grey50"
        )
    }
    
    if (input$select3 == 'Educational_Attainment') {
      p <-
        ggplot() + geom_polygon(
          data = m,
          mapping = aes(
            x = long,
            y = lat,
            group = group,
            fill = Educational_Attainment,
            text = paste0("State: ", region)
          )
        ) +
        scale_fill_distiller(name = "Educational_Attainment",
                             palette = "Oranges",
                             na.value = "grey50")
      # scale_fill_continuous(name="Educational_Attainment",low = "light orange", high = "dark orange", na.value = "grey50")
    }
    
    
    if (input$select4 == 'Children Incidents') {
      c1 <-
        filter(Child, as.numeric(format(Incident_Date, '%Y')) == input$select2)
      p <-
        p + geom_point(
          data = c1,
          mapping = aes(x = long, y = lat),
          color = "yellow",
          alpha = 1 / 3
        )
      #geom_point(data=s1,mapping=aes(x=long,y=lat),color="purple")
    }
    
    if (input$select4 == 'School Incidents') {
      #geom_point(data=c1,mapping=aes(x=long,y=lat),color="yellow")+
      s1 <-
        filter(School, as.numeric(format(Incident_Date, '%Y')) == input$select2)
      p <-
        p + geom_point(
          data = s1,
          mapping = aes(x = long, y = lat),
          color = "purple",
          alpha = 1 / 3
        )
      
    }
    p <- p +
      labs(y = "Latitude",
           x = "Longitude") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
      )
    return(p)
    
  })
  
  # Tab4 Trend
  ## Trend plot
  output$trend = renderPlotly({
    trend <-
      ggplot(data = row_join,
             mapping = aes(x = Year, y = Incidents, color = Category)) +
      geom_line(size = 1) +
      geom_point() +
      scale_color_manual(values = c("dark red", "dark grey")) +
      labs(y = "Number of incidents",
           x = "Year",
           color = "") +
      scale_x_continuous(limits = c(2014, 2021),
                         breaks = seq(2013, 2021, 1)) +
      theme(
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
      labs(x = "Year", y = "Number of Incidents", color = NULL) +
      theme(
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
      theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(1, 12, by = 1)) +
      scale_fill_manual(values = wes_palette("BottleRocket1")) +
      facet_wrap( ~ Killed_or_Injured) +
      labs(x = "Month", y = "Number of Incidents") +
      theme(panel.background = element_blank(),
            legend.position="none")
    
    ggplotly(shoot_by_month)
  })
}

shinyApp(ui = ui, server = server)