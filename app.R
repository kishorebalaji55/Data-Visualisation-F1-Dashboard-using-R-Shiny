library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(bslib)

# loading data
races <- read_csv("data/races.csv")
drivers <- read_csv("data/drivers.csv")
constructors <- read_csv("data/constructors.csv")
results <- read_csv("data/results.csv")
lap_times <- read_csv("data/lap_times.csv")
pit_stops <- read_csv("data/pit_stops.csv")

# data pre-processing
race_roster <- results %>%
  select(raceId, driverId, constructorId, positionOrder, points, grid) %>%
  inner_join(races %>% select(raceId, year, name, round, date), by = "raceId") %>%
  inner_join(drivers %>% select(driverId, code, surname), by = "driverId") %>%
  inner_join(constructors %>% select(constructorId, constructorRef = name), by = "constructorId") %>%
  mutate(driver_label = paste(surname)) 

pit_counts <- pit_stops %>%
  group_by(raceId, driverId) %>%
  summarise(pit_count = n(), .groups = "drop")

race_roster <- race_roster %>%
  left_join(pit_counts, by = c("raceId", "driverId")) %>%
  mutate(pit_count = replace_na(pit_count, 0)) %>%
  mutate(pos_change = grid - positionOrder)

# helper attributes and variables
theme_f1_dark <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#1e1e1e", color = NA),
      panel.background = element_rect(fill = "#1e1e1e", color = NA),
      legend.background = element_rect(fill = "#1e1e1e", color = NA),
      text = element_text(color = "#ecf0f1"),
      axis.text = element_text(color = "#bdc3c7"),
      axis.text.x = element_text(angle = 45, hjust = 1), 
      panel.grid = element_line(color = "#34495e"),
      legend.key = element_blank()
    )
}

dark_layout <- function(p) {
  p %>% layout(
    paper_bgcolor = '#1e1e1e',
    plot_bgcolor = '#1e1e1e',
    font = list(color = '#ecf0f1'),
    xaxis = list(gridcolor = '#34495e'),
    yaxis = list(gridcolor = '#34495e')
  )
}

my_card <- function(title, plot_obj, footer = NULL) {
  div(class = "box", style = "margin-bottom: 15px; padding: 0;",
      div(class = "box-header", title = title),
      div(class = "box-body", plot_obj),
      if (!is.null(footer)) div(class = "box-footer", style = "background: #111; color: #bdc3c7; font-size: 0.8em; padding: 5px;", footer) else NULL
  )
}

# UI definition
ui <- dashboardPage(
  skin = "black",
  title = "F1 HUB",
  
  dashboardHeader(
    title = span(
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/3/33/F1.svg", 
               height = "30px", style = "margin-right:10px;"),
      "Telemetry Hub"
    ),
    titleWidth = 250
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(id = "tabs",
                menuItem("Race Day Analysis", tabName = "race_day", icon = icon("tachometer-alt")),
                menuItem("Multi-Year Trends", tabName = "season_stats", icon = icon("chart-line")),
                menuItem("Strategy Correlation", tabName = "complex_stats", icon = icon("project-diagram")), 
                menuItem("Animated Insights", tabName = "animated_stats", icon = icon("film")) 
    ),
    hr(),
    
    # page 1
    conditionalPanel(condition = "input.tabs == 'race_day'",
                     h4("Race Filters", style = "padding-left:15px; color:#aaa;"),
                     selectInput("p1_year", "Select Season:", choices = sort(unique(race_roster$year), decreasing = TRUE), selected = 2023),
                     uiOutput("p1_race_ui"),
                     pickerInput("p1_drivers", "Focus Drivers:", choices = NULL, multiple = TRUE, options = list("actions-box" = TRUE))
    ),
    
    # page 2
    conditionalPanel(condition = "input.tabs == 'season_stats'",
                     h4("Trend Filters", style = "padding-left:15px; color:#aaa;"),
                     sliderInput("p2_year_range", "Year Range:", min = 1950, max = 2024, value = c(2018, 2023), sep = ""),
                     pickerInput("p2_teams", "Compare Teams:", choices = sort(unique(race_roster$constructorRef)), selected = c("Red Bull", "Mercedes", "Ferrari", "McLaren"), multiple = TRUE, options = list("actions-box" = TRUE))
    ),
    
    # page 3
    conditionalPanel(condition = "input.tabs == 'complex_stats'",
                     h4("Strategy Context", style = "padding-left:15px; color:#aaa;"),
                     selectInput("p3_year", "Season Context:", choices = sort(unique(race_roster$year), decreasing = TRUE), selected = 2023)
    ),
    
    # page 4
    conditionalPanel(condition = "input.tabs == 'animated_stats'",
                     h4("Animation Context", style = "padding-left:15px; color:#aaa;"),
                     selectInput("p4_year", "Select Season to Animate:", choices = sort(unique(race_roster$year), decreasing = TRUE), selected = 2023)
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #2c3e50; }
      .box { background: #1e1e1e; border-top: 3px solid #ff232b; color: white; box-shadow: 0 4px 8px rgba(0,0,0,0.5); }
      .box-header { color: white; background: #111; padding: 10px; }
      .box-footer { background: #111; }
      .skin-black .main-header .logo { background-color: #1e1e1e; color: white; border-bottom: 1px solid #333; }
      .skin-black .main-header .navbar { background-color: #1e1e1e; }
      .skin-black .main-sidebar { background-color: #000000; }
    "))),
    
    tabItems(
      # page 1
      tabItem(tabName = "race_day",
              fluidRow(
                column(8, my_card(title = "Race Trace: Position Changes", plotlyOutput("p1_lap_chart", height = "400px"))),
                column(4, my_card(title = "Position Heatmap", plotlyOutput("p1_heatmap", height = "400px")))
              ),
              fluidRow(
                column(6, my_card(title = "Pit Stop Strategy", plotlyOutput("p1_pit_plot", height = "300px"))),
                column(6, my_card(title = "Pace Consistency", plotlyOutput("p1_pace_plot", height = "300px")))
              )
      ),
      
      # page 2
      tabItem(tabName = "season_stats",
              fluidRow(column(12, my_card(title = "Constructor Supremacy", plotlyOutput("p2_team_trend", height = "350px")))),
              fluidRow(
                column(8, my_card(title = "Driver Points Trajectory", plotlyOutput("p2_driver_trend", height = "350px"))),
                column(4, my_card(title = "Win Share", plotlyOutput("p2_win_share", height = "350px")))
              )
      ),
      
      # page 3
      tabItem(tabName = "complex_stats",
              fluidRow(column(12, my_card(title = "Strategy Impact Matrix (Static)", plotlyOutput("p3_matrix_plot", height = "500px")))),
              fluidRow(column(12, my_card(title = "Overtaking Efficiency (Static)", plotlyOutput("p3_efficiency_plot", height = "300px"))))
      ),
      
      # page 4
      tabItem(tabName = "animated_stats",
              fluidRow(
                column(12, my_card(
                  title = "Animation 1: Grid vs. Finish (Overtaking Dynamics)", 
                  plotlyOutput("p4_anim_scatter", height = "500px"),
                  footer = "Bubbles moving ABOVE the white diagonal line = Positions Gained. Bubbles BELOW = Positions Lost."
                ))
              ),
              fluidRow(
                column(12, my_card(
                  title = "Animation 2: Championship Bar Race", 
                  plotlyOutput("p4_anim_race", height = "500px"),
                  footer = "Watch the championship battle evolve round-by-round."
                ))
              )
      )
    )
  )
)

# server side
server <- function(input, output, session) {
  
  # page 1
  output$p1_race_ui <- renderUI({
    req(input$p1_year)
    races_list <- race_roster %>% filter(year == input$p1_year) %>% distinct(name, round) %>% arrange(round) %>% pull(name)
    selectInput("p1_race", "Grand Prix:", choices = races_list)
  })
  observeEvent(input$p1_race, {
    req(input$p1_year, input$p1_race)
    drivers_in_race <- race_roster %>% filter(year == input$p1_year, name == input$p1_race) %>% arrange(positionOrder)
    updatePickerInput(session, "p1_drivers", choices = setNames(drivers_in_race$driverId, drivers_in_race$driver_label), selected = drivers_in_race$driverId[1:10])
  })
  p1_laps_data <- reactive({
    req(input$p1_race, input$p1_drivers)
    target_race_id <- races %>% filter(year == input$p1_year, name == input$p1_race) %>% pull(raceId)
    lap_times %>% filter(raceId == target_race_id, driverId %in% input$p1_drivers) %>% left_join(drivers, by="driverId") %>%
      mutate(driver_label = surname) 
  })
  
  # charts in page 1
  output$p1_lap_chart <- renderPlotly({
    req(p1_laps_data())
    p <- ggplot(p1_laps_data(), aes(x=lap, y=position, group=surname, color=driver_label)) +
      geom_line(size=0.8) + scale_y_reverse(breaks=1:20) + theme_f1_dark() + labs(y="Pos", x="Lap", color="Driver")
    ggplotly(p, tooltip = c("x", "y", "group")) %>% dark_layout()
  })
  output$p1_heatmap <- renderPlotly({
    req(p1_laps_data())
    p <- ggplot(p1_laps_data(), aes(x=lap, y=reorder(driver_label, -position), fill=position)) +
      geom_tile() + scale_fill_viridis_c(direction=-1) + theme_f1_dark() + labs(x="Lap", y="")
    ggplotly(p) %>% dark_layout()
  })
  output$p1_pit_plot <- renderPlotly({
    req(input$p1_race)
    target_race_id <- races %>% filter(year == input$p1_year, name == input$p1_race) %>% pull(raceId)
    df <- pit_stops %>% filter(raceId == target_race_id, driverId %in% input$p1_drivers) %>% 
      left_join(drivers, by="driverId") %>% mutate(sec = milliseconds/1000, driver_label = surname)
    p <- ggplot(df, aes(x=lap, y=sec, color=driver_label)) + geom_point(size=3) + theme_f1_dark() + labs(y="Stop Time (s)", color="Driver")
    ggplotly(p) %>% dark_layout()
  })
  output$p1_pace_plot <- renderPlotly({
    req(p1_laps_data())
    clean <- p1_laps_data() %>% group_by(driverId) %>% filter(milliseconds < median(milliseconds)*1.07)
    avg_pace <- mean(clean$milliseconds/1000)
    p <- ggplot(clean, aes(x=driver_label, y=milliseconds/1000, fill=driver_label)) + 
      geom_violin(color="white") + geom_hline(yintercept=avg_pace, linetype="dashed", color="white") + 
      theme_f1_dark() + labs(y="Lap Time (s)", x="") + theme(legend.position="none")
    ggplotly(p) %>% dark_layout()
  })
  
  # page 2
  p2_data <- reactive({
    req(input$p2_year_range, input$p2_teams)
    race_roster %>% filter(year >= input$p2_year_range[1], year <= input$p2_year_range[2], constructorRef %in% input$p2_teams)
  })
  
  # team trends
  output$p2_team_trend <- renderPlotly({
    req(p2_data())
    team_trend <- p2_data() %>% group_by(year, constructorRef) %>% summarise(total_points = sum(points), .groups="drop")
    p <- ggplot(team_trend, aes(x = year, y = total_points, color = constructorRef)) +
      geom_line(size = 1.2) + geom_point(size = 2) + 
      scale_x_continuous(breaks = function(x) seq(ceiling(min(x)), floor(max(x)), by = 1)) + # Force Integer Years
      theme_f1_dark() + labs(x = "Season", y = "Points")
    ggplotly(p) %>% dark_layout()
  })
  
  # driver trends
  output$p2_driver_trend <- renderPlotly({
    req(p2_data())
    driver_trend <- p2_data() %>% group_by(year, driver_label, constructorRef) %>% summarise(pts = sum(points), .groups="drop")
    p <- ggplot(driver_trend, aes(x = year, y = pts, color = driver_label, group = driver_label)) +
      geom_line() + geom_point() + facet_wrap(~constructorRef) + 
      scale_x_continuous(breaks = function(x) seq(ceiling(min(x)), floor(max(x)), by = 1)) + # Force Integer Years
      theme_f1_dark() + labs(x = "Year", y = "Points", color="Driver") +
      theme(strip.background = element_rect(fill="#34495e"), strip.text = element_text(color="white"))
    ggplotly(p) %>% dark_layout()
  })
  
  output$p2_win_share <- renderPlotly({
    req(p2_data())
    wins <- p2_data() %>% filter(positionOrder == 1) %>% count(constructorRef)
    plot_ly(wins, labels = ~constructorRef, values = ~n, type = 'pie', hole = 0.4) %>% 
      layout(title = list(text="Win Share", font=list(color="white")), showlegend = FALSE, paper_bgcolor = '#1e1e1e', plot_bgcolor = '#1e1e1e', font = list(color = '#ecf0f1'))
  })
  
  # page 3: scatter plot and cummulative gains
  output$p3_matrix_plot <- renderPlotly({
    req(input$p3_year)
    df <- race_roster %>% filter(year == input$p3_year, points > 0) 
    p <- ggplot(df, aes(x = grid, y = positionOrder, size = pit_count, color = constructorRef, text = paste(driver_label, "@", name))) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "white") + 
      geom_jitter(alpha = 0.7, width = 0.2, height = 0.2) + 
      scale_x_reverse(breaks = 1:20) + scale_y_reverse(breaks = 1:20) +
      theme_f1_dark() + labs(x = "Grid Start", y = "Finish Pos", color = "Team", size = "Stops")
    ggplotly(p, tooltip = c("text", "size")) %>% dark_layout()
  })
  output$p3_efficiency_plot <- renderPlotly({
    req(input$p3_year)
    df <- race_roster %>% filter(year == input$p3_year) %>% group_by(constructorRef) %>% summarise(avg_gain = mean(pos_change), .groups = "drop") %>% arrange(desc(avg_gain))
    p <- ggplot(df, aes(x = reorder(constructorRef, avg_gain), y = avg_gain, fill = avg_gain > 0)) +
      geom_col() + theme_f1_dark() + labs(x = "Team", y = "Avg Pos Gained") + theme(legend.position = "none")
    ggplotly(p) %>% dark_layout()
  })
  
  # animations page 4
  output$p4_anim_scatter <- renderPlotly({
    req(input$p4_year)
    df <- race_roster %>% filter(year == input$p4_year, points > 0) 
    p <- ggplot(df, aes(x = grid, y = positionOrder, size = pit_count, color = constructorRef, 
                        frame = round, 
                        text = paste("Driver:", driver_label, "<br>Round:", round))) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "white", alpha=0.5) + 
      geom_jitter(alpha = 0.8, width = 0.1, height = 0.1) + 
      scale_x_reverse(breaks = 1:20) + scale_y_reverse(breaks = 1:20) +
      theme_f1_dark() + labs(x = "Grid Start", y = "Finish Position", color = "Team")
    
    ggplotly(p, tooltip = c("text")) %>% 
      animation_opts(frame = 1000, transition = 500) %>% 
      dark_layout()
  })
  
  output$p4_anim_race <- renderPlotly({
    req(input$p4_year)
    df_race <- race_roster %>%
      filter(year == input$p4_year) %>%
      select(round, name, driver_label, points, constructorRef) %>%
      arrange(round) %>%
      group_by(driver_label) %>%
      mutate(cum_points = cumsum(points)) %>%
      ungroup()
    
    df_ranked <- df_race %>%
      group_by(round) %>%
      mutate(rank = rank(-cum_points, ties.method = "first")) %>%
      filter(rank <= 10) %>% 
      ungroup()
    
    p <- ggplot(df_ranked, aes(x = cum_points, y = rank, group = driver_label, fill = constructorRef, frame = round)) +
      geom_tile(aes(x = cum_points/2, width = cum_points, height = 0.8)) +
      geom_text(aes(x = -1, label = driver_label), hjust = 1, color = "white") +
      scale_y_reverse(breaks = 1:10) + 
      theme_f1_dark() +
      labs(x = "Cumulative Points", y = "Rank", fill = "Team") +
      theme(legend.position = "none", axis.text.y = element_blank()) 
    
    ggplotly(p) %>% 
      animation_opts(frame = 800, transition = 400, redraw = FALSE) %>% 
      dark_layout()
  })
}

shinyApp(ui, server)