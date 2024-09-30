# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(nbastatR)
library(hoopR)
library(bslib)
library(DT)
library(shinycssloaders)

# Load court dimensions for visualization
devtools::source_url("https://github.com/Henryjean/NBA-Court/blob/main/CourtDimensions.R?raw=TRUE")
court_points <- court_points %>% filter(y < 47)

# Load shot data for the 2023-24 season
Sys.setenv(VROOM_CONNECTION_SIZE = 131072*2)
shots.data <- teams_shots(all_active_teams = T, season_types = "Regular Season", seasons = 2024)
league.shots <- nba_shotchartleaguewide(season = "2023-24")
league.avg <- league.shots$League_Wide

# Preprocess league average data
league.avg <- league.avg %>%
  mutate(shot.zone = paste(SHOT_ZONE_BASIC, SHOT_ZONE_AREA, SHOT_ZONE_RANGE, sep = " - "),
         FG_PCT = round(as.numeric(FG_PCT)*100),2) %>%
  filter(SHOT_ZONE_RANGE != "Back Court Shot")

#Prepare shot data for player analysis
shots.data <- shots.data %>%
  mutate(nameZone = paste0(nameZone,"(", slugZone, ")"),
         shot.zone = paste(zoneBasic, nameZone, zoneRange, sep = " - "),
         locationX = as.numeric(as.character(locationX)) / 10,
         locationY = as.numeric(as.character(locationY)) / 10 + hoop_center_y) %>%
  filter(zoneRange != "Back Court Shot")

# Adjust shot coordinates to match court location
shots.data$locationX <- shots.data$locationX * -1 
shots.data <- shots.data %>% filter(distanceShot <= 35)

# Define bins for heatmap (X and Y breakpoints)
x.breaks <- seq(-25, 25, by = 1)
y.breaks <- seq(0, 40, by = 1)

# Define midpoints for heatmap bins
x.midpoints <- x.breaks[-length(x.breaks)] + diff(x.breaks) / 2
y.midpoints <- y.breaks[-length(y.breaks)] + diff(y.breaks) / 2

# Aggregate shot data into hex bins
shot.chart.hex <- shots.data %>%
  mutate(
    hex.x.bin = cut(locationX, breaks = x.breaks),
    hex.y.bin = cut(locationY, breaks = y.breaks),
    hex.x = x_midpoints[as.numeric(hex.x.bin)],  # Convert bin to midpoint
    hex.y = y_midpoints[as.numeric(hex.y.bin)]   # Convert bin to midpoint
  ) %>%
  group_by(hex.x, hex.y, shot.zone, namePlayer) %>%
  summarize(
    shots.made = sum(isShotMade == TRUE),
    shots.attempted = sum(isShotAttempted == TRUE),
    fg.percentage = round((shots.made / shots.attempted) * 100,2),
    .groups = "drop"
  ) %>%
  filter(!is.na(hex.x) & !is.na(hex.y))

# Join with league average data and calculate z-scores
zone.fg <- shot.chart.hex %>%
  group_by(shot.zone, namePlayer) %>%
  summarize(
    zone.pct = round((sum(shots.made) / sum(shots.attempted)) *100,2),
    zone.shots.made = sum(shots.made),
    zone.shots.attempted = sum(shots.attempted))

shot.chart.hex <- shot.chart.hex %>% 
  left_join(zone.fg, by = c("shot.zone", "namePlayer")) %>%
  left_join(select(league.avg, shot.zone, FG_PCT), by = "shot.zone") %>%
  mutate(sd.pct = sd(zone.pct, na.rm = TRUE), 
         z.score = (zone.pct - FG_PCT) / sd.pct)

# User interface layout
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "flatly"),
  title = "NBA Shot Charts: 2023-24 Season",
  sidebar = tagList(selectInput("player", "Select a Player:", 
                                choices = sort(unique(shots.data$namePlayer)),
                                selectize = TRUE,
                                selected = "Jalen Brunson"),
                    HTML("<p style='color: black;'>Please be patient while the charts load.<br><br>
                         The heatmap shows how a player shoots from different zones on the court, using colors to highlight their shooting percentage compared to the league average. <br><br>
                         The shot density chart illustrates how often a player takes shots from different spots on the court compared to how often other players shoot from those same areas. Thanks to Owen Phillips of The F5 for the guidance on creating this chart.</p>")
                    ),
  layout_columns(
    card(full_screen = TRUE, 
         plotOutput("shot.chart.heatmap")),
    card(full_screen = TRUE, 
         plotOutput("shot.chart.density")),
    card(div(DTOutput("pct.table")), 
         style = "font-size:80%"),
    col_widths = c(6,6,12),
    row_heights = c(1.5,1)
    )
  )


# Server-side logic for rendering charts and tables
server <- function(input, output, session) {

  # Update player dropdown based on available shot data
  updateSelectInput(session, "player", choices = sort(unique(shots.data$namePlayer)))
  
  # Reactive data for heatmap plot
  heat.map.data <- reactive({
    
      shot.chart.hex %>%
      filter(namePlayer == input$player)
    
  })
  
  cached_density_data <- reactiveVal(NULL)
  cached_player <- reactiveVal(NULL)
  
  # Reactive data for shot density chart
  density.data <- reactive({
    
    # If cached player matches the selected player, return cached data
    if (!is.null(cached_player()) && cached_player() == input$player) {
      return(cached_density_data())
    }
    
    # Code for calculating density difference between selected player and league
    player1 <- input$player
    n <- 200
    
    pl1 <- shots.data %>% 
      select(locationX, locationY, namePlayer) %>% 
      filter(namePlayer == player1)
    
    pl2 <- shots.data %>% 
      select(locationX, locationY, namePlayer) %>% 
      filter(namePlayer != player1)
    
    pl1.x <- pull(pl1, locationX)
    pl1.y <- pull(pl1, locationY) 
    
    pl2.x <- pull(pl2, locationX)
    pl2.y <- pull(pl2, locationY)
    
    x.rng = range(c(pl1.x, pl2.x))
    y.rng = range(c(pl1.y, pl2.y))
    
    bandwidth.x <- MASS::bandwidth.nrd(c(pl1.x, pl2.x))
    bandwidth.y <- MASS::bandwidth.nrd(c(pl1.y, pl2.y))
    
    bandwidth.calc <- c(bandwidth.x, bandwidth.y)
    
    d2.pl1 = MASS::kde2d(pl1.x, pl1.y, h = bandwidth.calc, n=n, lims=c(x.rng, y.rng))
    d2.pl2 = MASS::kde2d(pl2.x, pl2.y, h = bandwidth.calc, n=n, lims=c(x.rng, y.rng))
    
    df.diff <- d2.pl1
    
    df.diff$z <- d2.pl1$z - d2.pl2$z
    
    colnames(df.diff$z) <- df.diff$y
    
    df.diff <- df.diff$z %>% 
      as.data.frame() %>% 
      mutate(x = df.diff$x) %>% 
      pivot_longer(-x, names_to = "y", values_to = "z") %>% 
      mutate(y = as.double(y),
             bandwidth = list(bandwidth.calc),
             name = player1)
    
    df.diff$z <- ifelse(df.diff$z < 0, 0, df.diff$z)
    
    # Cache the result and store the selected player
    cached_density_data(df.diff)
    cached_player(input$player)
    
    return(df.diff)
  })
  
  # Render shot chart heatmap
  output$shot.chart.heatmap <- renderPlot({
    
    
    ggplot() + 
      geom_tile(data = heat.map.data(), aes(x = hex.x, y = hex.y, fill = z.score), color = "#666666", size = 0.1) +
      scale_fill_gradient2(low="blue",mid="white", high="red", midpoint=0) + 
      guides(color=FALSE, fill = FALSE) +
      labs(title = paste(input$player, " FG% Heatmap"),
           caption = paste0("Red spots = Higher field goal percentage than the league average",
                            "\n",
                            "Blue Spots = Lower field goal percentage than the league average")) +
      coord_fixed(clip = 'off')+
      scale_y_continuous(limits = c(-2.5, 45)) +
      scale_x_continuous(limits = c(-30, 30)) + 
      geom_path(data = court_points,
                aes(x = x, y = y, group = desc, linetype = dash),
                color = "black", size = .25)  +
      theme(legend.position = 'none',
            line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(face = 'bold', hjust= .5, size = 20), 
            plot.subtitle = element_text(hjust = .5, size = 15), 
            plot.caption = element_text(hjust = 0, size = 15),
            plot.background = element_rect(fill = 'white', color = "white"),
            panel.background = element_rect(fill = "#f8f1e6")) 
  })
  
  # Render shot density chart
  output$shot.chart.density <- renderPlot({
    
    ggplot() +
      geom_contour_fill(data = density.data() %>% filter(z >= mean(z)), aes(x = x, y = y, z = sqrt(z)))  +
      geom_contour_tanaka(data = density.data(), aes(x = x, y = y, z = sqrt(z)), bins = 5)  +
      coord_fixed(clip = 'off') +
      scale_fill_gradient2(low = "#f8f1e6", mid = "#f8f1e6", high = "darkgreen") +
      scale_y_continuous(limits = c(-2.5, 45)) +
      scale_x_continuous(limits = c(-30, 30)) +
      theme(legend.position = 'none',
            line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(), 
            plot.title = element_text(size = 20, hjust= .5, face = 'bold'),
            plot.caption = element_text(size = 15, hjust= 0),
            plot.background = element_rect(fill = 'white', color = "white"),
            panel.background = element_rect(fill = "#f8f1e6")) +
      labs(title = paste(input$player, "Shot Density Chart"), 
           caption = paste0("Darker regions represent a greater number of shots taken in that area compared to the rest of the league")) +
      geom_path(data = court_points,
                aes(x = x, y = y, group = desc, linetype = dash),
                color = "black", size = .25) 
  })
  
  # Render shot percentage table
  output$pct.table <- DT::renderDT({
    fg.table <- heat.map.data() %>% 
      select(namePlayer, shot.zone,zone.shots.made, zone.shots.attempted, zone.pct, FG_PCT) %>%
      rename("Player Name" = namePlayer, "Shot Zone" = shot.zone,"Shots Made" = zone.shots.made, "Shots Attempted" = zone.shots.attempted, "Player FG%" = zone.pct, "League Avg FG%" = FG_PCT) %>%
      distinct()
    
    datatable(fg.table, rownames = FALSE, style ="bootstrap", options = list(pageLength = 20, dom = 't', order = list(list(1, 'asc'))))
  })
}

# Launch the Shiny app
shinyApp(ui = ui, server = server)