# Sterb's NFL Drive Analyzer
# @EthanSterbis on X

library(shiny)
library(dplyr)
library(readr)
library(bslib)
library(DT)

# =======================================================
# Load Data
# =======================================================
pbp <- read_csv("data/pbp.csv", show_col_types = FALSE)

# Keep only plays with valid posteam & defteam
pbp <- pbp %>% filter(!is.na(posteam), !is.na(defteam))

# =======================================================
# Safe helpers
# =======================================================
safe_min <- function(x) {
  out <- suppressWarnings(min(x, na.rm = TRUE))
  if (!is.finite(out)) NA_real_ else out
}
safe_max <- function(x) {
  out <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(out)) NA_real_ else out
}
safe_time <- function(sec) {
  ifelse(!is.finite(sec) | is.na(sec),
         NA_character_,
         sprintf("%02d:%02d", floor(sec / 60), round(sec %% 60)))
}

# =======================================================
# Precompute drive-level aggregates
# =======================================================
make_drive_data <- function(dat){
  dat %>%
    rename(drive_result = fixed_drive_result) %>%
    group_by(drive_id = paste(game_id, drive, posteam, sep="_")) %>%
    summarise(
      season        = first(season),
      season_type   = first(season_type),
      week          = first(week),
      game_id       = first(game_id),
      drive_num     = first(drive),
      posteam       = first(posteam),
      defteam       = first(defteam),
      drive_result  = first(drive_result),
      
      Plays         = n(),
      
      PointsPerDrive = case_when(
        drive_result == "Touchdown" ~ 6,
        drive_result == "Field goal" ~ 3,
        drive_result == "Opp touchdown" ~ -6,
        drive_result == "Safety" ~ -2,
        TRUE ~ 0
      ),
      
      EPA_per_Play   = mean(epa, na.rm=TRUE),
      WPA_per_Drive  = sum(wpa, na.rm=TRUE),
      
      DriveTimeSec   = safe_max(game_seconds_remaining) -
        safe_min(game_seconds_remaining),
      
      Sacks          = sum(sack == 1, na.rm=TRUE),
      ExplosivePlays = sum((pass == 1 & yards_gained >= 15) |
                             (rush == 1 & yards_gained >= 10), na.rm=TRUE),
      
      PROE           = mean(pass_oe, na.rm=TRUE),
      StartFP        = first(yardline_100),
      
      # Filters
      MinWP = safe_min(wp),
      MaxWP = safe_max(wp),
      StartScoreDiff = first(posteam_score) - first(defteam_score),
      StartQtr = first(qtr),
      
      .groups="drop"
    ) %>%
    mutate(`Time/Dr` = safe_time(DriveTimeSec))
}

drive_data <- make_drive_data(pbp)

# =======================================================
# Team List
# =======================================================
all_teams <- sort(unique(c(drive_data$posteam, drive_data$defteam)))

# =======================================================
# UI
# =======================================================
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg      = "#2B2B2B",
    fg      = "rgb(234,234,234)",
    primary = "#43B6FF"
  ),
  tags$head(
    tags$title("Sterb's NFL Drive Analyzer"),
    tags$style(HTML("
      th { text-align: center !important; }
      td { text-align: center !important; }
    "))
  ),
  h2("Sterb's NFL Drive Analyzer", style="padding-top:10px; font-size:22px;"),
  fluidRow(
    column(3,
           div(style="margin-left:10px; font-size:14px;",
               h4("Filters"),
               radioButtons("team_perspective", "Offense/Defense:",
                            choices = c("Offense","Defense"),
                            selected="Offense", inline=TRUE),
               selectInput("season","Season(s)",
                           choices=sort(unique(drive_data$season)),
                           selected=max(drive_data$season), multiple=TRUE),
               sliderInput("reg_weeks","Reg Weeks",
                           min=0, max=18, value=c(1,18), step=2),
               selectInput("post_weeks","Post Weeks",
                           choices=c("None","WC","DIV","CONF","SB"), selected="None"),
               checkboxGroupInput("quarters","Quarters:",
                                  choices=c("1"=1,"2"=2,"3"=3,"4"=4,"OT"=5),
                                  selected=c(1,2,3,4,5), inline=TRUE),
               selectInput("posteam","Offense Team", choices=all_teams,
                           selected=NULL, multiple=TRUE),
               selectInput("defteam","Defense Team", choices=all_teams,
                           selected=NULL, multiple=TRUE),
               selectInput("drive_result","Drive Result",
                           choices=sort(unique(drive_data$drive_result)),
                           selected=NULL, multiple=TRUE),
               selectInput("score_state","Score State",
                           choices=c("Any","Leading","Trailing","Tied"),
                           selected="Any"),
               sliderInput("wp_range","Win Probability Range",
                           min=0,max=1,value=c(0,1),step=0.05),
               sliderInput("sack_range","# Sacks in Drive",
                           min=0,max=5,value=c(0,5)),
               sliderInput("explosive_range","Explosive Plays in Drive",
                           min=0,max=5,value=c(0,5)),
               sliderInput("start_fp_range","Starting Field Position",
                           min=min(drive_data$StartFP, na.rm=TRUE),
                           max=max(drive_data$StartFP, na.rm=TRUE),
                           value=c(min(drive_data$StartFP, na.rm=TRUE),
                                   max(drive_data$StartFP, na.rm=TRUE)))
           )
    ),
    column(9,
           tabsetPanel(
             tabPanel("Drive Summary", DTOutput("summary_tbl")),
             tabPanel("Drive Outcomes", DTOutput("outcome_tbl")),
             tabPanel("Detailed Drives", DTOutput("drive_tbl"))
           )
    )
  )
)

# =======================================================
# SERVER
# =======================================================
server <- function(input, output, session){
  
  filtered <- reactive({
    dat <- drive_data
    if(length(input$season)) dat <- dat %>% filter(season %in% input$season)
    if(length(input$posteam)) dat <- dat %>% filter(posteam %in% input$posteam)
    if(length(input$defteam)) dat <- dat %>% filter(defteam %in% input$defteam)
    if(length(input$drive_result)) dat <- dat %>% filter(drive_result %in% input$drive_result)
    
    if(input$post_weeks != "None"){
      wk_map <- c("WC"=19,"DIV"=20,"CONF"=21,"SB"=22)
      dat <- dat %>% filter(week == wk_map[[input$post_weeks]])
    } else {
      dat <- dat %>% filter(week >= input$reg_weeks[1], week <= input$reg_weeks[2])
    }
    
    if(length(input$quarters)) {
      dat <- dat %>% filter(StartQtr %in% as.numeric(input$quarters))
    }
    
    if (input$score_state == "Leading") {
      dat <- dat %>% filter(StartScoreDiff > 0)
    } else if (input$score_state == "Trailing") {
      dat <- dat %>% filter(StartScoreDiff < 0)
    } else if (input$score_state == "Tied") {
      dat <- dat %>% filter(StartScoreDiff == 0)
    }
    
    dat <- dat %>%
      filter(!(MinWP > input$wp_range[2] | MaxWP < input$wp_range[1])) %>%
      filter(Sacks >= input$sack_range[1], Sacks <= input$sack_range[2],
             ExplosivePlays >= input$explosive_range[1], ExplosivePlays <= input$explosive_range[2],
             StartFP >= input$start_fp_range[1], StartFP <= input$start_fp_range[2])
    
    dat
  })
  
  # Drive Summary
  output$summary_tbl <- renderDT({
    dat <- filtered()
    
    if (input$team_perspective == "Offense") {
      agg <- dat %>%
        group_by(Team = posteam) %>%
        summarise(
          Drives = n(),
          `Points/Dr` = mean(PointsPerDrive, na.rm=TRUE),
          `EPA/Play/Dr` = mean(EPA_per_Play, na.rm=TRUE),
          `WPA/Dr` = mean(WPA_per_Drive, na.rm=TRUE),
          `Time/Dr` = safe_time(mean(DriveTimeSec, na.rm=TRUE)),
          `Plays/Dr` = mean(Plays, na.rm=TRUE),
          `Sacks/Dr` = mean(Sacks, na.rm=TRUE),
          `Expl/Dr` = mean(ExplosivePlays, na.rm=TRUE),
          `Starting FP` = mean(StartFP, na.rm=TRUE),
          PROE = mean(PROE, na.rm=TRUE),
          .groups="drop"
        ) %>% arrange(desc(`Points/Dr`))
    } else {
      agg <- dat %>%
        group_by(Team = defteam) %>%
        summarise(
          Drives = n(),
          `Points/Dr` = mean(PointsPerDrive, na.rm=TRUE),
          `EPA/Play/Dr` = mean(EPA_per_Play, na.rm=TRUE),
          `WPA/Dr` = mean(WPA_per_Drive, na.rm=TRUE),
          `Time/Dr` = safe_time(mean(DriveTimeSec, na.rm=TRUE)),
          `Plays/Dr` = mean(Plays, na.rm=TRUE),
          `Sacks/Dr` = mean(Sacks, na.rm=TRUE),
          `Expl/Dr` = mean(ExplosivePlays, na.rm=TRUE),
          `Starting FP` = mean(StartFP, na.rm=TRUE),
          PROE = mean(PROE, na.rm=TRUE),
          .groups="drop"
        ) %>% arrange(`Points/Dr`)
    }
    
    agg <- agg %>%
      mutate(Rk = row_number()) %>%
      select(Rk, everything())
    
    datatable(agg, rownames=FALSE, options=list(
      dom="t", pageLength=nrow(agg), ordering=TRUE
    )) %>%
      formatRound(columns=3:ncol(agg), digits=3) %>%
      formatRound(columns="Drives", digits=0)
  })
  
  # Drive Outcomes
  output$outcome_tbl <- renderDT({
    dat <- filtered()
    out <- dat %>%
      count(`Drive Result` = drive_result) %>%
      mutate(`# Drives` = n,
             Pct = n/sum(n)) %>%
      select(`Drive Result`, `# Drives`, Pct)
    
    datatable(out, rownames=FALSE, options=list(
      dom="t", pageLength=nrow(out), ordering=TRUE
    )) %>%
      formatPercentage("Pct",3)
  })
  
  # Detailed Drives
  output$drive_tbl <- renderDT({
    dat <- filtered() %>%
      select(Season = season,
             Offense = posteam,
             Defense = defteam,
             Drive = drive_num,
             `Drive Result` = drive_result,
             Plays,
             `Points/Dr` = PointsPerDrive,
             `Time/Dr`,
             `EPA/Play/Dr` = EPA_per_Play,
             `WPA/Dr` = WPA_per_Drive,
             `Sacks/Dr` = Sacks,
             `Expl/Dr` = ExplosivePlays,
             `Starting FP` = StartFP,
             PROE)
    
    datatable(
      dat,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = TRUE,
        pageLength = nrow(dat)
      )
    ) %>%
      formatRound(c("Points/Dr","EPA/Play/Dr","WPA/Dr",
                    "Sacks/Dr","Expl/Dr","Starting FP","PROE"),3)
  })
  
}

shinyApp(ui, server)