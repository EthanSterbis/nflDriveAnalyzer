# Sterb's NFL Drive Analyzer
# @EthanSterbis on X

library(shiny)
library(dplyr)
library(readr)
library(bslib)
library(DT)

# =======================================================
# Load Precomputed Drive Data
# =======================================================
drive_data <- read_csv("data/drive_data.csv", show_col_types = FALSE)

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
      table.dataTable thead th {
        text-align: center !important;
      }
      table.dataTable td {
        font-size: 14px;
        text-align: center;
      }
      table.dataTable th {
        font-size: 12px;
        text-align: center !important;
      }
    "))
  ),
  h2("Sterb's NFL Drive Analyzer", style="padding-top:10px; font-size:22px;"),
  fluidRow(
    column(
      width = 3,  # slightly narrower filter column
      div(
        style="margin-left:10px; margin-right:5px; font-size:14px;",
        h4("Filters"),
        radioButtons(
          "team_perspective", "Offense/Defense:",
          choices = c("Offense","Defense"),
          selected="Offense", inline=TRUE
        ),
        selectInput(
          "season","Season(s)",
          choices=sort(unique(drive_data$season)),
          selected=max(drive_data$season), multiple=TRUE
        ),
        sliderInput("reg_weeks","Reg Weeks",
                    min=1, max=18, value=c(1,18), step=1),  # allow individual weeks
        selectInput("post_weeks","Post Weeks",
                    choices=c("None","WC","DIV","CONF","SB"), selected="None"),
        checkboxGroupInput(
          "quarters","Quarters:",
          choices=c("1"=1,"2"=2,"3"=3,"4"=4,"OT"=5),
          selected=c(1,2,3,4,5), inline=TRUE
        ),
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
    column(
      width = 9,  # slightly wider table column
      style="margin-left:-50px;",  # nudge table area left
      tabsetPanel(
        tabPanel("Summary", DTOutput("summary_tbl")),
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
    
    # Postseason mapping
    if(input$post_weeks != "None"){
      wk_map <- c("WC"=19,"DIV"=20,"CONF"=21,"SB"=22)
      dat <- dat %>% filter(week == wk_map[[input$post_weeks]])
    } else {
      dat <- dat %>% filter(week >= input$reg_weeks[1], week <= input$reg_weeks[2])
    }
    
    # Quarter filter
    if(length(input$quarters)) {
      dat <- dat %>% filter(StartQtr %in% as.numeric(input$quarters))
    }
    
    # Score state
    if (input$score_state == "Leading") {
      dat <- dat %>% filter(StartScoreDiff > 0)
    } else if (input$score_state == "Trailing") {
      dat <- dat %>% filter(StartScoreDiff < 0)
    } else if (input$score_state == "Tied") {
      dat <- dat %>% filter(StartScoreDiff == 0)
    }
    
    # Win probability filter
    dat <- dat %>%
      filter(!(MinWP > input$wp_range[2] | MaxWP < input$wp_range[1]))
    
    # Numeric filters
    dat <- dat %>%
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
          `Points/Dr`   = mean(PointsPerDrive, na.rm=TRUE),
          `EPA/Play/Dr` = mean(EPA_per_Play, na.rm=TRUE),
          `WPA/Dr`      = mean(WPA_per_Drive, na.rm=TRUE),
          `Time/Dr`     = mean(DriveTimeSec, na.rm=TRUE),   # keep raw seconds
          `Plays/Dr`    = mean(Plays, na.rm=TRUE),
          `Yds/Dr`      = mean(YardsPerDrive, na.rm=TRUE),
          `PctYds/Dr`   = mean(PctYardsDrive, na.rm=TRUE),
          `Sacks/Dr`    = mean(Sacks, na.rm=TRUE),
          `Expl/Dr`     = mean(ExplosivePlays, na.rm=TRUE),
          `Starting FP` = mean(StartFP, na.rm=TRUE),
          `EndFP`       = mean(EndFP, na.rm=TRUE),
          PROE          = mean(PROE, na.rm=TRUE),
          .groups="drop"
        ) %>%
        # Convert seconds -> MM:SS string
        mutate(`Time/Dr` = sprintf("%02d:%02d", floor(`Time/Dr`/60), round(`Time/Dr` %% 60))) %>%
        arrange(desc(`Points/Dr`))
    } else {
      agg <- dat %>%
        group_by(Team = defteam) %>%
        summarise(
          Drives = n(),
          `Points/Dr`   = mean(PointsPerDrive, na.rm=TRUE),
          `EPA/Play/Dr` = mean(EPA_per_Play, na.rm=TRUE),
          `WPA/Dr`      = mean(WPA_per_Drive, na.rm=TRUE),
          `Time/Dr`     = mean(DriveTimeSec, na.rm=TRUE),
          `Plays/Dr`    = mean(Plays, na.rm=TRUE),
          `Yds/Dr`      = mean(YardsPerDrive, na.rm=TRUE),
          `PctYds/Dr`   = mean(PctYardsDrive, na.rm=TRUE),
          `Sacks/Dr`    = mean(Sacks, na.rm=TRUE),
          `Expl/Dr`     = mean(ExplosivePlays, na.rm=TRUE),
          `Starting FP` = mean(StartFP, na.rm=TRUE),
          `EndFP`       = mean(EndFP, na.rm=TRUE),
          PROE          = mean(PROE, na.rm=TRUE),
          .groups="drop"
        ) %>%
        mutate(`Time/Dr` = sprintf("%02d:%02d", floor(`Time/Dr`/60), round(`Time/Dr` %% 60))) %>%
        arrange(`Points/Dr`)
    }
    
    agg <- agg %>%
      mutate(Rk = row_number()) %>%
      select(Rk, everything())
    
    datatable(
      agg,
      rownames=FALSE,
      options=list(
        dom="t", pageLength=nrow(agg), ordering=TRUE
      )
    ) %>%
      formatRound(columns=c("Points/Dr","EPA/Play/Dr","WPA/Dr","Plays/Dr",
                            "Yds/Dr","PctYds/Dr","Sacks/Dr","Expl/Dr","Starting FP","EndFP","PROE"), digits=3) %>%
      formatRound(columns="Drives", digits=0) %>%
      formatStyle(columns=names(agg), `text-align`="center", target="cell")
  })
  
  output$outcome_tbl <- renderDT({
    dat <- filtered()
    out <- dat %>%
      count(`Drive Result` = drive_result) %>%
      mutate(`# Drives` = n,
             Pct = n/sum(n)) %>%
      select(`Drive Result`, `# Drives`, Pct)
    
    datatable(
      out,
      rownames=FALSE,
      options=list(
        dom="t", 
        pageLength=nrow(out), 
        ordering=TRUE
      ),
      class = "cell-border stripe hover"
    ) %>%
      formatPercentage("Pct",3) %>%
      formatStyle(columns=names(out), 
                  `text-align`="center", 
                  target="cell")
  })
  
  # Detailed Drives
  output$drive_tbl <- renderDT({
    dat <- filtered() %>%
      mutate(`Time/Dr` = sub(":00$", "", `Time/Dr`)) %>%
      mutate(`StartGC` = sub(":00$", "", `StartGC`)) %>%
      arrange(
        season,
        week,
        posteam,
        StartQtr,
        desc(StartGC)
      ) %>%
      select(
        Season = season,
        Week = week,
        Offense = posteam,
        Defense = defteam,
        Qtr = StartQtr,
        GC = StartGC,
        Drive = drive_num,
        `Drive Result` = drive_result,
        Plays,
        `Points/Dr` = PointsPerDrive,
        `Time/Dr`,
        `EPA/Play/Dr` = EPA_per_Play,
        `WPA/Dr` = WPA_per_Drive,
        `Yds/Dr` = YardsPerDrive,
        `PctYds/Dr` = PctYardsDrive,
        `Sacks/Dr` = Sacks,
        `Expl/Dr` = ExplosivePlays,
        `Starting FP` = StartFP,
        `EndFP`,
        PROE
      )
    
    datatable(
      dat,
      rownames = FALSE,
      options = list(
        dom="t",
        paging=FALSE,
        ordering=TRUE,
        pageLength=nrow(dat),
        lengthChange=FALSE
      )
    ) %>%
      formatRound(c("Points/Dr","EPA/Play/Dr","WPA/Dr","Yds/Dr","PctYds/Dr",
                    "Sacks/Dr","Expl/Dr","Starting FP","EndFP","PROE"),3) %>%
      formatStyle(columns=names(dat), `text-align`="center", target="cell")
  })
}

shinyApp(ui, server)