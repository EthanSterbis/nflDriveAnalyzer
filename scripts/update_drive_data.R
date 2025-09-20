library(dplyr)
library(readr)
library(nflreadr)

# ========= CONFIG =========
seasons <- 2023:2025
out_file <- "data/drive_data.csv"

# ========= LOAD DATA =========
pbp <- nflreadr::load_pbp(seasons = seasons)

# ========= SAFE HELPERS =========
safe_min <- function(x) {
  out <- suppressWarnings(min(x, na.rm = TRUE))
  if (!is.finite(out)) NA_real_ else out
}
safe_max <- function(x) {
  out <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(out)) NA_real_ else out
}
safe_last <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  tail(x, 1)
}
safe_first_nonNA <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  head(x, 1)
}
safe_time <- function(sec) {
  ifelse(!is.finite(sec) | is.na(sec),
         NA_character_,
         sprintf("%02d:%02d", floor(sec / 60), round(sec %% 60)))
}

# ========= DRIVE AGGREGATES =========
drive_data <- pbp %>%
  filter(!is.na(posteam), !is.na(defteam)) %>%
  group_by(drive_id = paste(game_id, drive, posteam, sep = "_")) %>%
  summarise(
    season        = first(season),
    season_type   = first(season_type),
    week          = first(week),
    game_id       = first(game_id),
    drive_num     = first(drive),
    posteam       = first(posteam),
    defteam       = first(defteam),
    drive_result  = first(fixed_drive_result),
    
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
    DriveTimeSec   = safe_max(game_seconds_remaining) - safe_min(game_seconds_remaining),
    
    StartFP        = safe_first_nonNA(yardline_100[!is.na(down)]),
    
    EndFP          = ifelse(
      drive_result == "Touchdown",
      0,
      safe_last(yardline_100)
    ),
    
    YardsPerDrive  = ifelse(
      !is.na(StartFP) & !is.na(EndFP),
      StartFP - EndFP,
      NA_real_
    ),
    
    PctYardsDrive  = ifelse(
      !is.na(StartFP) & StartFP > 0,
      round((StartFP - EndFP) / StartFP * 100, 1),
      NA_real_
    ),
    
    Sacks          = sum(sack == 1, na.rm=TRUE),
    ExplosivePlays = sum((pass == 1 & yards_gained >= 15) |
                           (rush == 1 & yards_gained >= 10), na.rm=TRUE),
    PROE           = mean(pass_oe, na.rm=TRUE),
    
    MinWP = safe_min(wp),
    MaxWP = safe_max(wp),
    StartScoreDiff = first(posteam_score) - first(defteam_score),
    StartQtr = first(qtr),
    StartGC = first(time),
    .groups = "drop"
  ) %>%
  mutate(`Time/Dr` = safe_time(DriveTimeSec))

# ========= SAVE =========
write_csv(drive_data, out_file)

message("Drive data saved to: ", out_file)