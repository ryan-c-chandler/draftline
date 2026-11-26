# Prompts
library(nascaR.data)
library(tidyverse)
library(jsonlite)

setwd("C:/Users/ryanc/OneDrive/Desktop/Draftline")

# Get Data
cup_data <- load_series("cup")
nxs_data <- load_series("nxs")
truck_data <- load_series("truck")

# Clean Data

clean_surfaces <- function(df) {
  df |>
    mutate(
      Surface = case_when(
        Track == "Naval Base Coronado" & is.na(Surface) ~ "street",
        Track == "Daytona International Speedway Road Course" ~ "roval",
        Track == "Portland Speedway" & is.na(Surface) ~ "road",
        Track == "Charlotte Motor Speedway Road Course" ~ "roval",
        Track == "St. Petersburg Street Course" & is.na(Surface) ~ "street",
        Track == "Chicago Street Course" ~ "street",
        Track == "Indianapolis Motor Speedway Road Course" ~ "roval",
        is.na(Surface)                                        ~ "Paved",
        TRUE                                                  ~ Surface
      )
    )
}
cup_data <- clean_surfaces(cup_data)
nxs_data <- clean_surfaces(nxs_data)
truck_data <- clean_surfaces(truck_data)

# Save Data
saveRDS(cup_data, "cup_series.rds")
saveRDS(nxs_data, "nxs.rds")
saveRDS(truck_data, "truck_series.rds")

combined_data <- rbind(cup, nxs, truck)
saveRDS(combined_data, "all_series.rds")

# Read data
cup <- readRDS("cup_series.rds")
nxs <- readRDS("nxs.rds")
truck <- readRDS("truck_series.rds")
all <- readRDS("all_series.rds")

# Global Driver Fame/Obscurity Lookup (across all 3 national series)
driver_fame <- bind_rows(
  select(cup, Driver),
  select(nxs, Driver),
  select(truck, Driver)
) |>
  filter(!is.na(Driver), Driver != "") |>
  count(Driver, name = "career_starts")

# Helper: Auto-generate Jr / Sr / II / III / IV alias variants for any driver name
generate_suffix_aliases <- function(name) {
  clean <- str_trim(name)
  aliases <- character(0)
  
  # Match "Jr", "Jr.", ", Jr", ", Jr."
  if (str_detect(clean, "(?i),?\\s+jr\\.?$")) {
    base <- str_remove(clean, "(?i),?\\s+jr\\.?$")
    aliases <- c(aliases, base, paste0(base, " Jr"), paste0(base, " Jr."))
  }
  
  # Match "Sr", "Sr.", ", Sr", ", Sr."
  if (str_detect(clean, "(?i),?\\s+sr\\.?$")) {
    base <- str_remove(clean, "(?i),?\\s+sr\\.?$")
    aliases <- c(aliases, base, paste0(base, " Sr"), paste0(base, " Sr."))
  }
  
  # Match generational Roman numerals ("II", "III", "IV")
  if (str_detect(clean, "\\s+(II|III|IV)$")) {
    base <- str_remove(clean, "\\s+(II|III|IV)$")
    aliases <- c(aliases, base)
  }
  
  # Remove exact self-match and duplicates
  setdiff(unique(aliases), clean)
}

# Prompt Builder with Automatic Tie-Breaking & Suffix Aliases
build_prompt <- function(df, id, category, prompt_text, target_col = "Driver",
                         min_count = 1, fame_df = driver_fame, aliases_map = list()) {
  # Normalize alias map keys (strip periods/commas so "Martin Truex Jr." matches "Martin Truex Jr")
  if (length(aliases_map) > 0) {
    names(aliases_map) <- str_remove_all(names(aliases_map), "[.,]") |> str_squish()
  }
  
  # Count occurrences (e.g., wins, starts, poles) per unique entity
  summary_df <- df |>
    count(entity = .data[[target_col]], name = "n") |>
    filter(n >= min_count, !is.na(entity), entity != "")
  
  # Break ties on 'n' using total career starts (fewer starts = higher rarity)
  if (target_col == "Driver" && !is.null(fame_df)) {
    summary_df <- summary_df |>
      left_join(fame_df, by = c("entity" = "Driver")) |>
      mutate(career_starts = replace_na(career_starts, 0)) |>
      arrange(n, career_starts)
  } else {
    summary_df <- summary_df |>
      arrange(n)
  }
  
  summary_df <- summary_df |>
    mutate(
      # Assign rarity by percentile rank across sorted rows (rarest = 100 pts, most common = 10 pts)
      pct = percent_rank(row_number()),
      score = case_when(
        pct <= 0.10 ~ 100, # Photo Finish (rarest 10%)
        pct <= 0.20 ~ 85,  # Pole Position (10% - 20%)
        pct <= 0.40 ~ 70,  # Draft Master (20% - 40%)
        pct <= 0.75 ~ 50,  # Lead Lap (40% - 75%)
        pct <= 0.90 ~ 30,  # Mid-Pack (75% - 90%)
        TRUE        ~ 10   # Start and Park (most common 10%)
      )
    )
  
  # Sort output JSON from most common to rarest for clean inspection
  if ("career_starts" %in% names(summary_df)) {
    summary_df <- arrange(summary_df, desc(n), desc(career_starts))
  } else {
    summary_df <- arrange(summary_df, desc(n))
  }
  
  list(
    id = id,
    category = category,
    prompt = prompt_text,
    answers = map(seq_len(nrow(summary_df)), function(i) {
      ans <- summary_df$entity[i]
      lookup_key <- str_remove_all(ans, "[.,]") |> str_squish()
      
      # Combine manual nickname aliases + automatic Jr/Sr/II/III aliases
      manual_aliases <- aliases_map[[lookup_key]] %||% character(0)
      auto_aliases <- if (target_col == "Driver") generate_suffix_aliases(ans) else character(0)
      combined_aliases <- unique(c(manual_aliases, auto_aliases))
      
      list(
        name = ans,
        score = summary_df$score[i],
        aliases = as.list(combined_aliases)
      )
    })
  )
}

# Global alias dictionary applied automatically across any prompt (nicknames only—Jr/Sr are automatic!)
driver_aliases <- list(
  "Dale Earnhardt" = c("The Intimidator", "Dale Earnhardt Sr", "Dale Sr"),
  "Richard Petty"  = c("The King"),
  "Dale Earnhardt Jr" = c("Dale Jr", "Junior"),
  "David Pearson" = c("Silver Fox"),
  "Jimmie Johnson" = c("Seven Time", "JJ"),
  "Jeff Gordon" = c("JG24", "Rainbow Warrior", "Wonderboy"),
  "Kyle Larson" = c("Yung Money", "Tiny Kyle"),
  "Joey Logano" = c("Sliced Bread"),
  "Chase Elliott" = c("William Clyde", "Awesome Chase"),
  "Kyle Busch" = c("The GOAT", "The Goat", "Goat", "Rowdy", "The Candyman", "KFB"),
  "Tony Stewart" = c("Smoke"),
  "Kevin Harvick" = c("The Closer", "Happy Harvick"),
  "Martin Truex Jr" = c("MTJ", "Stage Winner"),
  "Ryan Blaney" = c("YRB", "RFB", "Hocevar's Punching Bag"),
  "Brad Keselowski" = c("Bad Brad"),
  "Kurt Busch" = c("Outlaw"),
  "Darrell Waltrip" = c("DW", "Jaws"),
  "Bill Elliott" = c("Awesome Bill", "Million Dollar Bill"),
  "Matt Kenseth" = c("Max Kansas"),
  "Dale Jarrett" = c("DJ"),
  "Terry Labonte" = c("Texas Terry", "The Iceman"),
  "Ned Jarrett" = c("Gentleman Ned"),
  "Joe Nemechek" = c("Front Row Joe"),
  "Jeff Burton" = c("The Mayor"),
  "Carson Hocevar" = c("Hurricane", "The Hurricane"),
  "Jamie McMurray" = c("Little Jamie Baby", "JM26"),
  "Ryan Newman" = c("Rocket Man", "Rocketman"),
  "Ricky Stenhouse Jr" = c("Wrecky Spinhouse"),
  "AJ Allmendinger" = c("The Dinger", "Dinger"),
  "Shane van Gisbergen" = c("SVG"),
  "Alex Bowman" = c("Bowman the Showman", "The Showman"),
  "Brian Vickers" = c("redacted"),
  "Carl Edwards" = c("Cousin Carl")
)

# A. Auto-generate 1 prompt per major track (e.g., 30+ prompts in 10 lines)
major_tracks <- cup |>
  filter(Finish == 1) |>
  count(Track) |>
  filter(n >= 15) |> # Only tracks with 15+ Cup races
  pull(Track)

track_prompts <- map(major_tracks, function(trk) {
  build_prompt(
    df = filter(cup, Track == trk, Finish == 1),
    id = paste0("win_", gsub("[^a-zA-Z0-9]", "_", tolower(trk))),
    category = "TRACK WINNERS",
    prompt_text = paste("Name a driver who has won a Cup Series race at", trk),
    aliases_map = driver_aliases
  )
})

# B. Auto-generate 1 prompt per iconic car number
iconic_nums_cup <- c("2", "3", "8", "9", "11", "17", "18", "21", "24", "43", "47", "48", "71", "77", "88", "99")

number_prompts <- map(iconic_nums_cup, function(num) {
  build_prompt(
    df = filter(cup, Car == num),
    id = paste0("car_num_", num),
    category = "CAR NUMBERS",
    prompt_text = paste0("Name a driver who has started a Cup Series race in the #", num, " car"),
    aliases_map = driver_aliases
  )
})

# C. Win in a certain season (all series)

# Cup
cup_years <- seq(from = 1949, to = 2026)

year_prompts_cup <- map(cup_years, function(year) {
  build_prompt(
    df = filter(cup, Season == year, Finish == 1),
    id = paste0("cup_win_", year),
    category = "SEASON WINNERS",
    prompt_text = paste0("Name a driver who won a Cup Series race in the ", year, " season"),
    aliases_map = driver_aliases
  )
})

# O'Reilly
nxs_years <- seq(from = 1982, to = 2026)

year_prompts_nxs <- map(nxs_years, function(year) {
  build_prompt(
    df = filter(nxs, Season == year, Finish == 1),
    id = paste0("nxs_win_", year),
    category = "SEASON WINNERS",
    prompt_text = paste0("Name a driver who won an O'Reilly Series race in the ", year, " season"),
    aliases_map = driver_aliases
  )
})

# Truck
truck_years <- seq(from = 1995, to = 2026)

year_prompts_truck <- map(truck_years, function(year) {
  build_prompt(
    df = filter(truck, Season == year, Finish == 1),
    id = paste0("truck_win_", year),
    category = "SEASON WINNERS",
    prompt_text = paste0("Name a driver who won a Truck Series race in the ", year, " season"),
    aliases_map = driver_aliases
  )
})

# D. Custom one-off stat prompts (poles, manufacturers, multi-series winners, etc.)
custom_prompts <- list(
  # Existing prompts
  build_prompt(filter(cup, Start == 1), "cup_poles", "QUALIFYING", "Name a driver who has won a NASCAR Cup Series pole", aliases_map = driver_aliases),
  build_prompt(filter(cup, Finish == 1, Make == "Pontiac"), "pontiac_wins", "MANUFACTURERS", "Name a driver who won a Cup race driving a Pontiac", aliases_map = driver_aliases),
  build_prompt(filter(cup, Led >= 200), "led_200", "DOMINANCE", "Name a driver who led 200+ laps in a single Cup Series race", aliases_map = driver_aliases),
  build_prompt(filter(cup, Finish == 1), "cup_winners", "CUP SERIES", "Name a driver who has won a Cup Series race", aliases_map = driver_aliases),
  build_prompt(filter(nxs, Finish == 1), "nxs_winners", "O'REILLY SERIES", "Name a driver who has won an O'Reilly Series race", aliases_map = driver_aliases),
  build_prompt(filter(truck, Finish == 1), "truck_winners", "TRUCK SERIES", "Name a driver who has won a Craftsman Truck Series race", aliases_map = driver_aliases),
  
  # 1. Career milestone threshold using min_count (e.g., 10+ Cup wins)
  build_prompt(filter(cup, Finish == 1), "cup_10_wins", "MILESTONES", "Name a driver with 10 or more NASCAR Cup Series wins", min_count = 10, aliases_map = driver_aliases),
  
  # 2. Crown Jewel / specific race filter (e.g., Daytona 500 = Race 1 of season)
  build_prompt(filter(cup, Track == "Daytona International Speedway", Race == 1, Finish == 1), "daytona_500", "CROWN JEWELS", "Name a Daytona 500 champion", aliases_map = driver_aliases),
  
  # 3. Grouping by a different column (e.g., Tracks instead of Drivers)
  build_prompt(filter(cup, Season >= 2000), "modern_tracks", "TRACKS", "Name a track that hosted a Cup Series race since 2000", target_col = "Track"),
  
  # 4. Underdog / rare feats (e.g., won a Cup race starting 20th or worse)
  build_prompt(filter(cup, Finish == 1, Start >= 20), "deep_field_win", "COMEBACKS", "Name a driver who won a Cup race after starting 20th or worse", aliases_map = driver_aliases),
  
  # 5. Grouping by Car Owner
  build_prompt(filter(cup, Finish == 1), "winning_teams", "TEAMS", "Name a team with at least 5 Cup Series wins", target_col = "Team", min_count = 5),
  
  # 6. Southern 500 Winners
  build_prompt(filter(cup, Track == "Darlington Raceway", str_detect(Name, regex("Southern 500", ignore_case = TRUE)), Finish == 1), "southern_500", "CROWN JEWELS", "Name a Southern 500 champion", aliases_map = driver_aliases),
  
  # 7. Coke 600 Winners
  build_prompt(filter(cup, Track == "Charlotte Motor Speedway", str_detect(Name, regex("World 600|Coca-Cola.*600", ignore_case = TRUE)), Finish == 1), "coke_600", "CROWN JEWELS", "Name a Coke 600 / World 600 champion", aliases_map = driver_aliases),
  
  # 8. Texas
  build_prompt(filter(cup, Track %in% c("Texas", "Circuit of the Americas"), Finish >= 10, Season >= 2022), "texas", "TEXAS", "Name a driver with a Top 10 in the Cup Series in the state of Texas in the Next Gen era", aliases_map = driver_aliases),
  
  # 9. Pole Winner
  build_prompt(filter(cup, Finish == 1, Start == 1), "pole_win_cup", "DOMINANCE", "Name a driver who won a Cup Series race starting from pole position", aliases_map = driver_aliases),
  
  # 10. Pole Winner NXS
  build_prompt(filter(nxs, Finish == 1, Start == 1), "pole_win_nxs", "DOMINANCE", "Name a driver who won an O'Reilly Series race starting from pole position", aliases_map = driver_aliases),
  
  # 11. Pole Winner Trucks
  build_prompt(filter(truck, Finish == 1, Start == 1), "pole_win_truck", "DOMINANCE", "Name a driver who won a Truck Series race starting from pole position", aliases_map = driver_aliases),
  
  # 12. Race Winner 2026
  build_prompt(filter(all, Finish == 1, Season == 2026), "winner_26", "WINNERS", "Name a driver who won a race in ANY of the top three series in 2026", aliases_map = driver_aliases),
  
  # 13. Race Winner 2025
  build_prompt(filter(all, Finish == 1, Season == 2025), "winner_25", "WINNERS", "Name a driver who won a race in ANY of the top three series in 2025", aliases_map = driver_aliases),
  
  # 14. Roval
  build_prompt(filter(all, Finish == 1, Surface == "roval"), "right_turn1", "ROAD COURSE RINGERS", "Name a driver who has won on a ROVAL in ANY of the top three series", aliases_map = driver_aliases),
  
  # 15. Road Course Ringers Cup
  build_prompt(filter(cup, Finish == 1, Season >= 1989, Surface %in% c("road", "roval", "street")), "right_turn2", "ROAD COURSE RINGERS", "Name a driver who has won a Cup Series race on any road course since 1989", aliases_map = driver_aliases),
  
  # 16. Road Course Ringers NXS
  build_prompt(filter(nxs, Finish == 1, Surface %in% c("road", "roval", "street")), "right_turn3", "ROAD COURSE RINGERS", "Name a driver who has won an O'Reilly Series race on any road course", aliases_map = driver_aliases),
  
  # 17. Road Course Ringers Truck
  build_prompt(filter(truck, Finish == 1, Surface %in% c("road", "roval", "street")), "right_turn4", "ROAD COURSE RINGERS", "Name a driver who has won a Truck Series race on any road course", aliases_map = driver_aliases),
  
  # 18. You can run the perfect race
  build_prompt(filter(all, Rating == 150.0), "perfect1", "YOU CAN RUN THE PERFECT RACE", "Name a driver who ran a race with a perfect driver rating (150.0) in ANY of the top three series (2005 and later)", aliases_map = driver_aliases),
  
  # 19. Denny Hamlin
  build_prompt(filter(cup, Rating >= 130.0, Win == 0), "perfect2", "YOU CAN (almost) RUN THE PERFECT RACE", "Name a driver who ran a Cup Series race with a driver rating above 130.0 and failed to win (2005 and later)", aliases_map = driver_aliases),
  
  # 20. Kyle Busch
  build_prompt(filter(all, Driver == "Kyle Busch", Win == 1), "rowdy", "KYLE BUSCH", "Name a track Kyle Busch has won at in ANY of the top three series", target_col = "Track")
)

# Combine all 50–100+ prompts and export to a single JSON file
all_prompts <- c(track_prompts, number_prompts, year_prompts_cup, year_prompts_nxs, year_prompts_truck, custom_prompts)
write_json(all_prompts, "prompts.json", auto_unbox = TRUE, pretty = TRUE)

# Combine all prompts and convert to a JSON string
all_prompts <- c(track_prompts, number_prompts, year_prompts_cup, year_prompts_nxs, year_prompts_truck, custom_prompts)
json_str <- toJSON(all_prompts, auto_unbox = TRUE, pretty = TRUE)

# 1. Save standalone JSON backup
writeLines(json_str, "prompts.json")

# 2. Read baseline template.txt and inject the latest prompts + scores
if (!file.exists("template.txt")) {
  stop("Missing 'template.txt' in your working directory!")
}

html_template <- paste(readLines("template.txt", warn = FALSE), collapse = "\n")
safe_json_str <- gsub("\\$", "\\\\$", json_str)

updated_html <- sub(
  "(?s)const PROMPTS_DB = \\[.*?\\];",
  paste0("const PROMPTS_DB = ", safe_json_str, ";"),
  html_template,
  perl = TRUE
)

# 3. Write both the ready-to-upload GitHub file AND an updated .txt copy
writeLines(updated_html, "index.html")
writeLines(updated_html, "latest_code.txt")

message("Yo, Mista White, 'index.html' and 'latest_code.txt' updated yo with like ", length(all_prompts), " prompts yo.")


