## ----run this on RMarkdown document to get extract below-----------------
#knitr::purl("NCAA March MadnessV9.Rmd", "NCAAMM RMD Code Extract.R")


## ----load_libraries, message=FALSE, warning=FALSE------------------------
library(tidyverse)
library(lubridate)

## ----set_data_dir--------------------------------------------------------
data_dir <- "input"

## ----load_data1, message = "FALSE"---------------------------------------
tourney_seeds <- read_csv(paste0(data_dir, "/TourneySeeds.csv")) %>%
  rename(season = Season) %>%
  rename(seed = Seed) %>%
  rename(team = Team)

## ----load_data2, message = "FALSE"---------------------------------------
seasons <- read_csv(paste0(data_dir, "/Seasons.csv")) %>%
  mutate(day_zero = mdy(Dayzero)) %>%
  rename(season = Season) %>%
  select(season, day_zero)
# Dayzero indicates the baseline date that is referenced in other data sets

## ----load_data3, message=TRUE--------------------------------------------
tourney_results <- read_csv(paste0(data_dir, "/TourneyCompactResults.csv")) %>%
  rename(season = Season) %>%
  rename(day_num = Daynum) %>%
  rename(winning_team = Wteam) %>%
  select(season, day_num, winning_team)

## ----build_round_structure-----------------------------------------------
round_df <- tibble(round = c(rep(1, 32), rep(2, 16), rep(3, 8), rep(4, 4), rep(5, 2), 6))
unlist(round_df$round)
length(round_df)

## ----get_all_results_function, cache = TRUE------------------------------
## Sequence through seasons generating 4 bracket dataframes for each season
# The id is assigned based on year and bracket that it came from

get_all_results <- function(return_type = c("df", "list"), 
                            rounds = c("all", "1-4", "5-6")) {
  tc_w_seeds_list <- list()
  for (l in 1:(nrow(seasons) - 1)) {
    # Get the season and day_zero to work with in this loop
    this_season <- unlist(seasons[l, 1])
    this_day_zero <- seasons[l, 2]
    
    # Add date-played
    tc_w_date_played <- tourney_results %>%
      filter(season == this_season) %>%
      mutate(date_played = this_day_zero$day_zero + day_num)
    
    # Which teams were assigned to each seed this season?
    this_season_seeds <- tourney_seeds %>%
      filter(season == this_season)
    
    # Join seeds to main data
    tc_w_region_seeds <- tc_w_date_played %>%
      left_join(this_season_seeds, by = c("winning_team" = "team")) %>%
      select(-season.y) %>%
      rename(season = season.x) %>%
      rename(region_seed = seed) %>%
      select(season, date_played, region_seed)
    
    # Some preliminary games are shown in the data.
    # These are always the first games and are insignificant to our modelling.
    # Remove preliminary games
    t <- nrow(tc_w_region_seeds) - 63
    if (t > 0) {
      s <- seq(1, t)
      tc_prelim_trim <- tc_w_region_seeds[-s,]
    } else {
      tc_prelim_trim <- tc_w_region_seeds
    }
    
    # Some teams from the prelim games won a game in the main tourney.
    # They are indicated by a suffix of a or b.
    # Remove the suffix because no other seeds with that ID will appear.
    tc_w_suffix_trimmed <- tc_prelim_trim %>%
      mutate(region_seed = str_replace(region_seed, "[ab]", ""))
    
    # Sort the dates, lowest to highest, and bind the round numbers to the df
    tc_w_rounds <- tc_w_suffix_trimmed %>%
      bind_cols(round_df) %>%
      select(season, round, region_seed)
    
    # Break out the region and rename to A, B, C, and D
    tc_w_regions <- tc_w_rounds %>%
      mutate(region_seed = str_replace_all(region_seed, c(
        "W" = "A",
        "X" = "B",
        "Y" = "C",
        "Z" = "D"
      ))) %>%
      mutate(region = str_extract(region_seed, "[ABCD]"))
    
    # Break out the seed and drop region_seed
    # We'll add region_seed back in a better format
    tc_w_seeds <- tc_w_regions %>%
      mutate(seed = str_extract(region_seed, "[01][0-9]")) %>%
      mutate(seed = as.integer(seed)) %>%
      select(-region_seed) %>%
      mutate(region_seed = paste0(region, "-", seed))
    
    # Filter based on rounds selected
    if(rounds == "all") {
      tc_w_seeds_filtered <- tc_w_seeds 
    } else if(rounds == "1-4") {
      tc_w_seeds_filtered <- tc_w_seeds %>%
        filter(round <= 4)
    } else if(rounds == "5-6") {
      tc_w_seeds_filtered <- tc_w_seeds %>%
        filter(round >= 5)
    } else {
      print("Please include the rounds parameter of all, 1-4, or 5-6")
    }
    
    tc_w_seeds_list[[l]] <- tc_w_seeds_filtered
  }
  
  tc_w_seeds_df <- do.call(rbind, tc_w_seeds_list) 
  
  if(return_type == "df") {
    return(tc_w_seeds_df)
  } else if(return_type == "list") {
    return(tc_w_seeds_list)
  } else {
    print("Please specify a return type of df or list")
  }
  
}


## ----get_reference_season_function---------------------------------------
# Put actuals for a year into same format as predictions
latest_season <- max(seasons$season)
get_reference_season <- function(my_season = 2017) {
  actual_1_4 <- get_all_results(return_type = "df", rounds = "1-4")
  actual_5_6 <- get_all_results(return_type = "df", rounds = "5-6")
  
  final_results <- rbind(actual_1_4, actual_5_6)
  
  this_season <- final_results %>%
    filter(season == my_season)
  
  rounds <- list()
  for (i in 1:6) {
    rounds[[i+1]] <- this_season %>%
      filter(round == i) %>%
      select(region_seed) %>%
      arrange(region_seed) %>%
      unlist()
  }
  rounds[[1]] <- unique(this_season$season)
  for(j in 1:7) {
    attributes(rounds[[j]]) <- NULL
  }
  names(rounds) <- c("id", "round_1", "round_2", "round_3", 
                     "round_4", "round_5", "round_6")
  
  return(rounds)
}


## FUNCTION: create_pure_naive_entries()
create_pure_naive_entries <- function(num_entries = 1) {
  final_entries <- list()
  #=============================
  # ROUND 1 SETUP
  #============================
  # 8 games with team 1 playing team 2, pairwise
  for (n in 1:num_entries) {
    final_df <- tibble()
    
    # These seeds always play each other, pairwise, in the first round.
    team_1_seed <- c(1, 8, 5, 4, 6, 3, 7, 2)
    team_2_seed <- c(16, 9, 12, 13, 11, 14, 10, 15)
    
    # Sequence through regions and generate random winners
    # Generate round 1 winners
    round_results_list <- list()
    this_region <- c(A = 1,
                     B = 2,
                     C = 3,
                     D = 4)
    
    for (r in this_region) {
      winner <- as.integer()
      for (i in 1:8) {
        winner[i] <- sample(c(team_1_seed[i], team_2_seed[i]), 1)
        #print(winner[i])
      }
      round_results_list[[r]] <- tibble(
        round = 1,
        region = names(this_region[r]),
        seed = winner
      )
    }
    round_df <- do.call(rbind, round_results_list)
    final_df <- round_df
    
    #========================
    # SETUP ROUND 2
    #========================
    # In order to traverse the bracket correctly I determine team_1 and team_2 this way...
    
    region_round_list <- list()
    round_results_list <- list()
    for (r in this_region) {
      region_round_list[[r]] <- round_df %>%
        filter(region == names(this_region[r])) %>%
        select(seed) %>%
        unlist()
      
      # Get every other record and assign to team 1 and team 2
      team_1_seed <- region_round_list[[r]][c(1, 3, 5, 7)]
      team_2_seed <- region_round_list[[r]][c(2, 4, 6, 8)]
      
      winner <- as.integer()
      for (i in 1:4) {
        winner[i] <- sample(c(team_1_seed[i], team_2_seed[i]), 1)
      }
      
      #this_round <- rep(2, 4)
      round_results_list[[r]] <- tibble(
        round = 2,
        region = names(this_region[r]),
        seed = winner
      )
    }
    
    round_df <- do.call(rbind, round_results_list)
    final_df <- rbind(final_df, round_df)
    
    #=====================================
    # SETUP ROUND 3
    #====================================
    region_round_list <- list()
    round_results_list <- list()
    for (r in this_region) {
      region_round_list[[r]] <- round_df %>%
        filter(region == names(this_region[r])) %>%
        select(seed) %>%
        unlist()
      
      team_1_seed <- region_round_list[[r]][c(1, 3)]
      team_2_seed <- region_round_list[[r]][c(2, 4)]
      
      winner <- as.integer()
      for (i in 1:2) {
        winner[i] <- sample(c(team_1_seed[i], team_2_seed[i]), 1)
      }
      
      #this_round <- rep(3, 2)
      round_results_list[[r]] <- tibble(
        round = 3,
        region = names(this_region[r]),
        seed = winner
      )
    }
    round_df <- do.call(rbind, round_results_list)
    final_df <- rbind(final_df, round_df)
    
    
    
    #===============================
    # SETUP ROUND 4
    #===============================
    # Determine regional champions
    #this_round <- 4
    region_round_list <- list()
    round_results_list <- list()
    for (r in this_region) {
      region_round_list[[r]] <- round_df %>%
        filter(region == names(this_region[r])) %>%
        select(seed) %>%
        unlist()
      
      team_1_seed <- region_round_list[[r]][1]
      team_2_seed <- region_round_list[[r]][2]
      
      winner <- as.integer()
      winner[r] <- sample(c(team_1_seed, team_2_seed), 1)
      
      round_results_list[[r]] <- tibble(
        round = 4,
        region = names(this_region[r]),
        seed = winner[r]
      )
      
    }
    round_df <- do.call(rbind, round_results_list)
    final_df <- rbind(final_df, round_df)
    
    #=============================================
    # SETUP ROUND 5
    #=============================================
    # Round 5 has the champions of region A & B platying each other
    # and the champions of C & D playing each other.
    
    # We need to append the region to the seed now.
    round_results_list <- list()
    final_df_2 <- final_df %>%
      mutate(region_seed = paste0(region, "-", seed))
    
    ab_game <- final_df_2 %>%
      filter(round == 4) %>%
      filter(region %in% c("A", "B")) %>%
      select(region_seed) %>%
      sample_n(1) %>%
      unlist()
    
    this_seed <- str_replace(ab_game, "^[ABCD]-", "")
    round_results_list[[1]] <- tibble(
      round = 5,
      region = "AB",
      seed = as.integer(str_replace(ab_game, "^[ABCD]-", "")),
      region_seed = ab_game
    )
    cd_game <- final_df_2 %>%
      filter(round == 4) %>%
      filter(region %in% c("C", "D")) %>%
      select(region_seed) %>%
      sample_n(1) %>%
      unlist()
    
    round_results_list[[2]] <- tibble(
      round = 5,
      region = "CD",
      seed = as.integer(str_replace(cd_game, "^[ABCD]-", "")),
      region_seed = cd_game
    )
    
    round_df <- do.call(rbind, round_results_list)
    final_df_2 <- rbind(final_df_2, round_df)
    
    #===========================
    # SETUP ROUND 6
    #===========================
    round_results_list <- list()
    abcd_game <- final_df_2 %>%
      filter(round == 5) %>%
      select(region_seed) %>%
      sample_n(1) %>%
      unlist()
    
    round_results_list[[1]] <- tibble(
      round = 6,
      region = "ABCD",
      seed = as.integer(str_replace(abcd_game, "^[ABCD]-", "")),
      region_seed = ab_game
    )
    
    round_df <- do.call(rbind, round_results_list)
    final_df_2 <- rbind(final_df_2, round_df)
    
    #==================================
    # CONVERT TO FORMAT USED BY SCORER
    #==================================
    # entry_num = n
    #final_entry <- list()
    round_results_list <- list()
    for (i in 1:6) {
      round_results_list[[i]] <- final_df_2 %>%
        filter(round == i) %>%
        select(region_seed) %>%
        unlist()
      
      #print(round_results_list[[i]])
    }
    for (j in 1:6) {
      attributes(round_results_list[[j]]) <- NULL
    }
    
    final_entries[[n]] <- list(
      id = n,
      round_1 = round_results_list[[1]],
      round_2 = round_results_list[[2]],
      round_3 = round_results_list[[3]],
      round_4 = round_results_list[[4]],
      round_5 = round_results_list[[5]],
      round_6 = round_results_list[[6]]
    )
  }
  return(final_entries)
}

## FUNCTION: score_entries()
score_entries <- function(my_entries, reference_season) {
  entry_results_list <- list()
  for (i in 1:length(my_entries)) {
    entry_results_list[[i]] <- tibble(
      entry_num = i,
      rnd_1_pts = length(intersect(
        my_entries[[i]]$round_1, reference_season$round_1
      )),
      rnd_2_pts = length(intersect(
        my_entries[[i]]$round_2, reference_season$round_2
      )) * 2,
      rnd_3_pts = length(intersect(
        my_entries[[i]]$round_3, reference_season$round_3
      )) * 4,
      rnd_4_pts = length(intersect(
        my_entries[[i]]$round_4, reference_season$round_4
      )) * 8, 
      rnd_5_pts = length(intersect(
        my_entries[[i]]$round_5, reference_season$round_5
      )) * 16,
      rnd_6_pts = length(intersect(
        my_entries[[i]]$round_6, reference_season$round_6
      )) * 32, 
      total_points = sum(
        rnd_1_pts,
        rnd_2_pts,
        rnd_3_pts,
        rnd_4_pts,
        rnd_5_pts,
        rnd_6_pts
      ))
    entry_results_final <- do.call(rbind, entry_results_list)
  }
  return(entry_results_final)
}

## FUNCTION: create_smart_naive_entries()
create_smart_naive_entries <- function(num_entries = 1) {
  # Every region in rounds 1 - 4 are the same since the smart naive person always
  # selects the lowest seed, because that seed is assumed to have the most strenth
  
  # Setup rounds 1 - 4
  sn_round_1 <- c(paste0("A-", 1:8),
                  paste0("B-", 1:8),
                  paste0("C-", 1:8),
                  paste0("D-", 1:8))
  
  sn_round_2 <-  c(paste0("A-", 1:4),
                   paste0("B-", 1:4),
                   paste0("C-", 1:4),
                   paste0("D-", 1:4))
  
  sn_round_3 <-  c(paste0("A-", 1:2),
                   paste0("B-", 1:2),
                   paste0("C-", 1:2),
                   paste0("D-", 1:2))
  
  sn_round_4 <-  c(paste0("A-", 1),
                   paste0("B-", 1),
                   paste0("C-", 1),
                   paste0("D-", 1))
  
  sn_list <- list()
  for (n in 1:num_entries) {
    sn_list[[n]] <- list(
      id = n,
      round_1 = sn_round_1,
      round_2 = sn_round_2,
      round_3 = sn_round_3,
      round_4 = sn_round_4
    )
    # Randomize round 5; since they are both seed #1
    sn_round_5_ab <- sample(c("A-1", "B-1"), 1)
    sn_round_5_cd <- sample(c("C-1", "D-1"), 1)
    
    sn_list[[n]]$round_5 <- c(sn_round_5_ab, sn_round_5_cd)
    
    # Randomize round 6; since they are both seed #1
    sn_list[[n]]$round_6 <-
      sample(c(sn_round_5_ab, sn_round_5_cd), 1)
    
    
  }
  return(sn_list)
}


## FUNCTION: create_historic_entries()
  create_historic_entries <- function(num_entries = 1) {
    all_results <- get_all_results(return_type = "df", rounds = "1-4")
    
    bracket_list <- list()
    master_list <- list()
    season_list <- list()
    # Create incrementer for master_list
    j <- 1
    for (this_season in unique(all_results$season)) {
      season_results <- all_results %>%
        filter(season == this_season)
      
      for (i in 1:4) {
        if (i == 1) {
          this_region = "A"
        } else if (i == 2) {
          this_region = "B"
        } else if (i == 3) {
          this_region = "C"
        } else if (i == 4) {
          this_region = "D"
        }
        
        region_results <- season_results %>%
          filter(region == this_region)
        
        round_list <- list()
        
        id <- all_results %>%
          filter(season == this_season) %>%
          filter(region == this_region) %>%
          mutate(id = paste0(season, "-", this_region)) %>%
          select(id) %>%
          unique() %>%
          unlist()
        
        round_list[[1]] <- all_results %>%
          filter(season == this_season) %>%
          filter(round == 1) %>%
          filter(region == this_region) %>%
          select(seed) %>%
          arrange(seed)
        
        round_list[[2]] <- all_results %>%
          filter(season == this_season) %>%
          filter(round == 2) %>%
          filter(region == this_region) %>%
          select(seed) %>%
          arrange(seed)
        
        round_list[[3]] <- all_results %>%
          filter(season == this_season) %>%
          filter(round == 3) %>%
          filter(region == this_region) %>%
          select(seed) %>%
          arrange(seed)
        
        round_list[[4]] <- all_results %>%
          filter(season == this_season) %>%
          filter(round == 4) %>%
          filter(region == this_region) %>%
          select(seed) %>%
          arrange(seed)
        
        region_list <- list(
          id = id,
          round_1 = round_list[[1]]$seed,
          round_2 = round_list[[2]]$seed,
          round_3 = round_list[[3]]$seed,
          round_4 = round_list[[4]]$seed
        )
        
        master_list[[j]] <- region_list
        j = j + 1
      }
    }
  
    round_5_6 <- get_all_results(return_type = "df", rounds = "5-6")
    all_seeds <- tibble(seed = 1:16)
    
    round_5_stats <- round_5_6 %>%
      filter(round == 5) %>%
      group_by(seed) %>%
      summarize(number = n()) %>%
      right_join(all_seeds, by = "seed") %>%
      replace_na(list(number = 0))
    
    round_6_stats <- round_5_6 %>%
      filter(round == 6) %>%
      group_by(seed) %>%
      summarize(number = n()) %>%
      right_join(all_seeds, by = "seed") %>%
      replace_na(list(number = 0))
    
    
    #==============================
    # ROUNDS 1-4
    #==============================
    entry_list <- list()
    for (j in 1:num_entries) {
      regional_bracket_list <- list()
      # Get four historic brackets by sampling the master list
      regional_bracket_list <-
        sample(master_list, 4, replace = TRUE)
      
      # For each element we need to append the region code: A, B, C, or D
      
      for (i in 1:4) {
        if (i == 1) {
          this_region = "A"
        } else if (i == 2) {
          this_region = "B"
        } else if (i == 3) {
          this_region = "C"
        } else if (i == 4) {
          this_region = "D"
        }
        
        regional_bracket_list[[i]]$round_1 <- paste0(this_region,
                                                     "-",
                                                     regional_bracket_list[[i]]$round_1)
        regional_bracket_list[[i]]$round_2 <- paste0(this_region,
                                                     "-",
                                                     regional_bracket_list[[i]]$round_2)
        regional_bracket_list[[i]]$round_3 <- paste0(this_region,
                                                     "-",
                                                     regional_bracket_list[[i]]$round_3)
        regional_bracket_list[[i]]$round_4 <- paste0(this_region,
                                                     "-",
                                                     regional_bracket_list[[i]]$round_4)
        
      }
      
      # Condense to a single list for the final entry
      entry_num <- j
      this_round_1 <- regional_bracket_list %>%
        map("round_1") %>%
        unlist()
      this_round_2 <- regional_bracket_list %>%
        map("round_2") %>%
        unlist()
      this_round_3 <- regional_bracket_list %>%
        map("round_3") %>%
        unlist()
      this_round_4 <- regional_bracket_list %>%
        map("round_4") %>%
        unlist()
      
      final_entry <- list(
        id = entry_num,
        round_1 = this_round_1,
        round_2 = this_round_2,
        round_3 = this_round_3,
        round_4 = this_round_4
      )
      
      #===============================
      # ROUND 5
      #===============================
      a_winner <- unlist(regional_bracket_list[[1]][5])
      b_winner <- unlist(regional_bracket_list[[2]][5])
      c_winner <- unlist(regional_bracket_list[[3]][5])
      d_winner <- unlist(regional_bracket_list[[4]][5])
      
      a_df <- tibble(
        ref_bracket = unlist(regional_bracket_list[[1]][1]),
        winner = unlist(regional_bracket_list[[1]][5]),
        seed = as.integer(str_replace(winner, "^[ABCD]-", "")),
        region = str_extract(winner, "^[ABCD]")
      )
      
      b_df <- tibble(
        ref_bracket = unlist(regional_bracket_list[[2]][1]),
        winner = unlist(regional_bracket_list[[2]][5]),
        seed = as.integer(str_replace(winner, "^[ABCD]-", "")),
        region = str_extract(winner, "^[ABCD]")
      )
      c_df <- tibble(
        ref_bracket = unlist(regional_bracket_list[[3]][1]),
        winner = unlist(regional_bracket_list[[3]][5]),
        seed = as.integer(str_replace(winner, "^[ABCD]-", "")),
        region = str_extract(winner, "^[ABCD]")
      )
      d_df <- tibble(
        ref_bracket = unlist(regional_bracket_list[[4]][1]),
        winner = unlist(regional_bracket_list[[4]][5]),
        seed = as.integer(str_replace(winner, "^[ABCD]-", "")),
        region = str_extract(winner, "^[ABCD]")
      )
      
      ab_round_5 <- rbind(a_df, b_df)
      
      # Does A win; this is binary so if A doesn't win then B wins
      # join in round 5 stats
      ab_final <- ab_round_5 %>%
        left_join(round_5_stats, by = "seed")
      
      if (ab_final$number[1] == ab_final$number[2]) {
        ab_winner <- sample(c(ab_final$winner[1], ab_final$winner[2]), 1)
      } else if (ab_final$number[1] == 0) {
        ab_winner <- ab_final$winner[2]
      } else if (ab_final$number[2] == 0) {
        ab_winner <- ab_final$winner[2]
      } else {
        a_num <- ab_final$number[1]
        b_num <- ab_final$number[2]
        
        a_pct <- a_num / (a_num + b_num)
        if (runif(1) <= a_pct) {
          ab_winner <- ab_final$winner[1]
        } else {
          ab_winner <- ab_final$winner[2]
        }
      }
      
      ab_winner_record <- ab_final %>%
        filter(winner == ab_winner) %>%
        select(-number)
      
      # Do the same for C & D
      cd_round_5 <- rbind(c_df, d_df)
      
      cd_final <- cd_round_5 %>%
        left_join(round_5_stats, by = "seed")
      
      if (cd_final$number[1] == cd_final$number[2]) {
        cd_winner <- sample(c(cd_final$winner[1], cd_final$winner[2]), 1)
      } else if (cd_final$number[1] == 0) {
        cd_winner <- cd_final$winner[2]
      } else if (cd_final$number[2] == 0) {
        cd_winner <- cd_final$winner[2]
      } else {
        c_num <- cd_final$number[1]
        d_num <- cd_final$number[2]
        
        c_pct <- c_num / (c_num + d_num)
        if (runif(1) <= c_pct) {
          cd_winner <- cd_final$winner[1]
        } else {
          cd_winner <- cd_final$winner[2]
        }
      }
      cd_winner_record <- cd_final %>%
        filter(winner == cd_winner) %>%
        select(-number)
      
      # Append round 5 winners to final entry
      final_entry$round_5 <- c(ab_winner_record$winner,
                               cd_winner_record$winner)
      
      #==================================
      # ROUND 6
      #==================================
      abcd_final <- rbind(ab_winner_record, cd_winner_record) %>%
        left_join(round_6_stats, by = "seed")
      
      # Check for special conditions
      if (abcd_final$number[1] == abcd_final$number[2]) {
        abcd_winner <-
          sample(c(abcd_final$winner[1], abcd_final$winner[2]), 1)
      } else if (abcd_final$number[1] == 0) {
        abcd_winner <- abcd_final$winner[2]
      } else if (abcd_final$number[2] == 0) {
        abcd_winner <- abcd_final$winner[1]
      } else {
        ab_num <- abcd_final$number[1]
        cd_num <- abcd_final$number[2]
        
        ab_pct <- ab_num / (ab_num + cd_num)
        if (runif(1) <= ab_pct) {
          abcd_winner <- abcd_final$winner[1]
        } else {
          abcd_winner <- abcd_final$winner[2]
        }
      }
      
      abcd_winner_record <- abcd_final %>%
        filter(winner == abcd_winner) %>%
        select(-number)
      
      # Append round 6 winner to final_entry
      final_entry$round_6 <- abcd_winner_record$winner
      
      entry_list[[j]] <- final_entry
    }
    return(entry_list)
  }


## FUNCTION: create_reference_season_list()
# Create function to build list of the results of every season
create_reference_season_list <- function() {
reference_season_list <- list()
#start_time <- Sys.time()
j <- 1
for(i in 1985:2016) {
  #print(paste("Building", i,  "season ..."))
  
  reference_season_list[[j]] <- get_reference_season(my_season = i)
  #print(paste("Season", i , "complete"))
  j <- j + 1
}
#end_time <- Sys.time() 
#print(end_time - start_time)

return(reference_season_list)
}

## FUNCTION: score_entry_lists
score_entry_lists <- function(my_entries) {
  #start_time <- Sys.time()
  #print(paste("Start time is:", start_time))
  all_seasons_score_list <- list()
  j <- 1
  for (i in seq_along(reference_season_list)) {
    all_scores_list <- list()
    this_season <- reference_season_list[[i]]$id
    #start_time <- Sys.time()
    #print(paste("Pure-naive for", this_season, "started ..."))
    my_pure_naive_scores <-
      score_entries(my_entries[[1]], reference_season_list[[i]])
    #end_time <- Sys.time()
    #print("COMPLETE")
    #print(end_time - start_time)
    
    #start_time <- Sys.time()
    #print(paste("Smart-naive for", this_season, "started ..."))
    my_smart_naive_scores <-
      score_entries(my_entries[[2]], reference_season_list[[i]])
    # end_time <- Sys.time()
    # print("COMPLETE")
    # print(end_time - start_time)
    # 
    # start_time <- Sys.time()
    # print(paste("Historic for", this_season, "started ..."))
    my_historic_scores <-
      score_entries(my_entries[[3]], reference_season_list[[i]])
    # end_time <- Sys.time()
    # print("COMPLETE")
    # print(end_time - start_time)
    
    all_scores_list <- list(
      pure_naive = my_pure_naive_scores,
      smart_naive = my_smart_naive_scores,
      historic_entries = my_historic_scores
    )
    
    all_scores_list[[1]]$season <- this_season
    all_scores_list[[2]]$season <- this_season
    all_scores_list[[3]]$season <- this_season
    
    all_scores_list[[1]]$model <- "pure-naive"
    all_scores_list[[2]]$model <- "smart-naive"
    all_scores_list[[3]]$model <- "historic"
    
    all_seasons_score_list[[j]] <- all_scores_list
    j <- j + 1
  }
  all_seasons_score_final_list <- do.call(rbind, all_seasons_score_list)
  all_seasons_score_final_df <- do.call(rbind, all_seasons_score_final_list)
  
  #end_time <- Sys.time() 
  #print(end_time - start_time)
  
  return(all_seasons_score_final_df)
}

## FUNCTION: summarize_scores()
summarize_scores <- function(my_scores) {
  summarized_scores <- my_scores %>%
    group_by(season, model) %>%
    summarize(
      avg_total_pts = mean(total_points),
      avg_round_1 = mean(rnd_1_pts),
      avg_round_2 = mean(rnd_2_pts),
      avg_round_3 = mean(rnd_3_pts),
      avg_round_4 = mean(rnd_4_pts),
      avg_round_5 = mean(rnd_5_pts),
      avg_round_6 = mean(rnd_6_pts)
    ) %>%
    ungroup()
  
  return(summarized_scores)
}
