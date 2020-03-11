## ----run this on RMarkdown document to get extract below-----------------
#knitr::purl("NCAA March MadnessV9.Rmd", "NCAAMM RMD Code Extract.R")

## ----load_libraries, message=FALSE, warning=FALSE------------------------
library(tidyverse)
library(lubridate)
source("march_madness_functions.R", local = TRUE)

## ----create_pure_naive_entries-------------------------------------------
set.seed(317)

## ----set_data_dir--------------------------------------------------------
data_dir <- "input"

# ## ----build_round_structure-----------------------------------------------
# round_df <- tibble(round = c(rep(1, 32), rep(2, 16), rep(3, 8), rep(4, 4), rep(5, 2), 6))
# unlist(round_df$round)
# length(round_df)


## ----print_round_structure for first year-----------------------------------------------
rounds_1_4 <- get_all_results(return_type = "list", rounds = "1-4")
# str(rounds_1_4[[1]])
# summary(rounds_1_4[[1]])

## Get actual results from given year for baselining
reference_season <- get_reference_season(my_season = 2007)
#reference_season

## Create 1000 pure_naive entries
pure_naive_entries <- create_pure_naive_entries(1000)
smart_naive_entries <- create_smart_naive_entries(1000)
historic_entries <- create_historic_entries(1000)

## Score all pure-naive entries against reference year
pure_naive_results <- score_entries(pure_naive_entries, reference_season)
smart_naive_results <- score_entries(smart_naive_entries, reference_season)
historic_results <- score_entries(historic_entries, reference_season)

#================================
df1 <- pure_naive_results %>%
  mutate(strategy = "pure_naive") %>%
  select(strategy, everything())

df2 <- smart_naive_results %>%
  mutate(strategy = "smart_naive") %>%
  select(strategy, everything())

df3 <- historic_results %>%
  mutate(strategy = "historic") %>%
  select(strategy, everything())

df <- df1 %>%
  bind_rows(df2) %>%
  bind_rows(df3) %>%
  mutate(strategy = factor(strategy, levels = c("pure_naive", "historic", "smart_naive"), 
                           ordered = TRUE)) %>%
  select(strategy, everything())

## ----plot_pure_naive_results---------------------------------------------
library(ggthemes)
library(scales)
library(plotly)
library(RColorBrewer) 
library(patchwork)

## Display first pure-naive entry 
pure_naive_entries[[1]]
smart_naive_entries[[1]]
historic_entries[[1]]

## Show summary statistics
summary(pure_naive_results$total_points)
sd(pure_naive_results$total_points)

summary(smart_naive_results$total_points)
sd(smart_naive_results$total_points)

summary(historic_results$total_points)
sd(historic_results$total_points)

# Setup colors
my_palette <- c("#2D9062", "#320A28", "#511703", "#CB9173", "#E0D68A") 
cols <- c("pure_naive" = my_palette[1], "smart_naive" = my_palette[2], "historic" = my_palette[4])
# 
# col_df <- tibble(strategy = factor(c("pure_naive", "smart_naive", "historic")), 
#                  color = c("#E0D68A", "#320A28", "#CB9173"))
# cols2 <- col_df$strategy = col_df$color
# show_col(my_palette, labels = TRUE)
# 
# #names(cols) <- c("pure_naive", "smart-naive", "historic")
# #model_type <- factor(c("pure_naive", "smart_naive", "historic"))
# 
# pal_name <- "Set3"
# pal <- brewer.pal(4, pal_name)
# show_col(pal, labels = TRUE)
# 
# line_col <- my_palette[1]

#=================================
# Set chart bounds
x_max <- max(df$total_points)
#y_max <- 0.065

density_n <- density(df1$total_points)$y
density_s <- density(df2$total_points)$y
density_h <- density(df3$total_points)$y
y_max <- max(c(density_n, density_s, density_h))
#=================================
# Pure-Naive 
plot_df1 <- df %>%
  filter(strategy == "pure_naive")

avg_score1 <- mean(pure_naive_results$total_points) 
text1 <- paste("Average entry score:", format(avg_score1, digits = 4))

p1 <- ggplot(plot_df1, aes(x=total_points)) +
  geom_density(aes(fill = strategy), alpha = 0.5) + 
  geom_vline(aes(xintercept = avg_score1)) +
  scale_fill_manual(values = cols) +
  labs(title = "Pure-Naive scores against 2007 tournament; 1000 entries", 
       x = "Total Points", y = "Density") +
  lims(x = c(min(plot_df1$total_points), x_max), y = c(0, y_max)) +
  theme_solarized() +
  theme(legend.position = "top") + 
  annotate("text", x = 60, y = 0.038, label = text1)

p1 

max(plot_df1$total_points)
  
#==============================
# Smart Naive
plot_df2 <- df %>%
  filter(strategy == "smart_naive")

avg_score2 <- mean(smart_naive_results$total_points) 
text2 <- paste("Average entry score:", format(avg_score2, digits = 4))

p2 <- ggplot(plot_df2, aes(x=total_points)) +
  geom_density(aes(fill = strategy), alpha = 0.5) + 
  geom_vline(aes(xintercept = avg_score2)) +
  scale_fill_manual(values = cols) +
  labs(title = "Smart-Naive performance against 2007 tournament; 1000 entries", 
       x = "Total Points", y = "Density") +
  lims(x = c(min(plot_df2$total_points), x_max), y = c(0, y_max)) +
  theme_solarized() +
  theme(legend.position = "top") + 
  annotate("text", x = 130, y = 0.04, label = text2)

p2
ggplotly(p2)

#==========================
# Historic 
plot_df3 <- df %>%
  filter(strategy == "historic")

avg_score3 <- mean(historic_results$total_points) 
text3 <- paste("Average entry score:", format(avg_score3, digits = 4))

p3 <- ggplot(plot_df3, aes(x=total_points)) +
  geom_density(aes(fill = strategy), alpha = 0.5) + 
  geom_vline(aes(xintercept = avg_score3)) +
  scale_fill_manual(values = cols) +
  labs(title = "Historic performance against 2007 tournament; 1000 entries", 
       x = "Total Points", y = "Density") +
  lims(x = c(min(plot_df3$total_points), x_max), y = c(0, y_max)) +
  theme_solarized() +
  theme(legend.position = "top") + 
  annotate("text", x = 100, y = 0.026, label = text3)

p3 

# Ridgline plot
library(ggridges)
avg_score <- mean(df$total_points)
text <- paste("Average entry score:", format(avg_score, digits = 4))

ggplot(df, aes(total_points, strategy)) +
  geom_density_ridges2(aes(fill = strategy), alpha = 0.5) +
  geom_vline(aes(xintercept = avg_score)) +
  lims(x = c(0, x_max)) +
  scale_fill_manual(values = cols) +
  labs(title = "All strategies against 2007 tournament", 
       x = "Total Points", y = "Density") +
  theme_solarized() +
  theme(legend.position = "top")
  #annotate("text", x = 50, y = "smart_naive", label = text3)

p1 / p2 / p3

## All on one plot
avg_score <- mean(df$total_points) 
text <- paste("Average entry score:", format(avg_score, digits = 4))

p4 <- ggplot(df, aes(x=total_points)) +
  geom_density(aes(fill = strategy), alpha = 0.5) + 
  geom_vline(aes(xintercept = avg_score)) +
  scale_fill_manual(values = cols) +
  labs(title = "All entries scored against 2007 tournament", 
       x = "Total Points", y = "Density") +
  theme_solarized() +
  theme(legend.position = "top") +
  annotate("text", x = 100, y = 0.036, label = text)

p4
ggplotly(p4)

#Show the top ten scores
df %>%
  arrange(-total_points) %>% 
  select(strategy, entry_num, total_points) %>%
  slice(1:10)
library(knitr)
kable(df, "html")

# Ridgline plot
library(ggridges)
ggplot(df, aes(total_points, strategy)) +
  geom_density_ridges2(aes(fill = strategy)) +
  lims(x = c(0, x_max)) +
  scale_fill_manual(values = cols) +
  labs(title = "All strategies against 2007 tournament", 
       x = "Total Points", y = "Density") +
  theme_solarized() +
  theme(legend.position = "top")

#===============================================================================
# Run entries against all years
all_years_scored <- readRDS("input/scored_entries_v3.RDS") %>%
  mutate(model = str_replace(model, "-", "_"), 
         model = factor(model, levels = c("pure_naive", "historic", "smart_naive"), 
                        ordered = TRUE)) %>%
  select(season, model, entry_num, total_points, everything())

## Get summaries for scores
(all_years_summarized <- summarize_scores(all_years_scored))

all_years_wide <- all_years_scored %>%
  select_at(c(1,2,4)) %>%
  mutate(model = as.character(model)) %>%
  
temp <- all_years_wide %>%
  pivot_wider(
              names_from = model, 
              values_from = total_points)

# sample_n(all_years_summarized, 10) %>%
#   arrange(season, model)
# 
## Summarize strategies over all years
all_years_summarized %>% 
  group_by(model) %>%
  summarize(model_avg = mean(avg_total_pts), 
            model_sd = sd(avg_total_pts)) %>%
  arrange(-model_avg)

## Plot densities for all years
#library(plotly)
avg_score <- mean(all_years_scored$total_points)
p5 <- ggplot(all_years_scored, aes(x=total_points)) +
  geom_density(aes(fill = model), alpha = 0.5) + 
  geom_vline(aes(xintercept = avg_score)) +
  scale_fill_manual(values = cols) +
  theme_solarized() +
  theme(legend.position = "top") +
  labs(title = "All entries against all years", 
       x = "Total Points", y = "Density") +
  annotate("text", x = 90, y = 0.04, label = text)

ggplotly(p5)

saveRDS(all_years_scored, "input/all_years_scored.RDS")
all_years_scored <- readRDS("input/all_years_scored.RDS")
d <- all_years_scored %>% 
  arrange(-total_points) %>%
  select(season, model, entry_num, total_points) %>%
  slice(1:10) 
library(kableExtra)
  kableExtra::kable_styling(d, kable_format = "html")
  
k <-  kable(d, "html")
