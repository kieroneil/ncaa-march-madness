# March Madness charts
library(tidyverse)
library(ggthemes)
library(plotly)
library(RColorBrewer) 
library(patchwork)

my_palette <- c("#2D9062", "#320A28", "#511703", "#CB9173", "#E0D68A") 
cols <- c("pure_naive" = my_palette[1], "smart_naive" = my_palette[2], "historic" = my_palette[4])

col_df <- tibble(strategy = factor(c("pure_naive", "smart_naive", "historic")), 
                 color = c("#E0D68A", "#320A28", "#CB9173"))
cols2 <- col_df$strategy = col_df$color
show_col(my_palette, labels = TRUE)

#names(cols) <- c("pure_naive", "smart-naive", "historic")
#model_type <- factor(c("pure_naive", "smart_naive", "historic"))

pal_name <- "Set3"
pal <- brewer.pal(4, pal_name)
show_col(pal, labels = TRUE)

line_col <- my_palette[1]

#================================
df1 <- my_pure_naive_results %>%
  mutate(strategy = "pure_naive")
df2 <- my_smart_naive_results %>%
  mutate(strategy = "smart_naive")
df3 <- my_historic_results %>%
  mutate(strategy = "historic")

df <- df1 %>%
  bind_rows(df2) %>%
  bind_rows(df3) %>%
  mutate(strategy = factor(strategy, levels = c("pure_naive", "historic", "smart_naive"), 
                           ordered = TRUE)) %>%
  select(strategy, everything())

x_max <- max(df$total_points)
y_max <- 0.045

# Ridgline plot
library(ggridges)
ggplot(df, aes(total_points, strategy)) +
  geom_density_ridges2(aes(fill = strategy)) +
  lims(x = c(0, x_max))
#=================================
# Pure-Naive 
plot_df1 <- df %>%
  filter(strategy == "pure_naive")

avg_score1 <- mean(my_pure_naive_results$total_points) 
text1 <- paste("Average entry score:", format(avg_score, digits = 4))

p1 <- ggplot(plot_df1, aes(x=total_points)) +
  geom_density(aes(fill = strategy), alpha = 0.5) + 
  geom_vline(aes(xintercept = avg_score1)) +
  scale_fill_manual(values = cols) +
  labs(title = "Pure-Naive performance against 2007 tournament", 
       x = "Total Points", y = "Density") +
  lims(x = c(0, x_max), y = c(0, y_max)) +
  theme_solarized() +
  theme(legend.position = "top")

p1 + annotate("text", x = 60, y = 0.038, label = text1)

#==============================
# Smart Naive
plot_df2 <- df %>%
  filter(strategy == "smart_naive")

avg_score2 <- mean(my_smart_naive_results$total_points) 
text2 <- paste("Average entry score:", format(avg_score2, digits = 4))

p2 <- ggplot(plot_df2, aes(x=total_points)) +
  geom_density(aes(fill = strategy), alpha = 0.5) + 
  geom_vline(aes(xintercept = avg_score2)) +
  scale_fill_manual(values = cols) +
  labs(title = "Smart-Naive performance against 2007 tournament", 
       x = "Total Points", y = "Density") +
  lims(x = c(0, x_max), y = c(0, y_max)) +
  theme_solarized() +
  theme(legend.position = "top")

p2 + annotate("text", x = 90, y = 0.035, label = text2)

#==========================
# Historic Results 
plot_df3 <- df %>%
  filter(strategy == "historic")

avg_score3 <- mean(my_historic_results$total_points) 
text3 <- paste("Average entry score:", format(avg_score3, digits = 4))

p3 <- ggplot(plot_df3, aes(x=total_points)) +
  geom_density(aes(fill = strategy), alpha = 0.5) + 
  geom_vline(aes(xintercept = avg_score3)) +
  scale_fill_manual(values = cols) +
  labs(title = "Historic performance against 2007 tournament", 
       x = "Total Points", y = "Density") +
  theme_solarized() +
  theme(legend.position = "top")

p3 + annotate("text", x = 110, y = 0.016, label = text3)

p1 / p2 / p3

## All on one plot
avg_score <- mean(df$total_points) 
text <- paste("Average entry score:", format(avg_score, digits = 4))

p4 <- ggplot(df, aes(x=total_points)) +
  geom_density(aes(fill = strategy), alpha = 0.5) + 
  geom_vline(aes(xintercept = avg_score)) +
  scale_fill_manual(values = cols) +
  labs(title = "All strategies against 2007 tournament", 
       x = "Total Points", y = "Density") +
  theme_solarized() +
  theme(legend.position = "top")

p4 <- ggplot(df, aes(x=total_points)) +
  geom_density(aes(fill = strategy), alpha = 0.5) + 
  geom_vline(aes(xintercept = avg_score)) +
  scale_fill_few() +
  theme_few() +
  theme(legend.position = "top") +
  labs(title = "All strategies against 2007 tournament", 
       x = "Total Points", y = "Density")
  
p4 + annotate("text", x = 100, y = 0.035, label = text)

# Pure-Naive & Smart-Naive
ggplot(combined_results, aes(x=total_points, fill = model, color = model)) +
  geom_density(aes(alpha = 0.5)) + 
  geom_vline(aes(xintercept = mean(total_points), 
                 color = model)) +
  scale_color_brewer(palette = "Set1") + 
  theme(legend.position = "none")

p1 / p2
# Historic Results 

ggplot(my_historic_results, aes(x=total_points, fill = model_type[3], color = model_type[3])) +
  geom_density(aes(alpha = 0.5)) + 
  geom_vline(aes(xintercept = mean(total_points), 
                 color = model_type[3])) +
  scale_fill_manual(values = pal) + 
  scale_color_manual(values = pal) +
  # scale_fill_brewer(palette = pal) + 
  # scale_color_brewer(palette = pal) +
  theme(legend.position = "none")

# Historic results with Smart-Naive  
ggplot(combined_results, aes(x=total_points)) +
  geom_density(aes(fill = model), alpha = 0.5) + 
  geom_vline(aes(xintercept = mean(total_points), 
                 color = my_palette[2])) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) 
#theme(legend.position = "none")

# All Strategies' entries applied to all tournaments
avg_points <- mean(scored_entries$total_points)

p <- ggplot(scored_entries, aes(x=total_points, fill = model, color = NULL)) +
  geom_density(alpha = 0.5) + 
  geom_vline(xintercept = avg_points, color = my_palette[2]) +
  scale_fill_brewer(palette = "Set2") + 
  ggtitle("Smart-Naive is the overall winner!!") + 
  geom_text(aes(x = avg_points + 30, y = .040, 
                 label = paste("Average points:", as.character(format(avg_points, digits = 4))))) +
  theme_solarized()
ggplotly(p)
