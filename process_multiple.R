library(here)
library(tidyverse)
data_m<-read.csv(here("raw data", "my_premier_league_data.csv"))
str(data_m)
head(data_m)
##check correlations per season
cor_by_season <- data_m %>%
  group_by(Season) %>%
  summarise(correlation = cor(Expected_goals, Actual_goals, method = "pearson"))
print(cor_by_season) ##show this on quarto
##scatter plot
ggplot(cor_by_season, aes(x = Season, y = correlation)) +
  geom_point(size = 3) +
  geom_line() +
  theme_minimal() +
  labs(title = "Correlation Between xG and Goals by Season",
       x = "Season",
       y = "Pearson correlation")

trend_model <- lm(correlation ~ Season, data = cor_by_season)
print(trend_model)

model <- lm(Actual_goals ~ Expected_goals * Season, data = data_m)
summary(model)

ggplot(data_m, aes(x = Expected_goals, y = Actual_goals, colour = factor(Season))) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Actual Goals and Expected Goals per Season",
    colour = "Season"
  ) +
  theme_minimal()

data_m <- data_m %>%
  mutate(Season = as.factor(Season))
df_long <- data_m %>%
  pivot_longer(
    c(Expected_goals, Actual_goals),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(Metric = factor(Metric, levels = c("Expected_goals", "Actual_goals"),
                         labels = c("xG", "GF")))


library(plotly)

p_2 <- ggplot(data_m, aes(x = Expected_goals, y = Actual_goals, color = Season)) +
  geom_point(size = 0.4) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.7,
    linetype = "dashed"
  )

ggplotly(p_2, tooltip = c("Season", "Club", "Expected_goals", "Actual_goals"))

install.packages("kableExtra")
library(knitr)
library(kableExtra)

cor_by_season <- data_m %>%
  group_by(Season) %>%
  summarise(r = round(cor(Expected_goals, Actual_goals, method = "pearson"), 3)) %>%
  mutate(r = cell_spec(
    r,
    color = ifelse(r < ovr_cor, "red", "green")
  ))
cor_by_season %>%
  kable(escape = FALSE, align = "c", caption = "Correlation by Season")
cor_by_season_col %>%
  kable(escape = FALSE, align = "c", caption = "Correlation by Season")

ggplot(cor_by_season, aes(x = Season, y = r, group = 1)) +
  geom_line(color = "red", size = 1) +
  geom_point(size = 3, color = "skyblue") +
  labs(
    title = "Strength of xG vs Actual Goals Correlation Over Seasons",
    x = "Season",
    y = "Pearson Correlation"
  ) +
  theme_minimal() +
  expand_limits(y = 0)

install.packages("patchwork")
library(ggplot2)
library(dplyr)
library(kableExtra)
library(ggpubr)   # for ggtexttable
library(patchwork)  # combine plots

# Prepare the table as a ggplot object
table_plot <- ggtexttable(
  cor_by_season_col %>% 
    kable(escape = FALSE, align = "c", caption = "Correlation by Season"),
  rows = NULL
)

# Prepare the plot
line_plot <- ggplot(cor_by_season, aes(x = Season, y = r, group = 1)) +
  geom_line(color = "green", size = 1) +
  geom_point(size = 3, color = "red") +
  labs(
    title = "Strength of xG vs Actual Goals Correlation Over Seasons",
    x = "Season",
    y = "Pearson Correlation"
  ) +
  theme_minimal()

cor_by_season_out <- cor_by_season %>%
  mutate(is_outlier_season = ifelse(Season == "2020/21", TRUE, FALSE))
line_plot <- ggplot(cor_by_season_out, aes(x = Season, y = r, group = 1)) +
  geom_line(color = "green", size = 1) +
  geom_point(aes(color = is_outlier_season), size = 3) +
  scale_color_manual(values = c("FALSE" = "green", "TRUE" = "red")) +
  labs(
    title = "Strength of xG vs Actual Goals Correlation Over Seasons",
    x = "Season",
    y = "Pearson Correlation",
    color = "Outlier"
  ) +
  theme_minimal() +
  expand_limits(y = 0.5)

# Boxplot
box_plot <- ggplot(cor_by_season_out, aes(y = r)) +
  geom_boxplot(fill = "green", outlier.color = "red", outlier.size = 3) +
  geom_point(data = filter(cor_by_season, is_outlier_season), 
             aes(x = 1, y = r), color = "red", size = 3) +
  labs(
    title = "Boxplot of Seasonal Correlations",
    y = "Pearson Correlation"
  ) +
  theme_minimal() +
  expand_limits(y = 0.5)

# Combine side by side
line_plot + box_plot + plot_layout(ncol = 2, widths = c(2, 1))

cor_by_season_out <- cor_by_season %>%
  mutate(is_outlier_season = ifelse(Season == "2020/21", TRUE, FALSE))
line_plot <- ggplot(cor_by_season_out, aes(x = Season, y = r, group = 1)) +
  geom_line(color = "green", size = 1) +
  geom_point(aes(color = is_outlier_season), size = 3) +
  scale_color_manual(values = c("FALSE" = "green", "TRUE" = "red")) +
  labs(
    title = "Strength of xG vs Actual Goals Correlation Over Seasons",
    x = "Season",
    y = "Pearson Correlation",
    color = "Outlier"
  ) +
  theme_minimal() +
  expand_limits(y = 0.5)

# Boxplot
box_plot <- ggplot(cor_by_season_out, aes(y = r)) +
  geom_boxplot(fill = "green", outlier.color = "red", outlier.size = 3) +
  geom_point(data = filter(cor_by_season_out, is_outlier_season), 
             aes(x = 1, y = r), color = "red", size = 3) +
  labs(
    title = "Boxplot",
  ) +
  theme_minimal() +
  expand_limits(y = 0.5)

# Combine side by side
line_plot + box_plot + plot_layout(ncol = 2, widths = c(2, 1))

##merge datasets
df1<-read_csv(here("raw data", "my_premier_league_data.csv"))
df2<-read_csv(here("raw data", "england-premier-league-teams-2018-to-2019-stats.csv"))
df2<-df2 %>%
  select(team_name, season, )

