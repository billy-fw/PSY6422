##testing for analysis of premier league football data
##load relevant packages
library(tidyverse)
library(here)
library(plotly)
##load data file
df<-read.csv(here("raw data", "england-premier-league-teams-2018-to-2019-stats.csv"))
head(df)
str(df)
glimpse(df)  ##show output on quarto (raw data glimpse)
##glimpse of this dataset highlights a lot of variables that i just do not need in my dataset
##i want to get rid of a lot of these columns to make the dataset cleaner to read
remove_variables<-c("over", "under")
df2<-df %>%
  select(-starts_with(remove_variables))
glimpse(df2)
#####OR
##keep only the variables i need
xg_var<-names(df %>% select(starts_with("xg")))
variables_to_select<-c("team_name", "wins", "draws", "losses", "goals_scored", "goals_conceded", "league_position", xg_var)
df_select<-df %>%
  select(variables_to_select)
##check for missing data
sum(is.na(df_select)) ##show output on quarto
##create row for average goal scored per match
df_select<-df_select %>%
  mutate(avg_goals_scored = goals_scored/38)
glimpse(df_select)
##round new column to 2 d.p
df_select<-df_select %>%
  mutate(avg_goals_scored = round(avg_goals_scored, 2))
glimpse(df_select$avg_goals_scored)
##rename columns for clarity
df_select<-df_select %>%
  rename(
    actual_goals = avg_goals_scored,
    expected_goals = xg_for_avg_overall
  )
glimpse(df_select) ##show output on quarto (final df)




###Analysis


##check for correlations
##does xg correlate to actual goals scored
cor(df_select$expected_goals, df_select$actual_goals) ##show this output on quarto
##create scatter plot
p<-ggplot(
  data = df_select,
  aes(x = actual_goals, ##set target variables on axis'
      y = expected_goals,
      color = as.factor(team_name), ##color by team name
      text = paste( ##set text for interactive plot
        "Team:", team_name ,
        "League Position:", league_position,
        "Expected Goals:", expected_goals,
        "Actual Goals:", actual_goals
      ))) +
  geom_point() +  ##add regression line
  geom_smooth(method = "lm",
              se = FALSE,
              linetype = "dashed"
  ) + 
  labs(title = "Expected Goals vs Actual Goals Scored", ##add labels
       x = "Acual Goals Scored",
       y = "Expected Goals Scored"
  ) +
  theme_minimal()
p<-ggplotly(p, tooltip = "text") ##make interactive
p <- layout(p, showlegend = FALSE) ##hide legend, not needed when interactive anyway
p
##set colours to match the strip each team plays in
show(df_select$team_name) ##set team colours
team_colors<- c(
  "Arsenal FC" = "#EF0107",                 # red
  "Tottenham Hotspur FC" = "#132257",       # navy
  "Manchester City FC" = "#6CABDD",         # sky blue
  "Leicester City FC" = "#0053A0",          # royal blue
  "Crystal Palace FC" = "#1B458F",          # blue (with red stripe)
  "Everton FC" = "#003399",                 # royal blue
  "Burnley FC" = "#6C1D45",                 # claret
  "Southampton FC" = "#D71920",             # red
  "AFC Bournemouth" = "#DA291C",            # red/black
  "Manchester United FC" = "#DA291C",       # red
  "Liverpool FC" = "#C8102E",               # red
  "Chelsea FC" = "#034694",                 # blue
  "West Ham United FC" = "#7A263A",         # claret
  "Watford FC" = "#FBEE23",                 # yellow
  "Newcastle United FC" = "#241F20",        # black (stripes)
  "Cardiff City FC" = "#1E63B1",            # blue
  "Fulham FC" = "#FFFFFF",                  # white
  "Brighton & Hove Albion FC" = "#0057B8",  # blue/white
  "Huddersfield Town FC" = "#0E63AD",       # blue/white
  "Wolverhampton Wanderers FC" = "#FDB913"  # gold
)
p<-ggplot(
  data = df_select,
  aes(x = actual_goals, ##set target variables on axis'
      y = expected_goals,
      color = as.factor(team_name), ##color by team name
      text = paste( ##set text for interactive plot
        "Team:", team_name, "<br>", ##add line breaks
        "League Position:", league_position, "<br>",
        "Expected Goals:", expected_goals, "<br>",
        "Actual Goals:", actual_goals
      ))) +
  geom_point() +  ##add regression line
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE,
    color = "darkgreen",
    linetype = "dashed",
    linewidth = 0.5
  ) + 
  labs(title = "Expected Goals vs Actual Goals Scored", ##add labels
       x = "Actual Goals Scored",
       y = "Expected Goals Scored"
  ) +
  scale_color_manual(values = team_colors) +
  theme_minimal()
p<-ggplotly(p, tooltip = "text") ##make interactive
p <- layout(p, showlegend = FALSE) ##hide legend, not needed when interactive anyway
p
##add correlation coefficient label
corr_value <- cor(df_select$actual_goals, df_select$expected_goals)
corr_label <- paste0("r = ", round(corr_value, 3))
p<-p %>% layout(
  annotations = list(
    x = 0.05,   
    y = 0.95,
    xref = "paper",
    yref = "paper",
    text = corr_label,
    showarrow = FALSE,
    font = list(size = 14, color = "darkgreen")
  )
)
p
