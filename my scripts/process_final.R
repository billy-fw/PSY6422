# Install required packages (run once locally)
install.packages(c(
  "tidyverse",
  "here",
  "kableExtra",
  "patchwork"
))

#Load relevant packages
library(here)
library(tidyverse)
library(kableExtra)
library(patchwork)

# Upload raw data
df<-read.csv(here("raw data", "my_premier_league_data.csv"))
# View data
head(df)

# Wrangling
## Check correlations across seasons
cor_by_season <- df %>%
  group_by(Season) %>%
  summarise(r = round(cor(Expected_goals, Actual_goals, method = "pearson"), 3)) ## Round to 3 d.p
head(cor_by_season) ## Display Results

#Visualizations
## Test overall correlation
ovr_cor<-cor(df$Expected_goals, df$Actual_goals)
## Round to 3 d.p
ovr_cor_rounded<-round(ovr_cor, 3)
## Print result
paste("r=", ovr_cor_rounded)
# Create scatter plot of xG and actual goals across all seasons
scatter_plot<-ggplot(data = df, 
               aes(x = Expected_goals,
                   y = Actual_goals)) +
  geom_point(size = 0.9, ## Add scatter function
             color = "skyblue",
             alpha = 0.7) +
  geom_smooth(method = "lm", ## Add regression line
              se = FALSE,
              color = "red") +
  labs(title = "Overall Relationship Between Expected Goals (xG) and Actual Goals Scored ", ## Add labels
       x = "Expected Goals (xG)",
       y = "Actual Goals"
  ) +
  annotate( ## Add correlation coefficient
    "text",
    x = max(df$Expected_goals) * 0.9, ## Place text 90% of the length along the x-axis
    y = max(df$Actual_goals) * 0.7,  ## 70% across the y axis
    label = paste("r =", ovr_cor_rounded),
    size = 4,
    color = "red"
  ) +
  theme_minimal() ## Set simple theme
scatter_plot ##View plot

# Check correlations per season and compare to mean
## Added again for clarity of code
cor_by_season <- df %>%
  group_by(Season) %>% ## Group data by season
  summarise(
    r = round(
      cor(Expected_goals, Actual_goals,
          method = "pearson"),
      3
      )
    )
cor_by_season_col <- df %>% ## Add color to specific values/conditional formatting
  group_by(Season) %>% ## Group by season
  summarise(
    r = round(
      cor(Expected_goals, Actual_goals,
          method = "pearson"),
      3
      )
    ) %>%
  mutate( ## Red < mean, green> mean
    r = cell_spec(
    r,
    color = ifelse(r < ovr_cor, "red", "green")
   )
  )
## Display results
cor_by_season_col %>%
  kable(
    escape = FALSE, ## Allows html formatting
    align = "c", ## Centre align 
    caption = "Correlation by Season"
    )

# Identify potential outlier and visualize correlation trends across seasons
cor_by_season_out <- cor_by_season %>%
  mutate(
    is_outlier_season = ifelse(Season == "2020/21", TRUE, FALSE) ## Flag 2020/21 season as potential outlier
    )
## Line plot showing correlation trend across seasons
line_plot <- ggplot(cor_by_season_out, aes(x = Season, y = r, group = 1)) +
  geom_line(color = "green", size = 1) + # Connect values
  geom_point(aes(color = is_outlier_season), size = 3) +   ## Plot points for each season, different color for outlier
  scale_color_manual(values = c("FALSE" = "green", "TRUE" = "red")) + ## Assign colors
  labs( ## Add labels and legend
    title = "Strength of xG vs Actual Goals Correlation Over Seasons",
    x = "Season",
    y = "Pearson Correlation",
    color = "Outlier"
  ) +
  theme_minimal() + ## Set theme
  expand_limits(y = 0.5) ## Set the y-axis to start at 0.5 for more meaningful visualisation

# Boxplot showing distribution of correalations across seasons
box_plot <- ggplot(cor_by_season_out, aes(y = r)) +
  geom_boxplot(fill = "green",
               outlier.color = "red",
               outlier.size = 3) +
  geom_point(data = filter(cor_by_season_out, is_outlier_season), ## Add visual emphasis to outlier
             aes(x = 1, y = r), color = "red", size = 3) +
  labs(
    title = "Boxplot", ## Add title
  ) +
  theme_minimal() + ## Keep consistent theme
  expand_limits(y = 0.5) ## Match Y-axis to compare across plots
# Combine plots side by side and display
line_plot + box_plot + plot_layout(ncol = 2, widths = c(2, 1))
# Multiple regression visualization by season
mr_plot<-ggplot(
  df,
  aes(
    x = Expected_goals,
    y = Actual_goals,
    colour = factor(Season)  ## Add separate regression lines for each season
    )
  ) +
  geom_point(alpha = 0.2, # Plot individual match observations and reduce size and transparacy 
             size = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + ## Add each line and remove CI
  labs( ## Add labs
    title = "Relationship Between Actual Goals and Expected Goals per Season",
    colour = "Season",
    x = "xG",
    y = "Actual Goals Scored"
  ) +
  theme_minimal() ## Keep theme
# Display plot
mr_plot

# Save plots
ggsave("plots/scatter_plot.pdf", plot = scatter_plot)
ggsave("plots/line_plot.pdf", plot = line_plot)
ggsave("plots/box_plot.pdf", plot = box_plot)
ggsave("plots/mr_plot.pdf", plot = mr_plot)


