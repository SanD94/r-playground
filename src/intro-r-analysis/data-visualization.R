library(aida)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rcartocolor)

anscombe %>% as_tibble


tidy_anscombe <- anscombe %>%
  as_tibble %>%
  pivot_longer(
    everything(),
    names_pattern = "(.)(.)",
    names_to = c(".value", "grp")
  ) %>%
  mutate(grp = paste0("Group ", grp))


tidy_anscombe
# summary
tidy_anscombe %>%
  group_by(grp) %>%
  summarise(
    mean_x = mean(x),
    mean_y = mean(y),
    min_x = min(x),
    min_y = min(y),
    max_x = max(x),
    max_y = max(y),
    crrltn = cor(x, y)
  )


project_colors <- carto_pal(11, "Safe")[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 1, 2)]
tidy_anscombe %>%
  ggplot(aes(x, y)) +
    geom_smooth(method = lm, se = F, color = "darkorange") +
    geom_point(color = project_colors[3], size = 2) +
    scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    labs(
      title = "Anscombe's Quartet", x = NULL, y = NULL,
      subtitle = bquote(y == 0.5 * x + 3 ~ (r %~~% .82) ~ "for all groups")
    ) +
    facet_wrap(~grp, ncol = 2, scales = "free_x") +
    theme(strip.background = element_rect(fill = "#f2f2f2", colour = "white"))
  

# Faceting

avocado_data <- aida::data_avocado
avocado_data_early_late <- avocado_data %>%
  mutate(early = ifelse(Date <= median(Date), "early", "late"))


avocado_data_early_late %>%
  ggplot(aes(x = log(total_volume_sold), y = average_price)) +
  geom_point(alpha = 0.3, color = "skyblue") +
  geom_smooth(method = "lm", color = "darkorange") +
#  facet_grid(type ~ early)
  facet_wrap(type ~ early)


avocado_data %>%
  ggplot(
    mapping = aes(
      x = log(total_volume_sold),
      y = average_price,
      color = type
    )
  ) +
  geom_point(alpha = 0.5) +
  guides(color = "none")