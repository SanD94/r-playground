library(aida)
library(tibble)
library(dplyr)
library(purrr)
library(magrittr)


data_bljm_processed <- aida::data_BLJM

### gruop and ungroup is important!!! like parantheses
data_bljm_processed %>%
  group_by(condition) %>%
  summarise(nr_observation_per_condition = n()) %>%
  ungroup()


data_bljm_processed %>% summarize(n_rows = n()) # a tibble
nrow(data_bljm_processed) # a number


data_bljm_processed %>%
  group_by(submission_id) %>%
  summarise(nr_data_points = n()) %>%
  ungroup()


data_bljm_processed %>%
  group_by(condition, response) %>%
  summarise(n = n()) %>%
  ungroup()

data_bljm_processed %>%
  dplyr::count(condition, response)


bljm_associated_counts <- data_bljm_processed %>%
  pivot_wider(names_from = condition, values_from = response) %>%
  select(-BM) %>%
  dplyr::count(JM, LB)
bljm_associated_counts


bljm_associated_counts %>%
  mutate(n = n / sum(n)) %>%
  pivot_wider(names_from = LB, values_from = n)

# Avocado Data
avocado_data <- aida::data_avocado

avocado_data %>%
  group_by(type) %>%
  summarise(
    mean_price = mean(average_price),
    median_price = median(average_price)
  )

# bootstrap function
bootstrapped_ci <- function(data_vector, n_resamples = 1000) {
  resampled_means <- map_dbl(seq(n_resamples), function(i) {
    mean(sample(x = data_vector,
      size = length(data_vector),
      replace = T
    ))
  })
  tibble(
    "lower" = quantile(resampled_means, 0.025),
    "mean" = mean(data_vector),
    "upper" = quantile(resampled_means, 0.975)
  )
}


bootstrapped_ci(avocado_data$average_price)

avocado_data %>%
  nest(price_tibbles = !type)
  summarise(
    CIs = map(price_tibbles, function(d) bootstrapped_ci(d$average_price))
  ) %>%
  unnest(CIs)


avocado_data %>%
  nest(price_tibbles = !type) %>%
  pull(price_tibbles) %>%
  .[1] %>% head()


# Covariance and Correlation
contrived_example <- tribble(
  ~x, ~y,
  2,  2,
  2.5, 4,
  3.5, 2.5,
  4, 3.5
)


means_contr_example <- map_df(contrived_example, mean)
means_contr_example

contrived_example %<>% mutate(
  area_rectangle = (x - mean(x)) * (y - mean(y)),
  covariance = 1 / (n() - 1) * sum((x - mean(x)) * (y - mean(y)))
)
contrived_example


with(contrived_example, cov(x, y))
with(contrived_example, cor(x, y))
with(avocado_data, cov(log(total_volume_sold), average_price))
with(avocado_data, cor(log(total_volume_sold), average_price))
