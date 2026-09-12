library(fable)
library(ggplot2movies)
library(hrbrthemes)
library(tidyverse)

set.seed(2023)

movies_prepped <- movies %>%
  as_tibble() %>%
  mutate(genre = case_when(Action == 1 ~ 'Action',
                           Animation == 1 ~ 'Animation',
                           Comedy == 1 ~ 'Comedy',
                           Drama == 1 ~ 'Drama',
                           Documentary == 1 ~ 'Documentary',
                           Romance == 1 ~ 'Romance',
                           Short == 1 ~ 'Short',
                           TRUE ~ 'Other')) %>%
  filter(genre %in% c('Documentary', 'Short') == FALSE) %>%
  group_by(year, genre) %>%
  summarise(n = n(),
            mean_length = mean(length, na.rm = TRUE),
            mean_budget = mean(budget, na.rm = TRUE),
            mean_rating = mean(rating, na.rm = TRUE),
            mean_votes = mean(votes, na.rm = TRUE)
  ) %>%
  filter(year >= 1960)


movies_tsibble <- movies_prepped %>%
  as_tsibble(index = year,
             key = c(genre))

movie_fcasts_models <- movies_tsibble %>%
  model(arima = ARIMA(mean_budget),
        ets = ETS(mean_budget)
  )

fcast_accuracy <- accuracy(movie_fcasts_models)

movie_fcasts_models %>%
  forecast(h = "5 years") %>%
  autoplot(movies_tsibble)+
  facet_wrap(genre ~ ., scales = 'free')+
  theme_ipsum()

movies_tsibble_long <- movies_prepped %>%
  pivot_longer(cols = n:mean_votes,
               names_to = 'metric',
               values_to = 'value') %>%
  as_tsibble(index = year,
             key = c(genre, metric))

movie_fcasts_models_long <- movies_tsibble_long %>%
  model(arima = ARIMA(value),
        ets = ETS(value)
  )

fcast_accuracy_long <- accuracy(movie_fcasts_models_long)

movie_fcasts_models_long %>%
  forecast(h = "5 years") %>%
  autoplot(movies_tsibble_long)+
  facet_grid(metric ~ genre, scales = 'free')+
  theme_ipsum()
