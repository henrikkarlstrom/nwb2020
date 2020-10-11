# This script reads in publication data and model 
# predictions and produces plots of: 
#   1)  the times series, its decomposition and 
#       prediction fits.
#   2)  cross-country international co-publication
#   3)  the redistribution effects of the 
#       internationalisation factor.


## SETUP ##

# Required library
library(tidyverse)
library(fable)

# Load in data
nvi_model <- readRDS("./processed_data/ts_model.rds") %>%
  index_by(month = yearmonth(date)) %>%
  summarise(
    coop_share = mean(.mean),
    interval80_lower = mean(interval80_lower),
    interval80_upper = mean(interval80_upper),
    interval95_lower = mean(interval95_lower),
    interval95_upper = mean(interval95_upper)
    )

actual_data <- read_csv("./processed_data/coop_share.csv") %>%
  as_tsibble(index = date)

series_components <- readRDS("./processed_data/model_stl.rds") %>%
  components()

coop_data <- readRDS("./processed_data/dimensions_data.rds")

## Initial trend plot ##
actual_data %>%
  as_tibble() %>%
  group_by(month = yearmonth(date)) %>%
  summarise(coop_share = mean(coop_share)) %>%
  filter(year(month) < 2017) %>%
  ggplot(aes(x = month, y = coop_share)) +
  geom_line(color = "#565657", alpha = 0.7) +
  theme_minimal() +
  scale_x_yearmonth(breaks = "1 year") +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format()
  ) +
  labs(
    title = "International co-publications share trend",
    subtitle = "Monthly average",
    x = NULL, 
    y = NULL
  ) +
  theme(
    axis.title.y = element_text(family = "Open Sans"),
    axis.text = element_text(family = "Open Sans"),
    plot.title = element_text(family = "Open Sans"),
    plot.subtitle = element_text(family = "Open Sans"),
    plot.title.position = "plot"
  )

# Save plot
ggsave("./plots/initial_trend.png", scale = 1, width = 8)

## Initial trend plot with smoothing ##
actual_data %>%
  as_tibble() %>%
  group_by(month = yearmonth(date)) %>%
  summarise(coop_share = mean(coop_share)) %>%
  ungroup() %>%
  filter(year(month) < 2017) %>%
  ggplot(aes(x = month, y = coop_share)) +
  geom_line(color = "#565657", alpha = 0.7) +
  geom_smooth(
    method = "loess",
    formula = "y ~ x",
    color = "#28585a", 
    fill = "#28585a",
    level = 0.95,
    size = 0.5
  ) +
  theme_minimal() +
  scale_x_yearmonth(breaks = "1 year") +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format()
  ) +
  labs(
    title = "International co-publications share trend",
    subtitle = "Trendline with smoothed conditional means",
    x = NULL, 
    y = NULL
  ) +
  theme(
    axis.title.y = element_text(family = "Open Sans"),
    axis.text = element_text(family = "Open Sans"),
    plot.title = element_text(family = "Open Sans"),
    plot.subtitle = element_text(family = "Open Sans"),
    plot.title.position = "plot"
  )

# Save plot
ggsave(
  filename = "./plots/initial_trend_smooth.png",
  scale = 1,
  width = 8
  )

## Time series components plot ##
series_components %>%
  pivot_longer(
    cols = c(coop_share, additive_terms, trend)
    ) %>%
  mutate(
    name = fct_relevel(
      name, 
      "coop_share", 
      "trend",
      "additive_terms"
      )
    ) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(color = "#28585a") +
  facet_grid(
    scales = "free_y",
    rows = vars(name),
    labeller = as_labeller(
      c("coop_share" = "Series",
        "trend" = "Trend",
        "additive_terms" = "Season")
      )
    ) +
  theme_minimal() +
  scale_x_yearmonth(breaks = "1 year") +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = c(0,0)
  ) +
  scale_x_yearmonth(date_breaks = "1 year") +
  labs(
    x = NULL, 
    y = NULL,
    title = "Time series decomposition"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(margin = margin(0, 0, 15, 0)),
    text = element_text(family = "Open Sans"),
    panel.spacing.y = unit(1, "cm")
  )


# Save the plot
ggsave("./plots/stl_decomp.png", scale = 1, width = 8)

## Prediction fit plot ##
actual_data %>%
  index_by(month = yearmonth(date)) %>%
  summarise(coop_share = mean(coop_share)) %>%
  filter_index(~ "2017 Jan") %>%
  ggplot(aes(x = month, y = coop_share)) +
  geom_line(
    color = "#565657",
    alpha = 0.4
  ) +
  geom_line(
    data = nvi_model,
    color = "#28585a"
  ) +
  scale_x_yearmonth(breaks = "1 year") +
  scale_y_continuous(
    limits = c(0,1),
    labels = scales::percent_format()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Monthly average",
    title = "Time series prediction based on pre-2017 data"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(family = "Open Sans"),
    axis.text = element_text(family = "Open Sans"),
    plot.title = element_text(family = "Open Sans"),
    plot.subtitle = element_text(family = "Open Sans"),
    plot.title.position = "plot"
  )

# Save plot
ggsave("./plots/pred_plot.png", scale = 1, width = 8)


## Prediction fit plot with probability intervals##
actual_data %>%
  index_by(month = yearmonth(date)) %>%
  summarise(coop_share = mean(coop_share)) %>%
  ggplot(aes(x = month, y = coop_share)) +
  geom_line(
    color = "#565657",
    alpha = 0.4
  ) +
  geom_line(
    data = nvi_model,
    color = "#28585a"
  ) +
  scale_x_yearmonth(breaks = "1 year") +
  scale_y_continuous(
    limits = c(0,1),
    labels = scales::percent_format()
  ) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Monthly average",
    title = "Trend pre-2017 accurately predicts co-publication shares"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(family = "Open Sans"),
    axis.text = element_text(family = "Open Sans"),
    plot.title = element_text(family = "Open Sans"),
    plot.subtitle = element_text(family = "Open Sans"),
    plot.title.position = "plot"
  )

# Save plot
ggsave("./plots/pred_vs_truth.png", scale = 1, width = 8)


## Cross-country plot
coop_data %>%
  mutate(coop_share = int_coop / all_pubs) %>%
  ggplot(aes(x = id, y = coop_share, color = country)) +
  geom_line(size = 1.1, alpha = 0.7) +
  scale_y_continuous(
    limits = c(0,1),
    labels = scales::percent_format()
  ) +
  scale_x_continuous(breaks = seq(2011,2019,1)) +
  scale_color_manual(
    values = c("#28585a", "#aad9dd", "#f7d019", "#c9d755")
    ) +
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "No discernible difference between countries",
    subtitle = "Yearly international co-publication share",
    caption = "Source: Dimensions"
    ) +
  theme(
    axis.title.y = element_text(family = "Open Sans"),
    axis.text = element_text(family = "Open Sans"),
    plot.title = element_text(family = "Open Sans"),
    plot.subtitle = element_text(family = "Open Sans"),
    plot.title.position = "plot",
    legend.position = "bottom"
  )

ggsave("./plots/cross_country.png", scale = 1, width = 8)
