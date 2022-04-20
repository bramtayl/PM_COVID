library(dplyr)
library(ggplot2)

# parameters to reuse
considered_period <- "Considered by Wu et al."
image_format <- "png"
image_units <- "in"
loess_parameter <- 0.1
log_population_density_label <- "Log population density (individuals/mi²)"
long_height <- 3
long_width <- 8
plot_theme <- theme_bw()
residual_label <- "Residuals of cumulative deaths through Oct 12, 2021"
square_height <- 6
square_width <- 6

daily_estimates <-
  read_csv("data/daily_estimates.csv") %>%
  # Wu et al. only plotted after this date
  filter(date >= as.Date("2020-04-18")) %>%
  mutate(
    Period = ifelse(
      date <= as.Date("2020-09-07"),
      considered_period,
      "Not considered by Wu et al."
    )
  )

date_scale <- function(a_plot) {
  a_plot +
  scale_x_date(
    date_breaks = "1 month",
    # Three letter month abbreviation, 4 digit years
    date_labels = "%b %Y",
    expand = c(0, 0)
  ) +
  # diagonal so labels will fit
  theme(
    axis.text.x = element_text(
      angle = 60,
      hjust = 1
    )
  )
}

mortality_graph <- function(a_plot) {
  date_scale(
    a_plot +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    plot_theme +
    labs(
      y = "Mortality Rate Ratios",
      x = ""
    ) +
    geom_hline(yintercept = 1, linetype = "dashed")
  )
}

# Wu et al.'s figure
mortality_graph(
  ggplot(
    daily_estimates %>%
    filter(Period == considered_period)
  ) +
  aes(
    x = date,
    y = estimate,
    ymin = lower_bound,
    ymax = upper_bound
  )
)
  
ggsave("data/early_plot.png",
  device = image_format,
  width = long_width,
  height = long_height,
  units = image_units
)

# additional data
mortality_graph(
  ggplot(daily_estimates) +
  aes(
    x = date,
    y = estimate,
    ymin = lower_bound,
    ymax = upper_bound,
    fill = Period
  )
) +
  scale_colour_grey(aesthetics = "fill")

ggsave("data/full_plot.png",
  device = image_format,
  width = long_width,
  height = long_height,
  units = image_units
)

final_residuals <-
  all_data %>%
  filter(date == as.Date("2021-10-12")) %>%
  mutate(
    residual = resid(run_regression(.)),
    log_population_density = log(population_density),
    # label some outliers
    label =
      ifelse(
        abs(residual) > 3000,
        metro_name,
        ""
      )
  )

ggplot(final_residuals) +
  aes(
    x = log_population_density,
    y = residual,
    label = label
  ) +
  geom_point() +
  plot_theme +
  labs(
    x = log_population_density_label,
    y = residual_label
  ) +
  geom_text(hjust = "right", nudge_x = -0.15)

ggsave("data/misspecification_scatterplot.png",
  device = image_format,
  width = square_width,
  height = square_height,
  units = image_units
)

correlations <-
  all_data %>%
  group_by(date) %>%
  summarize(
    coefficient = cor(
      population_density,
      cumulative_deaths / population
    )
  )

date_scale(
  ggplot(correlations) +
  aes(
    x = date,
    y = coefficient
  ) +
  geom_line() +
  plot_theme +
  labs(
    x = "Date",
    y = "Correlation coefficient between
cumulative mortality rates and
population density"
  )
)

ggsave("data/correlation_plot.png",
  device = image_format,
  width = long_width,
  height = long_height,
  units = image_units
)