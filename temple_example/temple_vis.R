library(tidyverse)
library(arrow)

dat <- read_parquet("temple_example/temple_details.parquet")

datp <- dat |>
    select(prophet, prophet_start) |>
    distinct()

(plt <- dat |>
    mutate(isus = ifelse(
        country == "United States",
        "US", "Outside US")) |>
    ggplot(aes(x = announcement, y = squarefootage)) +
    geom_point(aes(color = isus)) +
    geom_smooth(aes(color = isus), span = .75, alpha = 0) +
    geom_vline(xintercept = as.Date(datp$prophet_start)) +
    theme_bw() +
    guides(color = guide_legend(override.aes = list(lwd = NA, size = 2))) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(
        date_breaks = "10 years",
        date_labels = "%Y") +
    labs(x = "Date temple announced", y = "Size (sqft)", color = "In US"))

plt + ggrepel::geom_label_repel(
        data = datp,
        aes(x = as.Date(prophet_start), label = prophet),
        y = 220000, nudge_x = 2000, min.segment.length = 20)

plt +
    scale_x_date(
        date_breaks = "2 years",
          limits = as.Date(c('1/1/1970', '1/1/2023'),
          format = "%d/%m/%Y"),
          date_labels = "%Y") +
    ggrepel::geom_label_repel(
        data = datp,
        aes(x = as.Date(prophet_start), label = prophet),
        y = 120000, nudge_x = 1000, min.segment.length = 200) +
    scale_y_continuous(limits = c(0, 125000))


dat |>
    mutate(isus = ifelse(
        country == "United States",
        "US", "Outside US")) |>
    ggplot(aes(x = announcement, y = squarefootage)) +
    geom_point(aes(color = isus, shape = angelmoroni), size = 3) +
    geom_vline(xintercept = as.Date(datp$prophet_start)) +
    ggrepel::geom_label_repel(
        data = datp,
        aes(x = as.Date(prophet_start), label = prophet),
        y = 120000, nudge_x = 1000, min.segment.length = 200) +
    scale_y_continuous(limits = c(0, 125000)) +
    scale_x_date(
        date_breaks = "2 years",
          limits = as.Date(c('1/1/1995', '1/1/2023'),
          format = "%d/%m/%Y"),
          date_labels = "%Y")


dat |>
    group_by(prophet, prophet_start, prophet_end, interval) |>
    summarize(
        n = n(),
        min = min(squarefootage, na.rm = TRUE),
        max = max(squarefootage, na.rm = TRUE),
        mean = mean(squarefootage, na.rm = TRUE),
        sd = sd(squarefootage, na.rm = TRUE),
        sum = sum(squarefootage, na.rm = TRUE)
    ) |>
    ungroup() |>
    arrange(prophet_start) |>
    mutate(
        temples_per_year = n / (interval / lubridate::years(1)),
        sqft_per_year = sum / (interval / lubridate::years(1))
    ) |>
    select(-prophet_start, -prophet_end, -interval)  |>
    knitr::kable(digits = 0)


filter(dat, squarefootage > 60000, announcement > mdy("01/01/2018")) |>
select(temple, squarefootage, announcement, prophet)
