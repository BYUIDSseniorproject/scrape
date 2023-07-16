library(tidyverse)
library(rvest)

url <- "https://en.wikipedia.org/wiki/Web_scraping"

source <- read_html(url)

(h2_headers <- source |>
    html_elements("h2") |>
    html_text2())

#  Print the names of 20 random wikipedia articles
urlr <- "https://en.wikipedia.org/wiki/Special:Random"

read_html(urlr) |>
    html_elements("h1") |>
    html_text2()

myfunc <- function(urlr) {
    read_html(urlr) |>
    html_elements("h1") |>
    html_text2()
}


t20 <- purrr::map_chr(rep(urlr, 20), ~myfunc(.x))

# How many a tags (links) are in this Wikipedia Article? Don't count all the links on the webpage, just those found in the contents of the article.
url <- "https://en.wikipedia.org/wiki/Brigham_Young_University%E2%80%93Idaho"

read_html(url) |>
    html_elements("#bodyContent") |>
    html_elements('a') |>
    html_text() |>
    length()

# Collect the transcripts from all episodes of the Simpsons
urle <- "https://simpsons.fandom.com/wiki/Category:Transcripts"

urlt <- paste0(urle, read_html(urle) |>
    html_elements(".category-page__member-link") |>
    html_attr("href"))

