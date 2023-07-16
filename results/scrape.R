library(tidyverse)
library(rvest)
library(purrr)



url <- "https://en.wikipedia.org/wiki/Web_scraping"

source <- read_html(url)

(h2_headers <- source |>
    html_elements("h2") |>
    html_text2())


# Challenge
#  Print the names of 20 random wikipedia articles
urlr <- "https://en.wikipedia.org/wiki/Special:Random"

r20 <- function(x) {
    read_html(x) |>
    html_element("h1") |>
    html_text2()
}

(title20 <- purrr::map_chr(rep(urlr, 20), ~r20(.x)))

# How many a tags (links) are in this Wikipedia Article? Don't count all the links on the webpage, just those found in the contents of the article.
urla <- "https://en.wikipedia.org/wiki/Brigham_Young_University%E2%80%93Idaho"
# use selector to find node
read_html(urla) |>
    html_nodes("#content") |>
    html_elements("a") |>
    html_text2()

# Collect the transcripts from all episodes of the Simpsons
urls <- "https://simpsons.fandom.com/wiki/Category:Transcripts"

episode_urls <- paste0("https://simpsons.fandom.com", read_html(urls) |>
    html_nodes(".category-page__member-link") |>
    html_attr("href"))

# read_html(episode_urls[1]) |>
#     html_nodes(".mw-parser-output") |>
#     html_elements("p") |>
#     html_text()

get_text <- function(x) {
    read_html(x) |>
    html_nodes(".mw-parser-output") |>
    html_elements("p") |>
    html_text()
}
trans <- purrr::map(episode_urls, ~get_text(.x))

write_lines(trans, "test.txt")
