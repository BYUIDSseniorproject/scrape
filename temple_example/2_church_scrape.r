pacman::p_load(tidyverse, rvest, arrow, rworldmap, stringdist)

# Pull the address for the church websites
# https://stackoverflow.com/questions/60397456/scraping-escaped-json-data-within-a-script-type-text-javascript-in-r
temple_address <- "https://www.churchofjesuschrist.org/temples/list?lang=eng" |>
    read_html() |>
    html_node("body") |>
    html_element("script") |>
    html_text() |>
    (function(x) jsonlite::fromJSON(gsub("\\\"", "\"", x, fixed = TRUE)))() |>
    (function(x) x$query$templeList)() |>
    as_tibble() |>
    mutate(
        country = str_trim(country) |>
            str_replace("Verenigde State", "United States"),
        country = ifelse(location == "United States" & (country == "" | is.na(country)), location, country),
        stateRegion = ifelse(str_detect(name, "Burley"), "Idaho", stateRegion),
        city = ifelse(str_detect(name, "Burley"), "Burley", city)
    )

# country issues
text_table <- filter(temple_address,
    country == "" | is.na(country)) |> select(templeNameId)

text_test <- text_table |>
    pull(templeNameId) |>
    str_remove_all("-temple") |>
    str_remove_all("^(.+?)-") |>
    str_replace_all("-", " ")

countries <- c(str_to_lower(rworldmap::countryExData$Country), "vanuatu", "samoa", "republic of the congo", "england", "korea", "singapore", "liberia", "kiribati")

join_country <- text_table |>
    mutate(
        match_text = text_test,
        guess_country = countries[amatch(text_test, countries, maxDist = 20)] |>
            stringr::str_trim()
    )

# state issues
text_state <- filter(temple_address,
    stateRegion == "" | is.na(stateRegion), country == "United States") |> select(templeNameId)

state_or <- paste(state.name, collapse = "|")

temple_address <- temple_address |>
    left_join(join_country) |>
    mutate(country = ifelse(
            country == "" | is.na(country),
            guess_country,
            country),
        missing_state = ifelse(stateRegion == "" | is.na(stateRegion), TRUE, FALSE),
        stateRegion = ifelse(missing_state & country == "United States", city, stateRegion),
        first_word = name |> str_remove("Temple") |> str_remove(state_or),
        city = ifelse(missing_state & country == "United States", first_word ,city),
        city = str_trim(city),
        stateRegion = str_trim(stateRegion),
        country = str_trim(country)) |>
    select(-match_text) |>
    geocode(city = city, state = stateRegion, country = country)

# all look good
filter(temple_address, stateRegion == "Wyoming")
filter(temple_address, stateRegion == "Michigan")
filter(temple_address, stateRegion == "Idaho")
filter(temple_address, stateRegion == "Oregon")

write_csv(temple_address, "temple_example/temple_church.csv")
write_parquet(temple_address, "temple_example/temple_church.parquet")
