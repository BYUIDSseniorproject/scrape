# install.packages(pacman)

pacman::p_load(tidyverse, rvest, lubridate, magick, tidygeocoder, arrow, rworldmap)

base_url <- "https://churchofjesuschristtemples.org/statistics/"

# Times that temples were opened
temple_time <- paste0(base_url, "milestones/") |>
  read_html() |>
  html_nodes("table") |>
  html_table() %>% .[[2]] |>
  rename_all(str_to_lower) |>
  mutate(announcement = dmy(announcement), groundbreaking = dmy(groundbreaking),
         dedication = dmy(dedication))

temple_dim <- paste0(base_url, "dimensions/")  |>
    read_html() |>
    html_nodes("table") |>
    html_table() |>
    (function(x) x[[1]])() |>
    rename_all(str_to_lower) |>
    mutate(
        instructionrooms = as.numeric(instructionrooms),
        sealingrooms = as.numeric(sealingrooms),
        baptismrooms = as.numeric(baptismrooms),
        squarefootage = squarefootage |> str_remove(",") |> as.numeric(),
        acreage = as.numeric(acreage)
    )

temple_districts <- paste0(base_url, "districts/") |>
    read_html() |>
    html_nodes("table") |>
    html_table() |>
    (function(x) x[[1]])() |>
    rename_all(str_to_lower) |>
    rename(all_units = `all units`) |>
    mutate(
        temple = str_remove(temple, " District"),
        stakes = as.numeric(stakes),
        districts = as.numeric(districts),
        all_units = as.numeric(all_units)
    )

temple_elevations <- paste0(base_url, "elevations/") |>
    read_html() |>
    html_nodes("table") |>
    html_table() |>
    (function(x) x[[1]])() |>
    rename_all(str_to_lower) |>
    select(-`elevation(meters)`) |>
    rename(elevation_ft = `elevation(feet)`) |>
    mutate(
        elevation_ft = elevation_ft |> str_remove(",|ft.") |> as.numeric()
    )

temple_features <- paste0(base_url, "features/") |>
    read_html() |>
    html_nodes("table") |>
    html_table() |>
    (function(x) x[[2]])() |>
    rename_all(str_to_lower) |>
    rename(
        tower_spire = `number ofspires/towers`,
        att_tower_spire = `spire/towerattachment`
        ) |>
    mutate(
        tower_spire = as.numeric(tower_spire)
    )

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
            str_replace("Verenigde State", "United States")
    )

filter(temple_address, country == "", status == "CONSTRUCTION")
text_test <- filter(temple_address, country == "") |> pull(templeNameId)

countries <- str_to_lower(rworldmap::countryExData$Country)


guess_country <- text_test |>
    str_remove("-temple") |>
    str_split_fixed("-", 4) |>
    as_tibble() |>
    mutate(
        new_v2 = V2 == "new",
        new_v3 = V3 == "new",
        Vcomb = ifelse(
            new_v2,
            paste(V2, V3),
            ifelse(new_v3,
                paste(V3, V4),
                NA)
                ),
        uss_v2 = V2 %in% str_to_lower(state.name),
        uss_v3 = V3 %in% str_to_lower(state.name),
        c_v2 = V2 %in% countries,
        c_v3 = V3 %in% countries,
        c_v4 = V4 %in% countries,
        uss_vc = Vcomb %in% str_to_lower(state.name),
        c_vc = Vcomb %in% countries,
        sum = uss_v2 + uss_v3 + c_v2 + c_v3 + c_v4 + c_vc,
        templeNameId = text_test
    ) |>
    pivot_longer(uss_v2:c_vc, names_to = "type", values_to = "condition") |>
    filter(condition) |>
    group_by(templeNameId) |>
    summarise(
        type = type  |>
            str_extract("c|uss") |>
            unique() |>
            sort() |>
            paste(collapse = "-")
    ) |>
    mutate(type = case_when(
        str_detect(type, "uss") ~ "United States",
        str_detect(type, "c") ~ "International"
    ))

temple_address <- temple_address |>
    left_join(guess_country) |>
    mutate(
        country = ifelse(country == "", type, country)
    )

temples <- temple_dim |>
    left_join(temple_districts) |>
    left_join(temple_elevations) |>
    left_join(temple_features) |>
    left_join(temple_time) |>
    left_join(rename(temple_address, temple = name)) |>
    geocode(city = city, country = country)

### Get prophet dates

pro <- "https://en.wikipedia.org/wiki/List_of_presidents_of_the_Church_of_Jesus_Christ_of_Latter-day_Saints" |>
    read_html() |>
    html_nodes("table") |>
    html_table() |>
    (function(x) x[[1]])() |>
    select(-Portrait) |>
    rename_all(~str_to_lower(.)) |>
    rename_all(~str_replace_all(., " ", "_")) |>
    rename_all(~str_replace_all(., "\\.", "")) |>
    mutate(
        president_of_the_church = ifelse(is.na(no), NA, president_of_the_church),
        birth = ifelse(is.na(no), NA, birth),
        ordination = ifelse(is.na(no), lag(death, 1), ordination),
        death = ifelse(is.na(no), lead(ordination, 1), death),
        position = ifelse(is.na(no), "twelve", "church"),
        ordination = mdy(str_remove(ordination, "\\((.*)")),
        length = str_remove(length, "\\((.*)"),
        death = mdy(str_remove(death, "\\((.*)")),
        birth = mdy(birth),
        death = case_when(is.na(death) ~ as.Date(now()), TRUE ~ death),
        interval = interval(ordination, death)
    ) |>
    fill(no, president_of_the_church, birth, death, .direction = "up") |>
    rename(start = ordination, end = death)


int_merge <- function(x) {
  if(length(x) == 1) return(x)
  x <- x[order(int_start(x))]
  y <- x[1]
  for(i in 2:length(x)){
    if(int_overlaps(y[length(y)], x[i]))
      y[length(y)] <- interval(start = min(int_start(c(y[length(y)], x[i]))),
                               end = max(int_end(c(y[length(y)], x[i]))))
    else
      y <- c(y, x[i])
  }
  return(y)
}
# https://github.com/dgrtwo/fuzzyjoin
intervals <- select(pro, president_of_the_church, interval) |>
    group_by(president_of_the_church) |>
    summarise(interval = int_merge(interval)) |>
    pivot_wider(
        names_from = "president_of_the_church",
        values_from = "interval")

dat <- bind_cols(temples, intervals) |>
    pivot_longer(
        `Brigham Young`:`Wilford Woodruff`,
        names_to = "prophet",
        values_to = "interval") |>
    filter(announcement %within% interval) |>
    mutate(
        prophet_start = int_start(interval),
        prophet_end = int_end(interval))

dat <- dat |>
    mutate(country = ifelse(
        str_detect(temple, "Freetown|Lubumbashi|Papua|Vanuatu"),
        "International", country))

write_csv(dat, "temple_example/temple_details.csv")
write_parquet(dat, "temple_example/temple_details.parquet")
