# install.packages(pacman)

pacman::p_load(tidyverse, rvest, lubridate, magick, arrow, rworldmap, stringi)

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

temples <- temple_dim |>
    left_join(temple_districts) |>
    left_join(temple_elevations) |>
    left_join(temple_features) |>
    left_join(temple_time) 

t_url <- pull(temples, temple) |>
str_to_lower() |>
str_replace_all(" ", "-")

temple_urls <- paste0("https://churchofjesuschristtemples.org/", t_url) |>
    stri_trans_general(id = "Latin-ASCII")

stri_trans_general(temple_urls[269], id = "Latin-ASCII")

text <- temple_urls[269] |>
  read_html() |>
  rvest::html_elements(".feature_box_wrapper") |>
  html_text2()

get_html_text <- function(x) {
    print(x)
    rvest::read_html(x) |>
        rvest::html_elements(".feature_box_wrapper") |>
        rvest::html_text2()
}

get_address <- function(text) {
    text |>
        stringr::str_remove("Address\n|Location\n") |>
        stringr::str_split("Telephone|\n\n", n = 2) |>
        unlist() |>
        (\(x) x[1])() |>
        stringr::str_replace_all("\n", ",") |>
        stringr::str_replace_all(",", ", ") |>
        stringr::str_squish() |>
        stringr::str_remove("(,+$)")
}

temple_html <- map(temple_urls, get_html_text)
temple_address <- map(temple_html, get_address)

spatial_dat <- tibble(
        temple =  pull(temples, temple),
        temple_urls = temple_urls,
        temple_address = unlist(temple_address)) |>
    geocode(address = temple_address, method = "arcgis")

temples <- temples |>
    left_join(spatial_dat)
# https://churchofjesuschristtemples.org/aba-nigeria-temple/
# https://churchofjesuschristtemples.org/washington-d.c.-temple/
# https://churchofjesuschristtemples.org/heber-valley-utah-temple/
# https://churchofjesuschristtemples.org/port-moresby-papua-new-guinea-temple/
# https://churchofjesuschristtemples.org/anchorage-alaska-temple/
# https://churchofjesuschristtemples.org/asuncion-paraguay-temple/
# https://churchofjesuschristtemples.org/vitoria-brazil-temple/

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

write_csv(dat, "temple_example/temple_details.csv")
write_parquet(dat, "temple_example/temple_details.parquet")



