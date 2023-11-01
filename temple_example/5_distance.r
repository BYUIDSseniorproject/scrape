library(sf)
library(tidyverse)

pacman::p_load(tidyverse, rvest, arrow, rworldmap, stringdist, archive, sf)

get_shapefile <- function(path){
    
    options(timeout = max(500, getOption("timeout")))
    print(path)

    temp_dir <- tempdir()
    dest_dir <- tempfile()
    dir.create(dest_dir)

    if(grepl('http',path)){
        dest <- paste0(dest_dir,"/",basename(path))
        utils::download.file(path, dest, quiet=T)
        path <- dest
    }

    if(grepl('.tgz$|.tar.gz$',path)){
        utils::untar(path, exdir = dest_dir)
    } else if(grepl('.zip$',path)){
        utils::unzip(path, exdir = dest_dir)
    } else{
        stop('Unsupported filetype')
    }
    old_wd <- getwd()
    shape_name = grep('.shp$',list.files(dest_dir),value=T)
    setwd(dest_dir)
    out <- sf::st_read(shape_name,quiet=TRUE)
    setwd(old_wd)
    out
}

paste_file <- function(x, url = "https://www2.census.gov/geo/tiger/TIGER2018/TRACT") {
    paste0(url, "/", x)
}

# https://www2.census.gov/geo/tiger/
# https://www2.census.gov/geo/tiger/TIGER2018/TRACT/

url <- "https://www2.census.gov/geo/tiger/TIGER2018/TRACT"
ftp_html <- rvest::read_html(url) |> html_elements("a") |> html_text()
ftp_html <- ftp_html[str_detect(ftp_html, ".zip")]


# test <- paste0(url, "/", ftp_html[1])
# bob <- get_shapefile(test)
# EPSG:9311 https://epsg.io/9311
# http://downloads2.esri.com/support/documentation/ao_/710Understanding_Map_Projections.pdf


paths <- map(ftp_html, paste_file)
dat_list <- map(paths, get_shapefile)

dat <- bind_rows(dat_list) |>
    mutate(
        centroid = st_centroid(geometry)
    )

write_rds(dat, "tract/dat_2018.rds")

## Calculate Distance
tracts <- read_rds("tract/dat_2018.rds")
st_geometry(tracts) <- "centroid"

tracts <- tracts |>
    select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAME, centroid) |>
    rename(geometry = centroid) |>
    st_transform("EPSG:9311")

tdat <- read_parquet("temple_example/temple_details.parquet")
temple_address <- read_parquet("temple_example/temple_church.parquet") |>
        mutate(
            lat = ifelse(name == "Heber Valley Utah Temple", 40.499000, lat),
            long = ifelse(name == "Heber Valley Utah Temple", -111.433790, long),
        ) |>
        rename(lat_general = lat, long_general = long)
temple_details <- tdat |>
    left_join(rename(temple_address, temple = name)) |>
    filter(country == "United States") |>
    select(temple, templeNameId, lat, long) |>
    st_as_sf(coords = c("long", "lat"), crs = "EPSG:4269") |>
    st_transform("EPSG:9311")


distance_matrix <- st_distance(tracts, temple_details) |>
    as_tibble()

colnames(distance_matrix) <- pull(temple_details, templeNameId)

nearest_temple <- distance_matrix |>
    mutate(tract = pull(tracts, GEOID)) |>
    select(tract, everything()) |>
    pivot_longer(
        `albuquerque-new-mexico-temple`:`yorba-linda-california-temple`) |>
    group_by(tract) |>
    arrange(value) |>
    slice(1) |>
    ungroup() |>
    rename(meters = value) |>
    mutate(miles = units::set_units(meters, mile)) |>
    arrange(miles)

write_parquet(nearest_temple, "temple_example/tract_distance_to_nearest_temple.parquet")

# read_parquet("temple_example/tract_distance_to_nearest_temple.parquet")
