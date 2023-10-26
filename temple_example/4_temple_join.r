pacman::p_load(tidyverse, tidygeocoder, arrow, rworldmap, sf)

tdat <- read_parquet("temple_example/temple_details.parquet")
temple_address <- read_parquet("temple_example/temple_church.parquet") |>
        mutate(
            lat = ifelse(name == "Heber Valley Utah Temple", 40.499000, lat),
            long = ifelse(name == "Heber Valley Utah Temple", -111.433790, long),
        ) |>
        rename(lat_general = lat, long_general = long)

temple_details <- tdat |>
    left_join(rename(temple_address, temple = name))

# missing a few 
# https://www.mormonwiki.com/Geographical_List_of_Temples
count(us_temples, stateRegion) |> print(n = 100)


# now find the census information

sp_dat <- read_rds("cbg/dat.rds")

templl <- temple_details |>
    filter(country == "United States") |>
    select(temple, lat, long) |>
    st_as_sf(coords=c("long", "lat"), crs="EPSG:4269")

tract_match <- sf::st_join(templl, sp_dat)

temple_details <- temple_details |>
    left_join(select(tract_match, temple, geometry, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, GEOID)) |>
    select(-geometry)

write_parquet(temple_details, "temple_example/temple_details_spatial.parquet")
write_csv(temple_details, "temple_example/temple_details_spatial.csv")


