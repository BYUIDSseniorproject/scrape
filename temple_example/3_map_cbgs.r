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
    sf::st_read(shape_name,quiet=TRUE)
    setwd(old_wd)
}

paste_file <- function(x, url = "https://www2.census.gov/geo/tiger/TIGER2018/BG") {
    paste0(url, "/", x)
}

# https://www2.census.gov/geo/tiger/
# https://www2.census.gov/geo/tiger/TIGER2018/BG/

url <- "https://www2.census.gov/geo/tiger/TIGER2018/BG"
ftp_html <- rvest::read_html(url) |> html_elements("a") |> html_text()
ftp_html <- ftp_html[str_detect(ftp_html, ".zip")]


# test <- paste0(url, "/", ftp_html[1])
# bob <- get_shapefile(test)

paths <- map(ftp_html, paste_file)
dat_list <- map(paths, get_shapefile)

dat <- bind_rows(dat_list)

write_rds(dat, "cbg/dat.rds")

