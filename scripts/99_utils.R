#' 99_utils.R
#' =======================================================
#+ setup, include=FALSE
# knitr::opts_chunk$set(eval = FALSE)
#+


# ---- source_utils ----

library(sp)
suppressMessages(library(dplyr))
library(gstat)
library(ggplot2)
suppressMessages(library(sf))
suppressMessages(library(LAGOSNEgis))
suppressMessages(library(LAGOSNE))
suppressMessages(library(lwgeom))
library(classInt)
suppressMessages(library(tidyr))
library(stringr)
suppressMessages(library(cowplot))
suppressWarnings(library(mapview))
library(macroag)
library(broom)
suppressMessages(library(ggsn))
suppressMessages(library(nhdR))
library(FedData)
suppressMessages(library(raster))
library(progress)
suppressMessages(library(smoothr))
library(assertr)
suppressMessages(library(HydroData)) # install_github("mikejohnson51/HydroData")
suppressMessages(library(kableExtra))
library(vapour)
library(cdlTools)
suppressMessages(library(unpivotr))
library(snakecase)
suppressMessages(library(brms))
suppressMessages(library(tidybayes))
library(ggforce)
library(rnaturalearth)
library(gghighlight)

theme_opts <- theme(axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    panel.background = element_blank(),
                    legend.text = element_text(size = 8),
                    legend.title = element_text(size = 10),
                    legend.key.size = unit(0.7, "line"),
                    plot.margin = unit(c(0, -0.13, 0, -0.13), "cm"))
                    # plot.margin = unit(c(0, 0, -2, 0), "cm")) # t, r, b, l

signif_star <- function(x){
  if(!is.na(x)){
    if(x){
      "*"
    }else{
      ""
    }
  }else{
    ""
  }
}

county_sf <- function(){
  county_sf        <- st_as_sf(maps::map("county", fill = TRUE, plot = FALSE))
  county_sf        <- tidyr::separate(county_sf, ID, c("state", "county"), ",")
  county_sf$county <- gsub("\\.", "", gsub(" ", "", county_sf$county))
  county_sf        <- st_make_valid(county_sf)

  county_key <- data.frame(state = tolower(state.name), state_abb = state.abb,
                           stringsAsFactors = FALSE)
  county_sf <- left_join(county_sf, county_key)
  # county_sf <- county_sf[
  #   unlist(lapply(
  #     st_intersects(county_sf, iws),
  #     function(x) length(x) > 0)),]
  county_sf
}

state_sf <- function(){
  state_sf <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))
  key <- data.frame(ID = tolower(state.name),
                    ABB = state.abb, stringsAsFactors = FALSE)
  left_join(state_sf, key, by = "ID")
}

get_states <- function(bbox){
  state_sf <- sf::st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))
  key <- data.frame(ID = tolower(state.name),
                    ABB = state.abb, stringsAsFactors = FALSE)
  state_sf <- left_join(state_sf, key, by = "ID")
  bbox <- st_transform(st_as_sfc(bbox), st_crs(state_sf))

  state_sf <- state_sf[unlist(lapply(
    st_intersects(state_sf, bbox),
    function(x) length(x) > 0)),]

  state_sf$ABB
}

get_bbox <- function(boundary_iws){
  st_bbox(boundary_iws)
}

clip_to_iws <- function(iws, r){
  raster::mask(r, as_Spatial(iws))
}

get_raster_name <- function(r, path){
  paste0(path, r@data@names)
}

get_iws <- function(lagoslakeid){
  LAGOSNEgis::query_wbd(lagoslakeid, utm = FALSE)
}

state_key <- function(){
  data.frame(state = datasets::state.abb, 
             state_name = datasets::state.name)
}

# ---- advanced_gis ----

# kg per ha is false, kg is true
interp_to_iws <- function(raw, varname = "total", outname, is_extensive = TRUE){
  # raw <- animals_tidy
  # varname = "hog_sales"
  # is_extensive = TRUE
  print(varname)
  
  # raw <- dplyr::filter(animals_raw, county_name %in% c("LYON", "MURRAY", "PIPESTONE", "LINCOLN", "REDWOOD", "YELLOW MEDICINE") & state == "minnesota")
  
  raw <- raw[!is.na(raw[,varname])[,1],]
  raw <- raw[
    unlist(lapply(st_intersects(raw, iws), function(x) length(x) > 0)),]
  iws_sub <- iws[sapply(st_intersects(iws, raw), length) != 0,]
  # mapview::mapview(raw, zcol = "hog_sales") +
  #   mapview::mapview(iws)
  # median(raw$hog_sales)
  
  # st_interpolate_aw
  iws_interp <- suppressWarnings(
    st_interpolate_aw(raw[,varname], iws_sub,
                      extensive = is_extensive)) # kg per ha is false, kg is true
  iws_interp <- data.frame(outname = iws_interp[,varname],
                           lagoslakeid = iws_sub$lagoslakeid,
                           stringsAsFactors = FALSE)
  
  names(iws_interp)[1] <- outname
  iws_interp <- dplyr::select(iws_interp, lagoslakeid, everything())
  iws_interp <- dplyr::rename(iws_interp, geometry = outname.geometry)
  st_as_sf(iws_interp)
}

# https://gist.github.com/gadenbuie/284671997992aefe295bed34bb53fde6
backstitch <- function(
  infile,
  outfile = NULL,
  output_type = c('both'),
  chunk_header = "# ----"
) {
  requireNamespace('knitr', quietly = TRUE)
  requireNamespace('stringr', quietly = TRUE)
  stopifnot(output_type %in% c('script', 'code', 'both'))

  if (is.null(outfile) && output_type == 'both')
    stop("Please choose output_type of 'script' or 'code' when not outputting to a file.")

  knitr::knit_patterns$set(knitr::all_patterns[['md']])

  x <- readLines(infile)
  if (inherits(infile, 'connection')) close(infile)

  empty_lines <- which(stringr::str_detect(x, "^\\s?+$"))
  last_non_empty_line <- max(setdiff(seq_along(x), empty_lines))
  x <- x[1:last_non_empty_line]

  x_type <- rep('text', length(x))

  # Find YAML section
  yaml_markers <- which(stringr::str_detect(x, "^[-.]{3}\\s*$"))
  if (length(yaml_markers) > 2) {
    message("Input file may have multiple YAML chunks, only considering lines",
            paste(yaml_markers[1:2], collapse='-'), 'as YAML header.')
  }
  if (length(yaml_markers) > 0) {
    i.yaml <- yaml_markers[1]:yaml_markers[2]
    x_type[i.yaml] <- 'yaml'
  }

  # Mark code chunk.begin, chunk.end and regular chunk codelines
  i.chunk.begin <- which(stringr::str_detect(x, knitr::knit_patterns$get('chunk.begin')))
  i.chunk.end   <- which(stringr::str_detect(x, knitr::knit_patterns$get('chunk.end')))
  x_type[i.chunk.end] <- 'chunk.end'
  for (j in i.chunk.begin) {
    j.chunk.end <- min(i.chunk.end[i.chunk.end > j])-1
    x_type[j:j.chunk.end] <- 'chunk'
  }
  x_type[i.chunk.begin] <- 'chunk.begin'

  # Check for inline code
  i.inline <- which(stringr::str_detect(x, knitr::knit_patterns$get('inline.code')))
  i.inline <- intersect(i.inline, which(x_type == 'text'))
  x_type[i.inline] <- 'inline'

  # Check empty lines
  i.empty <- which(stringr::str_detect(x, "^\\s*$"))
  i.empty <- intersect(i.empty, which(x_type == 'text'))
  x_type[i.empty] <- 'empty'

  really_empty <- function(x_type, j, n = -1) {
    if (grepl('(chunk|yaml)', x_type[j + n])) {
      return('empty')
    } else if (n < 0) {
      return(really_empty(x_type, j, 1))
    } else if (x_type[j + n] %in% c('text', 'inline')) {
      return('text')
    } else {
      return(really_empty(x_type, j, n+1))
    }
  }

  for (j in i.empty) {
    x_type[j] <- really_empty(x_type, j)
  }

  # Rewrite lines helper functions
  comment <- function(x) paste("#'", x)
  make_chunk_header <- function(x, chunk_header) {
    stringr::str_replace(stringr::str_replace(x, knitr::knit_patterns$get('chunk.begin'), "\\1"),
                         "^r[, ]?", paste(chunk_header, ""))
  }
  # Rewrite lines
  y <- x
  regex_inline_grouped <- "`r[ ]?#?(([^`]+)\\s*)`"
  i.empty       <- which(x_type == 'empty')
  i.text        <- which(x_type == 'text')
  y[i.chunk.begin] <- make_chunk_header(x[i.chunk.begin], chunk_header)
  y[i.inline]      <- comment(stringr::str_replace_all(x[i.inline], regex_inline_grouped, "{{\\1}}"))
  y[i.text]        <- comment(x[i.text])
  if (length(yaml_markers) > 0) y[i.yaml] <- comment(x[i.yaml])
  y[i.empty]       <- ""
  y[i.chunk.end]   <- ""

  y_code <- y[which(stringr::str_detect(x_type, 'chunk'))]

  if (!is.null(outfile)){
    outfile_name <- stringr::str_replace(outfile, "(.+)\\.R$", "\\1")
    if (output_type == "script") {
      cat(c(y, ""), file = paste0(outfile_name, ".R"), sep = '\n')
    } else if (output_type == "code") {
      cat(c(y_code, ""), file = paste0(outfile_name, ".R"), sep = '\n')
    } else {
      cat(c(y, ""), file = paste0(outfile_name, ".R"), sep = '\n')
      cat(c(y_code, ""), file = paste0(outfile_name, "_code.R"), sep = '\n')
    }
  } else {
    switch(
      output_type,
      'script' = unname(y),
      'code' = unname(y_code)
    )
  }
}

# jsta::get_if_not_exists
get_if_not_exists <- function(x, destfile, read_function = readRDS, 
                              overwrite = FALSE, ...){
  
  if(is.function(x)){
    if(!file.exists(destfile) | overwrite){
      res <- x(destfile, ...)
      return(res)
    }else{
      message(paste0("A local evaulation of x already exists on disk"))
      return(read_function(destfile))
    }
  } 
  
  if(!is.function(x)){
    if(!file.exists(destfile) | overwrite){
      download.file(x, destfile)
    }else{
      message(paste0("A local copy of ", x, " already exists on disk"))
    }
    invisible(x)
  }
}
