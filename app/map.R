# functions to map occupation data =====

get_bins <- function(x, w, r, bin_max, na.rm = TRUE) {
  x1 <- x[!is.na(x) & !is.na(w)]
  w1 <- w[!is.na(x) & !is.na(w)]
  weighted_mean <- weighted.mean(x1, w1)
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  bins <- c(0, weighted_mean * r, bin_max)
  bins
}

map_metro <- function(df, shape_metro, shape_state, code_name, type) {
  type <- sym(type)
  
  cleaned_data <- df %>% 
    filter(occ_code_name == code_name)
  
  shape_data <- shape_metro %>% 
    left_join(cleaned_data, by = c("GEOID" = "area_code")) %>% 
    filter(!str_detect(NAME, ", AK|, HI|, PR")) %>% 
    filter(!is.na(emp_total), !is.na(!!type)) %>%
    arrange(!!type)
  
  if (type == "emp_rate") {
    bins <- get_bins(
      x = shape_data[[type]],
      w = shape_data[["emp_total"]],
      r = c(1/2, 7/10, 9/10, 10/9, 10/7, 2/1),
      bin_max = 5000
    ) %>% 
      round(2)
  } else {
    bins <- get_bins(
      x = shape_data[[type]],
      w = shape_data[["employment"]],
      r = c(75/100, 85/100, 95/100, 100/95, 100/85, 100/75),
      bin_max = 500000
    ) %>% 
      round(-2)
  }
  
  map_pal <- colorBin(
    palette = RColorBrewer::brewer.pal(7, "RdPu"),
    domain = shape_data[[type]],
    bins = bins
  )
  
  map_lab <- str_c(
    "<b>", shape_data[["area_name"]], "</b><br/>",
    "Employment rate: ", format(round(shape_data[["emp_rate"]], 1), nsmall = 1), "<br/>",
    "Median salary: ", format(round(shape_data[["wage_p50"]]), big.mark = ",")
  ) %>%
    map(htmltools::HTML)
  
  epsg2163 <- leafletCRS(
    crsClass = "L.Proj.CRS",
    code = 'EPSG:2163',
    proj4def = '+proj=laea +lat_0=40 +lon_0=-98 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs',
    resolutions = c(65536, 32768, 16384, 8192, 4096, 2048,1024, 512, 256, 128)
  )
  
  leaflet(data = shape_data, options = leafletOptions(minZoom = 3, maxZoom = 6, crs = epsg2163)) %>% 
    setView(-95, 38.5, zoom = 4) %>% 
    addCircles(
      radius = ~sqrt(emp_total) * 40,
      stroke = FALSE,
      fill = TRUE,
      fillColor = ~map_pal(shape_data[[type]]),
      fillOpacity = 1
    ) %>% 
    addPolygons(
      data = shape_state,
      color = "#444444",
      weight = 0.5,
      opacity = 1,
      smoothFactor = 0,
      fillOpacity = 0
    ) %>% 
    addCircles(
      data = shape_data,
      radius = ~sqrt(emp_total) * 40,
      stroke = FALSE,
      opacity = 0,
      fillOpacity = 0,
      label = map_lab,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", "font-size" = "12px")
      )
    ) %>%
    addLegend(
      position = "bottomright",
      pal = map_pal,
      values = ~shape_data[[type]],
      opacity = 1,
      title = "Stat"
    )
}
