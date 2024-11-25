UKCEH_colours <- function(hab_rast, short_list) {
  
  if(short_list) {
    habs <- c("Deciduous woodland", "Coniferous woodland", "Arable", "Improved grassland", 
              "Semi-natural grassland", "Mountain, heath and bog", "Saltwater", "Freshwater", "Coastal", 
              "Built-up areas and gardens",  "Cover crop", "Woodland with release pen")
  } else {
    habs <- c("Deciduous woodland", "Coniferous woodland", "Arable", "Improved grassland", 
              "Neutral grassland", "Calcareous grassland", "Acid grassland", "Fen, marsh & swamp", 
              "Heather", "Heather grassland", "Bog", "Inland rock", "Saltwater", "Freshwater", 
              "Supralittoral rock", "Supralittoral sediment", "Littoral rock", "Littoral sediment", 
              "Saltmarsh", "Urban", "Suburban", "Cover crop", "Woodland with release pen")
  }
  
  
  red <- c(255, 0, 115, 0, 127, 112, 153, 255, 128, 230, 0, 210, 0, 0, 204, 204, 255, 255, 128, 0, 128, 255, 204)
  green <- c(0, 102, 38, 255, 229, 168, 129, 255, 26, 140, 128, 210, 0, 0, 179, 179, 255, 255, 128, 0, 128, 255, 102)
  blue <- c(0, 0, 0, 0, 127, 0, 0, 0, 128, 166, 115, 255, 128, 255, 0, 0, 128, 128, 255, 0, 128, 0, 0)
  
  cols <- rgb(red, green, blue, maxColorValue = 255)
  
  if(short_list) {
    cols <- cols[c(1:5, 9, 13, 14, 15, 20, 22, 23)]
  } 
  
  unique_vals <- unique(hab_rast %>%
                          ifel(. == 30, ifelse(short_list, 11, 23), .) %>%
                          ifel(. == 33, ifelse(short_list, 12, 22), .))[,1]
  
  habs <- habs[unique_vals]
  cols <- cols[unique_vals]
  
  return(data.frame(habitat = habs, colour = cols))
}



LC_to_AC <- function(hab_rast) {
  AC_hab_rast <- hab_rast %>%
    ifel(. %in% 5:8, 5, .) %>%
    ifel(. %in% 9:12, 6, .) %>%
    ifel(. %in% 13:14, .-6, .) %>%
    ifel(. %in% 15:19, 9, .) %>%
    ifel(. %in% 20:21, 10, .) %>%
    ifel(. == 30, 11, .)
  
  return(AC_hab_rast)
}