get_drugs <- function(id){

  req = GET(paste0("https://openprescribing.net/api/1.0/spending_by_sicbl/?code=",id,"&format=json"))
  data = fromJSON(rawToChar(req$content)) |>
    mutate(drug = id)
  data
}

get_drugs_by_gp <- function(id,gp){
req = GET(paste0("https://openprescribing.net/api/1.0/spending_by_practice/?code=",id,"&org=",gp,"&format=json"))
data = fromJSON(rawToChar(req$content)) |>
  mutate(drug = id)
data
}



# get_drugs <- function(id){
# 
# req = GET(paste0("https://openprescribing.net/api/1.0/spending_by_practice/?code=0704020AB&format=json"))
# data = fromJSON(rawToChar(req$content)) |>
#   mutate(drug = "0704020AB")
# data
# }

#&date=2022-04-01


bar_chart <- function(value, color = "red", display_value = NULL){
  
  # Choose to display percent of total
  if (is.null(display_value)) {
    display_value <- "&nbsp;"
  } else {
    display_value <- display_value
  }
  
  # paste color and value into the html string
  glue::glue("<span style=\"display: inline-block; direction: ltr; border-radius: 4px; padding-right: 2px; background-color: {color}; color: {color}; width: {value}%\"> {display_value} </span>")
}

# create a color palette w/ paletteer
# note you could just pass a single color directly to the bar_chart function
col_pal <- function(value,posnegcol){
  # set high and low
  domain_range <- range(c(min(value), max(value)
  ))
  # create the color based of domain
  scales::col_numeric(
    paletteer::paletteer_d(posnegcol) %>% as.character(), 
    domain = c(min(value), max(value))
  )(value)
}