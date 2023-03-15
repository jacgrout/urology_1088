get_drugs <- function(id){
  
  req = GET(paste0("https://openprescribing.net/api/1.0/spending_by_sicbl/?code=",id,"&format=json")) 
  data = fromJSON(rawToChar(req$content)) |>
    mutate(drug = id)
  data
}