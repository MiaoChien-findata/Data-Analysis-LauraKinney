# urls = URLs[[l]]
GetParkingTable <- function(urls) {
  out_list <- lapply(urls, GetParkingTable2_) 
  out <- data.table::rbindlist(out_list, fill = TRUE)
  # data.table::setDF(out) # convert data.table object into data.frame
  return(out)
  
}