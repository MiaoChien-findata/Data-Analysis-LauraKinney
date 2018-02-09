# url = URLs[[89]][1]

GetParkingTable_ = function(url){
  url = url[1]
  remDr$navigate(url)
  Sys.sleep(2)
  
  element.tables = remDr$findElements(using="css selector", ".text2")
  
  y = substr(url, regexpr("txtByear=",url)+9, regexpr("txtBmonth",url)-2)
  m = substr(url, regexpr("txtBmonth=",url)+10, regexpr("txtBday",url)-2)
  d = substr(url, regexpr("txtBday=",url)+8, regexpr("selList",url)-2)
  date = paste0(y,"-",m,"-",d) %>% as.Date()
  
  out_schema <- data.table(
    "name"= as.character(0L),
    "code"= as.character(0L),
    "order"= as.numeric(0L), 
    "number"= as.character(0L), 
    "plate"= as.character(0L), 
    "entered_at"= as.character(NA),
    "exited_at"= as.character(NA),
    "hours"= as.numeric(0L), 
    "discount"= as.character(0L), 
    "discount_hours"= as.numeric(0L), 
    "paid_amount"= as.numeric(0L),
    "checkout"= date
  )
  
  options(warn = -1)
  tryCatch({
    out <- GetTable(element.tables, url)
    out$checkout = date
    print(paste("Lot", out$code %>% unique(), date, " Done."))
  }, error = function(e) {
    out <<- out_schema
    print("error here, return out_schema")
  })
  
  # if(length(element.tables)==0){
  #   logerror(sprintf("This url is invalid: %s", url))
  # }
  
  return(out)
  
  
}







