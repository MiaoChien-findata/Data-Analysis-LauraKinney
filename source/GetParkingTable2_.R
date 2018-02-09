# url = URLs[[1]][1]
# url = urls[1]
GetParkingTable2_ = function(url){
  url = url[1]
  remDr$navigate(url)
  sec = myfunction()
  print(sec)
  Sys.sleep(sec)
  
  html = remDr$getPageSource()[[1]] %>% read_html() %>% html_table()
  
  
  y = substr(url, regexpr("txtByear=",url)+9, regexpr("txtBmonth",url)-2)
  m = substr(url, regexpr("txtBmonth=",url)+10, regexpr("txtBday",url)-2)
  d = substr(url, regexpr("txtBday=",url)+8, regexpr("selList",url)-2)
  date = paste0(y,"-",m,"-",d) %>% as.Date()
  
  out_schema <- data.table(
    "name"= as.character(NA),
    "code"= as.character(NA),
    "order"= as.numeric(NA), 
    "number"= as.character(NA), 
    "plate"= as.character(NA), 
    "spot"= as.character(NA),
    "entered_at"= as.character(NA),
    "exited_at"= as.character(NA),
    "hours"= as.numeric(NA), 
    "discount"= as.character(NA), 
    "discount_hours"= as.numeric(NA), 
    "paid_amount"= as.numeric(NA),
    "receipt"=as.character(NA), 
    "vat"=as.character(NA), 
    "crewid"=as.numeric(NA),
    "checkout"= date
  )
  
  options(warn = -1)
  tryCatch({
    out <- GetTable2(html, url)
    out$checkout = date
    print(paste("Lot", out$code %>% unique(), date, " Done."))
  }, error = function(e) {
    out <<- out_schema
    out$name = remDr$getPageSource()[[1]] %>%
                read_html() %>% html_node("body") %>% html_text() %>%
                gsub("\\s","",.) %>%  str_extract(., "^.*-") %>% gsub("-","",.)
    out$code = substr(url, regexpr("selList=", url)+8, regexpr("&selList2", url)-1)
    
    print("error here, return out_schema")
  })
  

  return(out)
  
  
}







