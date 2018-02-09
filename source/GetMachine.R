GetMachine <- function(URLs, remDr) {
  
  GetMachine_ = function(url, remDr){
    remDr$navigate(url) 
    sel = remDr$findElements(using = 'xpath', "//select[@id='selList2']/option")
    result = sapply(sel, function(x){x$getElementAttribute("value")}) %>% unlist()
    id = url %>% substr(., regexpr("park_id", url)+8, regexpr("NowState", url)-3)
    res = data.table(id = id, machine = result)
    
    return(res)
  }
  
  out_list <- lapply(URLs, GetMachine_, remDr)
  MachineList <- data.table::rbindlist(out_list, fill = TRUE)
  # data.table::setDF(out) # convert data.table object into data.frame
  MachineList
}