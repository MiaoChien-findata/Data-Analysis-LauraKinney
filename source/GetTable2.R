library(stringr)

GetTable2 = function(html, url){
  
  
  
  h = lapply(html, as.data.table) 
  # ind = NULL
  table = NULL
  for(i in 1:length(h)){
    this.tbl=h[[i]]
    if(length(names(this.tbl))==13){
      this.tbl = this.tbl[-1]
      table = rbind(table, this.tbl)
    }
    
  }
  
  Sys.sleep(1)
  
  
  
  table %<>% .[,.("name"= h[[1]][2]$X1 %>% str_extract(., "^.*-") %>% gsub("-","",.),
                 "code"= substr(url, regexpr("selList=", url)+8, regexpr("&selList2", url)-1),
                 "order" = X1 %>% as.numeric, 
                 "number" = X2, 
                 "plate" = X3, 
                 "spot" = X4, 
                 "entered_at"= X5 %>% strptime(., " %m/%d %H:%M",tz = "Asia/Taipei") %>% as.character,
                 "exited_at"= X6 %>% strptime(., " %m/%d %H:%M",tz = "Asia/Taipei") %>% as.character, 
                 "hours"=X7 %>% as.numeric(), 
                 "discount"=X8 , 
                 "discount_hours"=X9 %>% as.numeric(),
                 "paid_amount"=X10 %>% as.numeric(), 
                 "receipt"=X11 %>% as.character(),
                 "vat"=X12 %>% as.character(),
                 "crewid"=X13 %>% as.numeric()
                 )]
  
  
  
  return(table)
}
