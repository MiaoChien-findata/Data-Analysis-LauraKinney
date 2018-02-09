GetTable = function(element.tables, url){
  library(stringr)
  tbl = sapply(element.tables, function(x){x$getElementText()}) %>% str_split(pattern = "\n")
  
  ind = NULL
  for(i in 1:length(tbl)){
    this.tbl = tbl[[i]]
    if(length(this.tbl)==2){
      ind = c(ind, i)
    }
  }
  
  
  tbl2 = lapply(tbl[-ind], function(x){x %>% data.table() %>% t()}) 
  
  table = NULL
  for(i in 1:length(tbl2)){
    
    this.tbl2 = tbl2[[i]] %>% .[1:9]
    
    if(this.tbl2[3] %>% grepl("[//:]",.)){
      #如果第3項（車號）中卻含有時間表示
      this.tbl2 = c(this.tbl2[1:2], NA, this.tbl2[3:8])
    }
    
    if((this.tbl2[7] %>% gsub(" ","",.) %>% grepl("\\D",.))==FALSE){
      #如果第7項中只有數字則執行此區域
      this.tbl2 = c(this.tbl2[1:6],NA,this.tbl2[7:8])
    }
    
      
      
    table = rbind(table, this.tbl2) 
  }
  
  
  
  table %<>% data.table %>% .[,.("name"= tbl[ind[1]][[1]][1] %>% str_extract(., "^.*-") %>% gsub("-","",.),
                                 "code"= substr(url, regexpr("selList=", url)+8, regexpr("&selList2", url)-1),
                                 "order"= V1 %>% as.numeric, 
                                 "number"=V2, 
                                 "plate"=V3, 
                                 "entered_at"=V4 %>% strptime(., " %m/%d %H:%M",tz = "Asia/Taipei") %>% as.character,
                                 "exited_at"=V5 %>% strptime(., " %m/%d %H:%M",tz = "Asia/Taipei") %>% as.character, 
                                 "hours"=V6 %>% as.numeric(), 
                                 "discount"=V7, 
                                 "discount_hours"=V8 %>% as.numeric(),
                                 "paid_amount"=V9 %>% as.numeric())]
  return(table)
}