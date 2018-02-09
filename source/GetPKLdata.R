# pk = ParkList$ParkId[1]
GetPKLdata = function(pk){
  library(httr)
  library(rvest)
  url = paste0("http://www.dodohome.com.tw/menu09/detailpark.asp?parkno=", pk)
  res = GET(url)
  html = res %>% content(encoding="Big5")
  
  a = html %>% html_table(fill = TRUE)
  df = cbind(a[[2]]$X4,  a[[2]]$X5) %>% t() %>% data.table()

  dff = df[2]
  names(dff)=as.character(df[1])
  dff$站名 %<>% gsub("\\s","",.)
  dff$`DDH編號`=pk
  dff$地址 %<>% gsub("\\s","",.)
  dff$電話 %<>% gsub("\\s","",.)
  dff$營業時間 %<>% gsub("\\s","",.)
  dff$營業項目 %<>% gsub("\\s","",.)
  dff$停車型態 %<>% gsub("\\s","",.)
  dff$停車設備 %<>% gsub("\\s","",.)
  dff$收費標準 %<>% gsub("\\s","",.)
  dff$臨停信用卡優惠 %<>% gsub("\\s","",.)
  dff$出國停車信用卡優惠 %<>% gsub("\\s","",.)
  dff$身障優免 %<>% gsub("\\s","",.)
  dff$`備　　註` %<>% gsub("\\s","",.)
  dff$是否可停休旅車 %<>% gsub("\\s","",.)
  
  
  return(dff)
}