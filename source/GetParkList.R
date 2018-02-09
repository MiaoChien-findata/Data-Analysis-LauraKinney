GetParkList = function(){
  library(httr)
  library(rvest)
  url = "https://www.dodohome.com.tw/menu09/mainframe.asp"
  res = GET(url)
  html = res %>% content(encoding="Big5")
  
  a = html %>% html_nodes("a.delfix-bot") %>% html_attrs()
  
  c = html %>% html_nodes("a.delfix-bot") %>% html_text()
  
  GetParkName = function(b){
    this.a = b[3] %>% as.character 
    parkname=substr(this.a, regexpr("parkno=", this.a)+7, regexpr("detailpark',", this.a)-4)
    return(parkname)
  }
  
  parkid = lapply(a, GetParkName) %>% do.call(rbind,.) %>% data.table() %>% .[,.(ParkId=V1)]
  parkid$name = c
  
  return(parkid)  
}