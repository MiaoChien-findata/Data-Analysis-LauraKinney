GetParkList = function(){
  library(httr)
  library(rvest)
  url = "http://www.dodohome.com.tw/menu09/mainframe.asp"
  res = GET(url, add_headers(`User-Agent`= "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.99 Safari/537.36"))
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