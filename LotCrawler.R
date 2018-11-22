setwd("./Data-Analysis-LauraKinney")

library(data.table)
library(magrittr)



#======================#

 source("source/GetParkList.R")
 ParkList = GetParkList()


 source("source/GetPKLdata.R")
 today = lapply(ParkList$ParkId, GetPKLdata) %>% dplyr::bind_rows()

 
 
 lot.today = today
 
 lot.today %<>%
   .[,.(a=NA %>% as.character(), 
        照片=NA %>% as.character(), 
        場站編號=NA %>% as.character(), 
        DDH原始編碼=DDH編號,
        場站名稱= 站名, 
        是否啟用=NA %>% as.character(), 
        車場經度=NA %>% as.character(), 
        車場緯度=NA %>% as.character(),
        支援車辨=NA %>% as.character(), 
        停車型態, 
        停車設備, 
        列表清單顯示地址=地址 %>% gsub("\\s.*$","",.), 
        完整地址=地址 %>% gsub("\\s.*$","",.), 
        打電話用="0226550818",
        UI顯示用 = 電話, 
        `營業時間  (UI顯示用)`=營業時間 %<>% gsub("\\s","",.), 
        平日營業時間=營業時間 %<>% gsub("\\s","",.), 
        假日營業時間=營業時間 %<>% gsub("\\s","",.),
        平日人工收費=NA %>% as.character(), 
        假日人工收費=NA %>% as.character(), 
        營業項目_月租 = ifelse(grepl("月租",營業項目),"Y","N"),
        營業項目_預約=NA %>% as.character(), 
        營業項目_臨停=ifelse(grepl("臨停",營業項目),"Y","N"),
        營業項目_出國停車 = ifelse(grepl("出國",營業項目),"Y","N"),
        汽車停車位數=NA %>% as.character(), 
        可月租車位數=NA %>% as.character(), 
        可領取會員卡=NA %>% as.character(),
        臨停信用卡優惠= ifelse(grepl("無",臨停信用卡優惠),"N","Y"),
        `汽車停車位數_1`=NA %>% as.character(), 
        停車種類=NA %>% as.character(), 
        收費標準,
        收費標準_汽車月租=NA %>% as.character(), 
        收費標準_汽車臨停=NA %>% as.character(),  
        收費標準_機車月租=NA %>% as.character(),
        收費標準_機車臨停=NA %>% as.character(), 
        身障優免 = ifelse(grepl("有",身障優免),"Y","N"),
        是否可停休旅車 = ifelse(grepl("不可",是否可停休旅車),"N","Y"),
        備註=`備　　註`, 
        幾分鐘內免費停車=NA %>% as.character(), 
        演算法類別=NA %>% as.character(), 
        `Teamviewer\nID`=NA %>% as.character(),
        `Teamviewer\nPW`=NA  %>% as.character() )]

# saveRDS(lot.today, "DODO_PK_crawler/rds/temp.rds")
# lot.today = readRDS("DODO_PK_crawler/rds/temp.rds")
saveRDS(lot.today, paste0("rds/everyday_lot_list/",Sys.Date(),".rds"))


# 取前一天的紀錄
file = list.files("rds/")[grepl("PKLdata",list.files("rds/"))] %>% max()
lot.yesterday = readRDS(paste0("rds/", file))



#=================TEMP#=================#
# lot.today[4]$臨停信用卡優惠="N"
# lot.today[4]$停車型態="室內"
# lot.today[35]$臨停信用卡優惠="Y"
# lot.today[23]$身障優免="Y"
# saveRDS(lot.today, paste0("DODO_PK_crawler/rds/PKLdata_",Sys.Date()-1))
#=================TEMP#=================#


#-----------------------------------------------------------------#

url = paste0("http://www.dodohome.com.tw/news/mainframe.asp?Page=",1:3,"&reload_coolmenus")

library(httr)
library(rvest)
library(webshot)

# webshot(url[1], paste0("/home/miao/Dropbox/Findata/DODO_PK_crawler/screenshot/news_",Sys.Date(),".png"))
# webshot(url[1], paste0("/home/miao/Dropbox/Public/iParking/DODO_PK_crawler/png/news_",Sys.Date(),".png"))

webshot(url[1], paste0("screenshot/news_", Sys.Date(),".png"))

GetNews = function(url){
  res = GET(url)
  a = res %>% content(encoding="big5") %>% html_table(fill=TRUE, header=TRUE) %>%
    .[[3]] %>% data.table()
  return(a)
}

news = lapply(url, GetNews) %>% do.call(rbind,.)

file2 = list.files("rds/")[grepl("news",list.files("rds/"))] %>% max()
news.yesterday = readRDS(paste0("rds/", file2))




#-----------------------------------------------------------------#
# 新增的場站
lot.today.add = setdiff(lot.today$DDH原始編碼,lot.yesterday$DDH原始編碼)
lot.today.add.data = lot.today %>% .[DDH原始編碼 %in% lot.today.add]

write.csv(lot.today.add.data, paste0("lots_add/lot_add_", Sys.Date(),".csv"), fileEncoding = "UTF-8")



# 刪除的場站
lot.today.del = setdiff(lot.yesterday$DDH原始編碼, lot.today$DDH原始編碼)


lot.today.add_del  = c(lot.today.add, lot.today.del)

# 比較今天和昨天，找出場站有修改的資訊

if(length(lot.today.add_del)==0){
  # 沒有新增場站，沒有刪除場站
  # 比對前後兩天的差異就好
  lot.today.fix.today = fsetdiff(lot.today, lot.yesterday)
  lot.today.fix.yesterday = fsetdiff(lot.yesterday, lot.today)
  
}else{
  # 有新增刪除場站
  lot.yesterday.2 = lot.yesterday %>% .[!DDH原始編碼 %in% lot.today.add_del]
  lot.today.2 = lot.today %>% .[!DDH原始編碼 %in% lot.today.add_del]
  
  lot.today.fix.today = fsetdiff(lot.today.2, lot.yesterday.2)
  lot.today.fix.yesterday = fsetdiff(lot.yesterday.2, lot.today.2)    
}




# 把修改的東西丟到 html
fix.id = lot.today.fix.today$DDH原始編碼
library(xtable)

if(length(fix.id)!=0){
  lot.fix = list()
  for(i in 1:length(fix.id)){
    new = lot.today.fix.today %>% .[DDH原始編碼 %in% fix.id[i]]
    old = lot.today.fix.yesterday %>% .[DDH原始編碼 %in% fix.id[i]]
    colname.fix.ind = which(equals(new, old)==FALSE)
    
    res = data.frame(rbind(new, old))[,c(4,5,colname.fix.ind)]
    rownames(res) = c(as.character(Sys.Date()), file %>% gsub("PKLdata_","",.))
    
    lot.fix[[i]]=res
  }  
  
  con = vector()
  for(i in 1:length(lot.fix)){
    con = c(con, print(xtable(lot.fix[[i]]), type = 'html'), "<br><br>")
  }
  
}else{
  
  con = "沒有更新<br>"
}





news.today.add = fsetdiff(news, news.yesterday)
#-----------------------------------------------------------------#



line.no = "沒有更新<br>"
line.br = "<br>"

line1 = "<h1>《《停車場更新》》</h1>"
line2 = "<h2>新增停車場</h2>"

if(length(lot.today.add)!=0){
  line3 = paste(lot.today %>% .[DDH原始編碼 %in% lot.today.add] %>% .$場站名稱, collapse = "<br>") %>% paste0(., "<br>")  
  line4 = paste0("<b>新增車場資料表格下載：", "https://www.findata.com.tw/DoDo-WebSite-Data/lots_add/lot_add_",Sys.Date(),".csv","</b><br>")
}else{
  line3 = line.no
  line4 = line.br
}



line5 = "<h2>刪除停車場</h2>"

if(length(lot.today.del)!=0){
  line6 = paste(lot.yesterday %>% .[DDH原始編碼 %in% lot.today.del] %>% .$場站名稱, collapse = "<br>")  
}else{
  line6 = line.no
}


line.fix = "<h2>停車場資訊修改</h2>"



line_sep = "<br>===============================================<br>"

line7 = "<h1>《《最新消息》》</h1>"

if(nrow(news.today.add)!=0){
  line8 = paste("<h3>新增", nrow(news.today.add), "條最新消息</h3>")  %>% paste(., paste(news.today.add$`標　　　題`, collapse = "<br>")) %>% paste(.,"<br><br><br>")
  
  line9 = paste0("<b>當日最新消息截圖下載：","https://www.findata.com.tw/DoDo-WebSite-Data/screenshot/news_",Sys.Date(),".png","</b><br>")
}else{
  line8 = line.no %>% paste(., line.br)
  line9 = paste0("<b>當日最新消息截圖下載：","https://www.findata.com.tw/DoDo-WebSite-Data/screenshot/news_",Sys.Date(),".png","</b><br>")
}

line10 = "<b>去嘟嘟房首頁看看 ☞ https://www.dodohome.com.tw/dodohome.asp</b><br>"






content = paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
                Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
                <html xmlns="http://www.w3.org/1999/xhtml">
                <head>
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
                <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                <style type="text/css">
                </style>
                </head>
                <body>
                ',line1, line2, line3, line4, line5, line6, line.fix, 
                paste(con, collapse = "<br>")
                ,line_sep, line7, line8, line9, line_sep, line.br, line.br, line10,'
                </body>
                </html>')


source("source/SendMail.R")

MailList = list("miaochien@findata.com.tw", "albert@findata.com.tw", "joychiang@findata.com.tw",
                "defi781023@findata.com.tw")


SendMail(receiver = MailList, 
         subject= paste("《每日檢查》嘟嘟房停車場更新", Sys.Date()),
         html = content)


saveRDS(lot.today, paste0("rds/PKLdata_", Sys.Date()))
saveRDS(news, paste0("rds/news_", Sys.Date()))


system("cp -r screenshot /usr/share/nginx/html/DoDo-WebSite-Data")  
system("cp -r lots_add /usr/share/nginx/html/DoDo-WebSite-Data")  





