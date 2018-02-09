# year = 2017
# month = 3
# day = 1
# PKid = data$V1[1]
# PKid = data %>% .[V2=="建國北站"] %>% .$V1 %>% as.character()
# GetURLs(2017, 3, 1, data$V1[1])
                
# machineList = readRDS("list/MachineList.rds")
GetURLs = function(year, month, day, PKid, machineList){
  
  txtByear = year
  txtBmonth = month
  txtBday = day
  selList = PKid
  # selList2=paste0("S",selList,"01,A",selList,"01")
  
  
  selList2=paste(machineList %>% .[id==PKid] %>% .$machine, collapse=",")
  
  URL = paste0("http://park.dodohome.com.tw/chemSociety/DayParkDetailReport.asp?txtByear=",
                txtByear,
                "&txtBmonth=",txtBmonth,"&txtBday=",txtBday,"&selList=",selList,
                "&selList2=",selList2,"&txtByear1=",txtByear,"&txtBmonth1=",txtBmonth,
                "&txtBday1=&sDate=V1")
  
  return(URL)
}