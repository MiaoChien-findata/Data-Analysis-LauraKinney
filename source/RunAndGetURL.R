RunAndGetURL = function(remDr, year, month, day){
  source("source/GetParkList.R")
  parklist = GetParkList()
  
  
  # 拿各場站的付費機器
  urls = paste0("http://park.dodohome.com.tw/chemSociety/DayParkDetailSearch.asp?txtBday=",day,"&IsLoginid=6625&park_id=",
                 as.character(parklist$ParkId), ",&NowState=1&itArea=%A5x%A4%A4%A5%AB&sDate=V1&
                txtBday1=",day,"&txtBmonth=",month,"&txtBmonth1=",month,"&txtByear=",year,"&txtByear1=",year)
  
 
  #---------------# LOGIN IN #---------------# 
  
  # source("source/loginDODO.R")
  # remDr = loginDODO(rD)
  
  #-----------------------------------------------#
  source("source/GetMachine.R")
  MachineList = GetMachine(urls, remDr)
  
  
  source("source/GetURLs.R")
  URLs = lapply(parklist$ParkId, FUN = GetURLs, year=year , month=month, day=day, machineList=MachineList) 
  return(URLs)
  Sys.sleep(1)
}