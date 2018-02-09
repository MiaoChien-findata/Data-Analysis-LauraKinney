RunAndGetData = function(iteration=10, URLs){
  source("source/GetParkingTable_.R")
  source("source/GetTable.R")
  source("source/GetParkingTable.R")
  
  
  for (i in 1:iteration) {
    
    tryCatch({
      
      l = readRDS("output/l.rds") +1
      for(l in l:length(URLs)){
        pk = GetParkingTable(URLs[[l]])
        Sys.sleep(2)
        
        ParkingTable = readRDS("output/ParkingTable.rds")
        ParkingTable = rbind(ParkingTable, pk)
        
        ParkingTable$discount = sapply(ParkingTable$discount, function(x){ ifelse(grepl("[0-9]{17}",x), gsub("[0-9]","",x), x)})
        
        ParkingTable %<>% data.table %>% .[name!=0]
        
        
        saveRDS(ParkingTable, "output/ParkingTable.rds")
        saveRDS(l, "output/l.rds")
        
        info = paste0("Done. in ParkingLot No. ", l,"/",length(URLs))  
        print(info)
        writeLines(info, fileConn)
        
        
      }
      
    }, error=function(e){
      message("iteration = ",i,". This is the original error: ")
      message(e)
      
      # source("source/loginDODO.R")
      # remDr = loginDODO(rD)
      
    })
    
  }
  
}