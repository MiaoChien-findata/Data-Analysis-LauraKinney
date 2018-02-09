library(RSelenium)
library(getPass)


loginDODO = function(rD){
  url = "http://park.dodohome.com.tw/chemSociety/DayParkDetailSearch.asp?IsLoginid=6625"
  remDr <- rD$client
  
  remDr$open()
  # remDr$open(silent = TRUE)
  remDr$navigate(url)
  remDr$acceptAlert()
  
  
  #圖形碼#
  # pic = remDr$findElement("id", "imgCaptcha")
  # remDr$mouseMoveToLocation(webElement = pic)
  # Sys.sleep(3)
  # remDr$click(2) # 2 indicates click the right mouse button
  # remDr$sendKeysToActiveElement(list(key = 'right_arrow',key = 'right_arrow'))
  # remDr$click(1)
  
  code = getPass("Please enter CODE: ")
  user = "6625"
  pwd = "432812"
  remDr$findElement("id", "txtCaptcha")$sendKeysToElement(list(code))
  remDr$findElement("id", "selId")$sendKeysToElement(list(user))
  remDr$findElement("id", "txtPassword")$sendKeysToElement(list(pwd))
  
  
  button = remDr$findElement("id", "SubmitLogin")
  remDr$mouseMoveToLocation(webElement = button)
  remDr$click(1)
  return(remDr)
}
