library(httr)
library(jsonlite)
# receiver is a list
SendMail = function(receiver, subject, html=NA, text=NA){
  
  if(!is.na(html) & is.na(text)){
    body = list(`receiver`= receiver,
                `subject`= subject,
                `html`= html)
    res = POST(url = "http://10.140.2.2:3000/v1/support/sendMail", 
                      add_headers(`Content-Type`= "application/json; charset=utf-8"),
                      body = jsonlite::toJSON(body, auto_unbox = TRUE)  )
    print(content(res)$status)
                
  }else if(!is.na(text) & is.na(html)){
    
     # receiver = list("miaochien@findata.com.tw")
     # subject = "NonCDE測試"
     # text = "test"
     # 
    
    body = list(`receiver`= receiver,
                `subject`= subject,
                `text` = text)
    res = POST(url = "http://10.140.2.2:3000/v1/support/sendMail", 
               add_headers(`Content-Type`= "application/json; charset=utf-8"),
               body = jsonlite::toJSON(body, auto_unbox = TRUE)  )
    print(content(res)$status)
    
  }else if (!is.na(text) & !is.na(html)){
    print("html 或 text 擇一選填")
  }else{
    print("請填入 html 或 text")
  }
  
  
  
  
}



