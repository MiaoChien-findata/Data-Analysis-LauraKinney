library(rmarkdown)
setwd("~/Dropbox/Findata")
render("DODO_PK_crawler/LotCrawler.Rmd")


# copy the files to the new folder
file.copy(from = "DODO_PK_crawler/LotCrawler.html", to = "~/Dropbox/Public/iParking", overwrite = T)
