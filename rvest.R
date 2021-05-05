# 단일 페이지 스크레이핑

library(rvest)
url <- "https://www.data.go.kr/tcs/dss/selectDataSetList.do?dType=API&keyword=&detailKeyword=&publicDataPk=&recmSe=&detailText=&relatedKeyword=&commaNotInData=&commaAndData=&commaOrData=&must_not=&tabId=&dataSetCoreTf=&coreDataNm=&sort=&relRadio=&orgFullName=&orgFilter=&org=&orgSearch=&currentPage=1&perPage=10&brm=&instt=&svcType=&kwrdArray=&extsn=&coreDataNmArray=&pblonsipScopeCode="
html <- read_html(url)
title <- html_nodes(html, '#apiDataList > div.result-list') %>% 
  html_nodes(".title") %>%
  html_text()
title <- gsub("\r\n","",title)
title <- trimws(title,'both')
title

desc <- html_nodes(html, '#apiDataList > div.result-list') %>%
  html_nodes(".ellipsis") %>%
  html_text()
desc <- gsub("\r\n", "", desc)
desc <- trimws(desc, 'both')
desc

api <- data.frame(title, desc)
View(api)

# 복수 페이지 스크레이핑
titles <- NULL
descs <- NULL

for (page in 1:10) {
  url <- paste(url <- "https://www.data.go.kr/tcs/dss/selectDataSetList.do?dType=API&keyword=&detailKeyword=&publicDataPk=&recmSe=&detailText=&relatedKeyword=&commaNotInData=&commaAndData=&commaOrData=&must_not=&tabId=&dataSetCoreTf=&coreDataNm=&sort=&relRadio=&orgFullName=&orgFilter=&org=&orgSearch=&currentPage=", page, "1&perPage=10&brm=&instt=&svcType=&kwrdArray=&extsn=&coreDataNmArray=&pblonsipScopeCode=", sep="")
  html <- read_html(url)
  
  title <- html_nodes(html, '#apiDataList > div.result-list') %>% 
    html_nodes(".title") %>%
    html_text()
  title <- gsub("\r\n","",title)
  title <- trimws(title,'both')
  
  desc <- html_nodes(html, '#apiDataList > div.result-list') %>%
    html_nodes(".ellipsis") %>%
    html_text()
  desc <- gsub("\r\n", "", desc)
  desc <- trimws(desc, 'both')
  
  titles <- c(titles, title)
  descs <- c(descs, desc)
}
api_s <- data.frame(titles, descs)
View(api_s)
