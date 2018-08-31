library(RCurl)
library(XML)
library(plyr)
library(dplyr)

getShiti <- function(page, tkbh){
  url<-sprintf("http://121.192.191.91/redir.php?catalog_id=6&cmd=learning&tikubh=%s&page=%s",
               tkbh,
               page)
  html <- getURL(url,.encoding='gb2312')
  html<-iconv(html, 'gb2312', to='utf-8')
  
  # parse html
  doc = htmlParse(html, asText=TRUE)
  question <- xpathSApply(doc, "//div[@class='shiti']/h3", xmlValue)
  options <- xpathSApply(doc, "//div[@class='shiti']/ul",
                         function(x){
                           values <- xpathSApply(x, 'li', xmlValue)
                           if(length(values)==2){
                             paste0(LETTERS[1:2],'.', values, collapse = ' ')
                           }else{
                             paste0(values, collapse = ' ')
                           }
                         })
  answer <- xpathSApply(doc, "//div[@class='shiti-content']/span", xmlValue)
  tryCatch(
    data.frame(question=question, options=options, answer=answer),
    error=function(x){
      data.frame()
    }
    )
}

getTiku <- function(tkbh, maxPage, filename){
  ldply(1:maxPage, getShiti, tkbh=tkbh, .progress = "text", .parallel=F) %>%
    mutate(
      question=gsub('^[0-9]+、', '', question),
      answer=gsub('（标准答案： +| +）', '', answer)) %>%
    write.csv(file=filename)
}

tk_settings = list(
  huaxue =
    list(
      filename='化学类试题.csv',
      tkbh = 1436,
      maxPage = 92),
  yixueshengwu =
    list(
      filename='医学生物类试题.csv',
      tkbh = 1467,
      maxPage = 43),
  tongshianquan =
    list(
      filename='通识安全类试题.csv',
      tkbh = 1471,
      maxPage = 83),
  fushe =
    list(
      filename='辐射类试题.csv',
      tkbh = 1486,
      maxPage = 38),
  xiaofang =
    list(
      filename='消防安全试题.csv',
      tkbh = 4200,
      maxPage = 10),
  anquanzhishijingsai =
    list(
      filename='安全知识竞赛试题.csv',
      tkbh = 71905,
      maxPage = 373)
  )

lapply(tk_settings, function(x){
  message(x$filename)
  do.call(getTiku, args=x)
  Sys.sleep(20)
})
