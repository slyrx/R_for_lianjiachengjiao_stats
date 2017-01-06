#确保能返回list类型的标签列表
cutStrLoop<-function(liststr){
  patternstr<- ">.*<"
  listlen<-length(liststr)
  k<-vector(length = listlen)
  i<-0
  for(n in liststr){
    i<-i+1
    startnum<-regexpr(patternstr, n)
    endnum <- startnum + attr(startnum, "match.length")-1
    n = substr(n, startnum+1, endnum-1)
    k[i]<-n
  }
  return(k)
}

#锁定链家成交画面
abstractlianjiaershou<-function(weburl){
  doc <- xml2::read_html(weburl)
  pp <- xml2::xml_find_all(doc, '/html/body//div[4]/div[1]/ul/li/div/div[2]/div[3]/span', ns=xml2::xml_ns(doc))
  qq <- xml2::xml_find_all(doc, '/html/body/div[4]/div[1]/ul/li/div/div[3]/div[3]/span', ns=xml2::xml_ns(doc))
  kk <- xml2::xml_find_all(doc, '/html/body//div[4]/div[1]/ul/li/div/div[2]/div[2]', ns=xml2::xml_ns(doc))
  oo <- xml2::xml_find_all(doc, '/html/body/div[4]/div[1]/ul/li/div/div[1]/a', ns=xml2::xml_ns(doc))
  
  totalprice<-c(cutStrLoop(pp))
  perMprice<-c(cutStrLoop(qq))
  datelist<-c(cutStrLoop(kk))
  housename<-c(cutStrLoop(oo))
  
  a<-matrix(totalprice, nrow = 30, ncol = 1)
  b<-matrix(perMprice,nrow = 30, ncol = 1)
  c<-matrix(datelist,nrow = 30, ncol = 1)
  d<-matrix(housename,nrow = 30, ncol = 1)
  liantable<-data.frame(a,b,c,d,stringsAsFactors = FALSE)
  
  return(liantable)
}


#批量获取链家日成交数据
bat<-function(){
  liantable1<-c()
  k<-0
  #共100页，一半只提取30页即可获取道最新一天的成交量
  for (i in 1:30) {
    print(i)
    weburl1<- sprintf("http://bj.lianjia.com/chengjiao/pg%d/",i) 
    liantable2 <- abstractlianjiaershou(weburl1)
    #检查如果是na则重新抓取
    while (anyNA(liantable2) == TRUE) {
      k=k+1
      print(k)
      liantable2 <- abstractlianjiaershou(weburl1)
    }
    
    liantable1<- rbind(liantable1, liantable2)
  }
  write.csv(liantable1, "lianyable.csv")
  return(liantable1)
}







