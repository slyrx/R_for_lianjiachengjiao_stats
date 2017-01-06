#将四个值拼接
#resblockinfo<-data.frame(json_data1$itemData$signPrice, json_data1$itemData$unitPrice, json_data1$itemData$signTime, json_data1$itemData$houseArea)

#
getresblockinfo<-function(weburl){
  json_data1 <- fromJSON(weburl)
  resblockinfo<-data.frame(json_data1$itemData$signPrice, json_data1$itemData$unitPrice, json_data1$itemData$signTime, json_data1$itemData$houseArea)
  
  return(resblockinfo)
}

batresblock<-function(webroot){
  resblockinfo1<-c()
  for (i in 1:363) {
    print(i)
    weburl = sprintf("%s%d",webroot, i)
    #weburl = sprintf("http://bj.lianjia.com/chengjiao/getinfo/?page=1&id=101100944547&type=resblock&p=%d",i)
    resblockinfo<- getresblockinfo(weburl)
    resblockinfo1<- rbind(resblockinfo1, resblockinfo)
  }
  write.csv(resblockinfo1, "resblockinfo.csv")
  return(resblockinfo1)
}

#删除缺失值
delzero<-function(unitprice1){
  for (i in 1:1089) {
    print(i)
    if(is.na(unitprice1[i])){
      
    }else{
      rs<-unitprice1[i]==0
      if(rs){
        unitprice1[i]<-NA
      }
    }
  }
  return(unitprice1)
}


#jingjidao4<-jingjidao3[jingjidao3$json_data1.itemData.unitPrice!=0,]




