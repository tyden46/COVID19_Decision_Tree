library(stringr)
setwd("C://Users//tyden46//Documents")
coronaVirusTable=read.csv("https://raw.githubusercontent.com/beoutbreakprepared/nCoV2019/master/latest_data/latestdata.csv")
outcomeAvailable=coronaVirusTable[which(coronaVirusTable$outcome!=""),]
goodData=outcomeAvailable[which(outcomeAvailable$sex!=""),]
goodData=goodData[which(goodData$age!=""),]
print(goodData$age)
cleanAges=c()
for(x in goodData$age){
  if(str_detect(x, "-")){
    x=str_extract_all(x, "[:alnum:]+")
    if(length(x[[1]])==2){
      x=(as.numeric(x[[1]][1])+as.numeric(x[[1]][2]))/2
    }else{
      x=x[[1]][1]
    }
  }
  cleanAges=append(cleanAges, x)
}
goodData$age=cleanAges
cleanOutcomes=c()
for(x in goodData$outcome){
  x=str_to_lower(x)
  if(str_detect(x, "died") | str_detect(x, "death") | str_detect(x, "deceased") | str_detect(x, "dead")){
    x="died"
  }else if(str_detect(x, "recover") | str_detect(x, "alive") |
           str_detect(x, "discharge") | str_detect(x, "hospital") | 
           str_detect(x, "release") | str_detect(x, "severe") |
           str_detect(x, "stable") | str_detect(x, "improved") |
           str_detect(x, "treat") | str_detect(x, "critical")){
    x="alive"
  }else{
    print(paste("Unknown Condition:", x, sep=" "))
    x="unknown"
  }
  cleanOutcomes=append(cleanOutcomes, x)  
}

goodData$outcome=cleanOutcomes
goodData=goodData[which(goodData$country!=""),]
goodData=goodData[,c(2,3,6,12,19,23,24)]
write.csv(goodData[,c(6,1,2,3,5)], "CovidSurvivalData.csv", row.names = FALSE)
