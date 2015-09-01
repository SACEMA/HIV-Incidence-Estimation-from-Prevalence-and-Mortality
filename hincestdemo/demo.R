require(xlsx)

data1 = read.xlsx(file="HincestDemoData.xls",sheetName = "AgeStatusTime")
data2 = read.xlsx(file="HincestDemoData.xls",sheetName = "MortalityInclusion")

require(hincest)

fage=sort(unique(data1$Age))
cat('\n','Please wait while the program is running','\n')
res=hincest(age=fage, inst=15, Age=data1$Age, HIV=data1$HIV, ti=data1$ti,
Mort=data2$MortRates, r = data2$r, sizeboot = 1000, cilevel = 0.95, ntrials = 10)

resdata= data.frame(Age=fage,Estimate=res$inciEst,LowerBound=res$LowerBound,UpperBound=res$UpperBound)
write.xlsx(resdata,file="RESHincestDemoData.xls",sheetName = "IncidenceAndCI",row.names=FALSE)
cat('\n','End. The results are saved in the file HincestDemoData.xls','\n')
resdata
plot(res)
q()

