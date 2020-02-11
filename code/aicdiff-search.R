# Search through files and find the one with the closest aicdiff

library(seasonal)

dataDir <- "~/github/CR-SAclass/data/retail/"
allData <- list.files(dataDir, pattern = "*.dat")
allData <- allData[1:50] # run errors after series 50

out <- data.frame(series  = allData, 
                  aicdiff = rep(-99, length(allData))) # storage for output
for(i in 1:length(allData)){
  print(i)
  x <- import.ts(file.path(dataDir, allData[i]))
  m <- seas(x, x11 = "")
  aicValues <- udg(m, c("aictest.trans.aicc.nolog", "aictest.trans.aicc.log"))
  d <- diff(aicValues)
  out$aicdiff[i] <- d
}
 
# grocery store retail data has the closest AIC between log and none
out[36, ]
x <- import.ts(file.path(dataDir, 'Grocery stores.dat'))
plot(x)
class(x)
