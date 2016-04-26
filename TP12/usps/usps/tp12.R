source ("readUSPS.R")
source ("freeman.R")
uspsData = readUSPSdata(".")
tabFreeman = list()
for(i in 1:11000){
  
  tabFreeman[[i]] = freeman(uspsData[[1]][,,i])
}



