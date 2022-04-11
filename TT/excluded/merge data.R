setwd("F:/Tools/Rworkspace/TT")
a <- list.files()
dir <- paste("./",a,sep="")
n <- length(dir)
merge.data <- read.csv(file = dir[1], header = T, sep = ",", skipNul=TRUE)
merge.data <- cbind(dir[1], merge.data)
merge.data <- rename(merge.data, c("dir[1]"="dataTT"))

for (i in 2:10){
  new.data <- read.csv(file = dir[i], header=T, sep=",", skipNul=TRUE)
  new.data <- cbind(dir[i], new.data)
  new.data <- rename(new.data, c("dir[i]"="dataTT"))
  merge.data <- rbind(merge.data,new.data)
}

write.csv(merge.data, file="./merge_all.csv", row.names=FALSE)



