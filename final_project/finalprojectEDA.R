
library(data.table)

setnames(abalone, "X1", "Sex")
setnames(abalone, "X2", "Length")
setnames(abalone, "X3", "Diameter")
setnames(abalone, "X4", "Height")
setnames(abalone, "X5", "WholeWeight")
setnames(abalone, "X6", "ShuckedWeight")
setnames(abalone, "X7", "VisceraWeight")
setnames(abalone, "X8", "ShellWeight")
setnames(abalone, "X9", "Rings")

abalone$Sex <- factor(abalone$Sex)

write.csv(abalone, "abalone.csv")
