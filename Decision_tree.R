library(ISLR)
library(rpart)
library(tree)
dap = read.csv(file=file.choose(),header=T,sep=",",dec=",")
attach(dap)

dap$p2p <- as.numeric(as.character(dap$p2p))
dap$cdn <- as.numeric(as.character(dap$cdn))
#Create a categorical variable 
hybird = ifelse(dap$p2p >= 2 ,"hybird", "not hybird")
dap = data.frame(dap,hybird)
dap = dap[,-5]

fit <- rpart(hybird ~ isp + browser + connected +  X.stream,
             method="class", data=dap)


# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

text(fit,pretty = 0)

post(fit, file = "e:/tre.ps", 
     title = "Classification Tree")

