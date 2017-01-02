library(gtrendsR)
usr <- "mansur.omar1992@gmail.com"
psw <- "omar94550781"
gconnect(usr, psw) 
lang_trend <- gtrends(c("streamrooot", "stream root"), res="7")
plot(lang_trend)
