library(ggplot2)
library(tidyverse)

setwd("/Users/kjx/Library/CloudStorage/OneDrive-UniversitiKebangsaanMalaysia/Y3S2/Selective Issues/Tugasan")

cpi <- read.csv(file = "cpi_state.csv", header = T, sep = ";")
tibble(cpi) -> cpi
str(cpi)
head(cpi)

summary(cpi)

format(cpi$date, format = "%d/%m/%Y") -> cpi$date

as.Date(cpi$date, format = "%d/%m/%Y") -> cpi$date

attach(cpi)
detach(cpi)
cat <- unique(category)
date <- unique(date)
state <- names(cpi[, -c(1,2)])

cpi[, 1:3] %>% subset(category == c("alcohol_tobacco", "education"))

mat <- c()
#####
seperate <- function(x){
  
  for (i in 1:length(cat)){
    
    state <- cpi[, c(1:2, x)]
    state[which(category == cat[i]), ] -> a
    a[, 3] -> l
    mat <- append(mat, l)
    
    
  }
  matrix(mat, ncol = 13, byrow = F) %>% as.data.frame() -> aa
  names(aa) <- cat
  bb <- data.frame(date, aa)
  return(bb)

}


johor <- sepi(3)
kedah <- sepi(4)          
kelantan <- sepi(5)    
melaka <- sepi(6)
negeri.sembilan <- sepi(7)
pahang  <- sepi(8)        
perak <- sepi(9)          
perlis<- sepi(10)         
pulau.pinang  <- sepi(11) 
sabah <- sepi(12)          
sarawak <- sepi(13)        
selangor  <- sepi(14)     
terengganu  <- sepi(15)    
kuala.lumpur <- sepi(16)
labuan <- sepi(17)      
putrajaya  <- sepi(18) 

#####

#Average CPI by states
attach(cpi)
detach(cpi)

cpi %>% filter(category == "overall") -> overall
over_state <- overall[, -c(1,2)]
apply(over_state, MARGIN = 2, FUN = mean) -> mean_overall

as.data.frame(mean_overall, row.names = c(1:16)) -> data1

data1$state = state


ggplot(data1, aes(x = state, y=mean_overall)) + geom_bar(stat = "identity", aes(fill = state)) + labs(title = "Overall CPI") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + scale_fill_viridis(discrete = T, option = "A")

cpi[, -1] %>% group_by(category) %>% aggregate(avg = rowMeans(cpi[, -c(1,2)])) -> avg1


aggregate(cpi, by = as.list(category), FUN = "mean")

avg1 %>% group_by(category) %>% summarise(date_avg = mean(avg1$avg))

####

apply(over_state, MARGIN = 1, FUN = mean) -> over2
tibble(date, over2) -> over3
over3$date = as.Date(date, format = "%d/%m/%Y")

ggplot(over3) + geom_line(aes(x = date, y = over2, color = "Overall CPI")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ scale_x_date(date_breaks = "1 month", date_labels = "%B %Y") + guides(colour = guide_legend("Legend")) + ylab("CPI") + ggtitle("Overall CPI Trend")

####


####

apply(cpi[, -c(1,2)], MARGIN = 1, FUN = mean) -> over_cat
tibble(cpi[, c(1,2)], over_cat) -> over_cat2
over_cat2 %>% group_by(category)
over_cat2$date = as.Date(over_cat2$date, format = "%d/%m/%Y")



ggplot(over_cat2, aes(x = date, y = over_cat)) + geom_bar(stat = "identity", position = "fill", aes(fill = category)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ scale_x_date(date_breaks = "1 month", date_labels = "%B %Y") + guides(colour = guide_legend("Legend")) + ylab("CPI") + ggtitle("Overall CPI Trend for each Category") + scale_fill_discrete()


plot
ggplotly(plot)

#####

data.frame(johor = apply(johor[, -1], MARGIN = 2, FUN = mean),
           kedah = apply(kedah[, -1], MARGIN = 2, FUN = mean),
           kelantan = apply(kelantan[, -1], MARGIN = 2, FUN = mean),
           melaka = apply(melaka[, -1], MARGIN = 2, FUN = mean),
           negeri.sembilan = apply(negeri.sembilan[, -1], MARGIN = 2, FUN = mean),
           pahang = apply(pahang[, -1], MARGIN = 2, FUN = mean),
           perak = apply(perak[, -1], MARGIN = 2, FUN = mean),
           perlis = apply(perlis[, -1], MARGIN = 2, FUN = mean),
           pulau.pinang  = apply(pulau.pinang[, -1], MARGIN = 2, FUN = mean),
           sabah= apply(sabah[, -1], MARGIN = 2, FUN = mean),
           sarawak = apply(sarawak[, -1], MARGIN = 2, FUN = mean),
           selangor  = apply(selangor[, -1], MARGIN = 2, FUN = mean),
           terengganu = apply(terengganu[, -1], MARGIN = 2, FUN = mean),
           kuala.lumpur = apply(kuala.lumpur[, -1], MARGIN = 2, FUN = mean),
           labuan = apply(labuan[, -1], MARGIN = 2, FUN = mean),
           putrajaya = apply(putrajaya[, -1], MARGIN = 2, FUN = mean)
           ) -> by_state

tibble(cat, by_state) -> by_state2

melt(by_state2) -> by_state3


ggplot(by_state3) + geom_bar(stat = "identity", aes(x = variable, y = value, fill = cat), position = "stack") + scale_fill_viridis(discrete = T, option = "C")



cpi2$date = as.Date(cpi2$date, format = "%d/%m/%Y")
tibble(melt(cpi)) -> cpi2

ggplot(cpi2) + geom_line(aes(x = date, y = value, color = category)) + facet_wrap(~variable, nrow = 4)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ scale_x_date(date_breaks = "1 month", date_labels = "%B %Y") -> plot
ggplotly(plot)





ggplot(by_state3) + geom_tile(aes(x = variable, y = value)) + facet_wrap(~cat, nrow = 2)


####

tibble(melt(putrajaya)) -> putrajaya2
putrajaya2$date <- as.Date(putrajaya2$date, format = "%d/%m/%Y")


ggplot(putrajaya2) + geom_line(aes(x = date, y = value, group = variable, color = variable))  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ scale_x_date(date_breaks = "1 month", date_labels = "%B %Y") -> plot2

ggplotly(plot2)


####

dif <- function(state){
  
  state[27, -c(1)] - state[1, -c(1)] -> t
  matrix(t, ncol = 1, byrow = F) -> t2
  data.frame(cat, `state` = t2) -> t3
  
  return(t3)
}


c_johor <- dif(johor)
c_kedah <- dif(kedah)
c_kelantan<- dif(kelantan)
c_melaka<- dif(melaka)
c_negeri.sembilan<- dif(negeri.sembilan)
c_pahang<- dif(pahang)
c_perak<- dif(perak)
c_perlis<- dif(perlis)
c_pulau.pinang<- dif(pulau.pinang)
c_sabah<- dif(sabah)
c_sarawak<- dif(sarawak)
c_selangor<- dif(selangor)
c_terengganu<- dif(terengganu)
c_kuala.lumpur<- dif(kuala.lumpur)
c_labuan<- dif(labuan)
c_putrajaya<- dif(putrajaya)

cbind(as.numeric(c_johor[,2]),as.numeric(c_kedah[,2]),
      as.numeric(c_kelantan[,2]),
      as.numeric(c_melaka[,2]),
      as.numeric(c_negeri.sembilan[,2]),
      as.numeric(c_pahang[,2]),
      as.numeric(c_perak[,2]), 
      as.numeric(c_perlis[,2]),
      as.numeric(c_pulau.pinang[,2]),
      as.numeric(c_sabah[,2]),
      as.numeric(c_sarawak[,2]),
      as.numeric(c_selangor[,2]),
      as.numeric(c_terengganu[,2]),
      as.numeric(c_kuala.lumpur[,2]),
      as.numeric(c_labuan[,2]), 
      as.numeric(c_putrajaya[,2])

      ) -> c2



data.frame(cat, c2) -> c3

names(c3) <- c("cat", state)

melt(c3, id = "cat") -> c4

ifelse(c4$value >= 0, "Positive", "Negative") -> growth

data.frame(c4, growth) -> c5


ggplot(data = c5, aes(x = cat, y = value, label = value)) + geom_bar(stat = "identity", aes(fill = growth)) + scale_fill_manual(name = "Growth", labels = c("Positive", "Negative"), values = c("Positive" = "green", "Negative" = "red")) + facet_wrap(~variable, nrow = 4) + coord_flip() 




barplot(as.data.frame(t))




####


dates <- seq.Date (from = ymd ('2021-01-01'), to = ymd ('2023-03-01'), by = 'month')
dates <- format (dates, '%m/%Y')

johor$date = dates
johor2$date = as.Date(date, format = "%d/%m/%Y")
tibble(johor2)
melt(johor) -> johor2


ggplot(johor2) + geom_line(aes(x = date, y = value, group = variable, color = variable)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")


p <- function(state){
  
  state$date = dates
  melt(state) -> state2
  state2$date = as.Date(date, format = "%d/%m/%Y")
  tibble(state2)
 
  
  
  ggplot(state2) + geom_line(aes(x = date, y = value, group = variable, color = variable)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ scale_x_date(date_breaks = "1 month", date_labels = "%B %Y")
  
}

