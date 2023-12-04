library(dplyr)
library(tidyr)

weather <- read.csv("./Dataset_construction/weather_data.csv")
colnames(weather)[c(3,4,5,6,7,8,9)] = c("temp","prec_sum","prec_hours","max_wind","max_radiation","day_above_quantile","IdSensore")

density = read.csv("./Dataset_construction/density.csv")
names(density) = c("IdSensore","Comune","2010","2011","2012","2013",
                   "2014","2015","2016","2017","2018","2019","2020","2021","2022")

y120 <- read.csv("./Datasets/Dataset_120.csv")
y180 <- read.csv("./Datasets/Dataset_180.csv")

colnames(y120)[2] <- "IdSensore"
colnames(y180)[2] <- "IdSensore"

y120$Count_180 = y180$Count_180

merged_df <- merge(y120, weather, by = c("IdSensore","Month", "Year" ))
colnames(merged_df)[c(4,5)] = c("count_120","count_180")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#EFFFFF", "#77AADD", "#4477AA"))
corrplot::corrplot(cor(na.omit(merged_df[,c(4,5,6,8,9,10)])), method = "color", col = col(200),
                   addCoef.col = "black", # order = "hclust" ,
                   tl.col="black", tl.srt=45, insig = "blank", sig.level = .05, diag = F)



# Temperature
wrapped1<- weather[,c(1,2,3,9)] %>%
  pivot_wider(names_from = IdSensore, values_from = temp, values_fill = 0)

# Radiation
wrapped2<- weather[,c(1,2,7,9)] %>%
  pivot_wider(names_from = IdSensore, values_from = max_radiation, values_fill = 0)


# plot together 

x11()
par(mfrow = c(2,1))

# 1
matplot(wrapped1[64:84,-c(1,2)], type = 'l',  
        ylab = "Mean temperature", xaxt = "n", main = "Temperature in 2019-2021")

ticks <- c(4,11.5,18.5)
labels <- c("2019","2020","2021")
axis(1, at = c(ticks), labels = labels)
abline(v = c(8,15), lty = 4) 

#2
matplot(wrapped2[64:84,-c(1,2)], type = 'l',  
        ylab = "Max radiation", xaxt = "n")

ticks <- c(4,11.5,18.5)
labels <- c("2019","2020","2021")
axis(1, at = c(ticks), labels = labels)
abline(v = c(8,15), lty = 4) 

