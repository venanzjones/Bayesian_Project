library(dplyr)
library(tidyr)
weather <- read.csv("Weather Analysis/weather_data.csv")
colnames(weather)[c(3,4,5,6)] = c("temp","prec_sum","prec_hours","max_wind")

#temperature
wrapped_temp <- weather[,-c(4,5,6)] %>%   
  pivot_wider(names_from = Station, values_from = temp, values_fill = 0)

matplot(wrapped_temp[,-c(1,2)], pch = 16, type = 'l')

# prec sum 
wrapped_prec_sum <- weather[,-c(3,5,6)] %>%   
  pivot_wider(names_from = Station, values_from = prec_sum, values_fill = 0)

matplot(wrapped_prec_sum[,-c(1,2)], pch = 16, type = 'l')

# prec hours
wrapped_prec_hours <- weather[,-c(3,4,6)] %>%   
  pivot_wider(names_from = Station, values_from = prec_hours, values_fill = 0)

matplot(wrapped_prec_hours[,-c(1,2)], pch = 16, type = 'l')

# wind
wrapped_wind <- weather[,-c(3,4,5)] %>%   
  pivot_wider(names_from = Station, values_from = max_wind, values_fill = 0)

matplot(wrapped_wind[,-c(1,2)], pch = 16, type = 'l')

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot::corrplot(cor(weather[,c(3,4,5,6)]), method = "color", col = col(200),
                   addCoef.col = "black", order = "hclust" ,
                   tl.col="black", tl.srt=45, insig = "blank", sig.level = .01)


