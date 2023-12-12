res <- read.csv("Datasets/Residuals_180.csv")
data <- read.csv("Datasets/Dataset_180.csv")
data <- data[!is.na(data$Count_180) ,]

temp <- as.numeric(res[, 1])
hist(temp)

temp <- round(as.numeric(colMeans(res)))

sum(temp==0)/length(temp)
hist(temp)

dati <- data.frame(res = as.numeric(colMeans(res)), stazione = data$idSensore, anno = data$Year, mese = data$Month)

mat <- matrix(NA, nrow = length(unique(dati$stazione)), ncol = length(unique(dati$anno))*length(unique(dati$mese)))
for (i in 1:length(unique(dati$stazione)))
{
  for (j in 1:length(unique(dati$anno)))
  {
    for (k in 1:length(unique(dati$mese)))
    {
      row <- which(dati$stazione == unique(dati$stazione)[i] & dati$anno ==
                     unique(dati$anno)[j] & dati$mese==unique(dati$mese)[k])
      if(length(row))
      {
        mat[i, k+7*(j-1)] <- dati$res[row]
      }
    }
  }
}

matplot(mat, type='l')

zip <- as.numeric(round(dati$res))
rows <- which(data$Count_180 == 0)
hist(zip[rows])
sum(zip[rows]>0)/length(rows)

####120_ Dataset####
res <- read.csv("Datasets/Residuals_120.csv")
data <- read.csv("Datasets/Dataset_120.csv")
data <- data[!is.na(data$Count_120) ,]

temp <- as.numeric(res[, 1])
hist(temp)

temp <- round(as.numeric(colMeans(res)))

sum(temp==0)/length(temp)
hist(temp)

dati <- data.frame(res = as.numeric(colMeans(res)), stazione = data$idSensore, anno = data$Year, mese = data$Month)

mat <- matrix(NA, nrow = length(unique(dati$stazione)), ncol = length(unique(dati$anno))*length(unique(dati$mese)))
for (i in 1:length(unique(dati$stazione)))
{
  for (j in 1:length(unique(dati$anno)))
  {
    for (k in 1:length(unique(dati$mese)))
    {
      row <- which(dati$stazione == unique(dati$stazione)[i] & dati$anno ==
                     unique(dati$anno)[j] & dati$mese==unique(dati$mese)[k])
      if(length(row))
      {
        mat[i, k+7*(j-1)] <- dati$res[row]
      }
    }
  }
}

matplot(mat, type='l')

zip <- as.numeric(round(dati$res))
rows <- which(data$Count_120 == 0)
hist(zip[rows])
sum(zip[rows]>0)/length(rows)



