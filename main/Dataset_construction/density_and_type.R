# Here I create the dataset "by hand" (most tedious and boring part, thank Vena later)

stazioni = read.csv("./Dati_iniziali/stazioni_O3.csv", )

Types = data.frame(type = rep("urban",51))

type = {rep("urban",51)}
# 45.62736  ,9.026401 urban
# 45.69795  ,9.406191 urban
# 45.30950  ,9.585790 rural 3
type[3] = "rural"
# 45.86375  ,9.399950 urban
# 45.46242  ,8.880210 urban
# 45.14254 ,10.043836 urban
# 45.69043  ,9.484261 factory 7
type[7] = "factory"
# 44.99955  ,9.008437 urban 
# 46.13228  ,9.566180 urban
# 45.55104  ,9.162614 urban 


# 45.69104  ,9.643651 urban
# 45.15047 , 9.930596 factory 12 
type[12] = "factory"
# 45.04008  ,8.914145 rural 13
type[13] = "rural"
# 45.47900  ,9.235491 urban
# 45.30278  ,9.495274 urban
# 45.54852  ,8.847322 urban
# 45.81504  ,9.066971 urban
# 45.23349  ,9.666250 rural 18
type[18] = "rural"
# 45.58026  ,9.273573 urban 
# 45.51304 ,10.191942 factory 20
type[20] = "factory"

# 45.24954 ,10.299119 urban
# 45.83690  ,8.803926 urban
# 45.36631  ,9.703946 urban
# 45.06966  ,8.868889 rural 24
type[24] = "rural"
# 45.01688 ,11.076095 rural 25 
type[25] = "rural"
# 45.19468  ,9.164638 urban
# 45.64963 ,10.205090 factory 27
type[27] = "factory"
# 46.13814  ,9.384687 urban
# 45.87460 ,10.177357 urban
# 45.58289  ,8.842165 factory  30
type[30] = "factory"
# 45.11350  ,8.874065 rural 31
type[31] = "rural"
# 45.62056  ,9.611738 rural 32
type[32] = "rural"
# 45.84221  ,9.351658 urban
# 45.46375 ,10.480772 urban
# 45.48363  ,9.327360 urban

# 44.92469 ,10.517502 urban
# 45.80857  ,9.221779 urban
# 45.66176  ,9.156644 urban
# 45.16057 ,10.795564 urban
# 45.49823  ,9.556232 rural 40
type[40] = "rural"
# 46.46952 ,10.375433 rural 41
type[41] = "rural"
# 45.41277 ,10.683357 urban
# 45.61924  ,8.756970 urban
# 45.46335  ,9.195325 urban
# 46.01583  ,9.286409 urban
# 45.73084  ,9.125734 urban
# 45.91279  ,9.497538 rural 47
type[47] = "rural"
# 45.60123  ,9.275073 urban
# 45.28196 , 8.988563 urban
# 46.16785,  9.879210 urban
# 45.27849, 10.006202 rural 51
type[51] = "rural"

Types$type = type


density = stazioni.usate[,c("IdSensore","Comune")]
density[,c(3:15)] = 0
names(density) = c("IdSensore","Comune","2010","2011","2012","2013",
                   "2014","2015","2016","2017","2018","2019","2020","2021","2022")

comuni = unique(density$Comune) 
for( comune in comuni ){
  cm <- read.csv(paste0("./Dataset_construction/comuni/", comune, ".csv"), header = T)[,c(11:20)]
  density[which(density$Comune == comune),3:12] = tail.matrix(cm, 1)
}

ultimi = readxl::read_xlsx("./Dataset_construction/comuni/prova.xlsx")

for( comune in comuni ){
  if (comune != "Magenta" & comune !="Calusco d'Adda" & comune != "Cornale"){
  density[which(density$Comune == comune),13] = ultimi[which(ultimi$Comune == comune),2]
  density[which(density$Comune == comune),14] = ultimi[which(ultimi$Comune == comune),3]
  density[which(density$Comune == comune),15] = ultimi[which(ultimi$Comune == comune),4]
}
}

density[which(density$Comune ==  "Calusco d'Adda"),13] = 8260
density[which(density$Comune ==  "Calusco d'Adda"),14] = 8210
density[which(density$Comune ==  "Calusco d'Adda"),15] = 8231

density[which(density$Comune ==  "Magenta"),13] = 24082	
density[which(density$Comune ==  "Magenta"),14] = 24107
density[which(density$Comune ==  "Magenta"),15] = 24130

density[which(density$Comune ==  "Cornale"),13] = 831	
density[which(density$Comune ==  "Cornale"),14] = 820	
density[which(density$Comune ==  "Cornale"),15] = 798

estensioni = readxl::read_xlsx("./Dataset_construction/comuni/Estensione_comuni.xlsx")
colnames(estensioni) = c("Comune","Est")

for (comune in comuni) {
  idx = which(density$Comune == comune)
  est = as.numeric(estensioni[which(estensioni$Comune == comune), 2]$Est)
  density[idx, 3:15] = density[idx, 3:15] / est
}


density$Type = Types$type
write.csv(density, "./Dataset_construction/density.csv", row.names = FALSE)
