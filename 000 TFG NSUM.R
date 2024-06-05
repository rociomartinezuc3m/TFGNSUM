

# Resultados reales

c.val.real <- c(34.9, 32.1, 15.7, 15.2, 2.1)




#Librerías
  
  
library(networkscaleup)
#library(NSUM)

library(readr)
library(digest)
library(openxlsx)
library(data.table)


#Dataset into R
  
df <- read.csv("00Pollfish_Survey_Elecciones_generales_Comunitat_Valenciana_381775628.csv", sep=";")

# bye columna de permiso

df <- as.data.frame(df[,-4])

# cambiamos los nombres a las columnas

colnames(df) <- c("Gender","Age","Year.Birth","PP","PSOE","Vox","Sumar","Blanco","NoSabe","edad.18.30","edad.31.54","edad.mas.54","autonomo","medico","desempleado","Education","Employment.Status","Income", "Provincia")


#Dataset: preprocessing
  

#Respondent voting inputs
voting.inputs <- c("PP","PSOE","Vox","Sumar","Blanco")
#Sociodemo characteristics
socio.demo.inputs <- c("Gender","Age","Year.Birth")
#Control inputs
control.inputs <- c("edad.18.30","edad.31.54","edad.mas.54","autonomo","medico","desempleado")
#Prov inputs
prov.inputs <- c("Provincia")
#Edades inputs (for outlier detection and removal)
edades.inputs <- c("edad.18.30","edad.31.54","edad.mas.54")



# COMUNIDAD VALENCIANA

Nos quedamos con las columnas que no tengan Nulos ninguno de los campos importantes:
  **socio.demo.inputs, voting.inputs, control.inputs, prov.inputs**
  
## Visualización de datos
  
dat <- df[,c(socio.demo.inputs,voting.inputs,control.inputs,prov.inputs)]
dat <- dat[1:198,]# prov.inputs
library(DataExplorer)
library(ggplot2)
library(skimr)
skim(dat)
plot_missing(
  data   = dat,
  title  = "Porcentaje de valores ausentes", ggtheme = theme_bw(),
  theme_config = list(legend.position = "none") )



dat <- df[,c(socio.demo.inputs,voting.inputs,control.inputs,prov.inputs)]
colnames(dat)
dim(dat)
dat <- na.omit(dat) #Only rows with complete cases are considered
dim(dat)

input.dat<-dat
dim(input.dat)



#Correlation
unique(input.dat$Age)
correlacion_votos_edades <- cor(input.dat[, voting.inputs], input.dat[, edades.inputs])
print(correlacion_votos_edades)





#Real data
  
c.val.subpopulation.sizes <- c(737916,1274008,1597865,360765,17430,3525000) 

# Data sources: https://www.ine.es/dyngs/CEL/index.htm?cid=41 
#               https://www.mites.gob.es/ficheros/ministerio/sec_trabajo/autonomos/economia-soc/NoticiasDoc/NoticiasPortada/2022/Nota_Afiliacin-trabajo-autnomo_marzo_2022.pdf
#               https://www.sanidad.gob.es/estadEstudios/sanidadDatos/tablas/tabla13.htm  
#[1] = 1.022.704 = 737.916(Val) (18 - 34)
#[2] = 1.739.995 = 1.274.008(val) (35 - 54)
#[3] = 2.085.330 = 1.597.865(val) (>=55)
#[4] = 417316 = autónomos dados de alta en 20222 --> c.val = 360765
# https://www.mites.gob.es/ficheros/ministerio/sec_trabajo/autonomos/economia-soc/NoticiasDoc/NoticiasPortada/2022/Nota_Afiliacin-trabajo-autnomo_marzo_2022.pdf
#[5] = 25328 = total profesionales de la medicina --_> c.val 17179 // 17 430
# https://www.sanidad.gob.es/estadEstudios/sanidadDatos/tablas/tabla13.htm  
#^[6] = 399100 = desempleados ---> 3525000 
# https://www.ine.es/jaxiT3/Datos.htm?t=4245



Total <- 3609789 # Data source https://www.ine.es/jaxi/Datos.htm?tpx=48409 (dato de la suma de las edades)
Total

c.val.subpopulation.sizes # Totals
round(c.val.subpopulation.sizes/Total,4) # Proportions
names(c.val.subpopulation.sizes) <- control.inputs


#NSUM estimation methods
  
#Preparation


# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum <- input.dat[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados brutos 
## 1.1 Con los datos brutos %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results <- colMeans(input.dat.nsum[,voting.inputs]/rowSums(input.dat.nsum[,voting.inputs]),na.rm = TRUE)
c.val.naive.results



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results <- colSums(input.dat.nsum[,voting.inputs])/sum(input.dat.nsum[,voting.inputs])
c.val.RoS.results


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees <- Total*rowMeans(input.dat.nsum[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes <- Total*apply(input.dat.nsum[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees,na.rm = TRUE))

c.val.MoS.results <- prop.table(c.val.MoS.sizes)
c.val.MoS.results


#MLE method
###########

c.val.mle.est <- networkscaleup::killworth(input.dat.nsum,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results <- prop.table(Total*apply(input.dat.nsum[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est$degrees)))

c.val.mle.results


#Plug-in MLE method
###################

c.val.pimle.est <- networkscaleup::killworth(input.dat.nsum,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results <- prop.table(Total*apply(input.dat.nsum[,voting.inputs],2,function (x) mean(x[c.val.pimle.est$degrees>0]/c.val.pimle.est$degrees[c.val.pimle.est$degrees>0]))) 

c.val.pimle.results



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val <- data.frame(naive.est = c.val.naive.results,MoS.est = c.val.MoS.results,RoS.est=c.val.RoS.results,mle.est = c.val.mle.results, pimle.est = c.val.pimle.results)
Summary.Estimation.Matrix.c.val
t(round(100*Summary.Estimation.Matrix.c.val[c("Vox","PP","PSOE","Sumar","Blanco"),],1))

### 1.1.1 MSE


c.val.real <- c(34.9, 32.1, 15.7, 15.2, 2.1)

Summary.Estimation.Matrix.c.val <- round(Summary.Estimation.Matrix.c.val*100)
Summary.Estimation.Matrix.c.val


mse.naive.c.val <-mean((Summary.Estimation.Matrix.c.val[,1] - c.val.real)^2)
mse.mos.c.val <-mean((Summary.Estimation.Matrix.c.val[,2] - c.val.real)^2)
mse.mle.c.val <-mean((Summary.Estimation.Matrix.c.val[,4] - c.val.real)^2)
mse.pimle.c.val <-mean((Summary.Estimation.Matrix.c.val[,5] - c.val.real)^2)
mse.c.val <- data.frame(mse.naive.c.val = mse.naive.c.val,mse.mos.c.val = mse.mos.c.val, mse.mle.c.val = mse.mle.c.val, mse.pimle.c.val = mse.pimle.c.val)
mse.c.val
cat("El MSE medio es:",mean(mse.naive.c.val,mse.mos.c.val,mse.mle.c.val,mse.pimle.c.val), "\n")




## 1.2. Escaños con datos brutos 


# Function to compute seats using D'Hondt method

compute_dhont_seats <- function(votes, seats) {
  # Create a vector to store the number of seats each party receives
  party_seats <- rep(0, length(votes))
  
  # Loop to allocate seats
  for (i in 1:seats) {
    # Calculate the allocation ratio for each party
    allocation_ratios <- votes / (party_seats + 1)
    
    # Find the party with the highest allocation ratio
    party_with_highest_ratio <- which.max(allocation_ratios)
    
    # Allocate a seat to the party with the highest ratio
    party_seats[party_with_highest_ratio] <- party_seats[party_with_highest_ratio] + 1
  }
  
  # Return the vector of seats allocated to each party
  return(party_seats)
}

#Estimation of number of seats using D'Hont method (need to upload the function compute_dhont_seats)

total.number.seats <- 33

DHont.pimle <- compute_dhont_seats(Summary.Estimation.Matrix.c.val$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val$pimle.est[1:4]),total.number.seats)
names(DHont.pimle) <- rownames(Summary.Estimation.Matrix.c.val["pimle.est"])[sort(Summary.Estimation.Matrix.c.val[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle

DHont.mle <- compute_dhont_seats(Summary.Estimation.Matrix.c.val$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val$mle.est[1:4]),total.number.seats)
names(DHont.mle) <- rownames(Summary.Estimation.Matrix.c.val["mle.est"])[sort(Summary.Estimation.Matrix.c.val[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.mle

DHont.MoS <- compute_dhont_seats(Summary.Estimation.Matrix.c.val$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val$MoS.est[1:4]),total.number.seats)
names(DHont.MoS) <- rownames(Summary.Estimation.Matrix.c.val["MoS.est"])[sort(Summary.Estimation.Matrix.c.val[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS

DHont.naive <- compute_dhont_seats(Summary.Estimation.Matrix.c.val$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val$naive.est[1:4]),total.number.seats)
names(DHont.naive) <- rownames(Summary.Estimation.Matrix.c.val["naive.est"])[sort(Summary.Estimation.Matrix.c.val[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont <- data.frame(naive.est = DHont.naive[political.groups],MoS.est = DHont.MoS[political.groups],mle.est = DHont.mle[political.groups], pimle.est = DHont.pimle[political.groups])
t(Summary.Estimation.Matrix.c.val.DHont)




#Outliers: primer criterio
  
  
###############################################################################

#Criterion for retaining the respondents with a network under a specific threshold
threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 #0.9 --> 0.8 (20%)
quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

#Criterion for retaining the respondents who declare a network with blank voting under a specific threshold

#threshold.net.size.vote.blank <- 9 # Disable this line if you want to fix the threshold by hand


input.dat.1 <- dat[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)) ,] # Filtering by network size vote declaration 
dim(input.dat.1)


#Correlation
  

unique(input.dat.1$Age)
correlacion_votos_edades.1 <- cor(input.dat.1[, voting.inputs], input.dat.1[, edades.inputs])
print(correlacion_votos_edades.1)




#NSUM estimation methods
#Preparation


# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.1 <- input.dat.1[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.1)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.1) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados con el primer filtrado
## 2.1. Con el primer criterio %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.1 <- colMeans(input.dat.nsum.1[,voting.inputs]/rowSums(input.dat.nsum.1[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.1



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.1 <- colSums(input.dat.nsum.1[,voting.inputs])/sum(input.dat.nsum.1[,voting.inputs])
c.val.RoS.results.1


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.1 <- Total*rowMeans(input.dat.nsum.1[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.1 <- Total*apply(input.dat.nsum.1[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.1,na.rm = TRUE))

c.val.MoS.results.1 <- prop.table(c.val.MoS.sizes.1)
c.val.MoS.results.1


#MLE method
###########

c.val.mle.est.1 <- networkscaleup::killworth(input.dat.nsum.1,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.1 <- prop.table(Total*apply(input.dat.nsum.1[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.1$degrees)))

c.val.mle.results.1


#Plug-in MLE method
###################

c.val.pimle.est.1 <- networkscaleup::killworth(input.dat.nsum.1,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.1 <- prop.table(Total*apply(input.dat.nsum.1[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.1$degrees>0]/c.val.pimle.est.1$degrees[c.val.pimle.est.1$degrees>0]))) 

c.val.pimle.results.1



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.1 <- data.frame(naive.est = c.val.naive.results.1,MoS.est = c.val.MoS.results.1,RoS.est=c.val.RoS.results.1,mle.est = c.val.mle.results.1, pimle.est = c.val.pimle.results.1)
Summary.Estimation.Matrix.c.val.1
t(round(100*Summary.Estimation.Matrix.c.val.1[c("Vox","PP","PSOE","Sumar","Blanco"),],1))


Summary.Estimation.Matrix.c.val.1 <- round(Summary.Estimation.Matrix.c.val.1*100)
Summary.Estimation.Matrix.c.val.1


mse.naive.c.val.1 <-mean((Summary.Estimation.Matrix.c.val.1[,1] - c.val.real)^2)
mse.mos.c.val.1 <-mean((Summary.Estimation.Matrix.c.val.1[,2] - c.val.real)^2)
mse.mle.c.val.1 <-mean((Summary.Estimation.Matrix.c.val.1[,4] - c.val.real)^2)
mse.pimle.c.val.1 <-mean((Summary.Estimation.Matrix.c.val.1[,5] - c.val.real)^2)
mse.c.val.1 <- data.frame(mse.naive.c.val.1 = mse.naive.c.val.1,mse.mos.c.val.1 = mse.mos.c.val.1, mse.mle.c.val.1 = mse.mle.c.val.1, mse.pimle.c.val.1 = mse.pimle.c.val.1)
mse.c.val.1
cat("El MSE medio es:",mean(mse.naive.c.val.1,mse.mos.c.val.1,mse.mle.c.val.1,mse.pimle.c.val.1), "\n")



## 2.2. Escaños con el primer criterio


DHont.pimle.1 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.1$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.1$pimle.est[1:4]),total.number.seats)
names(DHont.pimle.1) <- rownames(Summary.Estimation.Matrix.c.val.1["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.1[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.1

DHont.mle.1 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.1$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.1$mle.est[1:4]),total.number.seats)
names(DHont.mle.1) <- rownames(Summary.Estimation.Matrix.c.val.1["mle.est"])[sort(Summary.Estimation.Matrix.c.val.1[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.1

DHont.MoS.1 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.1$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.1$MoS.est[1:4]),total.number.seats)
names(DHont.MoS.1) <- rownames(Summary.Estimation.Matrix.c.val.1["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.1[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.1

DHont.naive.1 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.1$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.1$naive.est[1:4]),total.number.seats)
names(DHont.naive.1) <- rownames(Summary.Estimation.Matrix.c.val.1["naive.est"])[sort(Summary.Estimation.Matrix.c.val.1[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.1

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.1 <- data.frame(naive.est = DHont.naive.1[political.groups],MoS.est = DHont.MoS.1[political.groups],mle.est = DHont.mle.1[political.groups], pimle.est = DHont.pimle.1[political.groups])
Summary.Estimation.Matrix.c.val.DHont.1



#Outliers: segundo criterio (votos en blanco)
  

# Functions for outlier detection

# Calcula la desviación absoluta mediana (MAD)
median_abs_deviation <- function(x) {
  qq.scaled <- quantile(scale(x), c(.25, .5, .75), na.rm = T)
  quantile(abs(x - quantile(x, c(0.5), na.rm = T)), c(0.5), na.rm = T) * 1.4826
}

# Determina si los valores en un conjunto de datos son valores atípicos (outliers) 
# en función de la desviación absoluta mediana.

is_mad_outlier <- function(x,threshold.mad=5) {
  abs(x - quantile(x, c(0.5), na.rm = T)) / median_abs_deviation(x) > threshold.mad
}


if(median_abs_deviation(dat[,voting.inputs[5]])!=0){
  outliers.vote.blank.flag <- is_mad_outlier(dat[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
  outliers.vote.blank.flag
  
  threshold.net.size.vote.blank <- max(dat[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
  threshold.net.size.vote.blank
  
  input.dat.2 <- dat[which(dat[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
  dim(input.dat.2)
}else{
  input.dat.2<-dat
  dim(input.dat.2)
}



# Como se oberva la dimensión es igual a la que se obtiene en los datos brutos, por lo que no se puede aplicar el segundo criterio de filtrado


#Outliers: tercer criterio
  
###############################################################################


input.dat.3 <- dat[which(apply(dat[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] # Filtering criterion for a political group majoritary voting 
dim(input.dat.3)



#Correlation
  

unique(input.dat.3$Age)
correlacion_votos_edades.3 <- cor(input.dat.3[, voting.inputs], input.dat.3[, edades.inputs])
print(correlacion_votos_edades.3)



#NSUM estimation methods
#Preparation

# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.3 <- input.dat.3[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.3)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.3) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente





## Resultados con el tercer filtrado
## 3.1. Con el tercer criterio %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.3 <- colMeans(input.dat.nsum.3[,voting.inputs]/rowSums(input.dat.nsum.3[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.3



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.3 <- colSums(input.dat.nsum.3[,voting.inputs])/sum(input.dat.nsum.3[,voting.inputs])
c.val.RoS.results.3


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.3 <- Total*rowMeans(input.dat.nsum.3[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.3 <- Total*apply(input.dat.nsum.3[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.3,na.rm = TRUE))

c.val.MoS.results.3 <- prop.table(c.val.MoS.sizes.3)
c.val.MoS.results.3


#MLE method
###########

c.val.mle.est.3 <- networkscaleup::killworth(input.dat.nsum.3,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.3 <- prop.table(Total*apply(input.dat.nsum.3[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.3$degrees)))

c.val.mle.results.3


#Plug-in MLE method
###################

c.val.pimle.est.3 <- networkscaleup::killworth(input.dat.nsum.3,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.3 <- prop.table(Total*apply(input.dat.nsum.3[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.3$degrees>0]/c.val.pimle.est.3$degrees[c.val.pimle.est.3$degrees>0]))) 

c.val.pimle.results.3



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.3 <- data.frame(naive.est = c.val.naive.results.3,MoS.est = c.val.MoS.results.3,RoS.est=c.val.RoS.results.3,mle.est = c.val.mle.results.3, pimle.est = c.val.pimle.results.3)
Summary.Estimation.Matrix.c.val.3
t(round(100*Summary.Estimation.Matrix.c.val.3[c("Vox","PP","PSOE","Sumar","Blanco"),],1))


Summary.Estimation.Matrix.c.val.3 <- round(Summary.Estimation.Matrix.c.val.3*100)
Summary.Estimation.Matrix.c.val.3


mse.naive.c.val.3 <-mean((Summary.Estimation.Matrix.c.val.3[,1] - c.val.real)^2)
mse.mos.c.val.3 <-mean((Summary.Estimation.Matrix.c.val.3[,2] - c.val.real)^2)
mse.mle.c.val.3 <-mean((Summary.Estimation.Matrix.c.val.3[,4] - c.val.real)^2)
mse.pimle.c.val.3 <-mean((Summary.Estimation.Matrix.c.val.3[,5] - c.val.real)^2)
mse.c.val.3 <- data.frame(mse.naive.c.val.3 = mse.naive.c.val.3,mse.mos.c.val.3 = mse.mos.c.val.3, mse.mle.c.val.3 = mse.mle.c.val.3, mse.pimle.c.val.3 = mse.pimle.c.val.3)
mse.c.val.3
cat("El MSE medio es:",mean(mse.naive.c.val.3,mse.mos.c.val.3,mse.mle.c.val.3,mse.pimle.c.val.3), "\n")



## 3.2. Escaños con el tercer criterio


DHont.pimle.3 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.3$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.3$pimle.est[1:4]),total.number.seats)
names(DHont.pimle.3) <- rownames(Summary.Estimation.Matrix.c.val.3["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.3[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.3

DHont.mle.3 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.3$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.3$mle.est[1:4]),total.number.seats)
names(DHont.mle.3) <- rownames(Summary.Estimation.Matrix.c.val.3["mle.est"])[sort(Summary.Estimation.Matrix.c.val.3[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.3

DHont.MoS.3 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.3$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.3$MoS.est[1:4]),total.number.seats)
names(DHont.MoS.3) <- rownames(Summary.Estimation.Matrix.c.val.3["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.3[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.3

DHont.naive.3 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.3$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.3$naive.est[1:4]),total.number.seats)
names(DHont.naive.3) <- rownames(Summary.Estimation.Matrix.c.val.3["naive.est"])[sort(Summary.Estimation.Matrix.c.val.3[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.3

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.3 <- data.frame(naive.est = DHont.naive.3[political.groups],MoS.est = DHont.MoS.3[political.groups],mle.est = DHont.mle.3[political.groups], pimle.est = DHont.pimle.3[political.groups])
Summary.Estimation.Matrix.c.val.DHont.3

#Outliers con la combinación de los criterios
  

if(median_abs_deviation(dat[,voting.inputs[5]])!=0){
  outliers.vote.blank.flag <- is_mad_outlier(dat[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
  outliers.vote.blank.flag
  
  threshold.net.size.vote.blank <- max(dat[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
  threshold.net.size.vote.blank
  
  input.dat <- dat[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95 & dat[,voting.inputs[5]]<=threshold.net.size.vote.blank),] #Customized filtering 
  dim(input.dat)
  
  #Filtering of outliers using the criteria separately#
  
  #input.dat <- dat[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)) ,] # Filtering by network size vote declaration 
  #dim(input.dat)
  #input.dat <- dat[which(apply(dat[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] # Filtering criterion for a political group majoritary voting 
  #dim(input.dat)
  input.dat.4 <- dat[which(dat[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
  dim(input.dat.4)
}else{
  input.dat.4 <- dat[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] #Customized filtering 
  dim(input.dat.4)
}


## Resultados con la combinación de los criterios
#Correlation
  
unique(input.dat.4$Age)
correlacion_votos_edades.4 <- cor(input.dat.4[, voting.inputs], input.dat.4[, edades.inputs])
print(correlacion_votos_edades.4)





# NSUM estimation methods
#Preparation
  

# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.4 <- input.dat.4[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.4)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.4) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados con el primer filtrado
## 5.1. Con el primer criterio %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.4 <- colMeans(input.dat.nsum.4[,voting.inputs]/rowSums(input.dat.nsum.4[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.4



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.4 <- colSums(input.dat.nsum.4[,voting.inputs])/sum(input.dat.nsum.4[,voting.inputs])
c.val.RoS.results.4


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.4 <- Total*rowMeans(input.dat.nsum.4[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.4 <- Total*apply(input.dat.nsum.4[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.4,na.rm = TRUE))

c.val.MoS.results.4 <- prop.table(c.val.MoS.sizes.4)
c.val.MoS.results.4


#MLE method
###########

c.val.mle.est.4 <- networkscaleup::killworth(input.dat.nsum.4,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.4 <- prop.table(Total*apply(input.dat.nsum.4[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.4$degrees)))

c.val.mle.results.4


#Plug-in MLE method
###################

c.val.pimle.est.4 <- networkscaleup::killworth(input.dat.nsum.4,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.4 <- prop.table(Total*apply(input.dat.nsum.4[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.4$degrees>0]/c.val.pimle.est.4$degrees[c.val.pimle.est.4$degrees>0]))) 

c.val.pimle.results.4



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.4 <- data.frame(naive.est = c.val.naive.results.4,MoS.est = c.val.MoS.results.4,RoS.est=c.val.RoS.results.4,mle.est = c.val.mle.results.4, pimle.est = c.val.pimle.results.4)
Summary.Estimation.Matrix.c.val.4
t(round(100*Summary.Estimation.Matrix.c.val.4[c("Vox","PP","PSOE","Sumar","Blanco"),],1))


Summary.Estimation.Matrix.c.val.4 <- round(Summary.Estimation.Matrix.c.val.4*100)
Summary.Estimation.Matrix.c.val.4


mse.naive.c.val.4 <-mean((Summary.Estimation.Matrix.c.val.4[,1] - c.val.real)^2)
mse.mos.c.val.4 <-mean((Summary.Estimation.Matrix.c.val.4[,2] - c.val.real)^2)
mse.mle.c.val.4 <-mean((Summary.Estimation.Matrix.c.val.4[,4] - c.val.real)^2)
mse.pimle.c.val.4 <-mean((Summary.Estimation.Matrix.c.val.4[,5] - c.val.real)^2)
mse.c.val.4 <- data.frame(mse.naive.c.val.4 = mse.naive.c.val.4,mse.mos.c.val.4 = mse.mos.c.val.4, mse.mle.c.val.4 = mse.mle.c.val.4, mse.pimle.c.val.4 = mse.pimle.c.val.4)
mse.c.val.4
cat("El MSE medio es:",mean(mse.naive.c.val.4,mse.mos.c.val.4,mse.mle.c.val.4,mse.pimle.c.val.4), "\n")



## 5.2. Escaños con la combinación de criterios


DHont.pimle.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.4$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.4$pimle.est[1:4]),total.number.seats)
names(DHont.pimle.4) <- rownames(Summary.Estimation.Matrix.c.val.4["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.4[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.4

DHont.mle.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.4$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.4$mle.est[1:4]),total.number.seats)
names(DHont.mle.4) <- rownames(Summary.Estimation.Matrix.c.val.4["mle.est"])[sort(Summary.Estimation.Matrix.c.val.4[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.4

DHont.MoS.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.4$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.4$MoS.est[1:4]),total.number.seats)
names(DHont.MoS.4) <- rownames(Summary.Estimation.Matrix.c.val.4["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.4[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.4

DHont.naive.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.4$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.4$naive.est[1:4]),total.number.seats)
names(DHont.naive.4) <- rownames(Summary.Estimation.Matrix.c.val.4["naive.est"])[sort(Summary.Estimation.Matrix.c.val.4[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.4

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.4 <- data.frame(naive.est = DHont.naive.4[political.groups],MoS.est = DHont.MoS.4[political.groups],mle.est = DHont.mle.4[political.groups], pimle.est = DHont.pimle.4[political.groups])
Summary.Estimation.Matrix.c.val.DHont.4


## Diagrama de Venn


threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 #0.9 --> 0.8 (20%)
quantile.vote <- quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

index.1 <- which(apply(dat[,voting.inputs],1,sum) >= quantile.vote)



# 2. Outliers

index.2 <- which(apply(dat[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))>0.95)


# Diagrama de Venn

indices_comunes <- intersect(index.1, index.2)


library(VennDiagram)
venn.plot <- venn.diagram(
  x = list("Outliers 1" = index.1, "Outliers 2" = index.2),
  category.names = c("Outliers 1", "Outliers 2"),
  filename = NULL,
  width = 25,
  height = 25,
  main = "Outliers: Comunidad Valenciana",
  cat.col = c("black", "black"),  # Colores de los círculos
  fill = c("#B9DDF1", "#2E5B88"), # Colores de las categorías
  cat.fontfamily = "sans"
)

grid.draw(venn.plot)




# Mostrar el número de coincidencias
cat("Número de coincidencias entre Forma 1 y Forma 2:", length(indices_comunes), "\n")





# EVOLUCIÓN A LO LARGO DEL TIEMPO

#Distribución por fechas

datos.fechas <- read.csv("~/Desktop/UNI/4º EYE/TFG/00 R/00Pollfish_Survey_Elecciones_generales_Comunitat_Valenciana_fechas.csv",sep=";")
time.inputs <- c("Time.Started", "Time.Finished")
datos.fechas <- na.omit(datos.fechas)
dim(datos.fechas)

# Tiene tres filas más que nuestros datos, *dat*, por lo que eliminamos las filas que habíamos identificado como nulas previamente.

Recordemos que son las filas 13, 98 y 139 

#no.filas <- c(13, 98, 139)
#datos.fechas <- datos.fechas[-no.filas,]
#dim(datos.fechas)


#Solucionado el problema, estudiemos la frecuencia.


# Nos quedamos solo con la fecha del campo Time.Finshed
fecha.encuestas <- as.Date(datos.fechas$Time.Finished) 
fecha.encuestas[1:3]



frec.por.dias <- table(fecha.encuestas)
barplot(frec.por.dias, 
        main = "Distribución de finalización de encuestas a lo largo del tiempo",
        xlab = "Fecha",
        ylab = "Número de encuestas finalizadas",
        col = "skyblue")

#Número de encuestas antes y después


encuestas.before <- sum(fecha.encuestas <= as.Date("2023-07-14"))
encuestas.after <- sum(fecha.encuestas > as.Date("2023-07-14"))

Porcentaje de encuestas antes y después


encuestas.before.perc <- encuestas.before/nrow(datos.fechas)
encuestas.after.perc <- encuestas.after/nrow(datos.fechas)
cat("Antes del día 15 de julio se realizaron", encuestas.before, "lo que supone un", round(encuestas.before.perc,1),"%","\n")
cat("A partir del día 15 de julio en adelante se realizaron", encuestas.after, "lo que supone un", round(encuestas.after.perc,1),"%","\n")

#Creo los dos data.frames para los resultados antes y después de la campaña electoral

#dat.before <- dat[1:147,]
#dat.after <- dat[148:nrow(dat),]

dat.before <- dat[1:148,]
dat.after <- dat[149:nrow(dat),]



# Before

## Resultados brutos

## 1.1 Con los datos brutos %


#NSUM estimation methods
  
#Preparation


# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.before <- dat.before
input.dat.nsum.before <- input.dat.before[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.before)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.before) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados brutos 
## 1.1 Con los datos brutos %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.before <- colMeans(input.dat.nsum.before[,voting.inputs]/rowSums(input.dat.nsum.before[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.before



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.before <- colSums(input.dat.nsum.before[,voting.inputs])/sum(input.dat.nsum.before[,voting.inputs])
c.val.RoS.results.before


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.before <- Total*rowMeans(input.dat.nsum.before[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.before <- Total*apply(input.dat.nsum.before[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.before,na.rm = TRUE))

c.val.MoS.results.before <- prop.table(c.val.MoS.sizes.before)
c.val.MoS.results.before


#MLE method
###########

c.val.mle.est.before <- networkscaleup::killworth(input.dat.nsum.before,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.before <- prop.table(Total*apply(input.dat.nsum.before[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.before$degrees)))

c.val.mle.results.before


#Plug-in MLE method
###################

c.val.pimle.est.before <- networkscaleup::killworth(input.dat.nsum.before,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.before <- prop.table(Total*apply(input.dat.nsum.before[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.before$degrees>0]/c.val.pimle.est.before$degrees[c.val.pimle.est.before$degrees>0]))) 

c.val.pimle.results.before



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.before <- data.frame(naive.est = c.val.naive.results.before,MoS.est = c.val.MoS.results.before,RoS.est=c.val.RoS.results.before,mle.est = c.val.mle.results.before, pimle.est = c.val.pimle.results.before)
Summary.Estimation.Matrix.c.val.before
t(round(100*Summary.Estimation.Matrix.c.val.before[c("Vox","PP","PSOE","Sumar","Blanco"),],1))


#MSE
  
  
  

Summary.Estimation.Matrix.c.val.before <- round(Summary.Estimation.Matrix.c.val.before*100)
Summary.Estimation.Matrix.c.val.before


mse.naive.c.val.before <-mean((Summary.Estimation.Matrix.c.val.before[,1] - c.val.real)^2)
mse.mos.c.val.before <-mean((Summary.Estimation.Matrix.c.val.before[,2] - c.val.real)^2)
mse.mle.c.val.before <-mean((Summary.Estimation.Matrix.c.val.before[,4] - c.val.real)^2)
mse.pimle.c.val.before <-mean((Summary.Estimation.Matrix.c.val.before[,5] - c.val.real)^2)
mse.c.val.before <- data.frame(mse.naive.c.val.before = mse.naive.c.val.before,mse.mos.c.val.before = mse.mos.c.val.before, mse.mle.c.val.before = mse.mle.c.val.before, mse.pimle.c.val.before = mse.pimle.c.val.before)
mse.c.val.before
cat("El MSE medio es:",mean(mse.naive.c.val.before,mse.mos.c.val.before,mse.mle.c.val.before,mse.pimle.c.val.before), "\n")



## 2.2. Escaños con el primer criterio


DHont.pimle.before <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before$pimle.est[1:4]),total.number.seats)
names(DHont.pimle.before) <- rownames(Summary.Estimation.Matrix.c.val.before["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.before[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.before

DHont.mle.before <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before$mle.est[1:4]),total.number.seats)
names(DHont.mle.before) <- rownames(Summary.Estimation.Matrix.c.val.before["mle.est"])[sort(Summary.Estimation.Matrix.c.val.before[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.before

DHont.MoS.before <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before$MoS.est[1:4]),total.number.seats)
names(DHont.MoS.before) <- rownames(Summary.Estimation.Matrix.c.val.before["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.before[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.before

DHont.naive.before <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before$naive.est[1:4]),total.number.seats)
names(DHont.naive.before) <- rownames(Summary.Estimation.Matrix.c.val.before["naive.est"])[sort(Summary.Estimation.Matrix.c.val.before[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.before

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.before <- data.frame(naive.est = DHont.naive.before[political.groups],MoS.est = DHont.MoS.before[political.groups],mle.est = DHont.mle.before[political.groups], pimle.est = DHont.pimle.before[political.groups])
Summary.Estimation.Matrix.c.val.DHont.before






#Outliers: primer criterio
  

###############################################################################

#Criterion for retaining the respondents with a network under a specific threshold
threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 #0.9 --> 0.8 (20%)
quantile(apply(dat.before[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

#Criterion for retaining the respondents who declare a network with blank voting under a specific threshold

#threshold.net.size.vote.blank <- 9 # Disable this line if you want to fix the threshold by hand


input.dat.before.1 <- dat[which(apply(dat.before[,voting.inputs],1,sum) < quantile(apply(dat.before[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)) ,] # Filtering by network size vote declaration 
dim(input.dat.before.1)


#Correlation
  
  
unique(input.dat.before.1$Age)
correlacion_votos_edades.before.1 <- cor(input.dat.before.1[, voting.inputs], input.dat.before.1[, edades.inputs])
print(correlacion_votos_edades.before.1)





#NSUM estimation methods
#Preparation
  
  
# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.before.1 <- input.dat.before.1[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.before.1)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.before.1) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente

## Resultados con el primer filtrado
## 2.1. Con el primer criterio %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.before.1 <- colMeans(input.dat.nsum.before.1[,voting.inputs]/rowSums(input.dat.nsum.before.1[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.before.1



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.before.1 <- colSums(input.dat.nsum.before.1[,voting.inputs])/sum(input.dat.nsum.before.1[,voting.inputs])
c.val.RoS.results.before.1


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.before.1 <- Total*rowMeans(input.dat.nsum.before.1[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.before.1 <- Total*apply(input.dat.nsum.before.1[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.before.1,na.rm = TRUE))

c.val.MoS.results.before.1 <- prop.table(c.val.MoS.sizes.before.1)
c.val.MoS.results.before.1


#MLE method
###########

c.val.mle.est.before.1 <- networkscaleup::killworth(input.dat.nsum.before.1,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.before.1 <- prop.table(Total*apply(input.dat.nsum.before.1[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.before.1$degrees)))

c.val.mle.results.before.1


#Plug-in MLE method
###################

c.val.pimle.est.before.1 <- networkscaleup::killworth(input.dat.nsum.before.1,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.before.1 <- prop.table(Total*apply(input.dat.nsum.before.1[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.before.1$degrees>0]/c.val.pimle.est.before.1$degrees[c.val.pimle.est.before.1$degrees>0]))) 

c.val.pimle.results.before.1



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.before.1 <- data.frame(naive.est = c.val.naive.results.before.1,MoS.est = c.val.MoS.results.before.1,RoS.est=c.val.RoS.results.before.1,mle.est = c.val.mle.results.before.1, pimle.est = c.val.pimle.results.before.1)
Summary.Estimation.Matrix.c.val.before.1
t(round(100*Summary.Estimation.Matrix.c.val.before.1[c("Vox","PP","PSOE","Sumar","Blanco"),],1))



Summary.Estimation.Matrix.c.val.before.1 <- round(Summary.Estimation.Matrix.c.val.before.1*100)
Summary.Estimation.Matrix.c.val.before.1


mse.naive.c.val.before.1 <-mean((Summary.Estimation.Matrix.c.val.before.1[,1] - c.val.real)^2)
mse.mos.c.val.before.1 <-mean((Summary.Estimation.Matrix.c.val.before.1[,2] - c.val.real)^2)
mse.mle.c.val.before.1 <-mean((Summary.Estimation.Matrix.c.val.before.1[,4] - c.val.real)^2)
mse.pimle.c.val.before.1 <-mean((Summary.Estimation.Matrix.c.val.before.1[,5] - c.val.real)^2)
mse.c.val.before.1 <- data.frame(mse.naive.c.val.before.1 = mse.naive.c.val.before.1,mse.mos.c.val.before.1 = mse.mos.c.val.before.1, mse.mle.c.val.before.1 = mse.mle.c.val.before.1, mse.pimle.c.val.before.1 = mse.pimle.c.val.before.1)
mse.c.val.before.1
cat("El MSE medio es:",mean(mse.naive.c.val.before.1,mse.mos.c.val.before.1,mse.mle.c.val.before.1,mse.pimle.c.val.before.1), "\n")


## 2.2. Escaños con el primer criterio


DHont.pimle.before.1 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before.1$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before.1$pimle.est[1:4]),total.number.seats)
names(DHont.pimle.before.1) <- rownames(Summary.Estimation.Matrix.c.val.before.1["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.before.1[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.before.1

DHont.mle.before.1 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before.1$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before.1$mle.est[1:4]),total.number.seats)
names(DHont.mle.before.1) <- rownames(Summary.Estimation.Matrix.c.val.before.1["mle.est"])[sort(Summary.Estimation.Matrix.c.val.before.1[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.before.1

DHont.MoS.before.1 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before.1$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before.1$MoS.est[1:4]),total.number.seats)
names(DHont.MoS.before.1) <- rownames(Summary.Estimation.Matrix.c.val.before.1["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.before.1[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.before.1

DHont.naive.before.1 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before.1$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before.1$naive.est[1:4]),total.number.seats)
names(DHont.naive.before.1) <- rownames(Summary.Estimation.Matrix.c.val.before.1["naive.est"])[sort(Summary.Estimation.Matrix.c.val.before.1[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.before.1

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.before.1 <- data.frame(naive.est = DHont.naive.before.1[political.groups],MoS.est = DHont.MoS.before.1[political.groups],mle.est = DHont.mle.before.1[political.groups], pimle.est = DHont.pimle.before.1[political.groups])
Summary.Estimation.Matrix.c.val.DHont.before.1


#Outliers: segundo criterio (votos en blanco)
  
  
  
if(median_abs_deviation(dat.before[,voting.inputs[5]])!=0){
  outliers.vote.blank.flag <- is_mad_outlier(dat.before[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
  outliers.vote.blank.flag
  
  threshold.net.size.vote.blank <- max(dat.before[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
  threshold.net.size.vote.blank
  
  input.dat.before.2 <- dat.before[which(dat.before[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
  dim(input.dat.before.2)
}else{
  input.dat.before.2<-dat.before
  dim(input.dat.before.2)
}




#Como se oberva la dimensión es igual a la que se obtiene en los datos brutos, por lo que no se puede aplicar el segundo criterio de filtrado


#Outliers: tercer criterio
  
###############################################################################


input.dat.before.3 <- dat.before[which(apply(dat.before[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] # Filtering criterion for a political group majoritary voting 
dim(input.dat.before.3)



#Correlation
  
  
unique(input.dat.before.3$Age)
correlacion_votos_edades.before.3 <- cor(input.dat.before.3[, voting.inputs], input.dat.before.3[, edades.inputs])
print(correlacion_votos_edades.before.3)



#NSUM estimation methods
#Preparation
  
  
# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.before.3 <- input.dat.before.3[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.before.3)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.before.3) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente





## Resultados con el tercer filtrado
## 3.1. Con el tercer criterio %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.before.3 <- colMeans(input.dat.nsum.before.3[,voting.inputs]/rowSums(input.dat.nsum.before.3[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.before.3



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.before.3 <- colSums(input.dat.nsum.before.3[,voting.inputs])/sum(input.dat.nsum.before.3[,voting.inputs])
c.val.RoS.results.before.3


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.before.3 <- Total*rowMeans(input.dat.nsum.before.3[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.before.3 <- Total*apply(input.dat.nsum.before.3[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.before.3,na.rm = TRUE))

c.val.MoS.results.before.3 <- prop.table(c.val.MoS.sizes.before.3)
c.val.MoS.results.before.3


#MLE method
###########

c.val.mle.est.before.3 <- networkscaleup::killworth(input.dat.nsum.before.3,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.before.3 <- prop.table(Total*apply(input.dat.nsum.before.3[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.before.3$degrees)))

c.val.mle.results.before.3


#Plug-in MLE method
###################

c.val.pimle.est.before.3 <- networkscaleup::killworth(input.dat.nsum.before.3,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.before.3 <- prop.table(Total*apply(input.dat.nsum.before.3[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.before.3$degrees>0]/c.val.pimle.est.before.3$degrees[c.val.pimle.est.before.3$degrees>0]))) 

c.val.pimle.results.before.3



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.before.3 <- data.frame(naive.est = c.val.naive.results.before.3,MoS.est = c.val.MoS.results.before.3,RoS.est=c.val.RoS.results.before.3,mle.est = c.val.mle.results.before.3, pimle.est = c.val.pimle.results.before.3)
Summary.Estimation.Matrix.c.val.before.3
t(round(100*Summary.Estimation.Matrix.c.val.before.3[c("Vox","PP","PSOE","Sumar","Blanco"),],1))





Summary.Estimation.Matrix.c.val.before.3 <- round(Summary.Estimation.Matrix.c.val.before.3*100)
Summary.Estimation.Matrix.c.val.before.3


mse.naive.c.val.before.3 <-mean((Summary.Estimation.Matrix.c.val.before.3[,1] - c.val.real)^2)
mse.mos.c.val.before.3 <-mean((Summary.Estimation.Matrix.c.val.before.3[,2] - c.val.real)^2)
mse.mle.c.val.before.3 <-mean((Summary.Estimation.Matrix.c.val.before.3[,4] - c.val.real)^2)
mse.pimle.c.val.before.3 <-mean((Summary.Estimation.Matrix.c.val.before.3[,5] - c.val.real)^2)
mse.c.val.before.3 <- data.frame(mse.naive.c.val.before.3 = mse.naive.c.val.before.3,mse.mos.c.val.before.3 = mse.mos.c.val.before.3, mse.mle.c.val.before.3 = mse.mle.c.val.before.3, mse.pimle.c.val.before.3 = mse.pimle.c.val.before.3)
mse.c.val.before.3
cat("El MSE medio es:",mean(mse.naive.c.val.before.3,mse.mos.c.val.before.3,mse.mle.c.val.before.3,mse.pimle.c.val.before.3), "\n")


## 3.2. Escaños con el tercer criterio


DHont.pimle.before.3 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before.3$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before.3$pimle.est[1:4]),total.number.seats)
names(DHont.pimle.before.3) <- rownames(Summary.Estimation.Matrix.c.val.before.3["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.before.3[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.before.3

DHont.mle.before.3 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before.3$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before.3$mle.est[1:4]),total.number.seats)
names(DHont.mle.before.3) <- rownames(Summary.Estimation.Matrix.c.val.before.3["mle.est"])[sort(Summary.Estimation.Matrix.c.val.before.3[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.before.3

DHont.MoS.before.3 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before.3$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before.3$MoS.est[1:4]),total.number.seats)
names(DHont.MoS.before.3) <- rownames(Summary.Estimation.Matrix.c.val.before.3["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.before.3[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.before.3

DHont.naive.before.3 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before.3$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before.3$naive.est[1:4]),total.number.seats)
names(DHont.naive.before.3) <- rownames(Summary.Estimation.Matrix.c.val.before.3["naive.est"])[sort(Summary.Estimation.Matrix.c.val.before.3[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.before.3

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.before.3 <- data.frame(naive.est = DHont.naive.before.3[political.groups],MoS.est = DHont.MoS.before.3[political.groups],mle.est = DHont.mle.before.3[political.groups], pimle.est = DHont.pimle.before.3[political.groups])
Summary.Estimation.Matrix.c.val.DHont.before.3


#Outliers con la combinación de los criterios
  

if(median_abs_deviation(dat.before[,voting.inputs[5]])!=0){
  outliers.vote.blank.flag <- is_mad_outlier(dat.before[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
  outliers.vote.blank.flag
  
  threshold.net.size.vote.blank <- max(dat.before[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
  threshold.net.size.vote.blank
  
  input.dat <- dat.before[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat.before[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.before[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95 & dat.before[,voting.inputs[5]]<=threshold.net.size.vote.blank),] #Customized filtering 
  dim(input.dat.before)
  
  #Filtering of outliers using the criteria separately#
  
  #input.dat <- dat[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)) ,] # Filtering by network size vote declaration 
  #dim(input.dat)
  #input.dat <- dat[which(apply(dat[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] # Filtering criterion for a political group majoritary voting 
  #dim(input.dat)
  input.dat.before.4 <- dat.before[which(dat.before[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
  dim(input.dat.before.4)
}else{
  input.dat.before.4 <- dat.before[which(apply(dat.before[,voting.inputs],1,sum) < quantile(apply(dat.before[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.before[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] #Customized filtering 
  dim(input.dat.4)
}


#Correlation
  
 
unique(input.dat.before.4$Age)
correlacion_votos_edades.before.4 <- cor(input.dat.before.4[, voting.inputs], input.dat.before.4[, edades.inputs])
print(correlacion_votos_edades.before.4)



#NSUM estimation methods
#Preparation
  
  
# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.before.4 <- input.dat.before.4[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.before.4)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.before.4) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados con la combinación de filtrados
## 4.1. Con la combinación de criterios %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.before.4 <- colMeans(input.dat.nsum.before.4[,voting.inputs]/rowSums(input.dat.nsum.before.4[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.before.4



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.before.4 <- colSums(input.dat.nsum.before.4[,voting.inputs])/sum(input.dat.nsum.before.4[,voting.inputs])
c.val.RoS.results.before.4


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.before.4 <- Total*rowMeans(input.dat.nsum.before.4[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.before.4 <- Total*apply(input.dat.nsum.before.4[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.before.4,na.rm = TRUE))

c.val.MoS.results.before.4 <- prop.table(c.val.MoS.sizes.before.4)
c.val.MoS.results.before.4


#MLE method
###########

c.val.mle.est.before.4 <- networkscaleup::killworth(input.dat.nsum.before.4,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.before.4 <- prop.table(Total*apply(input.dat.nsum.before.4[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.before.4$degrees)))

c.val.mle.results.before.4


#Plug-in MLE method
###################

c.val.pimle.est.before.4 <- networkscaleup::killworth(input.dat.nsum.before.4,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.before.4 <- prop.table(Total*apply(input.dat.nsum.before.4[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.before.4$degrees>0]/c.val.pimle.est.before.4$degrees[c.val.pimle.est.before.4$degrees>0]))) 

c.val.pimle.results.before.4



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.before.4 <- data.frame(naive.est = c.val.naive.results.before.4,MoS.est = c.val.MoS.results.before.4,RoS.est=c.val.RoS.results.before.4,mle.est = c.val.mle.results.before.4, pimle.est = c.val.pimle.results.before.4)
Summary.Estimation.Matrix.c.val.before.4
t(round(100*Summary.Estimation.Matrix.c.val.before.4[c("Vox","PP","PSOE","Sumar","Blanco"),],1))






Summary.Estimation.Matrix.c.val.before.4 <- round(Summary.Estimation.Matrix.c.val.before.4*100)
Summary.Estimation.Matrix.c.val.before.4


mse.naive.c.val.before.4 <-mean((Summary.Estimation.Matrix.c.val.before.4[,1] - c.val.real)^2)
mse.mos.c.val.before.4 <-mean((Summary.Estimation.Matrix.c.val.before.4[,2] - c.val.real)^2)
mse.mle.c.val.before.4 <-mean((Summary.Estimation.Matrix.c.val.before.4[,4] - c.val.real)^2)
mse.pimle.c.val.before.4 <-mean((Summary.Estimation.Matrix.c.val.before.4[,5] - c.val.real)^2)
mse.c.val.before.4 <- data.frame(mse.naive.c.val.before.4 = mse.naive.c.val.before.4,mse.mos.c.val.before.4 = mse.mos.c.val.before.4, mse.mle.c.val.before.4 = mse.mle.c.val.before.4, mse.pimle.c.val.before.4 = mse.pimle.c.val.before.4)
mse.c.val.before.4
cat("El MSE medio es:",mean(mse.naive.c.val.before.4,mse.mos.c.val.before.4,mse.mle.c.val.before.4,mse.pimle.c.val.before.4), "\n")



## 4.2. Escaños con la combinación de criterios


DHont.pimle.before.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before.4$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before.4$pimle.est[1:4]),total.number.seats)
names(DHont.pimle.before.4) <- rownames(Summary.Estimation.Matrix.c.val.before.4["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.before.4[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.before.4

DHont.mle.before.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before.4$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before.4$mle.est[1:4]),total.number.seats)
names(DHont.mle.before.4) <- rownames(Summary.Estimation.Matrix.c.val.before.4["mle.est"])[sort(Summary.Estimation.Matrix.c.val.before.4[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.before.4

DHont.MoS.before.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before.4$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before.4$MoS.est[1:4]),total.number.seats)
names(DHont.MoS.before.4) <- rownames(Summary.Estimation.Matrix.c.val.before.4["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.before.4[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.before.4

DHont.naive.before.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.before.4$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.before.4$naive.est[1:4]),total.number.seats)
names(DHont.naive.before.4) <- rownames(Summary.Estimation.Matrix.c.val.before.4["naive.est"])[sort(Summary.Estimation.Matrix.c.val.before.4[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.before.4

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.before.4 <- data.frame(naive.est = DHont.naive.before.4[political.groups],MoS.est = DHont.MoS.before.4[political.groups],mle.est = DHont.mle.before.4[political.groups], pimle.est = DHont.pimle.before.4[political.groups])
Summary.Estimation.Matrix.c.val.DHont.before.4


## Diagrama de Venn


threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 #0.9 --> 0.8 (20%)
quantile.vote <- quantile(apply(dat.before[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

index.1.before <- which(apply(dat.before[,voting.inputs],1,sum) >= quantile.vote)



# 2. Outliers

index.2.before <- which(apply(dat.before[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))>0.95)


# Diagrama de Venn

indices_comunes.before <- intersect(index.1.before, index.2.before)



venn.plot <- venn.diagram(
  x = list("Outliers 1" = index.1.before, "Outliers 2" = index.2.before),
  category.names = c("Outliers 1", "Outliers 2"),
  filename = NULL,
  width = 25,
  height = 25,
  main = "Outliers: Comunidad Valenciana antes del incio del campaña",
  cat.col = c("black", "black"),  # Colores de los círculos
  fill = c("#B9DDF1", "#2E5B88"), # Colores de las categorías
  cat.fontfamily = "sans"
)

grid.draw(venn.plot)



# Mostrar el número de coincidencias
cat("Número de coincidencias entre Forma 1 y Forma 2:", length(indices_comunes.before), "\n")



# After

## Resultados brutos




#NSUM estimation methods
  
#Preparation


# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.after <- dat.after
input.dat.nsum.after <- input.dat.after[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.after)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.after) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados brutos 
## 1.1 Con los datos brutos %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.after <- colMeans(input.dat.nsum.after[,voting.inputs]/rowSums(input.dat.nsum.after[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.after



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.after <- colSums(input.dat.nsum.after[,voting.inputs])/sum(input.dat.nsum.after[,voting.inputs])
c.val.RoS.results.after


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.after <- Total*rowMeans(input.dat.nsum.after[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.after <- Total*apply(input.dat.nsum.after[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.after,na.rm = TRUE))

c.val.MoS.results.after <- prop.table(c.val.MoS.sizes.after)
c.val.MoS.results.after


#MLE method
###########

c.val.mle.est.after <- networkscaleup::killworth(input.dat.nsum.after,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.after <- prop.table(Total*apply(input.dat.nsum.after[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.after$degrees)))

c.val.mle.results.after


#Plug-in MLE method
###################

c.val.pimle.est.after <- networkscaleup::killworth(input.dat.nsum.after,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.after <- prop.table(Total*apply(input.dat.nsum.after[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.after$degrees>0]/c.val.pimle.est.after$degrees[c.val.pimle.est.after$degrees>0]))) 

c.val.pimle.results.after



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.after <- data.frame(naive.est = c.val.naive.results.after,MoS.est = c.val.MoS.results.after,RoS.est=c.val.RoS.results.after,mle.est = c.val.mle.results.after, pimle.est = c.val.pimle.results.after)
Summary.Estimation.Matrix.c.val.after
t(round(100*Summary.Estimation.Matrix.c.val.after[c("Vox","PP","PSOE","Sumar","Blanco"),],1))

#MSE
  
  
  


Summary.Estimation.Matrix.c.val.after <- round(Summary.Estimation.Matrix.c.val.after*100)
Summary.Estimation.Matrix.c.val.after


mse.naive.c.val.after <-mean((Summary.Estimation.Matrix.c.val.after[,1] - c.val.real)^2)
mse.mos.c.val.after <-mean((Summary.Estimation.Matrix.c.val.after[,2] - c.val.real)^2)
mse.mle.c.val.after <-mean((Summary.Estimation.Matrix.c.val.after[,4] - c.val.real)^2)
mse.pimle.c.val.after <-mean((Summary.Estimation.Matrix.c.val.after[,5] - c.val.real)^2)
mse.c.val.after <- data.frame(mse.naive.c.val.after = mse.naive.c.val.after,mse.mos.c.val.after = mse.mos.c.val.after, mse.mle.c.val.after = mse.mle.c.val.after, mse.pimle.c.val.after = mse.pimle.c.val.after)
mse.c.val.after
cat("El MSE medio es:",mean(mse.naive.c.val.after,mse.mos.c.val.after,mse.mle.c.val.after,mse.pimle.c.val.after), "\n")



## 2.2. Escaños con el primer criterio


DHont.pimle.after <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after$pimle.est[1:4]),total.number.seats)
names(DHont.pimle.after) <- rownames(Summary.Estimation.Matrix.c.val.after["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.after[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.after

DHont.mle.after <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after$mle.est[1:4]),total.number.seats)
names(DHont.mle.after) <- rownames(Summary.Estimation.Matrix.c.val.after["mle.est"])[sort(Summary.Estimation.Matrix.c.val.after[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.after

DHont.MoS.after <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after$MoS.est[1:4]),total.number.seats)
names(DHont.MoS.after) <- rownames(Summary.Estimation.Matrix.c.val.after["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.after[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.after

DHont.naive.after <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after$naive.est[1:4]),total.number.seats)
names(DHont.naive.after) <- rownames(Summary.Estimation.Matrix.c.val.after["naive.est"])[sort(Summary.Estimation.Matrix.c.val.after[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.after

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.after <- data.frame(naive.est = DHont.naive.after[political.groups],MoS.est = DHont.MoS.after[political.groups],mle.est = DHont.mle.after[political.groups], pimle.est = DHont.pimle.after[political.groups])
Summary.Estimation.Matrix.c.val.DHont.after






#Outliers: primer criterio
  
  
###############################################################################

#Criterion for retaining the respondents with a network under a specific threshold
threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 #0.9 --> 0.8 (20%)
quantile(apply(dat.after[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

#Criterion for retaining the respondents who declare a network with blank voting under a specific threshold

#threshold.net.size.vote.blank <- 9 # Disable this line if you want to fix the threshold by hand


input.dat.after.1 <- dat[which(apply(dat.after[,voting.inputs],1,sum) < quantile(apply(dat.after[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)) ,] # Filtering by network size vote declaration 
dim(input.dat.after.1)

#Correlation


unique(input.dat.after.1$Age)
correlacion_votos_edades.after.1 <- cor(input.dat.after.1[, voting.inputs], input.dat.after.1[, edades.inputs])
print(correlacion_votos_edades.after.1)





#NSUM estimation methods
#Preparation
  
  
# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.after.1 <- input.dat.after.1[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.after.1)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.after.1) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados con el primer filtrado
## 2.1. Con el primer criterio %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.after.1 <- colMeans(input.dat.nsum.after.1[,voting.inputs]/rowSums(input.dat.nsum.after.1[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.after.1



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.after.1 <- colSums(input.dat.nsum.after.1[,voting.inputs])/sum(input.dat.nsum.after.1[,voting.inputs])
c.val.RoS.results.after.1


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.after.1 <- Total*rowMeans(input.dat.nsum.after.1[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.after.1 <- Total*apply(input.dat.nsum.after.1[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.after.1,na.rm = TRUE))

c.val.MoS.results.after.1 <- prop.table(c.val.MoS.sizes.after.1)
c.val.MoS.results.after.1


#MLE method
###########

c.val.mle.est.after.1 <- networkscaleup::killworth(input.dat.nsum.after.1,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.after.1 <- prop.table(Total*apply(input.dat.nsum.after.1[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.after.1$degrees)))

c.val.mle.results.after.1


#Plug-in MLE method
###################

c.val.pimle.est.after.1 <- networkscaleup::killworth(input.dat.nsum.after.1,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.after.1 <- prop.table(Total*apply(input.dat.nsum.after.1[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.after.1$degrees>0]/c.val.pimle.est.after.1$degrees[c.val.pimle.est.after.1$degrees>0]))) 

c.val.pimle.results.after.1



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.after.1 <- data.frame(naive.est = c.val.naive.results.after.1,MoS.est = c.val.MoS.results.after.1,RoS.est=c.val.RoS.results.after.1,mle.est = c.val.mle.results.after.1, pimle.est = c.val.pimle.results.after.1)
Summary.Estimation.Matrix.c.val.after.1
t(round(100*Summary.Estimation.Matrix.c.val.after.1[c("Vox","PP","PSOE","Sumar","Blanco"),],1))




Summary.Estimation.Matrix.c.val.after.1 <- round(Summary.Estimation.Matrix.c.val.after.1*100)
Summary.Estimation.Matrix.c.val.after.1


mse.naive.c.val.after.1 <-mean((Summary.Estimation.Matrix.c.val.after.1[,1] - c.val.real)^2)
mse.mos.c.val.after.1 <-mean((Summary.Estimation.Matrix.c.val.after.1[,2] - c.val.real)^2)
mse.mle.c.val.after.1 <-mean((Summary.Estimation.Matrix.c.val.after.1[,4] - c.val.real)^2)
mse.pimle.c.val.after.1 <-mean((Summary.Estimation.Matrix.c.val.after.1[,5] - c.val.real)^2)
mse.c.val.after.1 <- data.frame(mse.naive.c.val.after.1 = mse.naive.c.val.after.1,mse.mos.c.val.after.1 = mse.mos.c.val.after.1, mse.mle.c.val.after.1 = mse.mle.c.val.after.1, mse.pimle.c.val.after.1 = mse.pimle.c.val.after.1)
mse.c.val.after.1
cat("El MSE medio es:",mean(mse.naive.c.val.after.1,mse.mos.c.val.after.1,mse.mle.c.val.after.1,mse.pimle.c.val.after.1), "\n")



## 2.2. Escaños con el primer criterio


DHont.pimle.after.1 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.1$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.1$pimle.est[1:4]),total.number.seats)
names(DHont.pimle.after.1) <- rownames(Summary.Estimation.Matrix.c.val.after.1["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.after.1[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.after.1

DHont.mle.after.1 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.1$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.1$mle.est[1:4]),total.number.seats)
names(DHont.mle.after.1) <- rownames(Summary.Estimation.Matrix.c.val.after.1["mle.est"])[sort(Summary.Estimation.Matrix.c.val.after.1[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.after.1

DHont.MoS.after.1 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.1$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.1$MoS.est[1:4]),total.number.seats)
names(DHont.MoS.after.1) <- rownames(Summary.Estimation.Matrix.c.val.after.1["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.after.1[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.after.1

DHont.naive.after.1 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.1$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.1$naive.est[1:4]),total.number.seats)
names(DHont.naive.after.1) <- rownames(Summary.Estimation.Matrix.c.val.after.1["naive.est"])[sort(Summary.Estimation.Matrix.c.val.after.1[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.after.1

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.after.1 <- data.frame(naive.est = DHont.naive.after.1[political.groups],MoS.est = DHont.MoS.after.1[political.groups],mle.est = DHont.mle.after.1[political.groups], pimle.est = DHont.pimle.after.1[political.groups])
Summary.Estimation.Matrix.c.val.DHont.after.1


#Outliers: segundo criterio (votos en blanco)
  


if(median_abs_deviation(dat.after[,voting.inputs[5]])!=0){
  outliers.vote.blank.flag <- is_mad_outlier(dat.after[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
  outliers.vote.blank.flag
  
  threshold.net.size.vote.blank <- max(dat.after[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
  threshold.net.size.vote.blank
  
  input.dat.after.2 <- dat.after[which(dat.after[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
  dim(input.dat.after.2)
}else{
  input.dat.after.2<-dat.after
  dim(input.dat.after.2)
}




#Aquí sí que se puede aplicar el segundo criterio

#Correlation
  

unique(input.dat.after.2$Age)
correlacion_votos_edades.after.2 <- cor(input.dat.after.2[, voting.inputs], input.dat.after.2[, edades.inputs])
print(correlacion_votos_edades.after.2)



#NSUM estimation methods
#Preparation
  

# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.after.2 <- input.dat.after.2[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.after.2)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.after.2) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente





## Resultados con el segundo filtrado
## 4.1. Con el segundo criterio %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.after.2 <- colMeans(input.dat.nsum.after.2[,voting.inputs]/rowSums(input.dat.nsum.after.2[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.after.2



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.after.2 <- colSums(input.dat.nsum.after.2[,voting.inputs])/sum(input.dat.nsum.after.2[,voting.inputs])
c.val.RoS.results.after.2


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.after.2 <- Total*rowMeans(input.dat.nsum.after.2[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.after.2 <- Total*apply(input.dat.nsum.after.2[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.after.2,na.rm = TRUE))

c.val.MoS.results.after.2 <- prop.table(c.val.MoS.sizes.after.2)
c.val.MoS.results.after.2


#MLE method
###########

c.val.mle.est.after.2 <- networkscaleup::killworth(input.dat.nsum.after.2,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.after.2 <- prop.table(Total*apply(input.dat.nsum.after.2[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.after.2$degrees)))

c.val.mle.results.after.2


#Plug-in MLE method
###################

c.val.pimle.est.after.2 <- networkscaleup::killworth(input.dat.nsum.after.2,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.after.2 <- prop.table(Total*apply(input.dat.nsum.after.2[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.after.2$degrees>0]/c.val.pimle.est.after.2$degrees[c.val.pimle.est.after.2$degrees>0]))) 

c.val.pimle.results.after.2



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.after.2 <- data.frame(naive.est = c.val.naive.results.after.2,MoS.est = c.val.MoS.results.after.2,RoS.est=c.val.RoS.results.after.2,mle.est = c.val.mle.results.after.2, pimle.est = c.val.pimle.results.after.2)
Summary.Estimation.Matrix.c.val.after.2
t(round(100*Summary.Estimation.Matrix.c.val.after.2[c("Vox","PP","PSOE","Sumar","Blanco"),],1))





Summary.Estimation.Matrix.c.val.after.2 <- round(Summary.Estimation.Matrix.c.val.after.2*100)
Summary.Estimation.Matrix.c.val.after.2


mse.naive.c.val.after.2 <-mean((Summary.Estimation.Matrix.c.val.after.2[,1] - c.val.real)^2)
mse.mos.c.val.after.2 <-mean((Summary.Estimation.Matrix.c.val.after.2[,2] - c.val.real)^2)
mse.mle.c.val.after.2 <-mean((Summary.Estimation.Matrix.c.val.after.2[,4] - c.val.real)^2)
mse.pimle.c.val.after.2 <-mean((Summary.Estimation.Matrix.c.val.after.2[,5] - c.val.real)^2)
mse.c.val.after.2 <- data.frame(mse.naive.c.val.after.2 = mse.naive.c.val.after.2,mse.mos.c.val.after.2 = mse.mos.c.val.after.2, mse.mle.c.val.after.2 = mse.mle.c.val.after.2, mse.pimle.c.val.after.2 = mse.pimle.c.val.after.2)
mse.c.val.after.2
cat("El MSE medio es:",mean(mse.naive.c.val.after.2,mse.mos.c.val.after.2,mse.mle.c.val.after.2,mse.pimle.c.val.after.2), "\n")



## 4.2. Escaños con el segundo criterio


DHont.pimle.after.2 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.2$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.2$pimle.est[1:4]),total.number.seats)
names(DHont.pimle.after.2) <- rownames(Summary.Estimation.Matrix.c.val.after.2["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.after.2[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.after.2

DHont.mle.after.2 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.2$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.2$mle.est[1:4]),total.number.seats)
names(DHont.mle.after.2) <- rownames(Summary.Estimation.Matrix.c.val.after.2["mle.est"])[sort(Summary.Estimation.Matrix.c.val.after.2[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.after.2

DHont.MoS.after.2 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.2$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.2$MoS.est[1:4]),total.number.seats)
names(DHont.MoS.after.2) <- rownames(Summary.Estimation.Matrix.c.val.after.2["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.after.2[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.after.2

DHont.naive.after.2 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.2$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.2$naive.est[1:4]),total.number.seats)
names(DHont.naive.after.2) <- rownames(Summary.Estimation.Matrix.c.val.after.2["naive.est"])[sort(Summary.Estimation.Matrix.c.val.after.2[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.after.2

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.after.2 <- data.frame(naive.est = DHont.naive.after.2[political.groups],MoS.est = DHont.MoS.after.2[political.groups],mle.est = DHont.mle.after.2[political.groups], pimle.est = DHont.pimle.after.2[political.groups])
Summary.Estimation.Matrix.c.val.DHont.after.2








#Outliers: tercer criterio
  




input.dat.after.3 <- dat.after[which(apply(dat.after[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] # Filtering criterion for a political group majoritary voting 
dim(input.dat.after.3)


#Correlation
  
  
unique(input.dat.after.3$Age)
correlacion_votos_edades.after.3 <- cor(input.dat.after.3[, voting.inputs], input.dat.after.3[, edades.inputs])
print(correlacion_votos_edades.after.3)


#NSUM estimation methods
#Preparation
  

# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.after.3 <- input.dat.after.3[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.after.3)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.after.3) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente





## Resultados con el tercer filtrado
## 3.1. Con el tercer criterio %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.after.3 <- colMeans(input.dat.nsum.after.3[,voting.inputs]/rowSums(input.dat.nsum.after.3[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.after.3



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.after.3 <- colSums(input.dat.nsum.after.3[,voting.inputs])/sum(input.dat.nsum.after.3[,voting.inputs])
c.val.RoS.results.after.3


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.after.3 <- Total*rowMeans(input.dat.nsum.after.3[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.after.3 <- Total*apply(input.dat.nsum.after.3[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.after.3,na.rm = TRUE))

c.val.MoS.results.after.3 <- prop.table(c.val.MoS.sizes.after.3)
c.val.MoS.results.after.3


#MLE method
###########

c.val.mle.est.after.3 <- networkscaleup::killworth(input.dat.nsum.after.3,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.after.3 <- prop.table(Total*apply(input.dat.nsum.after.3[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.after.3$degrees)))

c.val.mle.results.after.3


#Plug-in MLE method
###################

c.val.pimle.est.after.3 <- networkscaleup::killworth(input.dat.nsum.after.3,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.after.3 <- prop.table(Total*apply(input.dat.nsum.after.3[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.after.3$degrees>0]/c.val.pimle.est.after.3$degrees[c.val.pimle.est.after.3$degrees>0]))) 

c.val.pimle.results.after.3



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.after.3 <- data.frame(naive.est = c.val.naive.results.after.3,MoS.est = c.val.MoS.results.after.3,RoS.est=c.val.RoS.results.after.3,mle.est = c.val.mle.results.after.3, pimle.est = c.val.pimle.results.after.3)
Summary.Estimation.Matrix.c.val.after.3
t(round(100*Summary.Estimation.Matrix.c.val.after.3[c("Vox","PP","PSOE","Sumar","Blanco"),],1))





Summary.Estimation.Matrix.c.val.after.3 <- round(Summary.Estimation.Matrix.c.val.after.3*100)
Summary.Estimation.Matrix.c.val.after.3


mse.naive.c.val.after.3 <-mean((Summary.Estimation.Matrix.c.val.after.3[,1] - c.val.real)^2)
mse.mos.c.val.after.3 <-mean((Summary.Estimation.Matrix.c.val.after.3[,2] - c.val.real)^2)
mse.mle.c.val.after.3 <-mean((Summary.Estimation.Matrix.c.val.after.3[,4] - c.val.real)^2)
mse.pimle.c.val.after.3 <-mean((Summary.Estimation.Matrix.c.val.after.3[,5] - c.val.real)^2)
mse.c.val.after.3 <- data.frame(mse.naive.c.val.after.3 = mse.naive.c.val.after.3,mse.mos.c.val.after.3 = mse.mos.c.val.after.3, mse.mle.c.val.after.3 = mse.mle.c.val.after.3, mse.pimle.c.val.after.3 = mse.pimle.c.val.after.3)
mse.c.val.after.3
cat("El MSE medio es:",mean(mse.naive.c.val.after.3,mse.mos.c.val.after.3,mse.mle.c.val.after.3,mse.pimle.c.val.after.3), "\n")



## 3.2. Escaños con el tercer criterio


DHont.pimle.after.3 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.3$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.3$pimle.est[1:4]),total.number.seats)
names(DHont.pimle.after.3) <- rownames(Summary.Estimation.Matrix.c.val.after.3["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.after.3[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.after.3

DHont.mle.after.3 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.3$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.3$mle.est[1:4]),total.number.seats)
names(DHont.mle.after.3) <- rownames(Summary.Estimation.Matrix.c.val.after.3["mle.est"])[sort(Summary.Estimation.Matrix.c.val.after.3[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.after.3

DHont.MoS.after.3 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.3$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.3$MoS.est[1:4]),total.number.seats)
names(DHont.MoS.after.3) <- rownames(Summary.Estimation.Matrix.c.val.after.3["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.after.3[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.after.3

DHont.naive.after.3 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.3$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.3$naive.est[1:4]),total.number.seats)
names(DHont.naive.after.3) <- rownames(Summary.Estimation.Matrix.c.val.after.3["naive.est"])[sort(Summary.Estimation.Matrix.c.val.after.3[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.after.3

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.after.3 <- data.frame(naive.est = DHont.naive.after.3[political.groups],MoS.est = DHont.MoS.after.3[political.groups],mle.est = DHont.mle.after.3[political.groups], pimle.est = DHont.pimle.after.3[political.groups])
Summary.Estimation.Matrix.c.val.DHont.after.3

#Outliers con la combinación de los criterios
  
  
if(median_abs_deviation(dat.after[,voting.inputs[5]])!=0){
  outliers.vote.blank.flag <- is_mad_outlier(dat.after[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
  outliers.vote.blank.flag
  
  threshold.net.size.vote.blank <- max(dat.after[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
  threshold.net.size.vote.blank
  
  input.dat <- dat.after[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat.after[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.after[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95 & dat.after[,voting.inputs[5]]<=threshold.net.size.vote.blank),] #Customized filtering 
  dim(input.dat.after)
  
  #Filtering of outliers using the criteria separately#
  
  #input.dat <- dat[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)) ,] # Filtering by network size vote declaration 
  #dim(input.dat)
  #input.dat <- dat[which(apply(dat[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] # Filtering criterion for a political group majoritary voting 
  #dim(input.dat)
  input.dat.after.4 <- dat.after[which(dat.after[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
  dim(input.dat.after.4)
}else{
  input.dat.after.4 <- dat.after[which(apply(dat.after[,voting.inputs],1,sum) < quantile(apply(dat.after[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.after[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] #Customized filtering 
  dim(input.dat.4)
}

#Correlation
  
  
unique(input.dat.after.4$Age)
correlacion_votos_edades.after.4 <- cor(input.dat.after.4[, voting.inputs], input.dat.after.4[, edades.inputs])
print(correlacion_votos_edades.after.4)



NSUM estimation methods
#Preparation
  
  
# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.after.4 <- input.dat.after.4[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.after.4)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.after.4) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados con la combinación de filtrados
## 4.1. Con la combinación de criterios %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.after.4 <- colMeans(input.dat.nsum.after.4[,voting.inputs]/rowSums(input.dat.nsum.after.4[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.after.4



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.after.4 <- colSums(input.dat.nsum.after.4[,voting.inputs])/sum(input.dat.nsum.after.4[,voting.inputs])
c.val.RoS.results.after.4


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.after.4 <- Total*rowMeans(input.dat.nsum.after.4[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.after.4 <- Total*apply(input.dat.nsum.after.4[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.after.4,na.rm = TRUE))

c.val.MoS.results.after.4 <- prop.table(c.val.MoS.sizes.after.4)
c.val.MoS.results.after.4


#MLE method
###########

c.val.mle.est.after.4 <- networkscaleup::killworth(input.dat.nsum.after.4,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.after.4 <- prop.table(Total*apply(input.dat.nsum.after.4[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.after.4$degrees)))

c.val.mle.results.after.4


#Plug-in MLE method
###################

c.val.pimle.est.after.4 <- networkscaleup::killworth(input.dat.nsum.after.4,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.after.4 <- prop.table(Total*apply(input.dat.nsum.after.4[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.after.4$degrees>0]/c.val.pimle.est.after.4$degrees[c.val.pimle.est.after.4$degrees>0]))) 

c.val.pimle.results.after.4



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.after.4 <- data.frame(naive.est = c.val.naive.results.after.4,MoS.est = c.val.MoS.results.after.4,RoS.est=c.val.RoS.results.after.4,mle.est = c.val.mle.results.after.4, pimle.est = c.val.pimle.results.after.4)
Summary.Estimation.Matrix.c.val.after.4
t(round(100*Summary.Estimation.Matrix.c.val.after.4[c("Vox","PP","PSOE","Sumar","Blanco"),],1))




Summary.Estimation.Matrix.c.val.after.4 <- round(Summary.Estimation.Matrix.c.val.after.4*100)
Summary.Estimation.Matrix.c.val.after.4


mse.naive.c.val.after.4 <-mean((Summary.Estimation.Matrix.c.val.after.4[,1] - c.val.real)^2)
mse.mos.c.val.after.4 <-mean((Summary.Estimation.Matrix.c.val.after.4[,2] - c.val.real)^2)
mse.mle.c.val.after.4 <-mean((Summary.Estimation.Matrix.c.val.after.4[,4] - c.val.real)^2)
mse.pimle.c.val.after.4 <-mean((Summary.Estimation.Matrix.c.val.after.4[,5] - c.val.real)^2)
mse.c.val.after.4 <- data.frame(mse.naive.c.val.after.4 = mse.naive.c.val.after.4,mse.mos.c.val.after.4 = mse.mos.c.val.after.4, mse.mle.c.val.after.4 = mse.mle.c.val.after.4, mse.pimle.c.val.after.4 = mse.pimle.c.val.after.4)
mse.c.val.after.4
cat("El MSE medio es:",mean(mse.naive.c.val.after.4,mse.mos.c.val.after.4,mse.mle.c.val.after.4,mse.pimle.c.val.after.4), "\n")



## 4.2. Escaños con la combinación de criterios


DHont.pimle.after.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.4$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.4$pimle.est[1:4]),total.number.seats)
names(DHont.pimle.after.4) <- rownames(Summary.Estimation.Matrix.c.val.after.4["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.after.4[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.after.4

DHont.mle.after.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.4$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.4$mle.est[1:4]),total.number.seats)
names(DHont.mle.after.4) <- rownames(Summary.Estimation.Matrix.c.val.after.4["mle.est"])[sort(Summary.Estimation.Matrix.c.val.after.4[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.after.4

DHont.MoS.after.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.4$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.4$MoS.est[1:4]),total.number.seats)
names(DHont.MoS.after.4) <- rownames(Summary.Estimation.Matrix.c.val.after.4["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.after.4[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.after.4

DHont.naive.after.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.after.4$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.after.4$naive.est[1:4]),total.number.seats)
names(DHont.naive.after.4) <- rownames(Summary.Estimation.Matrix.c.val.after.4["naive.est"])[sort(Summary.Estimation.Matrix.c.val.after.4[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.after.4

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.after.4 <- data.frame(naive.est = DHont.naive.after.4[political.groups],MoS.est = DHont.MoS.after.4[political.groups],mle.est = DHont.mle.after.4[political.groups], pimle.est = DHont.pimle.after.4[political.groups])
Summary.Estimation.Matrix.c.val.DHont.after.4


threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 #0.9 --> 0.8 (20%)
quantile.vote <- quantile(apply(dat.after[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

index.1.after <- which(apply(dat.after[,voting.inputs],1,sum) >= quantile.vote)

outliers.vote.blank.flag <- is_mad_outlier(dat.after[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
outliers.vote.blank.flag

threshold.net.size.vote.blank <- max(dat.after[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
threshold.net.size.vote.blank

input.dat.after <- dat.after[which(dat.after[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
dim(input.dat.after)

index.3.after <- which(dat.after[,voting.inputs[5]]>threshold.net.size.vote.blank)

# 2. Outliers

index.2.after <- which(apply(dat.after[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))>0.95)



indices_comunes.after <- intersect(index.1.after, index.2.after)
indices_comunes.after <- intersect(indices_comunes.after,index.3.after)
library(VennDiagram)

venn.plot <- venn.diagram(
  x = list("Outliers 1" = index.1.after, "Outliers 3" = index.2.after, "Outliers 2" = index.3.after),
  category.names = c("Outliers 1", "Outliers 2", "Outliers 3"),
  filename = NULL,
  width = 16,
  height = 6,
  main = "Outliers: Comunidad Valenciana después del inicio de campaña",
  cat.col = c("black", "black", "black"),  # Colores de los círculos
  fill = c("#B9DDF1", "#2E5B88", "#6495BF"), # Colores de las categorías
  cat.fontfamily = "sans"
)
grid.draw(venn.plot)

# Mostrar el número de coincidencias
cat("Número de coincidencias entre Forma 1 y Forma 2:", length(indices_comunes.after), "\n")





# Provincias


dat <- df[,c(socio.demo.inputs,voting.inputs,control.inputs,prov.inputs)] # prov.inputs
colnames(dat)
dim(dat)
dat <- na.omit(dat) #Only rows with complete cases are considered
dim(dat)
alicante.n <- length(dat$Provincia[dat$Provincia == 3])
alicante.n
castellon.n <- length(dat$Provincia[dat$Provincia == 12])
castellon.n
valencia.n <- length(dat$Provincia[dat$Provincia == 46])
valencia.n
prov.percentage <- round(c(alicante.n/195, castellon.n/195, valencia.n/195)*100,1)
prov.percentage
sum(prov.percentage)
cat("el tamaño de Alicante es de", alicante.n, "lo cual supone un", round(alicante.n/195*100,1), "% de la muestra", "\n")
cat("el tamaño de Castellón es de", castellon.n, "lo cual supone un", round(castellon.n/195*100,1), "% de la muestra", "\n")
cat("el tamaño de Valencia es de", valencia.n, "lo cual supone un", round(valencia.n/195*100,1), "% de la muestra", "\n")


alicante.INE <-  1260967 
castellon.INE <-  419899 
valencia.INE <-  1928923 
n.INE <- 3609789 
prov.percentage.INE <- round(c(alicante.INE/n.INE, castellon.INE/n.INE,valencia.INE/n.INE)*100,3)
prov.percentage.INE
sum(prov.percentage.INE)

dat.alicante <- dat[dat$Provincia == 3,]
dim(dat.alicante)
dat.castellon <- dat[dat$Provincia == 12,]
dim(dat.castellon)
dat.valencia <- dat[dat$Provincia == 46,]
dim(dat.valencia)



# Alicante


input.dat.alicante<-dat.alicante
dim(input.dat.alicante)





#Correlation

unique(input.dat.alicante$Age)
correlacion_votos_edades <- cor(input.dat.alicante[, voting.inputs], input.dat.alicante[, edades.inputs])
print(correlacion_votos_edades)



alicante.subpopulation.sizes <- c( 264692, 448959, 547316,0,0,0)  # solo 3 primeros ,360765,17430,3525000



Total.alicante <-  1260967  # Data source https://www.ine.es/jaxi/Datos.htm?tpx=48409 (dato de la suma de las edades)
Total.alicante

alicante.subpopulation.sizes # Totals
round(alicante.subpopulation.sizes/Total,4) # Proportions
names(alicante.subpopulation.sizes) <- control.inputs



#NSUM estimation methods
  
#Preparation


# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.alicante <- input.dat.alicante[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.alicante)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.alicante) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados brutos 
## 1.1 Con los datos brutos %


#Naive approach
# estimamos por NAIVE
###############

alicante.naive.results <- colMeans(input.dat.nsum.alicante[,voting.inputs]/rowSums(input.dat.nsum.alicante[,voting.inputs]),na.rm = TRUE)
alicante.naive.results



#Ratio of sums approach
# estimamos por ratio of sums

#######################

alicante.RoS.results <- colSums(input.dat.nsum.alicante[,voting.inputs])/sum(input.dat.nsum.alicante[,voting.inputs])
alicante.RoS.results


#MoS approach (Hebecker 2015)
#############################

alicante.MoS.degrees <- Total.alicante*rowMeans(input.dat.nsum.alicante[,control.inputs[1:3]]/alicante.subpopulation.sizes[1:3])
alicante.MoS.sizes <- Total.alicante*apply(input.dat.nsum.alicante[,voting.inputs],2,function (x) mean(x/alicante.MoS.degrees,na.rm = TRUE))

alicante.MoS.results <- prop.table(alicante.MoS.sizes)
alicante.MoS.results


#MLE method
###########

alicante.mle.est <- networkscaleup::killworth(input.dat.nsum.alicante,known_sizes=alicante.subpopulation.sizes,known_ind=control.ind,N=Total.alicante,model="MLE")
alicante.mle.results <- prop.table(Total.alicante*apply(input.dat.nsum.alicante[,voting.inputs],2,function (x) mean(x)/mean(alicante.mle.est$degrees)))

alicante.mle.results


#Plug-in MLE method
###################

alicante.pimle.est <- networkscaleup::killworth(input.dat.nsum.alicante,known_sizes=alicante.subpopulation.sizes,known_ind=control.ind,N=Total.alicante,model="PIMLE")
alicante.pimle.results <- prop.table(Total.alicante*apply(input.dat.nsum.alicante[,voting.inputs],2,function (x) mean(x[alicante.pimle.est$degrees>0]/alicante.pimle.est$degrees[alicante.pimle.est$degrees>0]))) 

alicante.pimle.results



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.alicante <- data.frame(naive.est = alicante.naive.results,MoS.est = alicante.MoS.results,RoS.est=alicante.RoS.results,mle.est = alicante.mle.results, pimle.est = alicante.pimle.results)
Summary.Estimation.Matrix.alicante
t(round(100*Summary.Estimation.Matrix.alicante[c("Vox","PP","PSOE","Sumar","Blanco"),],1))

#MSE
  
  
alicante.real <- c(36.77, 31.99, 16.26, 12.84, 0.73)

Summary.Estimation.Matrix.alicante <- round(Summary.Estimation.Matrix.alicante*100)
Summary.Estimation.Matrix.alicante


mse.naive.alicante <-mean((Summary.Estimation.Matrix.alicante[,1] - alicante.real)^2)
mse.mos.alicante <-mean((Summary.Estimation.Matrix.alicante[,2] - alicante.real)^2)
mse.mle.alicante <-mean((Summary.Estimation.Matrix.alicante[,4] - alicante.real)^2)
mse.pimle.alicante <-mean((Summary.Estimation.Matrix.alicante[,5] - alicante.real)^2)
mse.alicante <- data.frame(mse.naive.alicante = mse.naive.alicante,mse.mos.alicante = mse.mos.alicante, mse.mle.alicante = mse.mle.alicante, mse.pimle.alicante = mse.pimle.alicante)
mse.alicante
cat("El MSE medio es:",mean(mse.naive.alicante,mse.mos.alicante,mse.mle.alicante,mse.pimle.alicante), "\n")




## 1.2. Escaños con datos brutos 



#Estimation of number of seats using D'Hont method (need to upload the function compute_dhont_seats)

total.number.seats.alicante <- 12

DHont.pimle.alicante <- compute_dhont_seats(Summary.Estimation.Matrix.alicante$pimle.est[1:4]/sum(Summary.Estimation.Matrix.alicante$pimle.est[1:4]),total.number.seats.alicante)
names(DHont.pimle.alicante) <- rownames(Summary.Estimation.Matrix.alicante["pimle.est"])[sort(Summary.Estimation.Matrix.alicante[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.alicante

DHont.mle.alicante <- compute_dhont_seats(Summary.Estimation.Matrix.alicante$mle.est[1:4]/sum(Summary.Estimation.Matrix.alicante$mle.est[1:4]),total.number.seats.alicante)
names(DHont.mle.alicante) <- rownames(Summary.Estimation.Matrix.alicante["mle.est"])[sort(Summary.Estimation.Matrix.alicante[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.mle.alicante

DHont.MoS.alicante <- compute_dhont_seats(Summary.Estimation.Matrix.alicante$MoS.est[1:4]/sum(Summary.Estimation.Matrix.alicante$MoS.est[1:4]),total.number.seats.alicante)
names(DHont.MoS.alicante) <- rownames(Summary.Estimation.Matrix.alicante["MoS.est"])[sort(Summary.Estimation.Matrix.alicante[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.alicante

DHont.naive.alicante <- compute_dhont_seats(Summary.Estimation.Matrix.alicante$naive.est[1:4]/sum(Summary.Estimation.Matrix.alicante$naive.est[1:4]),total.number.seats.alicante)
names(DHont.naive.alicante) <- rownames(Summary.Estimation.Matrix.alicante["naive.est"])[sort(Summary.Estimation.Matrix.alicante[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.alicante

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.alicante.DHont <- data.frame(naive.est = DHont.naive.alicante[political.groups],MoS.est = DHont.MoS.alicante[political.groups],mle.est = DHont.mle.alicante[political.groups], pimle.est = DHont.pimle.alicante[political.groups])
t(Summary.Estimation.Matrix.alicante.DHont)





#Outliers: primer criterio
  
  


#Criterion for retaining the respondents with a network under a specific threshold
threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 #0.9 --> 0.8 (20%)
quantile(apply(dat.alicante[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

#Criterion for retaining the respondents who declare a network with blank voting under a specific threshold

#threshold.net.size.vote.blank <- 9 # Disable this line if you want to fix the threshold by hand


input.dat.1.alicante <- dat.alicante[which(apply(dat.alicante[,voting.inputs],1,sum) < quantile(apply(dat.alicante[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)) ,] # Filtering by network size vote declaration 
dim(input.dat.1.alicante)



#Correlation
  
unique(input.dat.1.alicante$Age)
correlacion_votos_edades.1.alicante <- cor(input.dat.1.alicante[, voting.inputs], input.dat.1.alicante[, edades.inputs])
print(correlacion_votos_edades.1.alicante)





#NSUM estimation methods
#Preparation
  

# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.1.alicante <- input.dat.1.alicante[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.1.alicante)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.1.alicante) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados con el primer filtrado
## 2.1. Con el primer criterio %


#Naive approach
# estimamos por NAIVE
###############

alicante.naive.results.1 <- colMeans(input.dat.nsum.1.alicante[,voting.inputs]/rowSums(input.dat.nsum.1.alicante[,voting.inputs]),na.rm = TRUE)
alicante.naive.results.1



#Ratio of sums approach
# estimamos por ratio of sums

#######################

alicante.RoS.results.1 <- colSums(input.dat.nsum.1.alicante[,voting.inputs])/sum(input.dat.nsum.1.alicante[,voting.inputs])
alicante.RoS.results.1


#MoS approach (Hebecker 2015)
#############################

alicante.MoS.degrees.1 <- Total.alicante*rowMeans(input.dat.nsum.1.alicante[,control.inputs[1:3]]/alicante.subpopulation.sizes[1:3])
alicante.MoS.sizes.1 <- Total.alicante*apply(input.dat.nsum.1.alicante[,voting.inputs],2,function (x) mean(x/alicante.MoS.degrees.1,na.rm = TRUE))

alicante.MoS.results.1 <- prop.table(alicante.MoS.sizes.1)
alicante.MoS.results.1


#MLE method
###########

alicante.mle.est.1 <- networkscaleup::killworth(input.dat.nsum.1.alicante,known_sizes=alicante.subpopulation.sizes,known_ind=control.ind,N=Total.alicante,model="MLE")
alicante.mle.results.1<- prop.table(Total.alicante*apply(input.dat.nsum.1.alicante[,voting.inputs],2,function (x) mean(x)/mean(alicante.mle.est.1$degrees)))

alicante.mle.results.1


#Plug-in MLE method
###################

alicante.pimle.est.1 <- networkscaleup::killworth(input.dat.nsum.1.alicante,known_sizes=alicante.subpopulation.sizes,known_ind=control.ind,N=Total.alicante,model="PIMLE")
alicante.pimle.results.1 <- prop.table(Total.alicante*apply(input.dat.nsum.1.alicante[,voting.inputs],2,function (x) mean(x[alicante.pimle.est.1$degrees>0]/alicante.pimle.est.1$degrees[alicante.pimle.est.1$degrees>0]))) 

alicante.pimle.results.1



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.alicante.1 <- data.frame(naive.est = alicante.naive.results.1,MoS.est = alicante.MoS.results.1,RoS.est=alicante.RoS.results.1,mle.est = alicante.mle.results.1, pimle.est = alicante.pimle.results.1)
Summary.Estimation.Matrix.alicante.1
t(round(100*Summary.Estimation.Matrix.alicante.1[c("Vox","PP","PSOE","Sumar","Blanco"),],1))





Summary.Estimation.Matrix.alicante.1 <- round(Summary.Estimation.Matrix.alicante.1*100)
Summary.Estimation.Matrix.alicante.1


mse.naive.alicante.1 <-mean((Summary.Estimation.Matrix.alicante.1[,1] - alicante.real)^2)
mse.mos.alicante.1 <-mean((Summary.Estimation.Matrix.alicante.1[,2] - alicante.real)^2)
mse.mle.alicante.1 <-mean((Summary.Estimation.Matrix.alicante.1[,4] - alicante.real)^2)
mse.pimle.alicante.1 <-mean((Summary.Estimation.Matrix.alicante.1[,5] - alicante.real)^2)
mse.alicante.1 <- data.frame(mse.naive.alicante.1 = mse.naive.alicante.1,mse.mos.alicante.1 = mse.mos.alicante.1, mse.mle.alicante.1 = mse.mle.alicante.1, mse.pimle.alicante.1 = mse.pimle.alicante.1)
mse.alicante.1
cat("El MSE medio es:",mean(mse.naive.alicante.1,mse.mos.alicante.1,mse.mle.alicante.1,mse.pimle.alicante.1), "\n")



## 2.2. Escaños con el primer criterio


DHont.pimle.1.alicante <- compute_dhont_seats(Summary.Estimation.Matrix.alicante.1$pimle.est[1:4]/sum(Summary.Estimation.Matrix.alicante.1$pimle.est[1:4]),total.number.seats.alicante)
names(DHont.pimle.1.alicante) <- rownames(Summary.Estimation.Matrix.alicante.1["pimle.est"])[sort(Summary.Estimation.Matrix.alicante.1[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.1.alicante

DHont.mle.1.alicante <- compute_dhont_seats(Summary.Estimation.Matrix.alicante.1$mle.est[1:4]/sum(Summary.Estimation.Matrix.alicante.1$mle.est[1:4]),total.number.seats.alicante)
names(DHont.mle.1.alicante) <- rownames(Summary.Estimation.Matrix.alicante.1["mle.est"])[sort(Summary.Estimation.Matrix.alicante.1[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.1.alicante

DHont.MoS.1.alicante <- compute_dhont_seats(Summary.Estimation.Matrix.alicante.1$MoS.est[1:4]/sum(Summary.Estimation.Matrix.alicante.1$MoS.est[1:4]),total.number.seats.alicante)
names(DHont.MoS.1.alicante) <- rownames(Summary.Estimation.Matrix.alicante.1["MoS.est"])[sort(Summary.Estimation.Matrix.alicante.1[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.1.alicante

DHont.naive.1.alicante <- compute_dhont_seats(Summary.Estimation.Matrix.alicante.1$naive.est[1:4]/sum(Summary.Estimation.Matrix.alicante.1$naive.est[1:4]),total.number.seats.alicante)
names(DHont.naive.1.alicante) <- rownames(Summary.Estimation.Matrix.alicante.1["naive.est"])[sort(Summary.Estimation.Matrix.alicante.1[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.1.alicante

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.alicante.DHont.1 <- data.frame(naive.est = DHont.naive.1.alicante[political.groups],MoS.est = DHont.MoS.1.alicante[political.groups],mle.est = DHont.mle.1.alicante[political.groups], pimle.est = DHont.pimle.1.alicante[political.groups])
Summary.Estimation.Matrix.alicante.DHont.1



#Outliers: segundo criterio (votos en blanco)
  
  
  

if(median_abs_deviation(dat.alicante[,voting.inputs[5]])!=0){
  outliers.vote.blank.flag <- is_mad_outlier(dat.alicante[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
  outliers.vote.blank.flag
  
  threshold.net.size.vote.blank <- max(dat.alicante[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
  threshold.net.size.vote.blank
  
  input.dat.2.alicante <- dat.alicante[which(dat.alicante[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
  dim(input.dat.2.alicante)
}else{
  input.dat.2.alicante<-dat.alicante
  dim(input.dat.2.alicante)
}



#Como se oberva la dimensión es igual a la que se obtiene en los datos brutos, por lo que no se puede aplicar el segundo criterio de filtrado


#Outliers: tercer criterio
  
  



input.dat.3.alicante <- dat.alicante[which(apply(dat.alicante[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] # Filtering criterion for a political group majoritary voting 
dim(input.dat.3.alicante)



#Correlation
  

unique(input.dat.3.alicante$Age)
correlacion_votos_edades.3.alicante <- cor(input.dat.3.alicante[, voting.inputs], input.dat.3.alicante[, edades.inputs])
print(correlacion_votos_edades.3.alicante)



#NSUM estimation methods
#Preparation
  

# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.3.alicante <- input.dat.3.alicante[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.3.alicante)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.3.alicante) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente





## Resultados con el tercer filtrado
## 3.1. Con el tercer criterio %


#Naive approach
# estimamos por NAIVE
###############

alicante.naive.results.3 <- colMeans(input.dat.nsum.3.alicante[,voting.inputs]/rowSums(input.dat.nsum.3.alicante[,voting.inputs]),na.rm = TRUE)
alicante.naive.results.3



#Ratio of sums approach
# estimamos por ratio of sums

#######################

alicante.RoS.results.3 <- colSums(input.dat.nsum.3.alicante[,voting.inputs])/sum(input.dat.nsum.3.alicante[,voting.inputs])
alicante.RoS.results.3


#MoS approach (Hebecker 2015)
#############################

alicante.MoS.degrees.3 <- Total.alicante*rowMeans(input.dat.nsum.3.alicante[,control.inputs[1:3]]/alicante.subpopulation.sizes[1:3])
alicante.MoS.sizes.3 <- Total.alicante*apply(input.dat.nsum.3.alicante[,voting.inputs],2,function (x) mean(x/alicante.MoS.degrees.3,na.rm = TRUE))

alicante.MoS.results.3 <- prop.table(alicante.MoS.sizes.3)
alicante.MoS.results.3


#MLE method
###########

alicante.mle.est.3 <- networkscaleup::killworth(input.dat.nsum.3.alicante,known_sizes=alicante.subpopulation.sizes,known_ind=control.ind,N=Total.alicante,model="MLE")
alicante.mle.results.3 <- prop.table(Total.alicante*apply(input.dat.nsum.3.alicante[,voting.inputs],2,function (x) mean(x)/mean(alicante.mle.est.3$degrees)))

alicante.mle.results.3


#Plug-in MLE method
###################

alicante.pimle.est.3 <- networkscaleup::killworth(input.dat.nsum.3.alicante,known_sizes=alicante.subpopulation.sizes,known_ind=control.ind,N=Total.alicante,model="PIMLE")
alicante.pimle.results.3 <- prop.table(Total.alicante*apply(input.dat.nsum.3.alicante[,voting.inputs],2,function (x) mean(x[alicante.pimle.est.3$degrees>0]/alicante.pimle.est.3$degrees[alicante.pimle.est.3$degrees>0]))) 

alicante.pimle.results.3



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.alicante.3 <- data.frame(naive.est = alicante.naive.results.3,MoS.est = alicante.MoS.results.3,RoS.est=alicante.RoS.results.3,mle.est = alicante.mle.results.3, pimle.est = alicante.pimle.results.3)
Summary.Estimation.Matrix.alicante.3
t(round(100*Summary.Estimation.Matrix.alicante.3[c("Vox","PP","PSOE","Sumar","Blanco"),],1))





Summary.Estimation.Matrix.alicante.3 <- round(Summary.Estimation.Matrix.alicante.3*100)
Summary.Estimation.Matrix.alicante.3


mse.naive.alicante.3 <-mean((Summary.Estimation.Matrix.alicante.3[,1] - alicante.real)^2)
mse.mos.alicante.3 <-mean((Summary.Estimation.Matrix.alicante.3[,2] - alicante.real)^2)
mse.mle.alicante.3 <-mean((Summary.Estimation.Matrix.alicante.3[,4] - alicante.real)^2)
mse.pimle.alicante.3 <-mean((Summary.Estimation.Matrix.alicante.3[,5] - alicante.real)^2)
mse.alicante.3 <- data.frame(mse.naive.alicante.3 = mse.naive.alicante.3,mse.mos.alicante.3 = mse.mos.alicante.3, mse.mle.alicante.3 = mse.mle.alicante.3, mse.pimle.alicante.3 = mse.pimle.alicante.3)
mse.alicante.3
cat("El MSE medio es:",mean(mse.naive.alicante.3,mse.mos.alicante.3,mse.mle.alicante.3,mse.pimle.alicante.3), "\n")


## 3.2. Escaños con el tercer criterio


DHont.pimle.3.alicante <- compute_dhont_seats(Summary.Estimation.Matrix.alicante.3$pimle.est[1:4]/sum(Summary.Estimation.Matrix.alicante.3$pimle.est[1:4]),total.number.seats.alicante)
names(DHont.pimle.3.alicante) <- rownames(Summary.Estimation.Matrix.alicante.3["pimle.est"])[sort(Summary.Estimation.Matrix.alicante.3[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.3.alicante

DHont.mle.3.alicante <- compute_dhont_seats(Summary.Estimation.Matrix.alicante.3$mle.est[1:4]/sum(Summary.Estimation.Matrix.alicante.3$mle.est[1:4]),total.number.seats.alicante)
names(DHont.mle.3.alicante) <- rownames(Summary.Estimation.Matrix.alicante.3["mle.est"])[sort(Summary.Estimation.Matrix.alicante.3[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.3

DHont.MoS.3.alicante <- compute_dhont_seats(Summary.Estimation.Matrix.alicante.3$MoS.est[1:4]/sum(Summary.Estimation.Matrix.alicante.3$MoS.est[1:4]),total.number.seats.alicante)
names(DHont.MoS.3.alicante) <- rownames(Summary.Estimation.Matrix.alicante.3["MoS.est"])[sort(Summary.Estimation.Matrix.alicante.3[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.3.alicante

DHont.naive.3.alicante <- compute_dhont_seats(Summary.Estimation.Matrix.alicante.3$naive.est[1:4]/sum(Summary.Estimation.Matrix.alicante.3$naive.est[1:4]),total.number.seats.alicante)
names(DHont.naive.3.alicante) <- rownames(Summary.Estimation.Matrix.alicante.3["naive.est"])[sort(Summary.Estimation.Matrix.alicante.3[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.3.alicante

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.alicante.DHont.3 <- data.frame(naive.est = DHont.naive.3.alicante[political.groups],MoS.est = DHont.MoS.3.alicante[political.groups],mle.est = DHont.mle.3.alicante[political.groups], pimle.est = DHont.pimle.3.alicante[political.groups])
Summary.Estimation.Matrix.alicante.DHont.3


#Outliers con la combinación de los criterios
  
if(median_abs_deviation(dat.alicante[,voting.inputs[5]])!=0){
  outliers.vote.blank.flag <- is_mad_outlier(dat.alicante[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
  outliers.vote.blank.flag
  
  threshold.net.size.vote.blank <- max(dat.alicante[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
  threshold.net.size.vote.blank
  
  input.dat <- dat.alicante[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat.alicante[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.alicante[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95 & dat.alicante[,voting.inputs[5]]<=threshold.net.size.vote.blank),] #Customized filtering 
  dim(input.dat.alicante)
  
  #Filtering of outliers using the criteria separately#
  
  #input.dat <- dat[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)) ,] # Filtering by network size vote declaration 
  #dim(input.dat)
  #input.dat <- dat[which(apply(dat[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] # Filtering criterion for a political group majoritary voting 
  #dim(input.dat)
  input.dat.alicante.4 <- dat.alicante[which(dat.alicante[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
  dim(input.dat.alicante.4)
}else{
  input.dat.alicante.4 <- dat.alicante[which(apply(dat.alicante[,voting.inputs],1,sum) < quantile(apply(dat.alicante[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.alicante[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] #Customized filtering 
  dim(input.dat.4)
}


##Correlation
  

unique(input.dat.alicante.4$Age)
correlacion_votos_edades.alicante.4 <- cor(input.dat.alicante.4[, voting.inputs], input.dat.alicante.4[, edades.inputs])
print(correlacion_votos_edades.alicante.4)



#NSUM estimation methods
#Preparation


# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.alicante.4 <- input.dat.alicante.4[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.alicante.4)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.alicante.4) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente





## Resultados con la combinación de filtrados
## 4.1. Con la combinación de criterios %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.alicante.4 <- colMeans(input.dat.nsum.alicante.4[,voting.inputs]/rowSums(input.dat.nsum.alicante.4[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.alicante.4



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.alicante.4 <- colSums(input.dat.nsum.alicante.4[,voting.inputs])/sum(input.dat.nsum.alicante.4[,voting.inputs])
c.val.RoS.results.alicante.4


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.alicante.4 <- Total*rowMeans(input.dat.nsum.alicante.4[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.alicante.4 <- Total*apply(input.dat.nsum.alicante.4[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.alicante.4,na.rm = TRUE))

c.val.MoS.results.alicante.4 <- prop.table(c.val.MoS.sizes.alicante.4)
c.val.MoS.results.alicante.4


#MLE method
###########

c.val.mle.est.alicante.4 <- networkscaleup::killworth(input.dat.nsum.alicante.4,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.alicante.4 <- prop.table(Total*apply(input.dat.nsum.alicante.4[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.alicante.4$degrees)))

c.val.mle.results.alicante.4


#Plug-in MLE method
###################

c.val.pimle.est.alicante.4 <- networkscaleup::killworth(input.dat.nsum.alicante.4,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.alicante.4 <- prop.table(Total*apply(input.dat.nsum.alicante.4[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.alicante.4$degrees>0]/c.val.pimle.est.alicante.4$degrees[c.val.pimle.est.alicante.4$degrees>0]))) 

c.val.pimle.results.alicante.4



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.alicante.4 <- data.frame(naive.est = c.val.naive.results.alicante.4,MoS.est = c.val.MoS.results.alicante.4,RoS.est=c.val.RoS.results.alicante.4,mle.est = c.val.mle.results.alicante.4, pimle.est = c.val.pimle.results.alicante.4)
Summary.Estimation.Matrix.c.val.alicante.4
t(round(100*Summary.Estimation.Matrix.c.val.alicante.4[c("Vox","PP","PSOE","Sumar","Blanco"),],1))





Summary.Estimation.Matrix.c.val.alicante.4 <- round(Summary.Estimation.Matrix.c.val.alicante.4*100)
Summary.Estimation.Matrix.c.val.alicante.4


mse.naive.c.val.alicante.4 <-mean((Summary.Estimation.Matrix.c.val.alicante.4[,1] - c.val.real)^2)
mse.mos.c.val.alicante.4 <-mean((Summary.Estimation.Matrix.c.val.alicante.4[,2] - c.val.real)^2)
mse.mle.c.val.alicante.4 <-mean((Summary.Estimation.Matrix.c.val.alicante.4[,4] - c.val.real)^2)
mse.pimle.c.val.alicante.4 <-mean((Summary.Estimation.Matrix.c.val.alicante.4[,5] - c.val.real)^2)
mse.c.val.alicante.4 <- data.frame(mse.naive.c.val.alicante.4 = mse.naive.c.val.alicante.4,mse.mos.c.val.alicante.4 = mse.mos.c.val.alicante.4, mse.mle.c.val.alicante.4 = mse.mle.c.val.alicante.4, mse.pimle.c.val.alicante.4 = mse.pimle.c.val.alicante.4)
mse.c.val.alicante.4
cat("El MSE medio es:",mean(mse.naive.c.val.alicante.4,mse.mos.c.val.alicante.4,mse.mle.c.val.alicante.4,mse.pimle.c.val.alicante.4), "\n")


## 4.2. Escaños con la combinación de criterios


DHont.pimle.alicante.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.alicante.4$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.alicante.4$pimle.est[1:4]),total.number.seats.alicante)
names(DHont.pimle.alicante.4) <- rownames(Summary.Estimation.Matrix.c.val.alicante.4["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.alicante.4[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.alicante.4

DHont.mle.alicante.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.alicante.4$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.alicante.4$mle.est[1:4]),total.number.seats.alicante)
names(DHont.mle.alicante.4) <- rownames(Summary.Estimation.Matrix.c.val.alicante.4["mle.est"])[sort(Summary.Estimation.Matrix.c.val.alicante.4[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.alicante.4

DHont.MoS.alicante.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.alicante.4$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.alicante.4$MoS.est[1:4]),total.number.seats.alicante)
names(DHont.MoS.alicante.4) <- rownames(Summary.Estimation.Matrix.c.val.alicante.4["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.alicante.4[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.alicante.4

DHont.naive.alicante.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.alicante.4$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.alicante.4$naive.est[1:4]),total.number.seats.alicante)
names(DHont.naive.alicante.4) <- rownames(Summary.Estimation.Matrix.c.val.alicante.4["naive.est"])[sort(Summary.Estimation.Matrix.c.val.alicante.4[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.alicante.4

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.alicante.4 <- data.frame(naive.est = DHont.naive.alicante.4[political.groups],MoS.est = DHont.MoS.alicante.4[political.groups],mle.est = DHont.mle.alicante.4[political.groups], pimle.est = DHont.pimle.alicante.4[political.groups])
Summary.Estimation.Matrix.c.val.DHont.alicante.4



## Diagrama de Venn


threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 #0.9 --> 0.8 (20%)
quantile.vote.alicante <- quantile(apply(dat.alicante[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

index.1.alicante <- which(apply(dat.alicante[,voting.inputs],1,sum) >= quantile.vote.alicante)



# 2. Outliers

index.2.alicante <- which(apply(dat.alicante[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))>0.95)


# Diagrama de Venn

indices_comunes.alicante <- intersect(index.1.alicante, index.2.alicante)



venn.plot <- venn.diagram(
  x = list("Outliers 1" = index.1.alicante, "Outliers 2" = index.2.alicante),
  category.names = c("Outliers 1", "Outliers 2"),
  filename = NULL,
  width = 25,
  height = 25,
  main = "Outliers: Alicante",
  cat.col = c("black", "black"),  # Colores de los círculos
  fill = c("#B9DDF1", "#2E5B88"), # Colores de las categorías
  cat.fontfamily = "sans"
)

grid.draw(venn.plot)



# Castellón


input.dat.castellon<-dat.castellon
dim(input.dat.castellon)





#Correlation

unique(input.dat.castellon$Age)
correlacion_votos_edades <- cor(input.dat.castellon[, voting.inputs], input.dat.castellon[, edades.inputs])
print(correlacion_votos_edades)



castellon.subpopulation.sizes <- c(  83885,  145036,  190978 ,0,0,0)  # solo 3 primeros ,360765,17430,3525000



Total.castellon <-   419899   # Data source https://www.ine.es/jaxi/Datos.htm?tpx=48409 (dato de la suma de las edades)
Total.castellon

castellon.subpopulation.sizes # Totals
round(castellon.subpopulation.sizes/Total,4) # Proportions
names(castellon.subpopulation.sizes) <- control.inputs



#NSUM estimation methods
  
#Preparation


# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.castellon <- input.dat.castellon[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.castellon)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.castellon) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados brutos 
## 1.1 Con los datos brutos %


#Naive approach
# estimamos por NAIVE
###############

castellon.naive.results <- colMeans(input.dat.nsum.castellon[,voting.inputs]/rowSums(input.dat.nsum.castellon[,voting.inputs]),na.rm = TRUE)
castellon.naive.results



#Ratio of sums approach
# estimamos por ratio of sums

#######################

castellon.RoS.results <- colSums(input.dat.nsum.castellon[,voting.inputs])/sum(input.dat.nsum.castellon[,voting.inputs])
castellon.RoS.results


#MoS approach (Hebecker 2015)
#############################

castellon.MoS.degrees <- Total.castellon*rowMeans(input.dat.nsum.castellon[,control.inputs[1:3]]/castellon.subpopulation.sizes[1:3])
castellon.MoS.sizes <- Total.castellon*apply(input.dat.nsum.castellon[,voting.inputs],2,function (x) mean(x/castellon.MoS.degrees,na.rm = TRUE))

castellon.MoS.results <- prop.table(castellon.MoS.sizes)
castellon.MoS.results


#MLE method
###########

castellon.mle.est <- networkscaleup::killworth(input.dat.nsum.castellon,known_sizes=castellon.subpopulation.sizes,known_ind=control.ind,N=Total.castellon,model="MLE")
castellon.mle.results <- prop.table(Total.castellon*apply(input.dat.nsum.castellon[,voting.inputs],2,function (x) mean(x)/mean(castellon.mle.est$degrees)))

castellon.mle.results


#Plug-in MLE method
###################

castellon.pimle.est <- networkscaleup::killworth(input.dat.nsum.castellon,known_sizes=castellon.subpopulation.sizes,known_ind=control.ind,N=Total.castellon,model="PIMLE")
castellon.pimle.results <- prop.table(Total.castellon*apply(input.dat.nsum.castellon[,voting.inputs],2,function (x) mean(x[castellon.pimle.est$degrees>0]/castellon.pimle.est$degrees[castellon.pimle.est$degrees>0]))) 

castellon.pimle.results



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.castellon <- data.frame(naive.est = castellon.naive.results,MoS.est = castellon.MoS.results,RoS.est=castellon.RoS.results,mle.est = castellon.mle.results, pimle.est = castellon.pimle.results)
Summary.Estimation.Matrix.castellon
t(round(100*Summary.Estimation.Matrix.castellon[c("Vox","PP","PSOE","Sumar","Blanco"),],1))

#MSE
  

castellon.real <- c(35.18, 32.58, 15.91, 14.26, 0.89)

Summary.Estimation.Matrix.castellon <- round(Summary.Estimation.Matrix.castellon*100)
Summary.Estimation.Matrix.castellon


mse.naive.castellon <-mean((Summary.Estimation.Matrix.castellon[,1] - castellon.real)^2)
mse.mos.castellon <-mean((Summary.Estimation.Matrix.castellon[,2] - castellon.real)^2)
mse.mle.castellon <-mean((Summary.Estimation.Matrix.castellon[,4] - castellon.real)^2)
mse.pimle.castellon <-mean((Summary.Estimation.Matrix.castellon[,5] - castellon.real)^2)
mse.castellon <- data.frame(mse.naive.castellon = mse.naive.castellon,mse.mos.castellon = mse.mos.castellon, mse.mle.castellon = mse.mle.castellon, mse.pimle.castellon = mse.pimle.castellon)
mse.castellon
cat("El MSE medio es:",mean(mse.naive.castellon,mse.mos.castellon,mse.mle.castellon,mse.pimle.castellon), "\n")




## 1.2. Escaños con datos brutos 



#Estimation of number of seats using D'Hont method (need to upload the function compute_dhont_seats)

total.number.seats.castellon <- 5

DHont.pimle.castellon <- compute_dhont_seats(Summary.Estimation.Matrix.castellon$pimle.est[1:4]/sum(Summary.Estimation.Matrix.castellon$pimle.est[1:4]),total.number.seats.castellon)
names(DHont.pimle.castellon) <- rownames(Summary.Estimation.Matrix.castellon["pimle.est"])[sort(Summary.Estimation.Matrix.castellon[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.castellon

DHont.mle.castellon <- compute_dhont_seats(Summary.Estimation.Matrix.castellon$mle.est[1:4]/sum(Summary.Estimation.Matrix.castellon$mle.est[1:4]),total.number.seats.castellon)
names(DHont.mle.castellon) <- rownames(Summary.Estimation.Matrix.castellon["mle.est"])[sort(Summary.Estimation.Matrix.castellon[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.mle.castellon

DHont.MoS.castellon <- compute_dhont_seats(Summary.Estimation.Matrix.castellon$MoS.est[1:4]/sum(Summary.Estimation.Matrix.castellon$MoS.est[1:4]),total.number.seats.castellon)
names(DHont.MoS.castellon) <- rownames(Summary.Estimation.Matrix.castellon["MoS.est"])[sort(Summary.Estimation.Matrix.castellon[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.castellon

DHont.naive.castellon <- compute_dhont_seats(Summary.Estimation.Matrix.castellon$naive.est[1:4]/sum(Summary.Estimation.Matrix.castellon$naive.est[1:4]),total.number.seats.castellon)
names(DHont.naive.castellon) <- rownames(Summary.Estimation.Matrix.castellon["naive.est"])[sort(Summary.Estimation.Matrix.castellon[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.castellon

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.castellon.DHont <- data.frame(naive.est = DHont.naive.castellon[political.groups],MoS.est = DHont.MoS.castellon[political.groups],mle.est = DHont.mle.castellon[political.groups], pimle.est = DHont.pimle.castellon[political.groups])
t(Summary.Estimation.Matrix.castellon.DHont)





#Outliers: primer criterio
  
  
###############################################################################

#Criterion for retaining the respondents with a network under a specific threshold
threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 #0.9 --> 0.8 (20%)
quantile(apply(dat.castellon[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

#Criterion for retaining the respondents who declare a network with blank voting under a specific threshold

#threshold.net.size.vote.blank <- 9 # Disable this line if you want to fix the threshold by hand


input.dat.1.castellon <- dat.castellon[which(apply(dat.castellon[,voting.inputs],1,sum) < quantile(apply(dat.castellon[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)) ,] # Filtering by network size vote declaration 
dim(input.dat.1.castellon)


#Correlation
  
  
unique(input.dat.1.castellon$Age)
correlacion_votos_edades.1.castellon <- cor(input.dat.1.castellon[, voting.inputs], input.dat.1.castellon[, edades.inputs])
print(correlacion_votos_edades.1.castellon)





#NSUM estimation methods
#Preparation
  

# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.1.castellon <- input.dat.1.castellon[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.1.castellon)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.1.castellon) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados con el primer filtrado
## 2.1. Con el primer criterio %


#Naive approach
# estimamos por NAIVE
###############

castellon.naive.results.1 <- colMeans(input.dat.nsum.1.castellon[,voting.inputs]/rowSums(input.dat.nsum.1.castellon[,voting.inputs]),na.rm = TRUE)
castellon.naive.results.1



#Ratio of sums approach
# estimamos por ratio of sums

#######################

castellon.RoS.results.1 <- colSums(input.dat.nsum.1.castellon[,voting.inputs])/sum(input.dat.nsum.1.castellon[,voting.inputs])
castellon.RoS.results.1


#MoS approach (Hebecker 2015)
#############################

castellon.MoS.degrees.1 <- Total.castellon*rowMeans(input.dat.nsum.1.castellon[,control.inputs[1:3]]/castellon.subpopulation.sizes[1:3])
castellon.MoS.sizes.1 <- Total.castellon*apply(input.dat.nsum.1.castellon[,voting.inputs],2,function (x) mean(x/castellon.MoS.degrees.1,na.rm = TRUE))

castellon.MoS.results.1 <- prop.table(castellon.MoS.sizes.1)
castellon.MoS.results.1


#MLE method
###########

castellon.mle.est.1 <- networkscaleup::killworth(input.dat.nsum.1.castellon,known_sizes=castellon.subpopulation.sizes,known_ind=control.ind,N=Total.castellon,model="MLE")
castellon.mle.results.1<- prop.table(Total.castellon*apply(input.dat.nsum.1.castellon[,voting.inputs],2,function (x) mean(x)/mean(castellon.mle.est.1$degrees)))

castellon.mle.results.1


#Plug-in MLE method
###################

castellon.pimle.est.1 <- networkscaleup::killworth(input.dat.nsum.1.castellon,known_sizes=castellon.subpopulation.sizes,known_ind=control.ind,N=Total.castellon,model="PIMLE")
castellon.pimle.results.1 <- prop.table(Total.castellon*apply(input.dat.nsum.1.castellon[,voting.inputs],2,function (x) mean(x[castellon.pimle.est.1$degrees>0]/castellon.pimle.est.1$degrees[castellon.pimle.est.1$degrees>0]))) 

castellon.pimle.results.1



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.castellon.1 <- data.frame(naive.est = castellon.naive.results.1,MoS.est = castellon.MoS.results.1,RoS.est=castellon.RoS.results.1,mle.est = castellon.mle.results.1, pimle.est = castellon.pimle.results.1)
Summary.Estimation.Matrix.castellon.1
t(round(100*Summary.Estimation.Matrix.castellon.1[c("Vox","PP","PSOE","Sumar","Blanco"),],1))






Summary.Estimation.Matrix.castellon.1 <- round(Summary.Estimation.Matrix.castellon.1*100)
Summary.Estimation.Matrix.castellon.1


mse.naive.castellon.1 <-mean((Summary.Estimation.Matrix.castellon.1[,1] - castellon.real)^2)
mse.mos.castellon.1 <-mean((Summary.Estimation.Matrix.castellon.1[,2] - castellon.real)^2)
mse.mle.castellon.1 <-mean((Summary.Estimation.Matrix.castellon.1[,4] - castellon.real)^2)
mse.pimle.castellon.1 <-mean((Summary.Estimation.Matrix.castellon.1[,5] - castellon.real)^2)
mse.castellon.1 <- data.frame(mse.naive.castellon.1 = mse.naive.castellon.1,mse.mos.castellon.1 = mse.mos.castellon.1, mse.mle.castellon.1 = mse.mle.castellon.1, mse.pimle.castellon.1 = mse.pimle.castellon.1)
mse.castellon.1
cat("El MSE medio es:",mean(mse.naive.castellon.1,mse.mos.castellon.1,mse.mle.castellon.1,mse.pimle.castellon.1), "\n")



## 2.2. Escaños con el primer criterio


DHont.pimle.1.castellon <- compute_dhont_seats(Summary.Estimation.Matrix.castellon.1$pimle.est[1:4]/sum(Summary.Estimation.Matrix.castellon.1$pimle.est[1:4]),total.number.seats.castellon)
names(DHont.pimle.1.castellon) <- rownames(Summary.Estimation.Matrix.castellon.1["pimle.est"])[sort(Summary.Estimation.Matrix.castellon.1[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.1.castellon

DHont.mle.1.castellon <- compute_dhont_seats(Summary.Estimation.Matrix.castellon.1$mle.est[1:4]/sum(Summary.Estimation.Matrix.castellon.1$mle.est[1:4]),total.number.seats.castellon)
names(DHont.mle.1.castellon) <- rownames(Summary.Estimation.Matrix.castellon.1["mle.est"])[sort(Summary.Estimation.Matrix.castellon.1[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.1.castellon

DHont.MoS.1.castellon <- compute_dhont_seats(Summary.Estimation.Matrix.castellon.1$MoS.est[1:4]/sum(Summary.Estimation.Matrix.castellon.1$MoS.est[1:4]),total.number.seats.castellon)
names(DHont.MoS.1.castellon) <- rownames(Summary.Estimation.Matrix.castellon.1["MoS.est"])[sort(Summary.Estimation.Matrix.castellon.1[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.1.castellon

DHont.naive.1.castellon <- compute_dhont_seats(Summary.Estimation.Matrix.castellon.1$naive.est[1:4]/sum(Summary.Estimation.Matrix.castellon.1$naive.est[1:4]),total.number.seats.castellon)
names(DHont.naive.1.castellon) <- rownames(Summary.Estimation.Matrix.castellon.1["naive.est"])[sort(Summary.Estimation.Matrix.castellon.1[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.1.castellon

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.castellon.DHont.1 <- data.frame(naive.est = DHont.naive.1.castellon[political.groups],MoS.est = DHont.MoS.1.castellon[political.groups],mle.est = DHont.mle.1.castellon[political.groups], pimle.est = DHont.pimle.1.castellon[political.groups])
Summary.Estimation.Matrix.castellon.DHont.1



#Outliers: segundo criterio (votos en blanco)
  
  
  
if(median_abs_deviation(dat.castellon[,voting.inputs[5]])!=0){
  outliers.vote.blank.flag <- is_mad_outlier(dat.castellon[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
  outliers.vote.blank.flag
  
  threshold.net.size.vote.blank <- max(dat.castellon[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
  threshold.net.size.vote.blank
  
  input.dat.2.castellon <- dat.castellon[which(dat.castellon[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
  dim(input.dat.2.castellon)
}else{
  input.dat.2.castellon<-dat.castellon
  dim(input.dat.2.castellon)
}



#Como se oberva la dimensión es igual a la que se obtiene en los datos brutos, por lo que no se puede aplicar el segundo criterio de filtrado


#Outliers: tercer criterio
  
###############################################################################


input.dat.3.castellon <- dat.castellon[which(apply(dat.castellon[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] # Filtering criterion for a political group majoritary voting 
dim(input.dat.3.castellon)



#Correlation
  

unique(input.dat.3.castellon$Age)
correlacion_votos_edades.3.castellon <- cor(input.dat.3.castellon[, voting.inputs], input.dat.3.castellon[, edades.inputs])
print(correlacion_votos_edades.3.castellon)



#NSUM estimation methods
#Preparation
  
  
# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.3.castellon <- input.dat.3.castellon[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.3.castellon)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.3.castellon) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente





## Resultados con el tercer filtrado
## 3.1. Con el tercer criterio %


#Naive approach
# estimamos por NAIVE
###############

castellon.naive.results.3 <- colMeans(input.dat.nsum.3.castellon[,voting.inputs]/rowSums(input.dat.nsum.3.castellon[,voting.inputs]),na.rm = TRUE)
castellon.naive.results.3



#Ratio of sums approach
# estimamos por ratio of sums

#######################

castellon.RoS.results.3 <- colSums(input.dat.nsum.3.castellon[,voting.inputs])/sum(input.dat.nsum.3.castellon[,voting.inputs])
castellon.RoS.results.3


#MoS approach (Hebecker 2015)
#############################

castellon.MoS.degrees.3 <- Total.castellon*rowMeans(input.dat.nsum.3.castellon[,control.inputs[1:3]]/castellon.subpopulation.sizes[1:3])
castellon.MoS.sizes.3 <- Total.castellon*apply(input.dat.nsum.3.castellon[,voting.inputs],2,function (x) mean(x/castellon.MoS.degrees.3,na.rm = TRUE))

castellon.MoS.results.3 <- prop.table(castellon.MoS.sizes.3)
castellon.MoS.results.3


#MLE method
###########

castellon.mle.est.3 <- networkscaleup::killworth(input.dat.nsum.3.castellon,known_sizes=castellon.subpopulation.sizes,known_ind=control.ind,N=Total.castellon,model="MLE")
castellon.mle.results.3 <- prop.table(Total.castellon*apply(input.dat.nsum.3.castellon[,voting.inputs],2,function (x) mean(x)/mean(castellon.mle.est.3$degrees)))

castellon.mle.results.3


#Plug-in MLE method
###################

castellon.pimle.est.3 <- networkscaleup::killworth(input.dat.nsum.3.castellon,known_sizes=castellon.subpopulation.sizes,known_ind=control.ind,N=Total.castellon,model="PIMLE")
castellon.pimle.results.3 <- prop.table(Total.castellon*apply(input.dat.nsum.3.castellon[,voting.inputs],2,function (x) mean(x[castellon.pimle.est.3$degrees>0]/castellon.pimle.est.3$degrees[castellon.pimle.est.3$degrees>0]))) 

castellon.pimle.results.3



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.castellon.3 <- data.frame(naive.est = castellon.naive.results.3,MoS.est = castellon.MoS.results.3,RoS.est=castellon.RoS.results.3,mle.est = castellon.mle.results.3, pimle.est = castellon.pimle.results.3)
Summary.Estimation.Matrix.castellon.3
t(round(100*Summary.Estimation.Matrix.castellon.3[c("Vox","PP","PSOE","Sumar","Blanco"),],1))




Summary.Estimation.Matrix.castellon.3 <- round(Summary.Estimation.Matrix.castellon.3*100)
Summary.Estimation.Matrix.castellon.3


mse.naive.castellon.3 <-mean((Summary.Estimation.Matrix.castellon.3[,1] - castellon.real)^2)
mse.mos.castellon.3 <-mean((Summary.Estimation.Matrix.castellon.3[,2] - castellon.real)^2)
mse.mle.castellon.3 <-mean((Summary.Estimation.Matrix.castellon.3[,4] - castellon.real)^2)
mse.pimle.castellon.3 <-mean((Summary.Estimation.Matrix.castellon.3[,5] - castellon.real)^2)
mse.castellon.3 <- data.frame(mse.naive.castellon.3 = mse.naive.castellon.3,mse.mos.castellon.3 = mse.mos.castellon.3, mse.mle.castellon.3 = mse.mle.castellon.3, mse.pimle.castellon.3 = mse.pimle.castellon.3)
mse.castellon.3
cat("El MSE medio es:",mean(mse.naive.castellon.3,mse.mos.castellon.3,mse.mle.castellon.3,mse.pimle.castellon.3), "\n")



## 3.2. Escaños con el tercer criterio


DHont.pimle.3.castellon <- compute_dhont_seats(Summary.Estimation.Matrix.castellon.3$pimle.est[1:4]/sum(Summary.Estimation.Matrix.castellon.3$pimle.est[1:4]),total.number.seats.castellon)
names(DHont.pimle.3.castellon) <- rownames(Summary.Estimation.Matrix.castellon.3["pimle.est"])[sort(Summary.Estimation.Matrix.castellon.3[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.3.castellon

DHont.mle.3.castellon <- compute_dhont_seats(Summary.Estimation.Matrix.castellon.3$mle.est[1:4]/sum(Summary.Estimation.Matrix.castellon.3$mle.est[1:4]),total.number.seats.castellon)
names(DHont.mle.3.castellon) <- rownames(Summary.Estimation.Matrix.castellon.3["mle.est"])[sort(Summary.Estimation.Matrix.castellon.3[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.3

DHont.MoS.3.castellon <- compute_dhont_seats(Summary.Estimation.Matrix.castellon.3$MoS.est[1:4]/sum(Summary.Estimation.Matrix.castellon.3$MoS.est[1:4]),total.number.seats.castellon)
names(DHont.MoS.3.castellon) <- rownames(Summary.Estimation.Matrix.castellon.3["MoS.est"])[sort(Summary.Estimation.Matrix.castellon.3[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.3.castellon

DHont.naive.3.castellon <- compute_dhont_seats(Summary.Estimation.Matrix.castellon.3$naive.est[1:4]/sum(Summary.Estimation.Matrix.castellon.3$naive.est[1:4]),total.number.seats.castellon)
names(DHont.naive.3.castellon) <- rownames(Summary.Estimation.Matrix.castellon.3["naive.est"])[sort(Summary.Estimation.Matrix.castellon.3[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.3.castellon

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.castellon.DHont.3 <- data.frame(naive.est = DHont.naive.3.castellon[political.groups],MoS.est = DHont.MoS.3.castellon[political.groups],mle.est = DHont.mle.3.castellon[political.groups], pimle.est = DHont.pimle.3.castellon[political.groups])
Summary.Estimation.Matrix.castellon.DHont.3

#Outliers con la combinación de los criterios
  
  
if(median_abs_deviation(dat.castellon[,voting.inputs[5]])!=0){
  outliers.vote.blank.flag <- is_mad_outlier(dat.castellon[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
  outliers.vote.blank.flag
  
  threshold.net.size.vote.blank <- max(dat.castellon[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
  threshold.net.size.vote.blank
  
  input.dat <- dat.castellon[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat.castellon[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.castellon[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95 & dat.castellon[,voting.inputs[5]]<=threshold.net.size.vote.blank),] #Customized filtering 
  dim(input.dat.castellon)
  
  #Filtering of outliers using the criteria separately#
  
  #input.dat <- dat[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)) ,] # Filtering by network size vote declaration 
  #dim(input.dat)
  #input.dat <- dat[which(apply(dat[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] # Filtering criterion for a political group majoritary voting 
  #dim(input.dat)
  input.dat.castellon.4 <- dat.castellon[which(dat.castellon[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
  dim(input.dat.castellon.4)
}else{
  input.dat.castellon.4 <- dat.castellon[which(apply(dat.castellon[,voting.inputs],1,sum) < quantile(apply(dat.castellon[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.castellon[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] #Customized filtering 
  dim(input.dat.4)
}

#Correlation
  
  
unique(input.dat.castellon.4$Age)
correlacion_votos_edades.castellon.4 <- cor(input.dat.castellon.4[, voting.inputs], input.dat.castellon.4[, edades.inputs])
print(correlacion_votos_edades.castellon.4)


#NSUM estimation methods
#Preparation
  

# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.castellon.4 <- input.dat.castellon.4[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.castellon.4)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.castellon.4) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente





## Resultados con la combinación de filtrados
## 4.1. Con la combinación de criterios %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.castellon.4 <- colMeans(input.dat.nsum.castellon.4[,voting.inputs]/rowSums(input.dat.nsum.castellon.4[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.castellon.4



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.castellon.4 <- colSums(input.dat.nsum.castellon.4[,voting.inputs])/sum(input.dat.nsum.castellon.4[,voting.inputs])
c.val.RoS.results.castellon.4


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.castellon.4 <- Total*rowMeans(input.dat.nsum.castellon.4[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.castellon.4 <- Total*apply(input.dat.nsum.castellon.4[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.castellon.4,na.rm = TRUE))

c.val.MoS.results.castellon.4 <- prop.table(c.val.MoS.sizes.castellon.4)
c.val.MoS.results.castellon.4


#MLE method
###########

c.val.mle.est.castellon.4 <- networkscaleup::killworth(input.dat.nsum.castellon.4,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.castellon.4 <- prop.table(Total*apply(input.dat.nsum.castellon.4[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.castellon.4$degrees)))

c.val.mle.results.castellon.4


#Plug-in MLE method
###################

c.val.pimle.est.castellon.4 <- networkscaleup::killworth(input.dat.nsum.castellon.4,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.castellon.4 <- prop.table(Total*apply(input.dat.nsum.castellon.4[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.castellon.4$degrees>0]/c.val.pimle.est.castellon.4$degrees[c.val.pimle.est.castellon.4$degrees>0]))) 

c.val.pimle.results.castellon.4



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.castellon.4 <- data.frame(naive.est = c.val.naive.results.castellon.4,MoS.est = c.val.MoS.results.castellon.4,RoS.est=c.val.RoS.results.castellon.4,mle.est = c.val.mle.results.castellon.4, pimle.est = c.val.pimle.results.castellon.4)
Summary.Estimation.Matrix.c.val.castellon.4
t(round(100*Summary.Estimation.Matrix.c.val.castellon.4[c("Vox","PP","PSOE","Sumar","Blanco"),],1))






Summary.Estimation.Matrix.c.val.castellon.4 <- round(Summary.Estimation.Matrix.c.val.castellon.4*100)
Summary.Estimation.Matrix.c.val.castellon.4


mse.naive.c.val.castellon.4 <-mean((Summary.Estimation.Matrix.c.val.castellon.4[,1] - c.val.real)^2)
mse.mos.c.val.castellon.4 <-mean((Summary.Estimation.Matrix.c.val.castellon.4[,2] - c.val.real)^2)
mse.mle.c.val.castellon.4 <-mean((Summary.Estimation.Matrix.c.val.castellon.4[,4] - c.val.real)^2)
mse.pimle.c.val.castellon.4 <-mean((Summary.Estimation.Matrix.c.val.castellon.4[,5] - c.val.real)^2)
mse.c.val.castellon.4 <- data.frame(mse.naive.c.val.castellon.4 = mse.naive.c.val.castellon.4,mse.mos.c.val.castellon.4 = mse.mos.c.val.castellon.4, mse.mle.c.val.castellon.4 = mse.mle.c.val.castellon.4, mse.pimle.c.val.castellon.4 = mse.pimle.c.val.castellon.4)
mse.c.val.castellon.4
cat("El MSE medio es:",mean(mse.naive.c.val.castellon.4,mse.mos.c.val.castellon.4,mse.mle.c.val.castellon.4,mse.pimle.c.val.castellon.4), "\n")



## 4.2. Escaños con la combinación de criterios


DHont.pimle.castellon.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.castellon.4$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.castellon.4$pimle.est[1:4]),total.number.seats.castellon)
names(DHont.pimle.castellon.4) <- rownames(Summary.Estimation.Matrix.c.val.castellon.4["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.castellon.4[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.castellon.4

DHont.mle.castellon.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.castellon.4$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.castellon.4$mle.est[1:4]),total.number.seats.castellon)
names(DHont.mle.castellon.4) <- rownames(Summary.Estimation.Matrix.c.val.castellon.4["mle.est"])[sort(Summary.Estimation.Matrix.c.val.castellon.4[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.castellon.4

DHont.MoS.castellon.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.castellon.4$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.castellon.4$MoS.est[1:4]),total.number.seats.castellon)
names(DHont.MoS.castellon.4) <- rownames(Summary.Estimation.Matrix.c.val.castellon.4["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.castellon.4[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.castellon.4

DHont.naive.castellon.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.castellon.4$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.castellon.4$naive.est[1:4]),total.number.seats.castellon)
names(DHont.naive.castellon.4) <- rownames(Summary.Estimation.Matrix.c.val.castellon.4["naive.est"])[sort(Summary.Estimation.Matrix.c.val.castellon.4[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.castellon.4

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.castellon.4 <- data.frame(naive.est = DHont.naive.castellon.4[political.groups],MoS.est = DHont.MoS.castellon.4[political.groups],mle.est = DHont.mle.castellon.4[political.groups], pimle.est = DHont.pimle.castellon.4[political.groups])
Summary.Estimation.Matrix.c.val.DHont.castellon.4

## Diagrama de Venn


threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 #0.9 --> 0.8 (20%)
quantile.vote.castellon <- quantile(apply(dat.castellon[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

index.1.castellon <- which(apply(dat.castellon[,voting.inputs],1,sum) >= quantile.vote.castellon)



# 2. Outliers

index.2.castellon <- which(apply(dat.castellon[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))>0.95)


# Diagrama de Venn

indices_comunes.castellon <- intersect(index.1.castellon, index.2.castellon)


venn.plot <- venn.diagram(
  x = list("Outliers 1" = index.1.castellon, "Outliers 2" = index.2.castellon),
  category.names = c("Outliers 1", "Outliers 2"),
  filename = NULL,
  width = 25,
  height = 25,
  main = "Outliers: Castellón",
  cat.col = c("black", "black"),  # Colores de los círculos
  fill = c("#B9DDF1", "#2E5B88"), # Colores de las categorías
  cat.fontfamily = "sans"
)

grid.draw(venn.plot)


# Valencia


input.dat.valencia<-dat.valencia
dim(input.dat.valencia)





#Correlation

unique(input.dat.valencia$Age)
correlacion_votos_edades <- cor(input.dat.valencia[, voting.inputs], input.dat.valencia[, edades.inputs])
print(correlacion_votos_edades)


valencia.subpopulation.sizes <- c(   389339,   680013 ,   859571,0,0,0)  # solo 3 primeros ,360765,17430,3525000

Total.valencia <-    1928923    # Data source https://www.ine.es/jaxi/Datos.htm?tpx=48409 (dato de la suma de las edades)
Total.valencia

valencia.subpopulation.sizes # Totals
round(valencia.subpopulation.sizes/Total,4) # Proportions
names(valencia.subpopulation.sizes) <- control.inputs



#NSUM estimation methods
  
#Preparation


# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.valencia <- input.dat.valencia[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.valencia)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.valencia) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados brutos 
## 1.1 Con los datos brutos %


#Naive approach
# estimamos por NAIVE
###############

valencia.naive.results <- colMeans(input.dat.nsum.valencia[,voting.inputs]/rowSums(input.dat.nsum.valencia[,voting.inputs]),na.rm = TRUE)
valencia.naive.results



#Ratio of sums approach
# estimamos por ratio of sums

#######################

valencia.RoS.results <- colSums(input.dat.nsum.valencia[,voting.inputs])/sum(input.dat.nsum.valencia[,voting.inputs])
valencia.RoS.results


#MoS approach (Hebecker 2015)
#############################

valencia.MoS.degrees <- Total.valencia*rowMeans(input.dat.nsum.valencia[,control.inputs[1:3]]/valencia.subpopulation.sizes[1:3])
valencia.MoS.sizes <- Total.valencia*apply(input.dat.nsum.valencia[,voting.inputs],2,function (x) mean(x/valencia.MoS.degrees,na.rm = TRUE))

valencia.MoS.results <- prop.table(valencia.MoS.sizes)
valencia.MoS.results


#MLE method
###########

valencia.mle.est <- networkscaleup::killworth(input.dat.nsum.valencia,known_sizes=valencia.subpopulation.sizes,known_ind=control.ind,N=Total.valencia,model="MLE")
valencia.mle.results <- prop.table(Total.valencia*apply(input.dat.nsum.valencia[,voting.inputs],2,function (x) mean(x)/mean(valencia.mle.est$degrees)))

valencia.mle.results


#Plug-in MLE method
###################

valencia.pimle.est <- networkscaleup::killworth(input.dat.nsum.valencia,known_sizes=valencia.subpopulation.sizes,known_ind=control.ind,N=Total.valencia,model="PIMLE")
valencia.pimle.results <- prop.table(Total.valencia*apply(input.dat.nsum.valencia[,voting.inputs],2,function (x) mean(x[valencia.pimle.est$degrees>0]/valencia.pimle.est$degrees[valencia.pimle.est$degrees>0]))) 

valencia.pimle.results



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.valencia <- data.frame(naive.est = valencia.naive.results,MoS.est = valencia.MoS.results,RoS.est=valencia.RoS.results,mle.est = valencia.mle.results, pimle.est = valencia.pimle.results)
Summary.Estimation.Matrix.valencia
t(round(100*Summary.Estimation.Matrix.valencia[c("Vox","PP","PSOE","Sumar","Blanco"),],1))

#MSE
  
  
valencia.real <- c(33.67, 32.1, 15.22, 16.83, 0.76)

Summary.Estimation.Matrix.valencia <- round(Summary.Estimation.Matrix.valencia*100)
Summary.Estimation.Matrix.valencia


mse.naive.valencia <-mean((Summary.Estimation.Matrix.valencia[,1] - valencia.real)^2)
mse.mos.valencia <-mean((Summary.Estimation.Matrix.valencia[,2] - valencia.real)^2)
mse.mle.valencia <-mean((Summary.Estimation.Matrix.valencia[,4] - valencia.real)^2)
mse.pimle.valencia <-mean((Summary.Estimation.Matrix.valencia[,5] - valencia.real)^2)
mse.valencia <- data.frame(mse.naive.valencia = mse.naive.valencia,mse.mos.valencia = mse.mos.valencia, mse.mle.valencia = mse.mle.valencia, mse.pimle.valencia = mse.pimle.valencia)
mse.valencia
cat("El MSE medio es:",mean(mse.naive.valencia,mse.mos.valencia,mse.mle.valencia,mse.pimle.valencia), "\n")




## 1.2. Escaños con datos brutos 


#Estimation of number of seats using D'Hont method (need to upload the function compute_dhont_seats)

total.number.seats.valencia <- 16

DHont.pimle.valencia <- compute_dhont_seats(Summary.Estimation.Matrix.valencia$pimle.est[1:4]/sum(Summary.Estimation.Matrix.valencia$pimle.est[1:4]),total.number.seats.valencia)
names(DHont.pimle.valencia) <- rownames(Summary.Estimation.Matrix.valencia["pimle.est"])[sort(Summary.Estimation.Matrix.valencia[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.valencia

DHont.mle.valencia <- compute_dhont_seats(Summary.Estimation.Matrix.valencia$mle.est[1:4]/sum(Summary.Estimation.Matrix.valencia$mle.est[1:4]),total.number.seats.valencia)
names(DHont.mle.valencia) <- rownames(Summary.Estimation.Matrix.valencia["mle.est"])[sort(Summary.Estimation.Matrix.valencia[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.mle.valencia

DHont.MoS.valencia <- compute_dhont_seats(Summary.Estimation.Matrix.valencia$MoS.est[1:4]/sum(Summary.Estimation.Matrix.valencia$MoS.est[1:4]),total.number.seats.valencia)
names(DHont.MoS.valencia) <- rownames(Summary.Estimation.Matrix.valencia["MoS.est"])[sort(Summary.Estimation.Matrix.valencia[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.valencia

DHont.naive.valencia <- compute_dhont_seats(Summary.Estimation.Matrix.valencia$naive.est[1:4]/sum(Summary.Estimation.Matrix.valencia$naive.est[1:4]),total.number.seats.valencia)
names(DHont.naive.valencia) <- rownames(Summary.Estimation.Matrix.valencia["naive.est"])[sort(Summary.Estimation.Matrix.valencia[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.valencia

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.valencia.DHont <- data.frame(naive.est = DHont.naive.valencia[political.groups],MoS.est = DHont.MoS.valencia[political.groups],mle.est = DHont.mle.valencia[political.groups], pimle.est = DHont.pimle.valencia[political.groups])
t(Summary.Estimation.Matrix.valencia.DHont)





#Outliers: primer criterio
  
  
###############################################################################

#Criterion for retaining the respondents with a network under a specific threshold
threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 #0.9 --> 0.8 (20%)
quantile(apply(dat.valencia[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

#Criterion for retaining the respondents who declare a network with blank voting under a specific threshold

#threshold.net.size.vote.blank <- 9 # Disable this line if you want to fix the threshold by hand


input.dat.1.valencia <- dat.valencia[which(apply(dat.valencia[,voting.inputs],1,sum) < quantile(apply(dat.valencia[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)) ,] # Filtering by network size vote declaration 
dim(input.dat.1.valencia)



#Correlation
  
  
unique(input.dat.1.valencia$Age)
correlacion_votos_edades.1.valencia <- cor(input.dat.1.valencia[, voting.inputs], input.dat.1.valencia[, edades.inputs])
print(correlacion_votos_edades.1.valencia)



#NSUM estimation methods
#Preparation
  
  
# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.1.valencia <- input.dat.1.valencia[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.1.valencia)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.1.valencia) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente



## Resultados con el primer filtrado
## 2.1. Con el primer criterio %


#Naive approach
# estimamos por NAIVE
###############

valencia.naive.results.1 <- colMeans(input.dat.nsum.1.valencia[,voting.inputs]/rowSums(input.dat.nsum.1.valencia[,voting.inputs]),na.rm = TRUE)
valencia.naive.results.1



#Ratio of sums approach
# estimamos por ratio of sums

#######################

valencia.RoS.results.1 <- colSums(input.dat.nsum.1.valencia[,voting.inputs])/sum(input.dat.nsum.1.valencia[,voting.inputs])
valencia.RoS.results.1


#MoS approach (Hebecker 2015)
#############################

valencia.MoS.degrees.1 <- Total.valencia*rowMeans(input.dat.nsum.1.valencia[,control.inputs[1:3]]/valencia.subpopulation.sizes[1:3])
valencia.MoS.sizes.1 <- Total.valencia*apply(input.dat.nsum.1.valencia[,voting.inputs],2,function (x) mean(x/valencia.MoS.degrees.1,na.rm = TRUE))

valencia.MoS.results.1 <- prop.table(valencia.MoS.sizes.1)
valencia.MoS.results.1


#MLE method
###########

valencia.mle.est.1 <- networkscaleup::killworth(input.dat.nsum.1.valencia,known_sizes=valencia.subpopulation.sizes,known_ind=control.ind,N=Total.valencia,model="MLE")
valencia.mle.results.1<- prop.table(Total.valencia*apply(input.dat.nsum.1.valencia[,voting.inputs],2,function (x) mean(x)/mean(valencia.mle.est.1$degrees)))

valencia.mle.results.1


#Plug-in MLE method
###################

valencia.pimle.est.1 <- networkscaleup::killworth(input.dat.nsum.1.valencia,known_sizes=valencia.subpopulation.sizes,known_ind=control.ind,N=Total.valencia,model="PIMLE")
valencia.pimle.results.1 <- prop.table(Total.valencia*apply(input.dat.nsum.1.valencia[,voting.inputs],2,function (x) mean(x[valencia.pimle.est.1$degrees>0]/valencia.pimle.est.1$degrees[valencia.pimle.est.1$degrees>0]))) 

valencia.pimle.results.1



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.valencia.1 <- data.frame(naive.est = valencia.naive.results.1,MoS.est = valencia.MoS.results.1,RoS.est=valencia.RoS.results.1,mle.est = valencia.mle.results.1, pimle.est = valencia.pimle.results.1)
Summary.Estimation.Matrix.valencia.1
t(round(100*Summary.Estimation.Matrix.valencia.1[c("Vox","PP","PSOE","Sumar","Blanco"),],1))



Summary.Estimation.Matrix.valencia.1 <- round(Summary.Estimation.Matrix.valencia.1*100)
Summary.Estimation.Matrix.valencia.1


mse.naive.valencia.1 <-mean((Summary.Estimation.Matrix.valencia.1[,1] - valencia.real)^2)
mse.mos.valencia.1 <-mean((Summary.Estimation.Matrix.valencia.1[,2] - valencia.real)^2)
mse.mle.valencia.1 <-mean((Summary.Estimation.Matrix.valencia.1[,4] - valencia.real)^2)
mse.pimle.valencia.1 <-mean((Summary.Estimation.Matrix.valencia.1[,5] - valencia.real)^2)
mse.valencia.1 <- data.frame(mse.naive.valencia.1 = mse.naive.valencia.1,mse.mos.valencia.1 = mse.mos.valencia.1, mse.mle.valencia.1 = mse.mle.valencia.1, mse.pimle.valencia.1 = mse.pimle.valencia.1)
mse.valencia.1
cat("El MSE medio es:",mean(mse.naive.valencia.1,mse.mos.valencia.1,mse.mle.valencia.1,mse.pimle.valencia.1), "\n")


## 2.2. Escaños con el primer criterio


DHont.pimle.1.valencia <- compute_dhont_seats(Summary.Estimation.Matrix.valencia.1$pimle.est[1:4]/sum(Summary.Estimation.Matrix.valencia.1$pimle.est[1:4]),total.number.seats.valencia)
names(DHont.pimle.1.valencia) <- rownames(Summary.Estimation.Matrix.valencia.1["pimle.est"])[sort(Summary.Estimation.Matrix.valencia.1[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.1.valencia

DHont.mle.1.valencia <- compute_dhont_seats(Summary.Estimation.Matrix.valencia.1$mle.est[1:4]/sum(Summary.Estimation.Matrix.valencia.1$mle.est[1:4]),total.number.seats.valencia)
names(DHont.mle.1.valencia) <- rownames(Summary.Estimation.Matrix.valencia.1["mle.est"])[sort(Summary.Estimation.Matrix.valencia.1[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.1.valencia

DHont.MoS.1.valencia <- compute_dhont_seats(Summary.Estimation.Matrix.valencia.1$MoS.est[1:4]/sum(Summary.Estimation.Matrix.valencia.1$MoS.est[1:4]),total.number.seats.valencia)
names(DHont.MoS.1.valencia) <- rownames(Summary.Estimation.Matrix.valencia.1["MoS.est"])[sort(Summary.Estimation.Matrix.valencia.1[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.1.valencia

DHont.naive.1.valencia <- compute_dhont_seats(Summary.Estimation.Matrix.valencia.1$naive.est[1:4]/sum(Summary.Estimation.Matrix.valencia.1$naive.est[1:4]),total.number.seats.valencia)
names(DHont.naive.1.valencia) <- rownames(Summary.Estimation.Matrix.valencia.1["naive.est"])[sort(Summary.Estimation.Matrix.valencia.1[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.1.valencia

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.valencia.DHont.1 <- data.frame(naive.est = DHont.naive.1.valencia[political.groups],MoS.est = DHont.MoS.1.valencia[political.groups],mle.est = DHont.mle.1.valencia[political.groups], pimle.est = DHont.pimle.1.valencia[political.groups])
Summary.Estimation.Matrix.valencia.DHont.1



#Outliers: segundo criterio (votos en blanco)
  
  
  
  
if(median_abs_deviation(dat.valencia[,voting.inputs[5]])!=0){
  outliers.vote.blank.flag <- is_mad_outlier(dat.valencia[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
  outliers.vote.blank.flag
  
  threshold.net.size.vote.blank <- max(dat.valencia[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
  threshold.net.size.vote.blank
  
  input.dat.2.valencia <- dat.valencia[which(dat.valencia[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
  dim(input.dat.2.valencia)
}else{
  input.dat.2.valencia<-dat.valencia
  dim(input.dat.2.valencia)
}



#Correlation
  
  
unique(input.dat.2.valencia$Age)
correlacion_votos_edades.valencia.2 <- cor(input.dat.2.valencia[, voting.inputs], input.dat.2.valencia[, edades.inputs])
print(correlacion_votos_edades.valencia.2)



#NSUM estimation methods
#Preparation
  
  
# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.valencia.2 <- input.dat.2.valencia[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.valencia.2)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.valencia.2) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente




## Resultados con el segundo filtrado
## 4.1. Con el segundo criterio %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.valencia.2 <- colMeans(input.dat.nsum.valencia.2[,voting.inputs]/rowSums(input.dat.nsum.valencia.2[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.valencia.2



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.valencia.2 <- colSums(input.dat.nsum.valencia.2[,voting.inputs])/sum(input.dat.nsum.valencia.2[,voting.inputs])
c.val.RoS.results.valencia.2


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.valencia.2 <- Total*rowMeans(input.dat.nsum.valencia.2[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.valencia.2 <- Total*apply(input.dat.nsum.valencia.2[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.valencia.2,na.rm = TRUE))

c.val.MoS.results.valencia.2 <- prop.table(c.val.MoS.sizes.valencia.2)
c.val.MoS.results.valencia.2


#MLE method
###########

c.val.mle.est.valencia.2 <- networkscaleup::killworth(input.dat.nsum.valencia.2,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.valencia.2 <- prop.table(Total*apply(input.dat.nsum.valencia.2[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.valencia.2$degrees)))

c.val.mle.results.valencia.2


#Plug-in MLE method
###################

c.val.pimle.est.valencia.2 <- networkscaleup::killworth(input.dat.nsum.valencia.2,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.valencia.2 <- prop.table(Total*apply(input.dat.nsum.valencia.2[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.valencia.2$degrees>0]/c.val.pimle.est.valencia.2$degrees[c.val.pimle.est.valencia.2$degrees>0]))) 

c.val.pimle.results.valencia.2



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.valencia.2 <- data.frame(naive.est = c.val.naive.results.valencia.2,MoS.est = c.val.MoS.results.valencia.2,RoS.est=c.val.RoS.results.valencia.2,mle.est = c.val.mle.results.valencia.2, pimle.est = c.val.pimle.results.valencia.2)
Summary.Estimation.Matrix.c.val.valencia.2
t(round(100*Summary.Estimation.Matrix.c.val.valencia.2[c("Vox","PP","PSOE","Sumar","Blanco"),],1))



Summary.Estimation.Matrix.c.val.valencia.2 <- round(Summary.Estimation.Matrix.c.val.valencia.2*100)
Summary.Estimation.Matrix.c.val.valencia.2


mse.naive.c.val.valencia.2 <-mean((Summary.Estimation.Matrix.c.val.valencia.2[,1] - c.val.real)^2)
mse.mos.c.val.valencia.2 <-mean((Summary.Estimation.Matrix.c.val.valencia.2[,2] - c.val.real)^2)
mse.mle.c.val.valencia.2 <-mean((Summary.Estimation.Matrix.c.val.valencia.2[,4] - c.val.real)^2)
mse.pimle.c.val.valencia.2 <-mean((Summary.Estimation.Matrix.c.val.valencia.2[,5] - c.val.real)^2)
mse.c.val.valencia.2 <- data.frame(mse.naive.c.val.valencia.2 = mse.naive.c.val.valencia.2,mse.mos.c.val.valencia.2 = mse.mos.c.val.valencia.2, mse.mle.c.val.valencia.2 = mse.mle.c.val.valencia.2, mse.pimle.c.val.valencia.2 = mse.pimle.c.val.valencia.2)
mse.c.val.valencia.2
cat("El MSE medio es:",mean(mse.naive.c.val.valencia.2,mse.mos.c.val.valencia.2,mse.mle.c.val.valencia.2,mse.pimle.c.val.valencia.2), "\n")



## 4.2. Escaños con el segundo criterio


DHont.pimle.valencia.2 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.valencia.2$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.valencia.2$pimle.est[1:4]),total.number.seats.valencia)
names(DHont.pimle.valencia.2) <- rownames(Summary.Estimation.Matrix.c.val.valencia.2["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.valencia.2[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.valencia.2

DHont.mle.valencia.2 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.valencia.2$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.valencia.2$mle.est[1:4]),total.number.seats.valencia)
names(DHont.mle.valencia.2) <- rownames(Summary.Estimation.Matrix.c.val.valencia.2["mle.est"])[sort(Summary.Estimation.Matrix.c.val.valencia.2[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.valencia.2

DHont.MoS.valencia.2 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.valencia.2$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.valencia.2$MoS.est[1:4]),total.number.seats.valencia)
names(DHont.MoS.valencia.2) <- rownames(Summary.Estimation.Matrix.c.val.valencia.2["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.valencia.2[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.valencia.2

DHont.naive.valencia.2 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.valencia.2$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.valencia.2$naive.est[1:4]),total.number.seats.valencia)
names(DHont.naive.valencia.2) <- rownames(Summary.Estimation.Matrix.c.val.valencia.2["naive.est"])[sort(Summary.Estimation.Matrix.c.val.valencia.2[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.valencia.2

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.valencia.2 <- data.frame(naive.est = DHont.naive.valencia.2[political.groups],MoS.est = DHont.MoS.valencia.2[political.groups],mle.est = DHont.mle.valencia.2[political.groups], pimle.est = DHont.pimle.valencia.2[political.groups])
Summary.Estimation.Matrix.c.val.DHont.valencia.2











#Outliers: tercer criterio
  
  
###############################################################################


input.dat.3.valencia <- dat.valencia[which(apply(dat.valencia[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] # Filtering criterion for a political group majoritary voting 
dim(input.dat.3.valencia)



#Correlation
  
  
unique(input.dat.3.valencia$Age)
correlacion_votos_edades.3.valencia <- cor(input.dat.3.valencia[, voting.inputs], input.dat.3.valencia[, edades.inputs])
print(correlacion_votos_edades.3.valencia)



NSUM estimation methods
**Preparation**
  
  
# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.3.valencia <- input.dat.3.valencia[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.3.valencia)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.3.valencia) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente





## Resultados con el tercer filtrado
## 3.1. Con el tercer criterio %


#Naive approach
# estimamos por NAIVE
###############

valencia.naive.results.3 <- colMeans(input.dat.nsum.3.valencia[,voting.inputs]/rowSums(input.dat.nsum.3.valencia[,voting.inputs]),na.rm = TRUE)
valencia.naive.results.3



#Ratio of sums approach
# estimamos por ratio of sums

#######################

valencia.RoS.results.3 <- colSums(input.dat.nsum.3.valencia[,voting.inputs])/sum(input.dat.nsum.3.valencia[,voting.inputs])
valencia.RoS.results.3


#MoS approach (Hebecker 2015)
#############################

valencia.MoS.degrees.3 <- Total.valencia*rowMeans(input.dat.nsum.3.valencia[,control.inputs[1:3]]/valencia.subpopulation.sizes[1:3])
valencia.MoS.sizes.3 <- Total.valencia*apply(input.dat.nsum.3.valencia[,voting.inputs],2,function (x) mean(x/valencia.MoS.degrees.3,na.rm = TRUE))

valencia.MoS.results.3 <- prop.table(valencia.MoS.sizes.3)
valencia.MoS.results.3


#MLE method
###########

valencia.mle.est.3 <- networkscaleup::killworth(input.dat.nsum.3.valencia,known_sizes=valencia.subpopulation.sizes,known_ind=control.ind,N=Total.valencia,model="MLE")
valencia.mle.results.3 <- prop.table(Total.valencia*apply(input.dat.nsum.3.valencia[,voting.inputs],2,function (x) mean(x)/mean(valencia.mle.est.3$degrees)))

valencia.mle.results.3


#Plug-in MLE method
###################

valencia.pimle.est.3 <- networkscaleup::killworth(input.dat.nsum.3.valencia,known_sizes=valencia.subpopulation.sizes,known_ind=control.ind,N=Total.valencia,model="PIMLE")
valencia.pimle.results.3 <- prop.table(Total.valencia*apply(input.dat.nsum.3.valencia[,voting.inputs],2,function (x) mean(x[valencia.pimle.est.3$degrees>0]/valencia.pimle.est.3$degrees[valencia.pimle.est.3$degrees>0]))) 

valencia.pimle.results.3



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.valencia.3 <- data.frame(naive.est = valencia.naive.results.3,MoS.est = valencia.MoS.results.3,RoS.est=valencia.RoS.results.3,mle.est = valencia.mle.results.3, pimle.est = valencia.pimle.results.3)
Summary.Estimation.Matrix.valencia.3
t(round(100*Summary.Estimation.Matrix.valencia.3[c("Vox","PP","PSOE","Sumar","Blanco"),],1))






Summary.Estimation.Matrix.valencia.3 <- round(Summary.Estimation.Matrix.valencia.3*100)
Summary.Estimation.Matrix.valencia.3


mse.naive.valencia.3 <-mean((Summary.Estimation.Matrix.valencia.3[,1] - valencia.real)^2)
mse.mos.valencia.3 <-mean((Summary.Estimation.Matrix.valencia.3[,2] - valencia.real)^2)
mse.mle.valencia.3 <-mean((Summary.Estimation.Matrix.valencia.3[,4] - valencia.real)^2)
mse.pimle.valencia.3 <-mean((Summary.Estimation.Matrix.valencia.3[,5] - valencia.real)^2)
mse.valencia.3 <- data.frame(mse.naive.valencia.3 = mse.naive.valencia.3,mse.mos.valencia.3 = mse.mos.valencia.3, mse.mle.valencia.3 = mse.mle.valencia.3, mse.pimle.valencia.3 = mse.pimle.valencia.3)
mse.valencia.3
cat("El MSE medio es:",mean(mse.naive.valencia.3,mse.mos.valencia.3,mse.mle.valencia.3,mse.pimle.valencia.3), "\n")



## 3.2. Escaños con el tercer criterio


DHont.pimle.3.valencia <- compute_dhont_seats(Summary.Estimation.Matrix.valencia.3$pimle.est[1:4]/sum(Summary.Estimation.Matrix.valencia.3$pimle.est[1:4]),total.number.seats.valencia)
names(DHont.pimle.3.valencia) <- rownames(Summary.Estimation.Matrix.valencia.3["pimle.est"])[sort(Summary.Estimation.Matrix.valencia.3[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.3.valencia

DHont.mle.3.valencia <- compute_dhont_seats(Summary.Estimation.Matrix.valencia.3$mle.est[1:4]/sum(Summary.Estimation.Matrix.valencia.3$mle.est[1:4]),total.number.seats.valencia)
names(DHont.mle.3.valencia) <- rownames(Summary.Estimation.Matrix.valencia.3["mle.est"])[sort(Summary.Estimation.Matrix.valencia.3[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.3

DHont.MoS.3.valencia <- compute_dhont_seats(Summary.Estimation.Matrix.valencia.3$MoS.est[1:4]/sum(Summary.Estimation.Matrix.valencia.3$MoS.est[1:4]),total.number.seats.valencia)
names(DHont.MoS.3.valencia) <- rownames(Summary.Estimation.Matrix.valencia.3["MoS.est"])[sort(Summary.Estimation.Matrix.valencia.3[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.3.valencia

DHont.naive.3.valencia <- compute_dhont_seats(Summary.Estimation.Matrix.valencia.3$naive.est[1:4]/sum(Summary.Estimation.Matrix.valencia.3$naive.est[1:4]),total.number.seats.valencia)
names(DHont.naive.3.valencia) <- rownames(Summary.Estimation.Matrix.valencia.3["naive.est"])[sort(Summary.Estimation.Matrix.valencia.3[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.3.valencia

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.valencia.DHont.3 <- data.frame(naive.est = DHont.naive.3.valencia[political.groups],MoS.est = DHont.MoS.3.valencia[political.groups],mle.est = DHont.mle.3.valencia[political.groups], pimle.est = DHont.pimle.3.valencia[political.groups])
Summary.Estimation.Matrix.valencia.DHont.3


#Outliers con la combinación de los criterios
  

if(median_abs_deviation(dat.valencia[,voting.inputs[5]])!=0){
  outliers.vote.blank.flag <- is_mad_outlier(dat.valencia[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
  outliers.vote.blank.flag
  
  threshold.net.size.vote.blank <- max(dat.valencia[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
  threshold.net.size.vote.blank
  
  input.dat <- dat.valencia[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat.valencia[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.valencia[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95 & dat.valencia[,voting.inputs[5]]<=threshold.net.size.vote.blank),] #Customized filtering 
  dim(input.dat.valencia)
  
  #Filtering of outliers using the criteria separately#
  
  #input.dat <- dat[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)) ,] # Filtering by network size vote declaration 
  #dim(input.dat)
  #input.dat <- dat[which(apply(dat[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] # Filtering criterion for a political group majoritary voting 
  #dim(input.dat)
  input.dat.valencia.4 <- dat.valencia[which(dat.valencia[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
  dim(input.dat.valencia.4)
}else{
  input.dat.valencia.4 <- dat.valencia[which(apply(dat.valencia[,voting.inputs],1,sum) < quantile(apply(dat.valencia[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.valencia[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95),] #Customized filtering 
  dim(input.dat.4)
}


#Correlation
  
  
unique(input.dat.valencia.4$Age)
correlacion_votos_edades.valencia.4 <- cor(input.dat.valencia.4[, voting.inputs], input.dat.valencia.4[, edades.inputs])
print(correlacion_votos_edades.valencia.4)



#NSUM estimation methods
#Preparation
  
  
# eliminamos los datos del entrevistado para sacar nuestras estimaciones
input.dat.nsum.valencia.4 <- input.dat.valencia.4[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum.valencia.4)

# Preparing the data for applying the estimation methods

# me quedo con las columnas solo: control.inputs (de mi data.frame input.dat.nsum)

control.ind <-  which(colnames(input.dat.nsum.valencia.4) %in% control.inputs)
control.ind # me saca el nº de la columna correspondiente





## Resultados con la combinación de filtrados
## 4.1. Con la combinación de criterios %


#Naive approach
# estimamos por NAIVE
###############

c.val.naive.results.valencia.4 <- colMeans(input.dat.nsum.valencia.4[,voting.inputs]/rowSums(input.dat.nsum.valencia.4[,voting.inputs]),na.rm = TRUE)
c.val.naive.results.valencia.4



#Ratio of sums approach
# estimamos por ratio of sums

#######################

c.val.RoS.results.valencia.4 <- colSums(input.dat.nsum.valencia.4[,voting.inputs])/sum(input.dat.nsum.valencia.4[,voting.inputs])
c.val.RoS.results.valencia.4


#MoS approach (Hebecker 2015)
#############################

c.val.MoS.degrees.valencia.4 <- Total*rowMeans(input.dat.nsum.valencia.4[,control.inputs[1:3]]/c.val.subpopulation.sizes[1:3])
c.val.MoS.sizes.valencia.4 <- Total*apply(input.dat.nsum.valencia.4[,voting.inputs],2,function (x) mean(x/c.val.MoS.degrees.valencia.4,na.rm = TRUE))

c.val.MoS.results.valencia.4 <- prop.table(c.val.MoS.sizes.valencia.4)
c.val.MoS.results.valencia.4


#MLE method
###########

c.val.mle.est.valencia.4 <- networkscaleup::killworth(input.dat.nsum.valencia.4,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
c.val.mle.results.valencia.4 <- prop.table(Total*apply(input.dat.nsum.valencia.4[,voting.inputs],2,function (x) mean(x)/mean(c.val.mle.est.valencia.4$degrees)))

c.val.mle.results.valencia.4


#Plug-in MLE method
###################

c.val.pimle.est.valencia.4 <- networkscaleup::killworth(input.dat.nsum.valencia.4,known_sizes=c.val.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
c.val.pimle.results.valencia.4 <- prop.table(Total*apply(input.dat.nsum.valencia.4[,voting.inputs],2,function (x) mean(x[c.val.pimle.est.valencia.4$degrees>0]/c.val.pimle.est.valencia.4$degrees[c.val.pimle.est.valencia.4$degrees>0]))) 

c.val.pimle.results.valencia.4



#.......................................
#Matrix with summary estimation outcomes
#.......................................

Summary.Estimation.Matrix.c.val.valencia.4 <- data.frame(naive.est = c.val.naive.results.valencia.4,MoS.est = c.val.MoS.results.valencia.4,RoS.est=c.val.RoS.results.valencia.4,mle.est = c.val.mle.results.valencia.4, pimle.est = c.val.pimle.results.valencia.4)
Summary.Estimation.Matrix.c.val.valencia.4
t(round(100*Summary.Estimation.Matrix.c.val.valencia.4[c("Vox","PP","PSOE","Sumar","Blanco"),],1))


Summary.Estimation.Matrix.c.val.valencia.4 <- round(Summary.Estimation.Matrix.c.val.valencia.4*100)
Summary.Estimation.Matrix.c.val.valencia.4


mse.naive.c.val.valencia.4 <-mean((Summary.Estimation.Matrix.c.val.valencia.4[,1] - c.val.real)^2)
mse.mos.c.val.valencia.4 <-mean((Summary.Estimation.Matrix.c.val.valencia.4[,2] - c.val.real)^2)
mse.mle.c.val.valencia.4 <-mean((Summary.Estimation.Matrix.c.val.valencia.4[,4] - c.val.real)^2)
mse.pimle.c.val.valencia.4 <-mean((Summary.Estimation.Matrix.c.val.valencia.4[,5] - c.val.real)^2)
mse.c.val.valencia.4 <- data.frame(mse.naive.c.val.valencia.4 = mse.naive.c.val.valencia.4,mse.mos.c.val.valencia.4 = mse.mos.c.val.valencia.4, mse.mle.c.val.valencia.4 = mse.mle.c.val.valencia.4, mse.pimle.c.val.valencia.4 = mse.pimle.c.val.valencia.4)
mse.c.val.valencia.4
cat("El MSE medio es:",mean(mse.naive.c.val.valencia.4,mse.mos.c.val.valencia.4,mse.mle.c.val.valencia.4,mse.pimle.c.val.valencia.4), "\n")


## 4.2. Escaños con la combinación de criterios


DHont.pimle.valencia.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.valencia.4$pimle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.valencia.4$pimle.est[1:4]),total.number.seats.valencia)
names(DHont.pimle.valencia.4) <- rownames(Summary.Estimation.Matrix.c.val.valencia.4["pimle.est"])[sort(Summary.Estimation.Matrix.c.val.valencia.4[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.valencia.4

DHont.mle.valencia.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.valencia.4$mle.est[1:4]/sum(Summary.Estimation.Matrix.c.val.valencia.4$mle.est[1:4]),total.number.seats.valencia)
names(DHont.mle.valencia.4) <- rownames(Summary.Estimation.Matrix.c.val.valencia.4["mle.est"])[sort(Summary.Estimation.Matrix.c.val.valencia.4[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle.valencia.4

DHont.MoS.valencia.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.valencia.4$MoS.est[1:4]/sum(Summary.Estimation.Matrix.c.val.valencia.4$MoS.est[1:4]),total.number.seats.valencia)
names(DHont.MoS.valencia.4) <- rownames(Summary.Estimation.Matrix.c.val.valencia.4["MoS.est"])[sort(Summary.Estimation.Matrix.c.val.valencia.4[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS.valencia.4

DHont.naive.valencia.4 <- compute_dhont_seats(Summary.Estimation.Matrix.c.val.valencia.4$naive.est[1:4]/sum(Summary.Estimation.Matrix.c.val.valencia.4$naive.est[1:4]),total.number.seats.valencia)
names(DHont.naive.valencia.4) <- rownames(Summary.Estimation.Matrix.c.val.valencia.4["naive.est"])[sort(Summary.Estimation.Matrix.c.val.valencia.4[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive.valencia.4

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.c.val.DHont.valencia.4 <- data.frame(naive.est = DHont.naive.valencia.4[political.groups],MoS.est = DHont.MoS.valencia.4[political.groups],mle.est = DHont.mle.valencia.4[political.groups], pimle.est = DHont.pimle.valencia.4[political.groups])
Summary.Estimation.Matrix.c.val.DHont.valencia.4


threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 #0.9 --> 0.8 (20%)
quantile.vote <- quantile(apply(dat.valencia[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

index.1.valencia <- which(apply(dat.valencia[,voting.inputs],1,sum) >= quantile.vote)

outliers.vote.blank.flag <- is_mad_outlier(dat.valencia[,voting.inputs[5]],2) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
outliers.vote.blank.flag

threshold.net.size.vote.blank <- max(dat.valencia[!outliers.vote.blank.flag,voting.inputs[5]]) # Calculate it from the MAD
threshold.net.size.vote.blank

input.dat.valencia <- dat.valencia[which(dat.valencia[,voting.inputs[5]]<=threshold.net.size.vote.blank),] # Filtering criterion of unusual blank vote 
dim(input.dat.valencia)

index.3.valencia <- which(dat.valencia[,voting.inputs[5]]>threshold.net.size.vote.blank)

# 2. Outliers

index.2.valencia <- which(apply(dat.valencia[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))>0.95)


indices_comunes.valencia <- intersect(index.1.valencia, index.2.valencia)
indices_comunes.valencia <- intersect(indices_comunes.valencia,index.3.valencia)
library(VennDiagram)

venn.plot <- venn.diagram(
  x = list("Outliers 1" = index.1.valencia, "Outliers 3" = index.2.after, "Outliers 2" = index.3.valencia),
  category.names = c("Outliers 1", "Outliers 2", "Outliers 3"),
  filename = NULL,
  width = 16,
  height = 6,
  main = "Outliers: Valencia",
  cat.col = c("black", "black", "black"),  # Colores de los círculos
  fill = c("#B9DDF1", "#2E5B88", "#6495BF"), # Colores de las categorías
  cat.fontfamily = "sans"
)
grid.draw(venn.plot)

# Mostrar el número de coincidencias
cat("Número de coincidencias entre Forma 1 y Forma 2:", length(indices_comunes.valencia), "\n")




# CLUSTERING




## Cluster 1

#Sin datos demográficos y con conocidos según situación laboral


# Cargar librerías necesarias


# Supongamos que tienes tus datos en un dataframe llamado 'datos_encuestados'

# Seleccionar las variables relevantes para el clustering
data_cluster <- dat[, c("Vox", "PP", "PSOE", "Sumar","edad.18.30", "edad.31.54", "edad.mas.54", "autonomo", "medico", "desempleado")]

# Estandarizar los datos
scaled_data <- scale(data_cluster)

library(NbClust)
library(factoextra)
fviz_nbclust(scaled_data, kmeans, method = "silhouette")
library(tidyverse)
#m.distancia <- get_dist(scaled_data, method = "euclidean") # 
#fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

k2 <- kmeans(scaled_data, centers = 2, nstart = 25)
k2
str(k2)

#plotear los cluster
fviz_cluster(k2, data = scaled_data)


library(cluster)

# Calculamos la medida de la silueta para cada punto
silhouette_vals <- silhouette(k2$cluster, dist(scaled_data))

# Obtenemos la silueta media
silhouette_avg <- mean(silhouette_vals[, "sil_width"])

# Imprimimos el resultado
print(paste("Silhouette average:", silhouette_avg))







cluster_counts <- table(k2$cluster)
print(cluster_counts)



#Hay 2 del primer cluster 

#Hay 194 del segundo cluster



k3 <- kmeans(scaled_data, centers = 3, nstart = 25)
k3

#plotear los cluster
fviz_cluster(k3, data = scaled_data)

# Calculamos la medida de la silueta para cada punto
silhouette_vals.k3 <- silhouette(k3$cluster, dist(scaled_data))

# Obtenemos la silueta media
silhouette_avg.k3 <- mean(silhouette_vals.k3[, "sil_width"])

# Imprimimos el resultado
print(paste("Silhouette average:", silhouette_avg.k3))




# ANOVA




dat <- df[,c(socio.demo.inputs,voting.inputs,control.inputs,prov.inputs)] # prov.inputs
colnames(dat)
dim(dat)
dat <- na.omit(dat) #Only rows with complete cases are considered
dim(dat)



which(is.na(df$Provincia) & 1:nrow(df)<200)
dat$RealAge = 2023 -dat$Year.Birth
dat$RangeAge = 0

for( i in 1:length(dat$RangeAge)){
  if(dat$RealAge[i]>= 18 & dat$RealAge[i]<= 24){
    dat$RangeAge[i] = 1
  }else{
    if(dat$RealAge[i]>= 25 & dat$RealAge[i]<= 34){
      dat$RangeAge[i] = 2
    }else{
      if(dat$RealAge[i]>= 35 & dat$RealAge[i]<= 44){
        dat$RangeAge[i] = 3
      }else{
        if(dat$RealAge[i]>= 45 & dat$RealAge[i]<= 54){
          dat$RangeAge[i] = 4
        }else{
          dat$RangeAge[i] = 5
        }
      }
    }
  }
}

sum1= 0
for( i in 1:length(dat$RangeAge)){
  if(dat$RangeAge[i] == 1){
    sum1= sum1+1
  }
}



sum2= 0
for( i in 1:length(dat$RangeAge)){
  if(dat$RangeAge[i] == 2){
    sum2= sum2+1
  }
}


sum3= 0
for( i in 1:length(dat$RangeAge)){
  if(dat$RangeAge[i] == 3){
    sum3= sum3+1
  }
}


sum4= 0
for( i in 1:length(dat$RangeAge)){
  if(dat$RangeAge[i] == 4){
    sum4= sum4+1
  }
}

sum5= 0
for( i in 1:length(dat$RangeAge)){
  if(dat$RangeAge[i] == 5){
    sum5= sum5+1
  }
}

cat("Hay", sum1, "personas que tienen entre [18, 24] años","\n")
cat("Hay", sum2, "personas que tienen entre [25, 34] años","\n")
cat("Hay", sum3, "personas que tienen entre [35, 44] años","\n")
cat("Hay", sum4, "personas que tienen entre [45, 54] años","\n")
cat("Hay", sum5, "personas que tienen >= 55 años","\n")


## Anova


#Contraste para el PSOE por edad
#H_0: \mu_1 = ... = \mu_5

anova.psoe <- aov(PSOE ~ RangeAge, data = dat)
summary(anova.psoe)

#Contraste para el PP por edad

anova.pp <- aov(PP ~ RangeAge, data = dat)
summary(anova.pp)


#Contraste para el Sumar por edad

  
  
anova.sumar <- aov(Sumar ~ RangeAge, data = dat)
summary(anova.sumar)


#Contraste para el Vox por edad

  
  
anova.vox <- aov(Vox ~ RangeAge, data = dat)
summary(anova.vox)



#En ningún caso se tiene la evidencia suficiente para rechazar la hipótesis nula


