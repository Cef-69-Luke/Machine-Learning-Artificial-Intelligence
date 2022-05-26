################################SOMMARIO########################################
#title: "AI&ML"
#author: "Luca Cefaloni"
#date: "23/05/2022"

###############################LIBRERIE#########################################
#dati 
library(readxl)

#Grafici 
library(ggplot2) 

#PCA
library(FactoMineR) 
library(factoextra) 
library("corrplot")


#Regressione Lineare
library(caTools) 

#################################DATASET########################################
#DATI ORIGINALI 

#Data Attori 
Originali_Attori_Suburra1 <- read_excel("~/Documents/EAIeMLCefaloni/Dati/Manipolazione Dati/Originali Attori Suburra1.xlsx") #importazione dataset
summary(Originali_Attori_Suburra1)

#Dataset con attori con più battute
att_piu_batt <- Originali_Attori_Suburra1[,c(1,2,6,4,3,5,8,26)] #Attori più importanti


#Data Location 
Originali_Location_Suburra1 <- read_excel("~/Documents/EAIeMLCefaloni/Dati/Manipolazione Dati/Originali Location Suburra1.xlsx")#importazione dataset
summary(Originali_Location_Suburra1)

#Dataset on location più viste 
loc_piu_dur <- Originali_Location_Suburra1[-11, c(1,19,13,20,9,28,3,4,35)]# location con più durata 


#Dataset Attori e Location in percentuale 
Att_e_Loc_Perc <- read_excel("~/Documents/EAIeMLCefaloni/Dati/Manipolazione Dati/Att e Loc Perc.xlsx")#importazione dataset
summary(Att_e_Loc_Perc)

#Dataset Attori e Location in percentuale in csv  
Att.e.Loc.Perc <- read.csv2("~/Documents/EAIeMLCefaloni/Dati/Manipolazione Dati/Att e Loc Perc.csv")#importazione dataset

##################################ANALISI GENERALE##############################
#Analisi Visiva 
#Andamento delle battute per episodio, per gli attori con più frequenza
ggplot()+
  geom_line(data=att_piu_batt,aes(y=`Aureliano Adami`,x= Episodi ,colour="Aureliano"),size=1 )+
  geom_line(data=att_piu_batt,aes(y=`Sara Monaschi`,x= Episodi ,colour="Sara Monaschi"),size=1 )+
  geom_line(data=att_piu_batt,aes(y=`Gabriele "Lele" Marchilli`,x= Episodi ,colour="Gabriele Marchilli"),size=1 )+
  geom_line(data=att_piu_batt,aes(y=`Alberto "Spadino" Anacleti`,x= Episodi ,colour="Alberto Spadino Anacleti"),size=1 )+
  geom_line(data=att_piu_batt,aes(y=`Amedeo Cinaglia`,x= Episodi ,colour="Amedeo Cinaglia"),size=1 )+
  geom_line(data=att_piu_batt,aes(y=`Livia Adami`,x= Episodi ,colour="Livia Adami"),size=1 )+
  geom_line(data=att_piu_batt,aes(y=`Valerio Samurai`,x= Episodi ,colour="Valerio Samurai"),size=1 )+
  scale_color_manual(name = "Attori",
                     values = c("Aureliano" = "darkblue",
                                "Sara Monaschi" = "red",
                                "Gabriele Marchilli" = "green",
                                "Valerio Samurai" = "black",
                                "Alberto Spadino Anacleti" = "violet",
                                "Amedeo Cinaglia" = "orange",
                                "Livia Adami" = "pink"))+
  labs(x="Episodi", y="Battute", 
       title = "Andamento delle battute per episodio",
       subtitle = "Line Plot",
       caption = "Dati estrapolati da Suburra la serie")+
  scale_x_continuous(n.breaks = 10)


#Andamento delle location per episodio, per le location con più frequenza
ggplot()+
  geom_line(data=loc_piu_dur,aes(y=`Roma centro`,x= Episodi ,colour="Roma Centro"),size=1 )+
  geom_line(data=loc_piu_dur,aes(y=`Casa Anacleti`,x= Episodi ,colour="Casa Anacleti"),size=1 )+
  geom_line(data=loc_piu_dur,aes(y=`Periferia romana`,x= Episodi ,colour="Periferia romana"),size=1 )+
  geom_line(data=loc_piu_dur,aes(y=Vaticano,x= Episodi ,colour="Vaticano"),size=1 )+
  geom_line(data=loc_piu_dur,aes(y=`Casa Adami`,x= Episodi ,colour="Casa Adami"),size=1 )+
  geom_line(data=loc_piu_dur,aes(y=`Stabilimento Adami`,x= Episodi ,colour="Stabilimento Adami"),size=1 )+
  geom_line(data=loc_piu_dur,aes(y=`Chiosco Adami`,x= Episodi ,colour="Chiosco Adami"),size=1 )+
  scale_color_manual(name = "Location",
                     values = c("Roma Centro" = "darkblue",
                                "Casa Anacleti" = "red",
                                "Periferia romana" = "green",
                                "Vaticano" = "violet",
                                "Casa Adami" = "orange",
                                "Stabilimento Adami" = "pink",
                                "Chiosco Adami" = "black"))+
  labs(x="Episodi", y="Minuti scene", 
       title = "Andamento delle location per episodio",
       subtitle = "Line Plot",
       caption = "Dati estrapolati da Suburra la serie")+
  scale_x_continuous(n.breaks = 10)


#PCA

#PCA Attori

att.pca <- PCA(Originali_Attori_Suburra1[,-c(1,40,41)], graph = F) #calcolo della pca 
fviz_pca_biplot(att.pca, repel = TRUE,
                col.var = "#B22222", #Colore Variabili
                col.ind = "#000000"  # Colore Individui
) #realizzazione grafico pca

#PCA Location
loc.pca <- PCA(Originali_Location_Suburra1[,-c(1,39,40)], graph = F)


fviz_pca_biplot(loc.pca, repel = TRUE,
                col.var = "#B22222", # Colore Variabili 
                col.ind = "#000000"  # Colore Individui
)#realizzazione grafico pca


#################################REGRESSIONE#######################
#Regeressione lineare---------------------------------------
#dataset utilizzato 
dta <- Att_e_Loc_Perc[,-1] #definizione del dataset togliendo la variabile degli episodi

#funzione arrotondamento variabili 
round_df <- function(x, digits) {
  # arrotondo tutte le variabili numeriche
  # x: frame di dati
  # cifre: numero di cifre da arrotondare
  numeric_columns <- sapply(x, mode) == "numeric"
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

dta <- round_df(dta, 3) #Arrotondo tutte le variabili con 3 decimali


#impostazione seed
set.seed(1734) 

#divido il dataset per traning e testing
dta_split <- sample.split(Y = dta$`Indice IMDb`, SplitRatio = 0.7) 
trainSet <- subset(x = dta, dta_split==T) #dataset di traning
testSet <- subset(x = dta, dta_split==F) #dataset di testing 

dim(trainSet) #controllo dimensione train 
dim(testSet) #controllo diensione test
#modello di prova 
model.prova <- lm(`Indice IMDb` ~ ., data = trainSet) #modello dataset di prova

summary(model.prova) #modello creto errato 


matrix_correlation <- data.matrix(cor(dta)) #matrice correlzioni

summary(matrix_correlation)

#Modello attori ----------
model <- lm(`Indice IMDb` ~ `Aureliano Adami` + `Livia Adami` + Gabriella + 
              `Cugino Spadino Boris` + `Madre Samurai` + `Mara Guagli (Compagna Franco)` + 
              `Cardinale Nascari`, data = trainSet) #modello di traning 


summary(model)
#Grafico residui 

modelResiduals <- as.data.frame(residuals(model)) #creazione del dataframe dei residui
ggplot(modelResiduals, aes(residuals(model))) +
  geom_histogram(fill="deepskyblue", color="black", bins = 10) #graficazione residui

#Predizione modello di test
preds <- predict(model,testSet)

#valori attuali con quelli previsti 
modelprev <- cbind(testSet$`Indice IMDb`, preds)

colnames(modelprev) <- c('Actual', 'Predicted')

modelprev <- as.data.frame(modelprev)

modelprev

#calcolo dell'affidabilità 
mse <- mean((modelprev$Actual - modelprev$Predicted)**2)
rmse <- sqrt(mse)

#valori di affidabilità
aff <- cbind(mse, rmse)
colnames(aff) <- c("MSE", "RMSE")
aff <- as.data.frame(aff)
aff




#Modello location----------

#modello giusto
model_location <- lm(`Indice IMDb` ~ `Casa contessa` + `Chiosco Adami` + 
                       `Vela di Calatrua` + Eur + 
                       `Palestra Aureliano`+ `Roma centro` , data = trainSet) #modello di traning 
summary(model_location)


#Grafico resifui 

modelResidualslocation <- as.data.frame(residuals(model_location)) #creazione dataframe 
ggplot(modelResidualslocation, aes(residuals(model_location))) +
  geom_histogram(fill="deepskyblue", color="black", bins = 10) #graficazione residui

#predict test data
preds_loc <- predict(model_location,testSet)

#dataset valori attuali con quelli previsti 
modelprev_loc <- cbind(testSet$`Indice IMDb`, preds_loc)

colnames(modelprev_loc) <- c('Actual', 'Predicted')

modelprev_loc <- as.data.frame(modelprev_loc)
modelprev_loc

#calcolo dell'affidabilità 
mse_loc <- mean((modelprev_loc$Actual - modelprev_loc$Predicted)**2)
rmse_loc <- sqrt(mse_loc)

#valori di affidabilità
aff_loc <- cbind(mse_loc, rmse_loc)
colnames(aff_loc) <- c("MSE", "RMSE")
aff_loc <- as.data.frame(aff_loc)
aff_loc

