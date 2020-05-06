library(reshape2)
library(ggplot2)
library(corrplot)
library(ggfortify)
library(FactoMineR)
library(factoextra)
library(Hmisc)   #describe
library(e1071) 
library(psych)   #describe
library(naniar)  #gg_miss_upset
library(dplyr)   #glimpse
library(dataPreparation) # préparation des donnée d'entraînement pour la régression logistique
library(rpart) # Arbre de décision pour prédiction de facteurs manquants
library(rpart.plot) #dessin de l'arbre de décision grâce à la fonction prp()
library(tidyverse) #Kmeans clustering
library(cluster)
library(GGally) 
library(plotly)
# Suppression des valeurs NULL
clean <- function(dataset){
  dataset <- na.omit(dataset)
  return(dataset)
}

H <- clean(housing)
#ANALYSE UNIVARIÉE
#Boxplot des variables
boxplotting <- function(dataset,var)
{
  ggplot(dataset)+
    geom_boxplot(notch=TRUE,mapping=aes(x=deparse(substitute(var)),y=var), fill="plum") +labs(title="Box-plot",
                                                                                              subtitle=paste("Boxplot de la variable", deparse(substitute(var))),
                                                                                              x="Variables",
                                                                                              y="Valeurs")+theme(
                                                                                                # Grilles
                                                                                                legend.background = element_rect(fill = "darkgray"),
                                                                                                plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust=0.5),
                                                                                                legend.key = element_rect(fill = "lightgray", color = NA),
                                                                                                legend.position="bottom",
                                                                                                panel.background = element_rect(fill = "#ebebeb"),
                                                                                                panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                                                                                                                colour = "purple"),
                                                                                                panel.grid.minor =  element_line(size = 0.1, linetype = 'solid', colour = "orange") ,
                                                                                                # Change legend key size and keyA width
                                                                                                legend.key.size = unit(0.3, "cm"),
                                                                                                legend.key.width = unit(2.5,"cm"),
                                                                                                plot.background=element_rect(fill = "lightgray"),
                                                                                                axis.text.x = element_text(face = "bold", color = "black", 
                                                                                                                           size = 10, angle = 0),
                                                                                                axis.text.y = element_text(face = "bold", color = "black", 
                                                                                                                           size = 10, angle = 0),
                                                                                                axis.line = element_line(color = "brown", 
                                                                                                                         size = 0.8, linetype = "solid")
                                                                                              )
}

#index des valeurs atypiques
data.atypiqueValue.index <- function(data){
  k <- vector(mode = "integer")
  k <- as.data.frame(which(data %in% c(boxplot.stats(data)$out)))
  return(k)
}

#QQ-plot
qqPlotting <- function(dataset,var){
  ggplot()+
    stat_qq(aes(sample=var, col = "var"))+
    labs(title ="QQ-Plot",subtitle = paste("QQ-PLOT de la variable", names(var)))+theme(
      # Grilles
      legend.background = element_rect(fill = "darkgray"),
      plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust=0.5),
      legend.key = element_rect(fill = "lightgray", color = NA),
      legend.position="bottom",
      panel.background = element_rect(fill = "#ebebeb"),
      panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                      colour = "purple"),
      panel.grid.minor =  element_line(size = 0.1, linetype = 'solid', colour = "orange") ,
      # Change legend key size and keyA width
      legend.key.size = unit(0.3, "cm"),
      legend.key.width = unit(2.5,"cm"),
      plot.background=element_rect(fill = "lightgray"),
      axis.text.x = element_text(face = "bold", color = "black", 
                                 size = 10, angle = 0),
      axis.text.y = element_text(face = "bold", color = "black", 
                                 size = 10, angle = 0),
      axis.line = element_line(color = "brown", 
                               size = 0.8, linetype = "solid")
    )
}

distplot <- function(data,m,n,na.omit){
  ldata <- data
  if(missing(na.omit)){ 
    na.omit <- TRUE
  }
  H22 <- ldata[m:n]
  H22 <- as.data.frame(H22)
  if(na.omit){
    H22 <- na.omit(H22)
    ldata <- na.omit(ldata)
    if(n>length(ldata)){
      size <- length(ldata)
    }
    else{
      size <- dim(H22)[1] + m - 1
    }
    print(size)
  }
  else{
    size <- n
  }
  ggplot(H22, aes(x=ldata[m:size])) +
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 
}

#Nuage de point 
cloudplot <- function(data, sub_data1, sub_data2, shapeData)
{
  if(sub_data1 %in% names(data) && sub_data2 %in% names(data) && shapeData %in% names(data))
  {
    subd <- deparse(substitute(data))
    dt1 <- paste(subd,"$",sub_data1)
    dt2 <- paste(subd,"$",sub_data2)
    shp <- paste(subd,"$",shapeData)
    
    ggplot( data, aes(eval(parse(text=dt1)), eval(parse(text=dt2)))) +
      geom_point(aes(color = eval(parse(text=shp)), shape = eval(parse(text=shp)))) +
      labs(x=sub_data1, y=sub_data2) +
      
      scale_color_manual(values = c("#E4F00A", "lightgray", "#22FF00","green","red", "yellow","orange","pink")) +
      scale_fill_manual(values = c("#E4F00A", "lightgray", "#22FF00","green","cyan","orange","yellow","pink")) + theme(
        # Grilles
        panel.grid.major = element_line(size = 0.2, linetype = 'solid',
                                        colour = "purple"),
        panel.grid.minor =  element_line(size = 0.15, linetype = 'solid',
                                         colour = "cyan") ,
        # Plot ainsi que l'arrière plan
        plot.background=element_rect(fill = "#1a2b33"),
        
        panel.background = element_rect(fill = 'black'),
        # Legende
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = "#993333"),
        legend.key = element_rect(color = "cyan", fill = "black"),
        legend.title = element_text(color = "orange", size=13),
        legend.text = element_text(color = "white"),
        axis.title=element_text(face="bold.italic",
                                size="12", color="gray"),
        axis.text.x = element_text(face = "bold", color = "lightgreen", 
                                   size = 10, angle = 45),
        axis.text.y = element_text(face = "bold", color = "lightgreen", 
                                   size = 10, angle = 45),
        axis.line = element_line(color = "cyan", 
                                 size = 0.8, linetype = "solid")
      )    
  }
  else
  {
    print("WARNING: ARGUMENT USED NOT FOUND IN THE DATASET")
  }
}
cloudplot(housing[1:2000,],"id","price","laundry_options")
cloudplot(housing[2001:10000,],"id","price","laundry_options")

cloudplot(housing[1:2000,],"id","price","parking_options")
cloudplot(housing[2001:10000,],"id","price","parking_options")

#Préparation des données manquantes
housing$laundry_options[which(housing$laundry_options=="")] <- NA
housing$parking_options[which(housing$parking_options=="")] <- NA
housing$sqfeet[which(housing$sqfeet==0)] <- NA
housing$price[which(housing$price==0)] <- NA
#visualisation des données manquantes
gg_miss_upset(housing)
#gg_miss_fct(x = housing, fct = parking_options)
# Prédiction des données manquantes
##Utilisation d'un arbre de décision vu que nos données sont des facteurs: rpart
class_laundry <- rpart(laundry_options ~ price, data=housing[!is.na(housing$laundry_options), ], method="class", control=rpart.control(minsplit=5,cp=0,maxdepth=7), na.action=na.omit)  # class car facteurs
plotcp(class_laundry)
laundry_simple <- prune(class_laundry,cp=0.00015) #Choix de la complexité qui minimise l'erreur
laundry_optimal <- prune(class_laundry,cp=class_laundry$cptable[which.min(class_laundry$cptable[,4]),1])
#prp(laundry_optimal, extra=1)
housing$laundry_options[which(is.na(housing$laundry_options))] <- predict(laundry_optimal, housing[is.na(housing$laundry_options), ], type="class")

class_parking <- rpart(parking_options ~ price, data=housing[!is.na(housing$parking_options), ], method="class", control=rpart.control(minsplit=5,cp=0,maxdepth=8), na.action=na.omit)  
plotcp(class_parking)
parking_simple <- prune(class_parking,cp=0.00013) #Choix de la complexité qui minimise l'erreur
parking_optimal <- prune(class_parking,cp=class_parking$cptable[which.min(class_parking$cptable[,4]),1])
#prp(laundry_optimal)
housing$parking_options[which(is.na(housing$parking_options))] <- predict(parking_optimal, housing[is.na(housing$parking_options), ], type="class")

#Supression des autres colonnes NA
housing <- na.omit(housing)

#Clustering

numericHousing <- housing %>% select(id, price, sqfeet, beds, baths, cats_allowed, dogs_allowed, smoking_allowed, wheelchair_access, electric_vehicle_charge, comes_furnished, lat, long)
distance <- get_dist(numericHousing[1:3000,])
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#GSM
set.seed(2811)
gap_stat <- clusGap(numericHousing[1:1000,], FUN = kmeans, spaceH0 = c("scaledPCA", "original"), d.power=2, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

#Kmeans
set.seed(2811)
km_model <- numericHousing[1:1000,] %>% kmeans(centers = 4, nstart=30) #5 clusters et 30 itérations
km_model
cluster <- km_model$cluster
ggplot(numericHousing[1:1000,], aes(id, price, col = factor(cluster))) + geom_point(size = 2, alpha = 0.8, position = "jitter")
fviz_cluster(culster, data = numericHousing[1:1000])

#Silhouette du Kmeans
sil <- silhouette(cluster, dist(numericHousing[1:1000,]))
fviz_silhouette(sil)

#interpretation des clusters
INTERPRET <- as.data.frame(numericHousing[1:1000,])
INTERPRET$cluster <- as.factor(cluster)
p <- ggparcoord(data = INTERPRET, columns = c(2:6), groupColumn = "cluster", scale = "std") + labs(x = "Housing", y = "valeur en unité écart-type", title = "Clustering")
ggplotly(p)
HCPCluster <- HCPC(numericHousing)

#Analyse factorielle multiple
fact=MFA(housing[1:1000,],group=c(1,3,1,1,9,4,2,1),type=c(rep("c",1),rep("n",1),rep("c",1),rep("n",1),rep("c",1),rep("n",1),rep("c",1),rep("n",1)),ncp=5)
ACP_DATA <- PCA(numericHousing[1:1000,],scale.unit=TRUE,ncp=2)

fviz_pca_var(ACP_DATA,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#016e2a", "#ff2900"),
             repel = FALSE,
             #select=list(contrib=123),
             title = "Cercle des correlations",
             alpha.var = 1, col.quanti.sup = "green", col.circle = "black",)+theme( legend.background = element_rect(fill = "darkgray"),
                                                                                    plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust=0.5),
                                                                                    legend.key = element_rect(fill = "lightgray", color = NA),
                                                                                    legend.position="bottom",
                                                                                    panel.background = element_rect(fill = "#ebebeb"),
                                                                                    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                                                                                                    colour = "purple"),
                                                                                    panel.grid.minor =  element_line(size = 0.1, linetype = 'solid', colour = "orange") ,
                                                                                    # Change legend key size and keyA width
                                                                                    legend.key.size = unit(0.3, "cm"),
                                                                                    legend.key.width = unit(2.5,"cm"),
                                                                                    plot.background=element_rect(fill = "lightgray"),
                                                                                    axis.text.x = element_text(face = "bold", color = "black", 
                                                                                                               size = 10, angle = 0),
                                                                                    axis.text.y = element_text(face = "bold", color = "black", 
                                                                                                               size = 10, angle = 0),
                                                                                    axis.line = element_line(color = "brown", 
                                                                                                             size = 0.8, linetype = "solid"))+
  labs( title= "Cercle de corrélation \n des variables")

fviz_pca_ind(ACP_DATA,
             repel=TRUE,
             pointsize="cos2",
             col.ind="contrib",
             gradient.cols = c("#00AFBB", "#016e2a", "#ff2900"),
)+theme(legend.background = element_rect(fill = "darkgray"),
        plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust=0.5),
        legend.key = element_rect(fill = "lightgray", color = NA),
        legend.position="right",
        panel.background = element_rect(fill = "#ebebeb"),
        panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                        colour = "purple"),
        panel.grid.minor =  element_line(size = 0.1, linetype = 'solid', colour = "orange") ,
        # Change legend key size and keyA width
        legend.key.size = unit(1.3, "cm"),
        legend.key.width = unit(0.5,"cm"),
        plot.background=element_rect(fill = "lightgray"),
        axis.text.x = element_text(face = "bold", color = "black", 
                                   size = 10, angle = 0),
        axis.text.y = element_text(face = "bold", color = "black", 
                                   size = 10, angle = 0),
        axis.line = element_line(color = "brown", 
                                 size = 0.8, linetype = "solid"))+
  labs( title= "Graphique des individus \n ACP")




##séparer les données d'entraînements et de tests
### Index d'échantillon aléatoires
index_train <- sample(1:nrow(housing), 0.8 * nrow(housing)) #préparer es données de façon aléatoire en 80% données d'entraînement et 20% données de test
index_test <- setdiff(1:nrow(housing), index_train)

### Build X_train, y_train, X_test, y_test
X_train <- housing[index_train, -15]
y_train <- housing[index_train, "price"]

X_test <- housing[index_test, -15]
y_test <- housing[index_test, "price"]

###Filtrage des constantes
const_index <- whichAreConstant(housing)
###Filtrage des doublons
doublons_index <- whichAreInDouble(housing)
###Filtrage des Bijections
bijections_index <- whichAreBijection(housing)
###supressions des colonnes inutiles
X_train[[bijections_index]] = NULL
X_test[[bijections_index]] = NULL
###Scaling: En machine learning, les données scalables sont mieux géré que les données non-scalable
scales <- build_scales(dataSet = X_train, cols = "price", verbose = TRUE)
X_train <- fastScale(dataSet = X_train, scales = scales, verbose = TRUE)
#Discretization
bins <- build_bins(dataSet = X_train, cols = "age", n_bins = 10, type = "equal_freq")
#Réduction de dimension par la méthode ACP via la fonction PCA de FactoMineR




