
# Carga paquetes
library(openxlsx)
library(knitr)
library(dplyr)
library("stringr")
library(ggplot2)
library(cowplot) # para unir graficos de ggplot
library(tidyselect)
library(rminer)
library(caret)
library(corrplot)
library(car)
library(GGally)
library(tinytex)
library(klaR)


# Resolución de la práctica

## Descripción del dataset


# lectura dataset
base_heart <- read.csv("heart.csv",header = TRUE)

# Tipo de dato asignado a cada campo
sapply(base_heart, function(x) class(x))

# variables del dataset
variables_base <- names(base_heart)


## Limpieza de los datos

### Tipos de variables

# tipos de variables iniciales
res <- sapply(base_heart,class)
kable(data.frame(variables=names(res),clase=as.vector(res)))


# transformacion variables
base_heart$sex <- as.factor(base_heart$sex)
base_heart$cp <- as.factor(base_heart$cp)
base_heart$fbs <- as.factor(base_heart$fbs)
base_heart$restecg <- as.factor(base_heart$restecg)
base_heart$exng <- as.factor(base_heart$exng)
base_heart$slp <- as.factor(base_heart$slp)
base_heart$caa <- as.factor(base_heart$caa)
base_heart$thall <- as.factor(base_heart$thall)
base_heart$output <- as.factor(base_heart$output)

res <- sapply(base_heart,class)
kable(data.frame(variables=names(res),clase=as.vector(res)))


str(base_heart)

### Eliminación de valores nulos, outliers

# Números de valores desconocidos por campo
sapply(base_heart, function(x) sum(is.na(x)))

sum(duplicated(base_heart))

index <- which(duplicated(base_heart)==TRUE)
base_heart <- base_heart[-index,]

# revision de duplicados en la base
sum(duplicated(base_heart))

# dimension nueva base de datos
dim(base_heart)

#### Valores extremos(outliers)

variables_base <- names(base_heart)

# gráfico de caja y bigotes
c1 <- ggplot(base_heart, aes(y = trtbps)) +
  stat_boxplot(geom = "errorbar", width = 0.25) + 
  geom_boxplot(color = "black", fill = "lightblue") +
  labs(title = "Trtbps", x='', y='Frequency') + theme_classic()

c2 <- ggplot(base_heart, aes(y = chol)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(color = "black", fill = "lightblue") +
  labs(title = "Chol", x='', y='') + theme_classic()

c3 <- ggplot(base_heart, aes(y = thalachh)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +  
  geom_boxplot(color = "black", fill = "lightblue") + 
  labs(title = "Thalachh", x='', y='') + theme_classic()

c4 <- ggplot(base_heart, aes(y = oldpeak)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(color = "black", fill = "lightblue") +
  labs(title = "Oldpeak", x='', y='') + theme_classic()

c5 <- ggplot(base_heart, aes(y = age)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(color = "black", fill = "lightblue") +
  labs(title = "Age", x='', y='') + theme_classic()

plot_grid(c1,c2,c3,c4,c5, nrow=1, ncol=5, labels='') 

# valores atípicos de trtbps
boxplot.stats(base_heart$trtbps)$out
# valores atípicos de chol
boxplot.stats(base_heart$chol)$out
# valores atípicos de thalachh
boxplot.stats(base_heart$thalachh)$out
# valores atípicos de oldpeak
boxplot.stats(base_heart$oldpeak)$out

### Conversión de los datos

base_heart[,c(4,5,8,10)] <- scale(base_heart[,c(4,5,8,10)])

head(base_heart)

### Exportación de los datos preprocesados

write.csv(base_heart, "heart_data_clean.csv")

## Análisis de los datos

num_heart <- base_heart[, c(1,4,5,8,10)]
categ_heart <- base_heart[, -c(1,4,5,8,10)]

# Histogramas variables numericas
############

h1 <-ggplot(num_heart, aes(x = age)) + geom_histogram(color = "black", fill = "white") + 
  labs(title = "Age", x = 'age', y = 'Frequency') + theme_classic()

h2 <-ggplot(num_heart, aes(x = trtbps)) + geom_histogram(color = "black", fill = "white") + 
  labs(title = "Resting Blood Pressure", x = 'trtbps', y = 'Frequency') + theme_classic()

h3 <-ggplot(num_heart, aes(x = chol)) + geom_histogram(color = "black", fill = "white") + 
  labs(title = "Cholesterol", x = 'chol', y = 'Frequency') + theme_classic()

h4 <-ggplot(num_heart, aes(x = thalachh)) + geom_histogram(color = "black", fill = "white") + 
  labs(title = "Max Heart Rate", x = 'thalachh', y = 'Frequency') + theme_classic()

h5 <-ggplot(num_heart, aes(x = oldpeak)) + geom_histogram(color = "black", fill = "white") + 
  labs(title = "Old Peak", x = 'oldpeak', y = 'Frequency') + theme_classic() 

plot_grid(h1, h2, h3, h4, h5, nrow = 2, ncol = 3, labels = '') 


# Gráficos de barras variables categóricas
b1 <- ggplot(categ_heart, aes(x = sex, fill = sex)) + geom_bar(color = "black") +
  labs(title = "Sex", x = 'sex', y = 'Count') + theme_classic() +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(breaks=c("0", "1"), labels=c("Female", "Male"))

b2 <- ggplot(categ_heart, aes(x = cp, fill = cp)) + geom_bar(color = "black") +
  labs(title = "Chest Pain Type", x = 'cp', y = 'Count') + theme_classic() +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(breaks=c("0", "1", "2", "3"), labels=c("Typical angina", "Atypical angina", "Non-anginal pain", "Asymptomatic"))

b3 <- ggplot(categ_heart, aes(x = fbs, fill = fbs)) + geom_bar(color = "black") +
  labs(title = "Fasting Blood Sugar", x = 'fbs', y = 'Count') + theme_classic() +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(breaks=c("0", "1"),labels=c(">120 mg/gl", "<120 mg/gl"))

b4 <- ggplot(categ_heart, aes(x = restecg, fill = restecg)) + geom_bar(color = "black") +
  labs(title = "Resting EC", x = 'restecg', y = 'Count') + theme_classic() +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(breaks=c("0", "1", "2"), labels=c("Normal", "St-t wave abnormality", "Left Ventricular Hypertrophy"))

b5 <- ggplot(categ_heart, aes(x = exng, fill = exng)) + geom_bar(color = "black") +
  labs(title = "Exercise Induced Angina", x = 'exng', y = 'Count') + theme_classic() +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(breaks=c("0", "1"), labels=c("Yes", "No"))

b6 <- ggplot(categ_heart, aes(x = slp, fill = slp)) + geom_bar(color = "black") +
  labs(title = "Slope Peak", x = 'slp', y = 'Count') + theme_classic() +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(breaks=c("0", "1", "2"), labels=c("Unsloping", "Flat", "Downslopping"))

b7 <- ggplot(categ_heart, aes(x = caa, fill = caa)) + geom_bar(color = "black") +
  labs(title = "Nº of major vessels", x = 'caa', y = 'Count') + theme_classic() + 
  theme(legend.title = element_blank()) +
  scale_fill_discrete(breaks=c("0", "1", "2", "3"), labels=c("0", "1", "2", "3"))

b8 <- ggplot(categ_heart, aes(x = thall, fill = thall)) + geom_bar(color = "black") +
  labs(title = "Thalassemia", x = 'thall', y = 'Count') + theme_classic() +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(breaks=c("0", "1", "2", "3"), labels=c("Null", "Fixed defect", "Normal", "Reversable defect"))

b9 <- ggplot(categ_heart, aes(x = output, fill = output)) + geom_bar(color = "black") +
  labs(title = "Diagnosis", x = 'output', y = 'Count') + theme_classic() +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(breaks=c("0", "1"),
                      labels=c("Less chance heart attack", "More chance heart attack"))

plot_grid(b1, b2, b3, b4, b5, b6, b7, b8, b9, nrow = 5, ncol = 2, labels = '') 


### Selección de los grupos de datos que se quieren analizar/comparar.

# variables numericas
plt <- ggpairs(num_heart,columns=1:5,ggplot2::aes(alpha=0.75,color=base_heart$output),
               legend=2,upper = list(continuous = wrap("points",alpha = 0.75,size=2.8)),
               lower = list(continuous = wrap("points",alpha = 0.75,size=2.8))) + 
  theme(text=element_text(size=22)) + scale_colour_discrete(name="output",labels=c('Less chance Heart Attack', 'High Chance Heart Attack'))
plt 

# variables categoricas
p6 <- ggplot(categ_heart, aes(x = sex, fill = output)) + geom_bar() + 
  labs(title = "Sex", y = 'Count') + theme_classic() + theme(legend.position = "bottom") +
  scale_fill_manual(labels = c("Less chance heart attack", "More chance heart attack"),
                    values = c("darkorchid1", "chartreuse3"))

p7 <- ggplot(categ_heart, aes(x = cp, fill = output)) + geom_bar() + 
  labs(title = "Chest Pain Type", y = 'Count') + theme_classic() + theme(legend.position = "bottom") + scale_fill_manual(labels = c("Less chance heart attack", "More chance heart attack"), values = c("darkorchid1", "chartreuse3"))

p8 <- ggplot(categ_heart, aes(x = fbs, fill = output)) + geom_bar() + 
  labs(title = "Fasting Blood Sugar", y = 'Count') + theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(labels = c("Less chance heart attack", "More chance heart attack"),
                    values = c("darkorchid1", "chartreuse3"))

p9 <- ggplot(categ_heart, aes(x = restecg, fill = output)) + geom_bar() + 
  labs(title = "Resting EC", y = 'Count') + theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(labels = c("Less chance heart attack", "More chance heart attack"),
                    values = c("darkorchid1", "chartreuse3"))

p10 <- ggplot(categ_heart, aes(x = exng, fill = output)) + geom_bar() + 
  labs(title = "Exercise Induced Angina", y = 'Count') + theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(labels = c("Less chance heart attack", "More chance heart attack"),
                    values = c("darkorchid1", "chartreuse3"))

p11 <- ggplot(categ_heart, aes(x = slp, fill = output)) + geom_bar() + 
  labs(title = "Slope Peak", y = 'Count') + theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(labels = c("Less chance heart attack", "More chance heart attack"),
                    values = c("darkorchid1", "chartreuse3"))

p12 <- ggplot(categ_heart, aes(x = caa, fill = output)) + geom_bar() + 
  labs(title = "Nº of major vessels", y = 'Count') + theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(labels = c("Less chance heart attack", "More chance heart attack"),
                    values = c("darkorchid1", "chartreuse3"))

p13 <- ggplot(categ_heart, aes(x = thall, fill = output)) + geom_bar() + 
  labs(title = "Thalassemia", y = 'Count') + theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(labels = c("Less chance heart attack", "More chance heart attack"),
                    values = c("darkorchid1", "chartreuse3"))
options(repr.plot.width = 10, repr.plot.height =20)
plot_grid(p6, p7, p8, p9, p10, p11, p12, p13, nrow = 4, ncol = 2, labels = '')

# analisis estadistico descriptivo
summary(base_heart)

### Comprobación de la normalidad y homogeneidad de la varianza.

# normalidad shapiro-wilk
apply(num_heart, 2, shapiro.test)

# homocedasticidad fligner killeen
fligner.test(age ~ output, base_heart)
fligner.test(trtbps ~ output, base_heart)
fligner.test(chol ~ output, base_heart)
fligner.test(thalachh ~ output, base_heart)
fligner.test(oldpeak ~ output, base_heart)

## Pruebas estadísticas

### Contraste de hipótesis
 
# Test Chi-cuadrado para las variables cualitativas.
sex_output <- table(base_heart$sex, base_heart$output)
chisq.test(sex_output)

cp_output <- table(base_heart$cp, base_heart$output)
chisq.test(cp_output)

fbs_output <- table(base_heart$fbs, base_heart$output)
chisq.test(fbs_output)

restecg_output <- table(base_heart$restecg, base_heart$output)
chisq.test(restecg_output)

exng_output <- table(base_heart$exng, base_heart$output)
chisq.test(exng_output)

slp_output <- table(base_heart$slp, base_heart$output)
chisq.test(slp_output)

caa_output <- table(base_heart$caa, base_heart$output)
chisq.test(caa_output)

thall_output <- table(base_heart$thall, base_heart$output)
chisq.test(thall_output)

#Test de Mann-Whitney para las variables cuantitativas. 
lapply(base_heart[,c(1,4,5,8,10)], function(x) wilcox.test(x ~ base_heart$output, paired = F))

### Correlación
cm <- cor(num_heart, method = "spearman")
cm
corrplot(cm, method="color") # Representación gráfica

# Correlacion Spearman + significancia
cor1 <- cor.test(num_heart$age,num_heart$trtbps, method="spearman")
cor1$p.value
cor2 <- cor.test(num_heart$age,num_heart$chol, method="spearman")
cor2$p.value
cor3 <- cor.test(num_heart$age,num_heart$thalachh, method="spearman")
cor3$p.value
cor4 <- cor.test(num_heart$age,num_heart$oldpeak, method="spearman")
cor4$p.value
cor5 <- cor.test(num_heart$trtbps,num_heart$chol, method="spearman")
cor5$p.value
cor6 <- cor.test(num_heart$trtbps,num_heart$thalachh, method="spearman")
cor6$p.value
cor7 <- cor.test(num_heart$trtbps,num_heart$oldpeak, method="spearman")
cor7$p.value
cor8 <- cor.test(num_heart$chol,num_heart$thalachh, method="spearman")
cor8$p.value
cor9 <- cor.test(num_heart$chol,num_heart$oldpeak, method="spearman")
cor9$p.value
cor10 <- cor.test(num_heart$thalachh,num_heart$oldpeak, method="spearman")
cor10$p.value

### Métodos supervisados de clasificación.
####	Partición de los datos

# Division Test-train
set.seed(1234)
h <- holdout(base_heart$output, ratio = 2/3, mode = "stratified")
train_heart <- base_heart[h$tr,]
test_heart <- base_heart[h$ts,]

# Validacion cruzada k-fold
train_control <- trainControl(method = "cv", number = 4)


# 1) Naive Bayes
set.seed(1234)
# Entrenamiento modelo
nb_mod <-train(output ~ . , data = train_heart, method = "nb", trControl = train_control)
# Predicciones
nb_pred <- predict(nb_mod, newdata = test_heart)
# Resultados y accuracy
nb_results <- confusionMatrix(nb_pred, test_heart$output, positive = "1")
nb_results$table
nb_results$overall
nb_acc <- nb_results$overall['Accuracy']

# 2) RF
set.seed(1234)
rf_mod <-train(output ~ . , data = train_heart, method = "rf", trControl = train_control)
rf_pred <- predict(rf_mod, newdata = test_heart)
rf_results <-confusionMatrix(rf_pred, test_heart$output, positive = "1")
rf_results$table
rf_results$overall
rf_acc <- rf_results$overall['Accuracy']


# 3) SVM
set.seed(1234)
svm_mod <-train(output ~ . , data = train_heart, method = "svmRadial", trControl = train_control)
svm_pred <- predict(svm_mod, newdata = test_heart)
svm_results <-confusionMatrix(svm_pred, test_heart$output, positive = "1")
svm_results$table
svm_results$overall
svm_acc <- svm_results$overall['Accuracy']

# 4) glm logistic regression 
set.seed(1234)
lg_mod <-train(output ~ . , data = train_heart, method = "glm", trControl = train_control)
lg_pred <- predict(lg_mod, newdata = test_heart)
lg_results <-confusionMatrix(lg_pred, test_heart$output, positive = "1")
lg_results$table
lg_results$overall
lg_acc <- lg_results$overall['Accuracy']

# 5) knn
set.seed(1234)
knn_mod <-train(output ~ . , data = train_heart, method = "knn", trControl = train_control)
knn_pred <- predict(knn_mod, newdata = test_heart)
knn_results <-confusionMatrix(knn_pred, test_heart$output, positive = "1")
knn_results$table
knn_results$overall
knn_acc <- knn_results$overall['Accuracy']


# comparación accuracy
model_names <- c("Naive Bayes", "Random Forest", "SVM", "Logistic Regression", 'KNN')

acc <- c(nb_acc, rf_acc, svm_acc, lg_acc, knn_acc)
df_acc <- data.frame(model_names, acc)
df_acc$model_names <- factor(df_acc$model_names, levels = df_acc$model_names)

ggplot( mapping = aes(x=df_acc$model_names)) +
  geom_bar(aes(y = ..acc.., fill = df_acc$model_names),width = 0.9,show.legend = FALSE)+
  geom_text(aes( y = ..acc.., label = scales::percent(..acc..)),
            size=4, stat = "count", vjust = -1)+ ylim(0, 1)+labs(y = "Accuracy", x="")+
  theme(text = element_text(size = 15)) + theme_classic()
