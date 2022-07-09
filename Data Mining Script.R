library(readxl)
library(dplyr)
library(DescTools)
library(corrplot)
library(caret)
library(e1071)
library(klaR)
library(mboost)
library(randomForest)
library(gbm)

#Carregando a base
url = "https://raw.githubusercontent.com/Gabriel4210/Data-Mining-Atv/main/base.xls"
destfile = "base.xls"
curl::curl_download(url, destfile)
base = read_excel(destfile)
base = base[-1,-1]

# EDA
base = base %>% rename("limite" = "X1", "sexo" = "X2", "educacao" = "X3", "casamento" = "X4",
                "idade" = "X5", "psetemb" = "X6", "pagost" = "X7" , "pjul" = "X8",
                "pjun" = "X9", "pmaio" = "X10", "pabril" = "X11","vesetemb" = "X12", 
                "veagost" = "X13", "vejul" = "X14", "vejun" = "X15", "vemaio" = "X16", 
                "veabril" = "X17", "pantsetemb" = "X18", "pantagost" = "X19", "pantjul" = "X20",
                "pantjun" = "X21", "pantmaio" = "X22", "pantabril" = "X23", "Pagamento" = "Y")

base$limite =  as.numeric(base$limite)
base$sexo =  as.factor(base$sexo)
base$educacao =  as.factor(base$educacao)
base$casamento =  as.factor(base$casamento)
base$idade =  as.numeric(base$idade)
base$psetemb =  as.factor(base$psetemb)
base$pagost =  as.factor(base$pagost)
base$pjun =  as.factor(base$pjun)
base$pjul =  as.factor(base$pjul)
base$pmaio =  as.factor(base$pmaio)
base$pabril =  as.factor(base$pabril)
base$vesetemb =  as.numeric(base$vesetemb)
base$veagost =  as.numeric(base$veagost)
base$vejun =  as.numeric(base$vejun)
base$vejul =  as.numeric(base$vejul)
base$vemaio =  as.numeric(base$vemaio)
base$veabril =  as.numeric(base$veabril)
base$pantsetemb =  as.numeric(base$pantsetemb)
base$pantagost =  as.numeric(base$pantagost)
base$pantjun =  as.numeric(base$pantjun)
base$pantjul =  as.numeric(base$pantjul)
base$pantmaio =  as.numeric(base$pantmaio)
base$pantabril =  as.numeric(base$pantabril)
base$Pagamento =  as.factor(base$Pagamento)

#Checando as variaveis que podem ser significativas:
table(base$Pagamento, base$sexo)
table(base$Pagamento, base$educacao)
table(base$Pagamento, base$casamento)
table(base$Pagamento, base$psetemb)
table(base$Pagamento, base$pagost)
table(base$Pagamento, base$pjun)
table(base$Pagamento, base$pjul)
table(base$Pagamento, base$pmaio)
table(base$Pagamento, base$pabril)

corrplot::corrplot(DescTools::PairApply(base[,c(2,3,4,6,7,8,9,10,11,24)], DescTools::CramerV), method = "number", type = "lower", tl.cex = 0.8)
DescTools::PairApply(base[,c(2,3,4,6,7,8,9,10,11,24)], DescTools::CramerV)

#Notamos que existe uma correlação alta entre a variavel resposta e os ultimos pagamentos
#que diminui conforme se distancia no tempo, similar a uma série temporal e de fato
#os dados podem ser organizados como série temporal

plot(base$Pagamento, base$limite)
plot(base$Pagamento, base$idade)
plot(base$Pagamento, base$vesetemb)
plot(base$Pagamento, base$veagost)
plot(base$Pagamento, base$vejul)
plot(base$Pagamento, base$vejun)
plot(base$Pagamento, base$vemaio)
plot(base$Pagamento, base$veabril)
plot(base$Pagamento, base$pantsetemb)
plot(base$Pagamento, base$pantagost)
plot(base$Pagamento, base$pantjul)
plot(base$Pagamento, base$pantjun)
plot(base$Pagamento, base$pantmaio)
plot(base$Pagamento, base$pantabril)

base= base%>%filter(limite < 400000) 
base= base%>%filter(vesetemb < 400000)
base= base%>%filter(veagost < 400000)
base= base%>%filter(vejul < 400000)
base= base%>%filter(vejun < 400000)
base= base%>%filter(vemaio < 400000)
base= base%>%filter(veabril < 400000)
base= base%>%filter(pantsetemb < 400000)
base= base%>%filter(pantagost < 400000)
base= base%>%filter(pantjul < 400000)
base= base%>%filter(pantjun < 400000)
base= base%>%filter(pantmaio < 400000)
base= base%>%filter(pantabril < 400000)
#corrplot(cor(base[,c(1,5,12,13,14,15,16,17,18,19,20,21,22,23)]), method = "number", type = "lower", tl.cex = 0.8)

base$bomsete = as.factor(ifelse(base$psetemb == "-1" | base$psetemb == "-2" | base$psetemb == "0" , 0,1))
base$bomagost = as.factor(ifelse(base$pagost == "-1" | base$pagost == "-2" | base$pagost == "0" , 0,1))
base$bomjul = as.factor(ifelse(base$pjul == "-1" | base$pjul == "-2" | base$pjul == "0" , 0,1))
base$bomjun = as.factor(ifelse(base$pjun == "-1" | base$pjun == "-2" | base$pjun == "0" , 0,1))
base$bommaio = as.factor(ifelse(base$pmaio == "-1" | base$pmaio == "-2" | base$pmaio == "0" , 0,1))
base$bomabril = as.factor(ifelse(base$pabril == "-1" | base$pabril == "-2" | base$pabril == "0" , 0,1))


corrplot::corrplot(DescTools::PairApply(base[,c(24,25,26,27,28,29,30)], DescTools::CramerV), method = "number", type = "lower", tl.cex = 0.8)
DescTools::PairApply(base[,c(24,25,26,27,28,29,30)], DescTools::CramerV)

summary(base)
#Podemos notar que a variavel de resposta é desbalanceada, o modelo ira tender a
#classificar como não pagador um bom pagador

novabase=downSample(base[,-24], base$Pagamento, yname= "Pagamento")

#Particionando os dados
trein =sample(1:nrow(novabase), size=0.75*nrow(novabase))
treino=novabase[trein,]
teste=novabase[-trein,]
trainControl = trainControl(method="cv", number=10, repeats = 5)


##GLM
rl = train(factor(Pagamento) ~ ., data = treino, method = 'glm', trControl=trainControl)
predicao1 = predict(rl, newdata = teste)
cm1 = confusionMatrix(predicao1, factor(teste[['Pagamento']]) )
cm1

##KNN
knn = train(factor(Pagamento) ~ . , data = treino, method = 'knn', trControl=trainControl)
predicao2 = predict(knn, newdata = teste)
cm2 = confusionMatrix(predicao2, factor(teste[['Pagamento']]) )
cm2

##NB
nb = train(factor(Pagamento) ~ limite + factor(sexo) + factor(educacao) + idade +
             vesetemb + veagost+ veabril + vejul+ vejun+ vemaio+
             pantsetemb + pantagost+ pantabril + pantjul+ pantjun+ pantmaio+
             factor(bomsete)+ factor(bomagost)+ factor(bomabril) + factor(bomjul)+ factor(bomjun)+ factor(bommaio), data = treino, method = 'nb', trControl=trainControl)
predicao3 = predict(nb, newdata = teste)
cm3 = confusionMatrix(predicao3, factor(teste[['Pagamento']]) )
cm3

##Arvore
arvore = train(factor(Pagamento) ~ . , data = treino, method = 'rpart', trControl=trainControl)
predicao4 = predict(arvore, newdata = teste)
cm4 = confusionMatrix(predicao4, factor(teste[['Pagamento']]) )
cm4

##LDA
lda = train(factor(Pagamento) ~ limite + factor(sexo) + factor(educacao) + idade +
              vesetemb + veagost+ veabril + vejul+ vejun+ vemaio+
              pantsetemb + pantagost+ pantabril + pantjul+ pantjun+ pantmaio+
              factor(bomsete)+ factor(bomagost)+ factor(bomabril) + factor(bomjul)+ factor(bomjun)+ factor(bommaio) , data = treino, method = 'lda', trControl=trainControl)
predicao5 = predict(lda, newdata = teste)
cm5 = confusionMatrix(predicao5, factor(teste[['Pagamento']]) )
cm5

##GLMboost
glmboost = train(Pagamento ~ ., data = treino, method = "glmboost", trControl=trainControl)
predicao6 = predict(glmboost, newdata = teste)
cm6 = confusionMatrix(predicao6, factor(teste[['Pagamento']])  )
cm6

#RF
rf = train(factor(Pagamento) ~ . , data = treino, method = 'rf', trControl=trainControl)
predicao7 = predict(rf, newdata = teste)
cm7 = confusionMatrix(predicao7, factor(teste[['Pagamento']])  )
cm7

#bag
bag = train(factor(Pagamento) ~ . , data = treino, method = 'treebag',trControl=trainControl)
predicao8 = predict(bag, newdata = teste)
cm8 = confusionMatrix(predicao8, factor(teste[['Pagamento']]) )
cm8

#GBM
set.seed(421)
gbm = train(factor(Pagamento) ~ .
            , data=treino
            , distribution="bernoulli"
            , method="gbm"
            , trControl=trainControl
            , verbose=FALSE
            , bag.fraction=0.75
            , metric = c("Accuracy", "Sensitivity")
)                  

gbm$metric

cm1$byClass
cm2$byClass
cm3$byClass
cm4$byClass
cm5$byClass
cm6$byClass
cm7$byClass
cm8$byClass
