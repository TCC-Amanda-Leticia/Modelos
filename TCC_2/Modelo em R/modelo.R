# Pacotes de Banco de Dados  

# install.packages("Rtools")  
# install.packages("RMySQL") 
# install.packages("odbc")
# install.packages("dplyr") 
# install.packages("readr") 
# install.packages("rpart") 
# install.packages("rpart.plot") 
# install.packages("xtable") 

# Bibliotecas de Banco de dados 

library("DBI")  
library(RMySQL) 
library(odbc) 

# Bibliotecas de árvore de decisão


.libPaths() 

# Conexão com o Banco de dados  

drv <- dbDriver("MySQL")  

mydb = dbConnect(drv, user='root', password='259604', dbname='student_data', host='localhost')  

summary(mydb) 
dbListTables(mydb) 

# Recuperando ID das Disciplinas  

dbListFields(mydb, 'dados') 

dbListFields(mydb, 'fluxos') 

# Definindo variável de cursos 

cursos <- c("AUTOMOTIVA", "AEROESPACIAL", "SOFTWARE", "ENERGIA", "ELETRÔNICA", "ENGENHARIA FGA") 

# Definindo variável de semestres 

semestres <- c("2015/2", "2016/1", "2016/2", "2017/1", "2017/2", "2018/1", "2018/2", "2019/1", "2019/2", "2020/1", "2020/2", "2021/1", "2021/2") 

for (semestre in semestres) { 
  for (curso in cursos) { 
    
    #Recuperando disciplinas     
    
    dataset <- dbGetQuery(mydb, paste0("select Disciplina from student_data.fluxos where Abrev ='", curso, "'"))  
    
    listaDisciplinas <- unique(dataset[, "Disciplina"]) 

    # Pegando informações sobre os alunos matrículados do banco

    dataset <- dbGetQuery(mydb, paste0("SELECT IDFluxo, Status, Matriculas, Ingresso, IDDisciplina, IDEstudante FROM student_data.dados as
    dados INNER JOIN
    student_data.fluxos as fluxos ON dados.IDFluxo = fluxos.Fluxo where Abrev = '", curso,"' and Ingresso = '", semestre,"' ;"))

    listaEstudantes <- unique(dataset[, "IDEstudante"])

    # Criando matriz vazia com o tamanho de linhas com o total de estudantes e o tamanho de coluna com o total de disciplinas     

    dados <- data.frame(matrix(0,nrow = length(listaEstudantes), ncol = length(listaDisciplinas)))

    # Adicionando a lista de estudantes como linhas da tabela

    rownames(dados) <- listaEstudantes

    # Adicionando a lista de disciplinas como colunas da tabela
    colnames(dados) <- listaDisciplinas

    # Adcionando o número de matrículas que o aluno tem em determinado disciplina na nova matriz
    
    for (l in 1:nrow(dataset)) {
      i <- as.character(dataset[l, "IDEstudante"])
      j <- as.character(dataset[l, "IDDisciplina"])
      
      Tentativas <- as.integer(dataset[l, "Matriculas"])
      dados[i, j] <- Tentativas
    }
    
    # Cria lista de status vazia
    
    listaStatus <- c()

    # Populando lista de status dos alunos selecionados
    
    teste <- unique(dataset[ , c("IDEstudante", "Status")])
    
    for (l in 1:nrow(teste)) {
      listaStatus <- c(listaStatus, teste[l, "Status"])
    }

    
    #Adicionando lista de status como coluna da tabela de dados
    dados <- cbind(listaStatus, dados)
    
    # Renomeando coluna de lista de status
    names(dados)[1] <- "Status"
    
    dados[is.na(dados)] <- 0

    #Transformando a lista de status em categorias
    dados$Status <- factor(dados$Status)
    
    # Carregando pacotes
    #install.packages("caret")
    #install.packages("e1071")
    
    library(caret)
    library(Amelia)
    library(pROC)
    library(e1071)
    
    
    set.seed(9850)
    
    # Mudando variável status para realizar classificação
    levels(dados$Status) <- c("Formado", "Matriculado", "Evadido")
    
    #install.packages("C50")
    #install.packages("randomForest")
    
    library(rpart)
    library(rpart.plot)
    library(C50)
    library(randomForest)
    
    #### ARVORE DE DECISÃO
    
    modeloArvoreDecisao <- rpart(Status ~ ., dados)
    rpart.plot(modeloArvoreDecisao)
    modeloArvoreDecisao
    summary(modeloArvoreDecisao)
    
    #### C5.0
    
    modeloC50 <- C5.0(Status ~ ., data = dados)
    ### verificar o tries
    plot(modeloC50)
    summary(modeloC50)
    
    #### RANDOM FOREST
    
    quantidadeVariaveis <- length(listaDisciplinas)
    numero <- sqrt(quantidadeVariaveis)
    max <- ceiling(numero)
    
    modeloRandomForest <- randomForest(Status ~ ., data = dados, ntree = 500, mtry = max, importance = TRUE)
    modeloRandomForest
    plot(modeloRandomForest)
    
    alunos_evadidos <- 0
    for (ingresso in semestres) {
      rm(dataset)

      # Pegando informações sobre os alunos matrículados do banco

      dataset <- dbGetQuery(mydb, paste0("SELECT IDFluxo, Status, Matriculas, Ingresso, IDDisciplina, IDEstudante FROM student_data.dados as
    dados INNER JOIN
    student_data.fluxos as fluxos ON dados.IDFluxo = fluxos.Fluxo where Abrev = '", curso,"' and Ingresso = '", semestre,"' ;"))

      listaEstudantes <- unique(dataset[, "IDEstudante"])

      # Criando matriz vazia com o tamanho de linhas com o total de estudantes e o tamanho de coluna com o total de disciplinas

      dadosPrevisao <- data.frame(matrix(0,nrow = length(listaEstudantes), ncol = length(listaDisciplinas)))

      # Adicionando a lista de estudantes como linhas da tabela

      rownames(dadosPrevisao) <- listaEstudantes

      # Adicionando a lista de disciplinas como colunas da tabela
      colnames(dadosPrevisao) <- listaDisciplinas

      # Adcionando o número de matrículas que o aluno tem em determinado disciplina na nova matriz

      for (l in 1:nrow(dataset)) {
        i <- as.character(dataset[l, "IDEstudante"])
        j <- as.character(dataset[l, "IDDisciplina"])

        Tentativas <- as.integer(dataset[l, "Matriculas"])
        dadosPrevisao[i, j] <- Tentativas
      }

      # Cria lista de status vazia

      listaStatus <- c()

      # Populando lista de status dos alunos selecionados

      teste <- unique(dataset[ , c("IDEstudante", "Status")])

      for (l in 1:nrow(teste)) {
        listaStatus <- c(listaStatus, teste[l, "Status"])
      }


      #Adicionando lista de status como coluna da tabela de dados
      dadosPrevisao <- cbind(listaStatus, dadosPrevisao)

      # Renomeando coluna de lista de status
      names(dadosPrevisao)[1] <- "Status"

      dados[is.na(dadosPrevisao)] <- 0

      #Transformando a lista de status em categorias
      dados$Status <- factor(dados$Status)

      dadosPrevisao["previsaoArvoreDecisao"] <- predict(modeloArvoreDecisao, type = "class", newdata = dadosPrevisao)
      dadosPrevisao["previsaoC50"] <- predict(modeloC50, type = "class", newdata = dadosPrevisao)
      dadosPrevisao["previsaoRandomForest"] <- predict(modeloRandomForest, type = "class", newdata = dadosPrevisao)

      alunosEvadidosArvoreDecisao <- nrow(dadosPrevisao[dadosPrevisao[, "previsaoArvoreDecisao"] == 5,])
      alunosEvadidosC50 <- nrow(dadosPrevisao[dadosPrevisao[, "previsaoC50"] == 5,])
      alunosEvadidosRandomForest <- nrow(dadosPrevisao[dadosPrevisao[, "previsaoRandomForest"] == 5,])
    }
    
  }
} 

#### md - gerar relatório com a lista de alunos que podem evadir, curso, quantidade de alunos matrículados, resumo, quantos por semestre e quantos estão com previsão de evasão e quais são, acurácia em porcentagem. em pdf.
#### aparece em todos os modelos, entre semestres e entre modelos. Escala de cores, acima de 75% 
#### dados do curso
#### um pdf por curso
####
####

#Desconectando do banco

all_cons <- dbListConnections(MySQL())

print(all_cons)

for(con in all_cons)
  +  dbDisconnect(con)

print(paste(length(all_cons), " connections killed."))
