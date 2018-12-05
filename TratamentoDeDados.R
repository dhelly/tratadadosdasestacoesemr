##################################################
## Project: Tratamento de Dados
## Script purpose: Trata dados de arquivos TXT's
## Date: October 2018
## Author: Jaqueline Fernandes
## Email: jaqueline.duarte@ifcg.edu.br
##################################################



#instalação e carregamento dos pacotes necessario
requiredPackages = c('tidyverse', 'dplyr')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}


#definindo diretorio de trabalho - Local onde os dados serao salvos
setwd(choose.dir())


cabecalhoPrincipal <- c("codigo_estacao","ano", "mes", "dia","hora",
               "volts", "temp_painel",
               "temp_inst","temp_max","temp_min",
               "umid_inst","umid_max","umid_min",
               "pto_orvalho_inst","pto_orvalho_max","pto_orvalho_min",
               "pressao","pressao_max","pressao_min",
               "vento_direcao", "vento_vel", "vento_rajada",
               "radiacao","precipitacao", 
               "a", "b", "c", "d","e")


#importando arquivo
dados <- read.csv(file.choose(), sep = " ", header = F, col.names = cabecalhoPrincipal, stringsAsFactors = F)

#removendo o lixo
dados <- dados[,c(1:24)]
dados <- dados[dados$ano != '////',]

#convertendo a hora para numeric
dados[,c(2:24)] <- mutate_all(dados[,c(2:24)], function(x) as.numeric(as.character(x)))

###### Funcoes de Tratamento de Dados
## tabelao do ano
anoBissextos <- function(ano){
  if(ano %% 4 == 0){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

tabelaoAno <- function(ano){
  mes = 1:12
  dtMeses <- data.frame('mes' = mes,
                        'tdias' = c(31,28,31,30,31,30,31,31,30,31,30,31), stringsAsFactors = F)
  
  cabecalho <- c("codigo_estacao","ano", "mes", "dia","hora",
                 "volts", "temp_painel",
                 "temp_inst","temp_max","temp_min",
                 "umid_inst","umid_max","umid_min",
                 "pto_orvalho_inst","pto_orvalho_max","pto_orvalho_min",
                 "pressao","pressao_max","pressao_min",
                 "vento_direcao", "vento_vel", "vento_rajada",
                 "radiacao","precipitacao")
  dt <- data.frame(matrix(rep(NA,24), nrow = 1, ncol = 24))
  colnames(dt) <- cabecalho
  
  hora = 0:23
  for(m in mes) {
    dia = 1:dtMeses$tdias[dtMeses$mes == m]
    for (d in dia) {
      for(h in hora){
        r <- c(NA, ano,m,d,h, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
        dt <- rbind(dt,r) 
      }
      #bissexto
      if(m == '02' & d == 28){
        if(anoBissextos(ano)){
          for(h in hora){
            r <- c(NA, ano,m,29,h, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
            dt <- rbind(dt,r) 
          }
        }  
      }
    }
  }
  
  #convertendo hora
  dt$hora <- as.numeric(as.character(dt$hora))
  
  #remover primeira linha
  dt <- dt[-c(1),]  
  
  dt
}


#criar tabela
anoDados <- as.numeric(unique(dados[dados$ano, c('ano')]))
dttemp <- tabelaoAno(anoDados)

#copiar dados para a nova tabela
tam = dim(dttemp)[1]
for (i in 1:tam) {
  valor = filter(dados, ano == dttemp[i,'ano'] & mes == dttemp[i,'mes'] & dia == dttemp[i,'dia'] & hora == dttemp[i,'hora'])
  if(dim(valor)[1] != 0){
    dttemp[i,'volts'] <- valor$volts
    dttemp[i,'temp_painel']<- valor$temp_painel  
    
    dttemp[i,'temp_inst']<- valor$temp_inst
    dttemp[i,'temp_max']<- valor$temp_max
    dttemp[i,'temp_min']<- valor$temp_min
    
    dttemp[i,'umid_inst']<- valor$umid_inst
    dttemp[i,'umid_max']<- valor$umid_max
    dttemp[i,'umid_min']<- valor$umid_min
    
    dttemp[i,'pto_orvalho_inst']<- valor$pto_orvalho_inst
    dttemp[i,'pto_orvalho_max']<- valor$pto_orvalho_max
    dttemp[i,'pto_orvalho_min']<- valor$pto_orvalho_min
    
    dttemp[i,'pressao'] <- valor$pressao
    dttemp[i,'pressao_max'] <- valor$pressao_max
    dttemp[i,'pressao_min'] <- valor$pressao_min
    
    dttemp[i,'vento_direcao'] <- valor$vento_direcao
    dttemp[i,'vento_vel'] <- valor$vento_vel
    dttemp[i,'vento_rajada'] <- valor$vento_rajada
    
    dttemp[i,'radiacao'] <- valor$radiacao
    dttemp[i,'precipitacao'] <- valor$precipitacao
    
  }
  
}
cod_estacao <-unique(dados$codigo_estacao)
dttemp$codigo_estacao <-  cod_estacao

#exportando o resultado para o padrao csv
write.csv2(x = dttemp, file = paste(cod_estacao, anoDados,"csv", sep = '.'), na = "")







