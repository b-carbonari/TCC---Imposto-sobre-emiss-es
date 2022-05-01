################## TOP LEVEL ##################

install.packages("data.table","tidyverse", "openxlsx", "knittr",
                 "kableExtra", "scales", "ggrepel", "gridExtra")


getwd()
setwd("C:/Users/USER/Desktop/TCC/Modelo IO ambientalmente expandido")


library(data.table)
library(tidyverse)
library(openxlsx)
library(knitr) 
library(kableExtra)
library(scales)
library(ggrepel)
library(gridExtra)

################## Dados #####################

paridade<- fread("paridade setores 20211209.csv", 
                 header = TRUE, sep = ";", 
                 dec = ".", 
                 encoding = "UTF-8")

mip_setores<- fread("setores mip guilhoto 20211209.csv", 
                 header = TRUE, sep = ";", 
                 dec = ".", 
                 encoding = "UTF-8")

mip<- fread("MIP PxP 2018.csv", 
            header = FALSE, sep = ";", 
            dec = ",", 
            encoding = "UTF-8")

DA <- fread("DA MIP PxP 2018.csv", 
           header = FALSE, sep = ";", 
           dec = ",", 
           encoding = "UTF-8")

############Zeros à esquerda dos códigos

mip_setores$mip_n <- 1:68

mip_setores[, mip_cd:= str_pad(mip_cd, width = 4,
                               side = "left",
                               pad = "0")]

cols<- colnames(paridade)[5:29]


paridade[ , (cols) := lapply(.SD, 
                             FUN = str_pad,
                             width = 4,
                                     side = "left",
                                     pad = "0"), .SDcols = cols]

paridade<- paridade[ emit != "-",]

###Formatação - 1 combinação de mip_Cd e ipcc_cd por linha

paridade <- data.table(pivot_longer(paridade, cols = 5:ncol(paridade),
                                    values_to = "mip_cd", values_drop_na = TRUE))



################## Distribuindo emissões de setores com múltiplas paridades  ######



#Primeiro, distribuindo emissões ipcc para os setores mip de acordo com a DA relativa


mip_setores[, DA:= DA]

mip_setores[, DI:= rowSums(mip)]

mip_setores[, DF:= as.numeric(DA) - DI]

paridade[mip_setores, DA:= i.DA, 
         on = .(mip_cd) ]

paridade[,  DA_total:= sum(DA), 
         by = ipcc_cd][,
           DA_relativa:= DA/DA_total
         ][,
           mip_emit:= as.numeric(emit)*DA_relativa]

## depois, somando as emissões que cada mip recebeu de cada ipcc

paridade[, mip_emit:= sum(mip_emit, na.rm = TRUE),
         by = mip_cd]




## Próximo passo: montar a matriz R a partir da mip_emit

mip_setores[paridade, mip_emit:= i.mip_emit, on = .(mip_cd)]

#mip_ft_emit - mip_emit (milhões de kilogramas/ milhões de reais)
#ou seja, Kg/R$
mip_setores[, mip_ft_emit:= mip_emit/DA]

## limpando NA
mip_setores[is.na(mip_ft_emit), mip_ft_emit:= 0]
mip_setores[is.na(mip_emit), mip_emit:= 0]

R<- diag(mip_setores$mip_ft_emit)

################## Matrizes Relevantes #############


DA<- as.vector(DA$V1)

mip<- data.matrix(mip)

A = mip %*% diag(1 / DA)

##Inversa de leontief

B = solve(diag(68) - A)


# Conhecendo as emissões do setor, podemos calcular as emissões totais
# considerando as emissões embutidas nos insumos.


R %*% B %*% diag(mip_setores$DF)


#isso deu igual ao mip_em
rowSums(R %*% B %*% diag(mip_setores$DF))


##Emissões em Gg (Gigagramas, 1 Gg = 1 000 000 kg = 1000t)
## Valores da Mip em R$ 1 000 000

############## Calculando o vetor de Preços ######


#Por definição, Valor Agregado do setor i
#(incluindo importações, impostos, inputs pimários...)
#é igual à demanda total pelo setor i, menos
# a soma de todas as compras realizadas por i no setor produtivo

ve <- DA - colSums(mip)


# vec; o valor agregado por R$ de produção do setor
vec <- ve/DA

#e; emissões por real de produção do setor

e<- t(mip_setores$mip_ft_emit) %*% B


# taxa por tonelada (1000kg) de CO2e
# da Silva Freitas et .al usa R$50/tCO2e
#Moz-Christofoletti e Pereda usam USD40 e USD80
#considerando a taxa média de cÂmbio no ano de ref.
#no caso de 2018, a médial anual da compra foi 3,6536

#phi <- 80*3.6536

phi<- 50
# arrecadação total por setor
Tee = (phi/1000) * t(e) * DA

# arrecadaão por R$
tau = Tee/DA

## vetor de preços pós tributação
p<- round(t(B)%*%(vec+tau), digits = 9)


mip_setores[, p:= p]
######## Dados de consumo ########

#importando

pof<- fread("CADERNETA_COLETIVA.csv")
colnames(pof)[which(colnames(pof) == "V9001")] <- "cd_produto"

pof2<- fread("DESPESA_COLETIVA.csv")
colnames(pof2)[which(colnames(pof2) == "V9001")] <- "cd_produto"

pof_indiv <- fread("DESPESA_INDIVIDUAL.csv")
colnames(pof_indiv)[which(colnames(pof_indiv) == "V9001")] <- "cd_produto"

pof_produtos <- fread("Cadastro de Produtos.csv")
colnames(pof_produtos) <- c("quadro", "cd_produto", "nm_produto")

morador<- fread("morador.csv")

morador <- morador[,.(UF, TIPO_SITUACAO_REG,COD_UPA,
                      NUM_DOM,NUM_UC, COD_INFORMANTE,
                      V0403,
                      V0404,V0405,
                      V0425,PESO_FINAL, ANOS_ESTUDO,
                      RENDA_TOTAL,PC_RENDA_DISP)]

colnames(morador)[7:10] <- c("idade", "sexo",
                             "cor","curso_mais_elevado")
##########quarentena

`%!in%` <- Negate(`%in%`)

pof[,V8000_DEFLA:= V8000_DEFLA*FATOR_ANUALIZACAO]

pof2[QUADRO %in% c(10,19),
     V8000_DEFLA:= V8000_DEFLA*FATOR_ANUALIZACAO*V9011]

pof2[QUADRO != 10 & QUADRO != 19, V8000_DEFLA:= V8000_DEFLA*FATOR_ANUALIZACAO]


pof_indiv[QUADRO %in% c(44,47,48,49,50), 
        V8000_DEFLA:= V8000_DEFLA*FATOR_ANUALIZACAO*V9011]


pof_indiv[QUADRO %!in% c(44,47,48,49,50), 
          V8000_DEFLA:= V8000_DEFLA*FATOR_ANUALIZACAO]
###########40

colnames(morador)[7:10] <- c("idade", "sexo",
                             "cor","curso_mais_elevado")


##Chave identificadora da unidade de consumo

morador[,cod_uc := paste(COD_UPA,NUM_DOM, NUM_UC,sep = "_") ]

##Chave identificadora do individuo

morador[,cod_inf := paste(COD_UPA,NUM_DOM, NUM_UC, COD_INFORMANTE, sep = "_") ]



#numero de moradores por unidade de consumo
morador[, n:= .N, by = cod_uc]

#lista dos dados pof

pofs<- list(pof,pof2,pof_indiv)


#chave da unidade de consumo

chave_uc <- function(x) {
  x[, cod_uc:= 
      paste(COD_UPA,NUM_DOM,NUM_UC, sep = "_")]
}

pofs<-  map(.x = pofs, 
            .f = chave_uc )

#soma do dispendio domiciliar

#removendo os valores == 9999999.99,
#que indicam não-aplicabilidade
disp_na <- function(x) {
  x[V8000_DEFLA == 9999999.99,
    V8000_DEFLA:= NA]
}


pofs<-  map(.x = pofs, 
            .f = disp_na )


dispendio_uc <- function(x) {
  x[, disp_uc:= 
      sum(V8000_DEFLA, na.rm = TRUE),
    by = cod_uc]
}

pofs<-  map(.x = pofs, 
            .f = dispendio_uc )

##numero de habitantes

n_hab <- function(x) {
  x[morador, n:= 
      i.n,
    on = "cod_uc"]
}

pofs<-  map(.x = pofs, 
            .f = n_hab )

#renomeando as colunas de valor monetário e quadro e definindo chave

pofs <- pofs %>% map(~ rename(., quadro = QUADRO))
pofs <- pofs %>% map(~ rename(., vlr_defla = V8000_DEFLA))
pofs <- pofs %>% map(~ setkey(., cols = quadro))

pof<- pofs[[1]]
pof2<- pofs[[2]]
pof_indiv<- pofs[[3]]



##montando a distribuição de renda

#juntando os dispendios de todas pofs

morador[pof, disp_pof:=
          i.disp_uc,
        on = "cod_uc"]

morador[pof2, disp_pof2:=
          i.disp_uc,
        on = "cod_uc"]

morador[pof_indiv, disp_pof_indiv:=
          i.disp_uc,
        on = "cod_uc"]



morador[, disp_total:= sum(disp_pof, disp_pof2 , disp_pof_indiv, na.rm = TRUE),
        by = cod_inf]

morador[, disp_pof := NULL][,
                            disp_pof2 := NULL][,
                                               disp_pof_indiv:= NULL]

#fazendo o dispendio domiciliar equivalente

morador[, disp_uc_eq:= 
          disp_total/sqrt(n)]


#correção entre renda disponível e dispêndio 
cor(morador[!is.na(PC_RENDA_DISP) & !is.na(disp_uc_eq),
            PC_RENDA_DISP*12],
    morador[!is.na(PC_RENDA_DISP) & !is.na(disp_uc_eq),
            disp_uc_eq])

############## Aplicando o choque de preços ##################




#data.table pof tem só alimentos



#paridade mip e quadros pof

quadros_pof <- fread("paridade pof.csv", encoding = "UTF-8", colClasses = c(mip_cd = "character"))


quadros_pof[, q1:= as.character(q1)][,
                                     q2:= as.character(q2)][,
                                     q3:= as.character(q3)][,
                                     q4:= as.character(q4)][,
                                     q5:= as.character(q5)][,
                                     QUADRO:=NULL]

quadros_pof[ , mip_cd:= str_pad(mip_cd, width = 4,
                 side = "left",
                 pad = "0")]


quadros_pof <- data.table(pivot_longer(quadros_pof, cols = 4:ncol(quadros_pof),
                                    values_to = "quadro", values_drop_na = TRUE))

quadros_pof<- quadros_pof[mip_setores,
                         p:= i.p, on = "mip_nm"]


##Atribuindo um único coeficiente de preços por quadro pof
##aplicando o efeito do p de cada setor mip
##proporcionalmente ao produto total de cada setor mip
##pareado com o mesmo quadro pof


quadros_pof[mip_setores, DA:=
              i.DA, on = "mip_cd"]

quadros_pof[, p_quadro := weighted.mean(x = p, w = DA)
            , by = quadro]

#quadros_pof[, DA_total:=
  #            sum(DA, na.rm = TRUE),
   #           by = quadro ]

#quadros_pof[, DA_relativa:= 
 #             DA/DA_total]

#quadros_pof[, p_quadro:= p*(DA_relativa)]

#quadros_pof[, p_quadro:= 
  #            sum(p_quadro, na.rm = TRUE),
   #         by = quadro]

#quadros_pof[p_quadro == 0, p_quadro:= 1]

##incluir também um fator de emissões por quadro,
##para avaliar as emissões por quantil, posteriormente

#primeiramente, associando os quadros da pof aos
#fatores de emissões mip

quadros_pof[mip_setores,
            ft_emit:= i.mip_ft_emit,
            on = "mip_cd"]

quadros_pof[, ft_emit:= weighted.mean(x = ft_emit,
                                      w = DA, na.rm = TRUE),
            by = quadro]




###############PRÓXIMO PASSO: JOINAR POFS com QUADROS_POF


outros_serv <- c(22,45,50)
outros_prod <- c(13, 16, 39, 43)


pofs <- list(pof,pof2,pof_indiv)

p_pof <- function(x) {setDT(x)
  
                      x[, quadro:=
                          as.character(quadro)]
  
                      x[quadros_pof, p_quadro:=
                          i.p_quadro, on = "quadro"]
                
                      #tratando os quadros 63 a 69 como um único quadro
                      #(alimentos e bebidas)
                      
                      x[quadro >= 63 & quadro <= 69,
                                p_quadro := unique(quadros_pof[quadro == 63,
                                                               p_quadro])]
                      
                      #fazendo o caso especial dos combustíveis
                      
                      x[cd_produto >= "2301401" & cd_produto <= "2301701",
                                                p_quadro := unique(quadros_pof[quadro == 2301701,
                                                       p_quadro])]
                      
                      #serviços não atribuídos a mip específico
                      x[quadro %in% outros_serv, p_quadro:= unique(quadros_pof[quadro == 11111,
                                                                               p_quadro])]
                      
                      #produtos não atribuídos a mip específico
                      x[quadro %in% outros_prod, p_quadro:= unique(quadros_pof[quadro == 99999,
                                                                               p_quadro])]
                      
}

pofs<- map(.x = pofs,
           .f = p_pof)


#aplicando o choque de preços

tax_emit <- function(x) {
  
    setDT(x)
  
    x[, disp_tax:= V8000*p_quadro][,
                                   disp_tax_defla:= vlr_defla*p_quadro]
}

pofs<- map(.x = pofs,
           .f = tax_emit)


#aplicando os fatores de emissão
#ao consumo das famílias

ft_emit_pof <- function(x) {setDT(x)
  
  x[, quadro:=
      as.character(quadro)]
  
  x[quadros_pof, ft_emit:=
      i.ft_emit, on = "quadro"]
  
  #tratando os quadros 63 a 69 como um único quadro
  #(alimentos e bebidas)
  
  x[quadro >= 63 & quadro <= 69,
    ft_emit := unique(quadros_pof[quadro == 63,
                                   ft_emit])]
  
  #fazendo o caso especial dos combustíveis
  
  x[cd_produto >= "2301401" & cd_produto <= "2301701",
    ft_emit := unique(quadros_pof[quadro == 2301701,
                                   ft_emit])]
  
  #serviços não atribuídos a mip específico
  x[quadro %in% outros_serv, ft_emit:= unique(quadros_pof[quadro == 11111,
                                                           ft_emit])]
  
  #produtos não atribuídos a mip específico
  x[quadro %in% outros_prod, ft_emit:= unique(quadros_pof[quadro == 99999,
                                                           ft_emit])]
 
  #emissões por compra
  x[, emit_compra:= ft_emit*vlr_defla]
  
  #emissões por uc
  x[, emit_uc:= sum(emit_compra, na.rm= TRUE),
    by = cod_uc]
}




pofs<- map(.x = pofs,
           .f = ft_emit_pof)


####

dispendio_uc_tax<- function(x) {
  
  x[, disp_uc_tax:= 
      sum(disp_tax, na.rm = TRUE),
    by = cod_uc]
  
  x[, disp_uc_tax_defla:= 
      sum(disp_tax_defla, na.rm = TRUE),
    by = cod_uc]

  }



pofs<- map(.x = pofs,
           .f= dispendio_uc_tax)

pof<- pofs[[1]]
pof2<- pofs[[2]]
pof_indiv<- pofs[[3]]

## juntando os valores do dispendio dps do imposto
#das diferentes planilhas

morador[pof, disp_pof_tax:=
          i.disp_uc_tax_defla,
        on = "cod_uc"]

morador[pof2, disp_pof2_tax:=
          i.disp_uc_tax_defla,
        on = "cod_uc"]

morador[pof_indiv, disp_pof_indiv_tax:=
          i.disp_uc_tax_defla,
        on = "cod_uc"]

morador[, disp_total_tax:=
          sum(disp_pof_tax, disp_pof2_tax, disp_pof_indiv_tax, 
              na.rm = TRUE),
        by = cod_inf]

morador[, disp_pof_tax:= NULL][,
                            disp_pof2_tax := NULL][,
                                               disp_pof_indiv_tax:= NULL]


morador[, disp_uc_eq_tax:= 
                    disp_total_tax/sqrt(n)]

morador[, impact_tax:= disp_uc_eq - disp_uc_eq_tax]

morador[, welfare_chng:= impact_tax/disp_uc_eq]

#juntando as emissões das diferentes pofs

morador[pof,
        emit_uc_pof:= i.emit_uc,
        on = "cod_uc"]

morador[pof2,
        emit_uc_pof2:= i.emit_uc,
        on = "cod_uc"]

morador[pof_indiv,
        emit_uc_pof_indiv:= i.emit_uc,
        on = "cod_uc"]

morador[,emit_uc:= 
         sum(emit_uc_pof,emit_uc_pof2,emit_uc_pof_indiv, na.rm = TRUE),
        by = cod_inf]

morador[, emit_indiv:= emit_uc/n]

morador[, ft_emit_uc:= emit_uc/disp_total]


morador[,emit_uc_pof:NULL][,
          emit_uc_pof2:= NULL][,
            emit_uc_pof_indiv:= NULL]

########## Impacto do Imposto sobre Produção e Emissões ###########

mip_setores[, DA_tax := DA/p]

mip_setores[, mip_emit_tax := DA_tax* mip_ft_emit]

mip_setores[, var_emit:= mip_emit_tax - mip_emit]

mip_setores[, var_emit_rel:= var_emit/mip_emit]

mip_setores[, var_prod:= DA_tax - DA]

mip_setores[, var_prod_rel:= var_prod/DA]

#rEDUÇÃO TOTAL DA PRODUÇÃO
sum(mip_setores$DA_tax - mip_setores$DA )

sum(mip_setores$DA_tax - mip_setores$DA )/sum(mip_setores$DA)

#redução total das emissões
sum(mip_setores$mip_emit_tax - mip_setores$mip_emit )

sum(mip_setores$mip_emit_tax - mip_setores$mip_emit )/sum(mip_setores$mip_emit)

#Salário
salarios<- fread("salarios.csv", header = TRUE, colClasses = c(mip_cd = "character"))

mip_setores[salarios,
            salario:= i.salario,
            on = "mip_cd"]

mip_setores[salarios,
            vagas:= i.vagas,
            on = "mip_cd"]

mip_setores[, vagas_tax:= vagas*(1+var_emit_rel)]
mip_setores[, salario_tax:= salario*(1+var_emit_rel)]

mip_setores[, var_vagas:= vagas_tax - vagas]
#variação total no emprego

(sum(mip_setores$vagas_tax, na.rm = TRUE) - sum(mip_setores$vagas, na.rm = TRUE))/sum(mip_setores$vagas, na.rm = TRUE )


#variação na massa salarial

(sum(mip_setores$salario_tax, na.rm = TRUE) - sum(mip_setores$salario, na.rm = TRUE))/sum(mip_setores$salario, na.rm = TRUE )

######### Análise distributiva ###########

morador[, quantil:= cut(morador$disp_uc_eq ,
                        breaks =      quantile(morador$disp_uc_eq,
                                          probs = seq(0,1,0.01)),
                        labels=1:100, include.lowest=TRUE)]


morador[, welfare_chng_quant:= 
          mean(welfare_chng, na.rm = TRUE),
          by = quantil]

morador[, welfare_chng_quant_var:= 
          var(welfare_chng, na.rm = TRUE),
        by = quantil]

morador[, disp_uc_eq_quant:= 
          mean(disp_uc_eq, na.rm = TRUE),
        by = quantil]

morador[, impact_tax_quant:= 
          mean(impact_tax, na.rm = TRUE),
        by = quantil]

morador[, emit_indiv_quant:=
          mean(emit_indiv, na.rm = TRUE),
        by = quantil]

morador[, ft_emit_uc_quant:=
          mean(ft_emit_uc, na.rm = TRUE),
        by = quantil]


morador[, emit_uc_quant:=
          mean(emit_uc, na.rm = TRUE),
        by = quantil]

#arrecadacao baseada no produto novo

mip_setores[,arrecadacao:= (DA_tax*p) - DA_tax]

arrecada <- sum(mip_setores$arrecadacao)*1e+06

#taxa efetiva:
base<- sum(mip_setores$DA_tax)*1e+06


arrecada/base


#total ICMS 2018 478826044584.61

#arrecadacao sobre a base da pof

#arrecadação baseado nos níveis de consumo originais
arrecada_POF<- sum(pof$disp_tax_defla - pof$vlr_defla, na.rm = TRUE) +
  sum(pof2$disp_tax_defla - pof2$vlr_defla, na.rm = TRUE) +
  sum(pof_indiv$disp_tax_defla - pof_indiv$vlr_defla, na.rm = TRUE)

morador[, dividendo:= impact_tax + (arrecada_POF/nrow(morador))]

morador[, impact_dividendo_quant:= mean(dividendo, na.rm = TRUE), 
        by = quantil]

morador[, welfare_chng_dividendo:= dividendo/disp_uc_eq ]

morador[!is.infinite(welfare_chng_dividendo),
        welfare_chng_dividendo_quant:= mean(welfare_chng_dividendo,
                                              na.rm = TRUE),
        by = quantil]

#parcela de beneficiados por quantil

morador[, share_benef_quant:= length(which(dividendo > 0))/nrow(.SD[,]),
       by = quantil]

#Mudança nas emissões



####################PLOTS #############
#disp_uc_eq_quant
ggplot(morador[!duplicated(morador, by = c("quantil")),], aes(y = disp_uc_eq_quant, x = quantil)) +
  geom_col() +
  scale_x_discrete(breaks = seq(0, 100, by = 10)) +
  ylab("Dispendio domiciliar equivalente (R$)") +
  xlab("Quantis de renda (proxy)")


#welfare_chng_quant
ggplot(morador[!duplicated(morador, by = c("quantil")),], aes(y = welfare_chng_quant*100, x = quantil)) +
  geom_col() +
  scale_x_discrete(breaks = seq(0, 100, by = 10)) +
  ylab("Mudança no bem-estar (%)") +
  xlab("Quantis de renda (proxy)")


ggplot(morador[!duplicated(morador, by = c("quantil")),], aes(y = welfare_chng_quant_var, x = quantil)) +
  geom_col() +
  scale_x_discrete(breaks = seq(0, 100, by = 10)) +
  ylab("Mudança no bem-estar (variância)") +
  xlab("Quantis de renda (proxy)")


ggplot(morador[!duplicated(morador, by = c("quantil")),], aes(y = ft_emit_uc_quant, x = quantil)) +
  geom_col() +
  scale_x_discrete(breaks = seq(0, 100, by = 10)) +
  ylab("Intensidade de emissões de CO2e (Kg/R$)") +
  xlab("Quantis de renda (proxy)")

ggplot(morador[!duplicated(morador, by = c("quantil")),], aes(y = emit_indiv_quant, x = quantil)) +
  geom_col() +
  scale_x_discrete(breaks = seq(0, 100, by = 10)) +
  ylab("Emissões totais de CO2e (kg)") +
  xlab("Quantis de renda (proxy)")

ggplot(morador[!duplicated(morador, by = c("quantil")),], aes(y = emit_uc_quant, x = quantil)) +
  geom_col() +
  scale_x_discrete(breaks = seq(0, 100, by = 10)) +
  ylab("Emissões domiciliares totais de CO2e (kg)") +
  xlab("Quantis de renda (proxy)")


#variação na renda pós dividendo
ggplot(morador[!duplicated(morador, by = c("quantil")),], aes(y = impact_dividendo_quant, x = quantil)) +
  geom_col() +
  scale_x_discrete(breaks = seq(0, 100, by = 10)) +
  ylab("Variação no bem-estar após dividendo de carbono (R$)") +
  xlab("Quantis de renda (proxy)")


#variação no bem estar pós dividendo (%)
ggplot(morador[!duplicated(morador, by = c("quantil")),], aes(y = impact_dividendo_quant*100/disp_uc_eq_quant, x = quantil)) +
  geom_col() +
  scale_x_discrete(breaks = seq(0, 100, by = 10)) +
  ylab("Variação no bem-estar após dividendo de carbono (%)") +
  xlab("Quantis de renda (proxy)")

ggplot(morador[!duplicated(morador, by = c("quantil")),], aes(y = welfare_chng_dividendo_quant*100, x = quantil)) +
  geom_col() +
  scale_x_discrete(breaks = seq(0, 100, by = 10)) +
  ylab("Variação no bem-estar após dividendo de carbono (%)") +
  xlab("Quantis de renda (proxy)")


ggplot(morador[!duplicated(morador, by = c("quantil")),], aes(y = share_benef_quant, x = quantil)) +
  geom_col() +
  scale_x_discrete(breaks = seq(0, 100, by = 10)) +
  ylab("Parcela do quantil beneficiada pelo imposto + dividendo") +
  xlab("Quantis de renda (proxy)")

#Redução nas emissões
to_plot<- data.table(mip_setores[order(-mip_emit),.(mip_nm, DA,  DA_tax)])

melted<- melt(to_plot)


ggplot(melted,aes(x= mip_nm ,y=value, fill=variable)) + 
  geom_bar(stat="identity",position = "dodge") + 
  labs(y = "Produto (Milhões de R$)", x = "Setor MIP", color = "Produto\n") +
  scale_color_manual(labels = c("Antes do Imposto", "Com Imposto"), values = c("green", "red")) +
  coord_flip() +
  guides(color=guide_legend("Produto"))
  
  scale_color_hue(labels = c("Antes do Imposto", "Com Imposto"))
  
  
plot(morador$welfare_chng_quant, morador$quantil)

moraduhh[,or1:= order(morador$PC_RENDA_DISP)][,or2:= order(morador$disp_uc_eq)]
        
#plots

ggplot()

###### tabela multi preços #####

#parcela da arrecadação necessária para compensar metade mais pobre

-sum(morador[as.numeric(quantil) < 50 & !is.infinite(impact_tax),
  (disp_total/n) - (disp_total_tax/n)], na.rm = TRUE)/
  arrecada_POF

#arrecadação em R$
sum((phi) * (t(e)/1000) * mip_setores$DA_tax*(1000000))

#decil

morador[, decil:= cut(morador$disp_uc_eq ,
                        breaks =      quantile(morador$disp_uc_eq,
                                               probs = seq(0,1,0.1)),
                        labels=1:10, include.lowest=TRUE)]

#variação bem estar imposto

morador[, welfare_chng_dec:= 
          mean(welfare_chng, na.rm = TRUE),
        by = decil]


#variação welfare dividendo
morador[!is.infinite(welfare_chng_dividendo),
        welfare_chng_dividendo_dec:= mean(welfare_chng_dividendo,
                                            na.rm = TRUE),
        by = decil]

#valor do dividendo
arrecada_POF/nrow(morador)

#parcela beneficiada decil

morador[, share_benef_dec:= length(which(dividendo > 0))/nrow(.SD[,]),
       by = decil]


morador_publi<- morador[!duplicated(morador, by = c("decil")),
                 .(decil, welfare_chng_dec, 
                   welfare_chng_dividendo_dec,
                   share_benef_dec)]

morador_publi<- morador_publi[order(decil),]


write.xlsx(morador_publi, "50 reais 220325.xlsx")

#variação no produto
sum(mip_setores$DA_tax - mip_setores$DA )

sum(mip_setores$DA_tax - mip_setores$DA )/sum(mip_setores$DA)

#redução total das emissões
sum(mip_setores$mip_emit_tax - mip_setores$mip_emit )

sum(mip_setores$mip_emit_tax - mip_setores$mip_emit )/sum(mip_setores$mip_emit)

#valor do dividendo
arrecada_POF/nrow(morador)

#taxa efetiva
sum((phi) * (t(e)/1000) * mip_setores$DA_tax*(1000000), na.rm = TRUE)/
  sum(mip_setores$DA_tax*(1000000), na.rm = TRUE)

#Intensidade de emissões da economia
sum(mip_setores$mip_emit_tax, na.rm = TRUE)/sum(mip_setores$DA_tax, na.rm = TRUE)
