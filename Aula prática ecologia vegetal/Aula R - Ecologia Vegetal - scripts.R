##### SCRIPT AULA R ECOLOGIA VEGETAL #####


#INSTALANDO UM PACOTE
install.packages("vegan")
#ABRINDO UM PACOTE
library(vegan)

#SELECIONANDO UM DIRETÓRIO DE TRABALHO:
setwd("C:/Users/amand/OneDrive/Área de Trabalho/AulaR")
#CONFERINDO O DIRETÓRIO
getwd()

#IMPORTANDO UMA TABELA NO R:
tabela <- read.table("Tabela_aula.txt", header=T)
tabela
View (tabela)

#SELECIONANDO COLUNAS DA TABELA
altura<-tabela$ALTURA
altura
altura2<-tabela[,4]
altura2

#SELECIONANDO LINHAS DA TABELA [SÓ INDIVÍDUOS DA SP A RESTINGA]
restinga<-tabela[1:25,]
restinga
m_atl<-tabela[26:50,]
m_atl

spp_restinga <- subset(tabela, Populacao == "Restinga")
spp_restinga

### ANÁLISE EXPLORATÓRIA DOS DADOS ###

#Máximo, mínimo, média e mediana da Altura da sp A na Restinga
summary(tabela[1:25,4])

#Variância da Altura da sp A na Restinga
var(tabela[1:25,4])

###FUNÇÕES###
Media_Altura_R <- mean(restinga$ALTURA)
Media_Altura_R
Media_Altura_M <- mean(m_atl$ALTURA)
Media_Altura_M

###GRÁFICOS###

#MONTANDO UM GRÁFICO DE DISPERSÃO ALTURA X DAP
plot(tabela$DAP,tabela$ALTURA)

#PARA POR UM TÍTULO NO GRÁFICO E MUDAR O NOME DOS EIXOS
plot(tabela$DAP,tabela$ALTURA,ylab="Altura",xlab="Diâmetro na altura do peito", main="Gráfico de dispersão")

#MONTANDO UM BOXPLOT ALTURAS RESTINGA X MATA ATLÂNTICA
boxplot(ALTURA~Populacao,data=tabela)


#MONTANDO UM HISTOGRAMA PARA ALTURA
hist(tabela$ALTURA)

#MONTANDO UM GRÁFICO DE BARRAS ALTURAS RESTINGA X MATA ATLÂNTICA SP A
MEDIAS_ALTURA<-c(Media_Altura_R,Media_Altura_M)
barplot(MEDIAS_ALTURA,names.arg=c("Restinga","Mata Atlântica"),ylab="Altura")


#normalidade e distribuição dos dados
hist(tabela[1:25,4])
shapiro.test(tabela[1:25,4])    # se p>0,05= distribuicao normal; 


#transformando dados
tabela_log<-log10(tabela[1:25,4])


### ANÁLISES ESTATÍSTICAS ###

#Regressão [o conteudo de água no solo afeta a altura dessas espécies?]
regressao<-lm(tabela$ALTURA~tabela$Conteudo_agua_solo)
summary(regressao)
plot(tabela$ALTURA~tabela$Conteudo_agua_solo)
abline(regressao)


#Correlação [existe relação entre dap e altura?]
cor.test(tabela$ALTURA,tabela$DAP)


#Anova [existe diferença entre altura das espécies A, B e C?]
anova<-aov(tabela$ALTURA~tabela$Especie)
summary(anova)
TukeyHSD(anova)   #rodar se houver diferenca significativa na anova
boxplot(ALTURA~Especie,data=tabela, notch=TRUE)

#Teste-t [existe diferença entre altura entre restinga e mata?]
t.test(tabela$ALTURA~tabela$Populacao)
boxplot(ALTURA~Populacao,data=tabela, notch=TRUE)
