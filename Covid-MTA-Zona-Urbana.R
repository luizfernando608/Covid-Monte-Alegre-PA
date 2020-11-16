library(ggpubr)
library(ggplot2)
library(lubridate)
library(dplyr)
#library(gganimate)
library(plotly)
#library(ggimage)
library(ggplot2)
library(OpenStreetMap)
library(ggrepel)

covidmta = read.csv2("F:/Documentos/Estudos/covid MTA/casos mta.csv", encoding = 'UTF-8')
names(covidmta)[1] = 'Data'
covidmta$Data = dmy(covidmta$Data)
Odia = max(covidmta$Data)
CasosDoDia = covidmta$Casos[covidmta$Data == Odia]
ObitosDoDia = covidmta$Obitos[covidmta$Data == Odia]


dados = read.csv2("F:/Documentos/Estudos/covid MTA/CovidMTAbairro.csv", encoding = 'UTF-8')
names(dados)[1] = 'Bairro'
coordenandas = read.csv2("F:/Documentos/Estudos/covid MTA/cidadecoord.csv", encoding = 'UTF-8')
names(coordenandas)[1] = 'Bairro'


dados = dados%>%mutate(taxaCasos = NA)


for (bairro in levels(dados$Bairro)){
  contador = 0
  taxa = c()
  condicao = bairro == dados$Bairro
  
  for (caso in dados$Casos[condicao]){
    contador = contador + 1
    #print(caso)
    if (contador == 1){
      caso_anterior = 0
      #print(caso_anterior)
    }
    
    caso_atual = caso
    
    taxa = c(taxa, caso_atual - caso_anterior)
    print(taxa)
    caso_anterior = caso_atual
  }
  
  dados$taxaCasos[condicao] = taxa
  
}

dados$Data = dmy(dados$Data)
Odia = max(dados$Data)


barras =dados %>% filter(Data == Odia)%>%
  ggplot(color='black',
       aes(y=reorder(Bairro, Casos), 
       x= Casos,
       fill= Casos))+
  geom_bar(stat='identity',show.legend = TRUE)+
  scale_fill_gradientn(
    colors = c('yellow','orange','red','red'),
    limits = c(0,100))+
  ylab(NULL)+
  xlab(NULL)+
  ggtitle(paste('Número de Casos - Zona Urbana'))+
  geom_text(aes(label = Casos), nudge_x=5)+
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank())
  #+
#  geom_label(aes(label=Bairro),nudge_x = 10,fill='white')
#barras


mtamap = left_join(dados,coordenandas, by='Bairro') 

plot1 = mtamap %>% 
  filter(Data==Odia) 

#mta0= openmap(c(-1.975,-54.0901),c(-2.0129,-54.059),type="esri",zoom=14)

mta = mta0


mta2 = openproj(mta)


plot2 = 
  autoplot.OpenStreetMap(mta2)+
  geom_point(data = plot1,inherit.aes = FALSE,
             aes(size=Casos,color=Casos,y=Latitude,x=Longitude),
             show.legend = FALSE)+
  scale_size(limits = c(0,200),range = c(0,15))+
  geom_label_repel(data = plot1, 
                  aes(label= Bairro,x=Longitude,y=Latitude),
                  force=20,
                  point.padding = 0.3,
                  label.padding =0.1,
                  size=2.5,
                  box.padding = unit(0.5,'lines')
                  )+
  scale_color_gradientn(
    colors = c('yellow','orange','red','red'),
    limits = c(0,100))+
  ggtitle(paste('Casos entre Agosto e', Odia,'- Monte Alegre-PA'))+
  geom_label(aes(y=-1.975, x = -54.0901,label=paste('Casos: 
',CasosDoDia,'
Óbitos: 
',ObitosDoDia)),nudge_x =0.0021,nudge_y = -0.0032)+
  #xlim(-54.0909,-54.0549)+
  #ylim(-2.0138,-1.96881)#+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
  
#plot2


pCIDADE= ggarrange(plot2,
          barras,
          common.legend = TRUE,
          ncol = 2,
          legend = 'left')
pCIDADE

ggsave(paste('Panorama',Odia,'.png'),
       pCIDADE,dpi =600,
       path ="F:/Documentos/Estudos/covid MTA/", 
       width = 9.9,height =5.8)
 