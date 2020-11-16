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

covidmta = read.csv2("F:/Documentos/FGV/AED/casos mta.csv", encoding = 'UTF-8')
names(covidmta)[1] = 'Data'
covidmta$Data = dmy(covidmta$Data)
Odia = max(covidmta$Data)
CasosDoDia = covidmta$Casos[covidmta$Data == Odia]
ObitosDoDia = covidmta$Obitos[covidmta$Data == Odia]


dadosRural = read.csv2("F:/Documentos/FGV/AED//CasosRural.csv", encoding = 'UTF-8')
names(dadosRural)[1] = 'Comunidade'
coordenandasRural = read.csv2("F:/Documentos/FGV/AED/coordrural.csv", encoding = 'UTF-8')
names(coordenandasRural)[1] = 'Comunidade'


dadosRural$Data = dmy(dadosRural$Data)
Odia = max(dadosRural$Data)

dadoshoje = dadosRural %>% filter(Data == Odia)

barras = dadoshoje %>%
  #filter(Casos>2)%>%
  ggplot(color='black',
         aes(y=reorder(Comunidade, Casos), 
             x= Casos,
             fill= Casos))+
  geom_bar(stat='identity')+
  scale_fill_gradientn(
    colors = c('yellow','red','black','black'),
    limits = c(0,200))+
  ylab(NULL)+
  xlab(NULL)+
  ggtitle('Número de Casos - Zona Rural')+
  geom_text(aes(label = Casos), nudge_x=1,size=3.5)+
  theme(
    axis.title.x = element_blank(),
    #axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    #axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    #axis.text.y = element_blank()
    legend.key = element_blank()
  )
#+
#  geom_label(aes(label=Bairro),nudge_x = 10,fill='white')
#barras

num_comunidades = length(dadoshoje$Comunidade)

ruralmap = left_join(dadosRural,coordenandasRural, by='Comunidade') 

plot1 = ruralmap %>% 
  filter(Data==Odia)

#rural0= openmap(
 # c(-1.232,-53.9072),
  #c(-2.307,-54.5609),type="osm-transport")

rural = openproj(rural0)

#autoplot.OpenStreetMap(rural)

plot = autoplot.OpenStreetMap(rural)+
  geom_point(data = plot1,inherit.aes = FALSE,
             aes(size=Casos,color=Casos,y=Latitude,x=Longitude),
             show.legend = FALSE)+
  scale_size(limits = c(0,200),range = c(0,10))+
  scale_color_gradientn(colors = c('yellow','red','black','black'),
                        limits = c(0,200))+
  geom_label_repel(data = plot1, 
                   aes(label= Comunidade,x=Longitude,y=Latitude),
                   force=20,
                   point.padding = 0.2,
                   label.padding =0.1,
                   size=2.5,
                   box.padding = unit(0.5,'lines'))+
  ggtitle(paste('Panorama de', Odia, '- Monte Alegre-PA'))+
  geom_label(aes(y=-1.232, x = -54.9072,label=paste('Casos: 
',CasosDoDia,'
Óbitos: 
',ObitosDoDia)),nudge_x =0.41,nudge_y = -0.085)+
  #xlim(-54.0909,-54.0549)+
  #ylim(-2.0138,-1.96881)#+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        # axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'none',
      legend.box = 'none')

#plot
#plot

p = ggarrange(plot,
             barras,
             common.legend = TRUE,
             legend = 'left',
             ncol = 2)
p

ggsave(paste('Panorama-Rural',Odia,'.png'),p,dpi =600,
       path ="F:/Documentos/FGV/AED",
       width = 8.8,height = num_comunidades*0.121)
