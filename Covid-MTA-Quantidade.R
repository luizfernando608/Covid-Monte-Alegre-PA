library(lubridate)
library(ggplot2)
library(dplyr)

covidmta = read.csv2("F:/Documentos/Estudos/covid MTA/casos mta.csv", encoding = 'UTF-8')
names(covidmta)[1] = 'Data'

covidmta$Data = dmy(covidmta$Data)
Odia = max(covidmta$Data)

taxaOdia = covidmta$TaxaDiaria[covidmta$Data == Odia]

ggplot(covidmta,aes(x=Data,y=Casos))+
  geom_smooth()#+
  geom_point()

curva = covidmta %>% 
  ggplot()+
  ggtitle(paste('Taxa Diária de Casos -', Odia,' - Monte Alegre-PA'))+
  geom_bar(aes(x=Data,y=TaxaDiaria),stat = 'identity', fill='blue',alpha =0.3)+
  geom_line(aes(x=Data,y=Media),color='red',size = 1.5, show.legend = TRUE)+
  ylab('Taxa de Casos')#+
  #geom_smooth(aes(x=Data,y=TaxaDiaria))
  #theme(axis.text.y=element_blank(),
  #      axis.ticks.y = element_blank())#+
  #geom_line()
  #geom_label(x=Odia,y=taxaOdia,label=taxaOdia)
  #geom_line()
  #geom_bar(stat='identity')



curva


casos_ativos = covidmta %>% 
  ggplot()+
  ggtitle(paste('Casos ativos -', Odia,' - Monte Alegre-PA'))+
  geom_line(aes(x=Data,y=Media_Ativos),color='red',size = 1.5, show.legend = FALSE)

casos_ativos



ggsave(paste('Curva',Odia,'.png'),
       curva,
       dpi = 600,
       path ="F:/Documentos/Estudos/covid MTA",width = length(covidmta$Data)*0.1, height = 5.64)

ggsave(paste('Ativos',Odia,'.png'),
       casos_ativos,
       dpi = 600,
       path ="F:/Documentos/Estudos/covid MTA",width = length(covidmta$Data)*0.1, height = 5.64)





#covidmta %>% 
#  ggplot(aes(x=Data,MediaAcele))+
#  geom_smooth()+
#  geom_line()
  
#covidmta$MediaAcele

#covidgroup = covidmta %>% group_by(semana)%>% 
#  summarize(Casos=sum(Casos),TaxaDiaria=mean(TaxaDiaria))


#curvasemana = covidgroup %>%
#  ggplot(aes(x=semana,y=TaxaDiaria))+
#  geom_smooth(se=F)+
#  geom_line()

#curvasemana


#ggplot(covidmta,aes(x=Data,y=Aceleracao))+
  #geom_point()
  #geom_line()+
 # geom_smooth(se=FALSE)

#ggplot(covidmta,aes(x=Data,y=Ativos))+
  #geom_point()
 # geom_line()+
  #geom_smooth()

#by_week = covidmta %>% 
 # group_by(semana)%>%
  #summarize(
   # TaxaCasos = sum(TaxaCasos)
  #)

#by_week %>% ggplot(aes(semana,TaxaCasos))+
#  geom_smooth()
