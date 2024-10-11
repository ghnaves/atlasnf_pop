library(tidyverse)
library(readxl)
library(writexl)

dic_cadescolas<-read_excel('Censo Escolar 2021/Anexos/ANEXO I - Dicionario de Dados/dicionario_dados_educacao_basica.xlsx',
                           range='A10:F389',col_names = c('N','Var','Desc','Tipo','Tam','Categorias'))%>%
  filter(!is.na(Var))

var_cadescolas<-dic_cadescolas%>%
  filter(N %in% 
    c(1,3,4,6,7,8,9,10,12,13:27,64,80,104:106,110:111,116,119,147:149,
      181,183,185,188,193:194,
      299,300,303,306,309,312:314,
      339,340,343,346,349,352:354,
      355,356,359,362,365,368:370))

cadescolas<-read_delim('Censo Escolar 2021/dados/microdados_ed_basica_2021.csv',
                            delim=';',locale=locale(encoding='latin1'))%>%
  filter(NO_MESORREGIAO=='Norte Fluminense')%>%
  select(all_of(var_cadescolas$Var))%>%
  filter(TP_SITUACAO_FUNCIONAMENTO == 1)%>%
  mutate(PROP_SALAS_CLIMATIZADAS=100*QT_SALAS_UTILIZA_CLIMATIZADAS/QT_SALAS_UTILIZADAS,
          PROP_SALAS_ACESSIVEIS=100*QT_SALAS_UTILIZADAS_ACESSIVEIS/QT_SALAS_UTILIZADAS,
         QT_COMP_TABLET=
           if_else(QT_DESKTOP_ALUNO==88888,0,QT_DESKTOP_ALUNO)+
           if_else(QT_COMP_PORTATIL_ALUNO==88888,0,QT_COMP_PORTATIL_ALUNO)+
           if_else(QT_TABLET_ALUNO==88888,0,QT_TABLET_ALUNO))%>%
  select(-c(QT_SALAS_UTILIZA_CLIMATIZADAS,QT_SALAS_UTILIZADAS_ACESSIVEIS,
            QT_DESKTOP_ALUNO,QT_COMP_PORTATIL_ALUNO,
            QT_TABLET_ALUNO,TP_SITUACAO_FUNCIONAMENTO))%>%
  mutate(endereco=if_else(is.na(NU_ENDERECO),DS_ENDERECO,
                          paste(DS_ENDERECO,NU_ENDERECO,sep=', ')))%>%
  mutate(endereco=str_replace_all(endereco,'S/NO',''))%>%
  mutate(endereco=str_replace_all(endereco,'S/N',''))%>%
  mutate(endereco=str_replace_all(endereco,'SN',''))%>%
  mutate(endereco=str_replace_all(endereco,'º',''))
  


library(tidygeocoder)

geo_loc<-cadescolas%>%
  distinct(endereco,NO_MUNICIPIO,NO_UF,DS_ENDERECO,NU_ENDERECO,DS_COMPLEMENTO,NO_BAIRRO,CO_CEP,.keep_all = T)%>%
  rowid_to_column(var='id_tidygeocoder_osm')

temp<-geo(street=geo_loc$endereco,
          city=geo_loc$NO_MUNICIPIO,
          state=geo_loc$NO_UF,
          country =rep('Brazil',nrow(geo_loc)),
          postalcode = geo_loc$CO_CEP,
          method='osm',
          #api_url='https://nominatim.openstreetmap.org',
          progress_bar=TRUE)

cadescolas_geo<-cadescolas%>%
  inner_join(temp,by=c('endereco'='street',
                       'NO_MUNICIPIO'='city',
                       'NO_UF'='state',
                       'CO_CEP'='postalcode'))


dic_ext<-dic_cadescolas%>%
  filter(Var %in% names(cadescolas)[1:59])%>%
  rbind(tibble(
    N=rep(NA,5),
    Var=c('PROP_SALAS_CLIMATIZADAS',
          'QT_SALAS_UTILIZADAS_ACESSIVEIS',
          'QT_COMP_TABLET','lat','long'),
    Desc=c('Proporção de salas climatizadas (%)',
           'Proporção de salas acessíveis (%)',
           'Quantidade de computadores ou tablets para uso dos alunos',
           'Latitude', 'Longitude'),
    Tipo=rep('Num',5),
    Tam=rep(NA,5),
    Categorias=rep(NA,5)))


write_xlsx(cadescolas,'cadescolas (Censo Escolar).xlsx')
write_xlsx(dic_ext,'dic_cadescolas (Censo Escolar).xlsx')