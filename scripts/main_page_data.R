library('tidyverse')
library('openxlsx')
library('readxl')
library('kableExtra')

#Inicialização arquivos auxiliares
dtb<-openXL('data/raw/RELATORIO_DTB_BRASIL_MUNICIPIO.xls',
              col_names=c('uf_co','uf_no','inter_co','inter_no','imed_co','imed_nm',
                          'meso_co','meso_no','micro_co','micro_no','munic_co5','munic_co','munic_no'),
              col_types=c(rep(c('numeric','text'),5),'numeric','numeric','text'),skip=1)

dtb_nf<-dtb%>%
  filter(meso_no=='Norte Fluminense')

write_rds(dtb_nf,'data/processed/dtb_nf.rds')
write_rds(dtb,'data/processed/dtb.rds')

## Populacao Absoluta
temp_popserie<-read_xlsx('data/raw/tabela202.xlsx',range='A8:R5603',col_names = int2col(1:18),
                         col_types=c('text','numeric','text',rep('numeric',15)))%>%
  filter(A=='MU')%>%
  inner_join(dtb_nf,by=c('B'='munic_co'))%>%
  mutate(uf=trunc(B/100000),
         C=str_trim(C))%>%
  rename(munic_co=B,
         total_1970=D,urb_1970=E,rur_1970=F,
         total_1980=G,urb_1980=H,rur_1980=I,
         total_1991=J,urb_1991=K,rur_1991=L,
         total_2000=M,urb_2000=N,rur_2000=O,
         total_2010=P,urb_2010=Q,rur_2010=R)%>%
  pivot_longer(cols=matches('^(total|urb|rur)'),
               names_to='tipo_ano',
               values_to='pop')%>%
  separate(tipo_ano,c('sit','ano'),sep='_')%>%
  select(munic_co,uf,ano,sit,pop)

temp_pop2022<-read_xlsx('data/raw/tabela4709.xlsx',range='A5:D5601',col_names = int2col(1:4),
                        col_types=c('text','numeric','text','numeric'))%>%
  filter(A=='MU')%>%
  inner_join(dtb_nf,by=c('B'='munic_co'))%>%
  mutate(uf=trunc(B/100000),
         C=str_trim(C),
         sit='total',ano=2022)%>%
  rename(munic_co=B,
         pop=D)%>%
  select(uf,munic_co,ano,sit,pop)

pop_munic<-temp_popserie%>%
  rbind(temp_pop2022)

write_rds(pop_munic,'data/processed/pop_munic_nf.rds')

#Crescimento populacional

## IMPORTANTE: a polulação em 1970, totalizada, para o estado do Rio de Janeiro está
### incorreta (arquivo 'Tabela 200.xlsx'). O erro é aparentemente do SIDRA (IBGE).
### A população do estado foi obtida pela soma das populações dos municípios ('tabela 200 (1).xlsx')

temp_200<-read_xlsx('data/sidra.igbe.org.br/tabela200 (1).xlsx',range='A7:R100',
                      col_names = int2col(1:18),
                      col_types=c('text','numeric','text',rep('numeric',15)))%>%
  left_join(dtb,by=c('B'='munic_co'))%>%
  rename('ut_co'='B','ut_no'='C',
         'total1970'='D','total1980'='G','total1991'='J','total2000'='M','total2010'='P',
         'urb1970'='E','urb1980'='H','urb1991'='K','urb2000'='N','urb2010'='Q',
         'rur1970'='F','rur1980'='I','rur1991'='L','rur2000'='O','rur2010'='R')
  

temp_nf<-temp_200 %>%
  filter(meso_no=='Norte Fluminense')%>%
  group_by(meso_no) %>%
  summarise(across(starts_with(c("total", "urb", "rur")), sum, na.rm = TRUE),.groups = 'drop')%>%
  mutate(ut_co=3302)%>%
  select(ut_co,meso_no,total1970:rur2010)%>%
  rename('ut'='meso_no')

temp_rj<-temp_200 %>%
  filter(!(ut_no %in% c("Brasil","Rio de Janeiro")))%>%
  summarise(across(starts_with(c("total", "urb", "rur")), sum, na.rm = TRUE))%>%
  mutate(ut = "Rio de Janeiro",ut_co=33)%>%
  select(ut_co,ut,total1970:rur2010)

temp_munic<-temp_200%>%
  filter(!(ut_no %in% c("Brasil","Rio de Janeiro")))%>%
  mutate(ut = munic_no)%>%
  select(ut_co,ut,total1970:rur2010)
  
temp_ate2010<-temp_200%>%
  filter(ut_no == "Brasil") %>%
  select(ut_co,ut_no,total1970:rur2010)%>%
  rename(ut=ut_no)%>%
  rbind(temp_rj)%>%
  rbind(temp_nf)%>%
  rbind(temp_munic)

temp_4714<-read_xlsx('data/sidra.igbe.org.br/tabela4714.xlsx',range='A5:C98',
                  col_names = c('A', 'B','total2022'),
                  col_types=c('numeric','text','numeric'))%>%
  left_join(dtb,by=c('A'='munic_co'))

temp_nf<-temp_4714 %>%
  filter(meso_no=='Norte Fluminense')%>%
  group_by(meso_no) %>%
  summarise(across(starts_with(c("total", "urb", "rur")), sum, na.rm = TRUE),.groups = 'drop')%>%
  mutate(ut_co=3302)%>%
  select(ut_co,meso_no,total2022)%>%
  rename('ut'='meso_no')

temp_2022<-temp_4714%>%
  mutate(ut = B,ut_co=A)%>%
  select(ut_co,ut,total2022)%>%
  rbind(temp_nf)%>%
  mutate(ordem=case_when(ut_co==3302~3,
                         ut_co==1~1,
                         ut_co==33~2,
                         TRUE~4))%>%
  arrange(ordem)%>%
  select(-ordem,-ut)
  

pop_brrjnf<-temp_ate2010%>%
  inner_join(temp_2022,by='ut_co')
  
write_rds(pop_brrjnf,'data/processed/pop_brrjnf.rds')

growthratio_brrjnf<-pop_brrjnf%>%
  mutate(gr2010_2022=
           1/time_length(interval(dmy('1-8-2010'), dmy('1-8-2022')), "years")*
           log(total2022/total2010),
         gr2000_2010=
           1/time_length(interval(dmy('1-8-2000'), dmy('1-8-2010')), "years")*
           log(total2010/total2000),
         gr1991_2000=
           1/time_length(interval(dmy('1-9-1991'), dmy('1-8-2000')), "years")*
           log(total2000/total1991),
         gr1980_1991=
           1/time_length(interval(dmy('1-9-1980'), dmy('1-9-1991')), "years")*
           log(total1991/total1980),
         gr1970_1980=
           1/time_length(interval(dmy('1-9-1970'), dmy('1-9-1980')), "years")*
           log(total1980/total1970))%>%
  select(ut_co,ut,gr2010_2022,gr2000_2010,gr1991_2000,gr1980_1991,gr1970_1980)

growthratio_brrjnf%>%
  mutate(across(2:6,~percent_format(scale=100,
                                    big.mark = ".", 
                                    decimal.mark = ",",
                                    accuracy=0.01)(.)))%>%
  kbl() %>%
  kable_paper(full_width = F) %>%
  footnote(general = "Fonte: Censos Demográficos [@ibge2023]",
           general_title = "")


write_rds(growthratio_brrjnf,'data/processed/growthratio_brrjnf.rds')
  

#População por idade e sexo
temp_linhas <- read_lines("data/sidra.igbe.org.br/tabela9514.csv")
temp_eof <- which(str_detect(temp_linhas,"Fonte:"))
if(temp_eof>0){
  temp_9514<-read_csv('data/sidra.igbe.org.br/tabela9514.csv',skip=3,
                      col_names = c('munic_co', 'munic_no','skip','grid','ano','sex','pop'),
                      col_types=cols(
                        munic_co = col_double(),
                        munic_no = col_character(),
                        skip = col_skip(),
                        grid = col_character(),
                        sex = col_character(),
                        ano = col_double(),
                        pop = col_double()),
                      n_max = temp_eof-4)
}
temp_2022<-temp_9514%>%  
  mutate(grid =str_replace_all(grid, c(" a " = "-", " anos" = ""," ou mais"="+")))%>%
  mutate(grid=if_else(grid %in% c("80-84","85-89","90-94","95-99","100+"),
                      '80+',grid))%>%
  group_by(munic_co,munic_no,grid,ano,sex)%>%
  summarise(pop=sum(pop,na.rm = TRUE),.groups = 'drop')


temp_linhas <- read_lines("data/sidra.igbe.org.br/tabela200.csv")
temp_eof <- which(str_detect(temp_linhas,"Fonte:"))
if(temp_eof>0){
  temp_200<-read_csv('data/sidra.igbe.org.br/tabela200.csv',skip=4,
                      col_names = c('munic_co', 'munic_no','grid','ano','sex','pop'),
                      col_types=cols(
                        munic_co = col_double(),
                        munic_no = col_character(),
                        grid = col_character(),
                        sex = col_character(),
                        ano = col_double(),
                        pop = col_double()),
                      n_max = temp_eof-5)
}
temp_1<-temp_200%>%  
  mutate(grid =str_replace_all(grid, c(" a " = "-", " anos" = ""," ou mais"="+")))%>%
  mutate(grid=if_else(grid %in% c("80-84","85-89","90-94","95-99","100+"),
                      '80+',grid))%>%
  group_by(munic_co,munic_no,grid,ano,sex)%>%
  summarise(pop=sum(pop,na.rm = TRUE),.groups = 'drop')

temp_<-temp_1%>%
  filter(grid=='Idade ignorada')%>%
  select(-grid)%>%
  rename(ign=pop)

temp_serie<-temp_1%>%
  filter(grid!='Idade ignorada')%>%
  left_join(temp_,by=join_by(munic_co,munic_no,ano,sex))%>%
  group_by(munic_co,munic_no,ano,sex)%>%
  mutate(total=sum(pop,na.rm = TRUE))%>%
  mutate(pop=if_else(total==0,pop,pop+ign*pop/total))%>%
  select(munic_co,munic_no,ano,grid,sex,pop)%>%
  ungroup()

pop_grid_nf<-temp_serie%>%
  rbind(temp_2022)%>%
  inner_join(dtb_nf,by=c('munic_co','munic_no'))%>%
  filter(meso_no== 'Norte Fluminense')%>%
  mutate(grid=factor(grid,ordered=T,levels=c(paste0(seq(0,75,5),'-',seq(0,75,5)+4),'80+')))%>%
  select(meso_co,meso_no,ano,grid,sex,pop)%>%
  group_by(meso_co,meso_no,ano,grid,sex)%>%
  summarise(pop=sum(pop,na.rm = TRUE),.groups = 'drop')%>%
  arrange(ano,sex,grid)

write_rds(pop_grid_nf,'data/processed/pop_grid_nf.rds')

rm_temp()

temp_ggdf<-pop_grid_nf%>%
  group_by(ano)%>%
  mutate(total=sum(pop,na.rm = TRUE))%>%
  ungroup()%>%
  mutate(rel=if_else(sex=='Homens',-pop/total,pop/total))

temp_gg<-ggplot(data=temp_ggdf,aes(x=grid,y=rel,fill=sex))+
  geom_bar(stat='identity',position='stack')+
  coord_flip()+
  facet_wrap(~ano)+
  labs(title='Pirâmides Etárias',x= 'Grupos de idade')+
  scale_fill_manual(name='Sexo',values=rev(pal_startrek()(2)))+
  scale_y_continuous(labels = percent_format(scale=100),
                     name='População (%)')+
  theme_ipsum()

ggsave(here('output','plots','piramides.jpeg'),
       temp_gg,width=30,height=21,device='jpeg',dpi=150,units='cm')

#População por município
pop_total_nf<-pop_munic%>%
  group_by(sit,ano)%>%
  summarise(pop=sum(pop,na.rm = TRUE),.groups = 'drop')%>%
  pivot_wider(id_cols=sit,names_from = ano,values_from = pop)%>%
  mutate(sit=case_when(sit=='total'~'Total',
                       sit=='urb'~'Urbana',
                       sit=='rur'~'Rural'),
         ordem=case_when(sit=='Total'~3,
                         sit=='Urbana'~1,
                         sit=='Rural'~2))%>%
  arrange(ordem)%>%
  select(-ordem)

write_rds(pop_total_nf,'data/processed/pop_total_nf.rds')



pop_total_nf%>%
  kbl() %>%
  kable_paper(full_width = F)


pop_grid<-read_csv('data/raw/POP_MUNIC.csv',skip=4,na=c("", "NA",'...','-'),
                    col_names = c('munic_co','munic_no','grid_str','ano','sexo','pop'),
                    col_types=c('n','c','c','n','n'))%>%
  select(-munic_no)%>%
  inner_join(dtb_nf,by=c('munic_co'))%>%
  filter(meso_no== 'Norte Fluminense')%>%
  select(munic_co,munic_no,grid_str,ano,sexo,pop)%>%
  filter(grid_str!='Total')%>%
  mutate(grid_num=if_else(grid_str!='100 anos ou mais',
           as.numeric(str_trim(str_sub(grid_str,1,2))),
           100))%>%
  mutate(grid_num=if_else(grid_num>=80,80,grid_num))%>%
  mutate(grid_str=if_else(grid_num==80,'80+',
                          paste(str_pad(grid_num,width=2,pad=0),
                                str_pad(grid_num+4,width=2,pad=0),
                                sep='-')))%>%
  mutate(grid_f=factor(grid_str,ordered=T))%>%
  group_by(munic_co,munic_no,ano,sexo,grid_num,grid_str,grid_f)%>%
  summarise(pop=sum(pop,na.rm=T))%>%
  ungroup()%>%
  pivot_wider(names_from = sexo,values_from = pop)

write_xlsx(pop_munic,'pop_munic (Censos).xlsx')


# dados_wide<-tibble(pais=c('Brasil','Alemanha'),`2002`=c(2,0),`2014`=c(1,7))
# dados_wide
# 
# dados_long<-dados_wide%>%
#   gather(key=ano,value=gols,-pais)
# 
# dados_long
# 
# dados_wide2 <- dados_long %>%
#   spread(key=ano, value=gols) 
# dados_wide2

