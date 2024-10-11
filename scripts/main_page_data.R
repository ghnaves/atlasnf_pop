library('tidyverse')
library('openxlsx')
library('readxl')
library('kableExtra')

dtb<-read_xls('data/raw/RELATORIO_DTB_BRASIL_MUNICIPIO.xls',
              col_names=c('uf_co','uf_no','inter_co','inter_no','imed_co','imed_nm',
                          'meso_co','meso_no','micro_co','micro_no','munic_co5','munic_co','munic_no'),
              col_types=c(rep(c('numeric','text'),5),'numeric','numeric','text'),skip=1)

dtb_nf<-dtb%>%
  filter(meso_no=='Norte Fluminense')

write_rds(dtb_nf,'data/processed/dtb_nf.rds')
write_rds(dtb,'data/processed/dtb.rds')


temp_popserie<-read_xlsx('data/raw/tabela202.xlsx',range='A8:R5603',col_names = int2col(1:18),
                         col_types=c('text','numeric','text',rep('numeric',15)))%>%
  filter(A=='MU')%>%
  mutate(uf=trunc(B/100000),
         C=str_trim(C))%>%
  rename(munic_co=B,
         total1970=D,total1980=G,
         total1991=J,total2000=M,total2010=P)%>%
  select(uf,munic_co,total1970,total1980,total1991,total2000,total2010)

temp_pop2022<-read_xlsx('data/raw/tabela4709.xlsx',range='A5:D5601',col_names = int2col(1:4),
                        col_types=c('text','numeric','text','numeric'))%>%
  filter(A=='MU')%>%
  mutate(uf=trunc(B/100000),
         C=str_trim(C))%>%
  rename(munic_co=B,
         total2022=D)%>%
  select(uf,munic_co,total2022)

pop_total<-temp_pop2022%>%
  left_join(temp_popserie,by=c('uf','munic_co'))%>%
  filter(munic_co %in% munic_NF$munic_co)%>%
  inner_join(dtb,by=c('munic_co'))%>%
  bind_rows(
    summarise(., 
              munic_no = "Total", 
              across(starts_with('total'), sum, na.rm = TRUE))
  )

pop_total%>%
  select(munic_no,total1970,total1980,total1991,total2000,total2010,total2022)%>%
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

