library(tidyverse)
library(rio)
library(janitor)
library(formattable)

data <- import('industrias afectadas.xlsx') %>% 
  clean_names() 


pbg <- import('pbg rama.xlsx') %>% 
  clean_names() 


data_2 <-  data %>% 
  group_by(rama, desc_rama) %>% 
  summarise(empleo_activo = sum(empleo_abril_20[afectacion == 'Abierto']), 
            empleo_inactivo = sum(empleo_abril_20[afectacion != 'Abierto']),
            empresas_activas = sum(empresas_abril_20[afectacion == 'Abierto']),
            empresas_inactivas = sum(empresas_abril_20[afectacion != 'Abierto'])) %>% 
  mutate(particip_empleo = percent(empleo_activo/(empleo_activo + empleo_inactivo)),
         particip_empresas = percent(empresas_activas/(empresas_activas + empresas_inactivas))) %>% 
  left_join(pbg) %>% 
  arrange(desc(empleo_activo))



export(data_2, 'industrias_abiertas.xlsx')
