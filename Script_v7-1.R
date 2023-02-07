# '''
# Libraries
# '''
{
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(ggpubr)
  library(ggthemes)
  library(ggforce)
  library(stringr)
  library(viridis)
  library(performance)
  library(tidyverse)
  library(tidymodels)
  library(lsr)
}
tidymodels_prefer(quiet = FALSE)


# '''
# Data processing
# '''


# Import and merge all three CSV files into one data frame
df <- list.files(path='C:/Users/edriano.souza/GitHub/6_2022_ALS_Biomass_2023/DATA_GEDI/v0-7-1_2023/data/', full.names = TRUE) %>% 
  lapply(read_csv, col_types = cols(ti_name = col_character())) %>% #Função "lapply" para ler todos os arquivos CSV na lista
  bind_rows


# colnames(df)

df_a <- select(df, -X1)


# Data as data.frame
df_a <- as.data.frame(df_a)
colnames(df_a)



#   Adjust AOI name with Tiles 
data <- df_a %>%
  mutate(als_AOI_site = recode (als_index, #Samples UF = TO
                                `FN3_A01_2014_P10_chm_10_OK` = "FN3_A01_2014_P10",
                                `FN3_A01_2014_P10_chm_4_OK` = "FN3_A01_2014_P10",
                                `FN3_A01_2014_P12_chm_5_OK` = "FN3_A01_2014_P12",
                                `FN3_A01_2014_P12_chm_6_OK` = "FN3_A01_2014_P12",  
                                `FN3_A01_2014_P13_chm_8_OK` = "FN3_A01_2014_P13",  
                                `FN3_A01_2014_P15_chm_0_OK` = "FN3_A01_2014_P15",  
                                `FN3_A01_2014_P15_chm_5_OK` = "FN3_A01_2014_P15",  
                                `FN3_A01_2014_P15_chm_8_OK` = "FN3_A01_2014_P15",  
                                `FN3_A01_2014_P16_chm_1_OK` = "FN3_A01_2014_P16",  
                                `FN3_A01_2014_P16_chm_2_OK` = "FN3_A01_2014_P16",  
                                #`FN3_A01_2014_P19_chm_4_OK` = "FN3_A01_2014_P19", 
                                `FN3_A01_2014_P19_chm_5_OK` = "FN3_A01_2014_P19",  
                                `FN3_A01_2014_P21_chm_1_Ok` = "FN3_A01_2014_P21",  
                                `FN3_A01_2014_P21_chm_4_OK` = "FN3_A01_2014_P21",  
                                `FN3_A01_2014_P21_chm_6_Ok` = "FN3_A01_2014_P21",  
                                `FN3_A01_2014_P22_chm_4_Ok` = "FN3_A01_2014_P22",  
                                `FN3_A01_2014_P23_chm_2_Ok` = "FN3_A01_2014_P23",  
                                `FN3_A01_2014_P23_chm_3_Ok` = "FN3_A01_2014_P23",  
                                `FN3_A01_2014_P25_chm_6_Ok` = "FN3_A01_2014_P25",  
                                `FN3_A01_2014_P26_chm_1_Ok` = "FN3_A01_2014_P26",  
                                `FN3_A01_2014_P26_chm_3_Ok` = "FN3_A01_2014_P26",  
                                `FN3_A01_2014_P26_chm_4_Ok` = "FN3_A01_2014_P26",  
                                `FN3_A01_2014_P27_chm_4_Ok` = "FN3_A01_2014_P27",  
                                `FN3_A01_2014_P27_chm_5_Ok` = "FN3_A01_2014_P27",  
                                `FN3_A01_2014_P27_chm_7_OK` = "FN3_A01_2014_P27",  
                                `FN3_A01_2014_P28_chm_0_Ok` = "FN3_A01_2014_P28",  
                                `FN3_A01_2014_P28_chm_3_Ok` = "FN3_A01_2014_P28",  
                                `FN3_A01_2014_P28_chm_4_Ok` = "FN3_A01_2014_P28",  
                                `FN3_A01_2014_P28_chm_6_Ok` = "FN3_A01_2014_P28",  
                                #Samples GO - 2014
                                `GO1_A01_2014_chm_0` = "GO1_A01_2014",            
                                `GO1_A01_2014_chm_1` = "GO1_A01_2014",            
                                `GO1_A01_2014_chm_10` = "GO1_A01_2014",           
                                `GO1_A01_2014_chm_12` = "GO1_A01_2014",           
                                `GO1_A01_2014_chm_48` = "GO1_A01_2014",           
                                #GO - 2018
                                `GO1_A01_2018_chm_3` = "GO1_A01_2018",            
                                `GO1_A01_2018_chm_13` = "GO1_A01_2018",           
                                `GO1_A01_2018_chm_15` = "GO1_A01_2018",           
                                `GO1_A01_2018_chm_21` = "GO1_A01_2018",           
                                `GO1_A01_2018_chm_35` = "GO1_A01_2018"))          


#   Adjust TIs
data_stb <- data %>%
  mutate(Name_TI_OK = recode(ti_name, 
                             `Acim???`="Acimã",	
                             `Alto Rio Guam???`="Alto Rio Guamá",
                             `Alto Tarauac???u`="Alto Tarauacá",
                             `Alto Turia???u`="Alto Turiaçu",
                             `Amanay???`="Amanayé",
                             `Anamb???`="Anambé",
                             `Anan???s`="Ananás",
                             `Andir???-Marau`="Andirá-Marau",
                             `Apiak???-Kayabi`="Apiaká-Kayabi",
                             `Apiak??? do Pontal e Isolados`="Apiaká do Pontal e Isolados",
                             `Apinay???`="Apinayé",
                             `Apurin??? do Igarap??? Mucuim`="Apurinã do Igarapé Mucuim",
                             `Apurin??? do Igarap??? S???o Jo???o`="Apurinã do Igarapé São João",
                             `Apurin??? do Igarap??? Tauamirim`="Apurinã do Igarapé Tauamirim",
                             `Apurin??? km-124 BR-317`="Apurinã - km-124 BR-317",
                             `Ara??????`="Araçá",
                             `Arara do Rio  Am???nia`="Arara do Rio Amazônia",
                             `Arara/Igarap??? Humait???`="Arara/Igarapé Humaitá",
                             `Ararib???ia`="Araribóia",
                             `Arawet???/Igarap??? Ipixuna`="Araweté/Igarapé Ipixuna",
                             `Are???es`="Areões",
                             `Aripuan???`="Aripuanã",
                             `Aw???`="Awá",
                             `Ba???`="Baú",
                             `Ba???a dos Guat???`="Baía dos Guató",
                             `Badj???nk???re`="Badjônkôre",
                             `Banaw???`="Banawá",
                             `Barreira da Miss???o`="Barreira da Missão",
                             `Batel???o`="Batelão",
                             `Bet???nia`="Betânia",
                             `Boqueir???o`="Boqueirão",
                             `Bragan???a/Marituba`="Bragança/Marituba",
                             `Cacau do Tarauac???`="Cacau do Tarauacá",
                             `Camicu???`="Camicuã",
                             `Cassup???`="Cassupá",
                             `Catipari/Mamori???`="Catipari/Mamoriá",
                             `Ch???o Preto`="Chão Preto",
                             `Coat???-Laranjal`="Coatá-Laranjal",
                             `Cu???-Cu???/Marabitanas`="Cué-Cué/Marabitanas",
                             `Cui???-Cui???`="Cuiú-Cuiú",
                             `Cunh???-Sapucaia`="Cunhã-Sapucaia",
                             `Enawen??? Naw???`="Enawenê Nawê",
                             `Esp???rito Santo`="Espírito Santo",
                             `Esta??????o Parecis`="Estação Parecis",
                             `Fortaleza do Patau???`="Fortaleza do Patauá",
                             `Gavi???o`="Gavião",
                             `???gua Preta/Inari`="Água Preta/Inari",
                             `Guajah???`="Guajahã",
                             `Hi-Merim???`="Hi-Merimã",
                             `Igarap??? Capan???`="Igarapé Capanã",
                             `Igarap??? do Caucho`="Igarapé do Caucho",
                             `Igarap??? Grande`="Igarapé Grande",
                             `Igarap??? Lage`="Igarapé Lage",
                             `Igarap??? Lourdes`="Igarapé Lourdes",
                             `Igarap??? Paiol`="Igarapé Paiol",
                             `Igarap??? Ribeir???o`="Igarapé Ribeirão",
                             `Igarap??? Taboca do Alto Tarauac???`="Igarapé Taboca do Alto Tarauacá",
                             `Ilha do Camale???o`="Ilha do Camaleão",
                             `In???w???bohona`="Inãwébohona",
                             `Ituna/Itat???`="Ituna/Itatá",
                             `Jacare???ba/Katawixi`="Jacareúba/Katawixi",
                             `Jaminawa do Igarap??? Preto`="Jaminawa do Igarapé Preto",
                             `Jaminawa/Arara do Rio Bag???`="Jaminawa/Arara do Rio Bagé",
                             `Japu???ra`="Japuíra",
                             `Jumin???`="Juminá",
                             `Jurubaxi-T???a`="Jurubaxi-Téa",
                             `Kampa do Igarap??? Primavera`="Kampa do Igarapé Primavera",
                             `Kampa do Rio Am???nea`="Kampa do Rio Amônea",
                             `Kanamari do Rio Juru???`="Kanamari do Rio Juruá",
                             `Kanela/Memortumré`="Kanela/Memortumré",
                             `Karaj??? de Aruan??? II`="Karajá de Aruanã II",
                             `Karaj??? de Aruan??? III`="Karajá de Aruanã III",
                             `Karaj??? Santana do Araguaia`="Karajá Santana do Araguaia",
                             `Karara???`="Kararaô",
                             `Kaxinawa da Col???nia Vinte e Sete`="Kaxinawa da Colônia Vinte e Sete",
                             `Kaxinawa do Baixo Jord???o`="Kaxinawa do Baixo Jordão",
                             `Kaxinawa do Rio Humait???`="Kaxinawa do Rio Humaitá",
                             `Kaxinawa do Rio Jord???o`="Kaxinawa do Rio Jordão",
                             `Kaxinawa Praia do Carapan???`="Kaxinawa Praia do Carapanã",
                             `Kaxinawa Seringal Independ???ncia`="Kaxinawa Seringal Independência",
                             `Kayap???`="Kayapó",
                             `Krah???-Kanela`="Krahô-Kanela",
                             `Kraol???ndia`="Kraolândia",
                             `Krenreh???`="Krenrehé",
                             `Kulina do Igarap??? do Pau`="Kulina do Igarapé do Pau",
                             `Kulina do M???dio Juru???`="Kulina do Médio Juruá",
                             `Kumaru do Lago Ual???`="Kumaru do Lago Ualá",
                             `Kuru???ya`="Kuruáya",
                             `Kwaz??? do Rio S???o Pedro`="Kwazá do Rio São Pedro",
                             `Lago Aiapu???`="Aiapuá",
                             `Lago Capan???`="Lago Capanã",
                             `Lago do Lim???o`="Lago do Limão",
                             `Lauro Sodr???`="Lauro Sodré",
                             `Macarr???o`="Macaarrão",
                             `Mano???/Pium`="Manoá/Pium",
                             `Mar???`="Maró",
                             `Mara???/Urubaxi`="Maraã/Urubaxi",
                             `Marait???`="Maraitá",
                             `Maraja???`="Marajaí",
                             `Mar???iwats???d???`="Marãiwatsédé",
                             `M???dio Rio Negro I`="Médio Rio Negro I",
                             `M???dio Rio Negro II`="Médio Rio Negro II",
                             `M???e Maria`="Mãe Maria",
                             `Menk??? (reestudo)`="Menkü (reestudo)",
                             `M???ria`="Méria",
                             `Murutinga/Tracaj???`="Murutinga/Tracajá",
                             `Nhamund???-Mapuera`="Nhamundá-Mapuera",
                             `Nova Esperan???a do Rio Jandiatuba`="Nova Esperança do Rio Jandiatuba",
                             `Nova Jacund???`="Nova Jacundá",
                             `Paca???s-Novas`="Pacaás-Novas",
                             `Panar???`="Paraná",
                             `Paqui???amba`="Paquiçamba",
                             `Paqui???amba (reestudo)`="Paquiçamba (reestudo)",
                             `Parakan???`="Parakanã",
                             `Paran??? do Arauat???`="Paraná do Arauató",
                             `Paran??? do Bo???-Bo???`="Paraná do Boá-Boá",
                             `Paran??? do Paric???`="Paraná do Paricá",
                             `Patau???`="Patauá",
                             `Paumari do Cuniu???`="Paumari do Cuniuá",
                             `Paumari do Lago Manissu???`="Paumari do Lago Manissuã",
                             `Paumari do Lago Marah???`="Paumari do Lago Marahã",
                             `Paumari do Lago Paric???`="Paumari do Lago Paricá",
                             `Pequizal do Naruv???tu`="Pequizal do Naruvôtu",
                             `Pirah???`="Pirahã",
                             `Porquinhos dos Canela-Ap???njekra (reestudo)`="Porquinhos dos Canela-Apãnjekra (reestudo)",
                             `Praia do ???ndio`="Praia do Índio",
                             `Raimund???o`="Raimundão",
                             `Recreio/S???o F???lix`="Recreio/São Felix",
                             `Rio Apap???ris`="Rio Apapóris",
                             `Rio Bi???`="Rio Biá",
                             `Rio Greg???rio`="Rio Gregório",
                             `Rio Guapor???`="Rio Guaporé",
                             `Rio Manicor???`="Rio Manicoré",
                             `Rio Mequ???ns`="Rio Mequéns",
                             `Rio Omer???`="Rio Omerê",
                             `Rio Pindar???`="Rio Pindaré",
                             `Santa Cruz da Nova Alian???a`="Santa Cruz da Nova Aliança",
                             `Santa In???s`="Santa Inês", 
                             `Sarar???`="Sararé",
                             `Sarau???`="Sarauá",
                             `Sawr??? Muybu (Pimental)`="Sawré Muybu (Pimental)",
                             `Serra da Mo???a`="Serra da Moça",
                             `Seruini/Marien???`="Seruini/Marienê",
                             `Setem???`="Setemã",
                             `Sissa???ma`="Sissaíma",
                             `S???o Domingos`="São Domingos",
                             `S???o Domingos do Jacapari e Esta??????o`="São Domingos do Jacapari e Estação",
                             `S???o Francisco do Canimari`="São Francisco do Canimari",
                             `S???o Leopoldo`="São Leopoldo",
                             `S???o Marcos`="São Marcos",
                             `S???o Marcos (Xavante)`="São Marcos (Xavante)",
                             `S???o Pedro`="São Pedro",
                             `S???o Pedro do Sepatini`="São Pedro do Sepatini",
                             `S???o Sebasti???o`="São Sebastião",
                             `Soror???`="Sororó",
                             `Sururu???`="Sururuá",
                             `Taego ???wa`="Taego Ãwa",
                             `Tapirap???/Karaj???`="Tapirapé/Karajá",
                             `Temb???`="Tembé",
                             `Tenharim do Igarap??? Preto`="Tenharim do Igarapé Preto",
                             `Tikuna de Santo Ant???nio`="Tikuna de Santo Antônio",
                             `Tor???`="Torá",
                             `Trincheira/Bacaj???`="Trincheira/Bacajá",
                             `Trocar???`="Trocará",
                             `Tubar???o/Latund???`="Tubarão/Latundê",
                             `Tukuna Umaria???u`="Tukuna Umariaçu",
                             `Tumi???`="Tumiã",
                             `Tup???-Sup???`="Tupã-Supé",
                             `Tur???-Mariquita`="Turé-Mariquita",
                             `Tur??? Mariquita II`="Turé-Mariquita II",
                             `Ua?????? I e II`="Uaçá I e II",
                             `Uati-Paran???`="Uati-Paraná",
                             `Urucu-Juru???`="Urucu-Juruá",
                             `Utaria Wyhyna/Ir???du Ir???na`="Utaria Wyhyna/Iròdu Iràna",
                             `Vale do Guapor???`="Vale do Guaporé",
                             `???vare  I`="Évare I",
                             `???vare II`="Évare II",
                             `Wai???pi`="Waiãpi",
                             `WaiW???i`="WaiWái",
                             `Wedez???`="Wedezé",
                             `Xambio???`="Xambioá",
                             `Xikrin do Catet???`="Xikrin do Cateté",
                             `Zo??????`="Zo´é",
                             `Zor???`="Zoró",
                             `Zuruah???`="Zuruahã"))



# 
samples_ <- group_by(df_a,als_index,Distance)|> #Agrupar por n variavel 
  summarise(
    count = n()
  )


# Até aqui é mais bem avaliado tratar os dados recodificando, conforme fizemos. 


#####################################################
##################### Data principal ################
#####################################################
data_stb <- as.data.frame(data_stb)
#####################################################
#####################################################
#####################################################

#####################################################
# DataVis basic
####################################################
#Test Correlaction Pearson
p<- ggscatter(data_stb, x = "als_max", y = "rh98",
              add = "reg.line",                                 # Add regression line
              conf.int = TRUE,                                  # Add confidence interval
              add.params = list(color = "red",
                                fill = "lightgray"))+
  stat_cor(method = "pearson", label.x = 0, label.y = 50,
           cor.coef.name = c("r"),
           r.accuracy = .001,
           p.accuracy = .05)+ # Add correlation coefficient
  theme_bw() + 
  geom_rug() +
  #geom_hex()+
  #geom_point()+
  ylab("rh_98")+
  xlab("chm_mean_als")+
  #geom_label(position = "nudge")+
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 12, 
                                  family = "fonte.tt", 
                                  face = "bold",
                                  lineheight = 3.5,
                                  vjust = 0.5,
                                  hjust = 0.5), 
        plot.caption = element_text(size = 0.5,
                                    hjust = 0.99, family = "fonte.tt"),  
        axis.text = element_text(family = "fonte.tt", size=12))+
  #facet_wrap(~uf_name, ncol= 3) #trocar por variável de interesse;
p

#model_lm_tidy <- linear_reg() %>%
#  set_engine("lm") %>% 
#  fit(rh98 ~ als_mean +Distance, data= df_forest_filtered_OK)
#check_model(model_lm_tidy)



#####################################################
# DataVis Group AOI
####################################################
# Verificar número de AOI

## Count
#samples_n <- group_by(data_stb,als_AOI_site)|>
#  summarise(
#    count = n()
#  )
#samples_n<- as.data.frame(samples_n)


#samples_d <- group_by(data_stb,Distance)|>
#  summarise(
#    count = n(),
#    mean = mean(Distance))

#ggplot(samples_d, aes(x=Distance, y=count)) +
  #geom_segment(aes(x=Distance, xend=Distance, y=0, yend=count), color="skyblue") +
#  geom_bar(stat="identity") +
#  theme_bw() +
  #coord_flip()+
#  theme(legend.position = "none") +
#  scale_color_manual(values=palette, name="Distance")+
#  ylab("N footprints") +
#  xlab("Distance")



#samples_d<- as.data.frame(samples_d)

#samples_stb <- group_by(data_stb,als_AOI_site,Distance)|>
#  summarise(
#    count = n()
#  )

#samples_stb<- as.data.frame(samples_stb)



####################################################
#Aplicar filtros apenas para floresta
############################################################

# 1:0 Mapbiomas todos anos igual a 3;
#LULC Mapbiomas
columns_to_filter <- c("lulc_2010", "lulc_2011", "lulc_2012", "lulc_2013",
                       "lulc_2014", "lulc_2015", "lulc_2016", "lulc_2017",
                       "lulc_2018", "lulc_2019", "lulc_2020", "lulc_2021")

# Separar conjunto de dados filtrando 

## -------------------------------------------------Mapbiomas
###Filtrar conjunto de dados apenas para savana considerando LULC do Mapbiomas para todos os anos apenas savana
df_savanna_filtered <- data_stb
for (col in columns_to_filter) {
  df_savanna_filtered <- df_savanna_filtered %>% filter(eval(as.name(col)) == "[4.0]")
}
### Filtrar conjunto de dados apenas para savana considerando LULC do Mapbiomas para todos os anos apenas floresta
df_forest_filtered <- data_stb 
for (col in columns_to_filter) {
  df_forest_filtered <- df_forest_filtered %>% filter(eval(as.name(col)) == "[3.0]")
}


##### Subconjunto considerando apenas pixel com área >= # Esse passo aqui pode ser anteriormente; 

df_forest_filtered_OK <- df_forest_filtered %>%
  filter (gedi_area_he >= 0.06)
df_savanna_filtered_OK <- df_savanna_filtered %>%
  filter (gedi_area_he >= 0.06)



# Libraries
library(ggplot2)

# Change baseline
#ggplot(samples_stb, aes(x=count, y=als_index)) +
#  geom_segment(aes(x=count, xend=count, y=als_index, yend=als_index)) +
#  geom_point( color="orange", size=4) +
#  theme_light() +
#  theme(
#    panel.grid.major.x = element_blank(),
#    panel.border = element_blank(),
#    axis.ticks.x = element_blank()
#  ) +
#  xlab("") +
#  ylab("Value of Y")

#ggplot(samples_stb, aes(x=als_AOI_site, y=count)) +
#  geom_segment(aes(x=als_AOI_site, xend=als_AOI_site, y=0, yend=count), color=pallete) +
#  geom_point(color="blue", size=4, alpha=0.6) +
#  theme_light() +
#  coord_flip() +
#  theme(
#    panel.grid.major.y = element_blank(),
#    panel.border = element_blank(),
#    axis.ticks.y = element_blank())+
#  ylab("N footprints") +
#  xlab("ALS_GEDI_AOI")+
#  #theme(legend.position=c(.5,.1), legend.box = "horizontal",legend.justification = "center")+
#  theme(legend.key = element_blank())+
#  #theme(axis.text.x = element_text(margin = margin(t = .01, unit = "mm"))) +
#  #theme(axis.text.y = element_text(margin = margin(t = .01, unit = "mm"))) +
#  theme(axis.text.y = element_text(hjust = 1.5))+
#  theme(legend.key.height = unit(0.01, "in"))+
#  theme(legend.background = element_blank())+
#  theme(legend.title = element_text(color = "black", family = "fonte.tt", size=9))+
#  theme(axis.title = element_text(color = "black",family = "fonte.tt", size=9))+
#  theme(legend.text =  element_text(color = "black",family = "fonte.tt", size=9,face = "bold"))+ # Aqui e a letra da legenda
#  theme(axis.title.x = element_text(color = "black",family = "fonte.tt", size=9, face = "bold"))+
#  theme(axis.title.y = element_text(color = "black",family = "fonte.tt", size=9, face = "bold"))

#ggplot(samples_stb, aes(x=als_AOI_site, y=count, color=factor(als_AOI_site))) +
#  geom_segment(aes(x=als_AOI_site, xend=als_AOI_site, y=0, yend=count), color="skyblue") +
#  geom_point(size=4, alpha=0.6) +
#  theme_light() +
#  coord_flip()+
#  theme(legend.position = "none")+theme(axis.text.x = element_text(color = "black",family = "font.tt",size=9, angle = 90))

#samples_biomes <-group_by(data_ti,biome_name)|>
#  summarise(
#    count = n())
#samples_biomes <- as.data.frame(samples_biomes)

#samples_biomes_uf <-group_by(data_ti,biome_name,uf_name)|>
#  summarise(
#    count = n())
#samples_biomes_uf <- as.data.frame(samples_biomes_uf)

#ciMean(data_stb$rh98, conf =.95, na.rm=T)


### Create filter
teste<-
  df_forest_filtered_OK %>%
  filter(Distance == "0" | Distance == "5" | Distance == "10" | Distance == "15" | Distance == "25"|
           Distance == "30"| Distance == "35"| Distance == "55" | Distance == "60"| Distance == "65"|
           Distance == "85"| Distance == "90"| Distance == "100"| Distance == "105"| Distance == "115"| 
           Distance == "120"| Distance == "125"| Distance == "130"| Distance == "135"| Distance == "140"| 
           Distance == "145"| Distance == "155"| Distance == "160"| Distance == "170"| Distance == "175"| 
           Distance == "180"| Distance == "190"| Distance == "195")



##############################################################
##############################################################
# Outro filtro é o Hansen
########################################################

## -------------------------------------------------LULC Hansen and Plant Functional Type (MCD12Q1 V006) and each Biomes
#Separate sample Forest
data_df_forest <- df_forest_filtered_OK %>%
  filter (hansen_lossyear <= 0) %>%  # hansen forest
  filter (pft_class <= 2) # GEDI 1 km EASE 2.0 grid Plant Functional Type (PFT) ::  evergreen broadleaf trees (EBT, class 2)



