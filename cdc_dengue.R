library (tidyverse);library(janitor);library(ggplot2);library(readxl);library(dplyr);library(skimr)
library(epical);library(EpiWeek);library(scales);library(lubridate);library(zoo)
library(sf);library(ggspatial);library(maps);library(mapdata);library(ggrepel);library(RColorBrewer)
library(ggpubr);library(haven);library(osmdata);library(leaflet);library(RQGIS);library(geosphere);library(osmdata)
library(osmar);library(sp)

theme_set(theme_light())

ubigeo_sel <- c("010201", "010202", "021803", "060101", "060801", "060811", "060901", 
                "060907", "120601", "130101", "130205",
                "140101", "140112", "140117", "140201", "140301", "140307", "140308", "150101", "150128", 
                "150132", "150134",
                "150141", "150142", "160101", "160112", "160201", "170101", "200101", "200104", "200105", 
                "200107", "200108",
                "200109", "200110", "200111", "200114", "200115", "200201", "200207", "200210", "200301", 
                "200302", "200305",
                "200306", "200401", "200402", "200404", "200405", "200406", "200407", "200408", "200409", 
                "200501", "200503",
                "200504", "200505", "200506", "200507", "200601", "200602", "200603", "200604", "200605", 
                "200606", "200607",
                "200608", "200701", "200702", "200703", "200705", "200706", "200801", "200802", "200803", 
                "200804", "200805",
                "200806", "220101", "220206", "220601", "220701", "220709", "220801", "220901", "240101", 
                "240102", "240103",
                "240104", "240105", "240201", "240203", "240301", "240302", "240303", "250101", "250201")



date_month<-rep(seq(as.POSIXct("2010-01-01"), by="month", length=7),97)
ubigeo<-rep(ubigeo_sel,84)
df_dengue<-data.frame(month=date_month,ubigeo=ubigeo) %>% 
          mutate(ubigeo=as.character(ubigeo)) %>%
          arrange(ubigeo,month)



date_year2<-rep(seq(2010, length=7),1164)
date_month2<-rep(seq(1, length=12),679)
ubigeo2<-rep(ubigeo_sel,84)
df_dengue2<-data.frame(year=date_year2,month=date_month2,ubigeo=ubigeo) %>% 
  mutate(ubigeo=as.numeric(as.character(ubigeo))) %>%
    arrange(year,month,ubigeo)





dengue1015<-read_xlsx("DENGUE 2010-2016.xlsx") %>% clean_names()


pop_piura10<-read_excel("cuadro001_1.xls",sheet = 6, col_names = T) %>% clean_names()
pop_piura10<-pop_piura10 %>% filter(!is.na(ubigueo)) 
pop_piura10<- pop_piura10 %>% select(ubigueo, total)%>% filter(ubigueo %in% ubigeo_sel) %>% 
              mutate(ubigeo=ubigueo) %>% 
              mutate(ano=2010) %>% 
              select(-ubigueo)


pop_piura11<-read_excel("cuadro001_1.xls",sheet = 7, col_names = T) %>% clean_names()
pop_piura11<-pop_piura11 %>% filter(!is.na(ubigueo)) 
pop_piura11<- pop_piura11 %>% select(ubigueo, total) %>% filter(ubigueo %in% ubigeo_sel) %>% 
              mutate(ubigeo=ubigueo) %>% 
              mutate(ano=2011) %>% 
              select(-ubigueo)

pop_piura12<-read_excel("cuadro001_1.xls",sheet = 8, col_names = T) %>% clean_names()
pop_piura12<-pop_piura12 %>% filter(!is.na(ubigueo)) 
pop_piura12<- pop_piura12 %>% select(ubigueo, total) %>% filter(ubigueo %in% ubigeo_sel) %>% 
              mutate(ubigeo=ubigueo) %>% 
              mutate(ano=2012) %>% 
              select(-ubigueo)

pop_piura13<-read_excel("cuadro001_1.xls",sheet = 9, col_names = T) %>% clean_names()
pop_piura13<-pop_piura13 %>% filter(!is.na(ubigueo)) 
pop_piura13<- pop_piura13 %>% select(ubigueo, total) %>% filter(ubigueo %in% ubigeo_sel) %>% 
              mutate(ubigeo=ubigueo) %>% 
              mutate(ano=2013) %>% 
              select(-ubigueo)

pop_piura14<-read_excel("cuadro001_1.xls",sheet = 10, col_names = T) %>% clean_names()
pop_piura14<-pop_piura14 %>% filter(!is.na(ubigueo)) 
pop_piura14<- pop_piura14 %>% select(ubigueo, total) %>% filter(ubigueo %in% ubigeo_sel) %>% 
              mutate(ubigeo=ubigueo) %>% 
              mutate(ano=2014) %>% 
              select(-ubigueo)

pop_piura15<-read_excel("cuadro001_1.xls",sheet = 11, col_names = T) %>% clean_names()
pop_piura15<-pop_piura15 %>% filter(!is.na(ubigueo)) 
pop_piura15<- pop_piura15 %>% select(ubigueo, total) %>% filter(ubigueo %in% ubigeo_sel) %>% 
              mutate(ubigeo=ubigueo) %>% 
              mutate(ano=2015) %>% 
              select(-ubigueo)

pop_piura1015<-rbind(pop_piura10,pop_piura11,pop_piura12,pop_piura13,pop_piura14,pop_piura15)
pop_piura1015<- pop_piura1015 %>%  mutate(ano3=parse_date_time(ano,"Y"))


dengue1015j <- left_join(dengue1015,pop_piura1015,by=(c("ubigeo"="ubigeo","ano"="ano")))
dengue1015j <- dengue1015j %>% 
  mutate(ano3=parse_date_time(ano,"Y")) 
  


# Per Month-------------------------------------------------------
dengue1015_ssa <-  dengue1015 %>% 
  select(fecha_not,ubigeo,ano) %>% 
  filter(dengue1015$nom_diag=="DENGUE SIN SEÐALES DE ALARMA") %>%
  mutate(month2=parse_date_time(format(fecha_not,"%Y-%m"),"Y m"))%>%
  count(month2,ano,ubigeo) %>% 
  ungroup() %>% print()
dengue1015_ssa<-left_join(dengue1015_ssa,pop_piura1015,by=(c("ubigeo"="ubigeo","ano"="ano"))) 
dengue1015_ssa<- dengue1015_ssa %>% mutate(incidence=(1000*n)/total)
dengue1015_ssa<-add_epi_week(data=dengue1015_ssa, "month2", system = "who")
dengue1015_ssa_plot<-dengue1015_ssa %>% group_by(month2) %>% summarise(incidence=sum(incidence))


dengue1015_csa <-  dengue1015 %>% 
  select(fecha_not,ubigeo,ano) %>% 
  filter(dengue1015$nom_diag=="DENGUE CON SEÐALES DE ALARMA") %>%
  mutate(month2=parse_date_time(format(fecha_not,"%Y-%m"),"Y m"))%>%
  count(month2,ano,ubigeo) %>% 
  ungroup() %>% print()
dengue1015_csa<-left_join(dengue1015_csa,pop_piura1015,by=(c("ubigeo"="ubigeo","ano"="ano"))) 
dengue1015_csa<- dengue1015_csa %>% mutate(incidence=(1000*n)/total) %>% print()
dengue1015_csa<-add_epi_week(data=dengue1015_csa, "month2", system = "who") %>% print()
dengue1015_csa_plot<-dengue1015_csa %>% group_by(month2) %>% summarise(incidence=sum(incidence))

  
dengue1015_g <-  dengue1015 %>% 
  select(fecha_not,ubigeo,ano) %>% 
  filter(dengue1015$nom_diag=="DENGUE GRAVE") %>%
  mutate(month2=parse_date_time(format(fecha_not,"%Y-%m"),"Y m"))%>%
  count(month2,ano,ubigeo) %>% 
  ungroup() %>% print()
dengue1015_g<-left_join(dengue1015_g,pop_piura1015,by=(c("ubigeo"="ubigeo","ano"="ano"))) 
dengue1015_g<- dengue1015_g %>% mutate(incidence=(1000*n)/total)
dengue1015_g<-add_epi_week(data=dengue1015_g, "month2", system = "who") %>% print()
dengue1015_g_plot<-dengue1015_g %>% group_by(month2) %>% summarise(incidence=sum(incidence))



plot_incidence<-   ggplot(data=dengue1015_ssa_plot %>% filter(!is.na(incidence)), 
                  aes(x=month2,y=incidence), na.rm=T)+  
            geom_line() +
            geom_line(data=dengue1015_csa_plot %>% filter(!is.na(incidence)), 
                      aes(x=month2,y=incidence), 
                      color="blue", na.rm=T) +
            geom_line(data=dengue1015_g_plot %>% filter(!is.na(incidence)), 
                      aes(x=month2,y=incidence), 
                      color="red", na.rm=T) +
            scale_x_datetime(labels = date_format("%Y"), breaks = "1 year")+
            xlab("Year")+  ylab("Incidence per 1000 inhabitants")+
            ggtitle("Incidence of dengue reported cases 2010-2016")
ggsave("dengue1016_inc.png", width=330,height=210,units="mm")


# Per month + facet wrap -----------------------------------------------------------------

dengue1015_ssa_plot2<-dengue1015_ssa %>% group_by(month2,ubigeo) %>% summarise(incidence=sum(incidence))
dengue1015_ssa_plot2<- dengue1015_ssa %>% mutate(year=year(month2),
                                             mes=month(month2),
                                             ubigeo=as.numeric(ubigeo)) %>% 
                                    select(ano,mes,ubigeo,incidence)
prueba3<-left_join(df_dengue2,dengue1015_ssa_plot2,by=(c("year"="ano",
                                                       "month"="mes",
                                                       "ubigeo"="ubigeo"))) %>% 
                                    arrange(year,ubigeo,month) %>% 
                                    mutate(sqrow=row_number())
prueba3$incidence[is.na(prueba3$incidence)]<-0


plot_incidence3<-   ggplot(data=prueba3,aes(x=sqrow,y=incidence))+  
  geom_line()+
  facet_wrap(~as.factor(ubigeo))




dengue1015_csa_plot2<-dengue1015_csa %>% group_by(month2,ubigeo) %>% summarise(incidence=sum(incidence))
dengue1015_csa_plot2<- dengue1015_csa %>% mutate(year=year(month2),
                                                 mes=month(month2),
                                                 ubigeo=as.numeric(ubigeo)) %>% 
                                  select(ano,mes,ubigeo,incidence)
prueba4<-left_join(df_dengue2,dengue1015_csa_plot2,by=(c("year"="ano",
                                                         "month"="mes",
                                                         "ubigeo"="ubigeo"))) %>% 
                                  arrange(year,ubigeo,month) %>% 
                                  mutate(sqrow=row_number())
prueba4$incidence[is.na(prueba4$incidence)]<-0


plot_incidence4<-   ggplot(data=prueba4,aes(x=sqrow,y=incidence))+  
  geom_line()+
  facet_wrap(~as.factor(ubigeo))




dengue1015_g_plot2<-dengue1015_g %>% group_by(month2,ubigeo) %>% summarise(incidence=sum(incidence))
dengue1015_g_plot2<- dengue1015_g %>% mutate(year=year(month2),
                                             mes=month(month2),
                                             ubigeo=as.numeric(ubigeo)) %>% 
                                      select(ano,mes,ubigeo,incidence)
prueba2<-left_join(df_dengue2,dengue1015_g_plot2,by=(c("year"="ano",
                                                         "month"="mes",
                                                         "ubigeo"="ubigeo"))) %>% 
                        arrange(year,ubigeo,month) %>% 
                        mutate(sqrow=row_number())
prueba2$incidence[is.na(prueba2$incidence)]<-0


plot_incidence2<-   ggplot(data=prueba2,aes(x=sqrow,y=incidence))+  
  geom_line()+
  facet_wrap(~as.factor(ubigeo))



# Choropleth map by district--------------------------------------------------------------------------
peru <- st_read(dsn="maps/distritos_ubigeo",layer="geodir_ubigeo_inei")
peru <- st_as_sf(peru) %>% 
  mutate(centroid = purrr::map(geometry, st_centroid),
          coords = purrr::map(centroid, st_coordinates),
          coords_x = purrr::map_dbl(coords, 1),
          coords_y = purrr::map_dbl(coords, 2)) %>%
  as_tibble() %>%
  st_as_sf()
peru_piura <- peru %>% filter(as.integer(as.character(ubigeo))>200000 & as.integer(as.character(ubigeo))<210000) %>% 
                    select(ubigeo,geometry,distrito) %>% 
                    mutate(ubigeo=as.integer(as.character(ubigeo)))


sudamerica <- st_read(dsn="maps/sudamerica",layer="Sudamérica") %>% clean_names()  
sudamerica <- as.data.frame(sudamerica[6,1:2])
sudamerica<-  st_as_sf(sudamerica) %>% 
  mutate(centroid = purrr::map(geometry, st_centroid),
         coords = purrr::map(centroid, st_coordinates),
         coords_x = purrr::map_dbl(coords, 1),
         coords_y = purrr::map_dbl(coords, 2)) %>%
  as_tibble() %>%
  st_as_sf()

departamentos <- st_read(dsn="maps/departamentos",layer="DEPARTAMENTO") %>% clean_names()
departamentos <- st_as_sf(departamentos) %>% 
  mutate(centroid = purrr::map(geometry, st_centroid),
         coords = purrr::map(centroid, st_coordinates),
         coords_x = purrr::map_dbl(coords, 1),
         coords_y = purrr::map_dbl(coords, 2)) %>%
  as_tibble() %>%
  st_as_sf()
departamentos<- departamentos %>% mutate(departamen=str_to_title(as.character(departamen)))
dpto_sel<-departamentos %>%  filter (departamen %in% c("Tumbes","Lambayeque"))

prueba_dst_ssa<-prueba3 %>% group_by(ubigeo) %>% summarise(incidence=sum(incidence))
prueba_dst_ssa<-left_join(prueba_dst_ssa,peru_piura,by=("ubigeo"))   
prueba_dst_ssa<- prueba_dst_ssa %>% mutate(geometry=geometry.x) 
prueba_dst_ssa<-prueba_dst_ssa %>% 
                filter(ubigeo>200000 & ubigeo<210000)
prueba_dst_ssa<-st_as_sf(prueba_dst_ssa) %>% 
  mutate(centroid = purrr::map(geometry, st_centroid),
         coords = purrr::map(centroid, st_coordinates),
         coords_x = purrr::map_dbl(coords, 1),
         coords_y = purrr::map_dbl(coords, 2)) %>%
  as_tibble() %>%
  st_as_sf()


prueba_dst_ssa$nudge_x <- 0
prueba_dst_ssa$nudge_y <- 0

x_range <- abs(Reduce("-", range(prueba_dst_ssa$coords_x)))
y_range <- abs(Reduce("-", range(prueba_dst_ssa$coords_y)))

ix <- prueba_dst_ssa$distrito %in% c("Vice", "La Union","Bellavista de La Union")
prueba_dst_ssa$nudge_x[ix] <- -2 * 0.15 * x_range
prueba_dst_ssa$nudge_y[ix] <- -2 * 0.15 * y_range

ix2 <- prueba_dst_ssa$distrito %in% 
  c("La Brea","Arenal","Colan","Paita","Bernal","Bellavista", "La Arena","Miguel Checa",
    "La Huaca")
prueba_dst_ssa$nudge_x[ix2] <- -2 * 0.2 * x_range
prueba_dst_ssa$nudge_y[ix2] <-  1 * 0.02 *  y_range

ix3<- prueba_dst_ssa$distrito %in% 
  c("Pariñas","El Alto","Mancora", "Los Organos","Tamarindo","Ignacio Escudero")
prueba_dst_ssa$nudge_x[ix3] <- -2 * 0.2 * x_range
prueba_dst_ssa$nudge_y[ix3] <- 1 * 0.2 *  y_range

ix4<- prueba_dst_ssa$distrito %in% 
  c("Buenos Aires","San Juan de Bigote")
prueba_dst_ssa$nudge_x[ix4] <- 1 * 0.02 *  x_range
prueba_dst_ssa$nudge_y[ix4] <- -2 * 0.2 *  y_range

ix5<- prueba_dst_ssa$distrito %in% 
  c("Lancones","Suyo","Paimas","Ayabaca")
prueba_dst_ssa$nudge_x[ix5] <- 2 * 0.2 *  x_range
prueba_dst_ssa$nudge_y[ix5] <- 2 * 0.2 *  y_range

ix6<- prueba_dst_ssa$distrito %in% 
  c("Santo Domingo","Lalaquiz","Huancabamba","Canchaque","San Miguel de El Faique",
    "Santa Catalina de Mossa")
prueba_dst_ssa$nudge_x[ix6] <- 1.5 * 0.2 *  x_range
prueba_dst_ssa$nudge_y[ix6] <- 1 * 0.02 *  y_range

ix7<- prueba_dst_ssa$distrito %in% c("Sechura")
prueba_dst_ssa$nudge_x[ix7] <- -1.5 * 0.15 * x_range
prueba_dst_ssa$nudge_y[ix7] <- -1.5 * 0.15 * y_range



plot_dst_ssa<-  ggplot()+
  geom_sf(data=sudamerica, fill="antiquewhite")+
  geom_text_repel(data=sudamerica,
                  aes(x=coords_x, y=coords_y, label=pais))+
  geom_sf(data=departamentos)+
  geom_text_repel(data=dpto_sel,
                  aes(x=coords_x, y=coords_y, label=departamen))+
  geom_sf(data=prueba_dst_ssa,aes(fill=incidence))+
  geom_text_repel(data=prueba_dst_ssa,
                  aes(x=coords_x, y=coords_y, label=distrito),
                      nudge_x = prueba_dst_ssa$nudge_x,
                      nudge_y = prueba_dst_ssa$nudge_y,
                      size=2.5, segment.alpha = 0.5)+
  scale_fill_gradientn(colors=brewer.pal(9,"BuPu"))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))+
  ggtitle("Dengue without alarm symptoms") + 
  xlab("Longitude") + ylab("Latitude") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"), 
                         style = north_arrow_fancy_orienteering) + 
  annotate(geom = "text", x = -81.5, y = -6.5, label = "Pacific ocean", 
           fontface = "italic", color = "grey22", size = 4)+
  coord_sf(xlim = c(-82, -79), ylim = c(-7,-3), expand = T)+
  annotation_scale() 
  
ggsave("dengue1016_ssa.png", width=330,height=210,units="mm")


prueba_dst_csa<-prueba4 %>% group_by(ubigeo) %>% summarise(incidence=sum(incidence))
prueba_dst_csa<-left_join(prueba_dst_csa,peru_piura,by=("ubigeo"))   
prueba_dst_csa<- prueba_dst_csa %>% mutate(geometry=geometry.x) 
prueba_dst_csa<-prueba_dst_csa %>% 
                filter(ubigeo>200000 & ubigeo<210000)
prueba_dst_csa<-st_as_sf(prueba_dst_csa) %>% 
  mutate(centroid = purrr::map(geometry, st_centroid),
         coords = purrr::map(centroid, st_coordinates),
         coords_x = purrr::map_dbl(coords, 1),
         coords_y = purrr::map_dbl(coords, 2)) %>%
  as_tibble() %>%
  st_as_sf()


prueba_dst_csa$nudge_x <- 0
prueba_dst_csa$nudge_y <- 0

x_range <- abs(Reduce("-", range(prueba_dst_csa$coords_x)))
y_range <- abs(Reduce("-", range(prueba_dst_csa$coords_y)))

ix <- prueba_dst_csa$distrito %in% c("Sechura", "Vice", "La Union","Bellavista de La Union")
prueba_dst_csa$nudge_x[ix] <- -2 * 0.15 * x_range
prueba_dst_csa$nudge_y[ix] <- -2 * 0.15 * y_range

ix2 <- prueba_dst_csa$distrito %in% 
  c("La Brea","Arenal","Colan","Paita","Bernal","Bellavista", "La Arena","Miguel Checa",
    "La Huaca")
prueba_dst_csa$nudge_x[ix2] <- -2 * 0.2 * x_range
prueba_dst_csa$nudge_y[ix2] <-  1 * 0.02 *  y_range

ix3<- prueba_dst_csa$distrito %in% 
  c("Pariñas","El Alto","Mancora", "Los Organos","Tamarindo","Ignacio Escudero")
prueba_dst_csa$nudge_x[ix3] <- -2 * 0.2 * x_range
prueba_dst_csa$nudge_y[ix3] <- 1 * 0.2 *  y_range

ix4<- prueba_dst_csa$distrito %in% 
  c("Buenos Aires","San Juan de Bigote")
prueba_dst_csa$nudge_x[ix4] <- 1 * 0.02 *  x_range
prueba_dst_csa$nudge_y[ix4] <- -2 * 0.2 *  y_range

ix5<- prueba_dst_csa$distrito %in% 
  c("Lancones","Suyo","Paimas","Ayabaca")
prueba_dst_csa$nudge_x[ix5] <- 2 * 0.2 *  x_range
prueba_dst_csa$nudge_y[ix5] <- 2 * 0.2 *  y_range

ix6<- prueba_dst_csa$distrito %in% 
  c("Santo Domingo","Lalaquiz","Huancabamba","Canchaque","San Miguel de El Faique",
    "Santa Catalina de Mossa")
prueba_dst_csa$nudge_x[ix6] <- 1.5 * 0.2 *  x_range
prueba_dst_csa$nudge_y[ix6] <- 1 * 0.02 *  y_range

ix7<- prueba_dst_csa$distrito %in% c("Sechura")
prueba_dst_csa$nudge_x[ix7] <- -1.5 * 0.15 * x_range
prueba_dst_csa$nudge_y[ix7] <- -1.5 * 0.15 * y_range



plot_dst_csa<-  ggplot()+
  geom_sf(data=sudamerica, fill="antiquewhite")+
  geom_text_repel(data=sudamerica,
                  aes(x=coords_x, y=coords_y, label=pais))+
  geom_sf(data=departamentos)+
  geom_text_repel(data=dpto_sel,
                  aes(x=coords_x, y=coords_y, label=departamen))+
  geom_sf(data=prueba_dst_csa,aes(fill=incidence))+
  geom_text_repel(data=prueba_dst_csa,
                  aes(x=coords_x, y=coords_y, label=distrito),
                  nudge_x = prueba_dst_csa$nudge_x,
                  nudge_y = prueba_dst_csa$nudge_y,
                  size=2.5, segment.alpha = 0.5)+
  scale_fill_gradientn(colors=brewer.pal(9,"BuPu"))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))+
  ggtitle("Dengue with alarm symptoms") + 
  xlab("Longitude") + ylab("Latitude") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"), 
                         style = north_arrow_fancy_orienteering) + 
  annotate(geom = "text", x = -81.5, y = -6.5, label = "Pacific ocean", 
           fontface = "italic", color = "grey22", size = 4)+
  coord_sf(xlim = c(-82, -79), ylim = c(-7,-3), expand = T)+
  annotation_scale() 

ggsave("dengue1016_csa.png", width=330,height=210,units="mm")



prueba_dst_g<-prueba2 %>% group_by(ubigeo) %>% summarise(incidence=sum(incidence))
prueba_dst_g<-left_join(prueba_dst_g,peru_piura,by=("ubigeo"))   
prueba_dst_g<- prueba_dst_g %>% mutate(geometry=geometry.x) 
prueba_dst_g<-prueba_dst_g %>% 
  filter(ubigeo>200000 & ubigeo<210000)
prueba_dst_g<-st_as_sf(prueba_dst_g) %>% 
  mutate(centroid = purrr::map(geometry, st_centroid),
         coords = purrr::map(centroid, st_coordinates),
         coords_x = purrr::map_dbl(coords, 1),
         coords_y = purrr::map_dbl(coords, 2)) %>%
  as_tibble() %>%
  st_as_sf()


prueba_dst_g$nudge_x <- 0
prueba_dst_g$nudge_y <- 0

x_range <- abs(Reduce("-", range(prueba_dst_g$coords_x)))
y_range <- abs(Reduce("-", range(prueba_dst_g$coords_y)))

ix <- prueba_dst_g$distrito %in% c("Sechura", "Vice", "La Union","Bellavista de La Union")
prueba_dst_g$nudge_x[ix] <- -2 * 0.15 * x_range
prueba_dst_g$nudge_y[ix] <- -2 * 0.15 * y_range

ix2 <- prueba_dst_g$distrito %in% 
  c("La Brea","Arenal","Colan","Paita","Bernal","Bellavista", "La Arena","Miguel Checa",
    "La Huaca")
prueba_dst_g$nudge_x[ix2] <- -2 * 0.2 * x_range
prueba_dst_g$nudge_y[ix2] <-  1 * 0.02 *  y_range

ix3<- prueba_dst_g$distrito %in% 
  c("Pariñas","El Alto","Mancora", "Los Organos","Tamarindo","Ignacio Escudero")
prueba_dst_g$nudge_x[ix3] <- -2 * 0.2 * x_range
prueba_dst_g$nudge_y[ix3] <- 1 * 0.2 *  y_range

ix4<- prueba_dst_g$distrito %in% 
  c("Buenos Aires","San Juan de Bigote")
prueba_dst_g$nudge_x[ix4] <- 1 * 0.02 *  x_range
prueba_dst_g$nudge_y[ix4] <- -2 * 0.2 *  y_range

ix5<- prueba_dst_g$distrito %in% 
  c("Lancones","Suyo","Paimas","Ayabaca")
prueba_dst_g$nudge_x[ix5] <- 2 * 0.2 *  x_range
prueba_dst_g$nudge_y[ix5] <- 2 * 0.2 *  y_range

ix6<- prueba_dst_g$distrito %in% 
  c("Santo Domingo","Lalaquiz","Huancabamba","Canchaque","San Miguel de El Faique",
    "Santa Catalina de Mossa")
prueba_dst_g$nudge_x[ix6] <- 1.5 * 0.2 *  x_range
prueba_dst_g$nudge_y[ix6] <- 1 * 0.02 *  y_range

ix7<- prueba_dst_g$distrito %in% c("Sechura")
prueba_dst_g$nudge_x[ix7] <- -1.5 * 0.15 * x_range
prueba_dst_g$nudge_y[ix7] <- -1.5 * 0.15 * y_range


plot_dst_g<-  ggplot()+
  geom_sf(data=sudamerica, fill="antiquewhite")+
  geom_text_repel(data=sudamerica,
                  aes(x=coords_x, y=coords_y, label=pais))+
  geom_sf(data=departamentos)+
  geom_text_repel(data=dpto_sel,
                  aes(x=coords_x, y=coords_y, label=departamen))+
  geom_sf(data=prueba_dst_g,aes(fill=incidence))+
  geom_text_repel(data=prueba_dst_g,
                  aes(x=coords_x, y=coords_y, label=distrito),
                  nudge_x = prueba_dst_g$nudge_x,
                  nudge_y = prueba_dst_g$nudge_y,
                  size=2.5, segment.alpha = 0.5)+
  scale_fill_gradientn(colors=brewer.pal(9,"BuPu"))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))+
  ggtitle("Severe dengue") + 
  xlab("Longitude") + ylab("Latitude") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"), 
                         style = north_arrow_fancy_orienteering) + 
  annotate(geom = "text", x = -81.5, y = -6.5, label = "Pacific ocean", 
           fontface = "italic", color = "grey22", size = 4)+
  coord_sf(xlim = c(-82, -79), ylim = c(-7,-3), expand = T)+
  annotation_scale() 

ggsave("dengue1016_g.png", width=330,height=210,units="mm")

plot_dst_deng3<-ggarrange(plot_dst_ssa,plot_dst_csa,plot_dst_g,
                          ncol=3,nrow=1,
                          labels=c("A","B","C"))
ggsave("dengue1016_dst_deng3.png")



# Choropleth map by year--------------------------------------------------------------------------
prueba_dst_ssa_f<-prueba3 %>% group_by(year,ubigeo) %>% summarise(incidence=sum(incidence))
prueba_dst_ssa_f<-left_join(prueba_dst_ssa_f,peru_piura,by=("ubigeo"))   
prueba_dst_ssa_f<-prueba_dst_ssa_f %>% 
                  filter(ubigeo>200000 & ubigeo<210000)
prueba_dst_ssa_f<-st_as_sf(prueba_dst_ssa_f) %>% 
  mutate(centroid = purrr::map(geometry, st_centroid),
         coords = purrr::map(centroid, st_coordinates),
         coords_x = purrr::map_dbl(coords, 1),
         coords_y = purrr::map_dbl(coords, 2)) %>%
  as_tibble() %>%
  st_as_sf()


plot_dst_ssa_f<-  ggplot()+
  geom_sf(data=sudamerica, fill="antiquewhite")+
  geom_text_repel(data=sudamerica,
                  aes(x=coords_x, y=coords_y, label=pais),
                  size=2.5)+
  geom_sf(data=departamentos)+
  geom_text_repel(data=dpto_sel,
                  aes(x=coords_x, y=coords_y, label=departamen),
                  size=2)+
  geom_sf(data=prueba_dst_ssa_f,aes(fill=incidence))+
  scale_fill_gradientn(colors=brewer.pal(9,"BuPu"))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))+
  ggtitle("Dengue without alarm symptoms") + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-82, -79), ylim = c(-7,-3), expand = T, label_graticule = "NE")+
  scale_x_continuous(
    breaks = waiver(),
    labels = c("82°W", "","81°W","","80°W","","79°W"))+
  annotation_scale()+
  facet_wrap(~year)

ggsave("dengue1016_ssa_f.png", width=330,height=210,units="mm")


prueba_dst_csa_f<-prueba4 %>% group_by(year,ubigeo) %>% summarise(incidence=sum(incidence))
prueba_dst_csa_f<-left_join(prueba_dst_csa_f,peru_piura,by=("ubigeo"))   
prueba_dst_csa_f<-prueba_dst_csa_f %>% 
  filter(ubigeo>200000 & ubigeo<210000)
prueba_dst_csa_f<-st_as_sf(prueba_dst_csa_f) %>% 
  mutate(centroid = purrr::map(geometry, st_centroid),
         coords = purrr::map(centroid, st_coordinates),
         coords_x = purrr::map_dbl(coords, 1),
         coords_y = purrr::map_dbl(coords, 2)) %>%
  as_tibble() %>%
  st_as_sf()


plot_dst_csa_f<-  ggplot()+
  geom_sf(data=sudamerica, fill="antiquewhite")+
  geom_text_repel(data=sudamerica,
                  aes(x=coords_x, y=coords_y, label=pais),
                  size=2.5)+
  geom_sf(data=departamentos)+
  geom_text_repel(data=dpto_sel,
                  aes(x=coords_x, y=coords_y, label=departamen),
                  size=2)+
  geom_sf(data=prueba_dst_csa_f,aes(fill=incidence))+
  scale_fill_gradientn(colors=brewer.pal(9,"BuPu"))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))+
  ggtitle("Dengue with alarm symptoms") + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-82, -79), ylim = c(-7,-3), expand = T, label_graticule = "NE")+
  scale_x_continuous(
    breaks = waiver(),
    labels = c("82°W", "","81°W","","80°W","","79°W"))+
  annotation_scale()+
  facet_wrap(~year)

ggsave("dengue1016_csa_f.png", width=330,height=210,units="mm")


prueba_dst_g_f<-prueba2 %>% group_by(year,ubigeo) %>% summarise(incidence=sum(incidence))
prueba_dst_g_f<-left_join(prueba_dst_g_f,peru_piura,by=("ubigeo"))   
prueba_dst_g_f<-prueba_dst_g_f %>% 
  filter(ubigeo>200000 & ubigeo<210000)
prueba_dst_g_f<-st_as_sf(prueba_dst_g_f) %>% 
  mutate(centroid = purrr::map(geometry, st_centroid),
         coords = purrr::map(centroid, st_coordinates),
         coords_x = purrr::map_dbl(coords, 1),
         coords_y = purrr::map_dbl(coords, 2)) %>%
  as_tibble() %>%
  st_as_sf()


plot_dst_csa_f<-  ggplot()+
  geom_sf(data=sudamerica, fill="antiquewhite")+
  geom_text_repel(data=sudamerica,
                  aes(x=coords_x, y=coords_y, label=pais),
                  size=2.5)+
  geom_sf(data=departamentos)+
  geom_text_repel(data=dpto_sel,
                  aes(x=coords_x, y=coords_y, label=departamen),
                  size=2)+
  geom_sf(data=prueba_dst_g_f,aes(fill=incidence))+
  scale_fill_gradientn(colors=brewer.pal(9,"BuPu"))+
  theme_bw()+
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"))+
  ggtitle("Severe dengue") + 
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-82, -79), ylim = c(-7,-3), expand = T, label_graticule = "NE")+
  scale_x_continuous(
    breaks = waiver(),
    labels = c("82°W", "","81°W","","80°W","","79°W"))+
  annotation_scale()+
  facet_wrap(~year)

ggsave("dengue1016_g_f.png", width=330,height=210,units="mm")


# Ovitraps data--------------------------------------------------------------------------

ovitraps17<-read_dta("Piura 2017 panel.dta")  %>% as_factor() %>% mutate_if(is.character,as.factor) %>% 
  clean_names() %>% 
  mutate(
    id=as.integer(id), semana=as.integer(semana), orden=as.integer(orden), 
    cod_unico=as.integer(cod_unico),ubigeo=as.integer(ubigeo),cord_x=as.numeric(cord_x), longitud_w=as.numeric(x_corregid),
    cord_y=as.numeric(cord_y),latitud_s=as.numeric(y_corregid),n_huevos=as.integer(numero_hue),fecha=as.character(fecha),
    codigo_ovi=as.factor(codigo_ovi),hoja=as.factor(hoja),distrito=as.factor(distrito),
    localidad=as.factor(localidad),codigoovit=as.factor(codigoovit),direccion=as.character(direccion),
    altitud=as.integer(altitud)
  ) %>% 
  select(-x_corregid,-y_corregid,-numero_hue) #%>% 
  #st_as_sf(coords=c("cord_x","cord_y"), crs=4326)  
write_excel_csv(ovitraps17, "Piura 2017 panel.csv")

df_ovitraps_long_17<-ovitraps17 %>% select(id, longitud_w, latitud_s, fecha, n_huevos) 



ovi18_comunidad<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 52 - 2018.xlsx", 
                            sheet = "COMUNIDAD SALUDABLE",
                            range=cell_rows(1:67)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovi18_statere<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 52 - 2018.xlsx", 
                            sheet = "SANTA TERESITA",
                            range=cell_rows(1:27)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovi18_vprima<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 52 - 2018.xlsx", 
                          sheet = "VILLA PRIMAVERA",
                          range=cell_rows(1:19)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovi18_nueveoct<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 52 - 2018.xlsx", 
                         sheet = "09 DE OCTUBRE",
                         range=cell_rows(1:45)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovi18_elobrero<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 52 - 2018.xlsx", 
                           sheet = "EL OBRERO",
                           range=cell_rows(1:22)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovi18_nvosullana<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 52 - 2018.xlsx", 
                           sheet = "NUEVO SULLANA",
                           range=cell_rows(1:23)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovi18_bellavista<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 52 - 2018.xlsx", 
                             sheet = "BELLAVISTA",
                             range=cell_rows(1:43)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovitraps18<- bind_rows(list(ovi18_comunidad,ovi18_statere,ovi18_vprima,ovi18_nueveoct,ovi18_elobrero,
                            ovi18_nvosullana,ovi18_bellavista))
ovitraps18<-  ovitraps18 %>% 
    mutate(
      se_1=as.integer(na_if(se_1,"PERD")),se_2=as.integer(na_if(se_2,"PERD")),se_3=as.integer(na_if(se_3,"PERD")),
      se_4=as.integer(na_if(se_4,"PERD")),se_5=as.integer(na_if(se_5,"PERD")),se_6=as.integer(na_if(se_6,"PERD")),
      se_7=as.integer(na_if(se_7,"PERD")),se_8=as.integer(na_if(se_8,"PERD")),se_9=as.integer(na_if(se_9,"PERD")),
      se_10=as.integer(na_if(se_10,"PERD")),se_11=as.integer(na_if(se_11,"PERD")),se_12=as.integer(na_if(se_12,"PERD")),
      se_13=as.integer(na_if(se_13,"PERD")),se_14=as.integer(na_if(se_14,"PERD")),se_15=as.integer(na_if(se_15,"PERD")),
      se_16=as.integer(na_if(se_16,"PERD")),se_17=as.integer(na_if(se_17,"PERD")),se_18=as.integer(na_if(se_18,"PERD")),
      se_19=as.integer(na_if(se_19,"PERD")),se_20=as.integer(na_if(se_20,"PERD")),se_21=as.integer(na_if(se_21,"PERD")),
      se_22=as.integer(na_if(se_22,"PERD")),se_23=as.integer(na_if(se_23,"PERD")),se_24=as.integer(na_if(se_24,"PERD")),
      se_25=as.integer(na_if(se_25,"PERD")),se_26=as.integer(na_if(se_26,"PERD")),se_27=as.integer(na_if(se_27,"PERD")),
      se_28=as.integer(na_if(se_28,"PERD")),se_29=as.integer(na_if(se_29,"PERD")),se_30=as.integer(na_if(se_30,"PERD")),
      se_31=as.integer(na_if(se_31,"PERD")),se_32=as.integer(na_if(se_32,"PERD")),se_33=as.integer(na_if(se_33,"PERD")),
      se_34=as.integer(na_if(se_34,"PERD")),se_35=as.integer(na_if(se_35,"PERD")),se_36=as.integer(na_if(se_36,"PERD")),
      se_37=as.integer(na_if(se_37,"PERD")),se_38=as.integer(na_if(se_38,"PERD")),se_39=as.integer(na_if(se_39,"PERD")),
      se_40=as.integer(na_if(se_40,"PERD")),se_41=as.integer(na_if(se_41,"PERD")),se_42=as.integer(na_if(se_42,"PERD")),
      se_43=as.integer(na_if(se_43,"PERD")),se_44=as.integer(na_if(se_44,"PERD")),se_45=as.integer(na_if(se_45,"PERD")),
      se_46=as.integer(na_if(se_46,"PERD")),se_47=as.integer(na_if(se_47,"PERD")),se_48=as.integer(na_if(se_48,"PERD")),
      se_49=as.integer(na_if(se_49,"PERD")),se_50=as.integer(na_if(se_50,"PERD")),se_51=as.integer(na_if(se_51,"PERD")),
      se_52=as.integer(na_if(se_52,"PERD"))
      )
ovitraps18<-  ovitraps18 %>% 
  mutate(
    se_1=as.integer(na_if(se_1,"ANUL")),se_2=as.integer(na_if(se_2,"ANUL")),se_3=as.integer(na_if(se_3,"ANUL")),
    se_4=as.integer(na_if(se_4,"ANUL")),se_5=as.integer(na_if(se_5,"ANUL")),se_6=as.integer(na_if(se_6,"ANUL")),
    se_7=as.integer(na_if(se_7,"ANUL")),se_8=as.integer(na_if(se_8,"ANUL")),se_9=as.integer(na_if(se_9,"ANUL")),
    se_10=as.integer(na_if(se_10,"ANUL")),se_11=as.integer(na_if(se_11,"ANUL")),se_12=as.integer(na_if(se_12,"ANUL")),
    se_13=as.integer(na_if(se_13,"ANUL")),se_14=as.integer(na_if(se_14,"ANUL")),se_15=as.integer(na_if(se_15,"ANUL")),
    se_16=as.integer(na_if(se_16,"ANUL")),se_17=as.integer(na_if(se_17,"ANUL")),se_18=as.integer(na_if(se_18,"ANUL")),
    se_19=as.integer(na_if(se_19,"ANUL")),se_20=as.integer(na_if(se_20,"ANUL")),se_21=as.integer(na_if(se_21,"ANUL")),
    se_22=as.integer(na_if(se_22,"ANUL")),se_23=as.integer(na_if(se_23,"ANUL")),se_24=as.integer(na_if(se_24,"ANUL")),
    se_25=as.integer(na_if(se_25,"ANUL")),se_26=as.integer(na_if(se_26,"ANUL")),se_27=as.integer(na_if(se_27,"ANUL")),
    se_28=as.integer(na_if(se_28,"ANUL")),se_29=as.integer(na_if(se_29,"ANUL")),se_30=as.integer(na_if(se_30,"ANUL")),
    se_31=as.integer(na_if(se_31,"ANUL")),se_32=as.integer(na_if(se_32,"ANUL")),se_33=as.integer(na_if(se_33,"ANUL")),
    se_34=as.integer(na_if(se_34,"ANUL")),se_35=as.integer(na_if(se_35,"ANUL")),se_36=as.integer(na_if(se_36,"ANUL")),
    se_37=as.integer(na_if(se_37,"ANUL")),se_38=as.integer(na_if(se_38,"ANUL")),se_39=as.integer(na_if(se_39,"ANUL")),
    se_40=as.integer(na_if(se_40,"ANUL")),se_41=as.integer(na_if(se_41,"ANUL")),se_42=as.integer(na_if(se_42,"ANUL")),
    se_43=as.integer(na_if(se_43,"ANUL")),se_44=as.integer(na_if(se_44,"ANUL")),se_45=as.integer(na_if(se_45,"ANUL")),
    se_46=as.integer(na_if(se_46,"ANUL")),se_47=as.integer(na_if(se_47,"ANUL")),se_48=as.integer(na_if(se_48,"ANUL")),
    se_49=as.integer(na_if(se_49,"ANUL")),se_50=as.integer(na_if(se_50,"ANUL")),se_51=as.integer(na_if(se_51,"ANUL")),
    se_52=as.integer(na_if(se_52,"ANUL"))
    )
ovitraps18<- ovitraps18 %>% 
  mutate(longitud_w=as.numeric(longitud_w), latitud_s=as.numeric(latitud_s), id=row_number()) %>% 
  select(id, everything())

ovitraps18_long<- ovitraps18 %>% 
          pivot_longer(cols=starts_with("se_"),
                       names_to = "semana",
                       values_to = "n_huevos",
                       values_drop_na = F
                       )
ovitraps18_long <- ovitraps18_long %>% 
          mutate(semana=as.numeric(str_remove_all(semana,"se_")), ano=as.numeric(2018),fecha=NA)

ovi18_week <-epiweekToDate(ovitraps18_long$ano,ovitraps18_long$semana)
ovitraps18_long$fecha <- ovi18_week$d0
ovitraps18_long <- ovitraps18_long %>% mutate(fecha=as.character(fecha))
write_excel_csv(ovitraps18_long, "Piura 2018 panel.csv")



ovi19_comunidad<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 32 - 2019.xlsx", 
                            sheet = "COMUNIDAD SALUDABLE",
                            range=cell_rows(1:67)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovi19_statere<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 32 - 2019.xlsx", 
                          sheet = "SANTA TERESITA",
                          range=cell_rows(1:27)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovi19_vprima<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 32 - 2019.xlsx", 
                         sheet = "VILLA PRIMAVERA",
                         range=cell_rows(1:19)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovi19_nueveoct<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 32 - 2019.xlsx", 
                           sheet = "09 DE OCTUBRE",
                           range=cell_rows(1:45)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovi19_elobrero<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 32 - 2019.xlsx", 
                           sheet = "EL OBRERO",
                           range=cell_rows(1:22)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovi19_nvosullana<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 32 - 2019.xlsx", 
                             sheet = "NUEVO SULLANA",
                             range=cell_rows(1:23)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovi19_bellavista<- read_xlsx("datasets/OVITRAMPAS SRSLCC SE 01 al 32 - 2019.xlsx", 
                             sheet = "BELLAVISTA",
                             range=cell_rows(1:43)) %>% 
                  clean_names() %>% 
                  mutate_if(is.numeric,as.character)
ovitraps19<- bind_rows(list(ovi19_comunidad,ovi19_statere,ovi19_vprima,ovi19_nueveoct,ovi19_elobrero,
                            ovi19_nvosullana,ovi19_bellavista))
ovitraps19<-  ovitraps19 %>% 
  mutate(
    se_1=as.integer(na_if(se_1,"PERD")),se_2=as.integer(na_if(se_2,"PERD")),se_3=as.integer(na_if(se_3,"PERD")),
    se_4=as.integer(na_if(se_4,"PERD")),se_5=as.integer(na_if(se_5,"PERD")),se_6=as.integer(na_if(se_6,"PERD")),
    se_7=as.integer(na_if(se_7,"PERD")),se_8=as.integer(na_if(se_8,"PERD")),se_9=as.integer(na_if(se_9,"PERD")),
    se_10=as.integer(na_if(se_10,"PERD")),se_11=as.integer(na_if(se_11,"PERD")),se_12=as.integer(na_if(se_12,"PERD")),
    se_13=as.integer(na_if(se_13,"PERD")),se_14=as.integer(na_if(se_14,"PERD")),se_15=as.integer(na_if(se_15,"PERD")),
    se_16=as.integer(na_if(se_16,"PERD")),se_17=as.integer(na_if(se_17,"PERD")),se_18=as.integer(na_if(se_18,"PERD")),
    se_19=as.integer(na_if(se_19,"PERD")),se_20=as.integer(na_if(se_20,"PERD")),se_21=as.integer(na_if(se_21,"PERD")),
    se_22=as.integer(na_if(se_22,"PERD")),se_23=as.integer(na_if(se_23,"PERD")),se_24=as.integer(na_if(se_24,"PERD")),
    se_25=as.integer(na_if(se_25,"PERD")),se_26=as.integer(na_if(se_26,"PERD")),se_27=as.integer(na_if(se_27,"PERD")),
    se_28=as.integer(na_if(se_28,"PERD")),se_29=as.integer(na_if(se_29,"PERD")),se_30=as.integer(na_if(se_30,"PERD")),
    se_31=as.integer(na_if(se_31,"PERD")),se_32=as.integer(na_if(se_32,"PERD")),se_33=as.integer(na_if(se_33,"PERD")),
    se_34=as.integer(na_if(se_34,"PERD")),se_35=as.integer(na_if(se_35,"PERD")),se_36=as.integer(na_if(se_36,"PERD")),
    se_37=as.integer(na_if(se_37,"PERD")),se_38=as.integer(na_if(se_38,"PERD")),se_39=as.integer(na_if(se_39,"PERD")),
    se_40=as.integer(na_if(se_40,"PERD")),se_41=as.integer(na_if(se_41,"PERD")),se_42=as.integer(na_if(se_42,"PERD")),
    se_43=as.integer(na_if(se_43,"PERD")),se_44=as.integer(na_if(se_44,"PERD")),se_45=as.integer(na_if(se_45,"PERD")),
    se_46=as.integer(na_if(se_46,"PERD")),se_47=as.integer(na_if(se_47,"PERD")),se_48=as.integer(na_if(se_48,"PERD")),
    se_49=as.integer(na_if(se_49,"PERD")),se_50=as.integer(na_if(se_50,"PERD")),se_51=as.integer(na_if(se_51,"PERD")),
    se_52=as.integer(na_if(se_52,"PERD"))
    )
ovitraps19<-  ovitraps19 %>% 
  mutate(
    se_1=as.integer(na_if(se_1,"ANUL")),se_2=as.integer(na_if(se_2,"ANUL")),se_3=as.integer(na_if(se_3,"ANUL")),
    se_4=as.integer(na_if(se_4,"ANUL")),se_5=as.integer(na_if(se_5,"ANUL")),se_6=as.integer(na_if(se_6,"ANUL")),
    se_7=as.integer(na_if(se_7,"ANUL")),se_8=as.integer(na_if(se_8,"ANUL")),se_9=as.integer(na_if(se_9,"ANUL")),
    se_10=as.integer(na_if(se_10,"ANUL")),se_11=as.integer(na_if(se_11,"ANUL")),se_12=as.integer(na_if(se_12,"ANUL")),
    se_13=as.integer(na_if(se_13,"ANUL")),se_14=as.integer(na_if(se_14,"ANUL")),se_15=as.integer(na_if(se_15,"ANUL")),
    se_16=as.integer(na_if(se_16,"ANUL")),se_17=as.integer(na_if(se_17,"ANUL")),se_18=as.integer(na_if(se_18,"ANUL")),
    se_19=as.integer(na_if(se_19,"ANUL")),se_20=as.integer(na_if(se_20,"ANUL")),se_21=as.integer(na_if(se_21,"ANUL")),
    se_22=as.integer(na_if(se_22,"ANUL")),se_23=as.integer(na_if(se_23,"ANUL")),se_24=as.integer(na_if(se_24,"ANUL")),
    se_25=as.integer(na_if(se_25,"ANUL")),se_26=as.integer(na_if(se_26,"ANUL")),se_27=as.integer(na_if(se_27,"ANUL")),
    se_28=as.integer(na_if(se_28,"ANUL")),se_29=as.integer(na_if(se_29,"ANUL")),se_30=as.integer(na_if(se_30,"ANUL")),
    se_31=as.integer(na_if(se_31,"ANUL")),se_32=as.integer(na_if(se_32,"ANUL")),se_33=as.integer(na_if(se_33,"ANUL")),
    se_34=as.integer(na_if(se_34,"ANUL")),se_35=as.integer(na_if(se_35,"ANUL")),se_36=as.integer(na_if(se_36,"ANUL")),
    se_37=as.integer(na_if(se_37,"ANUL")),se_38=as.integer(na_if(se_38,"ANUL")),se_39=as.integer(na_if(se_39,"ANUL")),
    se_40=as.integer(na_if(se_40,"ANUL")),se_41=as.integer(na_if(se_41,"ANUL")),se_42=as.integer(na_if(se_42,"ANUL")),
    se_43=as.integer(na_if(se_43,"ANUL")),se_44=as.integer(na_if(se_44,"ANUL")),se_45=as.integer(na_if(se_45,"ANUL")),
    se_46=as.integer(na_if(se_46,"ANUL")),se_47=as.integer(na_if(se_47,"ANUL")),se_48=as.integer(na_if(se_48,"ANUL")),
    se_49=as.integer(na_if(se_49,"ANUL")),se_50=as.integer(na_if(se_50,"ANUL")),se_51=as.integer(na_if(se_51,"ANUL")),
    se_52=as.integer(na_if(se_52,"ANUL"))
    )
ovitraps19<- ovitraps19 %>% 
  mutate(longitud_w=as.numeric(longitud_w), latitud_s=as.numeric(latitud_s), id=row_number()) %>% 
  select(id, everything())

ovitraps19_long<- ovitraps19 %>% 
  pivot_longer(cols=starts_with("se_"),
               names_to = "semana",
               values_to = "n_huevos",
               values_drop_na = F
  )
ovitraps19_long <- ovitraps19_long %>% 
  mutate(semana=as.numeric(str_remove_all(semana,"se_")), ano=as.numeric(2019),fecha=NA)

ovi19_week <-epiweekToDate(ovitraps19_long$ano,ovitraps19_long$semana)
ovitraps19_long$fecha <- ovi19_week$d0
ovitraps19_long <- ovitraps19_long %>% mutate(fecha=as.character(fecha))
write_excel_csv(ovitraps19_long, "Piura 2019 panel.csv")


ovitraps1819_long<- bind_rows(ovitraps18_long,ovitraps19_long)
write_excel_csv(ovitraps1819_long, "Piura 2018-19 panel.csv")

df_ovitraps_long_1819<-ovitraps1819_long %>% select(id, longitud_w, latitud_s, fecha, n_huevos)

df_ovitraps_long_1719<- bind_rows(df_ovitraps_long_17,df_ovitraps_long_1819)
write_excel_csv(df_ovitraps_long_1719, "Piura 2017-19 panel.csv")

ovitraps1819_long<- ovitraps1819_long %>% mutate(fecha=parse_date_time(fecha, "Y m d"))


df_count<-df_ovitraps_long_1719 %>% count(id)





src <- osmsource_api(url = "https://api.openstreetmap.org/api/0.6/")
  bb <- corner_bbox(-80.7335,-4.9267,-80.6503,-4.8747)
ctown <- get_osm(bb, source = src)
plot(ctown)
points(-1.53492, 53.81934, col = "red", lwd = 5)

lat <- c(-4.9267,-4.8747)
long <- c(-80.7335,-80.6503)
bbox <- make_bbox(long,lat,f=0.05)
sullana_map <- get_map(bbox,source="osm", crop = T, scale=2) 
projection(sullana_map)<-CRS("+init=epsg:4326")
ggmap(sullana_map)+
geom_sf(data=ovitraps17$geometry,
        inherit.aes =FALSE,
        color="black",
        fill="grey",
        alpha=.2,
        size=2)#+
labs(x="",y="")


sullana_map3 <- get_stamenmap(bbox =bb, zoom=12)


require(raster)
require(ggmap)


gmap <- get_map(location = c(5, 51), maptype = "hybrid", source = "google", 
                crop = FALSE, zoom = 6)



sullana_map2<-st_read (dsn="maps/sullana/map.osm")

departamentos <- st_read(dsn="maps/departamentos",layer="DEPARTAMENTO") %>% clean_names()



leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = ovitraps17,
              color = 'white',
              weight = 1.5,
              opacity = 1,
              fillColor = 'black',
              fillOpacity = .8,
              highlightOptions = highlightOptions(color = "#FFF1BE", 
                                                  weight = 5),
              popup = ~neighborhood_label)



