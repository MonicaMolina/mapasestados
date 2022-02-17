#Quintana Roo, Hidalgo,Durango
pacman::p_load(tidyverse,readxl, plyr, dplyr, ggplot2, tidyr, tidyverse, extrafont, gganimate, gapminder, plotly, shiny, sp, sf, foreign, readr, rgdal, ggrepel, gridExtra, grid, lattice, pdftools, tesseract, rgdal, tmap, raster)

#read legislative elections datasets

elecciones<- read_xlsx("CongresosLocalesMR.xlsx")
summary(congreso)

elecciones$winner<- as.factor(elecciones$winner)
elecciones$second<- as.factor(elecciones$second)
congreso <- elecciones[elecciones$election_type=="congress",]
AG21 <- congreso[congreso$id_state==1 & congreso$election_year==2021,]
AG18 <- congreso[congreso$id_state==1 & congreso$election_year==2018,]
AG15 <- congreso[congreso$id_state==1 & congreso$election_year==2015,]
QR16 <- congreso[congreso$id_state== 23 & congreso$election_year==2016,]
QR19 <- congreso[congreso$id_state== 23 & congreso$election_year==2019,]
D21 <- congreso[congreso$id_state== 10 & congreso$election_year==2021,]
D18 <- congreso[congreso$id_state== 10 & congreso$election_year==2018,]
H21 <- congreso[congreso$id_state== 13 & congreso$election_year==2021,]
H18 <- congreso[congreso$id_state== 13 & congreso$election_year==2018,]
OX21 <- congreso[congreso$id_state== 20 & congreso$election_year==2021,]
OX18 <- congreso[congreso$id_state==20 & congreso$election_year==2018,]
TM21 <- congreso[congreso$id_state== 28 & congreso$election_year==2021,]
TM18 <- congreso[congreso$id_state==28 & congreso$election_year==2018,]
GQR <- elecciones[elecciones$id_state== 23 & elecciones$election_type=="governor",]
GH <- elecciones[elecciones$id_state== 13 & elecciones$election_type=="governor",]
GD <- elecciones[elecciones$id_state== 10 & elecciones$election_type=="governor",]
GA <- elecciones[elecciones$id_state== 1 & elecciones$election_type=="governor",]
GO <- elecciones[elecciones$id_state== 20 & elecciones$election_type=="governor",]
GT <- elecciones[elecciones$id_state== 28 & elecciones$election_type=="governor",]

Dtto <- st_read("DISTRITO_LOCAL.shp")

QR <- Dtto[Dtto$ENTIDAD == 23,]
D <- Dtto[Dtto$ENTIDAD == 10,]
H <- Dtto[Dtto$ENTIDAD == 13,]
A <- Dtto[Dtto$ENTIDAD == 1,]
O <- Dtto[Dtto$ENTIDAD == 20,]
TM <- Dtto[Dtto$ENTIDAD == 28,]

#

#congreso 2016

MQR19 <- merge(QR, QR19, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapaqroo19 <- MQR19 %>% ggplot() + geom_sf(fill= ifelse( MQR19$winner == "PAN", "#001D82", ifelse( MQR19$winner=="PRI","#42A03F", "#B1191E")), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapaqroo19

mapzoom2 <- MQR19 %>% filter(DISTRITO_L >= 2 & DISTRITO_L <= 7 ) %>% ggplot() + geom_sf(fill= "#B1191E", color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapa2 <- grid.arrange( grobs = list(mapaqroo19, mapzoom2),layout_matrix = rbind(c(1,  1, 1),
                                                                                 c(1, 1, 2),
                                                                                 c(1, 1, 2)))

mapa2

MQR16 <- merge(QR, QR16, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)
mapaqroo16 <- MQR16 %>% ggplot() + geom_sf(fill= ifelse( MQR19$winner == "PAN", "#001D82","#42A03F"), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapaqroo16

mapzoom <- MQR16 %>% filter(DISTRITO_L >= 2 & DISTRITO_L <= 7 ) %>% ggplot() + geom_sf(fill= "#42A03F", color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapa <- grid.arrange( grobs = list(mapaqroo16, mapzoom),layout_matrix = rbind(c(1,  1, 1),
                                                                               c(1, 1, 2),
                                                                               c(1, 1, 2)))

mapa

GQR16 <- merge(QR, GQR, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapaqroogob <- GQR16 %>% ggplot() + geom_sf(fill= ifelse( GQR16$winner == "PAN", "#001D82", ifelse( GQR16$winner=="PRI","#42A03F", "#B1191E")), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapaqroogob

mapzoomgob <- GQR16 %>% filter(DISTRITO_L >= 2 & DISTRITO_L <= 7 ) %>% ggplot() + geom_sf(fill= "#B1191E", color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapagob <- grid.arrange( grobs = list(mapaqroogob, mapzoomgob),layout_matrix = rbind(c(1,  1, 1),
                                                                                c(1, 1, 2),
                                                                                c(1, 1, 2)))

mapagob

#Hidalgo

HGO21 <- merge(H, H21, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapah21 <- HGO21 %>% ggplot() + geom_sf(fill= ifelse( HGO21$winner == "PAN", "#001D82", ifelse( HGO21$winner=="PRI","#42A03F", "#B1191E")), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapah21

HGO18 <- merge(H, H18, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapah18 <- HGO18 %>% ggplot() + geom_sf(fill= ifelse( HGO18$winner=="PRI","#42A03F", "#B1191E"), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapah18

HGob <- merge(H, GH, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapahgogob <- HGob %>% ggplot() + geom_sf(fill= ifelse( HGob$winner == "PAN", "#001D82", ifelse( HGob$winner=="PRI","#42A03F", ifelse( HGob=="Partido Duranguense PD","grey","#B1191E"))), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapahgogob

#Durango
DGO21 <- merge(D, D21, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapadgo21 <- DGO21 %>% ggplot() + geom_sf(fill= ifelse( DGO21$winner == "PAN", "#001D82", ifelse( DGO21$winner=="PRI","#42A03F", "#B1191E")), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapadgo21

DGO18 <- merge(D, D18, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapadgo18 <- DGO18 %>% ggplot() + geom_sf(fill= ifelse( DGO18$winner == "PAN", "#001D82", ifelse( DGO18$winner=="PRI","#42A03F", ifelse( DGO18=="Partido Duranguense PD","grey","#B1191E"))), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapadgo18

DGOGob <- merge(D, GD, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapadgogob <- DGOGob %>% ggplot() + geom_sf(fill= ifelse( DGOGob$winner == "PAN", "#001D82", ifelse( DGOGob$winner=="PRI","#42A03F", ifelse( DGOGob=="Partido Duranguense PD","grey","#B1191E"))), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapadgogob


#Aguascalientes

AG21 <- merge(A, AG21, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapaa21 <- AG21 %>% ggplot() + geom_sf(fill= ifelse( AG21$winner == "PAN", "#001D82", ifelse( AG21$winner=="PRI","#42A03F", "#B1191E")), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapaa21

AG18 <- merge(A, AG18, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapaa18 <- AG18 %>% ggplot() + geom_sf(fill= ifelse( AG18$winner == "PAN", "#001D82", ifelse( AG18$winner=="PRI","#42A03F", "#B1191E")), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapaa18

AGob <- merge(A, GA, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapaagsgob <- AGob %>% ggplot() + geom_sf(fill= ifelse( AGob$winner == "PAN", "#001D82", ifelse( AGob$winner=="PRI","#42A03F","#B1191E")), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapaagsgob

#Oaxaca

O21 <- merge(O, OX21, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapao21 <- O21 %>% ggplot() + geom_sf(fill= ifelse( O21$winner == "PAN", "#001D82", ifelse( O21$winner=="PRI","#42A03F", "#B1191E")), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapao21

O18 <- merge(O, OX18, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapao18 <- O18 %>% ggplot() + geom_sf(fill= ifelse( O18$winner == "PAN", "#001D82", ifelse( O18$winner=="PRI","#42A03F", "#B1191E")), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapao18

OGob <- merge(O, GO, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapaoxcgob <- OGob %>% ggplot() + geom_sf(fill= ifelse( OGob$winner == "PAN", "#001D82", ifelse( OGob$winner=="PRI","#42A03F", "#B1191E")), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapaoxcgob

#Tamaulipas

TM21 <- merge(TM, TM21, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapat21 <- TM21 %>% ggplot() + geom_sf(fill= ifelse( TM21$winner == "PAN", "#001D82", ifelse( TM21$winner=="PRI","#42A03F", "#B1191E")), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapat21

TM18 <- merge(TM, TM18, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapat18 <- TM18 %>% ggplot() + geom_sf(fill= ifelse( TM18$winner == "PAN", "#001D82", ifelse( TM18$winner=="PRI","#42A03F", "#B1191E")), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapat18

mapzoom <- TM18 %>% filter(DISTRITO_L==11) %>% ggplot() + geom_sf(fill= alpha("#B1191E")) + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + labs(title = "Distrito 11") + theme(plot.title = element_text(face = "bold", size = 6, family = "Lato", hjust = 0.5))

mapa <- grid.arrange( grobs = list(mapat18, mapzoom),layout_matrix = rbind(c(1,  1, 1),
                                                                       c(1, 1, 2),
                                                                       c(1, 1, 2)))


TGob <- merge(TM, GT, by.x="DISTRITO_L", by.y="id_district", all.x=TRUE, all.y=TRUE)

mapatamgob <- TGob %>% ggplot() + geom_sf(fill= ifelse( TGob$winner == "PAN", "#001D82", ifelse( TGob$winner=="PRI","#42A03F","#B1191E")), color="white") + theme_minimal() + theme(axis.title = element_blank(), axis.text=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

mapatamgob


#B1191E morena
#001D82 pan
#42A03F pri
