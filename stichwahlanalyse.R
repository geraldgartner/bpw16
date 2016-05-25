#Analyse der Stichwahl

library(ggthemes)
library(ggThemeAssist)
library(ggplot2)
library(dplyr)
library(tidyr)
library(git2r)
library(formatR)
library(scales)
library(grid)
library(extrafont)
library(corrplot)

#Unser Style


theme <- theme(plot.background = element_rect(fill = "gray97"), panel.grid.major = element_line(colour = "gray86", linetype = "dotted"), 
               panel.grid.minor = element_line(colour = "gray86", linetype = "dotted")) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        plot.background = element_rect(fill = "gray97", colour = "antiquewhite", size = 10, linetype = "solid")) +
  theme(axis.ticks = element_blank(), 
        axis.line = element_blank(),
        axis.title = element_text(vjust = 8), 
        panel.background = element_rect(fill = "grey97", linetype = "solid"), 
        plot.background = element_rect(colour = "gray97"), 
        plot.title = element_text(hjust=0, margin=unit(c(0,1,0.2,1), "cm")), 
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm")) +
  theme(axis.text=element_text(size=16))  


#Parteifarben festlegen
kandidatenfarben <- c("hofer" = "#7a8fcc","vdb" = "#548750","griss" = "#b398aa","hundstorfer" ="#b34848", "khol" = "#282828", "lugner" = "#bfb58e")
kandidaten <- c("hofer" = "Hofer", "khol" = "Khol","hundstorfer" = "Hundstorfer", "griss" = "Griss", "vdb" = "Van der Bellen", "lugner" = "Lugner")

library("googlesheets")
suppressPackageStartupMessages(library("dplyr"))

(my_sheets <- gs_ls())
gemeindedaten <- gs_key('1Lt137MmgPGeJugRguErIDNqjRbZojNsWWmUPTTOUPgs')

#Alle Worksheets aus dem GSheet Gemeindedaten importieren

demografie <- gs_read(gemeindedaten, ws = 'demografie2016', col_names = TRUE)
erwerbsstatus <- gs_read(gemeindedaten, ws = "erwerb2013", col_names = TRUE)
wahlgang1 <- gs_read(gemeindedaten, ws = "wahlgang1", col_names = TRUE)
wahlgang2 <- gs_read(gemeindedaten, ws = "wahlgang2", col_names = TRUE)
asyl <- gs_read(gemeindedaten, ws = "asyl", col_names = TRUE)
medianeinkommen <- gs_read(gemeindedaten, ws = "medianeinkommen", col_names = TRUE)

#Reshapen der Wahlergebnisse auf Long-Format

wahlgang1 <- wahlgang1 %>% gather(kandidat, ergebnis, griss:vdb)
wahlgang2 <- wahlgang2 %>% gather(kandidat, ergebnis, hofer:vdb)
wahlgang1$pct <- wahlgang1$ergebnis/wahlgang1$gueltig
wahlgang2$pct <- wahlgang2$ergebnis/wahlgang2$gueltig 

#Anlegen der Prozentspalten für die Korrelationen-Analyse
demografie$pctjunge <- demografie$`16_29`/demografie$`total_>16at`
demografie$pctjunge_m <- demografie$`16_29m`/demografie$`total_>16at`
demografie$pctjunge_w <- demografie$`16_29w`/demografie$`total_>16at`
demografie$pctalte <- demografie$`65plus`/demografie$`total_>16at`

demografie$pctnotATstb <- demografie$drittstaatsbuerger/demografie$bevgesamt

erwerbsstatus$pflichtschule_pct <- erwerbsstatus$pflichtschule/erwerbsstatus$bildung_total
erwerbsstatus$lehrabschluss_pct <- erwerbsstatus$lehrabschluss/erwerbsstatus$bildung_total
erwerbsstatus$mittlere_und_hoehere_schule_pct <- erwerbsstatus$mittlere_und_hoehere_schule/erwerbsstatus$bildung_total
erwerbsstatus$hochschule_u_akademie_pct <- erwerbsstatus$hochschule_u_akademie/erwerbsstatus$bildung_total

erwerbsstatus$arbeiter_pct <- erwerbsstatus$arbeiter/erwerbsstatus$erwerbstaetig
erwerbsstatus$angestellte_pct <- erwerbsstatus$pflichtschule/erwerbsstatus$erwerbstaetig
erwerbsstatus$selbststaendige_pct <- erwerbsstatus$pflichtschule/erwerbsstatus$erwerbstaetig

pctdemografie <- demografie[ , c("gkz", "urban", "pctjunge","pctjunge_m", "pctjunge_w", "pctalte", "pctnotATstb")]
pcterwerb <- erwerbsstatus [ , c("gkz", "pflichtschule_pct", "lehrabschluss_pct","mittlere_und_hoehere_schule_pct", "hochschule_u_akademie_pct", "arbeiter_pct", "angestellte_pct", "selbststaendige_pct" )]

#Mergen der Sheets für weitere Analyse
dem <- merge(x=pctdemografie, y=wahlgang2, by.x = "gkz", by.y = "gkz", incomparables = NA)
erwerb <- merge(x=pcterwerb, y=wahlgang2, by.x = "gkz", by.y = "gkz", incomparables = NA)
einkommen <- merge(x=medianeinkommen, y=wahlgang2, by.x = "gkz", by.y = "gkz", incomparables = NA)
asyl <- merge(asyl, wahlgang2, by.x = "gkz", by.y = "gkz", incomparables = NA)
asyl$aslymaerz <- as.numeric(gsub(",",".", asyl$aslymaerz))

#Filtern der Kandiaten vor der Korrelationsmatrix

demhofer <- filter(dem, dem$kandidat=="hofer")
demvdb <- filter(dem, dem$kandidat=="vdb" )

erwerbhofer <- filter(erwerb, dem$kandidat=="hofer")
erwerbvdb <- filter(erwerb, dem$kandidat=="vdb" )

demhofercor <- demhofer[, c(2:6, 14)]
demvdbcor <- demvdb[, c(2:6, 14)]

erwerbhofercor <- erwerbhofer[, c(2:8, 16)]
erwerbvdbcor <- erwerbvdb[, c(2:8, 16)]

#KORRELATIONEN-TABELLE FÜR HOFER

demhofercor <- cor(demhofercor, use="pairwise.complete.obs")
round(demhofercor, digits=2)

erwerbhofercor <- cor(erwerbhofercor, use="pairwise.complete.obs")
round(erwerbhofercor, digits=2)

#BESSERE FARBPALETTE
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(demhofercor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(200), addCoef.col="black",order="AOE")

as.matrix(as.data.frame(erwerbhofercor))
corrplot(erwerbhofercor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(200), addCoef.col="black", order="AOE")

#KORRELATIONEN-MATRIX FÜR VDB
demvdbcor <- cor(demvdbcor)
round(demvdbcor, digits=2)

erwerbvdbcor <- cor(erwerbvdbcor, use="pairwise.complete.obs")
round(erwerbvdbcor, digits=2)

# ============================================================================ #
# SCATTERPLOTTS DER BEZIEHUNGEN
# ============================================================================ #

arbeiterplot <- ggplot(erwerb, aes(x=arbeiter_pct, y=pct, colour=kandidat)) +
      geom_point(alpha=1/4) + 
      facet_grid(kandidat ~ ., labeller=labeller(.default=kandidaten)) +
      geom_smooth(method=lm)  +
      scale_y_continuous(labels = percent) +
      scale_x_continuous(labels = percent) +
      labs(x = "Stimmenanteil", y = "Anteil der Arbeiter") +
      ggtitle("Je mehr Arbeiter, \ndesto mehr Hofer-Wähler") +
      guides(fill=FALSE) +
#      scale_fill_manual(values = c("hofer"="#999999", "vdb"="#56B4E9")) +
      scale_colour_manual(values = c("hofer"="#7A8FCC", "vdb"="#548750")) +
      theme(strip.text.x = element_text(size=12), strip.background = element_rect(colour="grey86", linetype = "dotted", fill="grey97"),legend.position="none") +
      theme
  plot(arbeiterplot)

# ============================================================================ #
# WENN NUR LÄNDLICHE REGIONEN GEWÄHLT HÄTTEN
# ============================================================================ #


top_land <- tbl_df(dem) %>% group_by(kandidat) %>% top_n(10, urban==3)

topland <- top_land %>%
  group_by(kandidat) %>%
  summarise(Sumstimmen = sum(ergebnis), SumGueltig = sum(gueltig)) 

topland$prozent <- c((topland$Sumstimmen/topland$SumGueltig))

width = 494*2.54/96
height = 300*2.54/96
dpi = 92

top_land <- ggplot(topland, aes(x=reorder(kandidat, prozent), y=prozent, fill=kandidat)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "Anteil der Stimmen") +
  scale_fill_manual(values = kandidatenfarben) +
  guides(fill=FALSE) +
  geom_text(aes(label = paste(round(prozent*100,1),"%",sep=" ")), hjust= 1, color="white") +
  ggtitle("Wie ländliche Regionen gewählt hätten") +
  scale_x_discrete(labels=c("hofer" = "Hofer", "khol" = "Khol","hundstorfer" = "Hundstorfer", "griss" = "Griss", "vdb" = "Van der Bellen", "lugner", "lugner" = "Lugner")) 

plot(top_land)
ggsave("topland.pdf", useDingbats=FALSE)

# ============================================================================ #
# WENN NUR URBANE ZENTREN GEWÄHLT HÄTTEN
# ============================================================================ #


top_urban <- tbl_df(dem) %>% group_by(kandidat) %>% top_n(10, urban==1)

topurban <- top_urban %>%
  group_by(kandidat) %>%
  summarise(Sumstimmen = sum(ergebnis), SumGueltig = sum(gueltig)) 

topurban$prozent <- c((topurban$Sumstimmen/topurban$SumGueltig))

width = 494*2.54/96
height = 300*2.54/96
dpi = 92

topurban <- ggplot(topurban, aes(x=reorder(kandidat, prozent), y=prozent, fill=kandidat)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "Anteil der Stimmen") + 
  scale_fill_manual(values = kandidatenfarben) +
  guides(fill=FALSE) +
  geom_text(aes(label = paste(round(prozent*100,1),"%",sep=" ")), hjust= 1, color="white") +
  ggtitle("Wie Großstädte gewählt hätten") +
  scale_x_discrete(labels=c("hofer" = "Hofer", "khol" = "Khol","hundstorfer" = "Hundstorfer", "griss" = "Griss", "vdb" = "Van der Bellen", "lugner", "lugner" = "Lugner")) 

plot(topurban)
ggsave("topurban.pdf", useDingbats=FALSE)


# ============================================================================ #
# WENN NUR STÄDTISCHE REGIONEN GEWÄHLT HÄTTEN
# ============================================================================ #


top_stadt <- tbl_df(dem) %>% group_by(kandidat) %>% top_n(10, urban==2)

topstadt <- top_stadt %>%
  group_by(kandidat) %>%
  summarise(Sumstimmen = sum(ergebnis), SumGueltig = sum(gueltig)) 

topstadt$prozent <- c((topstadt$Sumstimmen/topstadt$SumGueltig))

width = 494*2.54/96
height = 300*2.54/96
dpi = 92

topstadt <- ggplot(topstadt, aes(x=reorder(kandidat, prozent), y=prozent, fill=kandidat)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "Anteil der Stimmen") +
  scale_fill_manual(values = kandidatenfarben) +
  guides(fill=FALSE) +
  geom_text(aes(label = paste(round(prozent*100,1),"%",sep=" ")), hjust= 1, color="white") +
  ggtitle("Wie städtische Regionen gewählt hätten") +
  scale_x_discrete(labels=c("hofer" = "Hofer", "khol" = "Khol","hundstorfer" = "Hundstorfer", "griss" = "Griss", "vdb" = "Van der Bellen", "lugner", "lugner" = "Lugner")) 

plot(topstadt)
ggsave("topstadt.pdf", useDingbats=FALSE)

