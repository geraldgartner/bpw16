library(ggthemes)
library(ggThemeAssist)
library(ggplot2)
library(dplyr)
library(tidyr)
library(git2r)
library(formatR)

#Unser Style


theme <- theme(plot.background = element_rect(fill = "gray97"), panel.grid.major = element_line(colour = "gray86", linetype = "dotted"), 
               panel.grid.minor = element_line(colour = "gray86", linetype = "dotted")) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        plot.background = element_rect(fill = "gray97", colour = "antiquewhite", size = 10, linetype = "solid")) +
  theme(axis.ticks = element_blank(), 
        axis.line = element_line(colour = "gray97", 
                                 size = 0), axis.title = element_text(vjust = 8), 
        axis.text = element_text(family = "Georgia", 
                                 size = 14, vjust = 0.25), 
        panel.background = element_rect(fill = "grey97", linetype = "solid"), 
        plot.background = element_rect(colour = "gray97"), 
        plot.title = element_text(hjust=0, margin=unit(c(0,1,0.2,1), "cm")), 
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm")) 

library("googlesheets")
suppressPackageStartupMessages(library("dplyr"))

(my_sheets <- gs_ls())
gemeindedaten <- gs_key('1Lt137MmgPGeJugRguErIDNqjRbZojNsWWmUPTTOUPgs')

#Alle Worksheets aus dem GSheet Gemeindedaten importieren

demografie <- gs_read(gemeindedaten, ws = 'demografie2016', col_names = TRUE)
erwerbsstatus <- gs_read(gemeindedaten, ws = "erwerb2013", col_names = TRUE)
wahlgang1 <- gs_read(gemeindedaten, ws = "wahlgang1", col_names = TRUE)

#Reshapen der Wahlergebnisse auf Long-Format

wahlgang1 <- wahlgang1 %>% gather(kandidat, ergebnis, griss:vdb)
wahlgang1$pct <- wahlgang1$ergebnis/wahlgang1$gueltig 


#Mergen der Sheets für weitere Analyse
dem <- merge(wahlgang1, demografie, by.x = "gkz", by.y = "gkz")

demhofer <- filter(dem, dem$kandidat=="hofer")
demvdb <- filter(dem, dem$kandidat=="vdb" )
demgriss <- filter(dem, dem$kandidat=="griss" )
demhund <- filter(dem, dem$kandidat=="hundstorfer" )
demkhol <- filter(dem, dem$kandidat=="khol" )

#Menschen mit Drittstaatsangehörigkeit wählen XY

drittstaatchart <- ggplot(dem, aes(dem$drittstaatsbuerger/dem$bevgesamt, dem$ergebnis/dem$gueltig)) + 
  geom_point(aes(colour=dem$kandidat), alpha = 0.2) + 
  geom_smooth(method=lm) + 
  theme +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="white",lwd=4) +
  labs(x = "Prozent der Stimmen", y = "Anteil der Ausländer") + 
  ggtitle("Stimmen vs. Anteil der Ausländer")
  

plot(drittstaatchart)
