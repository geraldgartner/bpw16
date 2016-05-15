library(ggthemes)
library(ggThemeAssist)
library(ggplot2)
library(dplyr)
library(tidyr)
library(git2r)
library(formatR)
library(scales)
library(grid)

#Unser Style


theme <- theme(plot.background = element_rect(fill = "gray97"), panel.grid.major = element_line(colour = "gray86", linetype = "dotted"), 
               panel.grid.minor = element_line(colour = "gray86", linetype = "dotted")) + 
  theme(plot.title = element_text(size = 22, face = "bold"), 
        plot.background = element_rect(fill = "gray97", colour = "antiquewhite", size = 10, linetype = "solid")) +
  theme(axis.ticks = element_blank(), 
        axis.line = element_blank(),
        axis.title = element_text(vjust = 8), 
        axis.text = element_text(family = "Georgia", 
                                 size = 14, vjust = 0.25), 
        panel.background = element_rect(fill = "grey97", linetype = "solid"), 
        plot.background = element_rect(colour = "gray97"), 
        plot.title = element_text(hjust=0, margin=unit(c(0,1,0.2,1), "cm")), 
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm")) 

#Parteifarben festlegen
kandidatenfarben <- c("hofer" = "#7a8fcc","vdb" = "#548750","griss" = "#b398aa","hundstorfer" ="#b34848", "khol" = "#282828", "lugner" = "#bfb58e")


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
dem$aslymaerz <- as.numeric(gsub(",",".", dem$aslymaerz))

demhofer <- filter(dem, dem$kandidat=="hofer")
demvdb <- filter(dem, dem$kandidat=="vdb" )
demgriss <- filter(dem, dem$kandidat=="griss" )
demhund <- filter(dem, dem$kandidat=="hundstorfer" )
demkhol <- filter(dem, dem$kandidat=="khol" )

#Menschen mit Drittstaatsangehörigkeit wählen XY

drittstaatchart <- ggplot(dem, aes(dem$drittstaatsbuerger/dem$bevgesamt, dem$ergebnis/dem$gueltig, 
                                   colour=dem$kandidat)) + 
  geom_point(alpha = 0.2) + 
  theme +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent) +
  annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="white",lwd=4) +
  labs(x = "Anteil der Ausländer", y = "Anteil der Stimmen") + 
  ggtitle("Stimmen vs. Anteil der Ausländer") +
  facet_wrap(~kandidat, nrow = 3)


plot(drittstaatchart)


#asylkorrelation
asylchart <- ggplot(demhofer, aes(demhofer$aslymaerz, demhofer$pct)) + 
  geom_point(alpha = 0.2) + 
  theme +
  scale_y_continuous(labels = percent) +
  annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="white",lwd=4) +
  labs(x = "Anteil der Ausländer", y = "Anteil der Stimmen") + 
  ggtitle("Stimmen vs. Anteil der Ausländer")


plot(asylchart)

fit <- lm(demhofer$aslymaerz, demhofer$pct, data=demhofer)
summary(fit)

top_kand <- tbl_df(dem) %>% group_by(kandidat) %>% top_n(10, urban==1)

topcity <- top_kand %>%
  group_by(kandidat) %>%
  summarise(Sumstimmen = sum(ergebnis), SumGueltig = sum(gueltig)) 

topcity$prozent <- c((topcity$Sumstimmen/topcity$SumGueltig))

width=100 
height=50 
dpi = 96 

topcity <- ggplot(topcity, aes(x=reorder(kandidat, prozent), y=prozent, fill=kandidat)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "Anteil der Stimmen") +
  scale_fill_manual(values = kandidatenfarben) +
  guides(fill=FALSE) +
  geom_text(aes(label = paste(round(prozent*100,1),"%",sep=" ")), hjust= 1.1, color="white") +
  ggtitle("Wie Städte gewählt hätten") 

plot(topcity)
quartz.save("topcity.png", type = "png", height=height/dpi, width=width/dpi, dpi=dpi, antialias=TRUE)

# Label richtigstellen oder daten umdrehen: scale_x_continous(labels = c("Hofer", "Van der Bellen", "Griss", "Hundstorfer", "Lugner", "Khol")) +

# ============================================================================ #
# WENN NUR LÄNDLICHE REGIONEN GEWÄHLT HÄTTEN
# ============================================================================ #


top_land <- tbl_df(dem) %>% group_by(kandidat) %>% top_n(10, urban==3)

topland <- top_land %>%
  group_by(kandidat) %>%
  summarise(Sumstimmen = sum(ergebnis), SumGueltig = sum(gueltig)) 

topland$prozent <- c((topland$Sumstimmen/topland$SumGueltig))

topland <- ggplot(topland, aes(x=reorder(kandidat, prozent), y=prozent, fill=kandidat)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "Anteil der Stimmen") +
  scale_fill_manual(values = kandidatenfarben) +
  guides(fill=FALSE) +
  geom_text(aes(label = paste(round(prozent*100,1),"%",sep=" ")), hjust= 1.1, color="white") +
  ggtitle("Wie die Dörfer gewählt hätten") 

plot(topland)
quartz.save("topland.png", type = "png", height=height/dpi, width=width/dpi, dpi=dpi, antialias=TRUE)

# ============================================================================ #
# WENN NUR STÄDTISCHE REGIONEN GEWÄHLT HÄTTEN
# ==========================# ==========================# ==========================

top_stadt <-
  tbl_df(dem) %>% group_by(kandidat) %>% top_n(10, urban == 2)

topstadt <- top_stadt %>%
  group_by(kandidat) %>%
  summarise(Sumstimmen = sum(ergebnis),
            SumGueltig = sum(gueltig))

topstadt$prozent <- c((topstadt$Sumstimmen / topstadt$SumGueltig))

topstadt <-
  ggplot(topstadt, aes(
    x = reorder(kandidat, prozent),
    y = prozent,
    fill = kandidat
  )) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "Anteil der Stimmen") +
  scale_fill_manual(values = kandidatenfarben) +
  guides(fill = FALSE) +
  geom_text(aes(label = paste(round(prozent * 100, 1), "%", sep = " ")), hjust = 1.1, color =
              "white") +
  ggtitle("Wie städtische Regionen gewählt hätten")

plot(topstadt)
quartz.save("topstadt.png", type = "png", height=height/dpi, width=width/dpi, dpi=dpi, antialias=TRUE)


# ============================================================================ #
# WENN NUR top 10 studentengemeinden GEWÄHLT HÄTTEN
# ==========================# ==========================# ==========================

top_stud <- tbl_df(dem) %>% group_by(kandidat) %>% top_n()

topstadt <- top_stadt %>%
  group_by(kandidat) %>%
  summarise(Sumstimmen = sum(ergebnis), SumGueltig = sum(gueltig)) 

topstadt$prozent <- c((topstadt$Sumstimmen/topstadt$SumGueltig))

topstadt <- ggplot(topstadt, aes(x=reorder(kandidat, prozent), y=prozent, fill=kandidat)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "Anteil der Stimmen") +
  scale_fill_manual(values = kandidatenfarben) +
  guides(fill=FALSE) +
  geom_text(aes(label = paste(round(prozent*100,1),"%",sep=" ")), hjust= 1.1, color="white") +
  ggtitle("Wie städtische Regionen gewählt hätten") 

plot(topstadt)
quartz.save("topstadt.png", type = "png", height=height/dpi, width=width/dpi, dpi=dpi, antialias=TRUE)
