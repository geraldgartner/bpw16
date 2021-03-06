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

#Unser Style


theme <- theme(plot.background = element_rect(fill = "gray97"), panel.grid.major = element_line(colour = "gray86", linetype = "dotted"), 
               panel.grid.minor = element_line(colour = "gray86", linetype = "dotted")) + 
  theme(plot.title = element_text(size = 18, face = "bold"), 
        plot.background = element_rect(fill = "gray97", colour = "antiquewhite", size = 10, linetype = "solid")) +
  theme(axis.ticks = element_blank(), 
        axis.line = element_blank(),
        axis.title = element_text(vjust = 8), 
        panel.background = element_rect(fill = "grey97", linetype = "solid"), 
        plot.background = element_rect(colour = "gray97"), 
        plot.title = element_text(hjust=0, margin=unit(c(0,1,0.2,1), "cm")), 
        plot.margin = unit(c(1,0.5,0.5,0.5), "cm")) +
    theme(axis.text=element_text(size=14))  
  

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

topland <- ggplot(topland, aes(x=reorder(kandidat, prozent), y=prozent, fill=kandidat)) +
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

plot(topland)
ggsave("topland.pdf", useDingbats=FALSE)


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
  ggtitle("Wie städtische Regionen gewählt hätten") +
  scale_x_discrete(labels=c("hofer" = "Hofer", "khol" = "Khol","hundstorfer" = "Hundstorfer", "griss" = "Griss", "vdb" = "Van der Bellen", "lugner", "lugner" = "Lugner"))

plot(topstadt)
quartz.save("topstadt.png", type = "png", height=height/dpi, width=width/dpi, dpi=dpi, antialias=TRUE)


# ============================================================================ #
# WENN NUR top 10 studentengemeinden GEWÄHLT HÄTTEN
# ==========================# ==========================# ==========================

#Mergen der Sheets für weitere Analyse
erwerb <- merge(wahlgang1, erwerbsstatus, by.x = "gkz", by.y = "gkz")


top_stud <- tbl_df(erwerb) %>% group_by(kandidat) %>% top_n(n = 21, wt = hochschule_u_akademie/bildung_total) 


topstud <- top_stud %>%
  group_by(kandidat) %>%
  summarise(Sumstimmen = sum(ergebnis), SumGueltig = sum(gueltig)) 

topstud$prozent <- c((topstud$Sumstimmen/topstud$SumGueltig))

topstud <- ggplot(topstud, aes(x=reorder(kandidat, prozent), y=prozent, fill=kandidat)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "Anteil der Stimmen") +
  scale_fill_manual(values = kandidatenfarben) +
  guides(fill=FALSE) +
  geom_text(aes(label = paste(round(prozent*100,1),"%",sep=" ")), hjust= 1.1, color="white") +
  ggtitle("Wie Gemeinden mit höchster Akademikerquote gewählt hätten") 

plot(topstud)
quartz.save("output/topstadt.png", type = "png", height=height/dpi, width=width/dpi, dpi=dpi, antialias=TRUE)

# ============================================================================ #
# WENN NUR top 10 arbeitergemeinden GEWÄHLT HÄTTEN
# ========================================================================

top_arb <- tbl_df(erwerb) %>% group_by(kandidat) %>% top_n(n = 21, wt = pflichtschule/bildung_total) 


toparb <- top_arb %>%
  group_by(kandidat) %>%
  summarise(Sumstimmen = sum(ergebnis), SumGueltig = sum(gueltig)) 

toparb$prozent <- c((toparb$Sumstimmen/toparb$SumGueltig))

toparb <- ggplot(toparb, aes(x=reorder(kandidat, prozent), y=prozent, fill=kandidat)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "Anteil der Stimmen") +
  theme +
  scale_fill_manual(values = kandidatenfarben) +
  guides(fill=FALSE) +
  geom_text(aes(label = paste(round(prozent*100,1),"%",sep=" ")), hjust= 1.1, color="white") +
  ggtitle("Wahlverhalten der Gemeinden mit höchster") +
  scale_x_discrete(labels=c("hofer" = "Hofer", "khol" = "Khol",
                            "hundstorfer" = "Hundstorfer", "griss" = "Griss", "vdb" = "Van der Bellen", "lugner", "lugner" = "Lugner"))

plot(toparb)
quartz.save("output/topstadt.png", type = "png", height=height/dpi, width=width/dpi, dpi=dpi, antialias=TRUE)
