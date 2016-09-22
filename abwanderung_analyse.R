library("dplyr")
library("googlesheets")
library("httr")
suppressPackageStartupMessages(library("dplyr"))

gemeindedaten <- gs_key('1Lt137MmgPGeJugRguErIDNqjRbZojNsWWmUPTTOUPgs')

abwanderung <- gs_read(gemeindedaten, ws = 'abwanderung', col_names = TRUE)


abwanderung$HOFPCT = as.numeric(gsub(",",".",abwanderung$HOF))*100
abwanderung$VDBPCT = as.numeric(gsub(",",".",abwanderung$VDB))*100
abwanderung$ABWPCT = as.numeric(gsub(",",".",abwanderung$"16vs06"))


erwerbsstatus <- gs_read(gemeindedaten, ws = "erwerb2013", col_names = TRUE)



erwerbsstatus$pflichtschule_pct <- erwerbsstatus$pflichtschule/erwerbsstatus$bildung_total
erwerbsstatus$lehrabschluss_pct <- erwerbsstatus$lehrabschluss/erwerbsstatus$bildung_total
erwerbsstatus$mittlere_und_hoehere_schule_pct <- erwerbsstatus$mittlere_und_hoehere_schule/erwerbsstatus$bildung_total
erwerbsstatus$hochschule_u_akademie_pct <- erwerbsstatus$hochschule_u_akademie/erwerbsstatus$bildung_total

erwerbsstatus$arbeiter_pct <- erwerbsstatus$arbeiter/erwerbsstatus$erwerbstaetig
erwerbsstatus$angestellte_pct <- erwerbsstatus$pflichtschule/erwerbsstatus$erwerbstaetig
erwerbsstatus$selbststaendige_pct <- erwerbsstatus$pflichtschule/erwerbsstatus$erwerbstaetig



abwerwerb = merge(x=abwanderung,y=erwerbsstatus,by.x="gkz",by.y="gkz", incomparables=NA)

summary(lm(abwanderung$HOFPCT ~ abwanderung$ABWPCT))
summary(lm(abwanderung$HOFPCT ~ abwanderung$ABWPCT + factor(abwanderung$urban)))

abwerwerb = merge(x=abwanderung,y=erwerbsstatus,by.x="gkz",by.y="gkz", incomparables=NA)

print(summary(lm(abwerwerb$HOFPCT ~ abwerwerb$ABWPCT + factor(abwerwerb$urban)+abwerwerb$selbststaendige_pct)))

print(summary(lm(abwerwerb$HOFPCT ~ abwerwerb$ABWPCT + factor(abwerwerb$urban) + abwerwerb$selbststaendige_pct)))
print(summary(lm(abwerwerb$HOFPCT ~ abwerwerb$ABWPCT + factor(abwerwerb$urban) + abwerwerb$pflichtschule_pct)))

print(summary(lm(abwerwerb$HOFPCT ~ abwerwerb$ABWPCT + factor(abwerwerb$urban) + abwerwerb$hochschule_u_akademie_pct)))

#Neue Berechnungen für Andi
print(summary(lm(abwerwerb$HOFPCT ~ abwerwerb$pflichtschule_pct + factor(abwerwerb$urban) + abwerwerb$arbeiter_pct)))
print(summary(lm(abwerwerb$VDBPCT ~ abwerwerb$hochschule_u_akademie_pct + factor(abwerwerb$urban) + abwerwerb$angestellte_pct)))


