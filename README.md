# Analyse der Bundespräsidentenwahl
R-Skripte für die Analyse der Bundespräsidentenwahl - 1. Wahlgang und Stichwahl

#Risiken und Nebenwirkungen der Daten

### Wie werden die Arbeiter- und Angestelltenquoten berechnet?
Dafür wird die abgestimmte Erwerbsstatistik der Statistik Austria verwendet. Sie wurde zuletzt Ende 2013 erhoben. Die Ergebnisse liegen immer zwei Jahre nach dem Stichtag vor. Herausgefiltert sind Personen, die nicht die österreichische Staatsbürgerschaft haben. Dieser Schritt wird vorgenommen, weil diese Personen vom Wahlrecht ausgeschlossen sind. 

### Welche Erwerbstätigen sind in den Daten zusammengefasst?
Nicht in der Zahl der Angestellten- oder Arbeiter enthalten sind freie Dienstnehmer, Beamte, Grundwehrdiener, Zivildiener, unselbständig Erwerbstätige mit unbekannter Stellung im Beruf, Grenzgänger ins Ausland ohne österreichische Sozialversicherung, 
sowie Beschäftigte nach Dienstleistungsscheckgesetz. Diese werden von der Statistik Austria in der Kategorie Sonstige zusammengefasst. In der Quote für Arbeiter und Angestellte sind auch Lehrlinge enthalten. Bei den Selbstständigen sind auch Personen enthalten, die im eigenen Betrieb mitarbeiten.

Personen, die jünger als 15 Jahre alt sind, sind von der Berechnung der Quoten ausgeschlossen. Wir wollen uns dem Erwerbsstatus der wahlberechtigten Bevölkerung annähern. 

Temporär Abwesenden und Arbeitslosen wird die Stellung im Beruf der zuletzt ausgeübten Erwerbstätigkeit zugeordnet. Im Jahr 2013 werden Vertragsbedienstete nicht gesondert ausgewiesen sondern je nach Tätigkeit den Gruppen „Arbeiterinnen, Arbeiter“, „Angestellte“ und „Sonstige unselbständig Erwerbstätige“ zugeordnet. 

Eine detaillierte Beschreibung der Abgestimmten Erwerbsstatistik gibt es hier: http://www.statistik.at/web_de/statistiken/bevoelkerung/volkszaehlungen_registerzaehlungen_abgestimmte_erwerbsstatistik/index.html 

Die Daten berücksichtigen die steirischen Gemeindezusammenenlegungen in vollem Umfang. Daten von fusionierten Gemeinden wurden zusammengeführt. Daten von den fünf geteilten Gemeinden wurden entsprechend zugewiesen. 

### Welche Ungenauigkeiten hat die Abgestimmte Erwerbsstatistik beim Erwerbsstatus?
Für 5 Prozent der Personen musste die Statistik Austria die höchste abgeschlossene Ausbildung schätzen. Ein Großteil davon sind Personen, die nach 2001 zugewandert sind. Diese Personen fließen nicht in die Analyse mit ein, weil sie zum allergrößten Teil nicht die österreichische Staatsbürgerschaft haben - eine der Grundvorraussetzungen für die Verleihung der Staatsbürgerschaft ist, dass über zehn Jahre hindurch ein rechtmäßiger und ununterbrochener Aufenthalt in Österreich gegeben ist.  

#Welche Einkommensdaten werden für die Analyse herangezogen?
Der Median des Gesamteinkommens inklusive Transferleistungen nach Gemeinden. Die jüngsten verfügbaren Zahlen stammen aus dem Jahr 2012. Abgebildet wird der Median des Einkommenes von Selbstständigen, Unselbstständigen und Nicht-Erwerbstätigen. Weil nur Einkommensdaten mit Gebietsstand 1.1.2013 gibt, werden alle steirischen Gemeinden, die von der Gemeindestrukturreform betroffen waren nicht berücksichtigt. Das verschränken zweier oder mehrerer Medianeinkommen wäre unzulässig. 
