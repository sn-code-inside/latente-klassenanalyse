## Die folgenden Pakete werden benötigt
library(readxl)
library(dplyr)
library(forcats)
library(poLCA)




# 1. Schritt: Daten einlesen; alternativ können Sie den bei GitHub hinterlegten Sub-Datensatz w7g_sub verwenden, dann geht es direkt zu Schritt 3 
# Die Daten können unter worldvaluessurvey.org heruntergeladen werden - wählen Sie "Germany" und dort das Excel-Txt-File Version 5 aus. 
# Zum Einlesen der Daten wird das Paket "readxl" benötigt. 
# Der Datensatz sollte im aktuellen working directory von R gespeichert werden
# Der Datensatz wird als Objekt mit dem Namen w7g  gespeichert. 


w7g <- read_excel("./F00013243-WVS_Wave_7_Germany_ExcelTxt_v5.0.xlsx")


# 2. Schritt: Für die folgenden Analysen wird nur ein Subset der Daten benötigt; für Kapitel 2 Variablen Q91-Q93, Kapitel 3 Q122-129, Kapitel 4 zusätzlich Q262 und Q275R. Die Variablen werden anhand ihrer Position im Datensatz selektiert. 

w7g_sub <- w7g %>% dplyr::select(124:126,155:162,297,313)

# 3. Schritt

# Wenn der Datensatz w7g_sub aus GitHub verwendet wird, muss dieser im gleichen Ordner wie das Syntaxfile gespeichert und mit dem Befehl 
#load("./w7g_sub.RData") geladen werden

# Für die folgenden Analysen sollen die Variablen Q91, Q92 und Q93 verwendet werden. 
# Sie werden in einem eigenen Sub-Datensatz gespeichert. 

df_ir <- w7g_sub %>% dplyr::select(1:3)

summary(df_ir)



# Es fallen zwei Dinge auf: die Variablen sind als 'character' gespeichert, und sie haben sehr lange Namen. 
# Um die Verteilung der Variablen betrachten zu können und später eine LCA durchzuführen, muss der Objekttyp zu 'factor' geändert werden; zusätzlich werden die Namen verkürzt, um die Variablen besser bearbeitbar zu machen.  

names <- c(security_council= "Q91: Countries with the permanent seats on the UN Security Council", imf = "Q92: Where are the headquarters of the International Monetary Fund (IMF) located?", amnesty = "Q93: Which of the following problems does the organization Amnesty International deal with?")
df_ir <- df_ir %>% rename(all_of(names))

df_ir <- df_ir %>% mutate(across(everything(), as.factor))
summary(df_ir)

# Die Kategorie "no answer" muss als fehlender Wert spezifiziert werden, dazu wird eine Funktion aus dem Paket `forcats` verwendet. 

df_ir <- df_ir %>%
  mutate(across(everything(), ~ fct_recode(.x, `NA` = "No answer")))

summary(df_ir)


# Um sich die prozentuale Häufigkeitsverteilung der Variablen anzusehen, können wir für jede Variable diesen Befehl ausführen: 

round(prop.table(table(df_ir$security_council)),2)
round(prop.table(table(df_ir$imf)),2)
round(prop.table(table(df_ir$amnesty)),2)




# Erstellen von drei Zählvariablen, um die Anzahl der richtigen Antworten zu erfassen 
count1 <- as.numeric(df_ir$security_council == "India")
count2 <- as.numeric(df_ir$imf == "Washington DC")
count3 <- as.numeric(df_ir$amnesty == "Human rights")
# Zusammenfügen der Zählvariablen zu einem data frame
df <- data.frame(count1, count2, count3)
# Zählen der richtigen Antworten mit rowSums
c <- rowSums(df)
# Erstellen einer Faktorvariable für die zwei Gruppen
group <- factor(levels = c("hoch", "niedrig"))
# Zuweisen der Werte zu den Gruppen
group[c>=2]<-"hoch"
group[c<=1]<-"niedrig"
group[is.na(c)]<-NA
summary(group)


# Durchführen einer latenten Klassenanalyse (genauere Beschreibung der Schritte siehe Kapitel 3)




# für poLCA muss ein Listenobjekt mit den Namen der Variablen in der Analyse erstellt werden
vars_ir <- cbind(security_council, imf, amnesty) ~ 1

# Es wird ein seed (Zufallszahl) gesetzt, um die Analyse replizierbar zu machen 
set.seed(120417)

# Durchführen der Analyse
lca.ir <- poLCA(vars_ir, df_ir, nclass=2, nrep=5)



# Die klassenspezifischen Antwortwahrscheinlichkeiten sind im LCA-Objekt unter 'probs' gespeichert
lca.ir$probs
# Die Klassengröße (class prevalence) befindet sich unter 'P'
lca.ir$P



# Zuordnungswahrscheinlichkeiten im Modell: posterior probabilities
#für die ersten 10 Fälle
lca.ir$posterior[1:10,]
# zur Anschauuung: die dazugehörigen Antworten der ersten 10 Fälle
df_ir[1:10,]



# Mit dem Paket DAKS und dem Befehl pattern kann man sich die vorkommenden Muster in den Daten anzeigen lassen
# wenn DAKS geladen wird, kann es Probleme mit anderen Paketen geben, daher besser direkt aufrufen 

DAKS::pattern(df_ir, n=64)



# Log-Likelihood abrufen: über modellname$llik bzw. modellname$attempts, hier: 
lca.ir$llik
lca.ir$attempts
# Die Anzahl der Durchgänge kann über den Unter-Befehl 'nrep=' verändert werden
#poLCA(formula, data, nclass=2, **nrep=5**)



#Beispiel: Modell mit 3,4,5... Klassen
lca.ir3 <- poLCA(vars_ir, df_ir, nclass=3, nrep=5)
lca.ir4 <- poLCA(vars_ir, df_ir, nclass=4, nrep=5)
lca.ir5 <- poLCA(vars_ir, df_ir, nclass=5, nrep=5)




# Anzahl der Modellparameter
lca.ir$npar
# Anzahl der Freiheitsgrade: "residual degrees of freedom", einer wird abgezogen
lca.ir$resid.df



#AIC und BIC können über modelname$aic und modelname$bic abgerufen werden
#Wir vergleichen die Werte für die Modelle mit 2,3,4 und 5 Klassen - in Kapitel 3 wird dies noch ausführlicher behandelt. 
lca.ir$aic
lca.ir3$aic
lca.ir4$aic
lca.ir5$aic

lca.ir$bic
lca.ir3$bic
lca.ir4$bic
lca.ir5$bic
# Hier stimmen AIC und BIC überein: der niedrigste Wert ist bei 2 Klassen, diese Lösung sollte gewählt werden. 

