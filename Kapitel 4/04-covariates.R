# Benötigt werden die Pakete
library(poLCA)
library(dplyr)
library(rlist)

# In Kapitel 4 wird die Drei-Klassen-Lösung aus Kapitel 3 verwendet. Sofern das Objekt nicht mehr im R-Environment vorhanden ist, kann es alternativ auch direkt aus GitHub geladen werden: 

lca.3<- rlist::list.load('lca_3.rdata')
load('./df_ex.rdata')

# Kapitel 4.1
# Anzeige einiger posterior probabilites

round(lca.3$posterior[6:10,],3)

# Betrachtung der Kovariaten Bildung und Alter, Definition fehlender Werte, Datenbearbeitung
summary(data.frame(df_ex$Q275))

df_ex$educ3 <- df_ex$Q275
df_ex$educ3[df_ex$educ3=="Other missing; Multiple answers Mail (EVS)"]<-NA
df_ex$educ3 <- droplevels(df_ex$educ3)
summary(df_ex$educ3)

class(df_ex$Q262)
levels(df_ex$Q262)


df_ex$age <- as.numeric(paste(df_ex$Q262))
class(df_ex$age)
summary(df_ex$age)


# Anfügen der wahrscheinlichsten Klassen an den Datensatz
df_ex$predclass <- lca.3$predclass


# Kreuztabelle wahrscheinlichste Klasse und Bildung

Zeilenprozente <- df_ex %>%
  dplyr::select(educ3, predclass) %>%   # Variablenauswahl
  table() %>%                           # Kontingenztabelle
  prop.table(1) %>%                     # Zeilenprozente
  round(., 2)                           # Runden

print(Zeilenprozente)


Spaltenprozente <- df_ex %>%
  dplyr::select(educ3, predclass) %>%   # Variablenauswahl
  table() %>%                           # Kontingenztabelle
  prop.table(2) %>%                     # Spaltenprozente
  round(., 2)                           # Runden

print(Spaltenprozente)


chisq.test(table(df_ex$educ3, df_ex$predclass))


# Durchschnittsalter der Klassen
tapply(df_ex$age, df_ex$predclass, mean)

# Kapitel 4.2
# Durchführung LCA mit Kovariaten 
lca.var.cov<- cbind(Q122, Q123,Q124,Q125, Q126, Q127, Q128, Q129) ~ educ3+age

set.seed(0608)
lca.3.cov <- poLCA(lca.var.cov, data = df_ex, nclass = 3, na.rm = FALSE,
                    nrep=10, verbose=FALSE)

# Alternativ: laden aus dem GitHub-Repositorium (lokal gespeicherte Liste)
#lca.3.cov <- rlist::list.load('lca.3.cov.rdata')


# Überprüfen der Konvergenz
lca.3.cov$llik
lca.3.cov$attempts

# Ordnen der Klassenreihenfolge
probs.start.cov <- poLCA.reorder(lca.3.cov$probs.start,
                                 order(lca.3.cov$P,decreasing=TRUE))
lca.3.cov <- poLCA(lca.var.cov, df_ex, nclass=3, na.rm=FALSE, 
                   probs.start = probs.start.cov)


# Visualisierung marginaler Effekte
# niedrige Bildung
educ_low <- cbind(1,1,0,c(18:96)) 
#1 Matrix-Objekt
exb.edulow <- exp(educ_low %*% lca.3.cov$coeff) 
#2 Exponenzierung und Matrix-Multiplikation
matplot(c(18:96), (cbind(1,exb.edulow)/(1+rowSums(exb.edulow))), 
        main = "Abbildung 5: Marginaler Effekt des Alters bei niedriger Bildung", 
        xlab = "Alter", 
        ylab = "Wahrscheinlichkeit, zu einer latenten Klasse zu gehören",  
        ylim = c(0,1), type="l", lwd=1, col=1)
text(80,0.4, "unentschlossen")  # Platzierung bei x=80, y=0.4
text(50, 0.6, "migrationskritisch")
text(30, 0.1, "migrationsaffin")


# mittlere Bildung
educ_med <- cbind(1,0,1,c(18:96)) 
exb.edumed <- exp(educ_med %*% lca.3.cov$coeff) 
matplot(c(18:96), (cbind(1,exb.edumed)/(1+rowSums(exb.edumed))), 
        main = "Abbildung 6: Marginaler Effekt des Alters bei mittlerer Bildung", 
        xlab = "Alter", 
        ylab = "Wahrscheinlichkeit, zu einer latenten Klasse zu gehören",  
        ylim = c(0,1), type="l", lwd=1, col=1)
text(80,0.1, "unentschlossen")
text(80, 0.75, "migrationskritisch")
text(50, 0.4, "migrationsaffin")


# hohe Bildung
exb.eduhi <- exp(educ_hi %*% lca.3.cov$coeff) 
matplot(c(18:96), (cbind(1,exb.eduhi)/(1+rowSums(exb.eduhi))), 
        main = "Abbildung 7: Marginaler Effekt des Alters bei hoher Bildung", 
        xlab = "Alter", 
        ylab = "Wahrscheinlichkeit, zu einer latenten Klasse zu gehören",  
        ylim = c(0,1), type="l", lwd=1, col=1)
text(80,0.1, "unentschlossen")
text(30, 0.75, "migrationsaffin")
text(85, 0.6, "migrationskritisch")

