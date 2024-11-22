# Diese Pakete werden benötigt: 
library(readxl)  # Excel-Datensatz einlesen
library(dplyr)   # Datenbearbeitung
library(sjlabelled) # Bearbeitung der Labels
library(poLCA)     # LCA
library(ggplot2)   # Grafik
library(wesanderson) # Farbpalette
library(reshape2)  #Datenbearbeitung
library(tidyLPA)  # LRT-Test (VLMR)
library(glca)   # LRT-Test (BLRT)

# Kapitel 3.1
# Variante 1: Aufruf des direkt beim WVS heruntergeladenenen Datensatzes (sollte lokal im gleichen Ordner wie die Syntax gespeichert sein), subsetting

w7g <- read_excel("./F00013243-WVS_Wave_7_Germany_ExcelTxt_v5.0.xlsx")
w7g_sub <- w7g %>% dplyr::select(124:126,155:162,297,313)


# Variante 2: Laden des Daten-Subsets aus GitHub (sollte lokal im gleichen Ordner wie die Syntax gespeichert sein)
load("./w7g_sub.RData")


# Erstellen eines Subdatensatzes für die Analysen
df_ex <- w7g_sub %>% dplyr::select(4:13)
names(df_ex) <- sub(": Immigration in your country:", "", names(df_ex)) # Namen abkürzen


# Häufigkeitstabelle (Tabelle 3.1)

t1 <- apply(df_ex[,1:8],2, table) #absolute Häufigkeiten
t1_r <- round(proportions(t1,2), 2) # relative Häufigkeiten
t1_rt <- t(t1_r) # transponieren

# Alternative mit dem pipe-operator
t1_rt <- df_ex[, 1:8] %>%
  apply(2, table) %>%                # Absolute Häufigkeiten berechnen
  proportions(2) %>%                 # Relative Häufigkeiten berechnen
  round(2) %>%                       # Runden
  t()                                # Transponieren


print(t1_rt)

# Kapitel 3.3

#Prüfung auf Variablentyp
class(df_ex$`Q122 Fills useful jobs in the workforce`)


# Umcodieren zu Faktorvariablen
df_ex <- df_ex %>%
  mutate_if(sapply(df_ex, is.character), as.factor)


# Erneute Prüfung
class(df_ex$`Q122 Fills useful jobs in the workforce`)
summary(df_ex$`Q122 Fills useful jobs in the workforce`)


# Fehlende Werte
df_ex <- df_ex %>%
  dplyr::mutate(across(everything(), ~if_else(. %in% c("Don't know", "No answer"), NA, .)))


summary(df_ex$`Q122 Fills useful jobs in the workforce`)

df_ex <- droplevels(df_ex)


# Fälle mit zu vielen fehlenden Werten ermitteln und ausschließen
na_check <- rowSums(is.na(df_ex))  
table(na_check)

df_ex$na_check <- na_check  # Anfügen an Datensatz
df_ex <- df_ex %>%    # Filtern
  filter(na_check <4)
nrow(df_ex) # Zählen der verbliebenen Fälle im Datensatz


# Namen verändern
collabs <- colnames(df_ex) 
#Variablennamen werden als Label gespeichert
colnames(df_ex) <- substr(colnames(df_ex),1,4) 
# Abkürzung der Namen
df_ex <- set_label(df_ex, label = collabs) 
# Setzen der Labels 


# Variablenobjekt erstellen für LCA
lca.var<- cbind(Q122, Q123,Q124,Q125, Q126, Q127, Q128, Q129) ~1

# Kapitel 3.5

# Liste für Ergebnisse der LCA mit Klassen 1 bis 10 erstellen, LCA durchführen 
set.seed(100221)
lc <- list() # leeres Listen-Objekt erstellen
 
for(i in 1:10){
  lc[[i]] <- poLCA(lca.var, df_ex, nclass=i, maxiter=5000,
               tol=1e-8, na.rm=FALSE,
              nrep=5, verbose=FALSE, calc.se=TRUE)
 }


# FUnktion für Entropie
machine_tolerance <- sqrt(.Machine$double.eps)
  entropy <- function(p) {
    p <- p[p > machine_tolerance]
    sum(-p * log(p))
  }


# Tabelle mit Ergebnissen erstellen
table_LCA <- data.frame(Modell=0, LL=0, npar=0, BIC=0, AIC=0,  Gsq= 0, 
                        df=0, p_gsq=0, e.R2=0)  

for(i in 1:length(lc)){
  table_LCA [i,2] <- round(lc[[i]]$llik,0)
  table_LCA [i,3] <- lc[[i]]$npar
  table_LCA [i,4] <- round(lc[[i]]$bic,0)
  table_LCA [i,5] <- round(lc[[i]]$aic,0)
  table_LCA [i,6] <- round(lc[[i]]$Gsq,0)
  table_LCA [i,7] <- lc[[i]]$resid.df
  table_LCA [i,8] <- round(pchisq(lc[[i]]$Gsq, df=lc[[i]]$resid.df, 
                                  lower.tail = FALSE),3)
  error_prior <- entropy(lc[[i]]$P) 
  error_post <- mean(apply(lc[[i]]$posterior, 1, entropy))
  table_LCA[i,9] <- round(((error_prior - error_post) / error_prior),2)
  
  if (i==1) {
    table_LCA [i,1] <- paste("Klasse")
  }
  if(i>1) {
     table_LCA [i,1] <- paste("Klassen")
  }
}


print(table_LCA)


# Scree-Plot Informationskriterien
df_ic <- table_LCA[,4:5]  # AIC und BIC-Werte in eigenes Objekt 
df_ic$n <- 1:nrow(df_ic)  # eigene Spalte mit laufenden Nummern 

ggplot(data = df_ic) +
geom_line(mapping = aes(y = BIC, x = n, linetype = "BIC"), show.legend = TRUE) +
geom_line(mapping = aes(y = AIC, x = n, linetype = "AIC"), show.legend = TRUE) +
scale_linetype_manual(values = c('BIC' = 'solid', 'AIC' = 'dashed'), name = "ICs") +
ggtitle("Abbildung 3: Scree-Plot Informationskriterien") +
xlab("Anzahl Klassen") +
ylab("Informationskriterien") +
scale_x_continuous(breaks = seq(1, 10, by=1))


# VLMR-Test
mod_null <- lc[[3]]
mod_alt <- lc[[4]]

mod_null$Nobs
mod_alt$Nobs

n <- mod_null$Nobs
null_ll <- mod_null$llik
null_param <- mod_null$npar
null_classes <- length(mod_null$P)

alt_ll <- mod_alt$llik
alt_param <- mod_alt$npar
alt_classes <- length(mod_alt$P)


tidyLPA::calc_lrt(n, null_ll, null_param, null_classes, alt_ll, alt_param, alt_classes)


# BLRT-Test

glca.var <- item(Q122, Q123, Q124, Q125, Q126, Q127, Q128, Q129) ~ 1
glca.3 = glca(glca.var, data=df_ex,nclass=3, verbose = FALSE)
glca.4 = glca(glca.var, data= df_ex,nclass=4, verbose=FALSE)
gofglca(glca.3, glca.4, test="boot")


# Einzelne Analysen für zwei und drei Klassen
lca.2 <- poLCA(lca.var, df_ex, nclass=2, na.rm=FALSE, maxiter=5000,
              tol=1e-8, nrep=5, verbose=TRUE)
lca.3 <- poLCA(lca.var, df_ex, nclass=3, na.rm=FALSE, maxiter=5000,
               tol=1e-8, nrep=5, verbose=TRUE)

# Berechnung Entropie:
fit <- lca.3
machine_tolerance <- sqrt(.Machine$double.eps)
entropy.R2 <- function(fit) {
  entropy <- function(p) {
  p <- p[p > machine_tolerance]
     sum(-p * log(p))
  }
}
error_prior <- entropy(fit$P)
error_post <- mean(apply(fit$posterior, 1, entropy))
R2_entropy <- (error_prior - error_post) / error_prior
R2_entropy

p_gsq <- round(pchisq(fit$Gsq, df=fit$resid.df, lower.tail = FALSE),3)
p_gsq

# Kapitel 3.6

# LCA nach Startwerten ordnen
probs.start.new <- poLCA.reorder(lc[[3]]$probs.start,order(lc[[3]]$P,decreasing=TRUE))
lca.3 <- poLCA(lca.var, df_ex, nclass=3, na.rm=FALSE, probs.start = probs.start.new)

# Schönere Darstellung 
obj <- names(df_ex[,-c(9:11)])
col_names <- c("agree", "disagree", "hard to say")

matrices <- list()
for (mat_name in obj) {
  mat <- lca.3$probs[[mat_name]]
  colnames(mat) <- col_names
  matrices[[mat_name]]<- mat
}

var_labs <- get_label(df_ex[,-c(9:11)])
new_names <- unname(var_labs)
names(matrices) <- new_names
matrices <- lapply(matrices,round,2)
print(matrices)

# Plot
plot(lca.3)


# Elegantere Darstellung

lcmodel <- reshape2::melt(lca.3$probs)
levels(lcmodel$Var2) <- c("agree", "disagree","hard to say")
levels(lcmodel$Var1) <- c("kritisch", "positiv","unentschieden")
ggplot(lcmodel,aes(x = L1, y = value, fill = Var2)) + 
geom_bar(stat = "identity", position = "stack") +
facet_grid(Var1 ~ .) +
scale_fill_manual(values=wes_palette(name="Royal1")) +
labs(x="Items zu Migration", y="klassenspezifische Antwortwahrscheinlichkeiten", 
     title="Abbildung 4: Balkendiagramm, 3-Klassen-Lösung", fill="Kategorien")


# Klassengrößen (Anteile am Datensatz)
lca.3$P


# Klassengrößen (wahrscheinlichste Klasse)
modp <- table(lca.3$predclass)/lca.3$N
round(modp, 3)


# LL-Werte
lc[[3]]$attempts

lc[[10]]$attempts

