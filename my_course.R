## ---- install packages, eval = F--------------------------------------------------------------
## install.packages(c(
##   "compositions", # librairie dédiée à l'analyse de données de compositions
##   "missForest", # traiter les données manquantes non compo
##   "energy",   # tester une loi normale multivariée
##   "readxl",   # importer des fichiers excel
##   "sf", # données spatiales
##   "tidyverse", # univers tidyverse
##   "xtable",     # exporter sous forme de table latex
##   "easyCODA",    # analyse univariee et multivariee pour CoDa
##   "zcompositions", # traitement des valeurs manquantes
##   "RColorBrewer" # palette de couleurs
##  ))
## devtools::install_github("tibo31/codareg")


## ---- charger une librairie-------------------------------------------------------------------
library("readxl")


## ---- importer data localement, eval = F------------------------------------------------------
## my_url <- "https://www.data.gouv.fr/fr/datasets/r/48a38a25-9e46-4d83-80db-947258df9409"
## download.file(my_url, destfile = paste0(getwd(), "/res_2022.xlsx"))


## ---- lire data-------------------------------------------------------------------------------
res_2022 <- read_excel("res_2022.xlsx")
class(res_2022)


## ---- afficher structure data-----------------------------------------------------------------
str(res_2022)


## ---- extraire colonnes data------------------------------------------------------------------
vote_share_1 <- res_2022[ , c("Macron_EXP", "Le Pen_EXP", "Mélenchon_EXP",
 "Zemmour_EXP", "Pécresse_EXP", "Jadot_EXP", "Lassalle_EXP", "Roussel_EXP",
 "Dupont-Aignan_EXP", "Hidalgo_EXP", "Poutou_EXP", "Arthaud_EXP")]


## ---- changer nom data------------------------------------------------------------------------
colnames(vote_share_1) <- c("Macron", "Le_Pen", "Mélenchon", "Zemmour", 
  "Pécresse", "Jadot", "Lassalle", "Roussel", "Dupont_Aignan", "Hidalgo",
  "Poutou", "Arthaud")


## ---- crer variable exprimes------------------------------------------------------------------
res_2022$non_exprimes <- res_2022$Abstentions + res_2022$Blancs +
  res_2022$Nuls


## ---- extraire data comptage------------------------------------------------------------------
vote_share_2 <- res_2022[ , c("Macron_VOIX", "Le Pen_VOIX", 
  "Mélenchon_VOIX", "Zemmour_VOIX", "Pécresse_VOIX", "Jadot_VOIX",
  "Lassalle_VOIX", "Roussel_VOIX", "Dupont-Aignan_VOIX", "Hidalgo_VOIX",
  "Poutou_VOIX", "Arthaud_VOIX", "non_exprimes")]


## ---- creer nouvelle variable-----------------------------------------------------------------
vote_share_2 <- sapply(vote_share_2, function(x) x / res_2022$Inscrits)


## ---- recoder nom data------------------------------------------------------------------------
colnames(vote_share_2) <- c("Macron", "Le_Pen", "Mélenchon", "Zemmour",
  "Pécresse", "Jadot",  "Lassalle", "Roussel", "Dupont_Aignan", "Hidalgo",
  "Poutou", "Arthaud", "non_inscrits")


## ---- eval = F--------------------------------------------------------------------------------
## data(package = "compositions")


## ---- transformer data format long, message = F-----------------------------------------------
library("tidyverse")
vote_share_long <- pivot_longer(data.frame(vote_share_1, 
                                           dep = res_2022$DEP_NOM), 
              cols = 1:12, names_to = "candidat", values_to = "share")


## ---- barplot des shares, fig.width = 12, fig.height = 6, fig.cap = "Diagramme en barre des votes (nombre de voix divisé par nombre d'exprimes) par département", fig.align="center"----
vote_share_long %>%
  mutate(dep = factor(dep, levels = 
              levels((vote_share_long %>%
                filter(candidat == "Zemmour") %>%
                mutate(dep = fct_reorder(dep, share)))$dep))) %>%
  ggplot() +
  geom_col(aes(x = dep, fill = candidat, y = share)) +
  theme(axis.text.x = element_text(angle = 90))


## ---- package spatial, message = F------------------------------------------------------------
library("sf")
source("spatial/spatial_circon.R")


## ---- merge data spatial----------------------------------------------------------------------
geo_dep <- merge(geo_dep[, c("nom_dpt", "code_dpt", "nom_reg")], 
     data.frame(vote_share_1, dep = res_2022$DEP_CODE, 
                Inscrits = res_2022$Inscrits), 
     by.x = "code_dpt", by.y = "dep")


## ---- transformer data long, fig.width = 15, fig.height = 6, fig.cap = "Diagrammes en barre des votes par département, groupés par région", fig.align="center"----
vote_share_long <- pivot_longer(geo_dep, cols = 4:15, names_to = "candidat", 
                                values_to = "share")
vote_share_long %>%
  mutate(nom_reg = gsub("-", "-\n", nom_reg)) %>%
  ggplot() +
  geom_col(aes(x = nom_dpt, fill = candidat, y = share)) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(. ~ nom_reg, scales = "free_x", space = "free") 


## ---- historique election presidentielles-----------------------------------------------------
time_chart <- data.frame(
  year = rep(as.Date(c("1958-01-01", "1965-01-01", "1969-01-01", "1974-01-01", 
      "1981-01-01", "1988-01-01", "1995-01-01", "2002-01-01", "2007-01-01", 
      "2012-01-01", "2017-01-01", "2022-01-01")), each = 4),
  vote = c(78.51, 21.49, 0, 0, 44.65, 31.72, 17.28, 6.35, 44.47, 32.22, 
           23.31, 0, 35.77, 47.96, 15.11, 1.16, 20.99, 50.70, 28.31, 0, 
           19.95, 49.11, 16.55, 14.39, 20.84, 40.84, 18.58, 19.74, 19.88, 
           42.87, 12.63, 24.62, 31.18, 36.10, 18.57, 14.15, 27.18, 44.00, 
           9.13, 19.69, 20.01, 27.85, 25.22, 26.92, 4.78, 31.92, 30.98, 32.32),
  parti = rep(c("droite", "gauche", "centre", "extrême"), 12)
)


## ---- evolution des parts, fig.width = 10, fig.height = 5, fig.cap = "Evolution des parts des principaux partis entre 1958 et 2022", fig.align="center"----
ggplot(time_chart) + 
  aes(x = year, y = vote, fill = parti) +
  geom_area(color = "black") +
  labs(title = "Votes au 1er tour de l'élection présidentielle",
       subtitle = "1958 à 2022",
       x = "Year",
       y = "percentage",
       fill = "Partis") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()


## ---- cartographie, fig.width = 7, fig.height = 5, fig.cap = "Cartographie des candidats qui ont remporté le plus de voix par département", fig.align="center"----
geo_dep$vainqueur <- names(vote_share_1)[
  apply(st_drop_geometry(geo_dep[, names(vote_share_1)]), 1, which.max)]
plot(geo_dep[, "vainqueur"], main = "", key.pos = 1, 
     key.width = lcm(1.3), key.length = .7)


## ---- option xtable, echo = F-----------------------------------------------------------------
options(xtable.comment = FALSE)


## ---- convertir dans une table latex, results='asis'------------------------------------------
xtable::xtable(t(sapply(vote_share_1, function(x) 
  c(min = min(x), q = quantile(x, 0.25), median = median(x), mean = mean(x),
    q = quantile(x, 0.75), max= max(x)))), 
  caption = "Statistique descriptive des parts de vote")


## ---- boxplot de chaque part, fig.width = 12, fig.height = 4, message = F, fig.cap = "Boîte à moustache des parts prises une à une", fig.align="center"----
par(mfrow = c(1, 12), las = 1, mar = c(0, 2, 0, 2))
res <- sapply(vote_share_1, function(x) {
  res <- boxplot(x, ylim = c(0, 70))
  length(res$out)
  })

## ---- results='asis'--------------------------------------------------------------------------
print(xtable::xtable(t(as.table(res)),  
  caption = "Nombre de valeurs extrêmes, détecté sur chaque boîte à moustache"),
  size = "tiny")


## ---- Dot Chart, fig.width = 18, fig.height = 4, message = F, fig.cap = "Dot chart des variables prises une à une", fig.align="center"----
library(easyCODA)
DOT(vote_share_1)


## ---- graphique de confiance, fig.width = 10, fig.height = 5, fig.cap = "Graphique de confiance", fig.align="center", message = F----
plotdata <- vote_share_long %>%
  st_drop_geometry() %>%
  select(candidat, share)  %>%
  group_by(candidat) %>%
  summarize(mean = mean(share),
            ci_1 = quantile(share, 0.05),
            ci_2 = quantile(share, 0.95))
ggplot(plotdata, aes(x = candidat,
                     y = mean, 
                     colour = candidat)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = ci_1, 
                    ymax = ci_2), 
                width = .1)


## ---- cartographie des 3 candidats, fig.width = 12, fig.height = 5, fig.cap = "Cartographie des parts obtenus par les trois premiers candidats", fig.align="center"----
library("mapsf")
par(mfrow = c(1, 3), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
candidat <- c("Macron", "Le_Pen", "Mélenchon")
ma_palette <- c("OrYel", "Dark Mint", "Reds")
for (i in 1:3) {
  mf_map(x = geo_dep, var = candidat[i], type = "choro",
       pal = ma_palette[i], 
       breaks = c(5, 15, 20, 22.5, 25, 27.5, 30, 45, 60),
       leg_title = candidat[i], 
       leg_val_rnd = 2)
}


## ---- spurious scatter plot, fig.width = 12, fig.height = 4, fig.cap = "Nuage de points des voix obtenus par Macron et Le Pen en valeurs absolue (sur la gauche), en part d'exprimés (au centre) et en part d'inscrits (sur la droite)", fig.align="center"----
par(mfrow = c(1, 3), oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1))
plot(Macron_VOIX ~ `Le Pen_VOIX`, data = res_2022, 
     main = "Nombre de voix")
plot(Macron_EXP ~ `Le Pen_EXP`, data = res_2022, 
     main = "Nombre de voix / nombre exprimes")
plot(Macron ~ Le_Pen, data = vote_share_2, 
     main = "Nombre de voix / nombre inscrits")


## ---- charger librairie traitement zero, message = F------------------------------------------
library(zCompositions)
data(LPdataZM)


## ---- afficher configuration des missing, fig.width = 12,fig.height =5, fig.cap = "Configuration des valeurs manquantes par individu et variable", warning = F, fig.align="center"----
zPatterns(LPdataZM, label = NA)


## ---- remplacement des missing----------------------------------------------------------------
require("missForest")
LPdataZM_nm <- missForest(LPdataZM)$ximp


## ---- affichage configuration des zeros, fig.width = 12, fig.height =5, fig.cap = "Configuration des zéros par individu et variable", warning = F, fig.align="center"----
zPatterns(LPdataZM_nm, label = 0, show.means = TRUE)


## ---- suppression de Ni-----------------------------------------------------------------------
LPdataZM_nm <- LPdataZM_nm[, !(names(LPdataZM_nm) %in% "Ni")]


## ---- methode multiplicative------------------------------------------------------------------
dl <- apply(LPdataZM_nm, 2, function(x) min(x[x != 0]))
LPdataZM_multRepl <- multRepl(LPdataZM_nm, label = 0, dl = dl)


## ---- methode multiplicative log normale------------------------------------------------------
LPdataZM_multLN <- multLN(LPdataZM_nm, label = 0, dl = dl)


## ---- methode EM------------------------------------------------------------------------------
LPdataZM_lrEM <- lrEM(LPdataZM_nm, label = 0, dl = dl)


## ---- ternary diagram, fig.width = 12, fig.height = 6, message = F, fig.cap = "Diagramme ternaire des trois candidats arrivés en tête du 1er tour, avec ajout des couleurs sur les axes dans la figure de droite, afin de simplifier la lecture des échelles", fig.align="center"----
library("ggtern")
p1 <- ggtern(data = vote_share_1, mapping = aes(x = Macron, 
                      y = Le_Pen, z = Mélenchon)) +
 geom_point(size = 1.5) 
p2 <- p1 +  theme_rgbw()
grid.arrange(p1, p2, ncol = 2)


## ---- ternary diagram avec info aux, fig.width = 12, fig.height = 6, message = F, fig.cap = "Ajout d'une information supplémentaire qualitative (à gauche) et quantitative (à droite)", fig.align="center"----
p3 <- ggtern(data = geo_dep, mapping = aes(x = Macron, y = Le_Pen, z = Mélenchon)) +
 geom_point(aes(colour = vainqueur), size = 1.5)  
p4 <- ggtern(data = res_2022, mapping = aes(x = Macron_VOIX, y = `Le Pen_VOIX`, 
                                           z = Mélenchon_VOIX)) +
 geom_point(aes(size = Inscrits))  
grid.arrange(p3, p4, ncol = 2)


## ---- ternary conditionnel, fig.width = 12, fig.height = 12, message = F, fig.cap = "Diagrammes ternaires conditionnelles à la région", fig.align="center"----
ggtern(data = geo_dep, mapping = aes(x = Macron, y = Le_Pen, z = Mélenchon)) +
 geom_point(mapping = aes(size = Inscrits, colour = vainqueur))  +
  facet_wrap(~ nom_reg)


## ---- fonction pour créer une séquence de nombre dans le simplexe-----------------------------
seq_simplex <- function(nb_noeud) {
  interval <- seq(0, 1, length.out = nb_noeud)
  res <- NULL
  for(i in 1:nb_noeud) {
    for(j in 1:(nb_noeud - i)) {
      res <- rbind(res,
                 c(x = 1 - interval[j] - interval[i], 
                   y = interval[j], 
                   z = interval[i]))
    }
  }
  return(res[-nrow(res), ])
}


## ---- triangle de Maxwell, fig.width = 4.5, fig.height = 4.5, message = F, fig.cap = "Triangle de Maxwell", fig.align="center"----
my_data <- as.data.frame(seq_simplex(100))

ggtern(data = my_data, mapping = aes(x = x, y = y, z = z)) +
  geom_point(size = 1.5, col = rgb(my_data$x, my_data$y, my_data$z)) 


## ---- diagramme ternaire sur données temporelles, fig.width = 4.5, fig.height = 4.5, message = F, fig.cap = "Diagramme ternaire sur des données temporelles", fig.align="center"----
time_chart_wide <- pivot_wider(time_chart, 
          names_from = parti, values_from = vote)
ggtern(data = time_chart_wide,
       mapping = aes(x = droite, y = gauche, z = extrême)) +
  geom_point(size = 1.5) +
  geom_line()


## ---- charger compositions, message = F-------------------------------------------------------
library("compositions")
comp_a <- acomp(res_2022[, c("Macron_VOIX", "Le Pen_VOIX", "Mélenchon_VOIX")])
names(comp_a) <- c("Macron", "Le Pen", "Mélenchon")


## ---- creer acomp-----------------------------------------------------------------------------
comp_b <- acomp(res_2022[, c("Macron_EXP", "Le Pen_EXP", "Mélenchon_EXP")])
names(comp_b) <- c("Macron", "Le Pen", "Mélenchon")


## ---- afficher acomp--------------------------------------------------------------------------
comp_a[1, ]
comp_b[1, ]


## ----  cloture--------------------------------------------------------------------------------
clo(c(1, 2, 3))


## ---- afficher premieres lignes---------------------------------------------------------------
head(comp_a[, c("Macron", "Le Pen")])


## ---- cenvertir acomp en matrix---------------------------------------------------------------
head(as(comp_a[, c("Macron", "Le Pen")], "matrix"))


## ---- creer donnees chimie--------------------------------------------------------------------
chimie <- data.frame(Cr = c(27.50, 30.40, 25.60), 
                     B = c(17, 23, 14), 
                     P = c(148, 433, 135),
                     V = c(29, 42, 33),
                     Cu = c(2.7, 3.8, 0),
                     Ti = c(4335, 3305, 3925),
                     Ni = c(0, 16.6, 14.2))
chimie_acomp <- acomp(chimie)
chimie_acomp


## ---- traiter les zeros-----------------------------------------------------------------------
chimie_acomp_2 <- zeroreplace(chimie_acomp, d = rep(0.001, 7), a = 2/3)


## ---- extraire colonnes sans cloture----------------------------------------------------------
aplus(chimie_acomp_2, c("Ti", "Ni"))


## ---- amalagamer colonnes---------------------------------------------------------------------
totals(aplus(chimie_acomp_2, c("Ti", "Ni")))


## ---- ajouter une composante------------------------------------------------------------------
acomp(
  cbind(aplus(chimie_acomp_2, c("Cr", "B", "P", "V", "Cu")),
      Ni_Ti = totals(aplus(chimie_acomp_2, c("Ti", "Ni"))))
)


## ---------------------------------------------------------------------------------------------
acompmargin(chimie_acomp_2, c("Cr", "B", "P", "V", "Cu"))


## ---- operateur perturbation------------------------------------------------------------------
a <- comp_a[1, ] 
b <- comp_a[2, ]
d <- a + b
d


## ---- perturber en dehors de acomp------------------------------------------------------------
produit <- res_2022[1, c("Macron_VOIX", "Le Pen_VOIX", "Mélenchon_VOIX")] * 
  res_2022[2, c("Macron_VOIX", "Le Pen_VOIX", "Mélenchon_VOIX")]
d_bis <- produit / sum(produit)
d_bis


## ---- operateur puissance---------------------------------------------------------------------
d * (1 / 2)


## ---- puissance en dehors de acomp------------------------------------------------------------
power <- d_bis ^ 0.5 
power / sum(power)


## ---- plot acomp, fig.width = 12, fig.height = 4.5, fig.cap = "Illustration de la moyenne dans le simplexe de deux vecteurs de composition",fig.align="center"----
par(mfrow = c(1, 3))
b <- acomp(c(0.2, 0.5, 0.3))
a1 <- acomp(c(0.4, 0.1, 0.5))
plot(a1)
plot(b, add = T)
plot((a1 + b) * (1 / 2), add = T, pch = 16, col = "red")
a2 <- acomp(c(0.4, 0.01, 0.59))
plot(a2)
plot(b, add = T)
plot((a2 + b) * (1 / 2), add = T, pch = 16, col = "red")
a3 <- acomp(c(0.4, 0.001, 0.599))
plot(a3)
plot(b, add = T)
plot((a3 + b) * (1 / 2), add = T, pch = 16, col = "red")


## ---- plot acomp avec lines, fig.width = 4.5, fig.height = 4.5, fig.cap = "Illustration de la moyenne dans le simplexe de deux vecteurs de composition et représentation d'une droite",fig.align="center"----
plot(a)
plot(b, add = T)
plot(d * (1 / 2), add = T, pch = 16, col = "red")
lines(d * seq(-100, 100, length.out = 1000), lty = 2)


## ---- creer data farine-----------------------------------------------------------------------
farine <- data.frame(type = c("T110", "T150", "T45", "T55", "T65", "T80", 
    "mais", "pois chiche", "riz", "sarrasin", "seigle"),
    proteines = c(10.3, 12.2, 9.94, 9.9, 14.9, 10.9, 6.23, 22.4, 8, 11.5, 8.7),
    glucides = c(70.2, 66.67, 76.31, 75.2, 69.1, 74.97, 78.74, 
                 57.90, 74.8, 68.43, 71.63),
    lipides = c(1.5, 1.52, 0.82, 1, 1, 1.18, 2.1, 6.69, 2.5, 2.19, 1.37))


## ---- ternary diagram de farine, fig.cap = "Diagramme ternaire de la composition en protéine, glucides et lipides de plusieurs types de farines", warning = F, fig.align="center"----
farine_comp <- acomp(farine[, c("proteines", "glucides", "lipides")])
ggtern(data = farine, mapping = aes(x = proteines, y = lipides, z = glucides)) +
  geom_point(size = 1.5) +
  geom_point(data = data.frame(t(as(mean(farine_comp), "matrix"))), 
      mapping = aes(x = proteines, y = lipides, z = glucides), col = "red")  +  
  theme(legend.position = c(0, 1), legend.justification = c(1, 1)) + 
  theme_rgbw()


## ---- centrer data farine---------------------------------------------------------------------
farine_mean <- farine_comp[1, ]
for (k in 2:nrow(farine_comp)) {
  farine_mean <- farine_mean + farine_comp[k, ]
}
farine_mean <- farine_mean / nrow(farine_comp)


## ---- moyenne geometrique sur data acomp------------------------------------------------------
mean(farine_comp)


## ---- centrer data----------------------------------------------------------------------------
farine_comp_ce <- farine_comp - farine_mean


## ---- fonction local pour calculer les coordonnes du ternary diagram--------------------------
compo_to_ternary <- function(s_3, A = c(0, 0), B = c(1, 0), 
                             C = c(0.5, sqrt(3) / 2)) {
  if (length(s_3) == 3)
    s_3 <- t(matrix(s_3))
  Y_simplex_x <- s_3[, 1] * A[1] + s_3[, 2] * B[1] + s_3[, 3] * C[1]
  Y_simplex_y <- s_3[, 1] * A[2] + s_3[, 2] * B[2] + s_3[, 3] * C[2]
  return(cbind(Y_simplex_x, Y_simplex_y))
}


## ---- ternary diagram avec data centree, fig.width = 4, fig.height = 4, fig.cap = "Diagramme ternaire des données de composition lipides, glucides, protéines, centré autour de la moyenne géométrique",fig.align="center"----
op <- par(oma = c(.1, .1, .1, .1), mar = c(0.3, 1.2, .5, 1.4))
plot(farine_comp_ce, pch = 16, cex = 0.5, labels = "")
text(c(0.04, .99, 1/2), c(-.06, -.06, sqrt(3)/2 + 0.02), 
     c("proteines", "glucides", "lipides"), pos = 3,  cex = 0.8,
     col = c("#E16A86", "#50A315", "#009ADE"))
coord_leg <- compo_to_ternary(farine_comp_ce)
text(coord_leg[, 1], coord_leg[, 2], farine$type, cex = 0.5, pos = 3)
# l'axe Lipides
for (k in c(0.01, 0.05, 0.1, 0.25)) {
  c_1 <- c(0.000001, 1 - k, k)
  c_2 <- c(1 - k, 0.000001, k)
  lines(acomp(rbind(acomp(c_1) - farine_mean,
              acomp(c_2) - farine_mean)), steps = 1, lty = 4, 
        lwd = 0.7, col = "#009ADE")
  coord_leg <- compo_to_ternary(acomp(c_1) - farine_mean)
  text(coord_leg[, 1], coord_leg[, 2], k, cex = 0.5, pos = 4, col = "#009ADE")
}
# l'axe Glucides
for (k in c(0.75, 0.9, 0.95, 0.99)) {
  c_1 <- c(0.000001, k, 1 - k)
  c_2 <- c(1 - k, k, 0.000001)
  lines(acomp(rbind(acomp(c_1) - farine_mean,
    acomp(c_2) - farine_mean)), steps = 1, lty = 4, lwd = 0.7, col = "#50A315")
  coord_leg <- compo_to_ternary(acomp(c_2) - farine_mean)
  text(coord_leg[, 1], coord_leg[, 2], k, cex = 0.5, pos = 1, 
       srt = -120, col = "#50A315")
}
# l'axe Protéines
for (k in c(0.05, 0.1, 0.25)) {
  c_1 <- c(k, 0.000001, 1 - k)
  c_2 <- c(k, 1 - k, 0.000001)
  lines(acomp(rbind(acomp(c_1) - farine_mean,
    acomp(c_2) - farine_mean)), steps = 1, lty = 4, lwd = 0.7, col = "#E16A86")
  coord_leg <- compo_to_ternary(acomp(c_1) - farine_mean)
  text(coord_leg[, 1], coord_leg[, 2], k, cex = 0.5, 
       pos = 3, srt = 120, col = "#E16A86")
}
# moyenne géométrique
g <- compo_to_ternary(farine_mean - farine_mean)
points(g[, 1], g[, 2], pch = 15, col = "red")


## ---- produit scalaire------------------------------------------------------------------------
scalar(farine_comp[1, ], farine_comp[2, ])


## ---- ps en dehors de acomp-------------------------------------------------------------------
D <- 3
ps <- 0
x <- farine_comp[1, ]
y <- farine_comp[2, ]
for (j in 1:(D - 1)) 
  for (i in (j + 1):D)
    ps <- ps + log(x[i] / x[j]) * log(y[i] / y[j])
(ps <- ps / 3)


## ---- normaliser------------------------------------------------------------------------------
norm(x)
norm(y)


## ---- normaliser en dehors de acomp-----------------------------------------------------------
norm_x <- 0
norm_y <- 0
for (j in 1:(D - 1)) {
  for (i in (j + 1):D) { 
    norm_x <- norm_x + log(x[i] / x[j]) * log(x[i] / x[j])
    norm_y <- norm_y + log(y[i] / y[j]) * log(y[i] / y[j])
  }
}
norm_x <- sqrt(norm_x / 3)
norm_y <- sqrt(norm_y / 3)


## ---- distance Aitchison----------------------------------------------------------------------
dist(farine_comp[1:2, ])


## ---- distance Aitchison alternative----------------------------------------------------------
sqrt(scalar(x-y, x-y))


## ---- transformation alr ilr clr--------------------------------------------------------------
V <- matrix(c(2 / sqrt(6), - 1 / sqrt(6), - 1 / sqrt(6),
            0, 1/ sqrt(2), - 1 / sqrt(2)), ncol = 2)
alr_a <- alr(comp_a, ivar = 3)
clr_a <- clr(comp_a)
ilr_a <- ilr(comp_a, V = V)


## ---- cartographie moyenne geometrique, echo = F, fig.width = 7, fig.height = 5, fig.cap = "Cartographie des valeurs de $g(x)$ dans le diagramme ternaire", fig.align="center"----
seq_compo <- NULL
pal1 <- RColorBrewer::brewer.pal(9, "YlGn")
for(i in seq(0.01, 0.99, 0.01)) {
  for(j in seq(0.01, 0.99, 0.01)) {
    if(1 - i - j > 0)
    seq_compo <- rbind(seq_compo,
                       c(i, j, 1 - i - j))
  }
}
g_x <- (seq_compo[, 1] * seq_compo[, 2] * seq_compo[, 3]) ^ (1 / 3)
# clr 1
plot(acomp(c(1/3, 1/3, 1/3)), pch = 16, cex = 1, 
     labels = c("Macron", "Le Pen", "Mélenchon"),
     main = "Clr 1")
bk <- seq(0, 1/3, length.out = 9)
bk[1] <- 0
bk[9] <- 1/3 
for(i in 1:nrow(seq_compo)) {
  ind <- findInterval(g_x[i], bk, all.inside = TRUE)
  plot(acomp(seq_compo[i, ]), col = pal1[ind], add = T, pch = 16)
}
plot(acomp(c(1/3, 1/3, 1/3)), pch = 15, cex = 0.75, col = "red", add = T)
decoup <- c("<=-3", "]-3;-2]", "]-2;-1]", 
            "]-1;0]", "]0;1]", "]1;2]", "]2;3]", ">3")
legend("topleft", legend = decoup, cex = 0.8, title = "g(x)", 
       fill = pal1)


## ---- somme clr egal zero---------------------------------------------------------------------
all(round(apply(clr_a, 1, sum), 12) == 0)


## ---- propriete clr---------------------------------------------------------------------------
(diag(3) - matrix(1/3, 3, 3)) %*% log(as.numeric(comp_a[1, ]))
clr_a[1, ]


## ---- propriete ilr---------------------------------------------------------------------------
ilr(x + y)
ilr(x) + ilr(y)


## ---- propriete ilr 2-------------------------------------------------------------------------
ilr(2 * x)
2 * ilr(x)


## ---- propriete ilr 3-------------------------------------------------------------------------
scalar(x, y)
sum(ilr(x) * ilr(y))


## ---- propriete ilr 4-------------------------------------------------------------------------
norm(x)
sqrt(sum(ilr(x) * ilr(x)))


## ---- propriete ilr 5-------------------------------------------------------------------------
norm(x-y)
dist(rbind(
  ilr(x),
  ilr(y)
))


## ---- scatter plot des transformations, fig.width = 6, fig.height = 3, fig.cap = "Représentation des scores des trois principaux candidats dans les espaces alr et ilr", fig.align="center"----
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1), oma = c(0, 0, 0, 0))
plot(alr_a[, 1], alr_a[, 2], xlab = "alr 1", ylab = "alr 2", main = "ALR")
abline(h = 0, v = 0, lty = 2)
plot(ilr_a[, 1], ilr_a[, 2], xlab = "ilr 1", ylab = "ilr 2", main = "ILR")
abline(h = 0, lty = 2)


## ---- cartographie des ilr dans le diagramme ternaire, echo = F, fig.width = 8, fig.height = 4, fig.cap = "Cartographie des valeurs des ilr 1 et ilr 2 dans le diagramme ternaire", fig.align="center"----
seq_compo <- NULL
pal1 <- RColorBrewer::brewer.pal(9, "BrBG")
for(i in seq(0.01, 0.99, 0.01)) {
  for(j in seq(0.01, 0.99, 0.01)) {
    if(1 - i - j > 0)
    seq_compo <- rbind(seq_compo,
                       c(i, j, 1 - i - j))
  }
}
op <- par(mfrow = c(1, 2))
# ilr 1
plot(acomp(c(1/3, 1/3, 1/3)), pch = 16, cex = 0.5, 
     labels = c("Macron", "Le Pen", "Mélenchon"),
     main = "ilr 1")
vec_1 <- NULL
bk <- seq(-4, 4, length.out = 9)
bk[1] <- -100
bk[9] <- 100 
for(i in 1:nrow(seq_compo)) {
  ilr1 <- log(seq_compo[i, 1]^2 / (seq_compo[i, 2]*seq_compo[i, 3])) / sqrt(6)
  ind <- findInterval(ilr1, bk, all.inside = TRUE)
  plot(acomp(seq_compo[i, ]), col = pal1[ind], add = T, pch = 16)
  vec_1 <- c(vec_1, ilr1)
}
plot(acomp(c(1/3, 1/3, 1/3)), pch = 15, cex = 0.75, col = "red", add = T)
decoup <- c("<=-3", "]-3;-2]", "]-2;-1]", 
            "]-1;0]", "]0;1]", "]1;2]", "]2;3]", ">3")
legend("topleft", legend = decoup, cex = 0.6, title = "ilr 1", 
       fill = pal1)
# ilr 2
plot(acomp(c(1/3, 1/3, 1/3)), pch = 16, cex = 0.5, 
     labels = c("Macron", "Le Pen", "Mélenchon"),
     main = "ilr 2")
vec_2 <- NULL
bk <- seq(-4, 4, length.out = 9)
bk[1] <- -100
bk[9] <- 100 
for(i in 1:nrow(seq_compo)) {
  ilr2 <- log(seq_compo[i, 2] / seq_compo[i, 3]) / sqrt(2)
  ind <- findInterval(ilr2, bk, all.inside = TRUE)
  plot(acomp(seq_compo[i, ]), col = pal1[ind], add = T, pch = 16)
  vec_2 <- c(vec_2, ilr2)
}
plot(acomp(c(1/3, 1/3, 1/3)), pch = 15, cex = 0.75, col = "red", add = T)
decoup <- c("<=-3", "]-3;-2]", "]-2;-1]", 
            "]-1;0]", "]0;1]", "]1;2]", "]2;3]", ">3")
legend("topleft", legend = decoup, cex = 0.6, title = "ilr 2", 
       fill = pal1)
par(op)


## ---- sccater plot des clr, fig.width = 9, fig.height = 3, fig.cap = "Représentation des scores dans l'espace clr", fig.align="center"----
par(mfrow = c(1, 3), mar = c(4, 4, 1, 1), oma = c(0, 0, 0, 0))
plot(clr_a[, 1], clr_a[, 2], xlab = "clr 1", ylab = "clr 2")
abline(h = 0, v = 0, lty = 2)
plot(clr_a[, 1], clr_a[, 3], xlab = "clr 1", ylab = "clr 3")
abline(h = 0, v = 0, lty = 2)
plot(clr_a[, 2], clr_a[, 3], xlab = "clr 2", ylab = "clr 3")
abline(h = 0, v = 0, lty = 2)


## ---- cartographie des clr, echo = F, fig.width = 12, fig.height = 4, fig.cap = "Cartographie des valeurs des CLR dans le diagramme ternaire", fig.align="center"----
seq_compo <- NULL
pal1 <- RColorBrewer::brewer.pal(9, "BrBG")
for(i in seq(0.01, 0.99, 0.01)) {
  for(j in seq(0.01, 0.99, 0.01)) {
    if(1 - i - j > 0)
    seq_compo <- rbind(seq_compo,
                       c(i, j, 1 - i - j))
  }
}
g_x <- (seq_compo[, 1] * seq_compo[, 2] * seq_compo[, 3]) ^ (1 / 3)
clr1 <- log(seq_compo[, 1] / g_x)
clr2 <- log(seq_compo[, 2] / g_x)
clr3 <- log(seq_compo[, 3] / g_x)
op <- par(mfrow = c(1, 3))
# clr 1
plot(acomp(c(1/3, 1/3, 1/3)), pch = 16, cex = 0.5, 
     labels = c("Macron", "Le Pen", "Mélenchon"),
     main = "Clr 1")
bk <- seq(-4, 4, length.out = 9)
bk[1] <- -100
bk[9] <- 100 
for(i in 1:nrow(seq_compo)) {
  ind <- findInterval(clr1[i], bk, all.inside = TRUE)
  plot(acomp(seq_compo[i, ]), col = pal1[ind], add = T, pch = 16)
}
plot(acomp(c(1/3, 1/3, 1/3)), pch = 15, cex = 0.75, col = "red", add = T)
decoup <- c("<=-3", "]-3;-2]", "]-2;-1]", 
            "]-1;0]", "]0;1]", "]1;2]", "]2;3]", ">3")
legend("topleft", legend = decoup, cex = 0.8, title = "clr 1", 
       fill = pal1)
# clr 2
plot(acomp(c(1/3, 1/3, 1/3)), pch = 16, cex = 0.5, 
     labels = c("Macron", "Le Pen", "Mélenchon"),
     main = "Clr 2")
bk <- seq(-4, 4, length.out = 9)
bk[1] <- -100
bk[9] <- 100 
for(i in 1:nrow(seq_compo)) {
  ind <- findInterval(clr2[i], bk, all.inside = TRUE)
  plot(acomp(seq_compo[i, ]), col = pal1[ind], add = T, pch = 16)
}
plot(acomp(c(1/3, 1/3, 1/3)), pch = 15, cex = 0.75, col = "red", add = T)
decoup <- c("<=-3", "]-3;-2]", "]-2;-1]", 
            "]-1;0]", "]0;1]", "]1;2]", "]2;3]", ">3")
legend("topleft", legend = decoup, cex = 0.8, title = "clr 2", 
       fill = pal1)
# clr 3
plot(acomp(c(1/3, 1/3, 1/3)), pch = 16, cex = 0.5, 
     labels = c("Macron", "Le Pen", "Mélenchon"),
     main = "Clr 3")
bk <- seq(-4, 4, length.out = 9)
bk[1] <- -100
bk[9] <- 100 
for(i in 1:nrow(seq_compo)) {
  ind <- findInterval(clr3[i], bk, all.inside = TRUE)
  plot(acomp(seq_compo[i, ]), col = pal1[ind], add = T, pch = 16)
}
plot(acomp(c(1/3, 1/3, 1/3)), pch = 15, cex = 0.75, col = "red", add = T)
decoup <- c("<=-3", "]-3;-2]", "]-2;-1]", 
            "]-1;0]", "]0;1]", "]1;2]", "]2;3]", ">3")
legend("topleft", legend = decoup, cex = 0.8, title = "clr 3", 
       fill = pal1)
par(op)


## ---- transformation de droites dans le simplexe, echo = F, fig.width = 12, fig.height = 4, fig.cap = "Transformations de droites dans le simplexe lorsque l'espace initial est ilr et alr", fig.align="center"----
my_pal <- RColorBrewer::brewer.pal(12, "Set3")
par(mfrow = c(1, 3))
# espace transforme
x <- seq(-10, 10, length.out = 100)
plot(x, x + 1, type = "l", xlim = c(-1, 1), ylim = c(-1, 1),
     xlab = 'coord 1', ylab = 'coord 2', col = my_pal[1])
lines(x, x, type = "l", col = my_pal[2])
lines(x, x - 1, type = "l", col = my_pal[3])
lines(x, -x + 1, type = "l", col = my_pal[4])
lines(x, -x, type = "l", col = my_pal[5])
lines(x, -x - 1, type = "l", col = my_pal[6])
lines(rep(0, 100), seq(-10, 10, length.out = 100), col = my_pal[7])
lines(rep(-1, 100), seq(-10, 10, length.out = 100), col = my_pal[8])
lines(rep(1, 100), seq(-10, 10, length.out = 100), col = my_pal[9])
lines(seq(-10, 10, length.out = 100), rep(0, 100), col = my_pal[10])
lines(seq(-10, 10, length.out = 100), rep(-1, 100), col = my_pal[11])
lines(seq(-10, 10, length.out = 100), rep(1, 100), col = my_pal[12])
# ilr to simplexe
plot(acomp(ilrInv(cbind(x, x + 1))), type = "l", col = my_pal[1], 
     main = "ILR -> simplexe", labels = c("x1", "x2", "x3"))
lines(acomp(ilrInv(cbind(x, x))), col = my_pal[2])
lines(acomp(ilrInv(cbind(x, x - 1))), type = "l", col = my_pal[3])
lines(acomp(ilrInv(cbind(x, -x + 1))), type = "l", col = my_pal[4])
lines(acomp(ilrInv(cbind(x, -x))), type = "l", col = my_pal[5])
lines(acomp(ilrInv(cbind(x, -x - 1))), type = "l", col = my_pal[6])
lines(acomp(ilrInv(cbind(rep(0, 100), seq(-10, 10, length.out = 100)))), col = my_pal[7])
lines(acomp(ilrInv(cbind(rep(-1, 100), seq(-10, 10, length.out = 100)))), col = my_pal[8])
lines(acomp(ilrInv(cbind(rep(1, 100), seq(-10, 10, length.out = 100)))), col = my_pal[9])
lines(acomp(ilrInv(cbind(seq(-10, 10, length.out = 100), rep(0, 100)))), col = my_pal[10])
lines(acomp(ilrInv(cbind(seq(-10, 10, length.out = 100), rep(-1, 100)))), col = my_pal[11])
lines(acomp(ilrInv(cbind(seq(-10, 10, length.out = 100), rep(1, 100)))), col = my_pal[12])
# alr to simplexe
plot(acomp(alrInv(cbind(x, x + 1))), type = "l", col = my_pal[1], 
     main = "ALR -> simplexe", labels = c("x1", "x2", "x3"))
lines(acomp(alrInv(cbind(x, x))), col = my_pal[2])
lines(acomp(alrInv(cbind(x, x - 1))), type = "l", col = my_pal[3])
lines(acomp(alrInv(cbind(x, -x + 1))), type = "l", col = my_pal[4])
lines(acomp(alrInv(cbind(x, -x))), type = "l", col = my_pal[5])
lines(acomp(alrInv(cbind(x, -x - 1))), type = "l", col = my_pal[6])
lines(acomp(alrInv(cbind(rep(0, 100), seq(-10, 10, length.out = 100)))), col = my_pal[7])
lines(acomp(alrInv(cbind(rep(-1, 100), seq(-10, 10, length.out = 100)))), col = my_pal[8])
lines(acomp(alrInv(cbind(rep(1, 100), seq(-10, 10, length.out = 100)))), col = my_pal[9])
lines(acomp(alrInv(cbind(seq(-10, 10, length.out = 100), rep(0, 100)))), col = my_pal[10])
lines(acomp(alrInv(cbind(seq(-10, 10, length.out = 100), rep(-1, 100)))), col = my_pal[11])
lines(acomp(alrInv(cbind(seq(-10, 10, length.out = 100), rep(1, 100)))), col = my_pal[12])


## ---- transformation de cercles/ellipses dans le simplexe, echo = F, fig.width = 12, fig.height = 4, fig.cap = "Transformations de cercles et d'ellipses dans le simplexe lorsque l'espace initial est ilr et alr", fig.align="center"----
# initialize a plot
# prepare "circle data"
radius = list(
  c(0.5, 0.5),
  c(1, 1),
  c(0.5, 1),
  c(1, 2)
)
center_xy <- list(
  c(0, 0),
  c(1, 2),
  c(-1, 0)
)
par(mfrow = c(1, 3))
plot(c(0, 1, -1), c(0, 2, 0), pch = 16, asp = 1, 
     xlab = "coord 1", ylab = "coord 2", xlim = c(-2, 2), ylim = c(-1, 2))
abline(h = 0, v = 0, lty = 2)
theta = seq(0, 2 * pi, length = 200) # angles for drawing points around the circle

# draw the circle/ellipse
for (i in 1:length(radius)) {
     for (j in 1:length(center_xy)) {
       lines(x = radius[[i]][1] * cos(theta) + center_xy[[j]][1], 
             y = radius[[i]][2] * sin(theta) + center_xy[[j]][2],
             col = my_pal[(i - 1) * length(radius) + j])
     }
}

# transformation dans le simplexe avec ilrInv
plot(acomp(ilrInv(cbind(c(0, 1, -1), c(0, 2, 0)))), pch = 16, 
     main = "ILR -> simplexe", labels = c("x1", "x2", "x3"))
for (i in 1:length(radius)) {
     for (j in 1:length(center_xy)) {
       lines(acomp(ilrInv(cbind(radius[[i]][1] * cos(theta) + center_xy[[j]][1],
                                radius[[i]][2] * sin(theta) + center_xy[[j]][2]))),
             col = my_pal[(i - 1) * length(radius) + j])
     }
}

# transformation dans le simplexe avec alrInv
plot(acomp(alrInv(cbind(c(0, 1, -1), c(0, 1, 0)))), pch = 16, 
     main = "ALR -> simplexe", labels = c("x1", "x2", "x3"))
for (i in 1:length(radius)) {
     for (j in 1:length(center_xy)) {
       lines(acomp(alrInv(cbind(radius[[i]][1] * cos(theta) + center_xy[[j]][1],
                                radius[[i]][2] * sin(theta) + center_xy[[j]][2]))),
             col = my_pal[(i - 1) * length(radius) + j])
     }
}


## ---- transformations inverses----------------------------------------------------------------
mean_alr <- apply(alr_a, 2, mean)
mean_ilr <- apply(ilr_a, 2, mean)
mean_clr <- apply(clr_a, 2, mean) 
alrInv(mean_alr)
ilrInv(mean_ilr)
clrInv(mean_clr)


## ---- variance dans ilr-----------------------------------------------------------------------
var(ilr_a)


## ---- inverse ilr-----------------------------------------------------------------------------
V %*% var(ilr_a) %*% t(V)


## ---- fonction var sur acomp------------------------------------------------------------------
var_s <- var(comp_a)


## ---- simuler une loi multinomiale------------------------------------------------------------
my_multi <- t(rmultinom(1000, size = 30, prob = c(1/6, 1/3, 1/2)))


## ---- aggreger les valeurs simulees-----------------------------------------------------------
my_multi <- my_multi[-which(apply(my_multi, 1, function(x) any(x == 0))), ]
my_multi_ag <- aggregate(rep(1, nrow(my_multi)), by = list(x1 = my_multi[, 1],
                              x2 = my_multi[, 2],
                              x3 = my_multi[, 3]), FUN = sum)


## ---- plot de multinomiale, gig.width = 6, fig.height = 6, fig.cap = "Echantillon simulé selon une loi $M(1/6,1/3,1/2, N=30)$",fig.align="center"----
pal1 <- RColorBrewer::brewer.pal(9, "YlGn")
bk <- seq(0, 45, by = 5)
ind <- findInterval(my_multi_ag$x, bk, all.inside = TRUE)
plot(acomp(my_multi_ag[, 1:3]), col = pal1[ind], pch = 16)
plot(acomp(c(1/6, 1/3, 1/2)), pch = 15, cex = 1, col = "red", add = T)
decoup <- c("<=5", "]5;10]", "]10;15]", "]15;20]", "]20;25]", "]25;30]",
            "]30;35]", "]35;40]", ">40")
legend("topleft", legend = decoup, cex = 0.8, title = "Nb points \n(sur 1000)", 
       fill = pal1)


## ---- plot de lois normales dans le simplexe, gig.width = 6, fig.height = 6, fig.cap = "Distribution de lois normales dans le simplexe pour différents paramètres de la moyenne (en lignes) et de la matrice de variance-covariance dans l'espace clr (en colonnes)",fig.align="center"----
library(latex2exp)
my_mean_vec <- acomp(rbind(
  c(1/3, 1/3, 1/3),
  c(0.5, 0.3, 0.2),
  c(0.2, 0.2, 0.6)
))

my_var_list <- list(
  0.1 * matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), ncol = 3),
  0.1 * matrix(c(3, 0, 0, 0, 2, 0, 0, 0, 1), ncol = 3),
  0.1 * matrix(c(1, 0.8, 0.2, 0.8, 1, 0.5, 0.2, 0.5, 1), ncol = 3)
)
opar <- par(mar = c(1, 1, 1, 1), mfrow = c(3, 3))
for(k in 1:3) {
  for(j in 1:3) {
    my_mean <- my_mean_vec[k, ]
    my_var <- my_var_list[[j]]
    plot(my_mean, pch = 15, col = "red", main = "données simulées")
    for(p in c(0.5, 1:9, 9.5)/10) {
      r <- sqrt(qchisq(p = p, df = 2))
      ellipses(my_mean, my_var, r, col="grey")
    }
    xr <- rnorm.acomp(n = 100, mean = my_mean, var = my_var)
    plot(xr, add = TRUE, pch = 19, cex = 0.5)
    if(j == 1)
      text(0.02, 0.5, TeX(sprintf(r'($\mu_ %g$)', k)), cex = 0.95)
    if(k == 1)
      text(0.5, 0.97, TeX(sprintf(r'($\Sigma_ %g$)', j)), cex = 0.95)
  }
}


## ---- simulation data et representation lois, fig.width = 12, fig.height = 5, fig.cap = "Données simulées selon une loi normale du simplexe (figure sur la gauche); données observées (figure sur la droite)",fig.align="center"----
opar <- par(mar = c(3, 3, 1, 1), mfrow = c(1, 2))
plot(clrInv(mean_clr), pch = 15, 
     col = "red", main = "données simulées")
for(p in c(0.5, 1:9, 9.5)/10) {
  r <- sqrt(qchisq(p = p, df = 2))
  ellipses(clrInv(mean_clr), var_s, r, col="grey")
}
xr <- rnorm.acomp(n = 107, mean = clrInv(mean_clr), var = var_s)
plot(xr, add = TRUE, pch = 19, cex = 0.5)

plot(clrInv(mean_clr), pch = 15, col = "red",
     main = "données observées")
for(p in c(0.5, 1:9, 9.5)/10) {
  r <- sqrt(qchisq(p = p, df = 2))
  ellipses(clrInv(mean_clr), var_s, r, col="grey")
}
plot(comp_a, add = T, pch = 19, cex = 0.5)
par(opar)



## ---- qqplot data, fig.width = 6, fig.height = 6, fig.cap = "QQplot des lois marginales sur les log-ratio",fig.align="center"----
qqnorm(comp_a)


## ---- test normalite multivariee--------------------------------------------------------------
energy::mvnorm.etest(ilr(comp_a), R = 199)


## ---- representation dirichlet, fig.width = 10, fig.height = 4, fig.cap = "Fonction de densité de la loi de Dirichlet lorsque D = 2",fig.align="center"----
x <- seq(0, 1, 0.01)
my_comp <- acomp(cbind(x, 1 - x))
par(mfrow = c(1, 3))
plot(x, dDirichlet(my_comp, alpha = c(A = 0.3, B = 0.3)), type = "l", ylab = "f(x)",
     main = TeX(r"($\alpha_1=0.3, alpha_2=0.3$)"))
plot(x, dDirichlet(my_comp, alpha = c(A = 1, B = 1)), type = "l", ylab = "f(x)",
     main = TeX(r"($\alpha_1=1, alpha_2=1$)"))
plot(x, dDirichlet(my_comp, alpha = c(A = 2, B = 2)), type = "l", ylab = "f(x)",
     main = TeX(r"($\alpha_1=2, alpha_2=2$)"))


## ---- estimer parametres dirichlet------------------------------------------------------------
fit_d <- fitDirichlet(comp_a)


## ---- representation loi dirichlet, fig.width = 5, fig.height = 5, fig.cap = "Estimation d'une loi de Dirichlet sur les données d'élection",fig.align="center"----
opar <- par(mar = c(3, 3, 1, 1))
myalpha = fit_d$alpha
plot(comp_a)
plot(acomp(myalpha), pch = 16, col = "red", add = T)
aux <- seq(from=0, to=1, by=0.01)
myx <- expand.grid(x=aux,y=aux)
c60 <- cos(pi/3)
s60 <- sin(pi/3)
myfun <- function(x){
  y <- c(x[1]-x[2]*c60/s60, x[2]/s60)
  y <- c(1-sum(y),y)
  dd <- ifelse(any(y < 0), NA, dDirichlet(y, alpha = myalpha))
  return(dd)
}
dx <- apply(myx, 1, myfun)
dim(dx) <- c(101, 101)
contour(dx, asp = 1, levels = quantile(
  dDirichlet(rDirichlet.acomp(1000, myalpha), myalpha), 
  c(0.5, 1:9, 9.5)/10, na.rm = T),
        add = TRUE, col = "grey")
par(opar)


## ---- data automobile, message = F------------------------------------------------------------
library(codareg)
data(BDDSegX)
Y_s <- BDDSegX[, c("S_A", "S_B", "S_C", "S_D", "S_E")]
colnames(Y_s) <- c("A", "B", "C", "D", "E")
Y_s_zoo <- zoo::zoo(as(Y_s, "matrix"), BDDSegX[, "Date"])
Y_s <- acomp(Y_s)


## ---- time series des shares, fig.width = 6, fig.height=4, fig.cap = "Série des parts de marchés automobile des 5 marques de constructeurs étudiées",fig.align="center"----
D_market <- ncol(Y_s)
hues <- seq(15, 375, length = D_market + 1)
my_col <- hcl(h = hues, l = 65, c = 100)[1:D_market]
par(las = 1, mar = c(3, 4, 1, 1))
plot(Y_s_zoo, screens = 1, col = my_col, lwd = 2,
     ylab = "Market share automobile", xlab = "Time", ylim = c(0, 0.5))
legend("topleft", legend = c("A", "B", "C", "D", "E"), lty = 1, col = my_col,
       horiz = T, cex = 0.75, lwd = 2)



## ---- matrice diagramme ternaire, fig.width = 5, fig.height = 5, fig.cap = "Matrice de diagramme ternaires, avec la troisième composante égale à la moyenne géométrique des autres composantes"----
plot(Y_s, margin = "acomp", pch = 3, cex = 0.6)


## ---- matrice de variation--------------------------------------------------------------------
var_T <- variation(Y_s)


## ---- detail matrice variation----------------------------------------------------------------
var(log(Y_s[, 1] / Y_s[, 2]))


## ---- boxplot des log ratio, fig.width = 5, fig.height = 5, fig.cap = "Matrice des boîtes à moustaches des log-ratio par paires"----
boxplot(Y_s)


## ---- variance totale-------------------------------------------------------------------------
VarTot <- sum(var_T * upper.tri(var_T)) / D_market ^ 2


## ---- contributions des clr-------------------------------------------------------------------
var_j <- diag(var(clr(Y_s)))
var_j / sum(var_j)


## ---- CAH sur les variables, fig.align='center', fig.width = 5, fig.height = 4, fig.cap="Dendrograme associé à la CAH réalisée sur la matrice de variation des données de constructeurs automobiles.", fig.align="center"----
dd <- as.dist(var_T)
hc <- hclust(dd, method = "ward.D")
plot(hc, xlab = "Méthode de Ward", sub = "")


## ---- faire des balances----------------------------------------------------------------------
head(balance(X = Y_s, expr = ~(D/E)/(A/(B/C))))


## ---- matrice de signe------------------------------------------------------------------------
sign_binary <- matrix(c(-1, -1, -1, 1, 1,
         0, 0, 0, 1, -1,
         1, -1, -1, 0, 0,
         0, 1, -1, 0, 0), byrow = T, ncol = 5)


## ---- contraste basee sur la matrice de signe-------------------------------------------------
V_binary <- sign_binary
for (j in 1:nrow(V_binary)) {
  card_J_plus <- length(which(sign_binary[j, ] == 1)) 
  card_J_moins <- length(which(sign_binary[j, ] == -1)) 
  alpha <- sqrt(card_J_plus * card_J_moins / (card_J_plus + card_J_moins))
  row_j <- V_binary[j, ]
  row_j[row_j == 1] <- 1 / card_J_plus
  row_j[row_j == -1] <- - 1 / card_J_moins
  V_binary[j, ] <- alpha * row_j                                
}


## ---- tranformation ilr manuel----------------------------------------------------------------
V_binary %*% log(as.numeric(Y_s[1, ]))


## ---- matrice de distance entre individus-----------------------------------------------------
dist_market <- dist(Y_s)


## ---- plot des MDS, fig.width = 6, fig.height = 5, fig.cap = "MDS des parts de marché automobiles", fig.align="center"----
mds <- cmdscale(dist_market)
x <- mds[, 1]
y <- mds[, 2]
plot(x, y, pch = 16, cex = 0.7, xlab = "", ylab = "", main = "",
     xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
abline(h = 0, v = 0, lty = 2)
clr_market <- clr(Y_s)
for(k in 1:5) {
  coeff_clr <- coefficients(lm(clr_market[, k] ~ x + y -1))
  points(coeff_clr[1], coeff_clr[2], col = "red", pch = 15)
  text(coeff_clr[1], coeff_clr[2], LETTERS[k], col = "red", pos = 3)
}


## ---- biplot sur ACP, fig.width = 10, fig.height = 5, fig.cap = "Biplot obtenu après une ACP réalisée sur les coordonnées CLR", fig.align="center"----
pca <- princomp(Y_s)
par(mfrow = c(1, 2), las = 1)
screeplot(pca)
biplot(pca, cex = 0.5)
abline(h = 0, v = 0, lty = 2)


## ---- afficher axe ACP dans ternary diagram, eval = F-----------------------------------------
## axe_1 <- clrInv(loadings(pca)[, 1])
## plot(acomp(Y_s), pch = 16, cex = 0.5) # , margin = "acomp")
## straight(mean(acomp(Y_s)), axe_1)


## ---- fig.cap = "Représentation de l'axe 1 de l'ACP dans le simplexe", fig.align="center", echo = F----
if (knitr::is_html_output()) {
  knitr::include_graphics("figures/pca_simplex.jpg")
} else {
  knitr::include_graphics("figures/pca_simplex.pdf")
}


## ---- LRA du chapitre 4, fig.width = 10, fig.height = 5, fig.cap = "Biplots non-pondéré et pondéré du tableau de données archéologique", fig.align="center"----
data(cups)
par(mfrow = c(1, 2))
PLOT.LRA(LRA(cups, weight = FALSE), main = "LRA non pondéré")
PLOT.LRA(LRA(cups, weight = TRUE), main = "LRA pondéré")


## ---- k-means---------------------------------------------------------------------------------
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()


## ---- representation des classes, fig.width = 9, fig.height=4, fig.cap = "Représentation des trois groupes détectés dans le graphique des séries temporelles (à gauche) et dans le diagramme ternaire centré",fig.align="center"----
par(las = 1, mar = c(3, 4, 1, 1), mfrow = c(1, 2))
plot(Y_s_zoo, screens = 1, col = my_col, lwd = 2,
     ylab = "Market share automobile", xlab = "Time", ylim = c(0, 0.5))
legend("topleft", legend = c("A", "B", "C", "D", "E"), lty = 1, col = my_col,
       horiz = T, cex = 0.75, lwd = 2)
abline(v = BDDSegX[c(60, 99), "Date"], lty = 2, col = "red")
my_col_2 <- hcl(h = hues, l = 65, c = 100)[1:3]
lines(BDDSegX[c(1, 60), "Date"], c(0, 0), lwd = 4, col = my_col_2[3])
lines(BDDSegX[c(61, 99), "Date"], c(0, 0), lwd = 4, col = my_col_2[2])
lines(BDDSegX[c(100, nrow(BDDSegX)), "Date"], c(0, 0), lwd = 4, col = my_col_2[1])

plot(Y_s[, c(1, 3, 5)], center = T, col = my_col_2[clust])
lines(Y_s[, c(1, 3, 5)])


## ---- ICS detection, fig.width = 10, fig.height = 5, fig.cap = "Détection des valeurs atypiques des élections présidentielles en utilisant la méthode ICS", fig.align="center", message = F, warning = F----
library(ICSOutlier)
my_ics <- ics2(ilr_a)
icsOutlier <- ics.outlier(my_ics, level.dist = 0.05, mDist = 50, ncores = 1)
op <- par(oma = c(1, 1, 1.4, 1), mar = c(3.3, 3.3, 1, 0.7),
          las = 1, mgp = c(2.25, 1, 0), mfrow = c(1, 2))
plot(icsOutlier@ics.distances, main = "",
     pch = ifelse(icsOutlier@ics.distances > icsOutlier@ics.dist.cutoff, 16, 3),
     xlab = "Observation Number", ylab = "ICS distances", cex.main = 2)
abline(h = icsOutlier@ics.dist.cutoff)
plot(comp_a, col = ifelse(icsOutlier@ics.distances > icsOutlier@ics.dist.cutoff,
                          "red", "grey"))


## ---- data chinoises--------------------------------------------------------------------------
data("CHNS11")
CHNS11 <- CHNS11 %>%
  rename(proteines = VC, lipides = VF, glucides = VP) 
X_compo <- acomp(CHNS11[, c("proteines", "lipides", "glucides")])


## ---- ternary diagram conditionnelle data chinoises, fig.width = 7, fig.height = 4, fig.cap = "Diagramme ternaire de la variable $X$ conditionnellement à la variable dépendante découpée en classe", fig.align="center"----
bk <- c(0, 18.5, 25, 30, 35, 100)
ind <- findInterval(CHNS11$BMI, bk, all.inside = TRUE)
CHNS11$class_Y <- factor(
  c("sous-poids", "normal", "surpoids", "obesite", "obesite_plus")[ind], 
  levels = c("sous-poids", "normal", "surpoids", "obesite", "obesite_plus"))
ggtern(data = CHNS11, mapping = aes(x = proteines, y = lipides, z = glucides)) +
  geom_point(mapping = aes(colour = class_Y)) +
  facet_wrap(~ class_Y)


## ---- regression X compo----------------------------------------------------------------------
ilr_nutrition <- ilr(X_compo)
ilr1 <- ilr_nutrition[, 1]
ilr2 <- ilr_nutrition[, 2]
lm_1 <- lm(BMI ~ ilr1 + ilr2, data = CHNS11)
summary(lm_1)


## ---- coeff regression dans CLR---------------------------------------------------------------
ilr2clr(coef(lm_1)[-1])


## ---- prediction quand X compo, fig.width = 8, fig.height = 4, fig.cap = "Représentation des prédictions où $X$ est d'abord représenté dans l'espace ILR, puis dans l'espace du simplexe", fig.align="center"----
pal1 <- RColorBrewer::brewer.pal(9, "YlGn")
seq_ilr <-  expand.grid(ilr1 = seq(-3, 3, length.out = 100),
                        ilr2 = seq(-3, 3, length.out = 100))
Y_pred <- predict(lm_1, newdata = 
    data.frame(ilr1 = seq_ilr[, 1], ilr2 = seq_ilr[, 2]))
bk <- seq(min(Y_pred), max(Y_pred), length.out = 9)
ind <- findInterval(Y_pred, bk, all.inside = TRUE)
op <- par(mfrow = c(1, 2))
# ilr 
plot(seq_ilr[, 1], seq_ilr[, 2], col = pal1[ind],  
     pch = 16, cex = 0.5, xlab = "ilr 1", ylab = "ilr 2")
# simplex
seq_simp <- ilrInv(seq_ilr)
names(seq_simp) <- c("proteines", "lipides", "glucides")
plot(seq_simp, col = pal1[ind],  pch = 16)

decoup <- c("<=-23.03", "]23.03;23.49]", "]23.49;23.95]", 
            "]23.95;24.40]", "]24.40;24.86]", "]24.86;25.32]", 
            "]25.32;25.78]", ">25.78")
legend("topleft", legend = decoup, cex = 0.6, title = "Y pred", 
       fill = pal1)



## ---- regression Y compo----------------------------------------------------------------------
lm_2 <- lm(ilr(Y_s) ~ PIB_Courant_t + TTC_Gazole, data = BDDSegX)


## ---- coeff regression dans CLR avec Y compo--------------------------------------------------
my_coeff <- data.frame(
  segments = c("A", "B", "C", "D", "E"),
  as(t(ilr2clr(coef(lm_2)[-1, ])), "matrix")
)


## ---- barplot des coeff regression, fig.width = 8, fig.height=4, fig.cap = "Représentation des coefficients dans l'espace CLR", fig.align="center"----
coeff_df <- pivot_longer(my_coeff, cols = 2:3, names_to = "variable",
             values_to = "estimate")
ggplot(coeff_df, aes(x = segments, y = estimate)) +
  geom_bar(stat = "identity") +
  facet_wrap(~variable, ncol = 5, scales = "free_y")


## ---- significativite des variables-----------------------------------------------------------
anova(lm_2)


## ---- prediction plot quand Y compo, fig.width = 10, fig.height = 4, fig.cap = "Représentation des prédictions des parts de marché en fonction de variables quantitatives", fig.align="center"----
# prediction quand PIB_Courant_t varie
seq_PIB_Courant_t <- seq(min(BDDSegX$PIB_Courant_t), max(BDDSegX$PIB_Courant_t),
                      length.out = 100) 
pred_Y <- as(ilrInv(predict(lm_2, newdata = data.frame(
  PIB_Courant_t = seq_PIB_Courant_t,
  TTC_Gazole = median(BDDSegX$TTC_Gazole)
))), "matrix")
colnames(pred_Y) <- c("A", "B", "C", "D", "E")
data_pred_1 <- data.frame(pred_Y, x = seq_PIB_Courant_t)
# prediction quand gazole varie
seq_TTC_Gazole <- seq(min(BDDSegX$TTC_Gazole), max(BDDSegX$TTC_Gazole), 
                      length.out = 100) 
pred_Y <- as(ilrInv(predict(lm_2, newdata = data.frame(
  PIB_Courant_t = median(BDDSegX$PIB_Courant_t),
  TTC_Gazole = seq_TTC_Gazole 
))), "matrix")
colnames(pred_Y) <- c("A", "B", "C", "D", "E")
data_pred_2 <- data.frame(pred_Y, x = seq_TTC_Gazole)
# Merge des deux predictions 
pred_lg <- rbind(
  data.frame(variable = "PIB",
     pivot_longer(data_pred_1, cols = 1:5, names_to = "segments", 
                  values_to = "estimate")),
  data.frame(variable = "Gazole",
     pivot_longer(data_pred_2, cols = 1:5, names_to = "segments", 
                  values_to = "estimate"))
)
ggplot(pred_lg, aes(x = x, y = estimate, colour = segments)) +
  geom_line() +
  facet_wrap(~variable, ncol = 5, scales = "free")

