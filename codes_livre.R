### Codes R du chapitre 13 Implementation avec R
### Ecrit par Thibault Laurent

############################################
# Section 13.1
############################################

install.packages(c(
   "compositions", "missForest", "energy", "sf", "tidyverse", 
   "easyCODA", "zCompositions", "RColorBrewer", "mapsf"
  ))
devtools::install_github("tibo31/codareg")

library(readxl)

my_url <- "https://github.com/tibo31/coda_jes"
elec_data <- paste0(my_url, "/raw/main/data/res_2022.xlsx")
download.file(elec_data, destfile = "res_2022.xlsx", 
               quiet = TRUE, mode = "wb")

res_2022 <- read_excel("res_2022.xlsx")
names(res_2022) <- gsub(" ", "_", names(res_2022))
names(res_2022) <- gsub("-", "_", names(res_2022))
class(res_2022)

str(res_2022)

noms <- c("Macron", "Le_Pen", "Mélenchon", "Zemmour", 
 "Pécresse", "Jadot", "Lassalle", "Roussel", "Dupont_Aignan", 
 "Hidalgo", "Poutou", "Arthaud")
vote_share_1 <- res_2022[ , paste0(noms, "_EXP")]

colnames(vote_share_1) <- noms

res_2022$non_exprimes <- res_2022$Abstentions + 
  res_2022$Blancs + res_2022$Nuls

vote_share_2 <- res_2022[ , c(paste0(noms, "_VOIX"), 
                              "non_exprimes")]
vote_share_2 <- sapply(vote_share_2, function(x) 
  x / res_2022$Inscrits)


colnames(vote_share_2) <- c(noms, "non_inscrits")


############################################
# Section 13.2
############################################

library(tidyverse)
vote_share_long <- pivot_longer(
  data.frame(vote_share_1, dep = res_2022$DEP_NOM), 
  cols = 1:12, names_to = "candidat", values_to = "share")

noms_tries <- c("Zemmour", "Le_Pen", "Dupont_Aignan", 
  "Pécresse", "Lassalle", "Macron", "Hidalgo", "Jadot", 
  "Arthaud", "Mélenchon", "Roussel", "Poutou")
vote_share_long$candidat <- factor(vote_share_long$candidat,
  levels = noms_tries)

# Figure 1
vote_share_long %>%
  mutate(dep = factor(dep, levels = 
              levels((vote_share_long %>%
                filter(candidat == "Zemmour") %>%
                mutate(dep = fct_reorder(dep, share)))$dep))) %>%
  ggplot() +
  geom_col(aes(x = dep, fill = candidat, y = share)) +
  theme(axis.text.x = element_text(angle = 65, size = 10,
                                   vjust = 1, hjust = 1),
        legend.position = "top") +
  xlab("Départements") + ylab("Parts de votes") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))  + 
  scale_fill_grey()

library(sf)
load("geo_dep.RData")
geo_dep <- merge(geo_dep[, c("nom_dpt", "code_dpt", "nom_reg")], 
     data.frame(vote_share_1, dep = res_2022$DEP_CODE, 
                Inscrits = res_2022$Inscrits), 
     by.x = "code_dpt", by.y = "dep")
vote_share_long <- pivot_longer(geo_dep, cols = 4:15, 
    names_to = "candidat", values_to = "share")
vote_share_long$candidat <- factor(vote_share_long$candidat,
  levels = noms_tries)

# figure 2
vote_share_long %>%
 mutate(nom_reg = gsub("-", "-\n", nom_reg)) %>%
 mutate(nom_reg = gsub(" ", " \n", nom_reg)) %>%
 ggplot() +
  geom_col(aes(x = nom_dpt, fill = candidat, y = share)) +
  theme(axis.text.x = element_text(angle = 90)) +
    xlab("Départements") + ylab("Parts de votes") +
  facet_grid(. ~ nom_reg, scales = "free_x", space = "free") + 
  scale_fill_grey()


time_chart <- data.frame(
 year = rep(as.Date(paste0(c("1958", "1965", "1969", "1974", 
      "1981", "1988", "1995", "2002", "2007", "2012", "2017", 
      "2022"), "-01-01")), each = 4),
 vote = c(78.51, 21.49, 0, 0, 44.65, 31.72, 17.28, 6.35, 44.47, 
  32.22, 23.31, 0, 35.77, 47.96, 15.11, 1.16, 20.99, 50.70, 
  28.31, 0, 19.95, 49.11, 16.55, 14.39, 20.84, 40.84, 18.58, 
  19.74, 19.88, 42.87, 12.63, 24.62, 31.18, 36.10, 18.57, 14.15, 
  27.18, 44.00, 9.13, 19.69, 20.01, 27.85, 25.22, 26.92, 4.78, 
  31.92, 30.98, 32.32),
 parti = rep(c("droite", "gauche", "centre", "extrême"), 12)
)

# figure 3
ggplot(time_chart) + 
  aes(x = year, y = vote, fill = parti) +
  geom_area(color = "black") +
  labs(title = "Votes au 1er tour de l'élection présidentielle",
       subtitle = "1958 à 2022", x = "Année",
       y = "Parts de votes", fill = "Partis")  + 
  scale_fill_grey() +
  theme_minimal()


geo_dep$vainqueur <- noms_tries[
  apply(st_drop_geometry(geo_dep[, noms_tries]), 1, which.max)]

# figure 4
geo_dep$vainqueur <- noms_tries[
  apply(st_drop_geometry(geo_dep[, noms_tries]), 1, which.max)]
ggplot(data = geo_dep, aes(fill = vainqueur)) +
  geom_sf() +
  theme_void() +
  scale_fill_grey()


t(sapply(vote_share_1, function(x) 
   c(min = min(x), q = quantile(x, 0.25), median = median(x), 
     mean = mean(x), q = quantile(x, 0.75), max = max(x))))

# figure 5
par(las = 1, mar = c(4, 6.6, 0, 0))
boxplot(share ~ candidat, data = vote_share_long, 
        horizontal = T, xlab = "Parts de vote", ylab = "")

# figure 6
plotdata <- vote_share_long %>%
  st_drop_geometry() %>%
  select(candidat, share)  %>%
  group_by(candidat) %>%
  summarize(mean = mean(share),
            ci_1 = quantile(share, 0.05),
            ci_2 = quantile(share, 0.95))
ggplot(plotdata, aes(x = candidat,
                     y = mean)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = ci_1, 
                    ymax = ci_2), 
                width = .1) +
  xlab("Candidat") + ylab("Moyenne des parts de votes et intervalle de confiance") 

# figure 7
library("mapsf")
candidat <- c("Macron", "Le_Pen", "Mélenchon")
ma_palette <- c("Grays", "Grays", "Grays")
i <- 1
  mf_map(x = geo_dep, var = candidat[i], type = "choro",
       pal = ma_palette[i], 
       breaks = c(5, 15, 20, 22.5, 25, 27.5, 30, 45, 60),
       leg_title = candidat[i], 
       leg_val_rnd = 0, leg_pos = "right")
for (i in 2:3) {
  mf_map(x = geo_dep, var = candidat[i], type = "choro",
       pal = ma_palette[i], 
       breaks = c(5, 15, 20, 22.5, 25, 27.5, 30, 45, 60),
       leg_title = candidat[i], 
       leg_val_rnd = 0, leg_pos = "right")
}

# figure 8
par(mfrow = c(1, 3), oma = c(0, 0, 0, 0), 
    mar = c(4, 4, 1, 1))
plot(Macron_VOIX ~ Le_Pen_VOIX, data = res_2022, 
     main = "", xlab = "Le Pen", ylab = "Macron")
plot(Macron_EXP ~ Le_Pen_EXP, data = res_2022, 
     main = "", xlab = "Le Pen", ylab = "Macron")
plot(Macron ~ Le_Pen, data = vote_share_2, 
     main = "", xlab = "Le Pen", ylab = "Macron")

####################################################
######             Section 13-3
####################################################

library(zCompositions)
data(LPdataZM)

# figure 9
zPatterns(LPdataZM, label = NA)

library(missForest)
LPdataZM_nm <- missForest(LPdataZM)$ximp

# figure 10
 zPatterns(LPdataZM_nm, label = 0, show.means = TRUE)

LPdataZM_nm <- LPdataZM_nm[, !(names(LPdataZM_nm) %in% "Ni")]

dl <- apply(LPdataZM_nm, 2, function(x) min(x[x != 0]))
LPdataZM_multRepl <- multRepl(LPdataZM_nm, label = 0, dl = dl)
LPdataZM_multLN <- multLN(LPdataZM_nm, label = 0, dl = dl)
LPdataZM_lrEM <- lrEM(LPdataZM_nm, label = 0, dl = dl)


####################################################
######             Section 13-3
####################################################

# figure 11
library("ggtern")
p1 <- ggtern(data = vote_share_1, mapping = aes(x = Macron, 
                      y = Le_Pen, z = Mélenchon)) +
 geom_point(size = 1.5) 
p2 <- p1 +  theme_rgbw()
grid.arrange(p1, p2, ncol = 2)

# figure 12
p3 <- ggtern(data = geo_dep, mapping = aes(x = Macron, y = Le_Pen, z = Mélenchon)) +
 geom_point(aes(colour = vainqueur), size = 1.5)  + 
  scale_colour_grey() 
p4 <- ggtern(data = geo_dep, mapping = aes(x = Macron, y = Le_Pen, 
                                           z = Mélenchon)) +
 geom_point(aes(size = Inscrits))  
grid.arrange(p3, p4, ncol = 2)


# figure 13
ggtern(data = geo_dep, mapping = aes(x = Macron, 
       y = Le_Pen, z = Mélenchon)) +
 geom_point(mapping = aes(size = Inscrits, colour = vainqueur)) +
 scale_colour_grey() + 
 facet_wrap(~ nom_reg) + 
 labs( x = "EM", y = "JLM", z = "ML-P")


seq_simplex <- function(nb_noeud, zero = T) {
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
  res <- res[-nrow(res), ]
  if (!zero) {
    res <- res[!apply(res, 1, function(x) any(x == 0)), ]
  }
  return(res)
}

# Triangle de Maxwell
my_data <- as.data.frame(seq_simplex(100))
ggtern(data = my_data, mapping = aes(x = x, y = y, z = z)) +
  geom_point(size = 1.5, 
             col = rgb(my_data$x, my_data$y, my_data$z)) 


# figure 14
time_chart_wide <- pivot_wider(time_chart, 
          names_from = parti, values_from = vote)
ggtern(data = time_chart_wide,
       mapping = aes(x = droite, y = gauche, z = extrême)) +
  geom_point(size = 1.5) +
  geom_line()


library(compositions)
noms <- c("Macron", "Le_Pen", "Mélenchon")
comp_a <- acomp(res_2022[, paste0(noms, "_VOIX")])
names(comp_a) <- noms

comp_b <- acomp(res_2022[, paste0(noms, "_EXP")])
names(comp_b) <- noms

comp_a[1, ]
comp_b[1, ]


clo(c(1, 2, 3))
head(comp_a[, c("Macron", "Le_Pen")])
head(as(comp_a[, c("Macron", "Le_Pen")], "matrix"))


chimie <- data.frame(Cr = c(27.50, 30.40, 25.60), 
                     B = c(17, 23, 14), 
                     P = c(148, 433, 135),
                     V = c(29, 42, 33),
                     Cu = c(2.7, 3.8, 0),
                     Ti = c(4335, 3305, 3925),
                     Ni = c(0, 16.6, 14.2))
chimie_acomp <- acomp(chimie)
chimie_acomp

chimie_acomp_2 <- zeroreplace(chimie_acomp, 
                              d = rep(0.001, 7), a = 2/3)
aplus(chimie_acomp_2, c("Ti", "Ni"))
totals(aplus(chimie_acomp_2, c("Ti", "Ni")))
acomp(cbind(
  aplus(chimie_acomp_2, c("Cr", "B", "P", "V", "Cu")),
  Ni_Ti = totals(aplus(chimie_acomp_2, c("Ti", "Ni")))))

acompmargin(chimie_acomp_2, c("Cr", "B", "P", "V", "Cu"))

a <- comp_a[1, ] 
b <- comp_a[2, ]
d <- a + b
d

produit <- res_2022[1, paste0(noms, "_VOIX")] * 
  res_2022[2, paste0(noms, "_VOIX")]
d_bis <- produit / sum(produit)
d_bis

d * (1 / 2)


power <- d_bis ^ 0.5 
power / sum(power)

# figure 15
par(mfrow = c(1, 3), oma = c(0, 1, 0, 1), mar = c(0, 1, 0, 1))
b1 <- acomp(c(x1 = 0.2, x2 = 0.5, x3 = 0.3))
a1 <- acomp(c(x1 = 0.4, x2 = 0.1, x3 = 0.5))
plot(a1)
plot(b1, add = T)
plot((a1 + b1) * (1 / 2), add = T, pch = 15)
a2 <- acomp(c(x1 = 0.4, x2 = 0.01, x3 = 0.59))
plot(a2)
plot(b1, add = T)
plot((a2 + b1) * (1 / 2), add = T, pch = 15)
a3 <- acomp(c(x1 = 0.4, x2 = 0.001, x3 = 0.599))
plot(a3)
plot(b1, add = T)
plot((a3 + b1) * (1 / 2), add = T, pch = 15)

# figure 16
par(oma = c(0, 2, 0, 2), mar = c(0, 2, 0, 2))
plot(a)
plot(b, add = T)
plot(d * (1 / 2), add = T, pch = 15)
lines(d * seq(-100, 100, length.out = 1000), lty = 2)



farine <- data.frame(
  type = c("T110", "T150", "T45", "T55", "T65", "T80", "mais", 
           "pois chiche", "riz", "sarrasin", "seigle"),
    proteines = c(10.3, 12.2, 9.94, 9.9, 14.9, 10.9, 6.23, 
                  22.4, 8, 11.5, 8.7),
    glucides = c(70.2, 66.67, 76.31, 75.2, 69.1, 74.97, 78.74, 
                 57.90, 74.8, 68.43, 71.63),
    lipides = c(1.5, 1.52, 0.82, 1, 1, 1.18, 2.1, 6.69, 2.5, 
                2.19, 1.37))


# figure 17
farine_comp <- acomp(farine[, c("proteines", "glucides", "lipides")])
ggtern(data = farine, mapping = aes(x = proteines, y = lipides, z = glucides)) +
  geom_point(size = 1.5) +
  geom_point(data = data.frame(t(as(mean(farine_comp), "matrix"))), 
      mapping = aes(x = proteines, y = lipides, z = glucides), col = "darkgrey", size = 2, pch = 15)  +  
  theme(legend.position = c(0, 1), legend.justification = c(1, 1)) + 
  theme_gray()


farine_mean <- farine_comp[1, ]
for (k in 2:nrow(farine_comp)) {
  farine_mean <- farine_mean + farine_comp[k, ]
}
farine_mean <- farine_mean / nrow(farine_comp)

mean(farine_comp)

farine_comp_ce <- farine_comp - farine_mean

compo_to_ternary <- function(s_3, A = c(0, 0), B = c(1, 0), 
                             C = c(0.5, sqrt(3) / 2)) {
  if (length(s_3) == 3)
    s_3 <- t(matrix(s_3))
  Y_s_x <- s_3[, 1] * A[1] + s_3[, 2] * B[1] + s_3[, 3] * C[1]
  Y_s_y <- s_3[, 1] * A[2] + s_3[, 2] * B[2] + s_3[, 3] * C[2]
  return(cbind(Y_s_x, Y_s_y))
}
axis_c <- function(g_compo, side = 3, 
    values = c(0.01, 0.05, 0.1, 0.25), my_col = "grey") {
  my_index <- list(c(3, 2), c(3, 1), c(2, 1))
  my_srt <- c(120, -120, 0)
  for (k in values) {
    c_1 <- c_2 <- rep(0.000001, 3)  
    c_1[side] <- c_2[side] <- k
    c_1[my_index[[side]][1]] <- 1 - k  
    c_2[my_index[[side]][2]] <- 1 - k  
    lines(acomp(rbind(acomp(c_1) - g_compo, acomp(c_2) - g_compo)), 
      steps = 1, lty = 4, lwd = 0.7, col = my_col)
    my_c <- list(c_1, c_2, c_1)
    coord_leg <- compo_to_ternary(acomp(my_c[[side]]) - g_compo)
    text(coord_leg[, 1], coord_leg[, 2], k, cex = 0.7, pos = 4, 
       col = my_col, srt = my_srt[side])
 }
}


# figure 18
par(oma = c(0, 2, 0, 2), mar = c(0, 2, 0, 2))
plot(farine_comp_ce, pch = 16, cex = 0.5, labels = c("P", "G", "L"))
coord_leg <- compo_to_ternary(farine_comp_ce)
text(coord_leg[, 1], coord_leg[, 2], farine$type, cex = 0.7, pos = 3)
axis_c(farine_mean, values = c(0.01, 0.05, 0.1, 0.25),
           side = 3)
axis_c(farine_mean, values = c(0.75, 0.9, 0.95, 0.99),
           side = 2)
axis_c(farine_mean, values = c(0.05, 0.1, 0.25),
           side = 1)
g <- compo_to_ternary(farine_mean - farine_mean)
points(g[, 1], g[, 2], pch = 15, col = "grey2")

scalar(farine_comp[1, ], farine_comp[2, ])

D <- 3
ps <- 0
x <- farine_comp[1, ]
y <- farine_comp[2, ]
for (j in 1:(D - 1)) 
  for (i in (j + 1):D)
    ps <- ps + log(x[i] / x[j]) * log(y[i] / y[j])
(ps <- ps / 3)

norm(x)
norm(y)

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


dist(farine_comp[1:2, ])

sqrt(scalar(x-y, x-y))

V <- matrix(c(2 / sqrt(6), - 1 / sqrt(6), - 1 / sqrt(6),
            0, 1/ sqrt(2), - 1 / sqrt(2)), ncol = 2)
alr_a <- alr(comp_a, ivar = 3)
clr_a <- clr(comp_a)
ilr_a <- ilr(comp_a, V = V)


# figure 19
pal1 <- RColorBrewer::brewer.pal(5, "Greys")
seq_compo <- as.data.frame(seq_simplex(100, zero = F))
g_x <- (seq_compo[, 1] * seq_compo[, 2] * seq_compo[, 3]) ^ (1 / 3)
par(oma = c(0, 2, 0, 2), mar = c(0, 2, 0, 2))
plot(acomp(c(1/3, 1/3, 1/3)), pch = 16, cex = 1, 
     labels = c("x1", "x2", "x3"))
bk <- c(0, 0.1, 0.2, 0.25, 0.3, 1/3)
for(i in 1:nrow(seq_compo)) {
  ind <- findInterval(g_x[i], bk, all.inside = TRUE)
  plot(acomp(seq_compo[i, ]), col = pal1[ind], add = T, pch = 16)
}
plot(acomp(c(1/3, 1/3, 1/3)), pch = 15, cex = 0.75, col = "red", add = T)
decoup <- c("]0;0.067]", "]0.067;0.133]", 
            "]0.133;0.2]", "]0.2;0.267]", "]0.267;1/3]")
legend("topleft", legend = decoup, cex = 0.8, title = "g(x)", 
       fill = pal1)


all(round(apply(clr_a, 1, sum), 12) == 0)
(diag(3) - matrix(1/3, 3, 3)) %*% log(as.numeric(comp_a[1, ]))
clr_a[1, ]

ilr(x + y)
ilr(x) + ilr(y)

ilr(2 * x)
2 * ilr(x)

scalar(x, y)
sum(ilr(x) * ilr(y))

norm(x)
sqrt(sum(ilr(x) * ilr(x)))

norm(x - y)
dist(rbind(ilr(x), ilr(y)))

# figure 20
par(mfrow = c(1, 2), mar = c(4, 4, 1, 1), oma = c(0, 0, 0, 0))
plot(alr_a[, 1], alr_a[, 2], xlab = "alr 1", ylab = "alr 2", main = "ALR")
abline(h = 0, v = 0, lty = 2)
plot(ilr_a[, 1], ilr_a[, 2], xlab = "ilr 1", ylab = "ilr 2", main = "ILR")
abline(h = 0, lty = 2)

# figure 21
seq_compo <- as.data.frame(seq_simplex(100, zero = F))
pal1 <- RColorBrewer::brewer.pal(9, "Greys")
op <- par(mfrow = c(1, 2))
# ilr 1
plot(acomp(c(1/3, 1/3, 1/3)), pch = 16, cex = 0.5, 
     labels = c("x1", "x2", "x3"),
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
plot(acomp(c(1/3, 1/3, 1/3)), pch = 15, cex = 0.75, col = "grey3", add = T)
decoup <- c("<=-3", "]-3;-2]", "]-2;-1]", 
            "]-1;0]", "]0;1]", "]1;2]", "]2;3]", ">3")
legend("topleft", legend = decoup, cex = 0.6, title = "ilr 1", 
       fill = pal1)
# ilr 2
plot(acomp(c(1/3, 1/3, 1/3)), pch = 16, cex = 0.5, 
     labels = c("x1", "x2", "x3"),
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
plot(acomp(c(1/3, 1/3, 1/3)), pch = 15, cex = 0.75, col = "grey3", add = T)
decoup <- c("<=-3", "]-3;-2]", "]-2;-1]", 
            "]-1;0]", "]0;1]", "]1;2]", "]2;3]", ">3")
legend("topleft", legend = decoup, cex = 0.6, title = "ilr 2", 
       fill = pal1)
par(op)


# figure 22
par(mfrow = c(1, 3), mar = c(4, 4, 1, 1), oma = c(0, 0, 0, 0))
plot(clr_a[, 1], clr_a[, 2], xlab = "clr 1", ylab = "clr 2")
abline(h = 0, v = 0, lty = 2)
plot(clr_a[, 1], clr_a[, 3], xlab = "clr 1", ylab = "clr 3")
abline(h = 0, v = 0, lty = 2)
plot(clr_a[, 2], clr_a[, 3], xlab = "clr 2", ylab = "clr 3")
abline(h = 0, v = 0, lty = 2)


# figure 23
seq_compo <- NULL
pal1 <- RColorBrewer::brewer.pal(9, "Greys")
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
     labels = c("x1", "x2", "x3"),
     main = "Clr 1")
bk <- seq(-4, 4, length.out = 9)
bk[1] <- -100
bk[9] <- 100 
for(i in 1:nrow(seq_compo)) {
  ind <- findInterval(clr1[i], bk, all.inside = TRUE)
  plot(acomp(seq_compo[i, ]), col = pal1[ind], add = T, pch = 16)
}
plot(acomp(c(1/3, 1/3, 1/3)), pch = 15, cex = 0.75, col = "grey3", add = T)
decoup <- c("<=-3", "]-3;-2]", "]-2;-1]", 
            "]-1;0]", "]0;1]", "]1;2]", "]2;3]", ">3")
legend("topleft", legend = decoup, cex = 1, title = "clr 1", fill = pal1)
# clr 2
plot(acomp(c(1/3, 1/3, 1/3)), pch = 16, cex = 0.5, 
     labels = c("x1", "x2", "x3"),
     main = "Clr 2")
bk <- seq(-4, 4, length.out = 9)
bk[1] <- -100
bk[9] <- 100 
for(i in 1:nrow(seq_compo)) {
  ind <- findInterval(clr2[i], bk, all.inside = TRUE)
  plot(acomp(seq_compo[i, ]), col = pal1[ind], add = T, pch = 16)
}
plot(acomp(c(1/3, 1/3, 1/3)), pch = 15, cex = 0.75, col = "grey3", add = T)
decoup <- c("<=-3", "]-3;-2]", "]-2;-1]", 
            "]-1;0]", "]0;1]", "]1;2]", "]2;3]", ">3")
legend("topleft", legend = decoup, cex = 1, title = "clr 2", 
       fill = pal1)
# clr 3
plot(acomp(c(1/3, 1/3, 1/3)), pch = 16, cex = 0.5, 
     labels = c("x1", "x2", "x3"),
     main = "Clr 3")
bk <- seq(-4, 4, length.out = 9)
bk[1] <- -100
bk[9] <- 100 
for(i in 1:nrow(seq_compo)) {
  ind <- findInterval(clr3[i], bk, all.inside = TRUE)
  plot(acomp(seq_compo[i, ]), col = pal1[ind], add = T, pch = 16)
}
plot(acomp(c(1/3, 1/3, 1/3)), pch = 15, cex = 0.75, col = "grey3", add = T)
decoup <- c("<=-3", "]-3;-2]", "]-2;-1]", 
            "]-1;0]", "]0;1]", "]1;2]", "]2;3]", ">3")
legend("topleft", legend = decoup, cex = 1, title = "clr 3", 
       fill = pal1)
par(op)



# figure 24
my_pal <- rep(colorspace::sequential_hcl(5, "Grays")[1:4], each = 3)
x <- seq(-10, 10, length.out = 100)
my_droites_1 <- list(
  cbind(x, x), cbind(x, x + 1), cbind(x, x - 1),
  cbind(x, -x), cbind(x, -x + 1), cbind(x, -x - 1), 
  cbind(rep(0, 100), x), cbind(rep(-1, 100), x), cbind(rep(1, 100), x),
  cbind(x, rep(0, 100)), cbind(x, rep(-1, 100)), cbind(x, rep(1, 100)))
par(mfrow = c(2, 1), 
    oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1))
 for (i in c("i", "a")) {
   plot(0, 0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1),
     xlab = paste0(i, 'lr 1'), ylab = paste0(i, 'lr 2'))
  for (k in 1:12) {
    lines(my_droites_1[[k]][, 1], my_droites_1[[k]][, 2], 
        col = my_pal[k], lty = rep(1:4, each = 3)[k])
  }
}

par(mfrow = c(2, 1), 
    oma = c(1, 0, 0, 0), mar = c(1, 1, 0, 1))
  plot(acomp(ilrInv(c(0, 0))), type = "n", 
     main = "ILR -> simplexe", labels = c("x1", "x2", "x3"))
  for (k in 1:12) {
    lines(acomp(ilrInv(my_droites_1[[k]])), 
          col = my_pal[k], lty = rep(1:4, each = 3)[k])
  }
  plot(acomp(alrInv(c(0, 0))), type = "n", 
     main = "ALR -> simplexe", labels = c("x1", "x2", "x3"))
  for (k in 1:12) {
    lines(acomp(alrInv(my_droites_1[[k]])), 
          col = my_pal[k], lty = rep(1:4, each = 3)[k])
  }


# figure 25
my_pal <- colorspace::sequential_hcl(5, "Grays")[1:4]
radius <- list(c(0.5, 0.5), c(1, 1), c(0.5, 1), c(1, 2))
center_xy <- list(c(0, 0), c(1, 2), c(-1, 0))
par(mfrow = c(2, 1), 
    oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1))
 for (l in c("i", "a")) {
   plot(c(0, 1, -1), c(0, 2, 0), pch = 16, asp = 1, 
     xlab = paste0(l, "lr 1"), ylab = paste0(l, "lr 2"), 
     xlim = c(-2, 2), ylim = c(-1, 2))
   abline(h = 0, v = 0, lty = 2)
   theta <- seq(0, 2 * pi, length = 200)
  # draw the circle/ellipse
  for (i in 1:length(radius)) {
    for (j in 1:length(center_xy)) {
    lines(x = radius[[i]][1] * cos(theta) + center_xy[[j]][1], 
          y = radius[[i]][2] * sin(theta) + center_xy[[j]][2],
             col = my_pal[j], lty = j)
  }
}

}
par(mfrow = c(2, 1), 
    oma = c(1, 0, 0, 0), mar = c(1, 1, 0, 1))
  plot(acomp(ilrInv(cbind(c(0, 1, -1), c(0, 2, 0)))), pch = 16, 
     main = "ILR -> simplexe", labels = c("x1", "x2", "x3"))
for (i in 1:length(radius)) {
     for (j in 1:length(center_xy)) {
       lines(acomp(ilrInv(cbind(
         radius[[i]][1] * cos(theta) + center_xy[[j]][1],
         radius[[i]][2] * sin(theta) + center_xy[[j]][2]))),
             col = my_pal[j], lty = j)
     }
}
plot(acomp(alrInv(cbind(c(0, 1, -1), c(0, 1, 0)))), pch = 16, 
     main = "ALR -> simplexe", labels = c("x1", "x2", "x3"))
for (i in 1:length(radius)) {
     for (j in 1:length(center_xy)) {
       lines(acomp(alrInv(
         cbind(radius[[i]][1] * cos(theta) + center_xy[[j]][1],
               radius[[i]][2] * sin(theta) + center_xy[[j]][2]))),
             col = my_pal[j], lty = j)
     }
}




mean_alr <- apply(alr_a, 2, mean)
mean_ilr <- apply(ilr_a, 2, mean)
mean_clr <- apply(clr_a, 2, mean) 
alrInv(mean_alr)
ilrInv(mean_ilr, V = V)
clrInv(mean_clr)

var_s <- var(comp_a)
var_s

V %*% var(ilr_a) %*% t(V)


###################################################
### Lois dans le simplexe

my_multi <- t(rmultinom(1000, size = 30, 
                        prob = c(1/6, 1/3, 1/2)))

my_multi_ag <- aggregate(rep(1, nrow(my_multi)), by = list(
  x1 = my_multi[, 1], x2 = my_multi[, 2], x3 = my_multi[, 3]), 
  FUN = sum)


# figure 26
pal1 <- RColorBrewer::brewer.pal(9, "Greys")
bk <- seq(0, 45, by = 5)
ind <- findInterval(my_multi_ag$x, bk, all.inside = TRUE)

ggtern(data = my_multi_ag, mapping = aes(x = x1, y = x2, z = x3)) +
  geom_point(size = 1.5, col = pal1[ind])



my_mean_vec <- acomp(rbind(
  c(1/3, 1/3, 1/3), c(0.5, 0.3, 0.2), c(0.2, 0.2, 0.6)
))
my_var_list <- list(
  0.1 * matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), ncol = 3),
  0.1 * matrix(c(3, 0, 0, 0, 2, 0, 0, 0, 1), ncol = 3),
  0.1 * matrix(c(1, 0.8, 0.2, 0.8, 1, 0.5, 0.2, 0.5, 1), 
               ncol = 3)
)


# figure 27
library(latex2exp)
opar <- par(mar = c(1, 1, 1, 1), mfrow = c(3, 3))
for(k in 1:3) {
  for(j in 1:3) {
    my_mean <- my_mean_vec[k, ]
    my_var <- my_var_list[[j]]
    plot(my_mean, pch = 15, col = "darkgrey", main = "données simulées")
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




# figure 28
opar <- par(mar = c(3, 3, 1, 1), mfrow = c(1, 2))
xr <- rnorm.acomp(n = 107, mean = clrInv(mean_clr), var = var_s)
plot(xr, pch = 19, labels= c("EM", "JLM", "MLP"),
     col = "grey", main = "données simulées", cex = 0.5)
for(p in c(0.5, 1:9, 9.5)/10) {
  r <- sqrt(qchisq(p = p, df = 2))
  ellipses(clrInv(mean_clr), var_s, r, col="lightgrey")
}
plot(clrInv(mean_clr), add = TRUE, pch = 15, cex = 1, col="grey3")

plot(comp_a, pch = 19, col = "grey", cex = 0.5,
     main = "données observées", labels = c("EM", "JLM", "MLP"))
for(p in c(0.5, 1:9, 9.5)/10) {
  r <- sqrt(qchisq(p = p, df = 2))
  ellipses(clrInv(mean_clr), var_s, r, col="lightgrey")
}
plot(clrInv(mean_clr), add = T, pch = 15, 
     cex = 1, col="grey3")
par(opar)


# figure 29
qqnorm(comp_a)

energy::mvnorm.etest(ilr(comp_a), R = 199)


# figure 30
x <- seq(0, 1, 0.01)
my_comp <- acomp(cbind(x, 1 - x))
par(mfrow = c(1, 3))
plot(x, dDirichlet(my_comp, alpha = c(A = 0.3, B = 0.3)), type = "l", ylab = "f(x)",
     main = TeX(r"($\alpha_1=0.3, alpha_2=0.3$)"))
plot(x, dDirichlet(my_comp, alpha = c(A = 1, B = 1)), type = "l", ylab = "f(x)",
     main = TeX(r"($\alpha_1=1, alpha_2=1$)"))
plot(x, dDirichlet(my_comp, alpha = c(A = 2, B = 2)), type = "l", ylab = "f(x)",
     main = TeX(r"($\alpha_1=2, alpha_2=2$)"))


fit_d <- fitDirichlet(comp_a)


# figure 31
par(oma = c(0, 2, 0, 2), mar = c(0, 2, 0, 2))
myalpha = fit_d$alpha
plot(comp_a)
plot(acomp(myalpha), pch = 15, col = "grey", add = T)
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

Z <- mvrnorm(n = 100, mu = c(0, 0), 
        Sigma = matrix(c(2, -1.5, -1.5, 2), 2, 2))
u <- rchisq(1, df = 5)

Y <- sqrt(5 / u) * Z
X <- ilrInv(Y)


# figure 32
plot(X, labels = c("y1", "y2", "y3"))


###################################################
### Section 13.5
###################################################

library(codareg)
data(BDDSegX)
Y_s <- BDDSegX[, c("S_A", "S_B", "S_C", "S_D", "S_E")]
colnames(Y_s) <- c("A", "B", "C", "D", "E")
Y_s_zoo <- zoo::zoo(as(Y_s, "matrix"), BDDSegX[, "Date"])
Y_s <- acomp(Y_s)


# figure 33
D_market <- ncol(Y_s)
my_col <- RColorBrewer::brewer.pal(6, "Greys")[2:6]
par(las = 1, mar = c(3, 4, 1, 1))
plot(Y_s_zoo, screens = 1, col = my_col, lwd = 2,
     ylab = "Market share automobile", xlab = "Time", ylim = c(0, 0.5))
legend("topleft", legend = c("A", "B", "C", "D", "E"), lty = 1, col = my_col,
       horiz = T, cex = 0.75, lwd = 2)

# figure 34
plot(Y_s, margin = "acomp", pch = 3, cex = 0.6)

var_T <- variation(Y_s)

var(log(Y_s[, 1] / Y_s[, 2]))

# figure 35
boxplot(Y_s)

VarTot <- sum(var_T * upper.tri(var_T)) / D_market ^ 2
var_j <- diag(var(clr(Y_s)))
var_j / sum(var_j)


# figure 36
dd <- as.dist(var_T)
hc <- hclust(dd, method = "ward.D")
plot(hc, main = "", xlab = "Méthode de Ward", sub = "")

head(balance(X = Y_s, expr = ~(D/E)/(A/(B/C))))
sign_binary <- matrix(c(-1, -1, -1, 1, 1,
         0, 0, 0, 1, -1,
         1, -1, -1, 0, 0,
         0, 1, -1, 0, 0), byrow = T, ncol = 5)

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

V_binary %*% log(as.numeric(Y_s[1, ]))

clr_market <- clr(Y_s)
dist_market <- dist(clr_market)

# figure 37
mds <- cmdscale(dist_market)
x <- mds[, 1]
y <- mds[, 2]
plot(x, y, pch = 16, cex = 0.7, xlab = "", ylab = "", main = "",
     xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
abline(h = 0, v = 0, lty = 2)
for(k in 1:5) {
  coeff_clr <- coefficients(lm(clr_market[, k] ~ x + y -1))
  points(coeff_clr[1], coeff_clr[2], col = "grey", pch = 15)
  text(coeff_clr[1], coeff_clr[2], LETTERS[k], col = "grey", pos = 3)
}

# figure 38
pca <- princomp(Y_s)
par(mfrow = c(1, 2), las = 1, mar = c(4, 4, 1, 1),
    oma = c(0, 0, 0, 0))
screeplot(pca, main = "")
biplot(pca, cex = 0.5, xlab = "Composante 1",
       ylab = "Composante 2", col = c("grey", "grey3"))
abline(h = 0, v = 0, lty = 2)

# figure 39
axe_1 <- clrInv(loadings(pca)[, 1])
plot(acomp(Y_s), pch = 16, cex = 0.5) 
straight(mean(acomp(Y_s)), axe_1)

# figure 40
library(easyCODA)
data(cups)
par(mfrow = c(1, 2))
PLOT.LRA(LRA(cups, weight = FALSE), main = "LRA non pondéré")
PLOT.LRA(LRA(cups, weight = TRUE), main = "LRA pondéré")


# figure 41
library(ICSOutlier)
my_ics <- ics2(ilr_a)
icsOut <- ics.outlier(my_ics, level.dist = 0.05, 
                          mDist = 50, ncores = 1)
op <- par(oma = c(1, 1, 1.4, 1), mar = c(3.3, 3.3, 1, 0.7),
          las = 1, mgp = c(2.25, 1, 0), mfrow = c(1, 2))
plot(icsOut@ics.distances, main = "",
     pch = ifelse(icsOut@ics.distances > icsOut@ics.dist.cutoff, 16, 3),
     col = ifelse(icsOut@ics.distances > icsOut@ics.dist.cutoff,
                          "grey3", "grey"),
     xlab = "Observation Number", ylab = "ICS distances", cex.main = 2)
abline(h = icsOut@ics.dist.cutoff)
plot(comp_a, col = ifelse(icsOut@ics.distances > icsOut@ics.dist.cutoff,
                          "grey3", "grey"), 
  pch = ifelse(icsOut@ics.distances > icsOut@ics.dist.cutoff, 16, 3))


data("CHNS11")
CHNS11 <- CHNS11 %>%
  rename(proteines = VC, lipides = VF, glucides = VP) 
X_compo <- acomp(CHNS11[, c("proteines", "lipides", "glucides")])


# figure 42
bk <- c(0, 18.5, 25, 30, 35, 100)
ind <- findInterval(CHNS11$BMI, bk, all.inside = TRUE)
CHNS11$class_Y <- factor(
  c("sous-poids", "normal", "surpoids", "obesite", "obesite_plus")[ind], 
  levels = c("sous-poids", "normal", "surpoids", "obesite", "obesite_plus"))
ggtern(data = CHNS11, mapping = aes(x = proteines, y = lipides, z = glucides)) +
  geom_point(mapping = aes(colour = class_Y)) +
  facet_wrap(~ class_Y, ncol = 3) + 
  scale_colour_grey() + 
  labs( x = "P", y = "G", z = "L")


ilr_nutrition <- ilr(X_compo)
ilr1 <- ilr_nutrition[, 1]
ilr2 <- ilr_nutrition[, 2]
lm_1 <- lm(BMI ~ ilr1 + ilr2, data = CHNS11)
summary(lm_1)

ilr2clr(coef(lm_1)[-1])


# figure 43
pal1 <- RColorBrewer::brewer.pal(9, "Greys")
seq_ilr <-  expand.grid(ilr1 = seq(-3, 3, length.out = 100),
                        ilr2 = seq(-3, 3, length.out = 100))
Y_pred <- predict(lm_1, newdata = 
    data.frame(ilr1 = seq_ilr[, 1], ilr2 = seq_ilr[, 2]))
bk <- seq(min(Y_pred), max(Y_pred), length.out = 9)
ind <- findInterval(Y_pred, bk, all.inside = TRUE)
op <- par(oma = c(1, 1, 1.4, 1), mar = c(3.3, 3.3, 1, 0.7),
          las = 1, mgp = c(2.25, 1, 0), mfrow = c(1, 2))
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
legend("topleft", legend = decoup, cex = 0.7, title = "Y pred", 
       fill = pal1)


lm_2 <- lm(ilr(Y_s) ~ PIB_Courant_t + TTC_Gazole, 
           data = BDDSegX)

my_coeff <- data.frame(segments = c("A", "B", "C", "D", "E"),
  as(t(ilr2clr(coef(lm_2)[-1, ])), "matrix"))


# figure 44
coeff_df <- pivot_longer(my_coeff, cols = 2:3, names_to = "variable",
             values_to = "estimate")
ggplot(coeff_df, aes(x = segments, y = estimate)) +
  geom_bar(stat = "identity") +
  facet_wrap(~variable, ncol = 5, scales = "free_y")


anova(lm_2)

# figure 44
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
  facet_wrap(~variable, ncol = 5, scales = "free") + 
  scale_colour_grey()





