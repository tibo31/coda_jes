## Mapping of the circonscriptions in 2017

# import
geo_circ <- read_sf("https://www.data.gouv.fr/fr/datasets/r/efa8c2e6-b8f7-4594-ad01-10b46b06b56a")

# aggrégation par département
geo_dep <- aggregate(geo_circ[ , c("code_dpt")],
                     by = list(nom_dpt = geo_circ$nom_dpt,
                               nom_reg = geo_circ$nom_reg,
                               code_reg = geo_circ$code_reg), FUN = unique)

# on déplace les départements d'IdF
paris <- geo_dep[geo_dep$code_dpt %in% c("75", "92", "93", "94"),]
paris_sfc <- st_geometry(paris)
center_paris <- st_centroid(st_union(paris))
st_geometry(paris) <- (paris_sfc - st_geometry(center_paris)) * 1.5  + st_geometry(center_paris) + 
  st_geometry(st_point(c(7.5, 1)))
st_crs(paris) <- st_crs(geo_dep)
st_geometry(geo_dep[geo_dep$code_dpt %in% c("75", "92", "93", "94"),]) <- st_geometry(paris)


# on déplace les départements d'O-M
guada <- geo_dep[geo_dep$code_dpt %in% "ZA",]
guada_sfc <- st_geometry(guada)
centro_guada <- st_centroid(st_union(guada))
st_geometry(guada) <- (guada_sfc - st_geometry(centro_guada)) * 1.25 + st_geometry(st_point(c(-6.5, 51)))
st_crs(guada) <- st_crs(geo_dep)
st_geometry(geo_dep[geo_dep$code_dpt %in% "ZA",]) <- st_geometry(guada) 
geo_dep[geo_dep$code_dpt %in% "ZA", "code_dpt"] <- "971"
geo_dep[geo_dep$code_dpt %in% "971", "nom_reg"] <- "OUTRE-MER"


marti <- geo_dep[geo_dep$nom_reg %in% "MARTINIQUE",]
marti_sfc <- st_geometry(marti)
centro_marti <- st_centroid(st_union(marti))
st_geometry(marti) <- (marti_sfc - st_geometry(centro_marti)) * 1.25 + st_geometry(st_point(c(-6.5, 50)))
st_crs(marti) <- st_crs(geo_dep)
st_geometry(geo_dep[geo_dep$nom_reg %in% "MARTINIQUE",]) <- st_geometry(marti) 
geo_dep[geo_dep$nom_reg %in% "MARTINIQUE", "code_dpt"] <- "972"
geo_dep[geo_dep$code_dpt %in% "972", "nom_reg"] <- "OUTRE-MER"



guya <- geo_dep[geo_dep$nom_reg %in% "GUYANE",]
guya_sfc <- st_geometry(guya )
centro_guya <- st_centroid(st_union(guya))
st_geometry(guya) <- (guya_sfc - st_geometry(centro_guya)) * 0.25 + st_geometry(st_point(c(-6.5, 49)))
st_crs(guya) <- st_crs(geo_dep)
st_geometry(geo_dep[geo_dep$nom_reg %in% "GUYANE",]) <- st_geometry(guya) 
geo_dep[geo_dep$nom_reg %in% "GUYANE", "code_dpt"] <- "973"
geo_dep[geo_dep$code_dpt %in% "973", "nom_reg"] <- "OUTRE-MER"



reun <- geo_dep[geo_dep$nom_dpt %in% "LA REUNION",]
reun_sfc <- st_geometry(reun)
centro_reun <- st_centroid(st_union(reun))
st_geometry(reun) <- (reun_sfc - st_geometry(centro_reun)) * 1.25 + st_geometry(st_point(c(-6.5, 48)))
st_crs(reun) <- st_crs(geo_dep)
st_geometry(geo_dep[geo_dep$nom_dpt %in% "LA REUNION",]) <- st_geometry(reun) 
geo_dep[geo_dep$nom_dpt %in% "LA REUNION", "code_dpt"] <- "974"
geo_dep[geo_dep$code_dpt %in% "974", "nom_reg"] <- "OUTRE-MER"



stpi <- geo_dep[geo_dep$nom_dpt %in% "SAINT-PIERRE-ET-MIQUELON",]
stpi_sfc <- st_geometry(stpi)
centro_stpi <- st_centroid(st_union(stpi))
st_geometry(stpi) <- (stpi_sfc - st_geometry(centro_stpi)) * 1.4 + st_geometry(st_point(c(-6.5, 47)))
st_crs(stpi) <- st_crs(geo_dep)
st_geometry(geo_dep[geo_dep$nom_dpt %in% "SAINT-PIERRE-ET-MIQUELON",]) <- st_geometry(stpi) 
geo_dep[geo_dep$nom_dpt %in% "SAINT-PIERRE-ET-MIQUELON", "code_dpt"] <- "975"
geo_dep[geo_dep$code_dpt %in% "975", "nom_reg"] <- "OUTRE-MER"



mayo <- geo_dep[geo_dep$nom_dpt %in% "MAYOTTE",]
mayo_sfc <- st_geometry(mayo)
centro_mayo <- st_centroid(st_union(mayo))
st_geometry(mayo) <- (mayo_sfc - st_geometry(centro_mayo)) * 1.4 + st_geometry(st_point(c(-6.5, 46)))
st_crs(mayo) <- st_crs(geo_dep)
st_geometry(geo_dep[geo_dep$nom_dpt %in% "MAYOTTE",]) <- st_geometry(mayo) 
geo_dep[geo_dep$nom_dpt %in% "MAYOTTE", "code_dpt"] <- "976"
geo_dep[geo_dep$code_dpt %in% "976", "nom_reg"] <- "OUTRE-MER"



wall <- geo_dep[geo_dep$nom_dpt %in% "WALLIS-ET-FUTUNA",]
wall_sfc <- st_geometry(wall)
centro_wall <- st_centroid(st_union(wall))
st_geometry(wall) <- (wall_sfc  - st_geometry(centro_wall)) * 0.6 + st_geometry(st_point(c(-6.5, 45)))
st_crs(wall) <- st_crs(geo_dep)
st_geometry(geo_dep[geo_dep$nom_dpt %in% "WALLIS-ET-FUTUNA",]) <- st_geometry(wall) 
geo_dep[geo_dep$nom_dpt %in% "WALLIS-ET-FUTUNA", "code_dpt"] <- "986"
geo_dep[geo_dep$code_dpt %in% "986", "nom_reg"] <- "OUTRE-MER"



stma <- geo_dep[geo_dep$nom_dpt %in% "SAINT-MARTIN/SAINT-BARTHELEMY",]
stma_sfc <- st_geometry(stma)
centro_stma <- st_centroid(st_union(stma))
st_geometry(stma) <- (stma_sfc - st_geometry(centro_stma)) * 1.5 + st_geometry(st_point(c(-6.5, 44)))
st_crs(stma) <- st_crs(geo_dep)
st_geometry(geo_dep[geo_dep$nom_dpt %in% "SAINT-MARTIN/SAINT-BARTHELEMY",]) <- st_geometry(stma) 
geo_dep[geo_dep$nom_dpt %in% "SAINT-MARTIN/SAINT-BARTHELEMY", "code_dpt"] <- "977/978"
geo_dep[geo_dep$code_dpt %in% "977/978", "nom_reg"] <- "OUTRE-MER"


poly <- geo_dep[geo_dep$nom_dpt %in% "POLYNESIE-FRANCAISE",]
poly_sfc <- st_geometry(poly)
centro_poly <- st_centroid(st_union(poly))
st_geometry(poly) <- (poly_sfc  - st_geometry(centro_poly)) * 0.05 + st_geometry(st_point(c(-6.5, 43)))
st_crs(poly) <- st_crs(geo_dep)
st_geometry(geo_dep[geo_dep$nom_dpt %in% "POLYNESIE-FRANCAISE",]) <- st_geometry(poly) 
geo_dep[geo_dep$nom_dpt %in% "POLYNESIE-FRANCAISE", "code_dpt"] <- "987"
geo_dep[geo_dep$code_dpt %in% "987", "nom_reg"] <- "OUTRE-MER"


nllca <- geo_dep[geo_dep$nom_dpt %in% "NOUVELLE-CALEDONIE",]
nllca_sfc <- st_geometry(nllca)
centro_nllca <- st_centroid(st_union(nllca))
st_geometry(nllca) <- (nllca_sfc  - st_geometry(centro_nllca)) * 0.2 + st_geometry(st_point(c(-6.5, 42)))
st_crs(nllca) <- st_crs(geo_dep)
st_geometry(geo_dep[geo_dep$nom_dpt %in% "NOUVELLE-CALEDONIE",]) <- st_geometry(nllca) 
geo_dep[geo_dep$nom_dpt %in% "NOUVELLE-CALEDONIE", "code_dpt"] <- "988"
geo_dep[geo_dep$code_dpt %in% "988", "nom_reg"] <- "OUTRE-MER"


# on change les nouvelles régions
geo_dep[geo_dep$nom_reg %in% "AQUITAINE-LIMOUSIN-POITOU-CHARENTES", "nom_reg"] <- 
  "NOUVELLE-AQUITAINE"
geo_dep[geo_dep$nom_reg %in% "ALSACE-CHAMPAGNE-ARDENNE-LORRAINE", "nom_reg"] <- 
  "GRAND-EST"
geo_dep[geo_dep$nom_reg %in% "LANGUEDOC-ROUSSILLON-MIDI-PYRENEES", "nom_reg"] <- 
  "OCCITANIE"
geo_dep[geo_dep$nom_reg %in% "NORD-PAS-DE-CALAIS-PICARDIE", "nom_reg"] <- 
  "HAUTS-DE-FRANCE"

# français résidant à l'étranger
geo_dep <- rbind(
  geo_dep, 
  st_sf(code_dpt = "999", nom_dpt = "999",
        nom_reg = "-", code_reg = "-",
        geometry = st_buffer(st_sfc(st_point(c(-3, 42))), dist = 0.5),
        crs = st_crs(geo_dep)))


new_bb = c(-5.115104, 41.368038,  9.559823, 51.089397)
names(new_bb) = c("xmin", "ymin", "xmax", "ymax")
attr(new_bb, "class") = "bbox"
attr(st_geometry(geo_dep), "bbox") = new_bb

