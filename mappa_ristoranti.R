#!/usr/bin/env Rscript

local_r_lib <- function(path = ".r_libs") {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  .libPaths(c(normalizePath(path), .libPaths()))
  normalizePath(path)
}

install_one <- function(pkg, lib) {
  is_darwin <- identical(Sys.info()[["sysname"]], "Darwin")
  # Su macOS spesso è molto più semplice installare i binari (evita toolchain + libs di sistema).
  if (is_darwin) {
    try(install.packages(pkg, repos = "https://cloud.r-project.org", lib = lib, type = "binary"), silent = TRUE)
  }
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", lib = lib, type = "source")
  }
}

ensure_pkgs <- function(pkgs, lib = local_r_lib()) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("Installazione pacchetti mancanti: ", paste(missing, collapse = ", "))
    for (p in missing) install_one(p, lib = lib)
  }
}

ensure_pkgs(c("sf", "dplyr", "readr", "ggplot2", "stringr", "viridis"))

suppressPackageStartupMessages({
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Il pacchetto 'sf' non è installabile automaticamente in questo ambiente.\n\n",
      "Su macOS, prova prima ad installare i binari:\n",
      "  install.packages('sf', type='binary')\n\n",
      "Se R prova a compilare dai sorgenti, servono dipendenze di sistema (Homebrew):\n",
      "  brew install pkg-config gdal geos proj udunits openssl@3 gcc\n\n",
      "Poi riprova a lanciare lo script."
    )
  }
  library(sf)
  sf::sf_use_s2(FALSE)
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(stringr)
  library(viridis)
})

# ---- Config ----
csv_url <- "https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/data_on_french_states.csv"
geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson"

data_dir <- "data"
out_dir <- "output"
csv_path <- file.path(data_dir, "ristoranti_francia.csv")
geojson_path <- file.path(data_dir, "communes_francia.geojson")
out_path <- file.path(out_dir, "densita_ristoranti_sud_francia.png")

dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

download_if_needed <- function(url, dest) {
  if (!file.exists(dest)) {
    message("Download: ", url)
    utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
  } else {
    message("Già presente: ", dest)
  }
}

# ---- 1) Scarica CSV ristoranti ----
download_if_needed(csv_url, csv_path)

# ---- 2) Scarica GeoJSON confini (communes) ----
download_if_needed(geojson_url, geojson_path)

# ---- Load data ----
# Il CSV usa il separatore ';' e descrive il numero di "equipments" per zona IRIS.
# Per ottenere un valore per comune, estraiamo le prime 5 cifre di `dciris`
# (codice comune INSEE) e sommiamo `nb_equip`.
rist <- readr::read_delim(
  csv_path,
  delim = ";",
  quote = "\"",
  show_col_types = FALSE,
  col_types = cols(
    reg = col_character(),
    dep = col_character(),
    depcom = col_character(),
    dciris = col_character(),
    an = col_character(),
    typequ = col_character(),
    nb_equip = col_double()
  )
)

communes <- suppressWarnings(sf::st_read(geojson_path, quiet = TRUE))

# ---- 3) Mappa densità ristoranti nel Sud della Francia ----
# Definizione "Sud": bounding box (approssimazione) nella CRS WGS84.
# - long: da -5.5 a 8.8 (Sud-Ovest -> Costa Azzurra)
# - lat:  da 42.0 a 46.3 (Pirenei/Med -> poco sotto Lione)
south_bbox_wgs84 <- sf::st_bbox(
  c(xmin = -5.5, ymin = 42.0, xmax = 8.8, ymax = 46.3),
  crs = sf::st_crs(4326)
)

communes_wgs84 <- sf::st_transform(communes, 4326)
communes_sud <- sf::st_crop(communes_wgs84, south_bbox_wgs84)

# Nel dataset, il codice "A504" identifica la categoria usata nell'esempio originale.
# Qui lo usiamo come proxy per "ristoranti" (se vuoi un altro codice, cambia questo valore).
rist_codice_ristoranti <- "A504"

rist_by_commune <- rist %>%
  filter(typequ == rist_codice_ristoranti) %>%
  mutate(code_commune = str_sub(dciris, 1, 5)) %>%
  group_by(code_commune) %>%
  summarise(ristoranti = sum(nb_equip, na.rm = TRUE), .groups = "drop")

# Calcolo densità per km^2 (serve una CRS metrica; 2154 = Lambert-93),
# ma per plottare usiamo lon/lat (4326) così i limiti del bbox sono coerenti.
communes_sud_m <- sf::st_transform(communes_sud, 2154) %>%
  mutate(area_km2 = as.numeric(sf::st_area(geometry)) / 1e6)

communes_sud_joined <- communes_sud_m %>%
  left_join(rist_by_commune, by = c("code" = "code_commune")) %>%
  mutate(
    ristoranti = if_else(is.na(ristoranti), 0, ristoranti),
    densita = if_else(area_km2 > 0, ristoranti / area_km2, NA_real_)
  ) %>%
  sf::st_transform(4326)

p <- ggplot() +
  geom_sf(data = communes_sud_joined, aes(fill = densita), color = NA) +
  geom_sf(data = communes_sud_joined, fill = NA, color = "grey80", linewidth = 0.08) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    trans = "sqrt",
    guide = guide_colorbar(title = "Ristoranti / km²")
  ) +
  coord_sf(
    xlim = c(south_bbox_wgs84["xmin"], south_bbox_wgs84["xmax"]),
    ylim = c(south_bbox_wgs84["ymin"], south_bbox_wgs84["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Densità dei ristoranti nel Sud della Francia",
    subtitle = paste0("Choropleth per comune (typequ = ", rist_codice_ristoranti, ") - Sud Francia"),
    x = NULL,
    y = NULL,
    caption = paste0(
      "CSV: ", csv_url, "\n",
      "GeoJSON: ", geojson_url
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey92", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

ggsave(out_path, p, width = 10, height = 8, dpi = 200)
message("Mappa salvata in: ", out_path)

