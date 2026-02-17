###############################################################################
# Umbralización de Otsu (Otsu’s thresholding)
# Autor: Renzo Angel De La Cruz Gonzales
# Contacto: renzoangeldelacruz@gmail.com
# GitHub: https://github.com/delacruz-renzo
###############################################################################

# =========================
# 0) Paquetes
# =========================
pkgs <- c("sf", "terra", "ggplot2", "dplyr", "mapview", "RStoolbox", "scales")

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  stop(
    "Faltan paquetes: ", paste(missing, collapse = ", "),
    "\nInstálalos con:\n  install.packages(c(",
    paste(sprintf('"%s"', missing), collapse = ", "),
    "))"
  )
}

invisible(lapply(pkgs, library, character.only = TRUE))

if (requireNamespace("EBImage", quietly = TRUE)) {
  library(EBImage)
} else {
  message(
    "EBImage no está instalado (opcional). Para instalarlo:\n",
    "  install.packages('BiocManager')\n",
    "  BiocManager::install('EBImage')"
  )
}

# =========================
# 1) Importar Datos
# =========================

limHumedal <- read_sf("shp/aoi_humedal.shp")

mapview::mapview(limHumedal, layer.name = "Área de Estudio", alpha.regions = 0, color = "red", 
                 lwd = 2, map.types = c("Esri.WorldImagery"))

CBERS_4A_files <- sort(list.files(path = "raster/MS", pattern = "\\.tif$", full.names = TRUE))

CBERS_4A_MS <- rast(CBERS_4A_files)

names(CBERS_4A_MS) <- c("CBERS4A_BAND1", "CBERS4A_BAND2", "CBERS4A_BAND3", "CBERS4A_BAND4")

CBERS_4A_MS <- terra::crop(CBERS_4A_MS, limHumedal)

CBERS_4A_MS <- terra::mask(CBERS_4A_MS, limHumedal)

plot(CBERS_4A_MS)

CBERS_4A_PAN <- rast("raster/PAN/CBERS_4A_WPM_20230319_245_128_L4_BAND0.tif")

names(CBERS_4A_PAN) <- c("CBERS4A_BAND0")

CBERS_4A_PAN <- terra::crop(CBERS_4A_PAN, limHumedal)

CBERS_4A_PAN <- terra::mask(CBERS_4A_PAN, limHumedal)

plot(CBERS_4A_PAN, col = gray.colors(256, start = 0, end = 1))

# =========================
# 2) Preprocesamiento
# =========================

coef_rad <- c(CBERS4A_BAND0 = 0.184471, CBERS4A_BAND1 = 0.29107, CBERS4A_BAND2 = 0.297832,
              CBERS4A_BAND3 = 0.232504, CBERS4A_BAND4 = 0.178993)

bandas_MS <- CBERS_4A_MS * coef_rad[names(CBERS_4A_MS)]

banda_PAN <- CBERS_4A_PAN * coef_rad[names(CBERS_4A_PAN)]

plotRGB(bandas_MS, r = 3, g = 2, b = 1, axes = TRUE, box = TRUE, stretch = "lin")

plotRGB(bandas_MS, r = 4, g = 3, b = 2, axes = TRUE, box = TRUE, stretch = "lin")

imagen_fusion <- RStoolbox::panSharpen(bandas_MS, banda_PAN, r = 4, g = 3, b = 2, method = "brovey")

plotRGB(imagen_fusion, r = 3, g = 2, b = 1, axes = TRUE, box = TRUE, stretch = "lin")

# =========================
# 3) NDVI
# =========================

NIR <- imagen_fusion["CBERS4A_BAND4_pan"]

Red <- imagen_fusion["CBERS4A_BAND3_pan"]

NDVI <- (NIR - Red)/(NIR + Red)

plot(NDVI, main = "NDVI", col = colorRampPalette(c("red", "yellow", "green"))(255))

# =========================
# 4) Otsu’s thresholding (varianza interclase)
# =========================
NDVI_df <- as.data.frame(NDVI)

colnames(NDVI_df) <- "NDVI"

media_NDVI   <- mean(NDVI_df$NDVI, na.rm = TRUE)

mediana_NDVI <- median(NDVI_df$NDVI, na.rm = TRUE)

otsu_threshold <- function(x_rast, L = 256, range = NULL, eps = 1e-10) {
  if (!inherits(x_rast, "SpatRaster")) {x_rast <- terra::rast(x_rast)}
  if (length(L) != 1 || !is.finite(L)) stop("L debe ser un único valor numérico finito.")
  L <- as.integer(L)
  if (L < 2) stop("L debe ser un entero >= 2.")
  if (is.null(range)) {
    r_min <- terra::global(x_rast, "min", na.rm = TRUE)[1, 1]
    r_max <- terra::global(x_rast, "max", na.rm = TRUE)[1, 1]
  } else {
    stopifnot(length(range) == 2)
    r_min <- as.numeric(range[1])
    r_max <- as.numeric(range[2])
  }
  if (!is.finite(r_min) || !is.finite(r_max)) {
    stop("El rango contiene valores no finitos (NA/Inf).")
  }
  if (r_max <= r_min) {
    stop("Rango degenerado: r_max <= r_min. Otsu no es informativo.")
  }
  rast_nbits <- round((x_rast - r_min) / (r_max - r_min) * (L - 1))
  rast_nbits <- terra::clamp(rast_nbits, lower = 0, upper = L - 1, values = TRUE)
  vals_nbits <- terra::values(rast_nbits, mat = FALSE)
  vals_nbits <- vals_nbits[is.finite(vals_nbits)]
  if (length(vals_nbits) == 0) {
    stop("No hay píxeles válidos para calcular el histograma.")
  }
  h <- tabulate(vals_nbits + 1, nbins = L) 
  N <- sum(h)
  P <- h / N
  i <- 0:(L - 1)
  omega0 <- cumsum(P)
  omega1 <- 1 - omega0
  muT <- sum(i * P)
  mu0 <- cumsum(i * P) / pmax(omega0, eps)
  mu1 <- (muT - cumsum(i * P)) / pmax(omega1, eps)
  sigma_b2 <- omega0 * omega1 * (mu0 - mu1)^2
  sigma_b2[omega0 < eps | omega1 < eps] <- -Inf
  threshold_bits <- which.max(sigma_b2) - 1
  threshold_scaled <- r_min + (threshold_bits / (L - 1)) * (r_max - r_min)
  return(list(
    threshold_scaled = threshold_scaled,
    threshold_bits   = threshold_bits,
    sigma_b2         = sigma_b2,
    levels           = i,
    range_used       = c(r_min, r_max)
  ))
}

otsu_ndvi <- otsu_threshold(NDVI)

cat("Umbral óptimo de Otsu (t*) en niveles discretos:", otsu_ndvi$threshold_bits, "\n")

cat("Umbral óptimo de Otsu (t*) en escala original:", otsu_ndvi$threshold_scaled, "\n")

cat("Rango utilizado para cálcular el umbral óptimo de Otsu (t*):", otsu_ndvi$range_used, "\n")

threshold_otsu_NDVI <- otsu_ndvi$threshold_scaled

ggplot(NDVI_df, aes(x = NDVI)) +
  geom_histogram(aes(fill = after_stat(x)), color = "white", linewidth = 0.1, bins = 30) +
  scale_fill_gradient(low = "#e5f5e0", high = "#006d2c") +
  geom_vline(xintercept = media_NDVI, color = "red", linewidth = 0.7, linetype = "dashed") +
  geom_vline(xintercept = mediana_NDVI, color = "blue", linewidth = 0.7, linetype = "dashed") +
  geom_vline(xintercept = threshold_otsu_NDVI, color = "black", linewidth = 0.7, linetype = "dashed") +
  theme_grey(base_size = 9) +
  annotate("text", x = media_NDVI, y = 3.5e4, label = "Media", color = "red", angle = 90, vjust = -0.5, 
           size = 3.4) +
  annotate("text", x = mediana_NDVI, y = 3.5e4, label = "Mediana", color = "blue", angle = 90, vjust = -0.5, 
           size = 3.4) +
  annotate("text", x = threshold_otsu_NDVI, y = 3.5e4, label = "Otsu (t*)", color = "black", angle = 90, vjust = -0.5, 
           size = 3.4) +
  labs(title = "Histograma de valores NDVI", x = "NDVI", y = "Frecuencia") +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.2)) +
  scale_y_continuous(labels = scales::scientific) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 9.5), legend.position = "none")

mask_otsu <- NDVI > threshold_otsu_NDVI

w <- matrix(1, 5, 5)

mask_otsu <- focal(mask_otsu, w, fun = modal, na.rm = TRUE)

mapview::mapview(NDVI,
                 layer.name = "NDVI",
                 col.regions = c("red", "yellow", "green"),
                 map.types = "Esri.WorldImagery",
                 legend = FALSE) +
  mapview::mapview(mask_otsu,
                   layer.name = "Otsu",
                   col.regions = c("gray90", "darkgreen"),
                   legend = FALSE)

threshold_bits_NDVI <- otsu_ndvi$threshold_bits

NDVI_df <- NDVI_df %>%
  mutate(NDVI_bits = round((NDVI - min(NDVI)) / (max(NDVI) - min(NDVI)) * (256 - 1))) %>%
  mutate(class = ifelse(NDVI_bits <= threshold_bits_NDVI, "0", "1")) %>%
  count(NDVI_bits, class, name = "count")

scale_factor  <- as.numeric(max(NDVI_df$count, na.rm = TRUE) / max(otsu_ndvi$sigma_b2, na.rm = TRUE))

varInterclases_plot <- data.frame(
  level = otsu_ndvi$levels[is.finite(otsu_ndvi$sigma_b2)],
  sigma = otsu_ndvi$sigma_b2[is.finite(otsu_ndvi$sigma_b2)] * scale_factor)

point_otsu <- varInterclases_plot %>%
  slice_min(abs(level - threshold_bits_NDVI), n = 1, with_ties = FALSE) %>%
  select(level, sigma)

ggplot() +
  geom_col(data = NDVI_df, aes(x = NDVI_bits, y = count, fill = class), color = "NA", width = 1) +
  geom_line(data = varInterclases_plot, aes(x = level, y = sigma), color = "red", linewidth = 0.9, alpha = 0.7) +
  geom_vline(xintercept = threshold_bits_NDVI, color = "black", linewidth = 0.7, linetype = "dashed") +
  geom_point(data = point_otsu, aes(x = level, y = sigma), color = "red", size = 3) +
  theme_minimal(base_size = 9) +
  scale_fill_manual(values = c("0" = "gray65", "1" = "#006d2c")) +
  scale_x_continuous(breaks = seq(0, 255, by = 50)) +
  scale_y_continuous(name = "Conteo de Píxeles", labels = scales::scientific, breaks = seq(0, 1e4, by = 2.38e3),
                     sec.axis = sec_axis(~ . / scale_factor, name = "Varianza Interclase")) +
  annotate("text", x = threshold_bits_NDVI, y = 4.6e3, label = "Otsu (t*)", color = "grey10", angle = 90, 
           vjust = -0.5, size = 3.4) +
  labs(x = "Intensidad NDVI (bins 0–255)", 
       title = "Histograma de intensidades NDVI y varianza interclase") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 9.5),
        panel.grid.minor = element_blank(), legend.position = "none")

otsu_ndvi2 <- otsu_threshold(NDVI, range = c(-1, 1))

cat("Umbral óptimo de Otsu (t*) utilizando rango teórico, en niveles discretos:", 
    otsu_ndvi2$threshold_bits, "\n")

cat("Umbral óptimo de Otsu (t*) utilizando rango teórico, en escala original:", 
    otsu_ndvi2$threshold_scaled, "\n")

NDVI_array <- as.array(NDVI)

otsu_ndvi3 <- otsu(x = NDVI_array, range = c(-1, 1), levels = 256)

cat("Umbral óptimo de Otsu (t*) cálculado mediante el paquete EBImage:", otsu_ndvi3, "\n")

# =========================
# 5) Otsu’s thresholding (varianza intraclase)
# =========================

otsu_threshold_min <- function(x_rast, L = 256, range = NULL, eps = 1e-10) {
  if (!inherits(x_rast, "SpatRaster")) {x_rast <- terra::rast(x_rast)}
  if (length(L) != 1 || !is.finite(L)) stop("L debe ser un único valor numérico finito.")
  L <- as.integer(L)
  if (L < 2) stop("L debe ser un entero >= 2.")
  if (is.null(range)) {
    r_min <- terra::global(x_rast, "min", na.rm = TRUE)[1, 1]
    r_max <- terra::global(x_rast, "max", na.rm = TRUE)[1, 1]
  } else {
    stopifnot(length(range) == 2)
    r_min <- as.numeric(range[1])
    r_max <- as.numeric(range[2])
  }
  if (!is.finite(r_min) || !is.finite(r_max)) {
    stop("El rango contiene valores no finitos (NA/Inf).")
  }
  if (r_max <= r_min) {
    stop("Rango degenerado: r_max <= r_min. Otsu no es informativo.")
  }
  rast_nbits <- round((x_rast - r_min) / (r_max - r_min) * (L - 1))
  rast_nbits <- terra::clamp(rast_nbits, lower = 0, upper = L - 1, values = TRUE)
  vals_nbits <- terra::values(rast_nbits, mat = FALSE)
  vals_nbits <- vals_nbits[is.finite(vals_nbits)]
  if (length(vals_nbits) == 0) {
    stop("No hay píxeles válidos para calcular el histograma.")
  }
  h <- tabulate(vals_nbits + 1, nbins = L) 
  N <- sum(h)
  P <- h / N
  i <- 0:(L - 1)
  omega0 <- cumsum(P)
  omega1 <- 1 - omega0
  cum_ip  <- cumsum(i * P)
  cum_i2p <- cumsum((i^2) * P)
  mu0 <- cum_ip / pmax(omega0, eps)
  muT <- sum(i * P)
  mu1 <- (muT - cum_ip) / pmax(omega1, eps)
  Ei2_total <- sum((i^2) * P)
  E2_0 <- cum_i2p / pmax(omega0, eps)
  E2_1 <- (Ei2_total - cum_i2p) / pmax(omega1, eps)
  sigma0_2 <- E2_0 - mu0^2
  sigma1_2 <- E2_1 - mu1^2
  sigma_w2 <- omega0 * sigma0_2 + omega1 * sigma1_2
  sigma_w2[omega0 < eps | omega1 < eps] <- Inf
  threshold_bits <- which.min(sigma_w2) - 1
  threshold_scaled <- r_min + (threshold_bits / (L - 1)) * (r_max - r_min)
  return(list(
    threshold_scaled = threshold_scaled,
    threshold_bits   = threshold_bits,
    sigma_w2         = sigma_w2,
    levels           = i,
    range_used       = c(r_min, r_max)
  ))
}
otsu_min_ndvi <- otsu_threshold_min(NDVI)

cat("Umbral óptimo por varianza intraclase:", otsu_min_ndvi$threshold_scaled, "\n")

cat("Umbral óptimo por varianza interclase:", otsu_ndvi$threshold_scaled, "\n")

# =========================
# 6) Otsu’s thresholding (discriminante de Fisher)
# =========================

otsu_threshold_fisher <- function(x_rast, L = 256, range = NULL, eps = 1e-10) {
  if (!inherits(x_rast, "SpatRaster")) {x_rast <- terra::rast(x_rast)}
  if (length(L) != 1 || !is.finite(L)) stop("L debe ser un único valor numérico finito.")
  L <- as.integer(L)
  if (L < 2) stop("L debe ser un entero >= 2.")
  if (is.null(range)) {
    r_min <- terra::global(x_rast, "min", na.rm = TRUE)[1, 1]
    r_max <- terra::global(x_rast, "max", na.rm = TRUE)[1, 1]
  } else {
    stopifnot(length(range) == 2)
    r_min <- as.numeric(range[1])
    r_max <- as.numeric(range[2])
  }
  if (!is.finite(r_min) || !is.finite(r_max)) stop("El rango contiene valores no finitos (NA/Inf).")
  if (r_max <= r_min) stop("Rango degenerado: r_max <= r_min. Otsu no es informativo.")
  rast_nbits <- round((x_rast - r_min) / (r_max - r_min) * (L - 1))
  rast_nbits <- terra::clamp(rast_nbits, lower = 0, upper = L - 1, values = TRUE)
  vals_nbits <- terra::values(rast_nbits, mat = FALSE)
  vals_nbits <- vals_nbits[is.finite(vals_nbits)]
  if (length(vals_nbits) == 0) stop("No hay píxeles válidos para calcular el histograma.")
  h <- tabulate(vals_nbits + 1, nbins = L)
  N <- sum(h)
  P <- h / N
  i <- 0:(L - 1)
  omega0 <- cumsum(P)
  omega1 <- 1 - omega0
  cum_ip  <- cumsum(i * P)
  cum_i2p <- cumsum((i^2) * P)
  mu0 <- cum_ip / pmax(omega0, eps)
  muT <- sum(i * P)
  mu1 <- (muT - cum_ip) / pmax(omega1, eps)
  Ei2_total <- sum((i^2) * P)
  E2_0 <- cum_i2p / pmax(omega0, eps)
  E2_1 <- (Ei2_total - cum_i2p) / pmax(omega1, eps)
  sigma0_2 <- E2_0 - mu0^2
  sigma1_2 <- E2_1 - mu1^2
  sigma_w2 <- omega0 * sigma0_2 + omega1 * sigma1_2
  sigma_w2[omega0 < eps | omega1 < eps] <- NA_real_
  sigma_b2 <- omega0 * omega1 * (mu0 - mu1)^2
  sigma_b2[omega0 < eps | omega1 < eps] <- NA_real_
  J <- sigma_b2 / pmax(sigma_w2, eps)
  threshold_bits <- which.max(J) - 1
  threshold_scaled <- r_min + (threshold_bits / (L - 1)) * (r_max - r_min)
  return(list(
    threshold_scaled = threshold_scaled,
    threshold_bits   = threshold_bits,
    J_fisher         = J,
    sigma_w2         = sigma_w2,
    sigma_b2         = sigma_b2,
    levels           = i,
    range_used       = c(r_min, r_max)
  ))
}

otsu_fisher_ndvi <- otsu_threshold_fisher(NDVI)

cat("Umbral óptimo por varianza interclase:", otsu_ndvi$threshold_scaled, "\n")

cat("Umbral óptimo por varianza intraclase:", otsu_min_ndvi$threshold_scaled, "\n")

cat("Umbral óptimo por criterio de Fisher:", otsu_fisher_ndvi$threshold_scaled, "\n")

NDVI_df2 <- as.data.frame(NDVI)

colnames(NDVI_df2) <- "NDVI"

NDVI_df2 <- NDVI_df2 %>%
  mutate(NDVI_bits = round((NDVI - min(NDVI)) / (max(NDVI) - min(NDVI)) * (256 - 1))) %>%
  count(NDVI_bits, name = "count") %>%
  mutate(count_norm = count / max(count, na.rm = TRUE))

varCriterios_plot <- data.frame(
  level = otsu_fisher_ndvi$levels,
  sigma_b2 = otsu_fisher_ndvi$sigma_b2 / max(otsu_fisher_ndvi$sigma_b2, na.rm = TRUE),
  sigma_w2 = otsu_fisher_ndvi$sigma_w2 / max(otsu_fisher_ndvi$sigma_w2, na.rm = TRUE),
  J = otsu_fisher_ndvi$J / max(otsu_fisher_ndvi$J, na.rm = TRUE)) %>%
  dplyr::filter(is.finite(sigma_b2), is.finite(sigma_w2), is.finite(J))

ggplot() +
  geom_col(data = NDVI_df2, aes(x = NDVI_bits, y = count_norm), fill = "gray75", color = NA, width = 1) +
  geom_line(data = varCriterios_plot, aes(x = level, y = sigma_b2, color = "Interclase"), linewidth = 1, 
            alpha = 0.65) +
  geom_line(data = varCriterios_plot, aes(x = level, y = sigma_w2, color = "Intraclase"), linewidth = 1, 
            alpha = 0.65) +
  geom_line(data = varCriterios_plot, aes(x = level, y = J, color = "Fisher"), linewidth = 1, 
            alpha = 0.65) +
  scale_color_manual(values = c("Interclase" = "red", "Intraclase" = "blue", "Fisher" = "darkgreen"),
                     labels = c("Interclase" = "Varianza\ninterclase (σ²_B)",
                                "Intraclase" = "Varianza\nintraclase (σ²_W)",
                                "Fisher"     = "Discriminante\nde Fisher (J)")) +
  scale_x_continuous(breaks = seq(0, 255, by = 50)) +
  geom_vline(xintercept = threshold_bits_NDVI, color = "black", linewidth = 0.7, linetype = "dashed") +
  geom_point(data = varCriterios_plot %>% 
               dplyr::filter(level == threshold_bits_NDVI), aes(x = level, y = sigma_b2 + 0.004),
             color = "red", size = 3) +
  geom_point(data = varCriterios_plot %>% 
               dplyr::filter(level == threshold_bits_NDVI), aes(x = level, y = sigma_w2),
             color = "blue", size = 3) +
  geom_point(data = varCriterios_plot %>% 
               dplyr::filter(level == threshold_bits_NDVI), aes(x = level, y = J - 0.004),
             color = "darkgreen", size = 3) +
  theme_minimal(base_size = 9) +
  annotate("text", x = threshold_bits_NDVI, y = 0.5, label = "Otsu (t*)", color = "grey10", angle = 90, 
           vjust = -0.5, size = 3.4) +
  labs(x = "Intensidad NDVI (bins 0–255)", y = "Normalización",
       title = "Histograma de intensidades NDVI y funciones objetivo del método de Otsu",
       color = "Función objetivo") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 9.5),
        legend.title = element_text(face = "bold"),
        panel.grid.minor = element_blank())



