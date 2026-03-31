# Umbralización de Otsu (Otsu's thresholding)
# Autor: Renzo Angel De La Cruz Gonzales
# Fecha: 2026-02-14
# Licencia: MIT
# GitHub: https://github.com/delacruz-renzo


#' Calcula el umbral óptimo de Otsu según el criterio de varianza interclase
#'
#' @param x_rast Imagen de entrada como objeto SpatRaster o ruta legible por terra.
#' @param L Número de niveles (bins) del histograma. Por defecto, 256.
#' @param range Vector de longitud 2 con el mínimo y máximo para escalar los valores.
#' @param eps Constante pequeña para evitar divisiones indeterminadas.
#'
#' @return Una lista con el umbral en escala original, el umbral en niveles
#' discretizados, la varianza interclase, los niveles evaluados y el rango utilizado.
#' @import terra
#' @export
otsu_threshold <- function(x_rast, L = 256, range = NULL, eps = 1e-10) {
  
  if (!inherits(x_rast, "SpatRaster")) {
    x_rast <- terra::rast(x_rast)
  }
  
  if (length(L) != 1 || !is.finite(L)) {
    stop("L debe ser un único valor numérico finito.")
  }
  
  L <- as.integer(L)
  
  if (L < 2) {
    stop("L debe ser un entero >= 2.")
  }
  
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


#' Calcula el umbral óptimo de Otsu según el criterio de varianza intraclase
#'
#' @param x_rast Imagen de entrada como objeto SpatRaster o ruta legible por terra.
#' @param L Número de niveles (bins) del histograma. Por defecto, 256.
#' @param range Vector de longitud 2 con el mínimo y máximo para escalar los valores.
#' @param eps Constante pequeña para evitar divisiones indeterminadas.
#'
#' @return Una lista con el umbral en escala original, el umbral en niveles discretizados,
#' la varianza intraclase, los niveles evaluados y el rango utilizado.
#' @import terra
#' @export
otsu_threshold_min <- function(x_rast, L = 256, range = NULL, eps = 1e-10) {
  
  if (!inherits(x_rast, "SpatRaster")) {
    x_rast <- terra::rast(x_rast)
  }
  
  if (length(L) != 1 || !is.finite(L)) {
    stop("L debe ser un único valor numérico finito.")
  }
  
  L <- as.integer(L)
  
  if (L < 2) {
    stop("L debe ser un entero >= 2.")
  }
  
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


#' Calcula el umbral óptimo de Otsu según el criterio Discriminante de Fisher
#'
#' @param x_rast Imagen de entrada como objeto SpatRaster o ruta legible por terra.
#' @param L Número de niveles (bins) del histograma. Por defecto, 256.
#' @param range Vector de longitud 2 con el mínimo y máximo para escalar los valores.
#' @param eps Constante pequeña para evitar divisiones indeterminadas.
#'
#' @return Una lista con el umbral en escala original, el umbral en niveles discretizados,
#' el Discriminante de Fisher, la varianza intraclase, la varianza interclase, los niveles
#' evaluados y el rango utilizado.
#' @import terra
#' @export
otsu_threshold_fisher <- function(x_rast, L = 256, range = NULL, eps = 1e-10) {
  
  if (!inherits(x_rast, "SpatRaster")) {
    x_rast <- terra::rast(x_rast)
  }
  
  if (length(L) != 1 || !is.finite(L)) {
    stop("L debe ser un único valor numérico finito.")
  }
  
  L <- as.integer(L)
  
  if (L < 2) {
    stop("L debe ser un entero >= 2.")
  }
  
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

