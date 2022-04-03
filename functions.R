calculate_volume <- function(samples) {
  # Function for calculating the volume of a signal
  # p(n) = sqrt(1/n sum(s^2(n)))
  round(sqrt((1/length(samples))*sum(samples**2)), digits = 2)
}

calculate_STE <- function(samples) {
  # Function for calculating the short time energy of a signal
  # STE(n) = p^2(n)
  round((1/length(samples))*sum(samples**2), digits = 2)
}

calculate_ZCR <- function(samples) {
  # Function for calculating the Zero Crossing Rate of a signal
  round(((1)/(2*length(samples)))*sum(abs(diff(sign(samples)))), digits = 2)
}

calculate_VSTD <- function(samples, number_of_frames) {
  # Function for calculating the Standard Deviation normalized of a signal
  round(suppressWarnings(sd(samples)/mean(unlist(lapply(split(samples, seq(1,number_of_frames)), FUN = calculate_volume)))), digits = 2)
}

calculate_VDR <- function(samples, number_of_frames) {
  # Function for calculating the Volume Dynamic Range of a signal
  volume <- suppressWarnings(unlist(lapply(split(samples, seq(1,number_of_frames)), FUN = calculate_volume)))
  
  round(1 - (min(volume))/(max(volume)), digits = 2)
}

calculate_LSTER <- function(samples, number_of_frames) {
  # Function for calculating the LSTER of a signal
  ste_vector <- suppressWarnings(unlist(lapply(split(samples, seq(1, number_of_frames)), FUN = calculate_STE)))
  
  
  round(((1)/(2*number_of_frames))*(sum(sign(0.5*mean(ste_vector) - ste_vector) + 1)), digits = 2)
}

calculate_SR <- function(samples, f, volume_threshold, zcr_threshold) {
  # Function for calculating the Silent Ratio Range of a signal
  (calculate_volume(samples) > volume_threshold) | (calculate_ZCR(samples) > zcr_threshold)
}

calculate_autocorelation <- function(samples) {
  acf(samples, plot = FALSE)
}

calculate_HZCRR <- function(samples, number_of_frames) {
  zcr_vector <- suppressWarnings(unlist(lapply(split(samples, seq(1, number_of_frames)), FUN = calculate_ZCR)))
  
  
  round(((1)/(2*number_of_frames))*(sum(sign(zcr_vector - 1.5*mean(zcr_vector)) + 1)), digits = 2)
}

calculate_entropy <- function(samples, number_of_frames) {
  ste_vector <- suppressWarnings(unlist(lapply(split(samples, seq(1, number_of_frames)), FUN = calculate_STE)))
  normalized_ste_vector <- ste_vector/sum(ste_vector)
  
  round(-sum(normalized_ste_vector * log(normalized_ste_vector, base = 2)), digits = 2)
}

