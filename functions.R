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

calculate_ZCR <- function(samples, f) {
  # Function for calculating the Zero Crossing Rate of a signal
  round((f/(2*length(samples)))*sum(abs(diff(sign(samples)))), digits = 2)
}

