klobuchar_model <- function(phi_u, lambda_u, E, A, alpha, beta, time) {
  E_semi <- E / 180  # promjena kuta E iz stupnjeva u radijane
  psi <- pi/2 - E_semi - asin((6378000/(6378000 + 350000))*cos(E_semi))
  
  phi_u_semi <- phi_u / 180  # promjena geografske širine iz stupnjeva u radijane
  phi_i <- phi_u_semi + psi * cos(A * pi / 180)  # promjena kuta A iz stupnjeva u radijane
  if (phi_i > 0.416) phi_i <- 0.416
  if (phi_i < -0.416) phi_i <- -0.416
  
  lambda_u_semi <- lambda_u / 180  # promjena geografske dužine iz stupnjeva u radijane
  lambda_i <- lambda_u_semi + psi * sin(A * pi / 180) / cos(phi_i * pi)
  
  phi_m <- phi_i + 0.064 * cos((lambda_i * 180 - 1.617) * pi / 180)
  
  t <- 43200 * lambda_i + time
  t <- t %% 86400 
  
  A_i <- alpha[1] + alpha[2] * phi_m + alpha[3] * phi_m^2 + alpha[4] * phi_m^3
  if (A_i < 0) A_i <- 0
  
  P_i <- beta[1] + beta[2] * phi_m + beta[3] * phi_m^2 + beta[4] * phi_m^3
  if (P_i < 72000) P_i <- 72000
  
  X_i <- 2 * pi * (t - 50400) / P_i
  
  F <- 1 + 16 * (0.53 - E_semi)^3
  
  if (abs(X_i) < 1.57) {
    T_iono <- F * (5e-9 + A_i * (1 - X_i^2 / 2 + X_i^4 / 24))
  } else {
    T_iono <- F * 5e-9
  }
  
  return(T_iono)
}

# Definicija položaja
latitude = 45.33709
longitude = 14.42496
E = 90
A = 180
# Koeficijenti Klobucharovog modela
alpha <- c(1.4900e-08, -7.4510e-09, -5.9600e-08, 1.1920e-07)
beta <- c(1.2900e+05, -1.9660e+05, 6.5540e+04, 3.2770e+05)

# Simulacija za 24 sata u koracima od 1 minute
time_seq <- seq(0, 86400, by = 60)

# Poziv modela
ionospheric_delays <- sapply(time_seq, function(time) {
  klobuchar_model(latitude, longitude, E, A, alpha, beta, time)
})

# Ispis rezultata
for (i in seq(1, length(ionospheric_delays), by = 60)) {
  #cat(sprintf("Time: %02d:00 - Ionospheric Delay: %e seconds\n", (i / 60) - 1, ionospheric_delays[i]))
  print("Time:")
  print((i/60))
  print("Ionospheric delay: ")
  print(ionospheric_delays[i])
  print("_______________________")
}
