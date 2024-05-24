rm(list=ls())
library(signal)

# Definirana kritična frekvencija od 0.1
fc <- 0.1

# Generiranje koeficijenata filtera
butter <- butter(3, W = fc, type = "low")

# Računanje frekvencije filtera
H_butter <- freqz(butter$b, butter$a, 100)

# Funkcija koja definira crtanje grafa
hPlot <- function(H, filter_name) {
  plot(H$f, abs(H$h),
       type = "l",
       col = "red",
       ylim = c(0, 1),
       xlab = "Normalizirana frekvencija",
       ylab = "Amplituda",
       main = paste("Filter", filter_name, "(z)", sep = " ")
  )
}

# Poziv funkcije za crtanje grafa
hPlot(H_butter, "Butterworth")
