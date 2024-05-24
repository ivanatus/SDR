# Funkcija za pretvaranje ASCII znakova u binarne podatke
ascii_to_binary <- function(text) {
  binary_data <- sapply(strsplit(utf8ToInt(text), ""), function(x) paste(as.integer(x), collapse = ""))
  return(binary_data)
}

# Funkcija za kodiranje poruke određenom kodnom sekvencijom
encode_message <- function(message, code_sequence) {
  encoded_message <- as.numeric(strsplit(message, "")[[1]]) * as.numeric(strsplit(as.character(code_sequence), "")[[1]])
  return(encoded_message)
}

# Funkcija za dekodiranje poruke koristeći kodnu sekvencu
decode_message <- function(encoded_message, code_sequence) {
  decoded_message <- ifelse(as.numeric(encoded_message) == 0, 0, 1) / as.numeric(strsplit(as.character(code_sequence), "")[[1]])
  return(decoded_message)
}

# Definiranje kodnih sekvenci za svaki kanal
channel1_code <- "11111"
channel2_code <- "11011"
channel3_code <- "10111"
channel4_code <- "10011"

# Funkcija za generiranje CDMA kompozitnog signala
generate_cdma_signal <- function(messages, codes) {
  composite_signal <- rep(0, length(messages[[1]]))
  for (i in seq_along(messages)) {
    encoded_message <- encode_message(messages[[i]], codes[[i]])
    composite_signal <- composite_signal + encoded_message
  }
  return(composite_signal)
}

# Primjeri poruka koje šalju odašiljači
messages <- list("10", "01", "11", "00")

# Kodne sekvence za svaki kanal
codes <- list(channel1_code, channel2_code, channel3_code, channel4_code)

# Generiranje CDMA kompozitnog signala
composite_signal <- generate_cdma_signal(messages, codes)

# Ispis rezultata
print("CDMA kompozitni signal:")
print(composite_signal)
