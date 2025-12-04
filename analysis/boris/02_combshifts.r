pckgs <- c(
  "tidyverse"
)
lapply(pckgs, require, character.only = TRUE)
len <- base::length

dfmerge <- function(path1, path2) {
  id <- str_split(path1, pattern = "_")[[1]][1] %>%
    str_split(string = ., pattern = "/")
  id <- id[[1]][3]
  a <- read.csv(path1) %>%
    filter((sujeto == "Niñx"))
  b <- read.csv(path2) %>%
    filter((categoría == "1_Fase_de_actividad")|(sujeto == "Adultx"))
  rbind(a, b) %>%
    write.csv(paste0("baked/bindos/", id, "_bind.csv"), row.names = FALSE)
}
dfmerge1 <- function(path1, path2) {
  id <- str_split(path1, pattern = "_")[[1]][1] %>%
    str_split(string = ., pattern = "/")
  id <- id[[1]][3]
  a <- read.csv(path1) %>%
    filter((sujeto == "Niñx"))
  b <- read.csv(path2) %>%
    filter((categoría == "1_Fase_de_actividad")|(sujeto == "Adultx"))
  rbind(a, b) %>% write.csv(paste0("baked/dups/bindos/", id, "_bind.csv"))
}

merger <- read.csv("merge.csv") %>% filter(ID != "C3D06")
dups <- merger %>%
  filter(ID  %in% c(
    "C3A02", "C3B12", "C3D07", "C3G02", "C3G04",
    "C3I09", "C3I10", "C3I18", "C3I20"
  ))


for (i in seq_len(nrow(merger))) {
  try(
    dfmerge(
      path2 = paste0("baked/shifties/", merger[i, 4], "_shifted.csv"),
      path1 = paste0("baked/shifties/", merger[i, 5], "_shifted.csv")
    ), silent = TRUE
  )
}

for (i in seq_len(nrow(dups))) {
  try(
    dfmerge1(
      path2 = paste0("baked/shifties/", dups[i, 6], "_shifted.csv"),
      path1 = paste0("baked/shifties/", dups[i, 7], "_shifted.csv")
    ), silent = TRUE
  )
}
