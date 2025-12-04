pckgs <- c(
  "dplyr",
  "purrr",
  "stringr"
)
lapply(pckgs, require, character.only = TRUE)
len <- base::length

dfproc <- function(path) {
  id <- str_split(path, pattern = "_")[[1]][1] %>%
    str_split(string = ., pattern = "/")
  id <- id[[1]][3]
  shifted <- read.csv(path)
  filtro <- shifted %>%
    filter(categoría == "1_Fase_de_actividad")
  binary_vector <- rep(NA, round(max(shifted$offset) * 30))
  categirical_vector <- binary_vector
  for (i in seq_len(nrow(filtro))) {
    categirical_vector[(
      round(filtro$onset[i] * 30)
    ):round(filtro$offset[i] * 30)] <- filtro$modificador[i]
  }
  fdf <- data.frame(
    frame = 1:len(binary_vector),
    fase = categirical_vector
  )
  filtro <- shifted %>%
    filter((sujeto == "Adultx") & (categoría == "2_Expresividad_facial"))
  categirical_vector <- binary_vector
  for (i in seq_len(nrow(filtro))) {
    categirical_vector[(
      round(filtro$onset[i] * 30)
    ):round(filtro$offset[i] * 30)] <- filtro$modificador[i]
  }
  aef <- data.frame(
    frame = 1:len(binary_vector),
    adulto_ef = categirical_vector
  )
  filtro <- shifted %>%
    filter((sujeto == "Adultx") & (categoría == "3_Mirada"))
  categirical_vector <- binary_vector
  for (i in seq_len(nrow(filtro))) {
    categirical_vector[(
      round(filtro$onset[i] * 30)
    ):round(filtro$offset[i] * 30)] <- filtro$modificador[i]
  }
  am <- data.frame(
    frame = 1:len(binary_vector),
    adulto_mirada = categirical_vector
  )
  filtro <- shifted %>%
    filter((sujeto == "Niñx") & (categoría == "2_Expresividad_facial"))
  categirical_vector <- binary_vector
  for (i in seq_len(nrow(filtro))) {
    categirical_vector[(
      round(filtro$onset[i] * 30)
    ):round(filtro$offset[i] * 30)] <- filtro$modificador[i]
  }
  nef <- data.frame(
    frame = 1:len(binary_vector),
    niño_ef = categirical_vector
  )
  filtro <- shifted %>%
    filter((sujeto == "Niñx") & (categoría == "3_Mirada"))
  categirical_vector <- binary_vector
  for (i in seq_len(nrow(filtro))) {
    categirical_vector[(
      round(filtro$onset[i] * 30)
    ):round(filtro$offset[i] * 30)] <- filtro$modificador[i]
  }
  nm <- data.frame(
    frame = 1:len(binary_vector),
    niño_mirada = categirical_vector
  )
  aef$aef_int <-  recode(
    as.character(aef$adulto_ef),
    "Indeterminable" = "NA",
    "Neutral" = "0",
    "Incomodidad/respuesta_fisiológica" = "0",
    "Aburrido/sueño" = "-1",
    "Ceño_confundido/concentrado" = "0",
    "Chistoso" = "1",
    "Felicidad" = "1",
    "Sorpresa" = "1",
    "Tristeza" = "-1",
    "Enojo" = "-1",
    "Miedo" = "-1",
    "Asco" = "-1",
    "Otro" = "0"
  ) %>% as.numeric()
  nef$nef_int <-  recode(
    as.character(nef$niño_ef),
    "Indeterminable" = "NA",
    "Neutral" = "0",
    "Incomodidad/respuesta_fisiológica" = "0",
    "Aburrido/sueño" = "-1",
    "Ceño_confundido/concentrado" = "0",
    "Chistoso" = "1",
    "Felicidad" = "1",
    "Sorpresa" = "1",
    "Tristeza" = "-1",
    "Enojo" = "-1",
    "Miedo" = "-1",
    "Asco" = "-1",
    "Otro" = "0"
  ) %>% as.numeric()
  am$mi <- recode(
    as.character(am$adulto_mirada),
    "Hoja_propia" = "Hoja_pareja",
    "Hoja_pareja" = "Hoja_propia"
  )
  am$am_int <- recode(
    as.character(am$adulto_mirada),
    "Indeterminable" = "NA",
    "Distensión/reorganización" = "0",
    "Instructivo" = "1",
    "Rostro_pareja" = "1",
    "Hoja_propia" = "1",
    "Hoja_pareja" = "1",
    "Rostro_tercero" = "1",
    "Otro_sintonía" = "1",
    "Activamente_desentendido" = "-1",
    "No_aplica" = "NA"
  ) %>% as.numeric()
  nm$nm_int <- recode(
    as.character(nm$niño_mirada),
    "Indeterminable" = "NA",
    "Distensión/reorganización" = "0",
    "Instructivo" = "1",
    "Rostro_pareja" = "1",
    "Hoja_propia" = "1",
    "Hoja_pareja" = "1",
    "Rostro_tercero" = "1",
    "Otro_sintonía" = "1",
    "Activamente_desentendido" = "-1",
    "No_aplica" = "NA"
  ) %>% as.numeric()
  sync <- data.frame(
    frame = nef$frame,
    pos_af = nef$nef_int + aef$aef_int == 2,
    neut_af = (nef$nef_int == aef$aef_int) & (nef$nef_int + aef$aef_int == 0),
    neg_af = nef$nef_int + aef$aef_int == -2,
    sync_af_int = nef$nef_int == aef$aef_int,
    sync_af_raw = nef$niño_ef == aef$adulto_ef,
    pos_at = nm$nm_int + am$am_int == 2,
    neut_at = (nm$nm_int == am$am_int) & (nm$nm_int + am$am_int == 0),
    neg_at = nm$nm_int + am$am_int == -2,
    sync_at_int = nm$nm_int == am$am_int,
    sync_at_raw = nm$niño_mirada == am$mi
  )
  wdf <- reduce(
    list(fdf, nef, aef, nm, am, sync),
    dplyr::left_join,
    by = "frame"
  )
  wdf <- wdf %>% mutate(
    niño_enjoy = nef_int == 1,
    niño_fuzz = nef_int == -1,
    niño_neut = nef_int == 0,
    niño_engag = nm_int == 1,
    niño_diseng = nm_int == -1,
    adulto_enjoy = aef_int == 1,
    adulto_fuzz = aef_int == -1,
    adulto_neut = aef_int == 0,
    adulto_engag = am_int == 1,
    adulto_diseng = am_int == -1,
  )
  wdf$id <- rep(id, len(binary_vector))
  whl <- wdf %>% filter(
    (fase == "Piedra_papel_tijera") |(fase == "Avioncito") | (fase == "Paloma")
  )
  ppt <- wdf %>% filter(fase == "Piedra_papel_tijera")
  av <- wdf %>% filter(fase == "Avioncito")
  pal <- wdf %>% filter(fase == "Paloma")
  figs <- wdf %>% filter((fase == "Avioncito") | (fase == "Paloma"))
  wdf %>%
    select(
      1, 32, 2, 3:31
    ) %>%
    write.csv(paste0("baked/tss/", id, "_ts.csv"), row.names = FALSE)
  #Debería ser más facil con groupby + mutate. No sé porqué lo hice tan cabeza.
  return(
    list(
      data.frame(
        id = id,
        nframes_tot = nrow(whl),
        nframes_ppt = nrow(ppt),
        nframes_av = nrow(av),
        nframes_pal = nrow(pal),
        nframes_figs = nrow(figs),

        prop_nenjoy_tot = mean(whl$niño_enjoy, na.rm = TRUE),
        prop_nenjoy_ppt = mean(ppt$niño_enjoy, na.rm = TRUE),
        prop_nenjoy_av = mean(av$niño_enjoy, na.rm = TRUE),
        prop_nenjoy_pal = mean(pal$niño_enjoy, na.rm = TRUE),
        prop_nenjoy_figs = mean(figs$niño_enjoy, na.rm = TRUE),
        prop_nfuzz_tot = mean(whl$niño_fuzz, na.rm = TRUE),
        prop_nfuzz_ppt = mean(ppt$niño_fuzz, na.rm = TRUE),
        prop_nfuzz_av = mean(av$niño_fuzz, na.rm = TRUE),
        prop_nfuzz_pal = mean(pal$niño_fuzz, na.rm = TRUE),
        prop_nfuzz_figs = mean(figs$niño_fuzz, na.rm = TRUE),
        prop_nneut_tot = mean(whl$niño_neut, na.rm = TRUE),
        prop_nneut_ppt = mean(ppt$niño_neut, na.rm = TRUE),
        prop_nneut_av = mean(av$niño_neut, na.rm = TRUE),
        prop_nneut_pal = mean(pal$niño_neut, na.rm = TRUE),
        prop_nneut_figs = mean(figs$niño_neut, na.rm = TRUE),

        prop_nemo_tot = mean(whl$nef_int, na.rm = TRUE),
        prop_nemo_ppt = mean(ppt$nef_int, na.rm = TRUE),
        prop_nemo_av = mean(av$nef_int, na.rm = TRUE),
        prop_nemo_pal = mean(pal$nef_int, na.rm = TRUE),
        prop_nemo_figs = mean(figs$nef_int, na.rm = TRUE),

        prop_nengaged_tot = mean(whl$niño_engag, na.rm = TRUE),
        prop_nengaged_ppt = mean(ppt$niño_engag, na.rm = TRUE),
        prop_nengaged_av = mean(av$niño_engag, na.rm = TRUE),
        prop_nengaged_pal = mean(pal$niño_engag, na.rm = TRUE),
        prop_nengaged_figs = mean(figs$niño_engag, na.rm = TRUE),
        prop_ndisengaged_tot = mean(whl$niño_diseng, na.rm = TRUE),
        prop_ndisengaged_ppt = mean(ppt$niño_diseng, na.rm = TRUE),
        prop_ndisengaged_av = mean(av$niño_diseng, na.rm = TRUE),
        prop_ndisengaged_pal = mean(pal$niño_diseng, na.rm = TRUE),
        prop_ndisengaged_figs = mean(figs$niño_diseng, na.rm = TRUE),

        prop_ngazes_tot = mean(whl$nm_int, na.rm = TRUE),
        prop_ngazes_ppt = mean(ppt$nm_int, na.rm = TRUE),
        prop_ngazes_av = mean(av$nm_int, na.rm = TRUE),
        prop_ngazes_pal = mean(pal$nm_int, na.rm = TRUE),
        prop_ngazes_figs = mean(figs$nm_int, na.rm = TRUE),

        prop_aenjoy_tot = mean(whl$adulto_enjoy, na.rm = TRUE),
        prop_aenjoy_ppt = mean(ppt$adulto_enjoy, na.rm = TRUE),
        prop_aenjoy_av = mean(av$adulto_enjoy, na.rm = TRUE),
        prop_aenjoy_pal = mean(pal$adulto_enjoy, na.rm = TRUE),
        prop_aenjoy_figs = mean(figs$adulto_enjoy, na.rm = TRUE),
        prop_afuzz_tot = mean(whl$adulto_fuzz, na.rm = TRUE),
        prop_afuzz_ppt = mean(ppt$adulto_fuzz, na.rm = TRUE),
        prop_afuzz_av = mean(av$adulto_fuzz, na.rm = TRUE),
        prop_afuzz_pal = mean(pal$adulto_fuzz, na.rm = TRUE),
        prop_afuzz_figs = mean(figs$adulto_fuzz, na.rm = TRUE),
        prop_aneut_tot = mean(whl$adulto_neut, na.rm = TRUE),
        prop_aneut_ppt = mean(ppt$adulto_neut, na.rm = TRUE),
        prop_aneut_av = mean(av$adulto_neut, na.rm = TRUE),
        prop_aneut_pal = mean(pal$adulto_neut, na.rm = TRUE),
        prop_aneut_figs = mean(figs$adulto_neut, na.rm = TRUE),

        prop_aemo_tot = mean(whl$aef_int, na.rm = TRUE),
        prop_aemo_ppt = mean(ppt$aef_int, na.rm = TRUE),
        prop_aemo_av = mean(av$aef_int, na.rm = TRUE),
        prop_aemo_pal = mean(pal$aef_int, na.rm = TRUE),
        prop_aemo_figs = mean(figs$aef_int, na.rm = TRUE),

        prop_aengaged_tot = mean(whl$adulto_engag, na.rm = TRUE),
        prop_aengaged_ppt = mean(ppt$adulto_engag, na.rm = TRUE),
        prop_aengaged_av = mean(av$adulto_engag, na.rm = TRUE),
        prop_aengaged_pal = mean(pal$adulto_engag, na.rm = TRUE),
        prop_aengaged_figs = mean(figs$adulto_engag, na.rm = TRUE),
        prop_adisengaged_tot = mean(whl$adulto_diseng, na.rm = TRUE),
        prop_adisengaged_ppt = mean(ppt$adulto_diseng, na.rm = TRUE),
        prop_adisengaged_av = mean(av$adulto_diseng, na.rm = TRUE),
        prop_adisengaged_pal = mean(pal$adulto_diseng, na.rm = TRUE),
        prop_adisengaged_figs = mean(figs$adulto_diseng, na.rm = TRUE),

        prop_agazes_tot = mean(whl$am_int, na.rm = TRUE),
        prop_agazes_ppt = mean(ppt$am_int, na.rm = TRUE),
        prop_agazes_av = mean(av$am_int, na.rm = TRUE),
        prop_agazes_pal = mean(pal$am_int, na.rm = TRUE),
        prop_agazes_figs = mean(figs$am_int, na.rm = TRUE),

        prop_shared_paf_tot = mean(whl$pos_af, na.rm = TRUE),
        prop_shared_paf_ppt = mean(ppt$pos_af, na.rm = TRUE),
        prop_shared_paf_av = mean(av$pos_af, na.rm = TRUE),
        prop_shared_paf_pal = mean(pal$pos_af, na.rm = TRUE),
        prop_shared_paf_figs = mean(figs$pos_af, na.rm = TRUE),

        prop_shared_val_tot = mean(whl$sync_af_int, na.rm = TRUE),
        prop_shared_val_ppt = mean(ppt$sync_af_int, na.rm = TRUE),
        prop_shared_val_av = mean(av$sync_af_int, na.rm = TRUE),
        prop_shared_val_pal = mean(pal$sync_af_int, na.rm = TRUE),
        prop_shared_val_figs = mean(figs$sync_af_int, na.rm = TRUE),

        prop_shared_neu_tot = mean(whl$neut_af, na.rm = TRUE),
        prop_shared_neu_ppt = mean(ppt$neut_af, na.rm = TRUE),
        prop_shared_neu_av = mean(av$neut_af, na.rm = TRUE),
        prop_shared_neu_pal = mean(pal$neut_af, na.rm = TRUE),
        prop_shared_neu_figs = mean(figs$neut_af, na.rm = TRUE),

        prop_sync_gaze_tot = mean(whl$sync_at_int, na.rm = TRUE),
        prop_sync_gaze_ppt = mean(ppt$sync_at_int, na.rm = TRUE),
        prop_sync_gaze_av = mean(av$sync_at_int, na.rm = TRUE),
        prop_sync_gaze_pal = mean(pal$sync_at_int, na.rm = TRUE),
        prop_sync_gaze_figs = mean(figs$sync_at_int, na.rm = TRUE),
        prop_sync_eng_tot = mean(whl$pos_at, na.rm = TRUE),
        prop_sync_eng_ppt = mean(ppt$pos_at, na.rm = TRUE),
        prop_sync_eng_av = mean(av$pos_at, na.rm = TRUE),
        prop_sync_eng_pal = mean(pal$pos_at, na.rm = TRUE),
        prop_sync_eng_figs = mean(figs$pos_at, na.rm = TRUE)
      ),
      wdf
    )
  )
}

lista1 <- list()
lista2 <- list()

for (i in seq_along(list.files("baked/bindos"))) {
  tryCatch(
    expr = {
      proc <- dfproc(paste0("baked/bindos/", list.files("baked/bindos")[i]))
      lista1[[i]] <- proc[[1]]
      lista2[[i]] <- proc[[2]]
    },
    error = function(e){print(i)}
  )
}

bind_rows(lista1) %>% write.csv("baked/props.csv", row.names = FALSE)
bind_rows(lista2) %>% write.csv("baked/mega_ts.csv", row.names = FALSE)

# proc <- dfproc(path)
# proc[[1]]
# proc[[2]]
# path <- paste0("baked/bindos/",list.files("baked/bindos")[30])
