library(dplyr)
library(tidyr)
merger <- function(a){
  Reduce(
    function(x, y) merge(x, y, by = "ID", all.x = TRUE, all.y =TRUE),
    a
  )
}

mj <- read.csv("data/raw/submuestras.csv")
mj <- mj %>% filter(inc == 1)
raw <- read.csv("data/raw/nux_raw.csv")
raw <- raw %>% filter(userid %in% c(
  mj$nid1, mj$nid2
))

raw$trial_timestamp <- strptime(
  raw$trial_timestamp,
  "%Y-%m-%dT%H:%M:%OSZ"
)
raw$response_timestamp <- strptime(
  raw$response_timestamp,
  "%Y-%m-%dT%H:%M:%OSZ"
)

raw$session_start_timestamp <- strptime(
  raw$session_start_timestamp,
  "%Y-%m-%dT%H:%M:%OSZ"
)

raw$session_end_timestamp <- strptime(
  raw$session_end_timestamp,
  "%Y-%m-%dT%H:%M:%OSZ"
)

raw$response_reaction_time[raw$response_reaction_time == 0] <- NA

raw <- rbind(
  inner_join(
    raw, select(mj, ID, userid = nid1),
  ),
  inner_join(
    raw, select(mj, ID, userid = nid2),
  )
)

session_summary <- raw[c("ID", "sessionid", "session")] %>%
  group_by(ID, session) %>%
  summarise(
    sesid_count = n_distinct(sessionid),
    nrow_count = n()
  )
# t y nrow x sessionid
session_detail <- raw[c("ID", "sessionid", "session", "trial_timestamp")] %>%
  group_by(ID, session, sessionid) %>%
  summarise(min_t = min(trial_timestamp, na.rm = TRUE),
            max_t = max(trial_timestamp, na.rm = TRUE),
            nrow_count = n()) %>%
  filter(nrow_count > 3 & !is.na(session))

unique(session_detail$ID)

session_summary <- session_detail %>%
  group_by(ID, session) %>%
  summarise(
    sesid_count = n_distinct(sessionid),
  )

#Filtro manual. Traté de elegir las primeras sesiones que parecieran completas
raw_sd <- raw %>% filter(
  sessionid %in% session_detail$sessionid[! session_detail$sessionid %in% c(
    "580de651-14e0-4de4-bab9-ccd0d4d9b7f0",
    "876092c6-e91b-42d1-b26f-d285e0835d22",
    "f92794e8-16ec-4073-968e-f8fae51276f8",
    "f1b083a4-fb7e-4b10-be4a-607c1287e954",
    "1996cfde-85dc-40e7-ae34-ab6fa8765e4b",
    "7cbf1277-6a9f-42dc-bac6-04afbea6e9ae",
    "d357897a-3fed-415b-b9ab-193ad686007b",
    "b4707c4b-85f7-4f0e-93ac-72573504c961",
    "0f8ea13d-ceab-4c82-b936-8f1e46774e6f",
    "a0afb05c-b26d-48ff-aabb-d5b2992d7600",
    "e560413c-0fd7-4c0e-8510-97d22681278d",
    "96a7993a-c76a-41d1-a6cb-37b80a80bbeb",
    "0456fc05-6398-434d-85f7-8502f5483c1b",
    "77bebd01-f6c0-438e-bc6f-8837f647cfe9",
    "6065a322-aab2-477e-b1e9-0a41d9e66b8d",
    "31473003-fcfd-48a2-b9d4-6fca6ad849c3",
    "c5e6a8fe-6712-4dcd-8665-bfed13dbdaae",
    "8f278a00-940f-471f-a6da-8b4b83b6eb77",
    "c42b2313-5cfc-4bfd-93af-f25b0fb4db1d",
    "e8aabff4-9dab-4844-8134-d724749ad4e4",
    "a9591354-f780-48a6-ac81-b6c6cbdefa2a",
    "373e473e-db2b-442d-8d24-fa8c1485ce74",
    "68b084b5-69dd-4fde-a525-59e325fa0af4",
    "81b3d041-f2f8-4908-902c-67a44ea42db4",
    "6635f1c1-2672-4c6b-a0b0-b59e4c58b63f",
    "61bdf977-ad49-48a9-9b82-dc7fe7f019e3",
    "c93c8fd2-cdb6-41c9-a43f-74b531c0c866",
    "982ae6b7-e1d1-4e0d-9bbe-c4390be82255",
    "0cc6e52c-42fe-4ad0-a9a2-3940b6ecfc49",
    "1a5e7649-2ff3-4fe1-baa8-e8ca0a914740",
    "80f30725-7963-42ce-a80d-aa5aa9d4d151",
    "4c3f2aad-cd25-4206-a1ca-1c3e6c23418f",
    "e0b46d74-84d3-43be-b71c-b68ed844646a",
    "8d4e729a-ee7f-41d3-be70-5830a3ba56f7",
    "1b896cdd-a0dd-4777-bf56-9bc6f2770452",
    "6c31b5b0-3ba8-4b91-8f3f-1a59b4e6325d",
    "7a85be28-3d4d-49b9-9904-cd2b1b336a27",
    "eaf260f2-67df-41e5-8b6d-9875f864174e",
    "e163e0d0-32b6-4aaa-8b25-6f83bb3b6ada",
    "7082e94b-6c5e-42b9-a47b-db811dd7671d",
    "dda7cda0-6705-41ac-b4c0-92d7e5d5fee3",
    "0641341e-bd61-4374-83f1-d295032d54ff",
    "f294f286-2b8f-43a5-90ac-2cbb462a8711",
    "52c1c024-957d-4483-bf92-d7cc5a0f4015",
    "5076b852-8c4c-4d08-89fa-e596d8b60ec5",
    "393186a7-4138-46cd-8c9a-31b0f6864f46",
    "83f4e249-5d88-4e12-b705-48257f51f788",
    "fab35611-a19e-4112-a2cd-09244afd758c",
    "7b16f5c2-4456-4c97-a2ba-ea78843854fc",
    "c85f3640-446c-4b64-935e-b96f824d04e1",
    "2aa7ad77-0a0e-4650-97d2-7afed3b07570",
    "ec3b71b8-0df0-4069-85d7-111d89e96ee5",
    "f83909ef-6e8a-41e3-915d-c2f7e9be48ad"
  )]
)

raw_sd[c("ID", "sessionid", "session")] %>%
  group_by(ID, session) %>%
  summarise(
    sesid_count = n_distinct(sessionid)
  ) %>% 
  filter(sesid_count > 1)

unique(raw_sd$ID)

raw_sd <- raw_sd %>%
  distinct() %>%
  arrange(ID, session, game_name, trial_timestamp)

raw_sd %>%
  group_by(ID, session) %>%
  summarise(
    dur = (
      max(session_end_timestamp, na.rm = T) - max(session_start_timestamp, na.rm = T)
    ) / 60
  ) %>% select(dur) %>% unlist()  %>% as.numeric() %>% ifelse(is.infinite(.), NA, .) %>%mean(na.rm=T)


raw_sd %>% filter(
  ID == "AA101" &
    game_name == "¿cómo se sienten?"
) %>%
  View()

raw_sd %>% filter(
  session == 2 &
    session_complete == "true"
) %>%
  select(ID, session_complete) %>%
  unique()

## Toca botón
tb <- raw_sd %>% filter(game_name == "toca botón")
tb1 <- tb %>%
  group_by(ID) %>%
  summarise(
    tb_score = max(score, na.rm = TRUE),
  ) %>%
  mutate(tb_errors = 24 - tb_score)
tb2 <- tb %>%
  filter(is_response_correct == "true" & stimuli_type == "target") %>%
  group_by(ID) %>%
  summarise(tb_meanrt_correct = mean(response_reaction_time, na.rm = TRUE))
tb3 <- tb %>%
  filter(is_response_correct == "false" & stimuli_type == "distractor") %>%
  group_by(ID) %>%
  summarise(tb_meanrt_incorrect = mean(response_reaction_time, na.rm = TRUE))

tb <- merger(list(tb1, tb2, tb3))
## No encaja
ne <- raw_sd %>% filter(game_name == "no encaja")
ne1 <- ne %>%
  group_by(ID) %>%
  summarise(ne_score = max(score, na.rm = TRUE)) %>%
  mutate(ne_errors = 18 - ne_score)
ne2 <- ne %>%
  filter(is_response_correct == "true") %>%
  group_by(ID) %>%
  summarise(ne_meanrt_correct = mean(response_reaction_time, na.rm = TRUE))
ne3 <- ne %>%
  filter(is_response_correct == "false") %>%
  group_by(ID) %>%
  summarise(ne_meanrt_incorrect = mean(response_reaction_time, na.rm = TRUE))
ne <- merger(list(ne1, ne2, ne3))
## Juego de memoria
mt <- raw_sd %>% filter(game_name == "juego de memoria")
mt1 <- mt %>%
  group_by(ID) %>%
  summarise(mt_score = max(score, na.rm = TRUE)) %>%
  mutate(mt_errors = 24 - mt_score)
mt2 <- mt %>%
  filter(is_response_correct == "true") %>%
  group_by(ID) %>%
  summarise(mt_meanrt_correct = mean(response_reaction_time, na.rm = TRUE))
mt3 <- mt %>%
  filter(is_response_correct == "false") %>%
  group_by(ID) %>%
  summarise(mt_meanrt_incorrect = mean(response_reaction_time, na.rm = TRUE))
mt <- merger(list(mt1, mt2, mt3))
## Bloques de corsi directo
bcd <- raw_sd %>% filter(game_name == "bloques de corsi 1")
bcd1 <- bcd %>%
  group_by(ID) %>%
  summarise(
    bcd_score = max(score, na.rm = TRUE),
    bcd_lgst_streak = max(
      rle(is_response_correct == "true")$lengths[rle(
        is_response_correct == "true"
      )$values], na.rm = TRUE
    )
  )
bcd1$bcd_lgst_streak[is.na(as.integer(bcd1$bcd_lgst_streak))] <- 0
bcd2 <- bcd %>%
  filter(is_response_correct == "true") %>%
  group_by(ID) %>%
  summarise(bcd_meanrt_correct = mean(response_reaction_time, na.rm = TRUE))
bcd3 <- bcd %>%
  filter(is_response_correct == "false") %>%
  group_by(ID) %>%
  summarise(bcd_meanrt_incorrect = mean(response_reaction_time, na.rm = TRUE))
bcd <- merger(list(bcd1, bcd2, bcd3))
## Bloques de corsi inverso
bci <- raw_sd %>% filter(game_name == "bloques de corsi 2")
bci1 <- bci %>%
  group_by(ID) %>%
  summarise(
    bci_score = max(score, na.rm = TRUE),
    bci_lgst_streak = max(
      rle(is_response_correct == "true")$lengths[rle(
        is_response_correct == "true"
      )$values], na.rm = TRUE
    )
  )
bci1$bci_lgst_streak[is.na(as.integer(bci1$bci_lgst_streak))] <- 0
bci2 <- bci %>%
  filter(is_response_correct == "true") %>%
  group_by(ID) %>%
  summarise(bci_meanrt_correct = mean(response_reaction_time, na.rm = TRUE))
bci3 <- bci %>%
  filter(is_response_correct == "false") %>%
  group_by(ID) %>%
  summarise(bci_meanrt_incorrect = mean(response_reaction_time, na.rm = TRUE))
bci <- merger(list(bci1, bci2, bci3))
## Simon
sm <- raw_sd %>% filter(game_name == "simón")
sm1 <- sm %>%
  group_by(ID) %>%
  summarise(
    sm_score=max(score, na.rm = TRUE),
    sm_lgst_streak = max(
      rle(is_response_correct == "true")$lengths[rle(
        is_response_correct == "true"
      )$values], na.rm = TRUE
    )
  )
sm2 <- sm %>%
  filter(is_response_correct == "true") %>%
  group_by(ID) %>%
  summarise(sm_meanrt_correct = mean(response_reaction_time, na.rm = TRUE))
sm3 <- sm %>%
  filter(is_response_correct == "false") %>%
  group_by(ID) %>%
  summarise(sm_meanrt_incorrect = mean(response_reaction_time, na.rm = TRUE))
sm <- merger(list(sm1, sm2, sm3))
## Como se sienten
cs <- raw_sd %>% filter(game_name == "¿cómo se sienten?")
cs1 <- cs %>%
  group_by(ID) %>%
  summarise(cs_score = max(score, na.rm = TRUE))
cs2 <- cs %>%
  filter(is_response_correct == "true") %>%
  group_by(ID) %>%
  summarise(cs_meanrt_correct = mean(emotion_reaction_time, na.rm = TRUE))
cs3 <- cs %>%
  filter(is_response_correct == "false") %>%
  group_by(ID) %>%
  summarise(cs_meanrt_incorrect = mean(emotion_reaction_time, na.rm = TRUE))
cs <- merger(list(cs1, cs2, cs3))
## Juego de magia (control reactivo/proactivo)
jm <- raw_sd %>% filter(game_name == "juego de magia")
jm1 <- jm %>%
  group_by(ID) %>%
  filter(experimental_phase == "test") %>%
  summarise(
    jm_tot_test_trials = sum(!is.na(trial_number)),
    jm_score = sum(ifelse(is_response_correct == "true", 1, 0)),
    jm_errors = sum(ifelse(is_response_correct == "false", 1, 0))
  ) %>%
  mutate(jm_complete = jm_tot_test_trials == 60)

jm2 <- jm %>%
  group_by(ID) %>%
  filter(experimental_phase == "test" & is_response_correct == "true") %>%
  summarise(jm_meanrt_correct = mean(response_reaction_time, na.rm = TRUE))

jm3 <- jm %>%
  group_by(ID) %>%
  filter(experimental_phase == "test" & is_response_correct == "false") %>%
  summarise(jm_meanrt_incorrect = mean(response_reaction_time, na.rm = TRUE))

jm4 <- jm %>%
  group_by(ID) %>%
  filter(experimental_phase == "test" & trial_type == 1) %>%
  summarise(jm_tot_ax_trial = sum(!is.na(trial_number)),
            jm_ax_score = sum(ifelse(is_response_correct == "true", 1, 0)),
            jm_ax_errors = sum(ifelse(is_response_correct == "false", 1, 0)))
jm5 <- jm %>%
  group_by(ID) %>%
  filter(experimental_phase == "test" & trial_type != 1) %>%
  summarise(jm_tot_else_trial = sum(!is.na(trial_number)),
            jm_else_score = sum(ifelse(is_response_correct == "true", 1, 0)),
            jm_else_errors = sum(ifelse(is_response_correct == "false", 1, 0)))
jm6 <- jm %>%
  group_by(ID) %>%
  filter(experimental_phase == "test" & trial_type == 1 & is_response_correct == "true") %>%
  summarise(jm_ax_meanrt_correct = mean(response_reaction_time, na.rm = TRUE))
jm7 <- jm %>%
  group_by(ID) %>%
  filter(experimental_phase == "test" & trial_type == 1 & is_response_correct == "false") %>%
  summarise(jm_ax_meanrt_incorrect = mean(response_reaction_time, na.rm = TRUE))
jm8 <- jm %>%
  group_by(ID) %>%
  filter(experimental_phase == "test" & trial_type != 1 & is_response_correct == "true") %>%
  summarise(jm_else_meanrt_correct = mean(response_reaction_time, na.rm = TRUE))
jm9 <- jm %>%
  group_by(ID) %>%
  filter(experimental_phase == "test" & trial_type != 1 & is_response_correct == "false") %>%
  summarise(jm_else_meanrt_incorrect = mean(response_reaction_time, na.rm = TRUE))

jm <- merger(list(jm1, jm2, jm3, jm4, jm5, jm6, jm7, jm8, jm9))
jm$jm_drt_ax <- jm$jm_ax_meanrt_correct - jm$jm_else_meanrt_correct

# Los calculos de los indices de proactividad que vimos en los papers van sobre distitos tipos de ensayos

# jm$jm_pbi_er <- (jm$ec_2 - jm$ec_3) / (jm$ec_2 + jm$ec_3)
# jm$jm_pbi_rt <- (jm$jm_meanrt_2 - jm$jm_meanrt_3) / (jm$jm_meanrt_2 + jm$jm_meanrt_3)
# jm$jm_d <- scale(jm$jm_score_1 / jm$jm_tot_trial_1) - scale(jm$ec_3)


#export
nuxe <- merger(list(tb, ne, mt, bcd, bci, sm, cs, jm))

nuxe %>% write.csv("data/raw/nux.csv", row.names = FALSE)

