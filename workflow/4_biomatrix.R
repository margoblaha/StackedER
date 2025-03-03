library(dplyr)

#---- 12. Assemble training dataset ----
wda <- readRDS("~/Dati/Output/climatic_covariates.RDS")

bio.matrix <- bio.matrix %>%
  left_join(wda, by = "join.id")  %>%
  rename(ID = Codice) %>%
  dplyr::select( -ID.y, -join.id) %>% 
  rename(ID = ID.x)

# remove all duplicate columns

names(bio.matrix)
summary(bio.matrix)

saveRDS(bio.matrix, file = "~/Dati/Output/bio_matrix.RDS")

table(lubridate::year(bio.matrix$date))
table(bio.matrix$Region)

set.seed(123)

bio.matrix$Region <- zoo::na.locf(bio.matrix$Region, na.rm = FALSE, fromLast = TRUE) # We only have ER data in this dataset
bio.matrix$Country <- zoo::na.locf(bio.matrix$Country, na.rm = FALSE, fromLast = TRUE) # We only have Italy data in this dataset
