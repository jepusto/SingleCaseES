library(readxl)
library(dplyr)

#--------------------
# Crozier
#--------------------

Crozier2005 <- read_excel("auxiliary/Figure1.xlsx") %>%
  rename(session = `...1`,
         score = `Series 1`) %>%
  mutate(phase = c(rep("A1", 6), rep("B1", 6), rep("A2", 6), rep("B2", 6))) %>%
  select(session, phase, score)

str(Crozier2005)
save(Crozier2005, file = "data/Crozier2005.Rdata", compress = TRUE, version = 2)

#--------------------
# Olszewski
#--------------------

olszewski_2017_blends <- read_excel("auxiliary/Figure2a.xlsx") %>%
  rename(session = `...1`,
         score = `Series 1`) %>%
  mutate(behavior = "Blends",
         phase = c(rep("A", 4), rep("B", 14)))

olszewski_2017_segmenting <- read_excel("auxiliary/Figure2b.xlsx") %>%
  rename(session = `...1`,
         score = `Series 1`) %>%
  filter(session %in% 1:18) %>%
  mutate(behavior = "Segmenting",
         phase = c(rep("A", 8), rep("B", 10)))

olszewski_2017_firstpartid <- read_excel("auxiliary/Figure2c.xlsx") %>%
  rename(session = `...1`,
         score = `Series 1`) %>%
  filter(session %in% 1:18) %>%
  mutate(behavior = "First Part ID",
         phase = c(rep("A", 11), rep("B", 7)))

olszewski_2017_firstsoundid <- read_excel("auxiliary/Figure2d.xlsx") %>%
  rename(session = `...1`,
         score = `Series 1`) %>%
  filter(session %in% 1:18) %>%
  mutate(behavior = "First Sound ID",
         phase = c(rep("A", 13), rep("B", 5)))

Olszewski2017 <- rbind(olszewski_2017_blends, olszewski_2017_segmenting, olszewski_2017_firstpartid, olszewski_2017_firstsoundid) %>%
  select(behavior, session, phase, score)

str(Olszewski2017)
save(Olszewski2017, file = "data/Olszewski2017.RData", compress = TRUE, version = 2)

#--------------------
# English
#--------------------

english_1997_1 <- read_excel("auxiliary/Figure3a.xlsx") %>%
  filter(!is.na(Column2) | !is.na(`...4`)) %>%
  select(Column1, Column2) %>%
  rename(session = Column1,
         score = Column2) %>%
  mutate(case = "1",
         phase = c(rep("A", 5), rep("B", 7)))

english_1997_2 <- read_excel("auxiliary/Figure3b.xlsx") %>%
  select(Column1, Column2) %>%
  filter(!is.na(Column2)) %>%
  rename(session = Column1,
         score = Column2) %>%
  mutate(case = "2",
         phase = c(rep("A", 6), rep("B", 6)))

english_1997_3 <- read_excel("auxiliary/Figure3c.xlsx") %>%
  select(Column1, Column2) %>%
  filter(!is.na(Column2)) %>%
  rename(session = Column1,
         score = Column2) %>%
  mutate(case = "3",
         phase = c(rep("A", 11), rep("B", 6)))

english_1997_4 <- read_excel("auxiliary/Figure3d.xlsx") %>%
  select(Column1, Column2) %>%
  filter(!is.na(Column2)) %>%
  rename(session = Column1,
         score = Column2) %>%
  mutate(case = "4",
         phase = c(rep("A", 10), rep("B", 8)))

English1997 <- rbind(english_1997_1, english_1997_2, english_1997_3, english_1997_4) %>%
  select(case, session, phase, score)

str(English1997)
save(English1997, file = "data/English1997.Rdata", compress = TRUE, version = 2)

