library(readxl)
library(dplyr)

#--------------------
# Crozier
#--------------------

Crozier2005 <- read_excel("auxiliary/Figure1.xlsx") %>%
  rename(session = `...1`,
         score = `Series 1`) %>%
  mutate(phase = c(rep("A1", 6), rep("B1", 6), rep("A2", 6), rep("B2", 6))) %>%
  select(session, phase, score) %>%
  as.data.frame()

str(Crozier2005)
save(Crozier2005, file = "data/Crozier2005.RData", compress = TRUE, version = 2)

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
  select(behavior, session, phase, score)%>%
  as.data.frame() %>%
  mutate(Include = 0,
         Include = ifelse(behavior == "Segmenting" & session %in% c(1:8,14:18), 1, 0),
         Include = ifelse(behavior == "First Part ID" & session %in% c(1:11,14:18), 1, Include),
         Include = ifelse(behavior == "Blends", 1, Include),
         Include = ifelse(behavior == "First Sound ID", 1, Include))

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
  mutate(case = "Sue",
         phase = c(rep("A", 5), rep("B", 7)))

english_1997_2 <- read_excel("auxiliary/Figure3b.xlsx") %>%
  select(Column1, Column2) %>%
  filter(!is.na(Column2)) %>%
  rename(session = Column1,
         score = Column2) %>%
  mutate(case = "Don",
         phase = c(rep("A", 6), rep("B", 6)))

english_1997_3 <- read_excel("auxiliary/Figure3c.xlsx") %>%
  select(Column1, Column2) %>%
  filter(!is.na(Column2)) %>%
  rename(session = Column1,
         score = Column2) %>%
  mutate(case = "Jake",
         phase = c(rep("A", 11), rep("B", 6)))

english_1997_4 <- read_excel("auxiliary/Figure3d.xlsx") %>%
  select(Column1, Column2) %>%
  filter(!is.na(Column2)) %>%
  rename(session = Column1,
         score = Column2) %>%
  mutate(case = "Pete",
         phase = c(rep("A", 10), rep("B", 8)))

English1997 <- rbind(english_1997_1, english_1997_2, english_1997_3, english_1997_4) %>%
  select(case, session, phase, score)%>%
  as.data.frame()

str(English1997)
save(English1997, file = "data/English1997.RData", compress = TRUE, version = 2)

#--------------------
# Facon
#--------------------

Facon2008 <- read_excel("auxiliary/Figure4.xlsx") %>%
  rename(session = `...1`,
         score = `Series 1`) %>%
  mutate(phase = c(rep("A", 4), rep("B", 3), rep("C", 7), rep("D", 5), rep("E", 4), rep("F", 8), rep("G", 3), rep("H", 9), rep("I", 6))) %>%
  select(session, phase, score) 

A <- "N/A"
B <- 43
C <- as.numeric(Facon2008 %>%
                  filter(phase == "B") %>%
                  arrange(desc(score)) %>%
                  top_n(5) %>%
                  summarise(mean(score)))
D <- as.numeric(Facon2008 %>%
                  filter(phase == "C") %>%
                  arrange(desc(score)) %>%
                  top_n(5) %>%
                  summarise(mean(score)))
E <- as.numeric(Facon2008 %>%
                  filter(phase == "D") %>%
                  arrange(desc(score)) %>%
                  top_n(5) %>%
                  summarise(mean(score)))
F <- as.numeric(Facon2008 %>%
                  filter(phase == "E") %>%
                  arrange(desc(score)) %>%
                  top_n(5) %>%
                  summarise(mean(score)))
G <- as.numeric(Facon2008 %>%
                  filter(phase == "F") %>%
                  arrange(desc(score)) %>%
                  top_n(5) %>%
                  summarise(mean(score)))
H <- as.numeric(Facon2008 %>%
                  filter(phase == "G") %>%
                  arrange(desc(score)) %>%
                  top_n(5) %>%
                  summarise(mean(score)))
I <- as.numeric(Facon2008 %>%
                  filter(phase == "H") %>%
                  arrange(desc(score)) %>%
                  top_n(5) %>%
                  summarise(mean(score)))

Facon2008 <- Facon2008 %>%
  mutate(criterion = recode(phase, "A" = NA_real_, "B" = B, "C" = C, "D" = D, "E" = E, "F" = F, "G" = G, "H" = H, "I" = I)) %>%
  as.data.frame()

str(Facon2008)
save(Facon2008, file = "data/Facon2008.RData", compress = TRUE, version = 2)

#--------------------
# Spencer
#--------------------

Spencer2012 <- read_excel("auxiliary/PoGO Data_to send.xlsx", sheet = 5) %>%
  as.data.frame()

str(Spencer2012)
save(Spencer2012, file = "data/Spencer2012.RData", compress = TRUE, version = 2)

#--------------------
# Kelley
#--------------------
 
Kelley2015 <- read_excel("auxiliary/PoGO Data_to send.xlsx", sheet = 7)

treatment <- Kelley2015 %>% 
  select(Treatment, `...2`, `...3`, `...4`) %>%
  rename(observation = Treatment,
         unit = `...2`, 
         pre = `...3`,
         post = `...4`) %>%
  filter(observation != "Observation") %>%
  mutate(condition = "treatment")

control <- Kelley2015 %>%
  select(Control, `...7`, `...8`, `...9`) %>%
  rename(observation = Control,
         unit = `...7`, 
         pre = `...8`,
         post = `...9`) %>%
  filter(observation != "Observation") %>%
  mutate(condition = "control")

Kelley2015 <- rbind(treatment, control) %>%
  select(condition, observation, unit, pre, post) %>%
  as.data.frame()

str(Kelley2015)
save(Kelley2015, file = "data/Kelley2015.RData", compress = TRUE, version = 2)

#--------------------
# Peters
#--------------------

Peters2020 <- read_excel("auxiliary/PoGO Data_to send.xlsx", sheet = 12) %>%
  as.data.frame()

str(Peters2020)
save(Peters2020, file = "data/Peters2020.RData", compress = TRUE, version = 2)

#--------------------
# Dennis
#--------------------

Dennis2021 <- read_excel("auxiliary/PoGO Data_to send.xlsx", sheet = 15) %>%
  as.data.frame()

str(Dennis2021)
save(Dennis2021, file = "data/Dennis2021.RData", compress = TRUE, version = 2)



