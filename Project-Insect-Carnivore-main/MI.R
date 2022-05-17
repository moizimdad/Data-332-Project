library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(tidytext)
library(hrbrthemes)
library(viridis)


setwd("C:/Users/estif/OneDrive/Documents/Data 332 Project/Project-Insect-Carnivore-main/data")
df <- read_excel ("Ladybug Data.xlsx", sheet = 1)
df2 <- read.csv("Scan Ladybug Data.csv")

colnames(df)[8] <- "SCAN"
colnames(df2)[8] <- "SCAN"


combined <- left_join(df, df2 %>% dplyr::select(SCAN, genus),
                           by = "SCAN")


## Cleaning Data
df <- df %>%
  dplyr::mutate(Species = ifelse((Species == 'Coccinella semtempuncata') | (Species == 'coccinella semtempuncata') | (Species == 'coccinella septempunctata') | (Species == 'Coccinella septempunctata') , 'Coccinella Septempunctata', Species))%>%
  dplyr::mutate(Species = ifelse((Species == 'Colemegilla maculata') | (Species == 'coleomegilla maculata') , "Coleomegilla maculata", Species)) %>%
  dplyr::mutate(Species = ifelse((Species == 'cycloneda munda') | (Species == 'Cycloneda Munda') , "Cycloneda munda", Species)) %>%
  dplyr::mutate(Species = ifelse((Species == 'Harminia axyridis') | (Species == 'harmonia axyrids') | (Species == 'Harmonia axyrisis') | (Species == 'harmonia axyridis'), "Harmonia axyridis", Species)) %>%
  dplyr::mutate(Species = ifelse((Species == 'hippodamia convergens') | (Species == 'Hippodamia covergence') , "Hippodamia convergens", Species)) %>%
  dplyr::mutate(Species = ifelse((Species == 'hippodamia parenthesis') , "Hippodamia parenthesis", Species)) %>%
  dplyr::mutate(Species = ifelse((Species == 'Propylea quatuordecimpuncata'), 'Propylea quatuordecimpunctata', Species)) %>%
  
  dplyr::mutate(collector = ifelse((collector == 'J Hughes') | (collector == 'J. Hughees') | (collector == 'j. hughes') | (collector == 'j. Hughes') | (collector == 'J. hughes') | (collector == 'jack hughes') | (collector == 'J. Hughes') , 'Jack Hughes', collector))%>%
  dplyr::mutate(collector = ifelse((collector == 'm gorsegner') | (collector == 'M. Gorsegner') | (collector == 'm. gorsegner') | (collector == 'M. gorsegner') | (collector == 'M.Gorsegner') , 'Marissa Gorsegner', collector))%>%
  dplyr::mutate(collector = ifelse((collector == 'O. Ruffatto') | (collector == 'o. ruffattto') | (collector == 'o. ruffatto') | (collector == 'O. ruffatto') | (collector == 'OliviaRuffatto') , 'Olivia Ruffatto', collector))%>%
  dplyr::mutate(collector = ifelse((collector == 'v cervantes') | (collector == 'V. Cervantes') | (collector == 'v. cervantes') | (collector == 'V. cervantes') | (collector == 'V.Cervantes') | (collector == 'Veronica Cervatnes') , 'Veronica Cervantes', collector))%>%
  
  dplyr::mutate(plot = ifelse((plot == 'Lp-PR-5') , 'LP-PR-5', plot))%>%
  
  dplyr::select('Species', 'plot','A_E_V', 'date',  'coordinates', 'collector', 'identifier', 'SCAN')

##Species with their count included
df_species <- df %>%
  dplyr:: count(Species)

df_collector <- df %>%
  dplyr:: count(collector)


### Exploring Data
summary(df)

#view(ladybug_clean)
head(df)


#plot
summary(df$Species)

#Editing value to make easy assumptions
df$plot <- str_replace_all(df$plot, c("-1" = "", "-2" = "", "-3" = "", "-4" = "", "-5" = ""))

# Species Graph
df_species_count <- df %>%
  count(Species, plot)

df_species_count %>%
  ggplot(aes(x = Species, y= n , fill = plot )) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ plot) +
  labs(x="Species", y = "Count")

#Species by 'collector'
dfCollector_count <- df %>%
  count(Species, collector)

dfCollector_count %>%
  
  ggplot(aes(x = Species, y= n , fill = collector )) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ collector) +
  labs(x="Species", y = "Count")

