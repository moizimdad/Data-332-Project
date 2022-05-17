library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(readr)
library(xlsx)
library(ggplot2)
library(ggthemes)
library(ggpmisc)
library(ggpubr)
library(maps)
library(usmap)

rm(list = ls())
setwd("~/R Projects/Data-332-Project/cabbage_butterfly-main/data")

## Importing the clean and raw datasets
df_clean <- read_excel("Cleaned Data LWA .xlsx") %>%
  dplyr::rename("coreid" = 'core ID')
df_raw <- read_excel("CompletePierisData_2022-03-09.xlsx") %>%
  dplyr::rename('Country' = "dwc:country") %>%
  dplyr::rename("ScientificName" = "dwc:scientificName")%>%
  dplyr::rename("RAnteriorSpot" = "RAnteriorSpotM3")%>%
  dplyr::rename("LAnteriorSpot" = "LAnteriorSpotM3")%>%
  dplyr::rename("StateOrProvince" = "dwc:stateProvince")%>%
  dplyr::rename("Latitude" = "dwc:decimalLatitude")%>%
  dplyr::rename("Date" = "dwc:eventDate")%>%
  dplyr::rename("Day" = "dwc:day")%>%
  dplyr::rename("Month" = "dwc:month")%>%
  dplyr::rename("Longitude" = "dwc:decimalLongitude")

## Creating a Date variable by concatenating the YMD values then using ymd() to change it into a Date format
df_raw$Date = paste(df_raw$YearUpdated, df_raw$Month, df_raw$Day, sep = "-")
df_raw$Date = ymd(df_raw$Date)

## Cleaning and preparing some of the columns and joining them with the clean data by coreid
df <- df_raw %>%
  dplyr::mutate(Country = ifelse((Country == 'U.S.A.') | (Country == 'USA'),
                                 "United States", Country)) %>%
  dplyr::mutate(SexUpdated = ifelse((SexUpdated == 'F') | (SexUpdated == 'F?') |
                                      (SexUpdated == 'female'), 'Female', SexUpdated)) %>%
  dplyr::mutate(SexUpdated = ifelse((SexUpdated=='M')|(SexUpdated == 'male?') |
                                      (SexUpdated == 'male'),'Male',SexUpdated)) %>%
  left_join(df_clean, by = c("coreid")) %>%
  dplyr::select('coreid', "Date","Day", "Month", "YearUpdated", 'Country',
                "StateOrProvince", "LAnteriorSpot", "RAnteriorSpot", 'Longitude', "Latitude")

## Final clean data with average values for length, width, and apex area for each wing
df_1 = df_clean %>%
  left_join(df, by = c('coreid')) %>%
  mutate(AverageWingLength = (`LW length` + `RW length`)/2)%>%
  mutate(AverageWingWidth = (`LW width` + `RW width`)/2)%>%
  mutate(AverageApexArea = (`LW apex A` + `RW apex A`)/2)%>%
  mutate(AverageAnteriorSpotA = (as.numeric(`LAnteriorSpot`) +
                                   as.numeric(`RAnteriorSpot`))/2)%>%
  slice(-c(51))


## Writing the final data-set into excel
#write.xlsx(df_1, file = "final_clean.xlsx")

#plots comparing sex with average wing length, width, and apex area by decade
wing_length_plot = df_1 %>%
  ggplot(aes(x = Date, y = AverageWingLength, color = sex)) +
  geom_line(aes(linetype = sex), size = 1.5, alpha = 0.8) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  labs(title = "Average Wing Length of Insects Overtime",
       subtitle = "How is wing length affected by sex over decades?",
       x = "Year (Decades)",
       y = "Average Wing Length",
       color = "sex") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  scale_linetype_manual(values = c("dashed", "solid"))+
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", 
             vjust = -0.5, x.label.fmt = "%Y") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%Y")



wing_width_plot = df_1 %>%
  ggplot(aes(x = Date, y = AverageWingWidth, color = sex)) +
  geom_point() +
  geom_smooth(aes(linetype = sex), method = lm, se = FALSE, fullrange = TRUE) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  labs(title = "Average Wing width of Insects Overtime",
       subtitle = "How is wing width affected by sex over decades?",
       x = "Year (Decades)",
       y = "Average Wing width",
       color = "sex") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text())
  # stat_peaks(geom = "text", colour = "red",
  #            vjust = -0.5, x.label.fmt = "%Y") +
  # stat_valleys(geom = "text", colour = "blue", angle = 45,
  #              vjust = 1.5, hjust = -1,  x.label.fmt = "%Y")

apex_area_plot = df_1 %>%
  ggplot(aes(x = Date, y = AverageApexArea, color = sex, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge")+ theme_minimal()+
  geom_smooth(method = glm, se = FALSE, fullrange = TRUE)+
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  labs(title = "Average Apex Area of Insects Overtime",
       subtitle = "How is apex area affected by sex over decades?",
       x = "Year (Decades)",
       y = "Average Apex Area",
       color = "sex") +
  theme_classic()
  
combined_plot = ggarrange(wing_length_plot, wing_width_plot, apex_area_plot,
                          ncol = 2, nrow = 2)
combined_plot


## Plot for mapping 
world <- map_data("world")
world_plot = ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightyellow", size = 0.1) +
  geom_point(
    data = df_1,
    aes(x = Longitude, y = Latitude,
        color = sex),
    alpha = 6) +
  labs(x = NULL, y = NULL, color = NULL)+
  theme_void() +
  theme(legend.position = "none")+
  labs(title="Insect Locations")


##  USA plot
df_usa = df_1 %>%
  dplyr::filter(Country == "United States")

usa = map_data("state")
usa_plot = ggplot() +
  geom_map(
    data = usa, map = usa,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightyellow", size = 0.1)+
  geom_point(
    data = df_usa,
    aes(x = Longitude, y = Latitude,
        color = sex),
    alpha = 5, size = 4) +
  labs(x = NULL, y = NULL, color = NULL)+
  theme_void() +
  theme(legend.position = "none")+
  labs(title="Insect Locations In the United States")
 
ggarrange(world_plot, usa_plot,
          ncol = 1, nrow = 2) 

## Wing Length, Wing Width, Apex Area, and Anterior Spot Area by Country
InsectMeasurments = c(df_1$AverageWingLength, df_1$AverageWingWidth, df_1$AverageApexArea, df_1$AverageAnteriorSpotA)
country_plot = df_1%>%
  gather(InsectMeasurments, Length, 19:22)%>%
  group_by(Country, InsectMeasurments)%>%
  summarise(mnl = round(mean(Length), digits=3), sdl = round(sd(Length), digits=3))%>%
  ggplot(aes(reorder(Country, mnl),mnl, fill = reorder(InsectMeasurments,mnl))) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(mapping = aes(label=mnl),position = position_dodge(width = 0.9),
            cex = 2.5, vjust = -4)+
  labs(title = "Insect Wing Measurments by Country",
       subtitle = "Wing Length, Width, and Area grouped by Countries",
       x = "Country", y = "Length in mm",
       caption = "Data: cabbage-butterfly dataset", fill = "Description: ")+# check for unit here
  geom_errorbar(mapping = aes(ymin = round(mnl-sdl, digits = 3), ymax = mnl+sdl),
                width = 0.2, position = position_dodge(width = 0.9))+
  theme_bw()
  


country_plot  
  
  
#Ho: mu < 10
# one-sided 95% confidence interval for mu
#AverageApexArea (the variable)


# paired sample t-test for apex area and sex
apex_area_t_test = t.test(df_1$AverageApexArea ~ df_1$sex,data = df_1, var.equal = TRUE)
apex_area_t_test

#paired sample t-test for wing length and sex
wing_length_t_test = t.test(df_1$AverageWingLength ~ df_1$sex, data = df_1, var.equal = TRUE)
wing_length_t_test

