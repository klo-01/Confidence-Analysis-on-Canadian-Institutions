#Loading the required packages
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

#----------------------------------------------------------------xxx------------------------------------------------------
#Loading the csv file
GG <- read_csv("Good Governance-Confidence in Institutions.csv")
GG
#----------------------------------------------------------------xxx------------------------------------------------------
#Data Cleaning


#Splitting the column "indicators" to "institution" and "confidence_level" to segregate them
conf <- GG %>%
  mutate(
    Institution = str_extract(Indicators, 
                              "Confidence in the .*? rating"),
    Institution = str_replace(Institution, 
                              "Confidence in the ", ""),
    Institution = str_replace(Institution, 
                              " rating", ""),
    
    Confidence_Level = case_when(
      str_detect(Indicators, "1 or 2") ~ "Low",
      str_detect(Indicators, "3") ~ "Neutral",
      str_detect(Indicators, "4 or 5") ~ "High"
    )
  ) %>%
  select(-Indicators)


#Fill all the blanks for Gender and Sociodemographics
conf <- conf %>%
  fill(Gender, .direction = "down") %>%
  fill(Sociodemographic, .direction = "down")


#Removing Dulpicates
conf <- conf %>% distinct()


#Check for missing values
colSums(is.na(conf))


#Reorder of the variables
conf_clean <- conf %>%
  select(Gender, Sociodemographic, Institution, Confidence_Level,
         Q4_2023, Q4_2024)


#Inspect the cleaned tibble
conf_clean

#----------------------------------------------------------------xxx------------------------------------------------------
#Confidence in Institutions

analysis1_data <- conf_clean %>%
  filter(str_detect(Gender, "Total"),
         str_detect(Sociodemographic, "Total")) %>%
  select(Institution, Confidence_Level, Q4_2024) %>%
  mutate(Confidence_Level = factor(Confidence_Level,
                                   levels = c("Low", "Neutral", "High")))
ggplot(analysis1_data,
       aes(x = Institution,
           y = Q4_2024,
           fill = Confidence_Level)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Confidence Distribution by Institution",
    x = "Institution",
    y = "Percentage Share",
    fill = "Confidence Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

#----------------------------------------------------------------xxx------------------------------------------------------
#Year-over-Year Change in Confidence

analysis2_data <- conf_clean %>%
  filter(
    Confidence_Level == "High",
    str_detect(Gender, "Total"),
    str_detect(Sociodemographic, "Total")
  ) %>%
  select(Institution, Q4_2023, Q4_2024) %>%
  mutate(
    Q4_2023 = Q4_2023 / 100,
    Q4_2024 = Q4_2024 / 100,
    Change = Q4_2024 - Q4_2023
  ) %>%
  pivot_longer(
    cols = c(Q4_2023, Q4_2024),
    names_to = "Quarter",
    values_to = "Value"
  ) %>%
  mutate(
    Quarter = recode(Quarter,
                     "Q4_2023" = "2023",
                     "Q4_2024" = "2024"),
    Quarter = factor(Quarter, levels = c("2023", "2024"))
  )

# --- Plot ---
ggplot(analysis2_data,
       aes(x = Institution,
           y = Value,
           fill = Quarter)) +
  geom_col(position = position_dodge(width = 0.7)) +
  
  # Add change labels ONLY for 2024 bars
  geom_text(
    data = analysis2_data %>% filter(Quarter == "2024"),
    aes(label = scales::percent(Change, accuracy = 0.1),
        y = Value + 0.02),   # slight nudge to the right
    position = position_dodge(width = 0.7),
    size = 3.5,
    fontface = "bold"
  ) +
  
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(
    "2023" = "#1B9E77",   # teal
    "2024" = "#D95F02"    # orange
  )) +
  labs(
    title = "Year-over-Year Change in Confidence Levels (2023 vs 2024)",
    subtitle = "Labels show percentage-point change from 2023 to 2024",
    x = "Institution",
    y = "Percentage",
    fill = "Quarter"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(),
    axis.text.y = element_text(face = "bold"),
    legend.position = "top"
  )


#----------------------------------------------------------------xxx------------------------------------------------------
# Confidence across Age

analysis3_age <- conf_clean %>%
  filter(
    Confidence_Level == "High",
    str_detect(Gender, "Total"),
    str_detect(Sociodemographic, "\\d"),      # keeps age groups
    !str_detect(Sociodemographic, "Total")    # removes total age categories
  ) %>%
  mutate(
    Age_Group = Sociodemographic,
    Q4_2024 = Q4_2024 / 100                   # convert to proportion
  ) %>%
  group_by(Age_Group) %>%                     # <-- KEY FIX
  summarise(
    High_Conf = mean(Q4_2024, na.rm = TRUE)   # one value per age group
  ) %>%
  ungroup()

analysis3_age <- analysis3_age %>%
  mutate(
    Age_Group = case_when(
      Age_Group == "15 to 24 years" ~ "15-24",
      Age_Group == "25 to 54 years" ~ "25-54",
      Age_Group == "55 to 64 years" ~ "55-64",
      Age_Group == "65 years and over" ~ "65+",
      TRUE ~ Age_Group
    )
  )


ggplot(analysis3_age,
       aes(x = Age_Group,
           y = High_Conf,
           group = 1)) +                      # ensures ONE line
  geom_col(fill = "#7570B3") +
  
  # Trend line connecting bar tops
  geom_line(color = "#1B9E77", size = 1.2) +
  geom_point(color = "#1B9E77", size = 3) +
  
  # Labels on top of bars
  geom_text(
    aes(label = scales::percent(High_Conf, accuracy = 0.1)),
    vjust = -0.3,
    size = 4,
    fontface = "bold"
  ) +
  
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Confidence Levels by Age Group",
    subtitle = "Bars show percentages; line shows trend across age groups",
    x = "Age Group",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold", vjust = 0.5)
  )

#----------------------------------------------------------------
# Confidence Level between the Immigration groups

analysis_imm <- conf_clean %>%
  filter(
    str_detect(Gender, "Total"),
    Confidence_Level %in% c("Low", "Neutral", "High"),
    str_detect(Sociodemographic, "Immigr") | str_detect(Sociodemographic, "Non")
  ) %>%
  mutate(
    Immigration_Status = case_when(
      str_detect(Sociodemographic, "Non") ~ "Non-Immigrant",
      str_detect(Sociodemographic, "Immigr") ~ "Immigrant"
    ),
    Q4_2024 = Q4_2024 / 100
  ) %>%
  group_by(Immigration_Status, Confidence_Level) %>%   # <-- KEY FIX
  summarise(
    Value = mean(Q4_2024, na.rm = TRUE)               # one value per bar
  ) %>%
  ungroup()

# Order confidence levels
analysis_imm$Confidence_Level <- factor(
  analysis_imm$Confidence_Level,
  levels = c("Low", "Neutral", "High")
)

ggplot(analysis_imm,
       aes(x = Confidence_Level,
           y = Value,
           fill = Immigration_Status)) +
  geom_col(position = position_dodge(width = 0.9)) +
  
  # One label per bar
  geom_text(
    aes(label = scales::percent(Value, accuracy = 0.1)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 4,
    fontface = "bold"
  ) +
  
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0.1))
  ) +
  
  # Your project palette
  scale_fill_manual(values = c(
    "Immigrant" = "#1B9E77",
    "Non-Immigrant" = "#D95F02"
  )) +
  
  labs(
    title = "Confidence Levels by Immigration Status",
    x = "Confidence Level",
    y = "Percentage",
    fill = "Immigration Status"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )

#----------------------------------------------------------------
#Confidence by Age and Area of Residence 

# --- Data Prep ---
analysis_gender_area <- conf_clean %>%
  filter(
    Confidence_Level %in% c("Low", "Neutral", "High"),
    str_detect(Gender, "Men|Women"),
    str_detect(Sociodemographic, "Urban|Rural")
  ) %>%
  mutate(
    Gender = ifelse(str_detect(Gender, "Men"), "Men", "Women"),
    Area = ifelse(str_detect(Sociodemographic, "Urban"), "Urban", "Rural"),
    Q4_2024 = Q4_2024 / 100
  ) %>%
  group_by(Gender, Area, Confidence_Level) %>%     # <-- KEY: one row per bar
  summarise(
    Value = mean(Q4_2024, na.rm = TRUE)
  ) %>%
  ungroup()

# Order confidence levels
analysis_gender_area$Confidence_Level <- factor(
  analysis_gender_area$Confidence_Level,
  levels = c("Low", "Neutral", "High")
)

# --- Function to generate each chart ---
plot_gender_area <- function(gender_name) {
  
  df <- analysis_gender_area %>% filter(Gender == gender_name)
  
  ggplot(df,
         aes(x = Area,
             y = Value,
             fill = Confidence_Level)) +
    geom_col(position = position_dodge(width = 0.9)) +
    
    # Labels
    geom_text(
      aes(label = scales::percent(Value, accuracy = 0.1)),
      position = position_dodge(width = 0.9),
      vjust = -0.3,
      size = 4,
      fontface = "bold"
    ) +
    
    scale_y_continuous(
      labels = scales::percent,
      expand = expansion(mult = c(0, 0.1))
    ) +
    
    # Consistent palette
    scale_fill_manual(values = c(
      "Low" = "#1B9E77",
      "Neutral" = "#D95F02",
      "High" = "#7570B3"
    )) +
    
    labs(
      title = paste("Confidence Levels by Area of Residence (", gender_name, ")", sep = ""),
      x = "Area of Residence",
      y = "Percentage",
      fill = "Confidence Level"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(face = "bold")
    )
}

# --- Generate the two charts ---
plot_gender_area("Men")
plot_gender_area("Women")

