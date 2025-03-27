#========================================= Q1 ===================================================
# Extracting Ethiopia data
Ethiopia_folate <- dbGetQuery(MIMI_DB, "
  SELECT hhid, folate, zinc
  FROM targets
  WHERE country = 'Ethiopia'
")

# Scatter plot
ggplot(Ethiopia_folate, aes(x = folate, y = zinc)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  labs(title = "Scatter Plot of Folate vs Zinc in Ethiopia",
       x = "Folate Levels",
       y = "Zinc Levels") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  
    plot.title = element_text(hjust = 0.5)  
  )

#========================================== Q2 ==================================================
# Extracting Senegal data
Senegal_data <- dbGetQuery(MIMI_DB, "
  SELECT t.hhid, t.folate, h.education, h.age
  FROM targets t
  JOIN hh_info h ON t.country = h.country AND t.hhid = h.hhid
  WHERE t.country = 'Senegal'
")

# Violin plot
ggplot(Senegal_data, aes(x = factor(education), y = folate)) +
  geom_violin(fill = "pink") +
  labs(title = "Distribution of Folate Levels by Education Status in Senegal",
       x = "Education Status",
       y = "Folate Levels") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

