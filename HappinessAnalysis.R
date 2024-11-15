# Load required libraries
library(dplyr)
library(ggplot2)
library(corrplot)

data <- read.csv("report.csv")


# 1. Calculate year-over-year changes ---------
happiness_changes <- data %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    happiness_change = Happiness.Score - lag(Happiness.Score),
    gdp_change = GDP.per.capita - lag(GDP.per.capita),
    freedom_change = Freedom - lag(Freedom),
    social_support_change = Social.support - lag(Social.support),
    health_change = Health - lag(Health),
    trust_change = Trust - lag(Trust),
    generosity_change = Generosity - lag(Generosity)
  ) %>%
  filter(!is.na(happiness_change))

happiness_changes_pct <- data %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    happiness_change = (Happiness.Score - lag(Happiness.Score)) / lag(Happiness.Score),
    gdp_change = (GDP.per.capita - lag(GDP.per.capita)) / lag(GDP.per.capita),
    freedom_change = Freedom - lag(Freedom),
    social_support_change = Social.support - lag(Social.support),
    health_change = Health - lag(Health),
    trust_change = Trust - lag(Trust),
    generosity_change = Generosity - lag(Generosity)
  ) %>%
  filter(!is.na(happiness_change))


# 2. Basic Linear Regression
basic_model <- lm(
  happiness_change ~ gdp_change + freedom_change + social_support_change + 
    health_change + trust_change + generosity_change,
  data = happiness_changes %>% filter(Regions == unique(happiness_changes$Regions)[2])
)

# Print summary of the model
summary(basic_model)

# 3. Analysis by Region
regional_analysis <- happiness_changes %>%
  group_by(Regions) %>%
  summarise(
    mean_happiness_change = mean(happiness_change, na.rm = TRUE),
    mean_gdp_change = mean(gdp_change, na.rm = TRUE),
    correlation = cor(happiness_change, gdp_change, use = "complete.obs")
  )

regional_analysis_pct <- happiness_changes_pct %>%
  group_by(Regions) %>%
  summarise(
    mean_happiness_change = mean(happiness_changes_pct, na.rm = TRUE),
    mean_gdp_change = mean(gdp_change, na.rm = TRUE),
    correlation = cor(happiness_change, gdp_change, use = "complete.obs")
  )

# 4. Time Trend Analysis
time_trends <- happiness_changes %>%
  group_by(Year) %>%
  summarise(
    mean_happiness = mean(Happiness.Score, na.rm = TRUE),
    mean_gdp = mean(GDP.per.capita, na.rm = TRUE)
  )



# 5. Correlation Matrix (General) -----------------------
correlation_matrix <- happiness_changes %>%
  ungroup() %>%  
  select(happiness_change, gdp_change, freedom_change, 
         social_support_change, health_change, trust_change, 
         generosity_change) %>%
  cor(use = "complete.obs")

correlation_matrix_rounded <- round(correlation_matrix, 3)


corrplot(correlation_matrix, 
         method = "color", 
         type = "upper", 
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7)

# 5.1 Correlation Matrix (Happiness Only) -----------------------


happiness_cors <- happiness_changes %>%
  ungroup() %>%
  select(happiness_change, gdp_change, freedom_change, 
         social_support_change, health_change, trust_change, 
         generosity_change) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  select(happiness_change) %>%
  round(3)

# Plotting happiness correlations only
corrplot(as.matrix(happiness_cors),
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         # Legend improvements
         cl.ratio = 0.2,          # Make color legend wider
         cl.align = "l",          # Align color legend to the left
         cl.length = 5,           # Number of breaks in color legend
         cl.cex = 0.8,           # Size of legend text
        # Another option
)


# Function to create happiness correlations for a specific region
create_region_happiness_cors <- function(data, region_name) {
  data %>%
    filter(Regions == region_name) %>%
    ungroup() %>%
    select(happiness_change, gdp_change, freedom_change, 
           social_support_change, health_change, trust_change, 
           generosity_change) %>%
    cor(use = "complete.obs") %>%
    as.data.frame() %>%
    select(happiness_change) %>%
    round(3)
}

# Get unique regions
regions <- unique(happiness_changes$Regions)

# Create plots for each region
par(mfrow = c(3, 3), mar = c(4, 4, 4, 4))  # Adjust layout for multiple plots

europe_cors <- create_region_happiness_cors(happiness_changes, "Europe")
corrplot(as.matrix(europe_cors),
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         cl.ratio = 0.2,
         cl.align = "l",
         cl.length = 5,
         cl.cex = 0.8,
         title = "Europe")

# Sub-Saharan Africa
africa_cors <- create_region_happiness_cors(happiness_changes, "Sub-Saharan Africa")
corrplot(as.matrix(africa_cors),
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         cl.ratio = 0.2,
         cl.align = "l",
         cl.length = 5,
         cl.cex = 0.8)

# North America
na_cors <- create_region_happiness_cors(happiness_changes, "North America")
corrplot(as.matrix(na_cors),
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         cl.ratio = 0.2,
         cl.align = "l",
         cl.length = 5,
         cl.cex = 0.8)

# Oceania
oceania_cors <- create_region_happiness_cors(happiness_changes, "Oceania")
corrplot(as.matrix(oceania_cors),
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         cl.ratio = 0.2,
         cl.align = "l",
         cl.length = 5,
         cl.cex = 0.8)

# South America
sa_cors <- create_region_happiness_cors(happiness_changes, "South America")
corrplot(as.matrix(sa_cors),
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         cl.ratio = 0.2,
         cl.align = "l",
         cl.length = 5,
         cl.cex = 0.8)

# Central America & Caribbean
caribbean_cors <- create_region_happiness_cors(happiness_changes, "Central America & Caribbean")
corrplot(as.matrix(caribbean_cors),
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         cl.ratio = 0.2,
         cl.align = "l",
         cl.length = 5,
         cl.cex = 0.8)

# Middle East & North Africa
mena_cors <- create_region_happiness_cors(happiness_changes, "Middle East & North Africa")
corrplot(as.matrix(mena_cors),
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         cl.ratio = 0.2,
         cl.align = "l",
         cl.length = 5,
         cl.cex = 0.8)

# Asia
asia_cors <- create_region_happiness_cors(happiness_changes, "Asia")
corrplot(as.matrix(asia_cors),
         method = "color",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         cl.ratio = 0.2,
         cl.align = "l",
         cl.length = 5,
         cl.cex = 0.8,
         title = "Asia")



# Reset plot layout
par(mfrow = c(1, 1))

#---------------------------------------------------------------------------------



#5.2 Correlation Matrix (General, Regional)
# Now let's create correlation matrices by region
# Function to create correlation matrix for a specific region
create_region_corr <- function(data, region_name) {
  data %>%
    filter(Regions == region_name) %>%
    ungroup() %>%
    select(happiness_change, gdp_change, freedom_change, 
           social_support_change, health_change, trust_change, 
           generosity_change) %>%
    cor(use = "complete.obs") %>%
    round(3)
}

# Get unique regions
regions <- unique(happiness_changes$Regions)

# Create a list of correlation matrices for each region
region_correlations <- lapply(regions, function(r) {
  create_region_corr(happiness_changes, r)
})

# Plot correlation matrix for each region
par(mfrow = c(3, 3), mar = c(2, 2, 2, 2))  # Adjust layout for multiple plots

for(i in seq_along(regions)) {
  corrplot(region_correlations[[i]],
           method = "color",
           type = "upper",
           order = "hclust",
           tl.col = "black",
           tl.srt = 45,
           addCoef.col = "black",
           number.cex = 0.5,
           title = regions[i],
           mar = c(0,0,1,0))
}

# Reset plot layout
par(mfrow = c(1, 1))

#Happiness change by region----------------


# You can also view individual regions:
# Example for Europe
europe_cors <- create_region_corr(happiness_changes, "Europe")
corrplot(europe_cors,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         title = "Europe Correlations")



# 6. Visualizations

# GDP vs Happiness Change
plot1 <- ggplot(happiness_changes, aes(x = gdp_change, y = happiness_change)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Relationship between GDP Change and Happiness Change",
       x = "Change in GDP per capita",
       y = "Change in Happiness Score") +
  theme_minimal() +
  facet_wrap(vars(Regions), scales='free')

x = c('Germany', 'France', 'Spain', 'United_Kingdom', 'Italy')
plot11 <- ggplot(happiness_changes_pct %>% filter(Country %in% x), aes(x = gdp_change, y = happiness_change)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Relationship between GDP Change and Happiness Change",
       x = "Change in GDP per capita",
       y = "Change in Happiness Score") +
  theme_minimal() +
  facet_wrap(vars(Regions), scales='free_x')


# Time trends plot
plot2 <- ggplot(time_trends, aes(x = Year)) +
  geom_line(aes(y = scale(mean_happiness), color = "Happiness")) +
  geom_line(aes(y = scale(mean_gdp), color = "GDP")) +
  labs(title = "Standardized Trends in Average Happiness and GDP",
       y = "Standardized Values",
       color = "Metric") +
  theme_minimal()

# 7. Country-specific analysis
country_summary <- happiness_changes %>%
  group_by(Country) %>%
  summarise(
    avg_happiness_change = mean(happiness_change, na.rm = TRUE),
    avg_gdp_change = mean(gdp_change, na.rm = TRUE),
    correlation = cor(happiness_change, gdp_change, use = "complete.obs")
  ) %>%
  arrange(desc(abs(correlation)))

# 8. High/Low GDP Impact Analysis
happiness_changes <- happiness_changes %>%
  mutate(
    gdp_impact = case_when(
      gdp_change > median(gdp_change, na.rm = TRUE) ~ "High",
      TRUE ~ "Low"
    )
  )

gdp_impact_analysis <- t.test(
  happiness_change ~ gdp_impact, 
  data = happiness_changes
)

# 9. Regional Visualization
plot3 <- ggplot(regional_analysis, 
                aes(x = mean_gdp_change, y = mean_happiness_change, label = Regions)) +
  geom_point() +
  geom_text(hjust = -0.1) +
  labs(title = "Regional Patterns in GDP and Happiness Changes",
       x = "Mean GDP Change",
       y = "Mean Happiness Change") +
  theme_minimal()

# Function to print key findings
print_analysis <- function() {
  cat("\n=== Basic Model Results ===\n")
  print(summary(basic_model))
  
  cat("\n=== Regional Analysis ===\n")
  print(regional_analysis)
  
  cat("\n=== Top 10 Countries by GDP-Happiness Correlation ===\n")
  print(head(country_summary, 10))
  
  cat("\n=== GDP Impact Analysis ===\n")
  print(gdp_impact_analysis)
  
  cat("\n=== Correlation Matrix ===\n")
  print(round(correlation_matrix, 3))
}

# Display plots and analysis
print_analysis()
print(plot1)
print(plot2)
print(plot3)
