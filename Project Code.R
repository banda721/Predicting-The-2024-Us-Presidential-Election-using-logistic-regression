
# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(maps)

fundamental_data_election <- read.csv("C:/Users/gihan/OneDrive/Gihan/BGSU_MAFE/Books/Grad Econometrics/Project/fundamental_data_election.csv")
Polls <- read.csv("C:/Users/gihan/OneDrive/Gihan/BGSU_MAFE/Books/Grad Econometrics/Polls.csv")
head(Polls,20)

Polls$pct_estimate <- as.numeric(as.character(Polls$pct_estimate))

# Group by 'cycle', 'state', and 'candidate_name', and calculate the mean of 'pct_estimate'
result <- Polls %>%
  group_by(cycle, state, candidate_name,party) %>%
  summarise(mean_pct_estimate = mean(pct_estimate, na.rm = TRUE), .groups = 'drop')
head(result,20)
unique(result$candidate_name)

wide_data_corrected <- result %>%
  select(-candidate_name) %>%  # Remove candidate_name before pivoting if it's no longer needed
  pivot_wider(names_from = party, values_from = mean_pct_estimate, names_prefix = "party_", values_fill = list(mean_pct_estimate = NA)) %>%
  group_by(cycle, state) %>%
  summarise(across(starts_with("party"), ~mean(.x, na.rm = TRUE)), .groups = 'drop')

# View the corrected data
print(head(wide_data_corrected, 20))

wide_data_corrected=subset(wide_data_corrected,select = -party_)

names(fundamental_data_election)
fundamental_data_election = fundamental_data_election %>% 
  rename ("Abb" = "state",
          "state" = "state_name",
          "cycle" = "election_year")

#merging Poll data and other data
Data = merge(fundamental_data_election, wide_data_corrected, by = c("cycle", "state"),all.x = TRUE)
Data = Data %>% 
  rename ("poll_party_Rep" = "party_Rep",
          "poll_party_Dem" = "party_Dem")
unique(Data)
summary(Data)
names(Data)
Data=subset(Data,select = -c(election_date,region))
head(Data)
str(Data)
summary(Data)

# Relace NAs with 0
Data <- Data %>% replace(is.na(.), 0)
head(Data,20)

Data <- Data %>%
  mutate(
    poll_party_Rep = case_when(
      state %in% c("Alaska", "Arizona", "Arkansas", "Georgia", "Idaho", "Louisiana", 
                   "Montana", "Nevada", "North Dakota", "Oklahoma") & 
        poll_party_Rep == 0 ~ 0.55,
      state %in% c("Delaware", "District of Columbia", "Hawaii", "Maine") & 
        poll_party_Rep == 0 ~ 0.41,
      TRUE ~ poll_party_Rep
    ),
    poll_party_Dem = case_when(
      state %in% c("Alaska", "Arizona", "Arkansas", "Georgia", "Idaho", "Louisiana", 
                   "Montana", "Nevada", "North Dakota", "Oklahoma") & 
        poll_party_Dem == 0 ~ 0.41,
      state %in% c("Delaware", "District of Columbia", "Hawaii", "Maine") & 
        poll_party_Dem == 0 ~ 0.55,
      TRUE ~ poll_party_Dem
    )
  )


################################Model################################################

#removing Historicals
historical_data <- Data[Data$cycle != 2024, ]
historical_data$outcome <- ifelse(historical_data$democratic_vote_share_adj > historical_data$republican_vote_share_adj, 1, 0)

# Split into training and test sets (using historical data only)
set.seed(123)
indices <- sample(1:nrow(historical_data), size = 0.7 * nrow(historical_data))
train_data <- historical_data[indices, ]
test_data <- historical_data[-indices, ]
names(test_data)
# Retrain the model using only historical data
model <- glm(outcome ~ PVI + state_unemployment + state_house_price + state_med_income + nat_gdp + net_approval + incumbent + poll_party_Rep + poll_party_Dem, 
             family = binomial(link = "logit"), data = train_data)

#model summary
summary(model)
exp(coefficients(model))
exp(confint(model))
?coefficients
# Retrain the model using only historical data
model1 <- glm(outcome ~ PVI + state_unemployment + state_house_price + state_med_income + nat_gdp 
              + net_approval + incumbent + poll_party_Rep + poll_party_Dem+state_personal_income+
                nat_nonfarm_payroll+nat_price_index+stock_market, 
             family = binomial(link = "logit"), data = train_data)

#model summary
summary(model1)


# Prediction
future_data <- Data[Data$cycle == 2024, ]

# Predict the outcome for 2024
future_data$predicted_probs <- predict(model1, newdata = future_data, type = "response")
future_data$predicted_outcome <- ifelse(future_data$predicted_probs > 0.5, 1, 0)

# Sum the electoral votes predicted for Democrats
future_data$electoral_votes <- Data$electoral_votes[Data$cycle == 2024]
democratic_electoral_votes <- sum(future_data$electoral_votes[future_data$predicted_outcome == 1])

# Determine the predicted winner
if (democratic_electoral_votes > 270) {
  print("Predicted winner: Democratic Party")
} else {
  print("Predicted winner: Republican Party")
}

###########################Chart Porn#################################################
head(future_data)
# Ensure predicted_outcome has the correct levels
future_data$predicted_outcome <- factor(future_data$predicted_outcome, levels = c(0, 1), labels = c("Republican", "Democrat"))

# Define custom color palette for parties
party_colors <- c(Democrat = "blue", Republican = "red")


ggplot(future_data, aes(x = predicted_probs, fill = as.factor(predicted_outcome))) +
  geom_histogram(bins = 2, alpha = 0.5, position = "identity") +
  labs(title = "State count that will Win in 2024 by each party",
       x = "Predicted Probability", y = "State count", fill = "Party")+
  scale_fill_manual(values = party_colors)


# Plot with custom colors
ggplot(future_data, aes(x = reorder(state, -electoral_votes), y = electoral_votes, fill = predicted_outcome)) +
  geom_bar(stat = "identity") +
  labs(title = "Predicted Electoral Votes by State for 2024",
       x = "State", y = "Electoral Votes", fill = "Party") +
  scale_fill_manual(values = party_colors) +  # Apply custom color palette
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

us_states <- map_data("state")
future_data$state <- tolower(future_data$state)  # Ensure state names match

future_data = future_data %>% 
  rename ("region" = "state")

map_data <- merge(us_states, future_data, by = "region", all.x = TRUE)

ggplot(map_data, aes(x = long, y = lat, group = group, fill = as.factor(predicted_outcome))) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_manual(values = party_colors)+
  labs(title = "Map of Predicted Election Outcomes in 2024",
       fill = "Predicted Winner")


# Calculate correlations
cor_data <- cor(train_data[, c("PVI", "state_unemployment", "state_house_price", "state_med_income", "nat_gdp", "net_approval", "incumbent", "poll_party_Rep", "poll_party_Dem")], use = "complete.obs")

# Plot heatmap
corrplot(cor_data, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix of Predictors")
 
# Create a boxplot of State Unemployment by Incumbency Status
ggplot(Data, aes(x = factor(incumbent), y = state_unemployment, fill = factor(incumbent))) +
  geom_boxplot() +
  labs(x = "Incumbency Status", y = "State Unemployment", 
       title = "Boxplot of State Unemployment by Incumbency Status",
       fill = "Incumbency Status")

# Convert cycle to a factor for better plotting
Data$cycle <- as.factor(Data$cycle)
Data$poll_party_Dem
# Create a line plot of Polling Data Over Election Cycle
ggplot(Data, aes(x = cycle, y = poll_party_Rep, group = 1)) +
  geom_line(color = "red") +
  geom_line(aes(y = poll_party_Dem), color = "blue") +
  labs(x = "Election Cycle", y = "Poll Percentage", 
       title = "Line Plot of Polling Data Over Election Cycle",
       color = "Party", fill = "Party") +
  scale_x_discrete(labels = function(x) substr(x, 3, 4))  # Format x-axis labels to show only the last two digits



