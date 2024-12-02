install.packages("viridis")

library(tidyverse)
library(ggplot2)
library(ggtext)
library(viridis)

# Import the data from the CSV
HBAT_data <- read.csv ("./HBAT.csv", header = TRUE)

# Rename the columns
names(HBAT_data) <- c("id", "Customer.Type","Industry.Type", "Firm.Size", "Region", 
                      "Distribution.System", "Product.Quality",
                      "Website.Activities", "Technical.Support",
                      "Complaint.Resolution", "Advertising", "Product.Line",
                      "Salesforce.Image", "Competitive.Pricing",
                      "Warranty.Claims", "New.Products", "Order.Bill",
                      "Price.Flexibility", "Delivery.Speed", "Satisfaction",
                      "Recommendation", "Future.Purchase", "Current.Purchase",
                      "Partnership")

## Data cleanup

summary(HBAT_data)
# No outliers at a glance by checking the min and max values

# Remove the ID because it is a sequential variable not gathered from the survey
HBAT_data <- HBAT_data %>% select(-id)

# Convert some columns to factors
HBAT_data$Customer.Type = factor(HBAT_data$Customer.Type, levels=c(1, 2, 3), labels=c("<1", "1-5", ">5"))
HBAT_data$Industry.Type = factor(HBAT_data$Industry.Type, levels=c(0, 1), labels=c("magazine", "newsprint"))
HBAT_data$Firm.Size = factor(HBAT_data$Firm.Size, levels=c(0, 1), labels=c("small", "large"))
HBAT_data$Region = factor(HBAT_data$Region, levels=c(0, 1), labels=c("UK", "international"))
HBAT_data$Distribution.System = factor(HBAT_data$Distribution.System, levels=c(0, 1), labels=c("broker", "direct"))
# Convert partnership to a boolean value
HBAT_data$Partnership = as.logical(HBAT_data$Partnership)

## Question 1. Analyse HBAT's customer profile using descriptive statistics and exploratory data analysis

# Graphs to understand the data

# Graph 1: Industry type vs distribution system vs number of customers in each

# Aggregate the data by distribution system and industry type and calculate the percentage of customers
graph_category_counts <- HBAT_data %>%
  mutate(Distribution.System = case_when(
    Distribution.System == "direct" ~ "Direct", 
    Distribution.System == "broker" ~ "Indirect"
  )) %>%
  group_by(Distribution.System, Industry.Type) %>%
  summarise(Count = n()) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = paste0(round(Percentage, 1), "%")
  )

# Render a bar chart with the grouped data
ggplot(graph_category_counts, aes(x = Distribution.System, y = Count, fill = Industry.Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(
    aes(label = Count), 
    position = position_dodge(width = 0.9), 
    vjust = -1, 
    size = 4,
    fontface = "bold"
  ) +
  geom_text(
    aes(label = Label, y = Count / 2), 
    position = position_dodge(width = 0.9), 
    color = "white", 
    size = 4,
    fontface = "bold"
  ) + 
  scale_fill_viridis(
    discrete = TRUE, 
    labels = c("Magazine", "Newsprint"),
    name = "Industry Type", 
    option="H"
  ) +
  labs(
    title = "Industry type by distrubtion system",
    x = "Distribution System",
    y = "Number of Customers"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.title.position = "plot",
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(20, 0, 20, 0)),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(0, 20, 0, 20)),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_y_continuous(limits = c(0, max(category_counts$Count) * 1.1))


# Graph 2
# Group by region and customer type and add the percentage of customers in each
graph_customer_type_by_region <- HBAT_data %>%
  mutate(
    Region = case_when(
      Region == 'UK' ~ "UK", 
      Region == 'international' ~ "Outside UK"
    ),
    Customer.Type = case_when(
      Customer.Type == "<1" ~ "<1 year", 
      Customer.Type == "1-5" ~ "1-5 years",
      Customer.Type == ">5" ~ ">5 years"
    )
  ) %>%
  group_by(Region, Customer.Type) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the bar chart to display the data
ggplot(graph_customer_type_by_region, aes(x = Region, y = Percentage, fill = Customer.Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f%%", Percentage)), 
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("<1 year" = "#31113b", "1-5 years" = "#790b02", ">5 years" = "#680e7d"),
    name = "Customer Type"
  ) +
  labs(
    title = "Customer Type Distribution by Region",
     x = "Region",
     y = "Percentage of Customers"
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.title.position = "plot",
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(20, 0, 20, 0)),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(0, 20, 0, 20)),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_y_continuous(limits = c(0, max(category_counts$Count) * 0.5))



# Graph 3

# Group the data by firm size and calculate percentage
firm_size_chart <- HBAT_data %>%
  group_by(Firm.Size) %>%
  summarise(Count = n()) %>%
  mutate(
    Percentage = round(100 * Count / sum(Count), 1),
    Label = paste0(
      case_when(
        Firm.Size == "small" ~ "Small firm", 
        Firm.Size == "large" ~ "Large firm"
      ), 
      "\n(", Percentage, "%)"
    )
  )

# Create the pie chart
ggplot(firm_size_chart, aes(x = "", y = Percentage, fill = Firm.Size)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(
    aes(label = Label), 
    fontface = "bold", 
    color = "white", 
    position = position_stack(vjust = 0.5)
  ) +
  scale_fill_manual(
    values = c("small" = "#31113b", "large" = "#790b02"),
    labels = c("Small firm", "Large firm"),
    name = "Firm Size"
  ) +
  labs(title = "Distribution of Firm Sizes") +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(face = "bold")
  )

# Hypothesis 1: Larger firms tend to make higher current purchases
# H0 = Large company current purchase <= Small company current purchase
# Ha = Large company current purchase > Small company current purchase
HBAT_small_company_customers = HBAT_data %>% 
  filter(Firm.Size == 'small')
HBAT_large_company_customers = HBAT_data %>% 
  filter(Firm.Size == 'large')

t.test(HBAT_large_company_customers$Current.Purchase, HBAT_small_company_customers$Current.Purchase, "greater")
# A p-value of 0.08854 is small but not statistically significant using the threshold of 0.05.
# It is possible to test the opposite, to confirm both are equal.

t.test(HBAT_large_company_customers$Current.Purchase, HBAT_small_company_customers$Current.Purchase, "less")
# The p-value of 0.9115 makes this not significant. 

# Statistically, there is no difference between the groups given no significant p-values are 
# found in the tests.


# Hypothesis 2: Larger firms are mostly outside the UK
# H0 = Larger firms outside the UK <= Larger firms in the UK
# Ha = Larger firms outside the UK > Larger firms in the UK
size_region_contingency_table <- table(HBAT_data$Firm.Size, HBAT_data$Region)
chisq.test(size_region_contingency_table)
# The p-value is extremely low and the x-squared value of 51.108 supports this.
# It confirms the larger companies tend to be outside the UK.


# Hypothesis 3: Product Quality leads to higher recommendation scores
# H0 = Correlation(Product.Quality, Recommendation) < 0.3
# Ha = Correlation(Product.Quality, Recommendation) > 0.3
cor.test(HBAT_data$Product.Quality, HBAT_data$Recommendation)
# Using a threshold of 0.3 for significance in correlation, the result 0.4360348
# indicates a positive correlation between product quality and recommendations.
# The p-value of 1.092e-10 makes this relation statistically significant.

summary(HBAT_data)
# It is possible to check the values from the summary to understand the data better.

# Customer type is well spread across the data set, with each one being around 33%.
# Industry type follows the same pattern with a 50/50 split between magazine and newsprint.
# Firm size is very close to 50/50 but slightly more biased to large companies.
# In the Region category there are more international (60%) than in the UK (40%).
# When it comes to the distribution system there are a few more using brokers than 
# direct sales (54/46).
# Perceived quality is always over 5, with the median being 8, indicating a skew towards 
# the upper range. This is supported by a 3rd Quartile of 9.1.
# The website image is not good with a max of 5.7 but the 3rd Quartile being 4.1 indicates 
# pretty bad scores overall.
# Support rating varies greatly and it is very similar to complaint resolution rating, 
# which might indicate some correlation.
# Advertising perception is pretty low. Given the question is not provided, it is difficult 
# to understand the reason behind it. E.g. It could be how many times they have seen adverts, 
# or if they like them.
# The product line seems to cover basic needs for a majority of people but not everyone is 
# happy with the offerings with the 1st quartile being 4.8.
# Salesforce image is again similar to technical support and complain resolution but more 
# centered given the minimum is 2.5.
# Pricing is generally perceived as competitive with a 1st quartile of 5.8 and a median over 7.
# The claims on the warranties areare also positive with a 1st quartile of 5.4 and a median over 6.1.
# Happiness with the new products released and their frequency is split with a median of 5.2. 
# Although there are some good perceptions, the 3rd Quartile of 6.225 indicates is generaly 
# on the lower side.
# Ordering and billing perception is low, with a 3rd quartile of 4.7. Some improvements are 
# likely needed in this area.
# The company is tight when it comes to offering flexible pricing, with a 3rd quartile of 
# 5.325. A max value of 7.5 might indicate a few users getting good deals.
# Delivery is one of the worst areas with the best score being 5.5.
# Satisfaction on the purchases made is generaly positive with a 1st quartile of 6 and a 
# minimum of 4.7.
# Recommendation follows a similar trend, with a lower minimum, all other values are very 
# close to satisfaction, indicating the possibility of some correlation between them.
# The likelihood of future purchases could also be correlated although values are more 
# positive, with a median 0.6 points over the other two and a 1st quartile of 7.1.
# The current purchase percentage indicates users are diversifying, with none having 
# 100% and a 1st quartile of 51.1% meaning HBAT is the main supplier for most of these users.
# Finally, 43% of users would consider developing an strategic partnership with the company.


## Question 2. Customer satisfaction and other purchase outcomes between the two channels in the distribution system

# Distribution systems: broker, direct
# Purchase outcomes: Satisfaction, Recommendation, Future.Purchase, Current.Purchase, Partnership

#  Split the purchase outcomes by distribution system
HBAT_broker_purchase_outcomes = HBAT_data %>% 
  filter(Distribution.System == 'broker') %>% 
  select(Satisfaction, Recommendation, Future.Purchase, Current.Purchase, Partnership)
HBAT_direct_purchase_outcomes = HBAT_data %>% 
  filter(Distribution.System == 'direct') %>% 
  select(Satisfaction, Recommendation, Future.Purchase, Current.Purchase, Partnership)

# Hypothesis 1: Brokers are more likely to want a strategic partnership

# H0 = Customers with direct channel partnership % <= Customers with broker channel partnership %
# Ha = Customers with direct channel partnership % > Customers with broker channel partnership %


t.test(HBAT_direct_purchase_outcomes$Partnership, HBAT_broker_purchase_outcomes$Partnership, alternative="greater")
# A p-value of 1.817e-07 is small enough to consider it significant within the 95% confidence interval.
# This indicates direct distribution system users are more likely to consider a partnerhsip.


ggplot(HBAT_data, aes(x = Partnership, fill = Distribution.System)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Distribution of Partnership Potential by Distribution Channel",
    x = "Partnership Potential",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(0, 0, 20, 0)),
    plot.title.position = "plot",
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(20, 0, 20, 0)),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(0, 20, 0, 20)),
    legend.title = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_fill_manual(values = c("broker" = "#FF9998", "direct" = "#9999FF"))

## Question 3. Relationships with its customers over time.

# Given the customer type variable defines the length of the relationship between customer
# and the company, different groups should have some distinct characteristics.

# Analysing satisfaction per customer type
# H0 = satisfaction <1 = satisfaction 1-5 = satisfaction >5
# Ha = satisfaction <1 != satisfaction 1-5 != satisfaction >5

satisfaction_type_anova <- aov(Satisfaction ~ Customer.Type, data = HBAT_data)
summary(satisfaction_type_anova)
# The f-value (113.8) suggests the variance between groups is much larger than the one within them.
# The low p-value (<2e-16) indicates a significant difference between groups.
TukeyHSD(satisfaction_type_anova)
# The results from Tukey's HSD indicate that there is a large difference between
# customers with less than 1 year and the others. Although there is a difference
# between the oldest ones and the 1-5 year range, it is not as significant. The 
# p-adj of 0.0006092 indicates a the groups being closer than those with 0.


# Analysing current purcahse level per customer type
# H0 = current purchase <1 = current purchase 1-5 = current purchase >5
# Ha = current purchase <1 != current purchase 1-5 != current purchase >5

current_purchase_type_anova <- aov(Current.Purchase ~ Customer.Type, data = HBAT_data)
summary(current_purchase_type_anova)
# The f-value (216.6) suggests the differences between groups are more distinguishable than
# in the satisfaction analysis. The p-value is still significant.
TukeyHSD(current_purchase_type_anova)
# In this case all groups are further to one another than in the satisfaction hypothesis
# because all p-adj are 0.
# There is a large difference between oldest (>5) and newest (<1) customers (17.9706)
# and a similar one between <1 and 1-5 (9.6194), and 1-5 and >5 (8.3511). This could
# indicate this growing linearly over time.


# Analysing future purcahse level per customer type
# H0 = future purchase <1 = future purchase 1-5 = future purchase >5
# Ha = future purchase <1 != future purchase 1-5 != future purchase >5

future_purchase_type_anova <- aov(Future.Purchase ~ Customer.Type, data = HBAT_data)
summary(future_purchase_type_anova)
# The f-value (50.12) is the lowest of the three tests meaning the difference between
# groups is closer to the one within than it was in the other hypothesis. The p-value
# still makes the results statistically significant.
TukeyHSD(future_purchase_type_anova)
# In the case of future purchase the difference between 1-5 and >5 is not statistically
# significant using a 0.05 threshold, meaning ther is no much difference between the two groups.
# Nonetheless, there is a definitive difference between the <1 group and the rest.


## Question 4. Interpret key factors that help HBAT understand customer views

# Principal Component Analysis
data.pca <- prcomp(HBAT_data[,6:18], center = TRUE, scale = TRUE)
summary(data.pca)

# Extract the eigenvalues from the PCA object
# The eigenvalues represent the amount of variance captured by each principal component.
# The higher variance means the component has a higher effect on the data set.

eigenvalues <- data.pca$sdev^2

# Create a scree plot
plot(
  eigenvalues, type = "b",
   xlab = "Principal Component",
   ylab = "Eigenvalue",
   main = "Scree plot for PCA"
)

# Add a line at the latest elbow. 
# This is where the difference between components starts becoming less significant.
# In this, case it would be 6 as 5 to 6 has still a significant decrease but 
# the slope between 6 and 7 starts being the same as the remaining ones.
abline(v = 6, col = "red")

# Selecting the factors 1 to 6
key.factors <- data.pca$x[,1:6]

# Add the new factors to the data (PC1 to PC6)
HBAT_data <- merge(HBAT_data, key.factors)

# Loading plots.
# The loadings represent the correlation between the different components from PCA with the original variables.
# With them it is possible to identify which variables influence the principal component.
# Values closer to 0 indicate little or no contribution.
# The variable `rotation` contains the loadings.
data.pca$rotation[,1:6]

load    <- data.pca$rotation
sorted.loadings <- load[order(load[, 1]), 1]
myTitle <- "Loadings Plot for PC1" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")
# Delivery.Spee (-0.459), Complaint.Resolution (-0.432), and Order.Bill (-0.414) 
# are the main contributors affecting negatively to PC1.
# It could represent an overall negative experience or a variable where customer 
# complaints and lower product quality lead to lower scores on this component.

sorted.loadings <- load[order(load[, 2]), 2]
myTitle <- "Loadings Plot for PC2" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")
# Competitive.Pricing (0.455) and Price.Flexibility (0.403) contribute positively 
# and Product.Quality(-0.436) and Product.Line (-0.425) negatively.
# The four variables have very similar loads although affect it in an opposite way.
# The two positive and the two negatives might be correlated within themselves.
# It could represent a trade-off between pricing and reputation.

sorted.loadings <- load[order(load[, 3]), 3]
myTitle <- "Loadings Plot for PC3" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")
# Technical.Support (0.547) and Warranty.Claims (0.541) are the main contributors affecting PC3 positively.
# Website.Activities (0.278) and Salesforce.Image (0.285) are the next pair also contributing positvely.
# The two pairs are very similar, meaning there is a strong correlation within each pair.
# This could represent overall customer support service.

sorted.loadings <- load[order(load[, 4]), 4]
myTitle <- "Loadings Plot for PC4" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")
# Website.Activities affects it the most positively, whereas Technical.Support affects it negatively.
# Also the pair Advertising and Salesforce.Image acts postively and contrary to Warranty.Claims and Price.Fexibility.
# It could represent digital presence.

sorted.loadings <- load[order(load[, 5]), 5]
myTitle <- "Loadings Plot for PC5" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")
# PC5 is dominated by New.Products in a negative way, with a -0.956.
# Other values would try to compensate for it like Product.Line (0.148) or Complaint.Resolution (0.138).
# It would represent new product impact balanced out by others like complains.

sorted.loadings <- load[order(load[, 6]), 6]
myTitle <- "Loadings Plot for PC6" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")
# Competitive.Pricing and Product.Quality are the main contributors positively (0.627 and 0.501).
# Advertising affects it negatively in a similar degree (0.501).
# It could represent the trade-off between pricing and marketing.

## Question 5. Classify HBAT’s customers into segments based on key perceptions of the firm’s performance

# Set a seed so any random values are constant across different executions.
set.seed(12345)

# K-means
# New function to compute total within-cluster sum of square.
# This only uses perceived performance indicators (columns 6 to 18)
wss <- function(k) {
  kmeans(HBAT_data[,6:18], k, nstart = 13 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 13
k.values <- 1:13

# Extract wss for clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Following the elbow method, 3 seems to be the sensible number of clusters.

# Run K-means with 3 clusters including all variables
k_3_clusters <- kmeans(HBAT_data[,6:18], 3)

# Label customers using the 3 clusters, assigning the group they belong to.
HBAT_data$Cluster3 <- k_3_clusters$cluster
HBAT_data$Cluster3 <- factor(HBAT_data$Cluster3)
# Use anova to see if there is a significant difference between the groups.
anova3 <- aov(Recommendation ~ Cluster3, data = HBAT_data)
summary(anova3)
# The F-value of 5110 indicates a large difference between groups compared to the
# differences within the group.
TukeyHSD (anova3)
# All the comparisons between the clusters are significant given the p-adj value is 0.
# Cluster 3 has the highest mean values, with Cluster 1 being the next one and finally Cluster 2.


# Split the clusters in different groups:
cluster_1_data <- HBAT_data %>% filter(Cluster3 == 1)
cluster_2_data <- HBAT_data %>% filter(Cluster3 == 2)
cluster_3_data <- HBAT_data %>% filter(Cluster3 == 3)

# First analysis of the clusters.

summary(cluster_1_data$Recommendation)
summary(cluster_2_data$Recommendation)
summary(cluster_3_data$Recommendation)
ggplot(HBAT_data, aes(x = Cluster3, y = Recommendation, fill = Cluster3)) +
  geom_boxplot() +
  labs(title = "Recommendation by Cluster", x = "Cluster", y = "Recommendation (0-10)") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Cluster")) +
  scale_fill_manual(
    values = c("1" = "#875c49", "2" = "#ab4b4d", "3" = "#8863ab")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.title.position = "plot",
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(20, 0, 20, 0)),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(0, 20, 0, 20)),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )
# Cluster 3 seems more likely to recommend overall, with Cluster 2 having the worst recommendations.

# Analyse future purcahses per cluster
summary(cluster_1_data$Future.Purchase)
summary(cluster_2_data$Future.Purchase)
summary(cluster_3_data$Future.Purchase)
ggplot(HBAT_data, aes(x = Cluster3, y = Future.Purchase, fill = Cluster3)) +
  geom_boxplot() +
  labs(title = "Future purchase by Cluster", x = "Cluster", y = "Future purchase (0-10)") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Cluster")) +
  scale_fill_manual(
    values = c("1" = "#875c49", "2" = "#ab4b4d", "3" = "#8863ab")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.title.position = "plot",
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(20, 0, 20, 0)),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(0, 20, 0, 20)),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )
# Cluster 3 seems more likely to purchase in the future, with Cluster 2 having the worst intentions.

# Analyse partnership per cluster
c1_partnership_pcnt = nrow(cluster_1_data[cluster_1_data$Partnership == TRUE,])/nrow(cluster_1_data) * 100
c2_partnership_pcnt = nrow(cluster_2_data[cluster_2_data$Partnership == TRUE,])/nrow(cluster_2_data) * 100
c3_partnership_pcnt = nrow(cluster_3_data[cluster_3_data$Partnership == TRUE,])/nrow(cluster_3_data) * 100

partnership_graph_data = data.frame(
  Cluster = c(1, 2, 3),
  Percentage = c(c1_partnership_pcnt, c2_partnership_pcnt, c3_partnership_pcnt)
)
ggplot(partnership_graph_data, aes(x = factor(Cluster), y = Percentage, fill = factor(Cluster))) +
  geom_col(color = "black") +
  labs(title = "Percentage by Cluster", x = "Cluster", y = "Percentage (%)") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Cluster")) +
  scale_fill_manual(
    values = c("1" = "#875c49", "2" = "#ab4b4d", "3" = "#8863ab")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.title.position = "plot",
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(20, 0, 20, 0)),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(0, 20, 0, 20)),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )
# Cluster 3 is more likely to partner with the company, with Cluster 2 being the less likely.

# Analyse satisfaction per cluster
summary(cluster_1_data$Satisfaction)
summary(cluster_2_data$Satisfaction)
summary(cluster_3_data$Satisfaction)
ggplot(HBAT_data, aes(x = Cluster3, y = Satisfaction, fill = Cluster3)) +
  geom_boxplot() +
  labs(title = "Satisfaction by Cluster", x = "Cluster", y = "Satisfaction (0-10)") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Cluster")) +
  scale_fill_manual(
    values = c("1" = "#875c49", "2" = "#ab4b4d", "3" = "#8863ab")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.title.position = "plot",
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(20, 0, 20, 0)),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(0, 20, 0, 20)),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )
# Cluster 3 is the most satisfied with the company, with Cluster 2 being the least.

# Analyse current purchase per cluster
summary(cluster_1_data$Current.Purchase)
summary(cluster_2_data$Current.Purchase)
summary(cluster_3_data$Current.Purchase)
ggplot(HBAT_data, aes(x = Cluster3, y = Current.Purchase, fill = Cluster3)) +
  geom_boxplot() +
  labs(title = "Purchase level by Cluster", x = "Cluster", y = "Current purchase level (%)") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Cluster")) +
  scale_fill_manual(
    values = c("1" = "#875c49", "2" = "#ab4b4d", "3" = "#8863ab")
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(0, 0, 10, 0)),
    plot.title.position = "plot",
    axis.title.x = element_text(face = "bold", size = 14, margin = margin(20, 0, 20, 0)),
    axis.title.y = element_text(face = "bold", size = 14, margin = margin(0, 20, 0, 20)),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

# Cluster 3 has higher values in terms of current purchase, followed by cluster 1.
# Although cluster 2 has the lowest absolute value (37.1) and a low median, their max is
# the second highest.

# With the data gathered so far, it could be inferred that Cluster 3 is the happiest with the company,
# Cluster 1 is the ones that are a bit more neutral and Cluster 2 being the most unhappy.


# It is possible to analyse the reasons a bit more by summarising the data for each cluster.
summary(cluster_1_data)
# This first cluster contains mostly a lot of companies between of the type 1-5 years (~60%).
# The industry type is evenly split but company size favours large with a 70/30 split.
# Virtually all the companies are international (97.1%) and there is a 65/35 split
# in distribution system, with broker being the preferred one.

summary(cluster_2_data)
# This second group does not contain any company over 5 years old, but still keeps the
# balance in the industry type (magazine ~49%, newsprint ~51%). It favours larger companies
# although the split is not as obvious as in the first one (61.5%), and international
# companies are also the norm here with a 82/18 split. Broker distribution system is a 85%
# of the group.


summary(cluster_3_data)
# The last group is mostly formed by older companies (68.4%) and has an even 50/50 split in 
# industry types. The companies tend to be smaller (68.4%) and mostly in the UK (78.26%).
# Direct distribution system is the norm with 68.4%.


# First conclusions from the clustering:
# Cluster 1 contains with a lot of companies between 1 and 5 years and mostly international 
# do not seem to be the happiest but neither the worst.
# Cluster 2 is formed from the most unhappy customers and does not contain companies
# older than 5 years, meaning there could be a correlation between time and overall happiness.
# Cluster 3 is mostly formed by UK companies, and are the happiest of them all. It indicates
# proximity and direct distribution systems could be related to overall perception.

# To determine which segments are more likely to recommend, purchase in the future, or increase 
# their purchase volume, hypothesis will be created from the first conclusions.

# In terms of recommendations, cluster 2 seems more likely than 1 and 3. Do two test to demonstrate.

# Hypothesis 1.1
# H0 = Cluster 3 Recommendation <= Cluster 1 Recommendation
# Ha = Cluster 3 Recommendation > Cluster 1 Recommendation
t.test(cluster_3_data$Recommendation, cluster_1_data$Recommendation, "greater")
# With a low p-value (< 2.2e-16) and within a 95% interval indicates an statistically significant
# difference. The large t-value of 47.88 also indicates a big difference between the clusters.

# Hypothesis 1.2
# H0 = Cluster 3 Recommendation <= Cluster 2 Recommendation
# Ha = Cluster 3 Recommendation > Cluster 2 Recommendation
t.test(cluster_3_data$Recommendation, cluster_2_data$Recommendation, "greater")
# This second test has the same p-value but the t-value is larger (104.79). 
# This indicates clusters 2 and 3 are not as close as 3 and 1.

# The two tests confirm Cluster 3 is more likely to recommend than Clusters 1 and .



# In terms of future purchases, the same applies.

# Hypothesis 1.1
# H0 = Cluster 3 Future.Purchase % <= Cluster 1 Future.Purchase
# Ha = Cluster 3 Future.Purchase % > Cluster 1 Future.Purchase
t.test(cluster_3_data$Future.Purchase, cluster_1_data$Future.Purchase, "greater")
# With a low p-value (< 2.2e-16) and within a 95% interval indicates an statistically significant
# difference. The large t-value of 52.707 also indicates a big difference between the clusters.

# Hypothesis 1.2
# H0 = Cluster 3 Future.Purchase <= Cluster 2 Future.Purchase
# Ha = Cluster 3 Future.Purchase > Cluster 2 Future.Purchase
t.test(cluster_3_data$Future.Purchase, cluster_2_data$Future.Purchase, "greater")
# This second test has the same p-value but the t-value is larger (99.512). 
# This indicates clusters 1 and 3 are closer together than 2 and 3.

# The two tests confirm Cluster 3 is more likely to increase purchase in the future than Clusters 1 and 2.



# Finally the same is done for purchase volume.

# Hypothesis 1.1
# H0 = Cluster 3 Current.Purchase % <= Cluster 1 Current.Purchase
# Ha = Cluster 3 Current.Purchase % > Cluster 1 Current.Purchase
t.test(cluster_3_data$Current.Purchase, cluster_1_data$Current.Purchase, "greater")
# With a low p-value (< 2.2e-16) and within a 95% interval indicates an statistically significant
# difference. The large t-value of 59.285 also indicates a big difference between the clusters.

# Hypothesis 1.2
# H0 = Cluster 3 Current.Purchase <= Cluster 2 Current.Purchase
# Ha = Cluster 3 Current.Purchase > Cluster 2 Current.Purchase
t.test(cluster_3_data$Current.Purchase, cluster_2_data$Current.Purchase, "greater")
# This second test has the same p-value but the t-value is larger (178.75). 
# This indicates clusters 1 and 3 are closer together than 2 and 3.

# The two tests confirm Cluster 3 is more has higher purchase volumes than Clusters 1 and 2.






## Question 6. Identify the perceptions of HBAT that best distinguish customers across customers interested in future strategic alliances/partnerships

# There are multiple classification algorithms that can be used for this task.
# Considering this is a binary outcome, logistic regression will be a good option.
# The results can help explaining which values are the most important. 

# Set up cross-validation with a 5-fold grouping for all tests.
set_up_train_cv <- trainControl(
  method = "cv", 
  number = 5,   
  classProbs = TRUE,                
  summaryFunction = twoClassSummary
)


# Defining Partnership:
# Train a logistic regression model with cross-validation (80/20 split)
# Randomly select the indexes for the training set(80%) with Partnership as outcome variable.
train_validation.Index <- createDataPartition(HBAT_data$Partnership, p=0.80, list=FALSE)
# Create the training set with the selected values.
train_validation.set <- HBAT_data[train_validation.Index,]
# Get the rest as testing set (remaining 20%).
testset <- HBAT_data[-train_validation.Index,]

# Train a logistic regression model using the perception variables and Partnership as outcome.
partnership_lr_mod <- train(
  form = make.names(Partnership) ~ Product.Quality + 
    Website.Activities + Technical.Support + 
    Complaint.Resolution + Advertising + Product.Line + 
    Salesforce.Image + Competitive.Pricing + Warranty.Claims + 
    New.Products + Order.Bill + Price.Flexibility + 
    Delivery.Speed,
  data = train_validation.set,
  trControl = set_up_train_cv,
  method = "glm",
  family = "binomial",
  metric = "ROC" 
)

# Check the results of the cross validation
partnership_lr_mod
# A ROC value of 0.9 indicates a good performance being able to classify between the two partnership groups.
# Sensitivity indicates how many times the positive result has actually been correctly guessed.
# In this case 83.08% of predicted partnerships in the test sets have been correct.
# Specificity indicates how many times the negative result has actually been correctly guessed.
# In this case 77.99% of predicted 'non-partnerships' were correct.
# The training data is better at predicting partnerships than it is at predicting they will not partner.

# Test the model with the testing data and see how it performs with a confusion matrix.
partnership_predictions <- make.names(predict(partnership_lr_mod, testset))
confusionMatrix(factor(partnership_predictions), factor(make.names(testset$Partnership)))
# Accuracy is 81% (between 80.12% and 81.85% within a 95% confidence interval).
# With a sensitivity of 83.60% and a specificity of 77.56%, it is slightly worse than during training,
# indicating there is no over-fitting and the model works well.
# A p-value of "< 2e-16" indicates that the model is better than random assignation.
# Prevalence indicates there is a higher chance of it predicting false than true.
# Mcnemar’s Test P-Value is not significant (0.5552), meaning there is no bias towards false negative
# or false positives.

# Summary the logistic model
summary(partnership_lr_mod)
# It is possible to see that at this point most of the variables influence the model.
# The ones that seem to be less relevant are Complaint.Resolution, New.Products and Order.Bill.


# Defining geographic regions:
# Train a logistic regression model with cross-validation (80/20 split)
# Randomly select the indexes for the training set(80%) with region as outcome variable.
region_train_validation.Index <- createDataPartition(HBAT_data$Region, p=0.80, list=FALSE)
# Create the training set with the selected values.
region_train_validation.set <- HBAT_data[region_train_validation.Index,]
# Get the rest as testing set (remaining 20%).
region_testset <- HBAT_data[-region_train_validation.Index,]

# Train a logistic regression model using the perception variables and Partnership as outcome.
region_lr_mod <- train(
  form = make.names(Region) ~ Product.Quality + 
    Website.Activities + Technical.Support + 
    Complaint.Resolution + Advertising + Product.Line + 
    Salesforce.Image + Competitive.Pricing + Warranty.Claims + 
    New.Products + Order.Bill + Price.Flexibility + 
    Delivery.Speed,
  data = region_train_validation.set,
  trControl = set_up_train_cv,
  method = "glm",
  family = "binomial",
  metric = "ROC" 
)

# Check the results of the cross validation
region_lr_mod
# A ROC value of 0.96 indicates a great performance being able to classify between the two region groups.
# Sensitivity indicates how many times the positive result has actually been correctly guessed.
# In this case 89.63% of predicted international in the test sets have been correct.
# Specificity indicates how many times the negative result has actually been correctly guessed.
# In this case 89.19% of predicted UK were correct.
# The training data is similarly good to classify in either of the two outcomes.

# Test the model with the testing data and see how it performs with a confusion matrix.
region_predictions <- make.names(predict(region_lr_mod, region_testset))
confusionMatrix(factor(region_predictions), factor(make.names(region_testset$Region)))
# Accuracy is 88.76% (between 88.05% and 89.45% within a 95% confidence interval).
# With a sensitivity of 89.52% and specificity of 87.65%, the results in the training set are very
# similar to those of cross-validation, indicating no over-fitting. It performs sightly worse
# when predicting UK companies.
# A p-value of "< 2e-16" indicates that the model is better than random assignation.
# Prevalence of 59.50% highlights an imbalance and a tendency to suggest an international region.
# This is to be expected, reflecting also the original data with a 40/60 split.
# Mcnemar’s Test P-Value is significant (0.001081), and the model has a tendency to incorrectly
# classify a UK region as international.

# Summary the logistic model
summary(region_lr_mod)
# New.Products is again not relevant for the classification, but in this case, it is together with 
# Product.Line and Warranty.Claims.



# Defining firm sizes:
# Train a logistic regression model with cross-validation (80/20 split).
# Randomly select the indexes for the training set(80%) with region as outcome variable.
firm_size_train_validation.Index <- createDataPartition(HBAT_data$Firm.Size, p=0.80, list=FALSE)
# Create the training set with the selected values.
firm_size_train_validation.set <- HBAT_data[firm_size_train_validation.Index,]
# Get the rest as testing set (remaining 20%).
firm_size_testset <- HBAT_data[-firm_size_train_validation.Index,]

# Train a logistic regression model using the perception variables and Partnership as outcome.
firm_size_lr_mod <- train(
  form = make.names(Firm.Size) ~ Product.Quality + 
    Website.Activities + Technical.Support + 
    Complaint.Resolution + Advertising + Product.Line + 
    Salesforce.Image + Competitive.Pricing + Warranty.Claims + 
    New.Products + Order.Bill + Price.Flexibility + 
    Delivery.Speed,
  data = firm_size_train_validation.set,
  trControl = set_up_train_cv,
  method = "glm",
  family = "binomial",
  metric = "ROC" 
)

# Check the results of the cross validation
firm_size_lr_mod
# A ROC value of 81.80% indicates a good performance being able to classify between the two sizes.
# It is worse than the other models previously used.
# Sensitivity is 75.10%, indicating how many of predicted large in the test sets have been correct.
# Specificity is 74.92%, indicating how many of predicted small were correct.
# The training data is similarly good to classify in either of the two outcomes but not very accurate.

# Test the model with the testing data and see how it performs with a confusion matrix.
firm_size_predictions <- make.names(predict(firm_size_lr_mod, firm_size_testset))
confusionMatrix(factor(firm_size_predictions), factor(make.names(firm_size_testset$Firm.Size)))
# The overall accuracy of the model is 75.28% which is not very good. Sensitivity (76.23%) and 
# Specificity (74.29%) are very close to the ones in training indicating no over-fitting.
# The Prevalence of 51% matches the spread in the original data.
# The Mcnemar's Test P-Value of 0.4054 indicates there is no significant difference between the groups.
# This might be the reason why it is not as good classifying as it could.

summary(firm_size_lr_mod)
# In this case the summary p-values indicate all are relevant to predicting size although
# Product.Line is again the least impacting, followed by Price.Flexibility.
# Others like Delivery.Speed, Complaint.Resolution or Advertising are not the most impactful,
# but still considered relevant.







## Question 7. Predict customer satisfaction based on their perceptions of HBAT’s performance

# Need to set up a new training set with Satisfaction as the outcome variable (80/20 split).
q7_train_validation.Index <- createDataPartition(HBAT_data$Satisfaction, p=0.80, list=FALSE)
# Select 20% of the data for test set
q7_testset <- HBAT_data[-q7_train_validation.Index,]
# Use the remaining 80% of data to training and testing the models
q7_train_validation.set <- HBAT_data[q7_train_validation.Index,]

# Set up cross-validation
q7_set_up_train_cv <- trainControl(method = "cv", number = 5)

q7_lm <- train(
  form = Satisfaction ~ Product.Quality + 
    Website.Activities + Technical.Support + 
    Complaint.Resolution + Advertising + Product.Line + 
    Salesforce.Image + Competitive.Pricing + Warranty.Claims + 
    New.Products + Order.Bill + Price.Flexibility + 
    Delivery.Speed,
  data = q7_train_validation.set,
  trControl = q7_set_up_train_cv,
  method = "lm"
)

q7_lm
# The R-squared of 80.42% indicates the current model can explain the Satisfaction variable
# relatively well.
# The RMSE of 0.5468 and a MAE of 0.4217 indicate that there are some larger errors making
# RMSE worse, but the predictions are generally half a point up or down.

q7_lm_test <- predict(q7_lm, q7_testset)
accuracy(q7_lm_test, q7_testset$Satisfaction)
# The values of RMSE (0.5529) and MAE (0.4279) are pretty much equal to those during cross-validation,
# indicating a very close performance in the testing set and no over-fitting.
# There is an average error of -0.0051, which is very small 

summary(q7_lm)
# All the variables have a low p-value but Warranty.Claims and Delivery.Speed, which could be omitted.
# The R-squared is around 80%, which means the model explains quite a lot but could be improved.


# Try a second algorithm, decision tree, to see if there are better results.
# This already ignores Warranty.Claims and Delivery.Speed variables.
q7_rpart_model <- train(
  form = Satisfaction ~ Product.Quality + 
    Website.Activities + Technical.Support + 
    Complaint.Resolution + Advertising + Product.Line + 
    Salesforce.Image + Competitive.Pricing + 
    New.Products + Order.Bill + Price.Flexibility,
  data = q7_train_validation.set,
  trControl = q7_set_up_train_cv,
  method = "rpart"
)

q7_rpart_model
# When training a decision tree, the complexity parameter (cp) helps avoiding over-fitting.
# A larger cp will lead to less branches compared to smaller one.
# In this case, three results can be seen and the one chosen is 0.09469. Chosing the smallest
# one could lead to a more over-fitted model.
# The reason for the selection is it having the lowest RMSE, but it also has the highest 
# R-squared and lowest MAE. The R-squared value being 52.72% means it will not be able to 
# explain the behaviour better than the original linear regression.
# It will likely perform worse in the test set than the previous model.

q7_rpart_test <- predict(q7_rpart_model, q7_testset)
accuracy(q7_rpart_test, q7_testset$Satisfaction)
# It has a smaller ME, indicating less bias towards one of the options, but every
# other value is worse. RMSE of 0.8928 is higher than the 0.5529 from the previous model. 
# Also MAE is 0.70 higher, with MPE and MAPE absolute numbers also being significantly worse.

summary(q7_rpart_model)
# The decision tree puts importance scores to the different variables, which helps understanding
# how it goes about deciding. In this case Product.Line (30), Product.Quality (23), 
# and Price.Flexibility (12) have the highest scores, with Salesforce.Image (3), 
# Website.Activities (3), Advertising (2) and  New.Products (1) being the lowest.



# Try the original linear regression algorithm (which seemed to perform better) but with
# the principal component analysis variables.
q7_lm_pc <- train(
  form = Satisfaction ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6,
  data = q7_train_validation.set,
  trControl = q7_set_up_train_cv,
  method = "lm"
)

q7_lm_pc
# An R-squared of 0.1% means this algorithm will not perform well at all.
# RMSE and MAE are also over 1, which will indicate pretty bad accuracy for later.


# Test anyway to confirm these results.
q7_lm_pc_test <- predict(q7_lm_pc, q7_testset)
accuracy(q7_lm_pc_test, q7_testset$Satisfaction)
# Although ME is very small, RMSE is over 1.2, which is more than twice the first model.
# MAE (1.0457), MPE (-3.4389) and MAPE (15.8555) are also worse off.

summary(q7_lm_pc)
# The summary highlights how the model does not explain much given a virtually 0 R-square 
# and the high extremely p-values, with none being significant to predict Satisfaction.


# Model 1 (linear regression) seems to be the best predictor.
