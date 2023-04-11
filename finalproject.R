##
library(dplyr)
library(ggplot2)
library(lemon)
##
final.hcris <-readRDS('data/output/HCRIS_Data.rds')
colnames(final.hcris)[colnames(final.hcris) == "provider_number"] <- "provider"

final.pos.data <- final.pos.data[!is.na(final.pos.data$own_type), ]
final.pos.data <- final.pos.data[final.pos.data$own_type %in% c(1, '01', 2, '02', 3, '03', 4, '04', 9, '09') & final.pos.data$category %in% c(1, '01'), ]
final.pos.data$profit_status <- ifelse(final.pos.data$own_type %in% c(4, '04', 9, '09'), "Profit", "Non-Profit")

merged <- merge(final.hcris, final.pos.data, by = c("provider", "year", "state", "street", "city", "zip"))

merged <- merged %>%
  mutate(price = ((ip_charges + icu_charges + ancillary_charges) * (1 - tot_discounts / tot_charges) - tot_mcare_payment)/ (tot_discharges - mcare_discharges))
merged <- merged %>% drop_na(price)

merged <- merged %>%
  mutate(ip_price = ((price * (tot_discharges - mcare_discharges)) + tot_mcare_payment) / (1 - tot_discounts / tot_charges) - icu_charges - ancillary_charges)
##


#graph1
summary_data <- merged %>%
  group_by(year, profit_status) %>%
  summarize(count = n())

graph1 <- ggplot(summary_data, aes(x = year, y = count, fill = profit_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#C3A3E8", "#5A2A82")) +
  labs(x = "Year", y = "Number of Hospitals",
       title= "Distribution of Hospitals, by Profit Status, Over Time", fill = "Profit Status") + 
  theme_classic() + ylim(0,500) +
  scale_x_continuous(limits = c(1995, 2022), breaks = seq(1990, 2022, by = 5))

graph1


#time series line chart - graph2

merged_fp <- merged %>% filter(profit_status == "Profit")
merged_np <- merged %>% filter(profit_status == "Non-Profit")

merged_fp_mean <- merged_fp %>% group_by(year) %>% summarize(mean_price = mean(price))
merged_np_mean <- merged_np %>% group_by(year) %>% summarize(mean_price = mean(price))

graph2<- ggplot() + 
  geom_line(data = merged_fp_mean, aes(x = year, y = mean_price, color = "Profit"), size = 1) +
  geom_line(data = merged_np_mean, aes(x = year, y = mean_price, color = "Non-Profit"), size = 1) +
  scale_color_manual(values = c("gray", "black")) +
  labs(title = "Hospital Prices by Profit Status Over Time",
       x = "Year",
       y = "Average Price of Hospital Stay") +
  ylim(3500, 20000) +
  theme_bw()

graph2


#graph3

graph3 <- ggplot(merged, aes(x = profit_status, y = price, fill = profit_status)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8, width = 0.4, height = 2) +
  labs(title = "Hospital Prices by Profit Status",
       x = "Profit Status",
       y = "Price", fill = "Profit Status") + 
  scale_fill_manual(values = c("#C3A3E8", "#A8D8EA")) + ylim(0,80000) +
  theme_bw()

graph3

#table1
non_profit_price <- mean(subset(merged, profit_status == "Non-Profit")$price)
profit_price <- mean(subset(merged, profit_status == "Profit")$price)

table1 <- data.frame(Profit_Status = c("Non-Profit", "Profit"),
                     Average_Price = c(non_profit_price, profit_price))
colnames(table1) <- c("Profit Status", "Average Price")
table1

#table2
table2 <- aggregate(cbind(price, beds_tot, tot_operating_exp) ~ profit_status, data = merged, function(x) round(mean(x),0))
colnames(table2) <- c("Profit Status", "Price", "Beds", "Operating Expense")
print(table2, row.names = FALSE)

#table3
table3 <- aggregate(price ~ own_type, data = merged, mean)
colnames(table3) <- c("Hospital Type", "Price")
rownames(table3) <- c("Non-Profit Church", "Non-Profit Private", "Non-Profit Other", "For-Profit", "For-Profit Physician-Owned")
table3$Price <- round(table3$Price, 2)
print(table3)

library(dplyr)

#table4
#proportion of each variable on tot_charges by profit status
sum_by_profit <- aggregate(cbind(ip_charges, icu_charges, ancillary_charges, tot_operating_exp, tot_charges) ~ profit_status, data = merged, sum)
table4 <- sum_by_profit %>%
  mutate(ip_charges = ip_charges / tot_charges,
         icu_charges = icu_charges / tot_charges,
         ancillary_charges = ancillary_charges / tot_charges,
         tot_operating_exp = tot_operating_exp / tot_charges) %>%
  select(profit_status, ip_charges, icu_charges, ancillary_charges, tot_operating_exp)
table4 <- table4 %>% mutate_at(vars(2:5), ~ round(., 2))
colnames(table4) <- c("Profit Status", "In Patient", "ICU", "Ancillary", "Operating Expenses")

print(table4)



#didnt work
library(GGally)
variables <- c("price", "beds_tot", "tot_operating_exp", "profit_status")

# Subset the merged data by the selected variables and remove missing values
data_subset <- na.omit(merged[, variables])

# Create a scatter plot matrix with a density plot for the diagonal
ggpairs(data_subset, 
        columns = variables, 
        aes(x = price, y = beds_tot, color = profit_status), 
        upper = list(continuous = wrap("cor", size = 3)), 
        lower = list(continuous = wrap("smooth", method = "lm")), 
        diag = list(continuous = wrap("density", alpha = 0.7)),
        title = "Correlation Matrix of Hospital Prices, Beds and Operating Expenses")





save.image("final_workspace.Rdata")
