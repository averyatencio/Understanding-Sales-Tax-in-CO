library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(readr)
library(ggplot2)


Sales_Report <- read_excel("~/Documents/Executive Internship/TIF/Data/Sales_County_Report_2016_To_Date.xlsx", 
                                               col_types = c("numeric", "numeric", "text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric"))

citysales <- read_excel("Documents/Executive Internship/TIF/Data/RealCitySales.xlsx")

countysales <- read_excel("Documents/Executive Internship/TIF/Data/novcountytax.xlsx")


ggplot(countysales, aes(x = county, y = county_sales_tax)) +
  geom_col(fill = "purple") +
  labs(title = "County Net Taxable Sales (November 2024)", x = "County", y = "Net Taxable Sales") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

threshold <- 10000000  

ggplot(citysales %>% filter(sales_tax_revenue > threshold), 
       aes(x = city, y = sales_tax_revenue)) +
  geom_col(fill = "lightblue") +
  labs(title = "City Net Taxable Sales November 2024 (>10M$) ", x = "City", y = "Net Taxable Sales") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

ggplot(citysales %>% filter(Year %in% c(2023, 2024), sales_tax_revenue > threshold), 
       aes(x = city, y = sales_tax_revenue, fill = factor(Year))) +
  geom_col(position = "dodge") +  # Places bars side by side
  labs(title = "City Net Taxable Sales Nov 2023 vs Nov 2024 (>$10M)", 
       x = "City", 
       y = "Net Taxable Sales", 
       fill = "Year") +  # Adds legend title
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
  scale_fill_manual(values = c("2023" = "lightblue", "2024" = "darkblue"))  # Custom colors


unique(Sales_Report$County)

Sales_Report <- Sales_Report %>% filter(County != "Total")

Sales_Report <- Sales_Report %>% filter(County != "Remainder")

ggplot(citysales %>% 
         filter(month == 11) %>%
         group_by(city) %>%
         filter(any(sales_tax_revenue > 250000000)), 
       aes(x = Year, y = sales_tax_revenue, color = city, group = city)) +
  geom_line(size = 0.5) +  # Line for each county
  geom_point(size = 2) +  # Add points for visibility
  labs(title = "City Net Taxable Sales in November (2016-2024) for Cities with Revenue > $250M",
       x = "Year",
       y = "Net Taxable Sales",
       color = "City") +
  theme_minimal() 


citysalesfull <- read_excel("Documents/Executive Internship/TIF/Data/Beginning Data/CitySalesFull(2016-2024).xlsx")

sales_2024 <- citysalesfull %>%
  filter(Year == 2024) %>%
  group_by(City) %>%
  summarise(Total_2024 = sum(State_Net_Taxable_Sales, na.rm = TRUE))

write.xlsx(sales_2024, "~/Documents/Executive Internship/TIF/Data/sales2024.xlsx")

################################################################################
############################ Yearly sales graphs ###############################
################################################################################

city_sales <- read_excel("Documents/Executive Internship/TIF/Data/RealCitySales.xlsx")

city_sales <- city_sales %>%
  group_by(city, Year) %>%
  summarise(sales_sum = sum(sales_tax_revenue, na.rm = TRUE))

threshold <- 500000000 

threshold1 <- 10000000

ggplot(city_sales %>% filter(sales_sum > threshold), 
       aes(x = city, y = sales_sum)) +
  geom_col(fill = "lightblue") +
  labs(title = "City Net Taxable Sales 2024 (>$500M) ", x = "City", y = "Net Taxable Sales") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

ggplot(city_sales %>% filter(sales_sum*0.029 > threshold1), 
       aes(x = city, y = sales_sum*0.029)) +
  geom_col(fill = "lightblue") +
  labs(title = "City Sales Tax Revenue 2024 (>$10M) ", x = "City", y = "Sales Tax Revenue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

county_sales <- read_excel("Documents/Executive Internship/TIF/Data/countysales_2024.xlsx")

county_sales <- county_sales %>%
  group_by(County) %>%
  summarise(sales_sum = sum(sales, na.rm = TRUE))

ggplot(county_sales %>% filter(sales_sum > 0), 
       aes(x = County, y = sales_sum)) +
  geom_col(fill = "purple") +
  labs(title = "County Net Taxable Sales 2024 ", x = "County", y = "Net Taxable Sales") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))

ggplot(county_sales %>% filter(sales_sum > 0), 
       aes(x = County, y = sales_sum*0.029)) +
  geom_col(fill = "purple") +
  labs(title = "County Sales Tax Revenue 2024 ", x = "County", y = "Sales Tax Revenue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))



city_sales %>%
  filter(sales_sum > 2500000000) %>%
  ggplot(aes(x = Year, y = sales_sum, color = city, group = city)) +
  geom_line(size = 0.5) +  # Line for each city
  geom_point(size = 2) +   # Add points for visibility
  labs(
    title = "City Net Taxable Sales (2016-2024) for Cities with NTS > $2.5B",
    caption = "*NTS = Net Taxable Sales",
    x = "Year",
    y = "Net Taxable Sales",
    color = "City"
  ) +
  theme_minimal()

city_sales %>%
  filter(sales_sum > 1500000000) %>%
  ggplot(aes(x = Year, y = sales_sum*0.029, color = city, group = city)) +
  geom_line(size = 0.5) +  # Line for each city
  geom_point(size = 2) +   # Add points for visibility
  labs(
    title = "City Sales Tax Revenue (2016-2024) for Cities with Revenue > $1.5B",
    x = "Year",
    y = "Sales Tax Revenue",
    color = "City"
  ) +
  theme_minimal()

################################################################################
################################# Projections ##################################
################################################################################

tax_data <- read_excel("~/Documents/Executive Internship/TIF/Data/RealCitySales.xlsx")

cities <- tax_data %>% distinct(city)

# collapse months #
tax_data <- tax_data %>%
  group_by(city, Year) %>%
  summarise(rev = sum(rev, na.rm = TRUE))

future_years <- expand.grid(
  city = cities$city,
  Year = 2025:2030
)

# add 2025-2030
city_rev_expanded <- bind_rows(tax_data, future_years) %>%
  arrange(city, Year)

write.xlsx(city_rev_expanded, "~/Documents/Executive Internship/TIF/Data/sales_expanded.xlsx")

## county tax data ##
county_data <- read_excel("~/Documents/Executive Internship/TIF/Data/county_sales.xlsx")

counties <- county_data %>% distinct(County)

# collapse months #
county_data <- county_data %>%
  group_by(County, Year) %>%
  summarise(rev_sum = sum(rev_county, na.rm = TRUE))

future_years_county <- expand.grid(
  County = counties$County,
  Year = 2025:2030
)

# add 2025-2030
county_rev_expanded <- bind_rows(county_data, future_years_county) %>%
  arrange(County, Year)

write.xlsx(county_rev_expanded, "~/Documents/Executive Internship/TIF/Data/county_expanded.xlsx")


################################ do the same for LR #############################

LR <- read_excel("~/Documents/Executive Internship/TIF/Data/CLR_parcelcount.xlsx")

stopid <- LR %>% distinct(STOPID)

future_yearsLR <- expand.grid(
  STOPID = stopid$STOPID,
  Year = 2025:2030
)

# add 2025-2030
LR_expanded <- bind_rows(LR, future_yearsLR) %>%
  arrange(STOPID, Year)

# fill in missing values
LR_expanded <- LR_expanded %>%
  arrange(STOPID, Year) %>%
  group_by(STOPID) %>%
  fill(everything(), .direction = "down") %>%
  ungroup()

# add projections
prj <- read_excel("~/Documents/Executive Internship/TIF/Data/sales_prj.xlsx")

LR_prj <- merge(LR_expanded, prj, by = c("city", "Year"), all.x = TRUE)

#### add city_out ##

prj1 <- read_excel("~/Documents/Executive Internship/TIF/Data/sales_prj_out.xlsx")

LR_prj1 <- merge(LR_prj, prj1, by = c("city_out", "Year"), all.x = TRUE)

write.xlsx(LR_prj1, "~/Documents/Executive Internship/TIF/Data/LR_prj.xlsx")

## graph ##

prj_LR <- read_excel("~/Documents/Executive Internship/TIF/Data/LR_prj.xlsx")

top_stopsLR <- prj_LR %>%
  filter(Year == 2024) %>%
  arrange(desc(estimated)) %>%
  slice_head(n = 10) %>%
  pull(STOPID)

prj_LR <- prj_LR %>%
  filter(estimated > 50000000)

ggplot(prj_LR, aes(x = Year, y = prj_total, group = STOPID, color = NAME)) +
  geom_line(alpha = 0.4) +
  labs(
    title = "Projected Sales Tax Revenue per Light Rail Station > $50M (2024–2030) ",
    x = "Year",
    y = "Projected Sales Tax Revenue",
    color = "Station"
  ) +
  theme_minimal()

prj_LR <- prj_LR %>%
  arrange(desc(estimated))

LR_wide <- prj_LR %>%
  select(city, NAME, Year, prj_total) %>%  # keep only necessary columns
  pivot_wider(
    names_from = Year,
    values_from = prj_total
  )

################################ do the same for F #############################

fl <- read_excel("~/Documents/Executive Internship/TIF/Data/CF_parcelcount.xlsx")

stopid <- fl %>% distinct(STOPID)

future_yearsF <- expand.grid(
  STOPID = stopid$STOPID,
  Year = 2025:2030
)

# add 2025-2030
F_expanded <- bind_rows(fl, future_yearsF) %>%
  arrange(STOPID, Year)

# fill in missing values
F_expanded <- F_expanded %>%
  arrange(STOPID, Year) %>%
  group_by(STOPID) %>%
  fill(everything(), .direction = "down") %>%
  ungroup()

# add projections
prj <- read_excel("~/Documents/Executive Internship/TIF/Data/sales_prj.xlsx")

F_prj <- merge(F_expanded, prj, by = c("city", "Year"), all.x = TRUE)

#### add city_out ##

prj1 <- read_excel("~/Documents/Executive Internship/TIF/Data/sales_prj_out.xlsx")

F_prj1 <- merge(F_prj, prj1, by = c("city_out", "Year"), all.x = TRUE)

#### add county projection ##

prj_c <- read_excel("~/Documents/Executive Internship/TIF/Data/county_prj.xlsx")

F_prj2 <- merge(F_prj1, prj_c, by = c("county", "Year"), all.x = TRUE)

write.xlsx(F_prj2, "~/Documents/Executive Internship/TIF/Data/F_prj.xlsx")

## graph ##

prj_F <- read_excel("~/Documents/Executive Internship/TIF/Data/F_prj.xlsx")

prj_F <- prj_F %>%
  filter(estimated > 50000000)

ggplot(prj_F, aes(x = Year, y = prj_total, group = STOPID, color = NAME)) +
  geom_line(alpha = 0.4) +
  labs(
    title = "Projected Sales Tax Revenue per RTD Facility > $50M (2024–2030) ",
    x = "Year",
    y = "Projected Sales Tax Revenue",
    color = "RTD Facility"
  ) +
  theme_minimal()

prj_F <- prj_F %>%
  arrange(desc(estimated))

F_wide <- prj_F %>%
  select(city, NAME, Year, prj_total) %>%  # keep only necessary columns
  pivot_wider(
    names_from = Year,
    values_from = prj_total
  )

################################ do the same for FRPR #############################

frpr <- read_excel("~/Documents/Executive Internship/TIF/Data/CFRPR_parcelcount.xlsx")

name <- frpr %>% distinct(Name)

future_yearsfrpr <- expand.grid(
  Name = name$Name,
  Year = 2025:2030
)

# add 2025-2030
frpr_expanded <- bind_rows(frpr, future_yearsfrpr) %>%
  arrange(Name, Year)

# fill in missing values
frpr_expanded <- frpr_expanded %>%
  arrange(Name, Year) %>%
  group_by(Name) %>%
  fill(everything(), .direction = "down") %>%
  ungroup()

# add projections
prj <- read_excel("~/Documents/Executive Internship/TIF/Data/sales_prj.xlsx")

frpr_prj <- merge(frpr_expanded, prj, by = c("city", "Year"), all.x = TRUE)

write.xlsx(frpr_prj, "~/Documents/Executive Internship/TIF/Data/frpr_prj.xlsx")

## graph ##

prj_frpr <- read_excel("~/Documents/Executive Internship/TIF/Data/frpr_prj.xlsx")

ggplot(prj_frpr, aes(x = Year, y = prj_total, group = Name, color = Name)) +
  geom_line(alpha = 0.4) +
  labs(
    title = "Projected Sales Tax Revenue per Proposed FRPR Station (2024–2030) ",
    x = "Year",
    y = "Projected Sales Tax Revenue",
    color = "FRPR Station"
  ) +
  theme_minimal()

prj_frpr <- prj_frpr %>%
  arrange(desc(estimated))

frpr_wide <- prj_frpr %>%
  select(city, Name, Year, prj_total) %>%  # keep only necessary columns
  pivot_wider(
    names_from = Year,
    values_from = prj_total
  )

################################ do the same for MH #############################

mh <- read_excel("~/Documents/Executive Internship/TIF/Data/CMH_parcelcount.xlsx")

name <- mh %>% distinct(MH)

future_yearsMH <- expand.grid(
  MH = mh$MH,
  Year = 2025:2030
)

# add 2025-2030
mh_expanded <- bind_rows(mh, future_yearsMH) %>%
  arrange(MH, Year)

# fill in missing values
mh_expanded <- mh_expanded %>%
  arrange(MH, Year) %>%
  group_by(MH) %>%
  fill(everything(), .direction = "down") %>%
  ungroup()

# add projections
prj <- read_excel("~/Documents/Executive Internship/TIF/Data/sales_prj.xlsx")

mh_prj <- merge(mh_expanded, prj, by = c("city", "Year"), all.x = TRUE)

#### add city_out ##

prj1 <- read_excel("~/Documents/Executive Internship/TIF/Data/sales_prj_out.xlsx")

mh_prj1 <- merge(mh_prj, prj1, by = c("city_out", "Year"), all.x = TRUE)

write.xlsx(mh_prj1, "~/Documents/Executive Internship/TIF/Data/mh_prj.xlsx")

## graph ##

prj_mh <- read_excel("~/Documents/Executive Internship/TIF/Data/mh_prj.xlsx")

ggplot(prj_mh, aes(x = Year, y = prj_total, group = MH, color = MH)) +
  geom_line(alpha = 0.4) +
  labs(
    title = "Projected Sales Tax Revenue per Proposed Mobility Hub (2024–2030) ",
    x = "Year",
    y = "Projected Sales Tax Revenue",
    color = "Mobility Hub"
  ) +
  theme_minimal()

prj_mh <- prj_mh %>%
  arrange(desc(estimated))

mh_wide <- prj_mh %>%
  select(city, MH, Year, prj_total) %>%  # keep only necessary columns
  pivot_wider(
    names_from = Year,
    values_from = prj_total
  )


################################ do the same for MR #############################

MR <- read_excel("~/Documents/Executive Internship/TIF/Data/CMR_parcelcount.xlsx")

name <- MR %>% distinct(Name)

future_yearsMR <- expand.grid(
  Name = name$Name,
  Year = 2025:2030
)

# add 2025-2030
MR_expanded <- bind_rows(MR, future_yearsMR) %>%
  arrange(Name, Year)

# fill in missing values
MR_expanded <- MR_expanded %>%
  arrange(Name, Year) %>%
  group_by(Name) %>%
  fill(everything(), .direction = "down") %>%
  ungroup()

# add city projections
prj <- read_excel("~/Documents/Executive Internship/TIF/Data/sales_prj.xlsx")

MR_prj <- merge(MR_expanded, prj, by = c("city", "Year"), all.x = TRUE)

#### add county projection ##

prj_c <- read_excel("~/Documents/Executive Internship/TIF/Data/county_prj.xlsx")

MR_prj1 <- merge(MR_prj, prj_c, by = c("county", "Year"), all.x = TRUE)

write.xlsx(MR_prj1, "~/Documents/Executive Internship/TIF/Data/MR_prj.xlsx")

## graph ##

prj_MR <- read_excel("~/Documents/Executive Internship/TIF/Data/MR_prj.xlsx")


ggplot(prj_MR, aes(x = Year, y = prj_total, group = Name, color = Name)) +
  geom_line(alpha = 0.4) +
  labs(
    title = "Projected Sales Tax Revenue per Proposed Mountain Rail Station (2024–2030) ",
    x = "Year",
    y = "Projected Sales Tax Revenue",
    color = "Station"
  ) +
  theme_minimal()

prj_MR <- prj_MR %>%
  arrange(desc(estimated))

MR_wide <- prj_MR %>%
  select(city, Name, Year, prj_total) %>%  # keep only necessary columns
  pivot_wider(
    names_from = Year,
    values_from = prj_total
  )
