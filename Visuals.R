library(tidyr)
library(openxlsx)
library(readxl)
library(ggplot2)
library(sf)
library(dplyr)
library(gt)

# Load Colorado city shapefile (adjust the path)
co_cities <- st_read("~/Documents/Executive Internship/TIF/QGIS/Boundary Shape Files/City_Boundaries/Colorado_City_Boundaries.shp")

growth_data <- read_excel("~/Documents/Executive Internship/TIF/Data/GrowthRate.xlsx") 

# Merge with your growth rate data (assuming you have a dataframe `growth_data`)
map_data <- co_cities %>% left_join(growth_data, by = "NAME")

# Plot
ggplot(map_data) +
  geom_sf(aes(fill = real_growth_rate), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  theme_minimal() +
  labs(title = "Total Sales Tax Revenue Growth (2016-2024)", fill = "Growth Rate (%)")

# Plot
ggplot(map_data) +
  geom_sf(aes(fill = nominial_growth_rate), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  theme_minimal() +
  labs(title = "Total Sales Tax Revenue Growth (2016-2024)", fill = "Growth Rate (%)")


# Sort cities by growth rate
growth_data1 <- growth_data %>% arrange(real_growth_rate)


ggplot(growth_data1, aes(x = reorder(NAME, real_growth_rate), y = real_growth_rate, fill = real_growth_rate)) +
  geom_col() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  scale_x_discrete(expand = c(0, 0)) +  
  theme_minimal() +
  labs(title = "Sales Tax Revenue Growth (2016-2024)", x = "City", y = "Growth Rate (%)") +
  theme(
    axis.text.y = element_text(size = 8)
  ) +
  coord_flip() +
  ylim(min(growth_data1$real_growth_rate) * 1.1, max(growth_data1$real_growth_rate) * 1.1)  # Extend limits slightly

ggplot(growth_data, aes(x = city, y = real_growth_rate)) +
  geom_col(fill = "lightblue") +
  labs(title = "Real Net Taxable Sales Growth Rate (2016-2024)", x = "City", y = "Real Growth Rate (%)") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 4.5)  # Decreases font size
  )

##################################################################################
##################### commercial parcel revenue tables #############################

df <- read_excel("Documents/Executive Internship/TIF/Data/parcel_rev.xlsx")


top5 <- df %>%
  arrange(desc(parcel_tax_rev)) %>%  # Sort in descending order
  slice_head(n = 5)  # Get top 5

bottom5 <- df %>%
  filter(parcel_tax_rev > 0) %>%  # Exclude parcels with 0 revenue
  arrange(parcel_tax_rev) %>%  # Sort in ascending order
  slice_head(n = 5)  

# Combine into one table
top_bottom <- bind_rows(top5, bottom5)

gt(top_bottom) %>%
  tab_header(
    title = "Top 5 and Bottom 5 Parcels by Tax Revenue"
  )


########################## table for transit point rev ###############################
parcelcount <- read_excel("Documents/Executive Internship/TIF/Data/CTP_parcelcount.xlsx")

top5 <- parcelcount %>%
  arrange(desc(rev)) %>%  # Sort in descending order
  slice_head(n = 5)  # Get top 5

bottom5 <- parcelcount %>%
  filter(rev > 0) %>%  # Exclude parcels with 0 revenue
  arrange(rev) %>%  # Sort in ascending order
  slice_head(n = 5)  

# Combine into one table
top_bottom <- bind_rows(top5, bottom5)

########################## table for lightrail rev ###############################
parcelcount <- read_excel("Documents/Executive Internship/TIF/Data/CLR_parcelcount.xlsx")

top5 <- parcelcount %>%
  arrange(desc(rev_total)) %>%  # Sort in descending order
  slice_head(n = 10)  # Get top 5

bottom5 <- parcelcount %>%
  filter(rev_total > 0) %>%  # Exclude parcels with 0 revenue
  arrange(rev_total) %>%  # Sort in ascending order
  slice_head(n = 5)  

# Combine into one table
top_bottom_lr <- bind_rows(top5, bottom5)

########################## table for facilities ###############################
parcelcount <- read_excel("Documents/Executive Internship/TIF/Data/CF_parcelcount.xlsx")

top5 <- parcelcount %>%
  arrange(desc(rev_total)) %>%  # Sort in descending order
  slice_head(n = 10)

  # Get top 5

bottom5 <- parcelcount %>%
  filter(rev > 0) %>%  # Exclude parcels with 0 revenue
  arrange(rev) %>%  # Sort in ascending order
  slice_head(n = 5)  

# Combine into one table
top_bottom_f <- bind_rows(top5, bottom5)

########################## table for BRT ###############################
parcelcount <- read_excel("Documents/Executive Internship/TIF/Data/CBRT_parcelcount.xlsx")

top5 <- parcelcount %>%
  arrange(desc(rev)) %>%  # Sort in descending order
  slice_head(n = 5)  # Get top 5

bottom5 <- parcelcount %>%
  filter(rev > 0) %>%  # Exclude parcels with 0 revenue
  arrange(rev) %>%  # Sort in ascending order
  slice_head(n = 5)  

# Combine into one table
top_bottom_brt <- bind_rows(top5, bottom5)

########################## table for transit 1313 ###############################
parcelcount <- read_excel("Documents/Executive Internship/TIF/Data/CTS_parcelcount.xlsx")

top5 <- parcelcount %>%
  arrange(desc(rev)) %>%  # Sort in descending order
  slice_head(n = 5)  # Get top 5

bottom5 <- parcelcount %>%
  filter(rev > 0) %>%  # Exclude parcels with 0 revenue
  arrange(rev) %>%  # Sort in ascending order
  slice_head(n = 5)  

# Combine into one table
top_bottom <- bind_rows(top5, bottom5)

###############################################################################
########################## Tracking top 5 ####################################
##############################################################################

sales_full <- read_excel("Documents/Executive Internship/TIF/Data/RealCitySales.xlsx")

sales_nov <- sales_full %>%
  filter(month == 11)

sales_filtered <- sales_nov %>%
  select(city, Year, sales_tax_revenue)

sales_yoy <- sales_filtered %>%
  pivot_wider(names_from = Year, values_from = sales_tax_revenue, names_prefix = "sales_tax_rev_")

write.xlsx(sales_yoy, "~/Documents/Executive Internship/TIF/Data/sales_yoy.xlsx")

################################ transit points ################################

TP <- read_excel("Documents/Executive Internship/TIF/Data/CTP_parcelcount.xlsx")

sales <- read_excel("Documents/Executive Internship/TIF/Data/sales_yoy.xlsx")

top5 <- TP %>%
  arrange(desc(rev)) %>%  # Sort in descending order
  slice_head(n = 5)  # Get top 5

TP_top5 <- merge(top5, sales, by = "city", all.x = TRUE)

write.xlsx(TP_top5, "~/Documents/Executive Internship/TIF/Data/TP_yoy.xlsx")

df <- read_excel("Documents/Executive Internship/TIF/Data/TP_yoy.xlsx")

df_long <- df %>%
  pivot_longer(cols = starts_with("gr_"), names_to = "year", values_to = "yoy_growth")

df_long$year <- gsub("gr_", "", df_long$year)

ggplot(df_long, aes(x = year, y = yoy_growth, group = stop_name, color = stop_name)) +
  geom_line() +                          # Add lines for each station
  geom_point() +                          # Add points to the lines
  labs(title = "Year-over-Year Growth Rate by Top 5 Revenue Earning Transit Points",
       x = "Year",
       y = "YoY Growth Rate (%)",
       color = "Stop Name") +                # Customize legend title
  scale_color_manual(values = c(
    "Silverthorne 1" = "pink",             # Assign pink to Silverthorne1
    "Silverthorne 2" = "pink",             # Assign pink to Silverthorne2
    "Dillon" = "pink",                    # Assign pink to Dillon
    "Aurora, CO, USA" = "lightblue",                  # Assign blue to Station4
    "French Creek" = "lightgreen"                  # Assign green to Station5
  )) + 
  theme_minimal() +                      # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

################################ light rail ################################

LR <- read_excel("Documents/Executive Internship/TIF/Data/CLR_parcelcount.xlsx")

sales <- read_excel("Documents/Executive Internship/TIF/Data/sales_yoy.xlsx")

top10 <- LR %>%
  arrange(desc(rev_total)) %>%  # Sort in descending order
  slice_head(n = 10)  # Get top 10

LR_top5 <- merge(top5, sales, by = "city", all.x = TRUE)

write.xlsx(LR_top5, "~/Documents/Executive Internship/TIF/Data/LR_yoy.xlsx")

df <- read_excel("Documents/Executive Internship/TIF/Data/LR_yoy.xlsx")

df_long <- df %>%
  pivot_longer(cols = starts_with("gr_"), names_to = "year", values_to = "yoy_growth")

df_long$year <- gsub("gr_", "", df_long$year)

ggplot(df_long, aes(x = year, y = yoy_growth, group = NAME, color = NAME)) +
  geom_line() +                          # Add lines for each station
  geom_point() +                          # Add points to the lines
  labs(title = "Year-over-Year Growth Rate by Top 5 Revenue Earning Light Rail Stations",
       x = "Year",
       y = "YoY Growth Rate (%)",
       color = "Stop Name") + 
  scale_color_manual(values = c(
    "13th Avenue Station" = "blue",            
    "2nd Ave / Abilene Station" = "blue",             
    "Aurora Metro Center Station" = "blue",                    
    "Arapahoe at Village Center Station" = "green",                 
    "Englewood Station" = "pink"                 
  )) + 
  theme_minimal() +                      # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

################################ facilities ################################

FC <- read_excel("Documents/Executive Internship/TIF/Data/CF_parcelcount.xlsx")

sales <- read_excel("Documents/Executive Internship/TIF/Data/sales_yoy.xlsx")

top5 <- FC %>%
  arrange(desc(rev_total)) %>%  # Sort in descending order
  slice_head(n = 10)  # Get top 10

FC_top5 <- merge(top5, sales, by = "city", all.x = TRUE)

write.xlsx(FC_top5, "~/Documents/Executive Internship/TIF/Data/FC_yoy.xlsx")

df <- read_excel("Documents/Executive Internship/TIF/Data/FC_yoy.xlsx")

df_long <- df %>%
  pivot_longer(cols = starts_with("gr_"), names_to = "year", values_to = "yoy_growth")

df_long$year <- gsub("gr_", "", df_long$year)

ggplot(df_long, aes(x = year, y = yoy_growth, group = NAME, color = NAME)) +
  geom_line() +                          # Add lines for each station
  geom_point() +                          # Add points to the lines
  labs(title = "Year-over-Year Growth Rate by Top 5 Revenue Earning RTD Facilities",
       x = "Year",
       y = "YoY Growth Rate (%)",
       color = "Stop Name") + 
  scale_color_manual(values = c(
    "13th Avenue Station" = "purple",            
    "2nd Ave / Abilene Station" = "purple",             
    "Aurora Metro Center Station" = "purple",                    
    "Arapahoe at Village Center Station" = "green",                 
    "Englewood Station" = "pink"                 
  )) + 
  theme_minimal() +                      # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

################################ FRPR ################################

FRPR <- read_excel("Documents/Executive Internship/TIF/Data/CFRPR_parcelcount.xlsx")

sales <- read_excel("Documents/Executive Internship/TIF/Data/sales_yoy.xlsx")

top5 <- FRPR %>%
  arrange(desc(rev)) %>%  # Sort in descending order
  slice_head(n = 5)  # Get top 5

FRPR_top5 <- merge(top5, sales, by = "city", all.x = TRUE)

write.xlsx(FRPR_top5, "~/Documents/Executive Internship/TIF/Data/FRPR_yoy.xlsx")

df <- read_excel("Documents/Executive Internship/TIF/Data/FRPR_yoy.xlsx")

df_long <- df %>%
  pivot_longer(cols = starts_with("gr_"), names_to = "year", values_to = "yoy_growth")

df_long$year <- gsub("gr_", "", df_long$year)

ggplot(df_long, aes(x = year, y = yoy_growth, group = city, color = city)) +
  geom_line() +                          # Add lines for each station
  geom_point() +                          # Add points to the lines
  labs(title = "Year-over-Year Growth Rate by Top Revenue Earning Proposed FRPR Station",
       x = "Year",
       y = "YoY Growth Rate (%)",
       color = "Stop Name") + 
  theme_minimal() +                      # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

################################ mountain rail ################################

MR <- read_excel("Documents/Executive Internship/TIF/Data/CMR_parcelcount.xlsx")

sales <- read_excel("Documents/Executive Internship/TIF/Data/sales_yoy.xlsx")

top5 <- MR %>%
  arrange(desc(rev)) %>%  # Sort in descending order
  slice_head(n = 5)  # Get top 5

MR_top5 <- merge(top5, sales, by = "city", all.x = TRUE)

write.xlsx(MR_top5, "~/Documents/Executive Internship/TIF/Data/MR_yoy.xlsx")

df <- read_excel("Documents/Executive Internship/TIF/Data/MR_yoy.xlsx")

df_long <- df %>%
  pivot_longer(cols = starts_with("gr_"), names_to = "year", values_to = "yoy_growth")

df_long$year <- gsub("gr_", "", df_long$year)

ggplot(df_long, aes(x = year, y = yoy_growth, group = Name, color = Name)) +
  geom_line() +                          # Add lines for each station
  geom_point() +                          # Add points to the lines
  labs(title = "Year-over-Year Growth Rate by Top Revenue Earning Mountain Rail Stations",
       x = "Year",
       y = "YoY Growth Rate (%)",
       color = "Stop Name") + 
  theme_minimal() +                      # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

################################ BRT ################################

BRT <- read_excel("Documents/Executive Internship/TIF/Data/CBRT_parcelcount.xlsx")

sales <- read_excel("Documents/Executive Internship/TIF/Data/sales_yoy.xlsx")

top5 <- BRT %>%
  arrange(desc(rev)) %>%  # Sort in descending order
  slice_head(n = 5)  # Get top 5

BRT_top5 <- merge(top5, sales, by = "city", all.x = TRUE)

write.xlsx(BRT_top5, "~/Documents/Executive Internship/TIF/Data/BRT_yoy.xlsx")

df <- read_excel("Documents/Executive Internship/TIF/Data/BRT_yoy.xlsx")

df_long <- df %>%
  pivot_longer(cols = starts_with("gr_"), names_to = "year", values_to = "yoy_growth")

df_long$year <- gsub("gr_", "", df_long$year)

ggplot(df_long, aes(x = year, y = yoy_growth, group = city, color = city)) +
  geom_line() +                          # Add lines for each station
  geom_point() +                          # Add points to the lines
  labs(title = "Year-over-Year Growth Rate by Top 5 Revenue Earning Proposed BRT Stations",
       x = "Year",
       y = "YoY Growth Rate (%)",
       color = "Stop Name") + 
  scale_color_manual(values = c(
    "Cherry Hills Village(6768)" = "violet",            
    "Cherry Hills Village(7075)" = "violet",             
    "Cherry Hills Village(7088)" = "violet",                    
    "Cherry Hills Village(8196)" = "violet",                 
    "Aurora" = "lightblue"                 
  )) + 
  theme_minimal() +                      # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

################################ transit 1313 ################################

TS <- read_excel("Documents/Executive Internship/TIF/Data/CTS_parcelcount.xlsx")

sales <- read_excel("Documents/Executive Internship/TIF/Data/sales_yoy.xlsx")

top5 <- TS %>%
  arrange(desc(rev)) %>%  # Sort in descending order
  slice_head(n = 5)  # Get top 5

TS_top5 <- merge(top5, sales, by = "city", all.x = TRUE)

write.xlsx(TS_top5, "~/Documents/Executive Internship/TIF/Data/TS_yoy.xlsx")

df <- read_excel("Documents/Executive Internship/TIF/Data/TS_yoy.xlsx")

df_long <- df %>%
  pivot_longer(cols = starts_with("gr_"), names_to = "year", values_to = "yoy_growth")

df_long$year <- gsub("gr_", "", df_long$year)

ggplot(df_long, aes(x = year, y = yoy_growth, group = name, color = name)) +
  geom_line() +                          # Add lines for each station
  geom_point() +                          # Add points to the lines
  labs(title = "Year-over-Year Growth Rate by Top 5 Revenue Earning Transit Stations(HB24-1313)",
       x = "Year",
       y = "YoY Growth Rate (%)",
       color = "Stop Name") + 
  scale_color_manual(values = c(
    "Belleview Station" = "orange",            
    "Englewood Station" = "orange",             
    "Orchard Station" = "orange",                    
    "Oxford-City of Sheridan Station 1" = "orange",                 
    "Oxford-City of Sheridan Station 2" = "blue"                 
  )) + 
  theme_minimal() +                      # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

################################ state sales yoy ################################

state_sales <- read_excel("Documents/Executive Internship/TIF/Data/state_sales.xlsx")

state_sales <- state_sales %>%
  group_by(year) %>%
  summarise(sales_sum = sum(sales, na.rm = TRUE))

sales_yoy <- state_sales %>%
  pivot_wider(names_from = year, values_from = sales_sum, names_prefix = "sales_")

write.xlsx(sales_yoy, "~/Documents/Executive Internship/TIF/Data/state_yoy.xlsx")

df <- read_excel("Documents/Executive Internship/TIF/Data/state_yoy.xlsx")

df_long <- df %>%
  pivot_longer(cols = starts_with("gr_"), names_to = "year", values_to = "yoy_growth")

df_long$year <- gsub("gr_", "", df_long$year)

write.xlsx(df_long, "~/Documents/Executive Internship/TIF/Data/stateyoy_pivot.xlsx")

df <- read_excel("Documents/Executive Internship/TIF/Data/stateyoy_pivot.xlsx")


ggplot(df, aes(x = year, y = yoy_growth, group = 1)) +
  geom_line(color = "darkgreen") +                          
  geom_point(color = "darkblue") +                          
  labs(title = "Year-over-Year Growth Rate for State",
       x = "Year",
       y = "YoY Growth Rate (%)") +       
  theme_minimal() +                      
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  










