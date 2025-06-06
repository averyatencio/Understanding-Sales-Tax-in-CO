library(tidyr)
library(openxlsx)
library(readxl)
library(dplyr)
library(priceR)
library(WDI)



########################### Merging City Sales #####################
CitySales1 <- read_excel("~/Documents/Executive Internship/TIF/Data/CitySales(2016-2020).xlsx", 
                                   col_types = c("numeric", "numeric", "text", 
                                                 "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric"))

CitySales2 <- read_excel("~/Documents/Executive Internship/TIF/Data/Sales_City_Report_2021_To_Date.xlsx", 
                                             col_types = c("numeric", "numeric", "text", 
                                                           "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric"))

CitySalesfull <- merge(CitySales1, CitySales2, by = c("City", "Year", "Month"), all = TRUE)


# Fix .x and .y Columns 
CitySalesfull <- CitySalesfull %>%
  mutate(
    Retailers = coalesce(Retailers.x, Retailers.y),
    Returns = coalesce(Returns.x, Returns.y),
    Gross_Sales = coalesce(Gross_Sales.x, Gross_Sales.y),
    Retail_Sales = coalesce(Retail_Sales.x, Retail_Sales.y),
    State_Net_Taxable_Sales = coalesce(State_Net_Taxable_Sales.x, State_Net_Taxable_Sales.y)
  ) %>%
  select(-ends_with(".x"), -ends_with(".y"))  

city_sales_grid <- expand_grid(City = unique(CitySalesfull$City),
                               Year = 2016:2024,
                               Month = 1:12)

CitySalesExpanded <- left_join(city_sales_grid, CitySalesfull, 
                               by = c("City", "Year", "Month"))


write.xlsx(CitySalesExpanded, file = "~/Documents/Executive Internship/TIF/Data/CitySalesFull(2016-2024).xlsx")


################################ For Future Use ##################################
# Retrieve CPI data 
cpi_data <- WDI(country = "US", indicator = "FP.CPI.TOTL", start = 2016, end = 2024)

# Merge CPI data with CitySales
CitySalesWithCPI <- merge(CitySalesExpanded, cpi_data, by.x = "Year", by.y = "year", all = TRUE)

write.xlsx(CitySalesWithCPI, file = "~/Documents/Executive Internship/TIF/Data/CitySalesCPI.xlsx")
##################################################################################
################################ Pivot CPI #######################################

citysales <- read_excel("~/Documents/Executive Internship/TIF/Data/CitySalesFull(2016-2024).xlsx")

cpi <- read_excel("~/Documents/Executive Internship/TIF/Data/CPI.xlsx")
 
cpi_long <- cpi %>%
  pivot_longer(cols = -Year, names_to = "Month", values_to = "CPI")

cpi_long <- cpi_long %>%
  mutate(Year = as.numeric(Year), Month = as.numeric(Month))

# Merge sales tax data with CPI data
merged_data <- citysales %>%
  left_join(cpi_long, by = c("Year", "Month"))

write.xlsx(merged_data, file = "~/Documents/Executive Internship/TIF/Data/CitySalesCPI.xlsx")

############################### Pivot Population ################################


# Load your data
df <- read_excel("~/Documents/Executive Internship/TIF/Data/CityPop.xlsx", 
                      col_types = c("text", "numeric", "numeric", 
                                    "numeric", "numeric"))

# Reshape from wide to long format
df_long <- df %>%
  pivot_longer(cols = starts_with("20"), 
               names_to = "Year", 
               values_to = "Population") %>%
  mutate(city = sub(" .*", "", city))  # Extract only the first word of the city name

write.xlsx(df_long, "~/Documents/Executive Internship/TIF/Data/CityPop(2020-2023).xlsx")

#################### merging to calculate pull factor ###########################
df_sales <- read_excel("~/Documents/Executive Internship/TIF/Data/RealCitySales.xlsx")

df_pop <- read_excel("~/Documents/Executive Internship/TIF/Data/CityPop(2020-2023).xlsx")

df_merged <- df_sales %>%
  left_join(df_pop, by = c("city", "Year"))  # Ensures no data is lost


df_merged <- merge(df_sales, df_pop, by = c("city", "Year"), all = TRUE)

write.xlsx(df_merged, "~/Documents/Executive Internship/TIF/Data/Pull_Factor.xlsx")

################## merging with med HH income ##################################

df <- read_excel("~/Documents/Executive Internship/TIF/Data/Beginning Data/County_Factors.xlsx")

df <- df %>%
  mutate(county = sub(",? County$", "", county))  # Removes ", County" but keeps full names

income <- read_excel("~/Documents/Executive Internship/TIF/Data/medHHincome.xlsx")

df_merged <- merge(df, income, by = c("County", "Year"), all = TRUE)

df_filled <- df_merged %>%
  fill(c("CountyCode", "shopping_center"), .direction = "down")

write.xlsx(df_filled, "~/Documents/Executive Internship/TIF/Data/inc1.xlsx")

############### merging with # of HH where rent is 20%+ of HH income ##############

Rent_Income <- read_excel("~/Documents/Executive Internship/TIF/Data/Beginning Data/Rent_Income.xlsx")

Rent_Income <- Rent_Income %>%
  mutate(county = sub(" .*", "", county))  # Extract only the first word of the county name

County_Factors <- read_excel("~/Documents/Executive Internship/TIF/Data/Beginning Data/County_Factors.xlsx")

Rent_Income <- Rent_Income %>%
  mutate(county = if_else(county == "El", "El Paso", county))

df_merged <- merge(df, Rent_Income, by = c("county", "year"), all = TRUE)

write.xlsx(df_merged, "~/Documents/Executive Internship/TIF/Data/County_Factors1.xlsx")

#################################################################################
####################### Merging parcel data #####################################
#################################################################################

commercial <- read_excel("Documents/Executive Internship/TIF/Data/commercial_parcels.xlsx")

city <- read_excel("Documents/Executive Internship/TIF/Data/citycommercial_parcels.xlsx")

city_subset <- city[, c("OBJECTID", "parcel_id", "dummy_intersect")]

merged_parcels <- merge(commercial, city_subset, by = c("OBJECTID", "parcel_id"), all.x = TRUE)

unique(merged_parcels$dummy_intersect)

merged_parcels$dummy_intersect[is.na(merged_parcels$dummy_intersect)] <- 0

write.xlsx(merged_parcels, "~/Documents/Executive Internship/TIF/Data/parcels_subset.xlsx")

########################### merging with land area ################################

city_area <- read_excel("Documents/Executive Internship/TIF/Data/city_area.xlsx")

county_area <- read_excel("Documents/Executive Internship/TIF/Data/county_area.xlsx")

parcels <- read_excel("Documents/Executive Internship/TIF/Data/parcels_subset.xlsx")


parcels <- merge(parcels, city_area, by = "city", all.x = TRUE)

parcels <- merge(parcels, county_area, by = "county", all.x = TRUE)

write.xlsx(parcels, "~/Documents/Executive Internship/TIF/Data/parcels_area.xlsx")

########################### merging with sales tax ################################

area <- read_excel("Documents/Executive Internship/TIF/Data/parcels_area.xlsx")

city <- read_excel("Documents/Executive Internship/TIF/Data/novcitytax.xlsx")

county <- read_excel("Documents/Executive Internship/TIF/Data/novcountytax.xlsx")

area <- merge(area, city, by = "city", all.x = TRUE)

area <- merge(area, county, by = "county", all.x = TRUE)

write.xlsx(area, "~/Documents/Executive Internship/TIF/Data/parcels_area_tax.xlsx")

####################### merging sales tax with county parcel count #################
county_sales <- read_excel("Documents/Executive Internship/TIF/Data/countysales_2024.xlsx", 
                               col_types = c("numeric", "numeric", "text", 
                                             "numeric"))
county_sales <- county_sales %>%
  group_by(County) %>%
  summarise(sales_sum = sum(sales, na.rm = TRUE))

county_count <- read_excel("Documents/Executive Internship/TIF/Data/county_count.xlsx")

county_parcel <- merge(county_sales, county_count, by = "County", all.x = TRUE)

write.xlsx(county_parcel, "~/Documents/Executive Internship/TIF/Data/county_salescount.xlsx")


####################### merging sales tax with city parcel count #################


sales_2024 <- read_excel("~/Documents/Executive Internship/TIF/Data/sales2024.xlsx")

city_parcelcount <- read_excel("~/Documents/Executive Internship/TIF/Data/city_parcelcount.xlsx")

city_parcelcount <- city_parcelcount %>%
  rename(city = NAME)

transitpoints_parcelcount <- read_excel("~/Documents/Executive Internship/TIF/Data/transitpoints_parcelcount.xlsx")

cs_parcel <- merge(sales_2024, city_parcelcount, by = "city", all.x = TRUE)

write.xlsx(cs_parcel, "~/Documents/Executive Internship/TIF/Data/city_parcelcount.xlsx")

cs_parcel_transit <- merge(transitpoints_parcelcount, cs_parcel, by = "city", all.x = TRUE)

write.xlsx(cs_parcel_transit, "~/Documents/Executive Internship/TIF/Data/CTP_parcelcount.xlsx")


####################### merging city parcel count with lightrail parcel count #################

lightrail_count <- read_excel("~/Documents/Executive Internship/TIF/Data/lightrail_count.xlsx")

cs_parcel <- read_excel("Documents/Executive Internship/TIF/Data/city_parcelcount.xlsx")

cs_parcel_lightrail <- merge(lightrail_count, cs_parcel, by = "city", all.x = TRUE)

LR_out <- merge(lightrail_count, cs_parcel, by = "city_out", all.x = TRUE)

LR_filtered <- LR_out %>% select(STOPID, sales_2024, cityID_unique)

LR_full <-  merge(cs_parcel_lightrail, LR_filtered, by = "STOPID", all.x = TRUE)

write.xlsx(LR_full, "~/Documents/Executive Internship/TIF/Data/CLR_parcelcount.xlsx")


####################### merging city parcel count with facilities parcel count #################

facilities_count <- read_excel("~/Documents/Executive Internship/TIF/Data/facilities_count.xlsx")

cs_parcel <- read_excel("Documents/Executive Internship/TIF/Data/city_parcelcount.xlsx")

cs_parcel_facilities <- merge(facilities_count, cs_parcel, by = "city", all.x = TRUE)

f_out <- merge(facilities_count, cs_parcel, by = "city_out", all.x = TRUE)

f_filtered1 <- f_out %>% select(STOPID, sales_2024, cityID_unique)

f_full1 <-  merge(cs_parcel_facilities, f_filtered1, by = "STOPID")

f_county <- merge(facilities_count, county_parcel, by = "county", all.x = TRUE)

f_filtered2 <- f_county %>% select(STOPID, sales, countyID_unique)

f_full2 <-  merge(f_full1, f_filtered2, by = "STOPID")

write.xlsx(f_full2, "~/Documents/Executive Internship/TIF/Data/CF_parcelcount.xlsx")

####################### merging city parcel count with FRPR parcel count #################

frpr_count <- read_excel("~/Documents/Executive Internship/TIF/Data/FRPR_count.xlsx")

cs_parcel <- read_excel("Documents/Executive Internship/TIF/Data/city_parcelcount.xlsx")

cs_parcel_frpr <- merge(frpr_count, cs_parcel, by = "city", all.x = TRUE)

write.xlsx(cs_parcel_frpr, "~/Documents/Executive Internship/TIF/Data/CFRPR_parcelcount.xlsx")

####################### merging city parcel count with mountain parcel count #################

m_count <- read_excel("~/Documents/Executive Internship/TIF/Data/mountain_count.xlsx")

cs_parcel <- read_excel("Documents/Executive Internship/TIF/Data/city_parcelcount.xlsx")

city_parcel <- merge(m_count, cs_parcel, by = "city", all.x = TRUE)

county_parcel <- read_excel("Documents/Executive Internship/TIF/Data/county_salescount.xlsx")

m_county <- merge(m_count, county_parcel, by = "county", all.x = TRUE)

m_filtered <- m_county %>% select(Name, sales, countyID_unique)

m_full <-  merge(city_parcel, m_filtered, by = "Name", all.x = TRUE)

write.xlsx(m_full, "~/Documents/Executive Internship/TIF/Data/CMR_parcelcount.xlsx")

####################### merging city parcel count with mobilityhub parcel count #################

mh_count <- read_excel("~/Documents/Executive Internship/TIF/Data/mobility_count.xlsx")

cs_parcel <- read_excel("Documents/Executive Internship/TIF/Data/city_parcelcount.xlsx")

cs_parcel_mh <- merge(mh_count, cs_parcel, by = "city", all.x = TRUE)

MH_out <- merge(mh_count, cs_parcel, by = "city_out", all.x = TRUE)

MH_filtered <- MH_out %>% select(MH, sales_2024, cityID_unique)

MH_full <-  merge(cs_parcel_mh, MH_filtered, by = "MH", all.x = TRUE)

write.xlsx(MH_full, "~/Documents/Executive Internship/TIF/Data/CMH_parcelcount.xlsx")










