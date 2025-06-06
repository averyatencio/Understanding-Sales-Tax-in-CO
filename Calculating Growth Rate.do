********************************************************************************
************************* Calculating Growth Rate ******************************
********************************************************************************

// Calculating Real Sales //

import excel "/Users/averyatencio/Documents/Executive Internship/TIF/Data/CitySalesCPI.xlsx", firstrow clear

rename City city
rename Year year
rename Month month
rename State_Net_Taxable_Sales sales_tax_revenue


// Store the 2024 CPI values by month
levelsof month if year == 2024, local(months)  // Get unique months
foreach m of local months {
    sum CPI if year == 2024 & month == `m', meanonly
    local CPI_2024_`m' = r(mean)  // Save 2024 CPI for that month
}

gen real_sales_tax = .
foreach m of local months {
    replace real_sales_tax = sales_tax_revenue * (`CPI_2024_`m'' / CPI) if year < 2024 & month == `m'
}


replace real_sales_tax = sales_tax_revenue if year == 2024


// Calculating Real Growth Rate //

keep if month == 11

keep if year == 2016 | year == 2024

sort city year

gen real_sales_2016 = .
gen real_sales_2024 = .

bysort city (year): replace real_sales_2016 = real_sales_tax if year == 2016
bysort city (year): replace real_sales_2024 = real_sales_tax if year == 2024


collapse (mean) real_sales_2016 real_sales_2024, by(city)

// Compute growth rate: ((2024 value / 2016 value) - 1) * 100
gen growth_rate = ((real_sales_2024 / real_sales_2016) - 1) * 100 if real_sales_2016 > 0

list city real_sales_2016 real_sales_2024 growth_rate if real_sales_2016 > 0 & real_sales_2024 > 0, sep(0)

// Calculating Nominal Growth Rate //
keep if month == 11

keep if year == 2016 | year == 2024

sort city year

gen sales_2016 = .
gen sales_2024 = .

bysort city (year): replace sales_2016 = sales_tax_revenue if year == 2016
bysort city (year): replace sales_2024 = sales_tax_revenue if year == 2024


collapse (mean) sales_2016 sales_2024, by(city)

// Compute growth rate: ((2024 value / 2016 value) - 1) * 100
gen growth_rate = ((sales_2024 / sales_2016) - 1) * 100 if sales_2016 > 0

list city real_sales_2016 real_sales_2024 growth_rate if real_sales_2016 > 0 & real_sales_2024 > 0, sep(0)
