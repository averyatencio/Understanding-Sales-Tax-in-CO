import excel "/Users/averyatencio/Documents/Executive Internship/TIF/Data/Sales_City_Report_2021_To_Date.xlsx", firstrow clear

keep if Month == 11 & Year == 2024

// getting 65+ //

import excel "/Users/averyatencio/Documents/Executive Internship/TIF/Data/CDCage.xlsx", firstrow clear

collapse (sum) Population, by(County CountyCode Year ) 

rename Population pop_65_plus  

// getting state pop //
import excel "/Users/averyatencio/Documents/Executive Internship/TIF/Data/CityPop(2020-2023).xlsx", firstrow clear

collapse (sum) Population, by(Year)

// getting state retail sales //

import excel "/Users/averyatencio/Documents/Executive Internship/TIF/Data/RealCitySales.xlsx", firstrow clear

collapse (sum) Retail_Sales, by(Year)

// calculating the pull factor //

import excel "/Users/averyatencio/Documents/Executive Internship/TIF/Data/Pull_Factor.xlsx", firstrow clear

gen pull_factor = .
replace pull_factor = pc_retail / 45097.91 if Year == 2020
replace pull_factor = pc_retail / 52000.23 if Year == 2021
replace pull_factor = pc_retail / 57704.3 if Year == 2022
replace pull_factor = pc_retail / 57571.05 if Year == 2023



