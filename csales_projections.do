********************************************************************************
******************************* Projections ************************************
********************************************************************************

import excel "/Users/averyatencio/Documents/Executive Internship/TIF/Data/sales_expanded.xlsx", firstrow clear


* Create numeric ID for city
encode city, gen(city_id)

* Create centered year variable (centered on base year, e.g. 2016)
gen Year_c = Year - 2016

* Generate log(revenue), if not already created
gen lrevenue = .
replace lrevenue = log(rev) if rev > 0

* Create variables to store predicted values
gen linear_pred = .
gen projected_revenue = .

* Loop over each city
levelsof city_id, local(cities)

foreach c in `cities' {
    
    * Restrict to historical years for model fitting
    reg lrevenue Year_c if city_id == `c' & Year <= 2024
    
    * Store coefficients
    scalar intercept = _b[_cons]
    scalar slope = _b[Year_c]
    
    * Apply model to ALL years for this city
    replace linear_pred = intercept + slope * Year_c if city_id == `c'
    replace projected_revenue = exp(linear_pred) if city_id == `c'
}


* Adjust this to include the cities you want to compare
twoway (line projected_revenue Year if city == "Denver", lcolor(red)) ///
       (line rev Year if city == "Denver", lcolor(blue)) ///
       (line projected_revenue Year if city == "Boulder", lcolor(red) lpattern(dash)) ///
       (line rev Year if city == "Boulder", lcolor(blue) lpattern(dash)) ///
       (line projected_revenue Year if city == "Aurora", lcolor(red) lpattern(dot)) ///
       (line rev Year if city == "Aurora", lcolor(blue) lpattern(dot)), ///
       xtitle(Year) ///
       ytitle("City Sales Tax Revenue") ///
       legend(order(1 "Denver Predicted" 2 "Denver Actual" ///
                    3 "Boulder Predicted" 4 "Boulder Actual" ///
                    5 "Aurora Predicted" 6 "Aurora Actual")) ///
       title("Predicted vs Actual City Sales Tax Revenue for Select Cities")


collapse (sum) rev projected_revenue, by(Year)

twoway (line projected_revenue Year, lcolor(red)) ///
       (line rev Year, lcolor(blue)), ///
       xtitle(Year) ///
       ytitle("Total City Sales Tax Revenue") ///
       legend(order(1 "Predicted" 2 "Actual")) ///
       title("Predicted vs Actual Total City Sales Tax Revenue Across Cities")


******************************* county projections *******************************
import excel "/Users/averyatencio/Documents/Executive Internship/TIF/Data/county_expanded.xlsx", firstrow clear


* Create numeric ID for city
encode County, gen(county_id)

* Create centered year variable (centered on base year, e.g. 2016)
gen Year_c = Year - 2016

* Generate log(revenue), if not already created
gen lrevenue = .
replace lrevenue = log(rev_sum) if rev > 0

* Create variables to store predicted values
gen linear_pred = .
gen projected_revenue = .


* Loop over each city
levelsof county_id, local(Counties)

foreach c in `Counties' {
    
    * Restrict to historical years for model fitting
    reg lrevenue Year_c if county_id == `c' & Year <= 2024
    
    * Store coefficients
    scalar intercept = _b[_cons]
    scalar slope = _b[Year_c]
    
    * Apply model to ALL years for this city
    replace linear_pred = intercept + slope * Year_c if county_id == `c'
    replace projected_revenue = exp(linear_pred) if county_id == `c'
}


* Adjust this to include the cities you want to compare
twoway (line projected_revenue Year if County == "Denver", lcolor(red)) ///
       (line rev Year if County == "Denver", lcolor(blue)) ///
       (line projected_revenue Year if County == "Arapahoe", lcolor(red) lpattern(dash)) ///
       (line rev Year if County == "Arapahoe", lcolor(blue) lpattern(dash)) ///
       (line projected_revenue Year if County == "Douglas", lcolor(red) lpattern(dot)) ///
       (line rev Year if County == "Douglas", lcolor(blue) lpattern(dot)), ///
       xtitle(Year) ///
       ytitle("County Sales Tax Revenue") ///
       legend(order(1 "Denver Predicted" 2 "Denver Actual" ///
                    3 "Arapahoe Predicted" 4 "Arapahoe Actual" ///
                    5 "Douglas Predicted" 6 "Douglas Actual")) ///
       title("Predicted vs Actual County Sales Tax Revenue for Select Counties")


collapse (sum) rev_sum projected_revenue, by(Year)

twoway (line projected_revenue Year, lcolor(red)) ///
       (line rev_sum Year, lcolor(blue)), ///
       xtitle(Year) ///
       ytitle("Total County Sales Tax Revenue") ///
       legend(order(1 "Predicted" 2 "Actual")) ///
       title("Predicted vs Actual Total County Sales Tax Revenue Across Counties")




