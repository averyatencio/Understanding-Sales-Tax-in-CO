import delimited "/Users/averyatencio/Documents/output.csv", clear

sort zoning_descrip
list zoning_descrip if _n == 1 | zoning_descrip != zoning_descrip[_n-1]

preserve
keep zoning_descrip
duplicates drop
export delimited using "unique_zoning.csv", replace
restore

preserve
keep zoning_code zoning_descrip tax_code tax_descrip
duplicates drop
export delimited using "unique_zoning.csv", replace
restore

**************************** calculatiing tax rev ******************************

import excel "/Users/averyatencio/Documents/Executive Internship/TIF/Data/parcels_area_tax.xlsx", firstrow clear

gen parcel_tax_rev = .

replace parcel_tax_rev = (landSqft / city_area) * city_sales_tax if dummy_intersect == 1

replace parcel_tax_rev = (landSqft / county_area) * county_sales_tax if dummy_intersect == 0

********************************** tables *************************************

import excel "/Users/averyatencio/Documents/Executive Internship/TIF/Data/parcel_rev.xlsx", firstrow clear

