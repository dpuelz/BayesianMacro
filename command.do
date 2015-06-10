#delimit;
clear;
clear mata;
clear matrix;
set more off;
set memory 800000;
set matsize 11000;
set maxvar 10000;

*import excel "R:\Kent\Projects\Japan\Depreciation.Exports\quarterly\quarterly.new.xlsx", sheet("hardcode") firstrow;
*save "R:\Kent\Projects\Japan\Depreciation.Exports\quarterly\quarterly.dta", replace;

*log using "R:\Kent\Projects\Japan\Depreciation.Exports\quarterly\command.txt", replace text;
use "R:\Kent\Projects\Japan\Depreciation.Exports\quarterly\quarterly.dta", clear;
cd "R:\Kent\Projects\Japan\Depreciation.Exports\quarterly\";

destring date, replace;

gen ifs_x = ifs_i*1000;

gen ifs_c = ifs_x + ifs_j;

xtset ifs_c d, quarterly;

gen exp_share = exp / exp_wld * 100;

gen exp_lcl = exp*fx_j;
gen exp_an_j = (exp_lcl+L.exp_lcl+L2.exp_lcl+L3.exp_lcl);
gen exp_shr_an = (exp_share+L.exp_share+L2.exp_share+L3.exp_share);
gen gdp_an_j = (gdp_j+L.gdp_j+L2.gdp_j+L3.gdp_j);
gen gdp_an_i = (gdp_i+L.gdp_i+L2.gdp_i+L3.gdp_i);
gen comp = reer_i/reer_j;
gen fx = fx_i/fx_j;


gen dgdp_i4 = (gdp_i/L4.gdp_i-1)*100;
gen dgdp_i5 = (gdp_i/L5.gdp_i-1)*100;
gen dgdp_i6 = (gdp_i/L6.gdp_i-1)*100;

gen dgdp_j4 = (gdp_j/L4.gdp_j-1)*100;
gen dgdp_j5 = (gdp_j/L5.gdp_j-1)*100;
gen dgdp_j6 = (gdp_j/L6.gdp_j-1)*100;

gen dgdp_an_i4 = (gdp_an_i/L4.gdp_an_i-1)*100;
gen dgdp_an_i5 = (gdp_an_i/L5.gdp_an_i-1)*100;
gen dgdp_an_i6 = (gdp_an_i/L6.gdp_an_i-1)*100;

gen dgdp_an_j4 = (gdp_an_j/L4.gdp_an_j-1)*100;
gen dgdp_an_j5 = (gdp_an_j/L5.gdp_an_j-1)*100;
gen dgdp_an_j6 = (gdp_an_j/L6.gdp_an_j-1)*100;

gen dfx4 = (1-(fx / L4.fx))*100;
gen dfx5 = (1-(fx / L5.fx))*100;
gen dfx6 = (1-(fx / L6.fx))*100;

gen dcomp4 = (comp-L4.comp);
gen dcomp5 = (comp-L5.comp);
gen dcomp6 = (comp-L6.comp);

gen dexp4 = (exp/L4.exp-1)*100;
gen dexp5 = (exp/L5.exp-1)*100;
gen dexp6 = (exp/L6.exp-1)*100;

gen dexp_an4 = (exp_an/L4.exp_an-1)*100;
gen dexp_an5 = (exp_an/L5.exp_an-1)*100;
gen dexp_an6 = (exp_an/L6.exp_an-1)*100;

gen dexp_shr_an4 = exp_shr_an - L4.exp_shr_an;
gen dexp_shr_an5 = exp_shr_an - L5.exp_shr_an;
gen dexp_shr_an6 = exp_shr_an - L6.exp_shr_an;

*gen reldgdp = (dgdp_j-dgdp_i);
gen relpppgdppc = gdpppppc_j/gdpppppc_i*100;
gen grav_j = pop_j*distw;
gen forex_gdp = forex_j / ngdp_usd * 100;
gen grav2_j = pop_j*distw*gdp_an_j;

gen L1dfx4 = L1.dfx4;
gen L2dfx4 = L2.dfx4;
gen L3dfx4 = L3.dfx4;
gen L4dfx4 = L4.dfx4;

gen L1dcomp1 = L1.dcomp4;
gen L2dcomp2 = L2.dcomp4;
gen L3dcomp3 = L3.dcomp4;
gen L4dcomp4 = L4.dcomp4;

gen fxeffect = .;

local controllist relpppgdppc tariff_bilateral forex_gdp gatt_j rta emu_j pop_j distw;

local mylist fxeffect dgdp_an_j4 dcomp4;

local timelist tdum*;

gen Q0 = 1;
gen Q1 = 0;
replace Q1 = 1 if quarter=="Q1";
gen Q2 = 0;
replace Q2 = 1 if quarter=="Q2";
gen Q3 = 0;
replace Q3 = 1 if quarter=="Q3";
gen Q4 = 0;
replace Q4 = 1 if quarter=="Q4";

gen GBR = 0;
replace GBR = 1 if ifs_j==112;
gen JAP = 0;
replace JAP = 1 if ifs_j==158;
gen KOR = 0;
replace KOR = 1 if ifs_j==542;
gen ALL = 1;



tab d, gen(tdum);

foreach var3 of varlist ALL GBR JAP KOR {;

	foreach var2 of varlist Q0 Q1 Q2 Q3 Q4 {;

			foreach var1 of varlist L1dfx4 L2dfx4 L3dfx4 L4dfx4 {;
			replace fxeffect = `var1';
				xtreg dexp_shr_an6 `mylist' `controllist' `timelist' if `var2'==1, fe;
				estimates store xx`var1'`var2'`var3', title(`var3' `var1' `var2');
		};
	};
};

label variable dexp_an4 "Change in annual exports";
label variable dgdp_an_j4 "Change in partner annual GDP";
*label variable dfx "Change in bilateral ER";
label variable fxeffect "Change in FX, various lags";
label variable L1dfx4 "1Q Lag YoY Change in bilateral ER";
label variable L2dfx4 "2Q Lag YoY Change in bilateral ER";
label variable L3dfx4 "3Q Lag YoY Change in bilateral ER";
label variable L4dfx4 "4Q Lag YoY Change in bilateral ER";
label variable dcomp4 "YoY Change in relative competitiveness";
*label variable reldgdp "Difference in GDP growth and partner GDP growth";
label variable relpppgdppc "Relative PPP GDP PC of partner country";
label variable grav_j "Population x distance";
*label variable tariff_pref "Partner import tariff avg minus bilateral tariff";
label variable forex_gdp "Foreign exchange reserves % GDP";
label variable contig "Contiguous";
label variable comlang_off "No common language";
label variable colony "Partner country a colony?";
label variable gatt_j "GATT/WTO membership";
label variable eu_j "EU membership";
label variable emu_j "EMU membership";
label variable tariff_bilateral "Partner import tariff";
label variable pop_j "Population of partner country";
label variable distw "Distance";
label variable rta "Regional FTA";



estout xx* using RegressionsShare.xls, replace note("Robust standard errors in parentheses") cells(b(star fmt(%9.2f)) se) 
 keep(`mylist' `controllist')
 stats(r2 N, fmt(%9.3g %9.1f) labels("R-squared" "Observations")) legend label collabels(none) starlevels(* 0.1 ** 0.05 *** 0.01) 
 
;


