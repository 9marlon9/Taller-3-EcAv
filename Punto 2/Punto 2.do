*----------------------------------------------
* TALLER: Evaluación del programa de incentivos
* Autor: Marlon  Angulo & Carlos Agamez
*----------------------------------------------

*----------------------------------------------
* Punto 2 - Análisis descriptivo
*----------------------------------------------


*Cargamos la base 
use "C:/Users/cdam4\Downloads/CS_data.dta", clear

* Declarar estructura panel ciudad–tiempo
xtset city_id date, monthly

* Revisar estructura
describe
summarize

*------------------------------------------------------
* (a) Ganancias totales por ciudad
*------------------------------------------------------

* Línea vertical: inicio del tratamiento (abril 2024)
** se mostrara una linea roja que indica el inicio del tratamiento

local treat_month = ym(2024,4)

twoway ///
(line revenue date if city_id != 8, lcolor(gs12) lwidth(thin)) ///
(line revenue date if city_id == 8, lcolor(black*1.3) lwidth(medthick)) ///
, ///
legend(order(2 "Ciudad 8 (tratada)" 1 "Otras ciudades") ///
       ring(0) pos(1) region(lstyle(none))) ///
title("Ganancias totales por ciudad (2018–2025)", size(medlarge)) ///
note("Línea roja: inicio del programa piloto (abril 2024)") ///
ytitle("Ganancias totales") ///
xtitle("Fecha (mensual)") ///
xline(`treat_month', lpattern(dash) lcolor(red)) ///
scheme(s1color)


*------------------------------------------------------
* (b) Logaritmo natural de las ganancias
*------------------------------------------------------

capture drop ln_revenue
gen ln_revenue = ln(revenue) if revenue > 0

local treat_month = ym(2024,4)

twoway ///
    (line ln_revenue date if city_id != 8, lcolor(gs12) lwidth(thin)) ///
    (line ln_revenue date if city_id == 8, lcolor(black*1.3) lwidth(medthick)) ///
    , ///
    legend(order(2 "Ciudad 8 (tratada)" 1 "Otras ciudades") ///
           ring(0) pos(1) region(lstyle(none))) ///
    title("Logaritmo natural de las ganancias por ciudad (2018–2025)", size(medlarge)) ///
    note("Línea roja: inicio del programa piloto (abril 2024)") ///
    ytitle("ln(Ganancias)") ///
    xtitle("Fecha (mensual)") ///
    xline(`treat_month', lpattern(dash) lcolor(red)) ///
    scheme(s1color)

*------------------------------------------------------
* (c) Accidentes per cápita normalizados por ciudad
*------------------------------------------------------
local treat_month = ym(2024,4)

twoway ///
(line accidents_std date if city_id != 8, lcolor(gs12) lwidth(thin)) ///
(line accidents_std date if city_id == 8, lcolor(black) lwidth(medthick)) ///
, ///
xline(`treat_month', lpattern(dash) lcolor(red) lwidth(medthick)) ///
legend(order(2 "Ciudad 8 (tratada)" 1 "Otras ciudades") ///
       ring(0) pos(1) region(lstyle(none))) ///
title("Accidentes per cápita (normalizados) por ciudad (2018–2025)", size(medlarge)) ///
note("La línea roja indica el inicio del programa piloto (abril 2024).") ///
ytitle("Accidentes (z-score)") ///
xtitle("Fecha (mensual)") ///
scheme(s1color)


*------------------------------------------------------
* PUNTO 5: 
* a) CONTROL SINTÉTICO PARA LA CIUDAD 8
* Metodología de Abadie y Gardeazabal (2003)
*------------------------------------------------------

* crear id 
egen city_num = group(city_id)

* estructura de panel 
tsset city_num month_number

* generamos los predictores 	
capture drop ln_revenue drivers_pre income_pre trips_pre acc_pre lnrev_pre

gen ln_revenue = ln(revenue)

bys city_num: egen drivers_pre = mean(drivers_active) if inrange(month_number, 76, 78)
bys city_num: egen income_pre  = mean(avg_income_per_hour) if inrange(month_number, 76, 78)
bys city_num: egen trips_pre   = mean(total_trips) if inrange(month_number, 76, 78)
bys city_num: egen acc_pre     = mean(accidents_std) if inrange(month_number, 76, 78)
bys city_num: egen lnrev_pre   = mean(ln_revenue) if inrange(month_number, 76, 78)


* control sintetico
synth ln_revenue drivers_pre income_pre trips_pre acc_pre lnrev_pre, ///
      trunit(8) trperiod(79) ///
      keep("C:/Users/cdam4/Downloads/synth_revenue.dta", replace) ///
      figure


* balance de predictores entre la ciudad tratada y la sintética
matrix list e(X_balance)

* Guardar la matriz de balance en un nombre más simple
matrix balance = e(X_balance)

* Verificar
matrix list balance

*------------------------------------------------------
* B) Estimación alternativa a la de Abadie (2003)
*------------------------------------------------------

* Crear logaritmo
capture drop ln_revenue
gen ln_revenue = ln(revenue)

* Conservar solo lo necesario para el reshape
keep city_id date ln_revenue
order city_id date ln_revenue
sort date city_id

* Convertir de long → wide
reshape wide ln_revenue, i(date) j(city_id)

* Definir fecha de tratamiento
local treat_month = 771

* Estimar regresión OLS SOLO con datos pre-tratamiento
reg ln_revenue8 ///
    ln_revenue1 ln_revenue2 ln_revenue3 ln_revenue4 ln_revenue5 ///
    ln_revenue6 ln_revenue7 ln_revenue9 ln_revenue10 ln_revenue11 ///
    ln_revenue12 ln_revenue13 ln_revenue14 ln_revenue15 ///
    if date < `treat_month'

*** Ejecutar el codigo del punto B en bloque de aqui hacia arriba**

* Listar todas las variables ln_revenue
ds ln_revenue*
local allvars `r(varlist)'

* Quitar la ciudad tratada (8)
local controls : list allvars - ln_revenue8

* Mostrar para verificar que el macro se creó
di "Variables de control: `controls'"

matrix b = e(b)'
matrix list b

*------------------------------------------------------
* PUNTO 6: Efecto del programa sobre los accidentes
*------------------------------------------------------

*cargamos nuevamente la base
use "C:/Users/cdam4\Downloads/CS_data.dta", clear

* Ver qué ciudad es la tratada
tab city_id if treated == 1

* Ver cuándo empieza el tratamiento
tab month_number if treated == 1
list month_number date if treated == 1

* Crear un identificador numérico para las ciudades (synth lo necesita)
egen city_num = group(city_id)

* Configurar como datos de panel
tsset city_num month_number

* Usar synth con las covariables del mes de enero 2024 (month_number = 78)
	
synth accidents_std drivers_active avg_income_per_hour total_trips accidents_std, ///
    trunit(8) trperiod(79) xperiod(78) ///
    keep("C:/Users/cdam4/Downloads/synth_accidents.dta", replace) ///
    figure


*tabla de pesos estimados 

use results_accidents, clear
list _Co_Number _W_Weight, clean

* Crear y guardar la gráfica - Punto a)
synth_plot
graph export "accidents_counterfactual.png", replace
graph save "accidents_counterfactual.gph", replace

*---------------------------------------------------
* PUNTO 7 – PANEL 2×1 DE EFECTOS ESTIMADOS (SYNTH)
*---------------------------------------------------

*-----------------------------------------------*
*  GRÁFICA 1: EFECTO SOBRE REVENUE (LOG)        *
*-----------------------------------------------

use "C:/Users/cdam4/Downloads/synth_revenue.dta", clear
describe

gen effect_revenue = _Y_treated - _Y_synthetic
label var effect_revenue "Efecto estimado en revenue"

* Generar el efecto
gen effect_revenue = _Y_treated - _Y_synthetic

twoway ///
    (line effect_revenue _time, ///
        lcolor(black) lwidth(medium)) ///
    , ///
    ytitle("Efecto estimado", size(medsmall)) ///
    xtitle("Tiempo", size(medsmall)) ///
    title("Efecto estimado sobre ln(Revenue)", size(medium)) ///
    graphregion(color(white)) ///
    bgcolor(white) ///
    legend(off) ///
    name(g_rev, replace)
*-----------------------------------------------*
*  GRÁFICA 2: EFECTO SOBRE ACCIDENTES           *
*-----------------------------------------------*

use "C:/Users/cdam4/Downloads/synth_accidents.dta", clear

* Generar el efecto
gen effect_accidents = _Y_treated - _Y_synthetic

twoway ///
    (line effect_accidents _time, ///
        lcolor(red) lwidth(medium)) ///
    , ///
    ytitle("Efecto estimado", size(medsmall)) ///
    xtitle("Tiempo", size(medsmall)) ///
    title("Efecto estimado sobre accidentes", size(medium)) ///
    graphregion(color(white)) ///
    bgcolor(white) ///
    legend(off) ///
    name(g_acc, replace)
*-----------------------------------------------*
*  PANEL 2 × 1                                  *
*-----------------------------------------------*

graph combine g_rev g_acc, ///
    col(2) ///
    title("Efectos estimados del programa (synth)") ///
    scheme(s1color)

use "C:/Users/cdam4/Downloads/synth_accidents.dta", clear

* Calcular error (observado – sintético)
gen error_acc = _Y_treated - _Y_synthetic

* Periodo pretratamiento (month_number <= 78)
keep if _time <= 78

* RMSPE = raíz del promedio de errores cuadrados
summ error_acc
gen sq_error = error_acc^2
sum sq_error
scalar RMSPE_synth_acc = sqrt(r(mean))

display "RMSPE (synth) para accidentes = " RMSPE_synth_acc


* Cargar base original
use "C:/Users/cdam4/Downloads/CS_data.dta", clear

* Crear logaritmo si no existe
capture confirm variable ln_revenue
if _rc gen ln_revenue = ln(revenue) if revenue>0


*****************************************************
* 1. ----- RMSPE de la especificación SYNTH ---------
*****************************************************

* Cargar archivo ya guardado del synth de accidentes
use "C:/Users/cdam4/Downloads/synth_accidents.dta", clear

* RMSPE pretratamiento
sum _Y_treated _Y_synthetic if _time < 75

gen sq_error = (_Y_treated - _Y_synthetic)^2
sum sq_error if _time < 75

scalar RMSPE_synth_acc = sqrt(r(mean))
display "RMSPE (synth accidentes) = " RMSPE_synth_acc


*****************************************************
* 2. ----- PESOS POR REGRESIÓN (MCO) ----------------
*****************************************************

* Volver a la base original
use "C:/Users/cdam4/Downloads/CS_data.dta", clear

* Mantenemos únicamente lo necesario
keep city_id date accidents_std

* Reshape largo -> ancho
reshape wide accidents_std, i(date) j(city_id)

* Definir mes de tratamiento
local treat_month = ym(2024,4)

* Filtrar pre-tratamiento
reg accidents_std8 accidents_std1 accidents_std2 accidents_std3 ///
    accidents_std4 accidents_std5 accidents_std6 accidents_std7 ///
    accidents_std9 accidents_std10 accidents_std11 accidents_std12 ///
    accidents_std13 accidents_std14 accidents_std15 ///
    if date < `treat_month'

*** Ejecutar el codigo desde mantener unicamente lo necesario hasta filtrar **


* Guardar coeficientes (pesos MCO)
matrix b = e(b)'
matrix list b

*****************************************************
* 3. --- Calcular RMSPE MCO -------------------------
*****************************************************
ereturn list

predict acc_hat8 if date < `treat_month'
replace acc_hat8 = . if date >= `treat_month'


*****************************************************
* 2. ----- PESOS POR REGRESIÓN (MCO) ----------------
*****************************************************

* Volver a la base original
use "C:/Users/cdam4/Downloads/CS_data.dta", clear

* Mantenemos únicamente lo necesario
keep city_id date accidents_std

* Reshape largo -> ancho
reshape wide accidents_std, i(date) j(city_id)

* Definir mes de tratamiento
local treat_month = ym(2024,4)

* Filtrar pre-tratamiento y estimar regresión
reg accidents_std8 accidents_std1 accidents_std2 accidents_std3 ///
    accidents_std4 accidents_std5 accidents_std6 accidents_std7 ///
    accidents_std9 accidents_std10 accidents_std11 accidents_std12 ///
    accidents_std13 accidents_std14 accidents_std15 ///
    if date < `treat_month'

* Guardar coeficientes (pesos MCO)
matrix b = e(b)'
matrix list b	

* Obtener valores predichos
predict y_hat if e(sample)

* Calcular error cuadrático
gen sq_error_mco = (accidents_std8 - y_hat)^2 if e(sample)

* Calcular RMSPE MCO
sum sq_error_mco if e(sample)
scalar RMSPE_mco_acc = sqrt(r(mean))
display "RMSPE (MCO accidentes) = " RMSPE_mco_acc

* Mostrar tabla comparativa
display "COMPARACIÓN DE RMSPE - ACCIDENTES"
display "Synth: " RMSPE_synth_acc
display "MCO:    " RMSPE_mco_acc

* Crear tabla de comparación
matrix RMSPE_comparison = (RMSPE_synth_acc, RMSPE_mco_acc)
matrix colnames RMSPE_comparison = "Synth" "MCO"
matrix rownames RMSPE_comparison = "RMSPE_Accidentes"

matrix list RMSPE_comparison

* También mostrar los pesos de regresión
matrix list e(b)

***********************************************
*          PUNTO 9 - ANÁLISIS DE PLACEBO
*          Para Revenue y Accidents_std
***********************************************

* ----------------------------------------------------
* PARTE 1: PLACEBO PARA REVENUE (GANANCIAS)
* ----------------------------------------------------

* 1.1 Cargar datos y efecto real para Ciudad 8
use "C:/Users/cdam4/Downloads/CS_data.dta", clear
tsset city_id month_number

synth revenue drivers_active avg_income_per_hour total_trips accidents_std ///
    , trunit(8) trperiod(76) keep("C:/Users/cdam4/Downloads/synth_real_revenue.dta", replace)

use "C:/Users/cdam4/Downloads/synth_real_revenue.dta", clear
gen city_id = 8
rename _time month_number
gen effect = _Y_treated - _Y_synthetic
keep city_id month_number effect
save "C:/Users/cdam4/Downloads/efecto_real_revenue.dta", replace

* 1.2 Placebos para todas las ciudades de control
clear
set obs 0
gen city_id = .
gen month_number = .
gen effect = .
save "C:/Users/cdam4/Downloads/results_placebos_revenue.dta", replace

foreach c in 1 2 3 4 5 6 7 9 10 11 12 13 14 15 {
    display "Procesando revenue - Ciudad: `c'"
    
    use "C:/Users/cdam4/Downloads/CS_data.dta", clear
    tsset city_id month_number

    synth revenue drivers_active avg_income_per_hour total_trips accidents_std ///
        , trunit(`c') trperiod(76) keep("C:/Users/cdam4/Downloads/synth_temp.dta", replace)

    use "C:/Users/cdam4/Downloads/synth_temp.dta", clear
    gen city_id = `c'
    rename _time month_number
    gen effect = _Y_treated - _Y_synthetic
    keep city_id month_number effect
    
    use "C:/Users/cdam4/Downloads/results_placebos_revenue.dta", clear
    append using "C:/Users/cdam4/Downloads/synth_temp.dta"
    save "C:/Users/cdam4/Downloads/results_placebos_revenue.dta", replace
}

* 1.3 Combinar y calcular p-valores CORREGIDOS para revenue
use "C:/Users/cdam4/Downloads/results_placebos_revenue.dta", clear
append using "C:/Users/cdam4/Downloads/efecto_real_revenue.dta"

display "=========================================="
display "P-VALORES CORREGIDOS - REVENUE"
display "Periodo | Efecto Real | P-valor"
display "--------------------------------"

forvalues t = 76/81 {
    * Efecto real de Ciudad 8
    qui sum effect if city_id == 8 & month_number == `t'
    local real_effect = r(mean)
    
    * Contar placebos con efecto más extremo (considerando signo)
    if `real_effect' < 0 {
        qui count if city_id != 8 & month_number == `t' & effect < `real_effect'
    }
    else {
        qui count if city_id != 8 & month_number == `t' & effect > `real_effect'
    }
    
    local pval = r(N) / 14
    display "   `t'   |  `real_effect' |  `pval'"
}

* 1.4 Gráfica para revenue con línea del efecto real
preserve
keep if month_number == 81

* Efecto real para periodo 81
sum effect if city_id == 8
local real_line = r(mean)

twoway (histogram effect if city_id != 8, color(gs12) bin(12) frequency) ///
       (function y = 0, range(`real_line' `real_line') lcolor(red) lwidth(3)), ///
       title("Prueba de Placebo - Revenue") ///
       xtitle("Efecto del Tratamiento") ///
       ytitle("Número de Ciudades") ///
       legend(off) ///
       text(3 `real_line' "Ciudad 8", color(red) size(small)) ///
       graphregion(color(white))
graph export "placebo_revenue_final.png", replace
restore

* ----------------------------------------------------
* PARTE 2: PLACEBO PARA ACCIDENTES_STD
* ----------------------------------------------------

* 2.1 Efecto real para Ciudad 8
use "C:/Users/cdam4/Downloads/CS_data.dta", clear
tsset city_id month_number

synth accidents_std drivers_active avg_income_per_hour total_trips accidents_std ///
    , trunit(8) trperiod(76) keep("C:/Users/cdam4/Downloads/synth_real_accidents.dta", replace)

use "C:/Users/cdam4/Downloads/synth_real_accidents.dta", clear
gen city_id = 8
rename _time month_number
gen effect = _Y_treated - _Y_synthetic
keep city_id month_number effect
save "C:/Users/cdam4/Downloads/efecto_real_accidents.dta", replace

* 2.2 Placebos para accidentes
clear
set obs 0
gen city_id = .
gen month_number = .
gen effect = .
save "C:/Users/cdam4/Downloads/results_placebos_accidents.dta", replace

foreach c in 1 2 3 4 5 6 7 9 10 11 12 13 14 15 {
    display "Procesando accidents - Ciudad: `c'"
    
    use "C:/Users/cdam4/Downloads/CS_data.dta", clear
    tsset city_id month_number

    synth accidents_std drivers_active avg_income_per_hour total_trips accidents_std ///
        , trunit(`c') trperiod(76) keep("C:/Users/cdam4/Downloads/synth_temp.dta", replace)

    use "C:/Users/cdam4/Downloads/synth_temp.dta", clear
    gen city_id = `c'
    rename _time month_number
    gen effect = _Y_treated - _Y_synthetic
    keep city_id month_number effect
    
    use "C:/Users/cdam4/Downloads/results_placebos_accidents.dta", clear
    append using "C:/Users/cdam4/Downloads/synth_temp.dta"
    save "C:/Users/cdam4/Downloads/results_placebos_accidents.dta", replace
}

* 2.3 Combinar y calcular p-valores CORREGIDOS para accidents
use "C:/Users/cdam4/Downloads/results_placebos_accidents.dta", clear
append using "C:/Users/cdam4/Downloads/efecto_real_accidents.dta"

display ""
display "=========================================="
display "P-VALORES CORREGIDOS - ACCIDENTES_STD"
display "Periodo | Efecto Real | P-valor"
display "--------------------------------"

forvalues t = 76/81 {
    * Efecto real de Ciudad 8
    qui sum effect if city_id == 8 & month_number == `t'
    local real_effect = r(mean)
    
    * Contar placebos con efecto más extremo (considerando signo)
    if `real_effect' < 0 {
        qui count if city_id != 8 & month_number == `t' & effect < `real_effect'
    }
    else {
        qui count if city_id != 8 & month_number == `t' & effect > `real_effect'
    }
    
    local pval = r(N) / 14
    display "   `t'   |  `real_effect' |  `pval'"
}

* 2.4 Gráfica para accidents con línea del efecto real
preserve
keep if month_number == 81

* Efecto real para periodo 81
sum effect if city_id == 8
local real_line = r(mean)

twoway (histogram effect if city_id != 8, color(gs12) bin(12) frequency) ///
       (function y = 0, range(`real_line' `real_line') lcolor(red) lwidth(3)), ///
       title("Prueba de Placebo - Accidents") ///
       xtitle("Efecto del Tratamiento") ///
       ytitle("Número de Ciudades") ///
       legend(off) ///
       text(3 `real_line' "Ciudad 8", color(red) size(small)) ///
       graphregion(color(white))
graph export "placebo_accidents_final.png", replace
restore

* GRÁFICA SIMPLE Y FUNCIONAL
preserve
keep if month_number == 81

* Efecto real para periodo 81
sum effect if city_id == 8
local real_line = r(mean)

* Histograma básico con línea vertical
histogram effect if city_id != 8, color(gs12) bin(12) frequency ///
    title("Prueba de Placebo - Revenue") ///
    xtitle("Efecto del Tratamiento") ///
    ytitle("Número de Ciudades") ///
    graphregion(color(white)) ///
    addplot(pci 0 `real_line' 8 `real_line', lcolor(red) lwidth(3))

graph export "placebo_revenue_final.png", replace
restore



* GRÁFICA ROBUSTA - SIN ERRORES
preserve
keep if month_number == 81

* Verificar los datos primero
summarize effect if city_id != 8, detail
summarize effect if city_id == 8

* Calcular rango para la línea
local real_effect = effect[1]  // Efecto de Ciudad 8

* Gráfica simple que siempre funciona
histogram effect if city_id != 8, color(gs12) bin(10) frequency ///
    title("Prueba de Placebo - Revenue") ///
    xtitle("Efecto del Tratamiento") ///
    ytitle("Número de Ciudades") ///
    graphregion(color(white)) ///
    name(placebo_plot, replace)

* Agregar línea manualmente después
twoway (histogram effect if city_id != 8, color(gs12) bin(10) frequency) ///
       , title("Prueba de Placebo - Revenue") ///
       xtitle("Efecto del Tratamiento") ///
       ytitle("Número de Ciudades") ///
       graphregion(color(white)) ///
       xline(`real_effect', lcolor(red) lwidth(3)) ///
       text(5 `real_effect' "Ciudad 8", color(red) size(small))

graph export "placebo_revenue_final.png", replace
restore
