* *****************************************************************
* ARCHIVO: Taller3_EcAv_P1_3b_3c_5a
* AUTORES: Marlon Angulo Ramos // Carlos Agamez
* FECHA: 24/10/2025
* DESCRIPCIÓN: Primer ejercicio, TWFE DNP
* *****************************************************************

*******************************************************************************
* 							PRIMER EJERCICIO
*******************************************************************************

clear all
ssc install bacondecomp, replace
ssc install csdid, replace
ssc install drdid, replace

cd "C:\Users\Marlon Angulo\Desktop\Maestría Andes\Econometría Avanzada\Taller 3\Punto 1"
use Empresas_Sim, clear

* 3b) Modelo 1
xtset id year
xtreg y_1 treat i.year, fe vce(cluster id)

* 3c) Modelo 2  
xtreg y_2 treat i.year, fe vce(cluster id)

* 5a) Metodología Callaway & Sant'Anna

* Estimar modelo CS
csdid y_2, ivar(id) time(year) gvar(treat_date) method(dripw)
* Obtener efectos dinámicos (Event Study)
csdid_estat event

* Guardar resultados en log
di "Pre_avg: " %9.4f -0.0002962 " (p = 0.960)"
di "Post_avg: " %9.4f 1.057901 " (p = 0.000)"
di ""
di "Efectos por período:"
di "Tm5 (k=-5): " %9.4f 0.00038 " (p = 0.990)"
di "Tm4 (k=-4): " %9.4f -0.0104595 " (p = 0.730)" 
di "Tm3 (k=-3): " %9.4f 0.0212086 " (p = 0.315)"
di "Tm2 (k=-2): " %9.4f 0.0024173 " (p = 0.909)"
di "Tm1 (k=-1): " %9.4f -0.0150274 " (p = 0.356)"
di "Tp0 (k=0): " %9.4f 0.3247758 " (p = 0.000)"
di "Tp1 (k=1): " %9.4f 0.6135675 " (p = 0.000)"
di "Tp2 (k=2): " %9.4f 0.9135009 " (p = 0.000)"
di "Tp3 (k=3): " %9.4f 1.18247 " (p = 0.000)"
di "Tp4 (k=4): " %9.4f 1.49911 " (p = 0.000)"
di "Tp5 (k=5): " %9.4f 1.813983 " (p = 0.000)"

* COMPARACIÓN CON TWFE (DEL PUNTO 4b)

di _newline(2)
di "=== COMPARACIÓN CON TWFE ==="
di "TWFE - Efectos pre-tratamiento (k < 0):"
di "k=-8: " %9.4f 2.1900 " (significativo)"
di "k=-7: " %9.4f 1.8535 " (significativo)"
di "k=-6: " %9.4f 1.5666 " (significativo)"
di "k=-5: " %9.4f 1.2280 " (significativo)"
di "k=-4: " %9.4f 0.9417 " (significativo)"
di "k=-3: " %9.4f 0.6292 " (significativo)"
di "k=-2: " %9.4f 0.3347 " (significativo)"
di ""
di "TWFE - Efectos post-tratamiento (k >= 0):"
di "k=0: " %9.4f 0.0283 " (no significativo)"
di "k=1: " %9.4f -0.0073 " (no significativo)"
di "k=2: " %9.4f 0.0178 " (no significativo)"
di "k=3: " %9.4f -0.0382 " (significativo)"
di "k=4: " %9.4f 0.0042 " (no significativo)"
di "k=5: " %9.4f -0.0410 " (significativo)"

* CREACIÓN DEL GRÁFICO COMPARATIVO - USANDO RESULTADOS EXISTENTES
clear
set obs 17  // -8 a -2, 0 a 5 (omitimos -1)

* Crear variable de tiempo relativo
gen k = _n - 9  // -8 a 8
replace k = . if k == -1  // Omitir referencia
drop if k > 5 | k < -8   // Mantener solo -8 a 5

* INSERTAR COEFICIENTES DE CALLAWAY & SANT'ANNA (DE LOS RESULTADOS)
gen cs_att = .
gen cs_se = .

* Períodos pre-tratamiento CS (k = -5 a -1) - Event Study
replace cs_att =  0.00038 if k == -5
replace cs_se  =  0.0311078 if k == -5

replace cs_att = -0.0104595 if k == -4
replace cs_se  =  0.0303282 if k == -4

replace cs_att =  0.0212086 if k == -3
replace cs_se  =  0.0211001 if k == -3

replace cs_att =  0.0024173 if k == -2
replace cs_se  =  0.0210378 if k == -2

replace cs_att = -0.0150274 if k == -1
replace cs_se  =  0.0162786 if k == -1

* Períodos post-tratamiento CS (k = 0 a 5) - Event Study
replace cs_att =  0.3247758 if k == 0
replace cs_se  =  0.016175 if k == 0

replace cs_att =  0.6135675 if k == 1
replace cs_se  =  0.0169489 if k == 1

replace cs_att =  0.9135009 if k == 2
replace cs_se  =  0.0213874 if k == 2

replace cs_att =  1.18247 if k == 3
replace cs_se  =  0.0217237 if k == 3

replace cs_att =  1.49911 if k == 4
replace cs_se  =  0.0341532 if k == 4

replace cs_att =  1.813983 if k == 5
replace cs_se  =  0.035334 if k == 5

* INSERTAR COEFICIENTES TWFE (DEL PUNTO 4b)
gen twfe_att = .
gen twfe_se = .

* Períodos pre-tratamiento TWFE
replace twfe_att =  2.1900 if k == -8
replace twfe_se  =  0.0490 if k == -8

replace twfe_att =  1.8535 if k == -7
replace twfe_se  =  0.0315 if k == -7

replace twfe_att =  1.5666 if k == -6
replace twfe_se  =  0.0413 if k == -6

replace twfe_att =  1.2280 if k == -5
replace twfe_se  =  0.0226 if k == -5

replace twfe_att =  0.9417 if k == -4
replace twfe_se  =  0.0352 if k == -4

replace twfe_att =  0.6292 if k == -3
replace twfe_se  =  0.0154 if k == -3

replace twfe_att =  0.3347 if k == -2
replace twfe_se  =  0.0295 if k == -2

* Períodos post-tratamiento TWFE
replace twfe_att =  0.0283 if k == 0
replace twfe_se  =  0.0249 if k == 0

replace twfe_att = -0.0073 if k == 1
replace twfe_se  =  0.0117 if k == 1

replace twfe_att =  0.0178 if k == 2
replace twfe_se  =  0.0220 if k == 2

replace twfe_att = -0.0382 if k == 3
replace twfe_se  =  0.0146 if k == 3

replace twfe_att =  0.0042 if k == 4
replace twfe_se  =  0.0205 if k == 4

replace twfe_att = -0.0410 if k == 5
replace twfe_se  =  0.0182 if k == 5

* CALCULAR INTERVALOS DE CONFIANZA
gen cs_ci_lower = cs_att - 1.96 * cs_se
gen cs_ci_upper = cs_att + 1.96 * cs_se

gen twfe_ci_lower = twfe_att - 1.96 * twfe_se
gen twfe_ci_upper = twfe_att + 1.96 * twfe_se

* CREAR EL GRÁFICO COMPARATIVO
twoway (rarea cs_ci_lower cs_ci_upper k, color(blue%30)) ///
       (line cs_att k, color(blue) lwidth(medthick)) ///
       (scatter cs_att k, color(blue) msymbol(O) msize(medium)) ///
       (rarea twfe_ci_lower twfe_ci_upper k, color(red%30)) ///
       (line twfe_att k, color(red) lwidth(medthick)) ///
       (scatter twfe_att k, color(red) msymbol(Oh) msize(medium)), ///
       xline(0, lpattern(dash) lcolor(black)) ///
       xlabel(-8(1)5) ///
       ytitle("Coeficiente") ///
       ylabel(-1(0.5)2.5, angle(horizontal)) ///
       xtitle("Periodos relativos al tratamiento (k)") ///
       legend(order(2 "Callaway & Sant'Anna" 5 "TWFE") pos(6) row(1)) ///
       title("Comparación: Callaway & Sant'Anna vs TWFE") ///
       subtitle("Efectos dinámicos del tratamiento - Modelo 2") ///
       note("Referencia omitida: k = -1" "Intervalos de confianza al 95%")

* TABLA COMPARATIVA
forvalues i = -8/5 {
    if `i' != -1 {
        local cs_att_val = cs_att[k == `i']
        local cs_se_val = cs_se[k == `i']
        local twfe_att_val = twfe_att[k == `i']
        local twfe_se_val = twfe_se[k == `i']
        
        di "`i'    " %7.4f `cs_att_val' "   " %7.4f `cs_se_val' "   " ///
           %7.4f `twfe_att_val' "   " %7.4f `twfe_se_val'
    }
}