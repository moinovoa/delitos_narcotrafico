# Medidas DAX del proyecto 
### Suma Conteo
* **Descripción** Suma del conteo de carpetas de investigación
* **Fórmula DAX**
`Suma_Conteo = SUM('Fuero_federal_2012-2026_feb2026'[CONTEO]`
### Color_columna
* **Descripción** Contar carpetas x mes de cada año para cada delito
* **Fórmula DAX**
```Color_Columna =
VAR ValorActual = [Suma_Conteo]

--- Iteramos sobre los meses de cada TIPO para encontrar el mínimo y máximo real de la columna
VAR MinColumna = CALCULATE(
    MINX(VALUES('Fuero_federal_2012-2026_feb2026'[MES]), [Suma_Conteo]), 
    ALLEXCEPT('Fuero_federal_2012-2026_feb2026', 'Fuero_federal_2012-2026_feb2026'[TIPO])
)
VAR MaxColumna = CALCULATE(
    MAXX(VALUES('Fuero_federal_2012-2026_feb2026'[MES]), [Suma_Conteo]), 
    ALLEXCEPT('Fuero_federal_2012-2026_feb2026', 'Fuero_federal_2012-2026_feb2026'[TIPO])
)

--- Normalizamos el valor de 0 a 1 de forma independiente por columna
RETURN 
    IF(
        MaxColumna = MinColumna, 
        0, 
        DIVIDE(ValorActual - MinColumna, MaxColumna - MinColumna)
    )
```