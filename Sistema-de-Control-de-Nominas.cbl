       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONVENIO-CAMIONEROS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

           01 CATEGORY-MESSAGE        PIC X(40) VALUE "Seleccione cat:".
           01 CATEGORY-1-MESSAGE    PIC X(40) VALUE "1.de Primera Cat.".
           01 CATEGORY-2-MESSAGE    PIC X(40) VALUE "2.de Segunda Cat.".
           01 EXIT-MESSAGE     PIC X(30) VALUE "3. Salir del programa.".
           01 INVALID-INPUT-MESSAGE PIC X(30) VALUE "Entrada invalida.".
           01 DAYS-WORKED-MESSAGE   PIC X(30) VALUE "Dias trabajados: ".
           01 OVERTIME-50-MESSAGE        PIC X(30) VALUE "extras 50%: ".
           01 OVERTIME-100-MESSAGE      PIC X(30) VALUE "extras 100%: ".
           01 YEARS-OF-SERVICE-MESSAGE   PIC X(30) VALUE "Antiguedad: ".
           01 VIANDAS-MESSAGE        PIC X(40) VALUE "Total viandas: $".
           01 VIAT-ESP-MESSAGE PIC X(40) VALUE "Viaticos especiales: $".
           01 TOTAL-SALARY-MESSAGE PIC X(40) VALUE "Sueldo total es: $".

           01 CATEGORY-SELECTION      PIC 9.
           01 BASIC-SALARY            PIC 9(7)V99.
           01 DAYS-WORKED             PIC 9(3).
           01 OVERTIME-HOURS-50       PIC 9(3).
           01 OVERTIME-HOURS-100      PIC 9(3).
           01 YEARS-OF-SERVICE        PIC 9(2).
           01 TOTAL-VIANDAS           PIC 9(7)V99.
           01 TOTAL-VIAT-ESP          PIC 9(7)V99.
           01 TOTAL-OVERTIME-50       PIC 9(7)V99.
           01 TOTAL-OVERTIME-100      PIC 9(7)V99.
           01 BONUS                   PIC 9(7)V99.
           01 TOTAL-SALARY            PIC 9(7)V99.
           01 DEDUCTIONS              PIC 9(7)V99.
           01 NET-SALARY              PIC 9(7)V99.

           01 VIANDA-DIARIA           PIC 9(7)V99 VALUE 9802.03.
           01 VIAT-ESP                PIC 9(7)V99 VALUE 4918.62.

           01 CATEGORY-1-BASIC        PIC 9(7)V99 VALUE 616861.82.
           01 CATEGORY-1-OVERTIME-50  PIC 9(5)V99 VALUE 5004.56.
           01 CATEGORY-1-OVERTIME-100 PIC 9(5)V99 VALUE 6672.79.

           01 CATEGORY-2-BASIC        PIC 9(7)V99 VALUE 605869.11.
           01 CATEGORY-2-OVERTIME-50  PIC 9(5)V99 VALUE 4915.40.
           01 CATEGORY-2-OVERTIME-100 PIC 9(5)V99 VALUE 6553.87.

       PROCEDURE DIVISION.
       INICIO-PROGRAMA.

           DISPLAY "=================================================="
           DISPLAY "             CONVENIO CAMIONEROS COBOL            "
           DISPLAY "==================================================".

       MAIN-LOOP.

           DISPLAY CATEGORY-MESSAGE
           DISPLAY CATEGORY-1-MESSAGE
           DISPLAY CATEGORY-2-MESSAGE
           DISPLAY EXIT-MESSAGE

           ACCEPT CATEGORY-SELECTION

           EVALUATE CATEGORY-SELECTION
               WHEN 1
                   SET BASIC-SALARY TO CATEGORY-1-BASIC
                   PERFORM CALCULATE-SALARY
               WHEN 2
                   SET BASIC-SALARY TO CATEGORY-2-BASIC
                   PERFORM CALCULATE-SALARY
               WHEN 3
                   DISPLAY "Saliendo del programa. Gracias."
                   STOP RUN
               WHEN OTHER
                   DISPLAY INVALID-INPUT-MESSAGE
                   PERFORM MAIN-LOOP
           END-EVALUATE.

       CALCULATE-SALARY.
           DISPLAY DAYS-WORKED-MESSAGE
           ACCEPT DAYS-WORKED

           DISPLAY OVERTIME-50-MESSAGE
           ACCEPT OVERTIME-HOURS-50

           DISPLAY OVERTIME-100-MESSAGE
           ACCEPT OVERTIME-HOURS-100

           DISPLAY YEARS-OF-SERVICE-MESSAGE
           ACCEPT YEARS-OF-SERVICE

           *> Calcular Viandas por día trabajado
           COMPUTE TOTAL-VIANDAS = DAYS-WORKED * VIANDA-DIARIA

           *> Calcular Viáticos Especiales por día trabajado
           COMPUTE TOTAL-VIAT-ESP = DAYS-WORKED * VIAT-ESP

           *> Calcular Horas Extras al 50%
           IF CATEGORY-SELECTION = 1
               COMPUTE TOTAL-OVERTIME-50 = OVERTIME-HOURS-50 * 5004.56
               COMPUTE TOTAL-OVERTIME-100 = OVERTIME-HOURS-100 * 6672.79
           ELSE
               COMPUTE TOTAL-OVERTIME-50 = OVERTIME-HOURS-50 * 4915.40
               COMPUTE TOTAL-OVERTIME-100 = OVERTIME-HOURS-100 * 6553.87
           END-IF

           *> Calcular 20% adicional sobre el sueldo básico
           COMPUTE BONUS = BASIC-SALARY * 0.20

           *> Calcular Sueldo con Antigüedad
           COMPUTE BASIC-SALARY = BASIC-SALARY + (BASIC-SALARY *
           YEARS-OF-SERVICE / 100)

           *> Calcular Deducciones: Jubilación y Obra Social
           COMPUTE DEDUCTIONS = BASIC-SALARY * 0.03 * 2

           *> Calcular Sueldo Total
           COMPUTE TOTAL-SALARY = BASIC-SALARY + TOTAL-VIANDAS +
           TOTAL-VIAT-ESP + TOTAL-OVERTIME-50 + TOTAL-OVERTIME-100 +
           BONUS

           *> Calcular Salario Neto (después de deducciones)
           COMPUTE NET-SALARY = TOTAL-SALARY - DEDUCTIONS

           *> Mostrar Desglose del Sueldo
           DISPLAY "=================================================="
           DISPLAY "            DETALLE DEL SUELDO CALCULADO          "
           DISPLAY "=================================================="
           DISPLAY "Sueldo Basico (con antiguedad): $", BASIC-SALARY
           DISPLAY "Descuento Jubilación y Obra Social: $", DEDUCTIONS
           DISPLAY VIANDAS-MESSAGE, TOTAL-VIANDAS
           DISPLAY VIAT-ESP-MESSAGE, TOTAL-VIAT-ESP
           DISPLAY "Total Horas Extras 50%: $", TOTAL-OVERTIME-50
           DISPLAY "Total Horas Extras 100%: $", TOTAL-OVERTIME-100
           DISPLAY "Presentismo 20% del Sueldo Básico: $", BONUS
           DISPLAY TOTAL-SALARY-MESSAGE, NET-SALARY
           DISPLAY "==================================================".

           PERFORM MAIN-LOOP.

           STOP RUN.
