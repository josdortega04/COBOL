      *================================================================*
      * PROGRAMA: REPORTVSM.COB                                         *
      * DESCRIPCION: Genera reporte completo leyendo el maestro VSAM   *
      *              secuencialmente. Muestra el estado actual de       *
      *              todas las cuentas despues de las actualizaciones   *
      * AUTOR: JOSE DAVID ORTEGA                                        *
      * FECHA: 2026                                                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTVSM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAESTRO-CUENTAS
               ASSIGN TO 'cuentas-master.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS MCU-CUENTA
               FILE STATUS IS WS-STATUS-MASTER.

           SELECT ARCHIVO-REPORTE
               ASSIGN TO 'reporte-vsam.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS-REP.

       DATA DIVISION.
       FILE SECTION.

       FD MAESTRO-CUENTAS.
       01 REG-MAESTRO.
           05 MCU-CUENTA        PIC X(10).
           05 MCU-TITULAR       PIC X(30).
           05 MCU-SALDO         PIC S9(10)V99.
           05 MCU-TOTAL-DEB     PIC 9(10)V99.
           05 MCU-TOTAL-CRE     PIC 9(10)V99.
           05 MCU-NUM-TRX       PIC 9(06).
           05 MCU-ESTADO        PIC X(01).
           05 MCU-FECHA-ACT     PIC 9(08).

       FD ARCHIVO-REPORTE.
       01 REG-LINEA             PIC X(100).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS.
           05 WS-STATUS-MASTER  PIC XX VALUE '00'.
           05 WS-STATUS-REP     PIC XX VALUE '00'.

       01 WS-EOF                PIC X VALUE 'N'.

       01 WS-TOTALES.
           05 WS-TOT-CUENTAS    PIC 9(6)      VALUE ZEROS.
           05 WS-TOT-ACTIVAS    PIC 9(6)      VALUE ZEROS.
           05 WS-TOT-NEGATIVAS  PIC 9(6)      VALUE ZEROS.
           05 WS-TOT-CERO       PIC 9(6)      VALUE ZEROS.
           05 WS-TOT-SALDO      PIC S9(12)V99 VALUE ZEROS.
           05 WS-TOT-DEB        PIC 9(12)V99  VALUE ZEROS.
           05 WS-TOT-CRE        PIC 9(12)V99  VALUE ZEROS.
           05 WS-TOT-TRX        PIC 9(8)      VALUE ZEROS.

       01 WS-ED-SALDO           PIC -Z,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-DEB             PIC Z,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-CRE             PIC Z,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-TOT-SALDO       PIC -Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-TOT-DEB         PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-TOT-CRE         PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.

       01 WS-SEPARADOR          PIC X(80)
           VALUE '================================================'.
       01 WS-LINEA              PIC X(100).
       01 WS-ESTADO-DESC         PIC X(25).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIAR
           PERFORM 2000-ENCABEZADO
           PERFORM 3000-PROCESAR UNTIL WS-EOF = 'S'
           PERFORM 4000-RESUMEN-EJECUTIVO
           PERFORM 5000-FINALIZAR
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INICIAR.
           OPEN INPUT MAESTRO-CUENTAS
           IF WS-STATUS-MASTER NOT = '00'
               DISPLAY '[ERROR] No se pudo abrir cuentas-master.dat'
               DISPLAY '[ERROR] Codigo: ' WS-STATUS-MASTER
               STOP RUN
           END-IF

           OPEN OUTPUT ARCHIVO-REPORTE
           IF WS-STATUS-REP NOT = '00'
           DISPLAY '[ERROR] No se pudo crear reporte-vsam.txt'
               STOP RUN
           END-IF

           READ MAESTRO-CUENTAS NEXT RECORD
               AT END MOVE 'S' TO WS-EOF
           END-READ
           IF WS-EOF = 'S'
               DISPLAY '[ERROR] Archivo maestro vacio'
               STOP RUN
           END-IF.

      *----------------------------------------------------------------*
       2000-ENCABEZADO.
           WRITE REG-LINEA FROM WS-SEPARADOR
           MOVE '        BANCO SISTEMA - ESTADO DE CUENTAS VSAM'
               TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           MOVE '        REPORTE POST-ACTUALIZACION DE SALDOS'
               TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           WRITE REG-LINEA FROM WS-SEPARADOR

           MOVE '  CUENTA     TITULAR                        SALDO'
               TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           MOVE '             DEBITOS          CREDITOS       TRX  EST'
               TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           MOVE '------------------------------------------------'
               TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA.

      *----------------------------------------------------------------*
       3000-PROCESAR.
           ADD 1              TO WS-TOT-CUENTAS
           ADD MCU-SALDO      TO WS-TOT-SALDO
           ADD MCU-TOTAL-DEB  TO WS-TOT-DEB
           ADD MCU-TOTAL-CRE  TO WS-TOT-CRE
           ADD MCU-NUM-TRX    TO WS-TOT-TRX

           EVALUATE MCU-ESTADO
               WHEN 'A'
                   ADD 1 TO WS-TOT-ACTIVAS
                   MOVE 'ACTIVA    ' TO WS-ESTADO-DESC
               WHEN 'N'
                   ADD 1 TO WS-TOT-NEGATIVAS
                   MOVE 'NEG.ROJOS ' TO WS-ESTADO-DESC
               WHEN 'C'
                   ADD 1 TO WS-TOT-CERO
                   MOVE 'EN CERO   ' TO WS-ESTADO-DESC
               WHEN OTHER
                   MOVE SPACES TO WS-ESTADO-DESC
           END-EVALUATE

           MOVE MCU-SALDO     TO WS-ED-SALDO
           MOVE MCU-TOTAL-DEB TO WS-ED-DEB
           MOVE MCU-TOTAL-CRE TO WS-ED-CRE

           STRING '  ' DELIMITED BY SIZE
                  MCU-CUENTA DELIMITED BY SIZE
                  '  ' DELIMITED BY SIZE
                  MCU-TITULAR DELIMITED BY SIZE
                  '  ' DELIMITED BY SIZE
                  WS-ED-SALDO DELIMITED BY SIZE
               INTO WS-LINEA
           END-STRING
           WRITE REG-LINEA FROM WS-LINEA

           STRING '             ' DELIMITED BY SIZE
                  WS-ED-DEB DELIMITED BY SIZE
                  '  ' DELIMITED BY SIZE
                  WS-ED-CRE DELIMITED BY SIZE
                  '  ' DELIMITED BY SIZE
                  MCU-NUM-TRX DELIMITED BY SIZE
                  '  ' DELIMITED BY SIZE
                  WS-ESTADO-DESC DELIMITED BY SIZE
               INTO WS-LINEA
           END-STRING
           WRITE REG-LINEA FROM WS-LINEA

           MOVE SPACES TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           READ MAESTRO-CUENTAS NEXT RECORD
               AT END MOVE 'S' TO WS-EOF
           END-READ.

      *----------------------------------------------------------------*
       4000-RESUMEN-EJECUTIVO.
           MOVE WS-TOT-SALDO  TO WS-ED-TOT-SALDO
           MOVE WS-TOT-DEB    TO WS-ED-TOT-DEB
           MOVE WS-TOT-CRE    TO WS-ED-TOT-CRE

           WRITE REG-LINEA FROM WS-SEPARADOR
           MOVE '        RESUMEN EJECUTIVO - ESTADO ACTUAL'
               TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA
           WRITE REG-LINEA FROM WS-SEPARADOR

           STRING '  Total cuentas     : '
                  DELIMITED BY SIZE
                  WS-TOT-CUENTAS DELIMITED BY SIZE
               INTO WS-LINEA
           END-STRING
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Cuentas activas   : '
                  DELIMITED BY SIZE
                  WS-TOT-ACTIVAS DELIMITED BY SIZE
               INTO WS-LINEA
           END-STRING
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Cuentas negativas : '
                  DELIMITED BY SIZE
                  WS-TOT-NEGATIVAS DELIMITED BY SIZE
               INTO WS-LINEA
           END-STRING
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Cuentas en cero   : '
                  DELIMITED BY SIZE
                  WS-TOT-CERO DELIMITED BY SIZE
               INTO WS-LINEA
           END-STRING
           WRITE REG-LINEA FROM WS-LINEA

           MOVE SPACES TO WS-LINEA
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Total debitos     : '
                  DELIMITED BY SIZE
                  WS-ED-TOT-DEB DELIMITED BY SIZE
               INTO WS-LINEA
           END-STRING
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Total creditos    : '
                  DELIMITED BY SIZE
                  WS-ED-TOT-CRE DELIMITED BY SIZE
               INTO WS-LINEA
           END-STRING
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Saldo consolidado : '
                  DELIMITED BY SIZE
                  WS-ED-TOT-SALDO DELIMITED BY SIZE
               INTO WS-LINEA
           END-STRING
           WRITE REG-LINEA FROM WS-LINEA

           STRING '  Total trx         : '
                  DELIMITED BY SIZE
                  WS-TOT-TRX DELIMITED BY SIZE
               INTO WS-LINEA
           END-STRING
           WRITE REG-LINEA FROM WS-LINEA

           WRITE REG-LINEA FROM WS-SEPARADOR

           DISPLAY '================================================'
           DISPLAY '   RESUMEN EJECUTIVO - ESTADO ACTUAL VSAM       '
           DISPLAY '================================================'
           DISPLAY '  Total cuentas     : ' WS-TOT-CUENTAS
           DISPLAY '  Cuentas activas   : ' WS-TOT-ACTIVAS
           DISPLAY '  Cuentas negativas : ' WS-TOT-NEGATIVAS
           DISPLAY '  Cuentas en cero   : ' WS-TOT-CERO
           DISPLAY '  Total debitos     : ' WS-ED-TOT-DEB
           DISPLAY '  Total creditos    : ' WS-ED-TOT-CRE
           DISPLAY '  Saldo consolidado : ' WS-ED-TOT-SALDO
           DISPLAY '  Total trx         : ' WS-TOT-TRX
           DISPLAY '================================================'
           DISPLAY '  Reporte generado  : reporte-vsam.txt          '
           DISPLAY '================================================'.

      *----------------------------------------------------------------*
       5000-FINALIZAR.
           CLOSE MAESTRO-CUENTAS
           CLOSE ARCHIVO-REPORTE.
