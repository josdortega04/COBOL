      *===============================================================*
      * PROGRAMA: CREAVSAM.COB                                         *
      * DESCRIPCION: Crea y carga el archivo maestro VSAM de cuentas   *
      *              Lee saldos.dat generado por CALCSALD y crea       *
      *              el archivo indexado por numero de cuenta          *
      * AUTOR: JOSE DAVID ORTEGA                                       *
      * FECHA: 2026                                                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREAVSAM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-- Archivo de entrada (salida de CALCSALD) --*
           SELECT ARCHIVO-SALDOS
               ASSIGN TO 'saldos.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS-SALDOS.

      *-- Archivo maestro VSAM indexado --*
           SELECT MAESTRO-CUENTAS
               ASSIGN TO 'cuentas-master.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS MCU-CUENTA
               FILE STATUS IS WS-STATUS-MASTER.

       DATA DIVISION.
       FILE SECTION.

      *-- Estructura del archivo de saldos --*
       FD ARCHIVO-SALDOS.
       01 REG-SALDO.
           05 SAL-CUENTA        PIC X(10).
           05 SAL-TOTAL-DEB     PIC 9(10)V99.
           05 SAL-TOTAL-CRE     PIC 9(10)V99.
           05 SAL-SALDO-FINAL   PIC S9(10)V99.
           05 SAL-NUM-TRX       PIC 9(06).

      *-- Estructura del archivo maestro VSAM --*
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

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS.
           05 WS-STATUS-SALDOS  PIC XX VALUE '00'.
           05 WS-STATUS-MASTER  PIC XX VALUE '00'.

       01 WS-EOF            PIC X VALUE 'N'.
       01 WS-CONT-LEIDOS    PIC 9(6) VALUE ZEROS.
       01 WS-CONT-GRABADOS  PIC 9(6) VALUE ZEROS.
       01 WS-CONT-ERRORES   PIC 9(6) VALUE ZEROS.
       01 WS-FECHA-HOY      PIC 9(8) VALUE 20260513.

      *-- Tabla de titulares de prueba --*
       01 WS-TITULARES.
           05 WS-TIT-1  PIC X(30) VALUE 'CARLOS RODRIGUEZ PEREZ   '.
           05 WS-TIT-2  PIC X(30) VALUE 'MARIA GONZALEZ LOPEZ     '.
           05 WS-TIT-3  PIC X(30) VALUE 'JOSE DAVID ORTEGA        '.
           05 WS-TIT-4  PIC X(30) VALUE 'ANA PATRICIA VILLA       '.
           05 WS-TIT-5  PIC X(30) VALUE 'LUIS FERNANDO CASTRO     '.
           05 WS-TIT-6  PIC X(30) VALUE 'DIANA MARCELA RIOS       '.
           05 WS-TIT-7  PIC X(30) VALUE 'PEDRO ANTONIO MORA       '.
           05 WS-TIT-8  PIC X(30) VALUE 'LAURA SOFIA MENDEZ       '.
           05 WS-TIT-9  PIC X(30) VALUE 'ANDRES FELIPE TORRES     '.
           05 WS-TIT-10 PIC X(30) VALUE 'PATRICIA ELENA SUAREZ    '.

       01 WS-TABLA-TIT REDEFINES WS-TITULARES.
           05 WS-TIT PIC X(30) OCCURS 10 TIMES.

       01 WS-INDICE         PIC 9(2) VALUE 1.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIAR
           PERFORM 2000-PROCESAR UNTIL WS-EOF = 'S'
           PERFORM 3000-FINALIZAR
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INICIAR.
           OPEN INPUT ARCHIVO-SALDOS
           IF WS-STATUS-SALDOS NOT = '00'
               DISPLAY '[ERROR] No se pudo abrir saldos.dat'
               DISPLAY '[ERROR] Codigo: ' WS-STATUS-SALDOS
               STOP RUN
           END-IF

           OPEN OUTPUT MAESTRO-CUENTAS
           IF WS-STATUS-MASTER NOT = '00'
               DISPLAY '[ERROR] No se pudo crear cuentas-master.dat'
               DISPLAY '[ERROR] Codigo: ' WS-STATUS-MASTER
               STOP RUN
           END-IF

           READ ARCHIVO-SALDOS
               AT END MOVE 'S' TO WS-EOF
           END-READ
           IF WS-EOF = 'S'
               DISPLAY '[ERROR] Archivo saldos.dat vacio'
               STOP RUN
           END-IF.

      *----------------------------------------------------------------*
       2000-PROCESAR.
           ADD 1 TO WS-CONT-LEIDOS

           PERFORM 2100-ARMAR-REGISTRO
           PERFORM 2200-GRABAR-MAESTRO

           IF WS-INDICE < 10
               ADD 1 TO WS-INDICE
           ELSE
               MOVE 1 TO WS-INDICE
           END-IF

           READ ARCHIVO-SALDOS
               AT END MOVE 'S' TO WS-EOF
           END-READ.

      *----------------------------------------------------------------*
       2100-ARMAR-REGISTRO.
           MOVE SAL-CUENTA      TO MCU-CUENTA
           MOVE SAL-SALDO-FINAL TO MCU-SALDO
           MOVE SAL-TOTAL-DEB   TO MCU-TOTAL-DEB
           MOVE SAL-TOTAL-CRE   TO MCU-TOTAL-CRE
           MOVE SAL-NUM-TRX     TO MCU-NUM-TRX
           MOVE WS-FECHA-HOY    TO MCU-FECHA-ACT
           MOVE WS-TIT(WS-INDICE) TO MCU-TITULAR

      *-- Determinar estado de la cuenta --*
           EVALUATE TRUE
               WHEN SAL-SALDO-FINAL > ZEROS
                   MOVE 'A' TO MCU-ESTADO
               WHEN SAL-SALDO-FINAL < ZEROS
                   MOVE 'N' TO MCU-ESTADO
               WHEN OTHER
                   MOVE 'C' TO MCU-ESTADO
           END-EVALUATE.

      *----------------------------------------------------------------*
       2200-GRABAR-MAESTRO.
           WRITE REG-MAESTRO
           IF WS-STATUS-MASTER = '00'
               ADD 1 TO WS-CONT-GRABADOS
           ELSE IF WS-STATUS-MASTER = '22'
               DISPLAY '[AVISO] Cuenta duplicada: ' MCU-CUENTA
               ADD 1 TO WS-CONT-ERRORES
           ELSE
               DISPLAY '[ERROR] Al grabar cuenta: ' MCU-CUENTA
               DISPLAY '[ERROR] Codigo: ' WS-STATUS-MASTER
               ADD 1 TO WS-CONT-ERRORES
           END-IF.

      *----------------------------------------------------------------*
       3000-FINALIZAR.
           DISPLAY "================================================"
           DISPLAY "   CARGA DE ARCHIVO MAESTRO VSAM                "
           DISPLAY "================================================"
           DISPLAY "  Registros leidos  : " WS-CONT-LEIDOS
           DISPLAY "  Registros grabados: " WS-CONT-GRABADOS
           DISPLAY "  Errores           : " WS-CONT-ERRORES
           DISPLAY "================================================"
           DISPLAY "  Archivo creado: cuentas-master.dat            "
           DISPLAY "================================================"
           CLOSE ARCHIVO-SALDOS
           CLOSE MAESTRO-CUENTAS.