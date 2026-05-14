      *================================================================*
      * PROGRAMA: CONSULTAC.COB                                         *
      * DESCRIPCION: Consulta una cuenta del maestro VSAM por clave    *
      *              directa. Demuestra ACCESS MODE RANDOM              *
      * AUTOR: JOSE DAVID ORTEGA                                        *
      * FECHA: 2026                                                     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONSULTAC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAESTRO-CUENTAS
               ASSIGN TO 'cuentas-master.dat'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS MCU-CUENTA
               FILE STATUS IS WS-STATUS-MASTER.

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

       WORKING-STORAGE SECTION.
       01 WS-STATUS-MASTER  PIC XX VALUE '00'.
       01 WS-CUENTA-BUSCAR  PIC X(10).
       01 WS-OPCION         PIC X VALUE 'S'.

       01 WS-ED-SALDO       PIC -Z,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-DEB         PIC Z,ZZZ,ZZZ,ZZ9.99.
       01 WS-ED-CRE         PIC Z,ZZZ,ZZZ,ZZ9.99.

       01 WS-ESTADO-DESC    PIC X(25).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIAR
           PERFORM 2000-MENU UNTIL WS-OPCION = 'N'
           PERFORM 3000-FINALIZAR
           STOP RUN.

      *----------------------------------------------------------------*
       1000-INICIAR.
           OPEN I-O MAESTRO-CUENTAS
           IF WS-STATUS-MASTER NOT = '00'
               DISPLAY '[ERROR] No se pudo abrir cuentas-master.dat'
               DISPLAY '[ERROR] Codigo: ' WS-STATUS-MASTER
               STOP RUN
           END-IF
           DISPLAY "================================================"
           DISPLAY "   CONSULTA DE CUENTAS - ACCESO DIRECTO VSAM    "
           DISPLAY "================================================".

      *----------------------------------------------------------------*
       2000-MENU.
           DISPLAY " "
           DISPLAY "Ingrese numero de cuenta (ej: CUENTA001): "
           ACCEPT WS-CUENTA-BUSCAR
           PERFORM 2100-CONSULTAR
           DISPLAY " "
           DISPLAY "Desea consultar otra cuenta? (S/N): "
           ACCEPT WS-OPCION
           IF WS-OPCION = 'n'
               MOVE 'N' TO WS-OPCION
           END-IF.

      *----------------------------------------------------------------*
       2100-CONSULTAR.
           MOVE WS-CUENTA-BUSCAR TO MCU-CUENTA
           READ MAESTRO-CUENTAS
               INVALID KEY
                   CONTINUE
           END-READ
           IF WS-STATUS-MASTER = '23'
           DISPLAY '================================================'
           DISPLAY '  CUENTA NO ENCONTRADA: ' WS-CUENTA-BUSCAR
           DISPLAY '================================================'
           ELSE
               PERFORM 2200-MOSTRAR-CUENTA
           END-IF.

      *----------------------------------------------------------------*
       2200-MOSTRAR-CUENTA.
           MOVE MCU-SALDO     TO WS-ED-SALDO
           MOVE MCU-TOTAL-DEB TO WS-ED-DEB
           MOVE MCU-TOTAL-CRE TO WS-ED-CRE

           EVALUATE MCU-ESTADO
               WHEN 'A'
                   MOVE 'ACTIVA - SALDO POSITIVO' TO WS-ESTADO-DESC
               WHEN 'N'
                   MOVE 'EN NUMEROS ROJOS        ' TO WS-ESTADO-DESC
               WHEN 'C'
                   MOVE 'CUENTA EN CERO          ' TO WS-ESTADO-DESC
               WHEN OTHER
                   MOVE 'ESTADO DESCONOCIDO      ' TO WS-ESTADO-DESC
           END-EVALUATE

           DISPLAY "================================================"
           DISPLAY "   DATOS DE LA CUENTA                           "
           DISPLAY "================================================"
           DISPLAY "  Cuenta    : " MCU-CUENTA
           DISPLAY "  Titular   : " MCU-TITULAR
           DISPLAY "  Saldo     : " WS-ED-SALDO
           DISPLAY "  Debitos   : " WS-ED-DEB
           DISPLAY "  Creditos  : " WS-ED-CRE
           DISPLAY "  Num. Trx  : " MCU-NUM-TRX
           DISPLAY "  Estado    : " WS-ESTADO-DESC
           DISPLAY "  Fecha act.: " MCU-FECHA-ACT
           DISPLAY "================================================".

      *----------------------------------------------------------------*
       3000-FINALIZAR.
           CLOSE MAESTRO-CUENTAS
           DISPLAY " "
           DISPLAY "  Sesion de consulta finalizada."
           DISPLAY "================================================".
