@echo off
chcp 65001 >nul
title Sistema de Procesamiento Bancario - COBOL

REM -- Agregar GnuCOBOL al PATH --
set PATH=C:\ProgramData\chocolatey\lib\gnucobol\tools\bin;%PATH%

echo ================================================
echo   SISTEMA DE PROCESAMIENTO BANCARIO - COBOL
echo   Autor: Jose David Ortega
echo ================================================
echo.

REM -- Verificar que GnuCOBOL esta instalado --
where cobc >nul 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo [ERROR] GnuCOBOL no esta instalado o no esta en el PATH.
    echo Descargalo desde: https://sourceforge.net/projects/gnucobol
    pause
    exit /b 1
)

REM -- Verificar que existe el archivo de entrada --
if not exist transacciones.dat (
    echo [ERROR] No se encontro el archivo transacciones.dat
    echo Asegurate de tener el archivo en esta carpeta.
    pause
    exit /b 1
)

echo [1/8] Compilando CARGTRX.COB...
cobc -x -o cargtrx.exe CARGTRX.COB
if %ERRORLEVEL% NEQ 0 (
    echo [ERROR] Fallo la compilacion de CARGTRX.COB
    pause
    exit /b 1
)
echo       OK

echo [2/8] Compilando CALCSALD.COB...
cobc -x -o calcsald.exe CALCSALD.COB
if %ERRORLEVEL% NEQ 0 (
    echo [ERROR] Fallo la compilacion de CALCSALD.COB
    pause
    exit /b 1
)
echo       OK

echo [3/8] Compilando GENREPORT.COB...
cobc -x -o genreport.exe GENREPORT.COB
if %ERRORLEVEL% NEQ 0 (
    echo [ERROR] Fallo la compilacion de GENREPORT.COB
    pause
    exit /b 1
)
echo       OK

echo [4/8] Compilando DETECTAN.COB...
cobc -x -o detectan.exe DETECTAN.COB
if %ERRORLEVEL% NEQ 0 (
    echo [ERROR] Fallo la compilacion de DETECTAN.COB
    pause
    exit /b 1
)
echo       OK

echo.
echo ================================================
echo   EJECUTANDO MODULOS EN ORDEN
echo ================================================
echo.

echo [5/8] Ejecutando CARGTRX - Carga y validacion...
echo ------------------------------------------------
cargtrx.exe
echo.

echo [6/8] Ejecutando CALCSALD - Calculo de saldos...
echo ------------------------------------------------
calcsald.exe
echo.

echo [7/8] Ejecutando GENREPORT - Reporte financiero...
echo ------------------------------------------------
genreport.exe
echo.

echo [8/8] Ejecutando DETECTAN - Deteccion de anomalias...
echo ------------------------------------------------
detectan.exe
echo.

echo ================================================
echo   PROCESO COMPLETADO
echo ================================================
echo.
echo Archivos generados:
echo   - trx-validas.dat       (transacciones validas)
echo   - trx-errores.dat       (transacciones con error)
echo   - saldos.dat            (saldos por cuenta)
echo   - reporte.txt           (reporte financiero)
echo   - anomalas.dat          (transacciones anomalas)
echo   - reporte-anomalas.txt  (reporte de anomalias)
echo.
pause