# Sistema de Procesamiento Bancario por Lotes — COBOL

Este proyecto nació de una necesidad concreta: entender cómo funcionan por dentro los sistemas que mueven el dinero de millones de personas todos los días. La mayoría de los bancos en América Latina y el mundo siguen corriendo sobre COBOL, no porque no haya alternativas, sino porque funciona, y funciona bien. Quise construir algo que reflejara esa realidad.

---

## ¿Qué hace este sistema?

Procesa transacciones bancarias en modo batch — es decir, en lotes, sin intervención humana, como ocurre cada noche en los centros de datos de cualquier banco. El flujo completo va desde la carga del archivo de transacciones hasta la detección de movimientos sospechosos, pasando por el cálculo de saldos y la generación de reportes.

No es un CRUD. No tiene interfaz gráfica. Es procesamiento de datos financieros puro, que es exactamente lo que hace COBOL en producción.

---

## Módulos

### CARGTRX.COB — Carga y validación
El punto de entrada del sistema. Lee el archivo de transacciones, valida cada registro contra reglas básicas de integridad (cuenta no vacía, tipo válido, monto mayor a cero) y separa los registros en dos salidas: los que pasan y los que no. Lleva contadores de todo lo que entra y lo que sale.

### CALCSALD.COB — Cálculo de saldos
Toma las transacciones válidas y calcula el saldo de cada cuenta usando control de corte — un patrón clásico del procesamiento batch donde los registros vienen ordenados y se acumulan por grupo hasta que el grupo cambia. Distingue débitos de créditos, maneja números con signo para saldos negativos y genera un archivo de saldos consolidado por cuenta.

### GENREPORT.COB — Reportería financiera
Genera un reporte en texto plano con el detalle de cada cuenta: débitos totales, créditos totales, saldo final y número de transacciones. Al final incluye un resumen ejecutivo con los totales generales y una clasificación de cuentas por estado (positivo, negativo, en cero). El reporte sale tanto en archivo como en pantalla.

### DETECTAN.COB — Detección de anomalías
El módulo más interesante. Aplica tres reglas de negocio sobre cada transacción: detecta montos que superan un límite configurable, identifica secuencias de débitos consecutivos en la misma cuenta (patrón común en fraudes) y marca posibles duplicados comparando cuenta, monto, fecha y tipo contra la transacción anterior. Las reglas están declaradas como variables, no hardcodeadas, para que sean fáciles de ajustar.

---

## Flujo del sistema

```
transacciones.dat
      |
  CARGTRX.COB ---------> trx-errores.dat
      |
  trx-validas.dat
      |
  CALCSALD.COB
      |
  saldos.dat
      |
  GENREPORT.COB --------> reporte.txt
      |
  DETECTAN.COB ---------> anomalas.dat
                          reporte-anomalas.txt
```

---

## Tecnologías

- **COBOL** — GnuCOBOL 3.x
- **Archivos secuenciales** — organización LINE SEQUENTIAL
- **Entorno de desarrollo** — VS Code con extensión COBOL de Broadcom
- **Sistema operativo** — Windows 11

---

## Cómo ejecutarlo

```bash


# Compilar todos los módulos
cobc -x -o cargtrx  CARGTRX.COB
cobc -x -o calcsald CALCSALD.COB
cobc -x -o genreport GENREPORT.COB
cobc -x -o detectan DETECTAN.COB

# Ejecutar en orden
./cargtrx
./calcsald
./genreport
./detectan
```

El archivo de entrada `transacciones.dat` debe estar en el mismo directorio con el siguiente formato por registro:

```
CUENTA(10) TIPO(1) MONTO(12) FECHA(8) DESCRIPCION(30)
```

---

## Lo que aprendí construyendo esto

Hay cosas que no se entienden hasta que se hacen. El control de corte parece simple en papel pero cuando lo estás implementando y tienes que pensar en qué pasa con el último grupo de registros — ese que nunca dispara el cambio de cuenta porque el archivo ya terminó — ahí entiendes por qué en los bancos tienen programadores COBOL con 30 años de experiencia que valen oro.

También entendí por qué los sistemas legacy no se reemplazan fácilmente: no es resistencia al cambio, es que replican décadas de lógica de negocio que nadie tiene documentada en ningún otro lado.

---

## Autor

**Jose David Ortega**  
Ingeniero de Sistemas  
[LinkedIn](#) · [GitHub](#)