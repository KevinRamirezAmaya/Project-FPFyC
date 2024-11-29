
 1.Archivos Entregados
    1.1 Comete
    1.2 Opinion
    1.3 Benchmark
    1.4 Common
 2.Instrucciones de ejecucion
    Abriremos una terminal Powershell en windows o la estandar de linux
    Escribimos 'ls' en la terminal para ver todos los archivos a los que podemos acceder
    Buscaremos la carpeta en la cual descargamos el archivo, generalmente Downloads
    En la terminal escribimos 'cd Downloads' con lo cual accederemos a la carpeta
    Escribimos 'ls' y miraremos que se encuentre la carpeta del proyecto que se descargo Morales_Rodriguez_Muñoz_Ramirez
    Escribimos 'cd Morales_Rodriguez_Muñoz_Ramirez' para acceder a la carpeta
    Estando dentro de esta en la terminal ejecutamos el siguiente comando 'sbt clean compile' para asegurarnos de que el
    sbt sera procesado sin errores, luego de esto escribimos 'sbt console' el cual nos permite acceder a un entorno de Scala
    Ahora para ejecutar las pruebas escribimos 'import Comete._ ;import common._  ;import Opinion._  ;import Benchmark._'
    Ya con esto el entorno Scala reconoce todas las funciones que se han definido en los paquetes permitiendo asi
    realizar las pruebas pertinentes
