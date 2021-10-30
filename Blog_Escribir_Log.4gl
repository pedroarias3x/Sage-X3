#Configuracion de un fichero de log utilizando clases
Local Char LOGFILE_NAME(30)


#Declaramos la instancia de la clase
Local Instance MYLOG Using C_ALOG
#Reservamos memoria para la clase
MYLOG = NewInstance C_ALOG AllocGroup null

# Llamamos al método para iniciar la clase
# 'OK' lo mantenemos como variable de estado para
# los errores
OK = fmet MYLOG.ABEGINLOG(" - TRAZA DE LOG - ")
#Obtenemos el nombre que ha asignado automaticamente al
# fichero de log
LOGFILE_NAME=fmet MYLOG.AGETNAME
#Mediante el método APUTLINE escribimos la traza
OK=fmet MYLOG.APUTLINE("--------------",OK)
OK=fmet MYLOG.APUTLINE("LISTADO DE ARTICULOS",OK)
OK=fmet MYLOG.APUTLINE("--------------",OK)

#Hacemos un filtrado para rellenar la traza
#En este caso, volcamos todos los artículos que empiecen por 'B'
Local File ITMMASTER [ITM] Where left$(ITMREF,1) = 'B'
Filter [ITM]
For [ITM]
  OK=fmet MYLOG.APUTLINE([F:ITM]ITMREF,OK)
Next
Filter [ITM]

OK=fmet MYLOG.APUTLINE("--------------",OK)
OK=fmet MYLOG.APUTLINE(num$(time$),OK)

#Llamamos al metodo para finalizar la escritura de la traza
OK=fmet MYLOG.AENDLOG
#Liberamos la memoria asignada a la instancia de log
FreeGroup MYLOG

#Sacamos por pantalla el nombre del fichero
Infbox [L]LOGFILE_NAME
