

#**
#* Esta funcion codifica un pdf en base 64
#*
#* @param PDFPATH : Char : INPUT : Ruta del fichero PDF
#* @param PDF64 : Clbfile : OUTPUT : PDF codificado
#*!
Subprog ENCODE_PDF(PDFPATH, PDF64)
Value Char PDFPATH
Variable Clbfile PDF64

#Variable slocales
Local Integer LENGTH
Local Blbfile PDF_BLB(10)
Local Clbfile PDF_CLB(8)
Local Char PDF_PATH(GDIMFIC) : PDF_PATH=PDFPATH

#Almacenamos el contenido del pdf
Openi   [L]PDF_PATH   Using [PDF]
Getseq  10,[L]PDF_BLB Using [PDF]
Openi                 Using [PDF]

#Codificamos
[L]LENGTH=b64Encode([L]PDF_BLB, [L]PDF_CLB)

#Analizamos la longitud para chequear si ha sido correcta
If [L]LENGTH>0
      PDF64 = [L]PDF_CLB
Else
      Call ERREUR("Error en codificacion") From GESECRAN
Endif

End

#**
#* Esta funcion devuelve la descripción 1 de un artículo
#* @
#* @param WITMREF : Char : INPUT : Código de artículo
#* @return PDF64 : Char : OUTPUT : Descripción 1
#*!
Funprog GET_ITMDES1(WITMREF)
Value Char WITMREF
#Variable local que devolveremos - Descripcion
Local Char WDES1(70)

#Abrimos la tabla de artículos
If !clalev([F:YITM])   : Local File ITMMASTER [YITM]     : Endif

#Leemos la descripcion 1
Read [F:YITM]ITM0 = WITMREF
If !fstat
  WDES1 = [F:YITM]ITMDES1
Endif

#Cerramos la tabla para liberar memoria
Close Local File [F:YITM]
End WDES1
