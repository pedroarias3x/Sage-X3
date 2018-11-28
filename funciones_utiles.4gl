# Uso del find para buscar 'T010','T009' en el vector de tarifas GPLI
If (GUSER='ACOYC' & find(GPLI(I),'T010','T009') : Endif



# ---------------------------------------------------------------------------------#
# Tratamiento de los botones en un archivo
# ---------------------------------------------------------------------------------#
$BOUTON
#------------------------#
# Traitement des boutons #
#------------------------#
Local Integer LRET
Case BOUT
    When "1" :  Gosub RELOAD
    When "2" :  Gosub PRINT
                Gosub RELOAD		#Cuando le dices que acto seguido haga otro gosub se va directamente
    When "3" :  Gosub UPFI
    When "4" :  Gosub CALPR

Endcase
Return

# ---------------------------------------------------------------------------------#
# Lectura de un fichero
# ---------------------------------------------------------------------------------#
# En este caso es un fichero de etiquetas en una ubicacion especifica que sabemos cual es
# Tenemos 2 campos para cada registro

###############################################################################
# UPFI : Cargar fichero
###############################################################################
$UPFI
#Local Char FICHERO : FICHERO = "D:\SAGE\X3APP\folders\ACF\BQR\etiquetas.txt"
# a la hora de cargar el fichero mediante esta variable char da error, hay que probar con todo en mayusculas $PENDIENTE$
Local Char WORDS(120)(1..2) # Vector de lectura del fichero (120) - longitud de cada campo (1..2) 2 campos distintos
Local Integer I : Raz I
Local Integer POSICION
Effzo [M:XETIQ]15

  Openi "D:\SAGE\X3APP\FOLDERS\ACF\BQR\ETIQUETAS.TXT" Using [DMP]   # Estableces la apertura del fichero y el "enlace que va a tener" [DMP], ruta en mayusculas
  Iomode adxium 50       Using [DMP] : # codigo ascii (50)/ UCS2 - (122) / defecto = UTF8
  Iomode adxirs chr$(10) Using [DMP] : # Salto de linea
  Iomode adxifs ";"      Using [DMP] : # Separador de palabras cualquier caracter
    POSICION = 0
    [M:XETIQ]NBLIG = 0
    Rdseq WORDS Using [DMP] #lees la primera linea del archivo
    While !fstat 		# Mientras que la flag de estado sea 0 - es decir, mientras lea lineas del documento
      [M:XETIQ]NBLIG += 1 # Siempre el parametro del NBLIGN debe ir uno mas por encima de donde vas a escribir, es decir
      # Necesitas que siempre en "pantalla" haya una linea mas a la que escribes escribes en 0 / NBLIG = 1
      [M:XETIQ]ITMREF(POSICION) = WORDS(1) # El campo 1 del registro
      [M:XETIQ]ITMDES(POSICION) = WORDS(2) # El campo 2 del registro
      [M:XETIQ]IMPORTE(POSICION) = 0
      [M:XETIQ]TRT(POSICION) = 2
      [M:XETIQ]STU(POSICION) = "UN"
      Affzo [M:XETIQ]ITMREF(POSICION)
      Affzo [M:XETIQ]ITMDES(POSICION)
      Affzo [M:XETIQ]IMPORTE(POSICION)
      Affzo [M:XETIQ]TRT(POSICION)
      Affzo [M:XETIQ]STU(POSICION)
     #[M:XETIQ]NBLIG += 1
      Rdseq WORDS Using [DMP]
      POSICION+=1
   Wend
  Openi Using [DMP] # Cierre del archivo en el que estamos trabajando

Affzo [M:XETIQ]NBLIG
Gosub CALPR # llamada a funcion que esta en el especifico

Return
###############################################################################

###############################################################################
# Funcion para el calculo de los precios de unas lineas
###############################################################################
# CALPR : Recalcular precios
###############################################################################
$CALPR
nolign = 0
For NOL=0 To [M:XETIQ]NBLIG-1
nolign+=1
   Local Decimal XQTDIS : XQTDIS = 0
   Read [ITM]ITM0 = [M:XETIQ]ITMREF(nolign-1)  # Lectura de las tablas ITM y ITV para nuestro ITMREF
   Read [ITV]ITV0 = [M:XETIQ]ITMREF(nolign-1);[M:XETIQ]STOFCY
   If !fstat # Si no hay errores lees la ITG
      Read [ITG]ITG0=[M:XETIQ]STOFCY;[F:ITM]TCLCOD
      If fstat
         Read [ITG]ITG1(1)=[F:ITM]TCLCOD
      Endif
      [M:XETIQ]TRT(nolign-1)    = 2 # El codigo TRT es un codigo tipo menu 1- no 2 - si usado para las impresiones
      Call STODISTOT("[F:ITV]",5,1,"",XQTDIS) From STKLIB
      Call CHARGE_DEFITMDES("DES1AXX",GLANGUE,[F:ITM]ITMREF,[M:XETIQ]ITMDES(nolign-1), "ITM") From TRTX3
      [M:XETIQ]EANCOD(nolign-1) = [F:ITM]EANCOD
      Call CAL_PROVEEDOR(NOL) #modificacion ped


      If [M:XETIQ]SOLOSTOK <>2
   #      nolign+=1
         [M:XETIQ]QTYSTU(nolign-1) = XQTDIS
         [M:XETIQ]INITDAT(nolign-1) = [00/00/0000]
         [M:XETIQ]FINDAT (nolign-1) = [00/00/0000]
         If [M:XETIQ]SINPRECIO <> 2
            Local Decimal PRECIO
            Local Date FECHAINI, FECHAFIN
            Call CAL_PRECIO([M:XETIQ]ITMREF(NOL),[M:XETIQ]STOFCY,"ACF-002000",GLOCALDEV,date$,1,NOL,PRECIO,FECHAINI,FECHAFIN)
            [M:XETIQ]IMPORTE(nolign-1) = PRECIO
            [M:XETIQ]INITDAT(nolign-1) = FECHAINI
            [M:XETIQ]FINDAT (nolign-1) = FECHAFIN
         Else
            [M:XETIQ]IMPORTE(nolign-1)=0
            Raz [M:XETIQ]INITDAT(nolign-1)
            Raz [M:XETIQ]FINDAT (nolign-1)
         Endif
      Else
         If XQTDIS<>0
           [M:XETIQ]QTYSTU(nolign-1) = XQTDIS
           If [M:XETIQ]SINPRECIO <> 2
              Local Decimal PRECIO
              Local Date FECHAINI, FECHAFIN
              Call CAL_PRECIO([M:XETIQ]ITMREF(nolign-1),[M:XETIQ]STOFCY,"ACF-002000",GLOCALDEV,date$,1,NOL,PRECIO,FEHAINI,FECHAFIN)
              [M:XETIQ]IMPORTE(nolign-1) = PRECIO
              [M:XETIQ]INITDAT(nolign-1) = FECHAINI
              [M:XETIQ]FINDAT (nolign-1) = FECHAFIN

            Else
               [M:XETIQ]IMPORTE(nolign-1)=0
               Raz [M:XETIQ]INITDAT(nolign-1)
               Raz [M:XETIQ]FINDAT (nolign-1)
            Endif
          Endif
      Endif
    Endif
Next
Affzo [M:XETIQ]15


###############################################################################
# Lectura de una tabla para un indice - ejemplos
###############################################################################
# Para poder leer la tabla necesitas uno de los indices establecidos, en el siguiente ejemplo cargas la ITM con el ITMREF y
# la ITV con el ITMREF y el STOFCY
If !clalev([F:ITM])   : Local File ITMMASTER [ITM]     : Endif
If !clalev([F:ITF])   : Local File ITMFACILIT[ITF]     : Endif
If !clalev([F:ITV])   : Local File ITMMVT    [ITV]     : Endif
Read [ITM]ITM0 = [M:XETIQ]ITMREF(nolign-1)
Read [ITV]ITV0 = [M:XETIQ]ITMREF(nolign-1);[M:XETIQ]STOFCY
# De esta forma leemos el registro de la tabla para nuestro ITMREF, asi podemos acceder a campos
# que de otra forma no pordiamos tener, como la descripcion etc...


###############################################################################
# Lectura de una tabla cuando no tienes los indices- ejemplos
# Ejemplo de funcion dentro de especifico
###############################################################################
# En este caso solo tienes el ITMREF
# Primero tienes que asegurarte de que tienes la tabla abierta
# Una vez que está abierta, haces un filter de la misma y te quedas con el registro que te interese
#########################################################
Subprog CAL_PROVEEDOR(NOL)
Value Integer NOL

   If !clalev([F:ITP]) : Local File ITMBPS      [ITP]     : Endif

   Filter [ITP] Where [F:ITP]ITMREF = [M:XETIQ]ITMREF(NOL)
      Read [ITP] First
      If !fstat
        [M:XETIQ]XITMREFBPS(NOL) = [F:ITP]BPSNUM
        #[F:XETIQ]XITMREFBPS(NOL) = [F:ITP]BPSNUM
        Affzo [M:XETIQ]XITMREFBPS(NOL)
        Else
        [M:XETIQ]XITMREFBPS(NOL) = ""
      Endif
   Filter [ITP]
End
#########################################################




###############################################################################
# PRINT : funcion para imprimir, le pasas los parametros de destiono y sus valores correspondientes
###############################################################################
$PRINT
Local Integer I : Raz I
Gosub REMPL_LBP
#If GOK = 1
  Local Char XDESTINO : XDESTINO = "PREVISU"
  Local Char XRPTCOD  : XRPTCOD  = "XETIQ01"

  Local Char TBPAR(15)(1..50),TBVAL(30)(1..50)
  TBPAR(1)  = 'usr'
  TBPAR(2)  = 'numedt'

  TBVAL(1)  = GUSER
  TBVAL(2)  = num$(adxuid(1))

  Call ETAT(XRPTCOD,XDESTINO,"",0,"",TBPAR,TBVAL) From AIMP3

#Endif
Return

###########################################################################
# Funcion para la escritura de los datos de pantalla en una tabla
###########################################################################
$REMPL_LBP

Trbegin [XETIQ]
Delete [XETIQ] Where CREUSR = GUSER & CREDAT < date$
fstat = 0
Delete [XETIQ] Where NUMREQ = adxuid(1)
fstat = 0
Commit
#On arrive ici pour chaque ligne du tableau
Call DEBTRANS From GLOCK
Trbegin [XETIQ]

Raz [F:XETIQ]
For I=0 To [M:XETIQ]NBLIG-1
  If [M:XETIQ]TRT(I) = 2
      [F:XETIQ]CREUSR = GUSER
      [F:XETIQ]NUMREQ = adxuid(1)
      [F:XETIQ]CREDAT = date$
      [F:XETIQ]STOFCY = [M:XETIQ]STOFCY
      [F:XETIQ]ITMREF = [M:XETIQ]ITMREF(I)
      [F:XETIQ]ITMDES = [M:XETIQ]ITMDES(I)
      [F:XETIQ]IMPORTE= [M:XETIQ]IMPORTE(I)
      [F:XETIQ]EANCOD = [M:XETIQ]EANCOD(I)
      [F:XETIQ]INITDAT = [M:XETIQ]INITDAT(I)
      [F:XETIQ]FINDAT = [M:XETIQ]FINDAT(I)
      [F:XETIQ]XITMREFBPS = [M:XETIQ]XITMREFBPS(I)

      If [M:XETIQ]FINDAT(I)<> [00/00/0000]
         [F:XETIQ]ARSWB = '*'
      Else
         [F:XETIQ]ARSWB = ''
      Endif
       Write [F:XETIQ]
      If fstat : GOK = 0 : Break :  Endif
  Endif
Next
If GOK = 0 : Rollback : Else : Commit : Endif
Return

#########################################################
# Simulacion de pantalla de pedidos para el calculo de las tarifas
#########################################################
# Parametros de entrada: codigo del articulo, planta, cliente, divisa, fecha , cantidad y numero de linea
# Parametros de salida: precio neto despues de la aplicacion de las tarificaciones y las fechas de las mismas
Subprog CAL_PRECIO(ITM,PLANTA,CLIENTE,CUR,FECHA,QTY,NOL,PRECIO,FECHAINI,FECHAFIN)
Value Char ITM
Value Char PLANTA
Value Char CLIENTE
Value Char CUR
Value Date FECHA
Value Decimal QTY
Value Integer NOL
Variable Decimal PRECIO
Variable Date FECHAINI
Variable Date FECHAFIN

Local Char    VAT(GLONVAT)(3) # variable global de tarificaciones
Global Date FINI, FFIN # Fechas globales de inicio y fin de las tarifas
FINI = [00/00/0000]
FFIN = [00/00/0000]
# si no coincide el ITMREF
If [F:ITM]ITMREF <> ITM
   Read [F:ITM]ITM0 = ITM
   If fstat  Raz [F:ITM] : End :  Endif
Endif
If [F:ITS]ITMREF <> [F:ITM]ITMREF
   Read [F:ITS]ITS0 = [F:ITM]ITMREF
   If fstat  Raz [F:ITS] : Endif
Endif
Local Char DEFMSK(250), DEFFIL(250)
DEFMSK = dbgstr(1)                        # Guardamos mascaras por defecto
DEFFIL = dbgstr(2)                       # Guardamos tablas por defecto
 # Ahora cargas las mascaras de pedidos de compras SOH para simular que estas en los mismos
Local Mask  SOH0 [SOH0]
Local Mask  SOH1 [SOH1]
Local Mask  SOH2 [SOH2]
Local Mask  SOH3 [SOH3]
Local Mask  SOH4 [SOH4]
Default Mask [SOH0]
Default Mask [SOH1]
Default Mask [SOH2]
Default Mask [SOH3]
Default Mask [SOH4]
Local Integer TYPRECH :   TYPRECH = 1
[M:SOH4]ITMREF(NOL) = [F:ITS]ITMREF
[M:SOH4]QTY(NOL)    = QTY
[M:SOH0]CUR = CUR
[M:SOH0]ORDDAT = FECHA
[M:SOH0]SALFCY = PLANTA
[M]DSTOFCY(NOL)= PLANTA
[M:SOH1]PRITYP = 1
[M:SOH0]CHGTYP = 1
[M:SOH0]BPCORD = CLIENTE
Read [BPC]BPC0 = [M:SOH0]BPCORD
Global Char GPNTITMREF
[M:SOH1]VACBPR  = [F:BPC]VACBPR
[M:SOH4]SAU(NOL)=[F:ITM]SAU
[M:SOH4]SAUSTUCOE(NOL) = [F:ITM]SAUSTUCOE
[M:SOH4]VACITM1(NOL) = [F:ITM]VACITM(0)
[M:SOH4]VACITM2(NOL) = [F:ITM]VACITM(1)
[M:SOH4]VACITM3(NOL) = [F:ITM]VACITM(2)
[F:SPK]=[M:SOH0]
[F:SPK]=[M:SOH1]
[F:SPK]=[M:SOH2]
[F:SPK]=[M:SOH3]
[F:SPK]=[M:SOH4]
Call RECHVATCOD(1,1,[M:SOH0]BPCORD,[M:SOH4]ITMREF(NOL),"SOH4",NOL,VAT) From TRTX3
[M:SOH4]VAT1(NOL)  = VAT(0)
[M:SOH4]VAT2(NOL)  = VAT(1)
[M:SOH4]VAT3(NOL)  = VAT(2)
Default Mask [SOH0] # establecess la mascara por defecto como la SOH0 para que te coja bien el CUR y el UOM en el TRTVENTAR
Call TARIFCHGT(5) From TRTPRICE
# Cuando llamas a las tarificaciones se crea un WBTT008 por ejemplo que es una ventana temporal para el calculo de las mismas
# ademas de este fichero tienes el GPLI donde guardas las tarifas que tines, GPLIO donde tienes las prioridades de las mismas
# y GPLYTYP que guarda el tipo de tarifa 1 - normal, 4 - agrupada, etc...
Call ALICLCAMT([F:ITS]ITMREF, 1, NOL, "SOH4", [M:SOH4]CLCAMT1(NOL), [M:SOH4]CLCAMT2(NOL)) From TRTX3
Call RECH_TARIF(TYPRECH,[M]ITMREF(NOL),NOL,[M]QTY(NOL),"SOH",[M]GROPRI(NOL)) From TRTVENTAR
Call CLCNETPRI([M]QTY(NOL), [M:SOH0]CUR, NOL) From TRTVENPRI
Call CLCPFM([M]DSTOFCY(NOL), [M:SOH1]PRITYP, [M:SOH0]CHGTYP, [M:SOH0]ORDDAT, [M:SOH0]CUR, NOL, 1) From TRTVENPRI
PRECIO = [M:SOH4]NETPRI(NOL)
FECHAINI = FINI
FECHAFIN = FFIN
If PRECIO = 0
   Raz FECHAINI
   Raz FECHAFIN
Endif
Default Mask DEFMSK
Default File DEFFIL

End

$FINSAI
If dim(FINI)>0 : Kill FINI, FFIN : Endif
Return
#########################################################













###############################################################################
# 							FUNCIONES UTILES
###############################################################################
###############################################################################
# En la siguiente funcion calculas la cantidad de un articulo tirando de la tabla ITV previamente abierta (ver arriba)
Call STODISTOT("[F:ITV]",5,1,"",XQTDIS) From STKLIB
###############################################################################
# En la siguiente funcion cargas la descripcion por defecto y la guardas en la linea
# Previamente a esto has identificado el ITMREF de la ITM con el ITMREF que buscas
Call CHARGE_DEFITMDES("DES1AXX",GLANGUE,[F:ITM]ITMREF,[M:XETIQ]ITMDES(nolign-1), "ITM") From TRTX3
###############################################################################
###############################################################################
# A la hora de crear una funcion en el propio especifico tienes una serie de valores de entrada
# definidos con value y una serie de parametros, para terminar una funcion lo haces con end. Ejemplo:
Subprog CAL_PROVEEDOR(NOL)
#ENTRADAS
Value Integer NOL
value char ITMDES
#SALIDAS
variable Integer SALIDA
end
###############################################################################
###############################################################################
# Borrar el cuadro de las lineas de la pantalla entero

Affzo [M:XETIQ]15
###############################################################################
# Pantalla emergente para introducir un dato
###############################################################################
# Funcion para obtener un valor por pantalla
# Valeur tiene que ser un char para que funcione correctamente la llamada
# Si es un dato numerico se tiene que hacer un casting
Local Integer OK
Local Char VALEUR

Call SAICAR(VALEUR,"Vales a emitir","Cantidad","XVD",0,0,"",OK) From GESECRAN
Infbox("Cantidad de vales de descuento: " + num$(VALEUR))
NVALES = val(num$(VALEUR))


###############################################################################
# Quitar espacios en blanco
###############################################################################
virebout


###############################################################################
# Leer campo fecha hora y extraer informacion
# Extraer campo de cadena
###############################################################################
#####################################################################################
Subprog TARJETA_REGALO(CENTRO,EQUIPO,TIPO)
Variable Char CENTRO
Variable Char EQUIPO
Variable Char TIPO

Local Integer OK
Local Integer TIEMPO_ACTUAL
Local Char TAB
Local Char TIEMPO_CREACION (8)
Local Integer POS
Local Char FECHA_CREACION
Local Char NUMERO_TARJETA

Call OUINON("Utilice el programa lector de tarjetas y pulse Sí cuando haya sido leída.",OK) From GESECRAN
If OK = 2
#  Infbox("Tarjeta leida, procediendo al calculo")
  If !clalev([F:XTR]) : Local File XTARJREGALO [XTR] : Endif

  Filter [XTR] Where [F:XTR]EQUIPO = EQUIPO and [F:XTR]CENTRO = CENTRO Order By [F:XTR]FECHA

#  Infbox("DENTO DEL FIL")
  Read [XTR] Last
  If !fstat
#  Infbox (num$([F:XTR]EQUIPO))
#  Infbox (num$([F:XTR]FECHA))
    TIEMPO_ACTUAL = time
    TAB = "T"
    POS = instr(1,num$([F:XTR]FECHA),TAB)
    TIEMPO_CREACION = right$(num$([F:XTR]FECHA),POS+1)

    FECHA_CREACION = left$(num$([F:XTR]FECHA),POS-1)
    FECHA_ACTUAL = num$(date$)
#    Infbox(num$([F:XTR]EQUIPO))
#    Infbox(num$(FECHA_CREACION))
#    Infbox(num$(FECHA_ACTUAL))
#
#    TAB2 = "-"
#    POS = instr(1,FECHA_CREACION,TAB2)
    Local Char ANNO : ANNO = left$(FECHA_CREACION,4)
    Local Char MES(2) : MES = mid$(FECHA_CREACION,6,7)
    Local Char DIA(2) : DIA = right$(FECHA_CREACION,9)
#    Infbox(ANNO + MES + DIA)
    Local Char FECHA_FINAL : FECHA_FINAL = DIA + "/" + MES + "/" + ANNO

    If (FECHA_FINAL = FECHA_ACTUAL)
#    Infbox("entra fecha ok")
      Local Char HORA(2) : HORA = left$(TIEMPO_CREACION,2)
      Local Char MINUTOS(2) : MINUTOS = mid$(TIEMPO_CREACION,4,6)
      Local Char SEGUNDOS(2) : SEGUNDOS = right$(TIEMPO_CREACION,7)
      Local Integer TIEMPO_FINAL : TIEMPO_FINAL = val(HORA)*3600 + val(MINUTOS)*60 + val(SEGUNDOS)
If GUSER = "ACOEN"
Infbox(HORA+":"+MINUTOS+":"+SEGUNDOS)
Infbox(num$(TIEMPO_ACTUAL)+" FINAL "+num$(TIEMPO_FINAL)) :
Endif

      If (abs(TIEMPO_ACTUAL - TIEMPO_FINAL)) <= 300 #Comprobacion de que la hora actual y la de creacion no supera los 5 minutos (300 segundos)
        If [F:XTR]TIPO = "P" and TIPO = "P"
#          Infbox("entra tipo y tiempos ok - p ")
          NUMERO_TARJETA = [F:XTR]TARJETA
          Infbox(num$(NUMERO_TARJETA))
          If clalev  ([M:SOH0]) = 0 : Local Mask SOH0 [SOH0] : Endif
          [M:SOH0]BPCORD = NUMERO_TARJETA
          Affzo [M:SOH0]BPCORD
          zonsui = "[M:SOH0]BPCORD"
          zoncou  = "BPCORD"
        Elsif [F:XTR]TIPO = "R" and TIPO = "R"
#          Infbox("entra tipo y tiempos ok - r")
          [M:XTR]XCARDNUM = [F:XTR]TARJETA
          If clalev ([F:XGC])=0 : Local File XGIFTCARD   [XGC]  : Endif
          Read [XGC]XGC0 = [F:XTR]TARJETA # Lectura de la tabla de tarjetas regalo
          If !fstat
            [M:XTR]XVALDAT = [F:XGC]XVALDAT
            [M:XTR]XSALDO = [F:XGC]XSALDO
          Endif
          Affzo [M:XTR]
        Endif # tipo
      Else
        Infbox("No se encuentra ninguna lectura de tarjeta.")
      Endif #5 minutos
    Endif #fecha
  Endif #fstat


Filter[XTR]
Else
  zonsui = "[M:SOH0]BPCORD"
  zoncou  = "BPCORD"
Endif # ok

Raz OK

End


#####################################################################################

###############################################################################
# Funcion de mensajes de error, una llamada con mess y la otra por call para cuando no funciona la otra
###############################################################################

If [F:XGC]XVALDAT < date$
    Call MESSAGE (mess(22,6011,1)) From GESECRAN : OK=0 : Return
    GMESSAGE=mess(22,6011,1) : OK=0 : Return
  Endif


###############################################################################
# Botones
###############################################################################
# Primer case visto en las etiquetas, los botonestienen definido su proceso correspondiente, tu puedes llamar a uno o a otro dependiendo
#de alguna variable
Local Integer LRET
Case BOUT
    When "1" :  Gosub RELOAD
    When "2" :  Gosub PRINT
                Gosub RELOAD		#Cuando le dices que acto seguido haga otro gosub se va directamente
    When "3" :  Gosub UPFI
    When "4" :  Gosub CALPR

Endcase
Return
#Puedes desactivar un boton en el subsoh y activarlo en el especifico, para volver a activar un boton, basta con poner en el IB_NBLIGN
GBOUT15 = mess(30,6011,1)


# Funcion para cambiar el formato a una fecha
 format$("D:YYYYMMDDhhmmss",datesyst)
 format$("D:DDMMYYYY",datesyst)



#Importacion a fichero y lectura del mismo mediante modelo de importacion

######################################################################################
$DIVISION
Infbox("division")
Call OUVRE_TRACE("Division de pedidos") From LECFIC
Call ECR_TRACE(" ",0) From GESECRAN
Call ECR_TRACE(string$(70,"-"),0) From GESECRAN
Call ECR_TRACE(" ",0) From GESECRAN
Call TEMPON("") From GESECRAN

Local Decimal XCANTIDAD, XIMPORTE
Local Char PEDIDO, SECUENCIA
Local Char CRITERE(250)
Global Integer LOK, SEL
Local Char PLVENTA, CLIENTE, FECHA
Local Char OBJETO, CLAVE1, CLAVE2
Local Decimal PORTES
Local Char ARTICULO, NUMERO(15)
Local Char RUTA(255), CABECERA(255),DETALLE(255), FICHERO(200), CABECERA2(255),DETALLE2(255)
Local Integer OK, LINEA
Local Decimal GASTO, XPORCENTAJE, XIMPORTE
If clalev ([F:SOH])  <= 0 Local File SORDER     [SOH] : Endif
If clalev ([F:SOP])  <= 0 Local File SORDERP    [SOP] : Endif
If clalev ([F:SOQ])  <= 0 Local File SORDERQ    [SOQ] : Endif
If clalev ([F:ITM])  <= 0 Local File ITMMASTER  [ITM] : Endif
If clalev ([F:BPC])  <= 0 Local File BPCUSTOMER [BPC] : Endif
If clalev ([F:BPD])  <= 0 Local File BPDLVCUST  [BPD] : Endif

adxirs = chr$(13)+chr$(10)
FICHERO = "PEDIDOS\"+[M:SOH0]SOHNUM
Openo filpath (GDIRBQE, FICHERO, "TXT"),0 Using [FIL]
Read [SOH]SOH0 = [M:SOH0]SOHNUM
XPORCENTAJE = ar2(([F:SOH]ORDNOT*[M:XDP]PORCENTAJE)/100)
XIMPORTE    = 0
If [M:XDP]PORCENTAJE<>100
    Sorta [M:SOH4]NBLIG [M:SOH4]NBLIG Order By [M:SOH4]LINORDNOT(indice)
    For X=0 To [M:SOH4]NBLIG-1
        Read [SOP]SOP0(2) = [M:SOH0]SOHNUM;[M:SOH4]SOPLIN(X)
        Read [SOQ]SOQ0(2) = [M:SOH0]SOHNUM;[M:SOH4]SOPLIN(X)
        If !fstat
            XIMPORTE += ar2([F:SOQ]QTY*[F:SOP]NETPRI)
            [M:SOH4]XDIV(X)=2
            Affzo [M:SOH4]XDIV(X)
        Endif
        If XIMPORTE>XPORCENTAJE
            [M:SOH4]XDIV(X)=1
            Affzo [M:SOH4]XDIV(X)
            LINEA = X
            Break
        Endif
    Next
#    If LINEA=0 : GMESSAGE = 'Imposible dividir el pedido, % demasiado pequeño' : Return : Endif
    If LINEA=0
        [M:SOH4]XDIV(0)=1
        Affzo [M:SOH4]XDIV(0)
    Endif
Elsif [M:XDP]PORCENTAJE=100
    Sorta [M:SOH4]NBLIG [M:SOH4]NBLIG Order By [M:SOH4]LINORDNOT(indice)
    For X=0 To [M:SOH4]NBLIG-1
        If !fstat
            [M:SOH4]XDIV(X)=2
            Affzo [M:SOH4]XDIV(X)
        Endif
    Next
Endif


If [M:XDP]PORCENTAJE<>100
    Gosub F_CABECERA
    Gosub F_DETALLE
    Gosub F_CABECERA2
    Gosub F_DETALLE2
Elsif [M:XDP]PORCENTAJE=100
    Gosub F_CABECERA3
    Gosub F_DETALLE2
Endif
Openo Using [FIL]


If [M:XDP]PORCENTAJE=100
    NUMERO  = [M:SOH0]SOHNUM
    Default Mask [SOH1]
    Default File [SOH]
    Local Char    CLECUR1(GLONID1) , CLECUR2(GLONID2)
    [L]CLECUR1=NUMERO : Raz [L]CLECUR2
    Call ANUCOD("SOH",CLECUR1,CLECUR2,OK) From GOBJACT
    Default File [SOH]
    GREP=""
    Call TRACE_COR("SOH",CLECUR1,CLECUR2,"A") From GOBJET
    Call ANNULE("SOH",CLECUR1,CLECUR2) From GOBJTXT
    Call ANNULE("SOH",CLECUR1,CLECUR2) From SUBALI
    CLECUR1 = "" : CLECUR2 = ""
Endif

RUTA = filpath (GDIRBQE, FICHERO, "TXT")
Call IMPORTSIL ('SCOSOH', RUTA) From GIMPOBJ
Call TEMPOFF From GESECRAN
Call FERME_TRACE From LECFIC
Call LEC_TRACE From LECFIC
Raz GPILCOD, GABREV, GALIPLK, GBOITE, GBOXSQ1, GCODOBJ, GDEROBJ
Raz GMASKOUV, GNAVIG, GUSRFCT
If dim([M:XDP]DIVISION)>0 : Close Global Mask [XDP] : Endif
If [V]GCLE<>""
    If [V]GCLE=NUMERO
        Call OBJET ("SOH", NUMERO,GBIDC2) From GOBJET
    Else
        Call OBJET ("SOH", GBIDC1,GBIDC2) From GOBJET
    Endif
Endif
Return

#########################################################################
$F_CABECERA
PLVENTA = [M:SOH0]SALFCY
NUMERO  = [M:SOH0]SOHNUM
CLIENTE = [M:SOH0]BPCORD
FECHA   = [M:SOH0]ORDDAT
Read [BPC]BPC0 = CLIENTE
Read [BPD]BPD0 = CLIENTE;[M:SOH1]BPAADD
CABECERA ='"E";"'+PLVENTA+'";"'+[F:SOH]SOHTYP+'";"'+NUMERO+'";"'+CLIENTE+'";"'+[M:SOH1]BPAADD+'";'+FECHA+';"'+[M:SOH0]CUSORDREF+'";"'+[M:SOH1]REP+'";"'+[M:SOH2]STOFCY+'";"'+[M:SOH0]CUR+'";"'+[M:SOH3]
& PTE+'";"'+[F:BPD]YBPTNUMC2+'";'+num$([M:SOH3]INVDTAAMT(0))+';'+num$([M:SOH3]INVDTAAMT(1))+';'+'0'+'"";"";"";""'

Wrseq CABECERA Using [FIL]
Return

#########################################################################
$F_DETALLE
For Y=0 To [M:SOH4]NBLIG-1
    If [M:SOH4]XDIV(Y)<>2
        ARTICULO = [M:SOH4]ITMREF(Y)
        Read [ITM]ITM0 = ARTICULO
        DETALLE = '"L";"'+ARTICULO+'";"'+[M:SOH4]SAU(Y)+'";'+num$([M:SOH4]QTY(Y))+';'+num$([M:SOH4]GROPRI(Y))+';'+num$([M:SOH4]DISCRGVAL1(Y))+';'+num$([M:SOH4]DISCRGVAL2(Y))+';"";""'
        Wrseq DETALLE Using [FIL]
    Endif
Next
Return


#########################################################################
$F_CABECERA2
PLVENTA = "X"+right$([M:SOH0]SALFCY,2)
NUMERO  = ""
CLIENTE = [M:SOH0]BPCORD
FECHA   = [M:SOH0]ORDDAT
Read [BPC]BPC0 = CLIENTE
Read [BPD]BPD0 = CLIENTE;[M:SOH1]BPAADD
CABECERA2 ='"E";"'+PLVENTA+'";"'+[F:SOH]SOHTYP+'";"'+NUMERO+'";"'+CLIENTE+'";"'+[M:SOH1]BPAADD+'";'+FECHA+';"'+[M:SOH0]CUSORDREF+'";"'+[M:SOH1]REP+'";"'+[M:SOH2]STOFCY+'";"'+[M:SOH0]CUR+'";"'+[F:BPC]
& XPTE+'";"'+[F:BPD]YBPTNUMC2+'";'+'0'+';'+'0'+';'+num$([F:BPC]INVDTAAMT(2))+'"";"";"";""'

Wrseq CABECERA2 Using [FIL]
Return

#########################################################################
$F_CABECERA3
PLVENTA = "X"+right$([M:SOH0]SALFCY,2)
NUMERO  = ""
CLIENTE = [M:SOH0]BPCORD
FECHA   = [M:SOH0]ORDDAT
Read [BPC]BPC0 = CLIENTE
Read [BPD]BPD0 = CLIENTE;[M:SOH1]BPAADD
CABECERA2 ='"E";"'+PLVENTA+'";"'+[F:SOH]SOHTYP+'";"'+NUMERO+'";"'+CLIENTE+'";"'+[M:SOH1]BPAADD+'";'+FECHA+';"'+[M:SOH0]CUSORDREF+'";"'+[M:SOH1]REP+'";"'+[M:SOH2]STOFCY+'";"'+[M:SOH0]CUR+'";"'+[F:BPC]
& XPTE+'";"'+[F:BPD]YBPTNUMC2+'";'+'0'+';'+num$([M:SOH3]INVDTAAMT(1))+';'+num$([F:BPC]INVDTAAMT(2))+'"";"";"";""'

Wrseq CABECERA2 Using [FIL]
Return

#########################################################################
$F_DETALLE2
For Y=0 To [M:SOH4]NBLIG-1
    If [M:SOH4]XDIV(Y)<>2
        ARTICULO = [M:SOH4]ITMREF(Y)
        Read [ITM]ITM0 = ARTICULO
        DETALLE2 = '"L";"'+ARTICULO+'";"'+[M:SOH4]SAU(Y)+'";'+num$([M:SOH4]QTY(Y))+';'+num$([M:SOH4]GROPRI(Y))+';'+num$([M:SOH4]DISCRGVAL1(Y))+';'+num$([M:SOH4]DISCRGVAL2(Y))+';"";""'
        Wrseq DETALLE2 Using [FIL]
    Endif
Next
#For Z=0 To [M:SOH4]NBLIG-1
#    If [M:SOH4]XDIV(Z)=2
#        ARTICULO = [M:SOH4]ITMREF(Z)
#        Read [ITM]ITM0 = ARTICULO
#        DETALLE2 = '"L";"'+ARTICULO+'";"'+[M:SOH4]SAU(Z)+'";'+num$([M:SOH4]QTY(Z))+';'+num$([M:SOH4]GROPRI(Z))+';'+num$([M:SOH4]DISCRGVAL1(Z))+';'+num$([M:SOH4]DISCRGVAL2(Z))+';"";""'
#        Wrseq DETALLE2 Using [FIL]
#    Endif
#Next
Return

BE0433511

Local Char ESCRITURA(255) #Variable temporal de escritura para cada una de las lineas
Local Char XDIR : XDIR = "C:\Sage\SOLOIO\folders\SOLOIO\SharedTransfer\" #Ruta donde se almacenaran los ficheros


# ---------------------------------------------------------------------------------#
#Borrado de ficheros si existen
# ---------------------------------------------------------------------------------#
NOMFIC=filpath(XDIR,"XSANDSTOCK01","txt")
If filinfo(NOMFIC,0) > 0
   Call EFFACE(NOMFIC,OK) From ORDSYS
Endif

# ---------------------------------------------------------------------------------#
#Desactivar botones
# ---------------------------------------------------------------------------------#
#----- Botones--------
# m: Marcar todo
# n: Desmarcar todosaicrº
# q: Renumerar prioridad
#-------------------- 

   If GPROFIL <> "ADMIN" and left$(GPROFIL,2) <> "PA" Then
      Call VIREBOUT(CHAINE,"q") From GOBJET 
   Endif 

   If GPROFIL <> "ADMIN" and left$(GPROFIL,2) <> "PA" Then
      Call VIREBOUT(CHAINE,"m") From GOBJET 
   Endif 

   If GPROFIL <> "ADMIN" and left$(GPROFIL,2) <> "PA" Then
      Call VIREBOUT(CHAINE,"n") From GOBJET 
   Endif 


   #Limpiar un bloque de la pantalla
  raz  [M:YBOH1] #Limpiamos la pantalla encuentra
  effzo  [M:YBOH1]10-20 #Con raz no puedes borrar bloques sueltos, tienes que usar effzo
  

  #Inicializar valores de campos
  Si estas en un tipo cuadro y quieres actualizar los valores de los campos 2 y 3 no puedes hacerlo
  en el init del boton 3 porque entonces solo actualiza el 2. Tendrias que hacerlo en el init del
  campo 4 para que se actualizasen.
  
  #Ver si un fichero que se esta generando tiene el tamaño correcto
  filinfo = 7 --> consultar la ayuda de Sage
  

  #Cuando te dice error en el criterio de selección o error SQL prueba a declarar la tabla por defecto
#Ejemplo declaracion de tabla por defecto para una accion de seleccion
  default file [F:STL]


  #Crear una seleccion personalizada --> accion de tipo seleccion de tabla (desarrollo > acciones)

  ##-------------------------------------------------------------
#-- Selecciones varias
##-------------------------------------------------------------

$ACTION
Case ACTION
        When "SEL_TABLE"    :  Gosub SEL_TABLE
Endcase
Return
#####################################################
$SEL_TABLE
Case TABLE
    When "YSELPACA"      : Gosub S_YPACA
Endcase
Return


#################################################################
$S_YPACA
Default File [STL]

CRITERE = "[F:STL]YGESBOB=2"
START   = "YPACA"
ORDRE   = "YPACA"
TIT(0) = 'Seleccion de pacas'

NBCOL = 0
NBCOL += 1 : COL(NBCOL) = "YPACA"    : TIT(NBCOL) = "Paca"        #Alternativa --> #: Gosub TEXTE
NBCOL += 1 : COL(NBCOL) = "ITMREF"   : TIT(NBCOL) = "Calidad"
NBCOL += 1 : COL(NBCOL) = "YANCHO"   : TIT(NBCOL) = "Ancho"
NBCOL += 1 : COL(NBCOL) = "YESPESOR" : TIT(NBCOL) = "Espesor"

Return



#Guardar fichero en puesto cliente 
Local Integer STAT 
Local Char ZFICSRV(250) : FICSRV = "C:\Sage\SAGEX3V7\X3V71TRAIN\Folders\SEED\tmp\SORDER.tra" 
Local Char ZFICCLI(250) : FICCLI = "SORDER"+date$ 
Call COPCLI(FICSRV,FICCLI,STAT) From ORDSYS 
If STAT <> 0 
    GERR = 2 
    GMESSAGE = "Erreur transfert Fichier"
Endif

# ---------------------------------------------------------------------------------#
#Ventanas de dialogo, ventanas simples con un par de campos solo. Estas pantallas no tienen acciones puesto que son como un subprog
que enlazas en la funcion principal, se llaman con DIALWIN, pasandole, el ok como variable, el mess titulo de la pantall y la abreviatura
de la ventana que contiene a esa paantalla. TODO se tiene que definir (ventana y pantalla) como cajas de dialogo
# ---------------------------------------------------------------------------------#
    Call DIALWIN(OK,mess(48,6000,1),"OYCC2") From GESECRAN



# ---------------------------------------------------------------------------------#
#Para hacer un filtro personalizado en el picking, desde la ventana tienes que definir en la seccion de browser los dos objetos que quieres
en caso de que sea el mismo objeto tienes que cambiarles las abreviaturas desde la misma linea desde donde introduces, luego a traves de 
codigo puedes establecer el filtro de cada una con el siguiente codigo
# ---------------------------------------------------------------------------------#
If currbox     = 'GAU_CHE' # para el objeto principal
  FILGAUCHE(0) = " YGESBOB = 2 & YDESTINO = ''  "
Elsif currbox  = 'GAU_CHE1' # para el secundario
  FILGAUSUP(0) = "  YGESBOB = 2 & YDESTINO <> '' "
Endif



###########################################################
# Funcion para abrir la traza
###########################################################
$ABRE_TRAZA
If SWTRA
    Call OUVRE_TRACE(TITTR) From LECFIC
    Call ECR_TRACE(string$(70,"-"),0) From GESECRAN
    Call ECR_TRACE(TITTR,0) From GESECRAN
    Call ECR_TRACE(string$(70,"-"),0) From GESECRAN
    Call ECR_TRACE(" ",0) From GESECRAN
Endif
Call TEMPON("") From GESECRAN

Return
###########################################################
# Funcion para cerrar la traza
###########################################################
$CIERRA_TRAZA
Call TEMPOFF From GESECRAN
If SWTRA
    Call FERME_TRACE From LECFIC
    Call LEC_TRACE From LECFIC
Endif
Return

#Llamada a un contador por parametro, creacion de un parametro general
# planta, paraametro y devuelve vaalor
Call PARAML([M:EXS0]FCY(NOL),"CNSTYPCUR",TYPCOU) From ADOVAL


# ---------------------------------------------------------------------------------#
#Ver si existe una transaccion de sistema llevandose a cabo
# ---------------------------------------------------------------------------------#
if Adxlog = 1 --> hay transaccion
if Adxlog <> 1 --> no hay y puedes empezar a escribir en la tabla

#Actualizar la tabla abierta con el registro adecuado

If [F:BPC]BPCNUM <> BPCORD
    Read [F:BPC]BPC0 = BPCORD
    If fstat Raz [F:BPC] : Endif
Endif


#Tratamiento de lineas especificas de pedidos o entregas o lo que sea, ventas o compras:
#Añades la accion VALIG que llama a la alimentacion de las lineas mediante las acciones

$VALLIG
#---------------------------------------------------#
# Traitements complementaires effectués à la ligne  #
# SIGN   vaut +1 en création / modification         #
#             -1 en annulation                      #
# TRTLIG vaut "C","M","A" en créa, modif, annule    #
# l'enregistrement "ligne" est en ligne             #
#---------------------------------------------------#
If [M:YSOH1]YGESBOB = 2
  Local Integer NOL : NOL=nolign-1
  Local Integer WRET : WRET = 0
  # --> Annulation
  If    SIGN = -1 & TRTLIG = "A"
      Gosub LECLIG_YSOD : If GOK < 1 Return : Endif
      Gosub SUPLIG_YSOD : If GOK < 1 Return : Endif
  # --> Modification
  Elsif SIGN = -1 & TRTLIG = "M"
      Gosub LECLIG_YSOD
  Elsif SIGN = +1 & TRTLIG = "M"
      Gosub INIMOD_YSOD : If GOK < 1 Return : Endif
   # --> Création
  Elsif SIGN = +1 & TRTLIG = "C"
      Gosub INICRE_YSOD : If GOK < 1 Return : Endif
  Endif
  If GOK < 1 Return : Endif

  # --> Création et modification apres passage dans le moteur d'allocation
  If SIGN = +1
      If TRTLIG = "C" Gosub CRELIG_YSOD : Endif
      If TRTLIG = "M" Gosub MODLIG_YSOD : Endif
  Endif
Endif
Return

#El valig llama a los YSOD, que son los siguientes: ver SPESOHA -> KOEX




####################################################################################
#       SUPRESION POR CODIGO DE UNA LINEA DE UN cuadro
####################################################################################



# --> ContrÃ´le de suppression de lignes
If status =  65
   GMODIF = 1
   NOL=nolign-1
   #Borrado del registro en la YSG
   Call BORRA_YSG([M:BPC0]BPCNUM, [M:YSG0]CPY(NOL), [M:YSG0]CIASEG(NOL), [M:YSG0]CONTRATO(NOL)) From YSEGCREDIT
    #Si ha habido algun error sales
   If mkstat End Endif
Endif

If status = 68 | status = 83
   GMODIF = 1
   For NOL=nolign-1 To nolign1-1
      Call BORRA_YSG([M:BPC0]BPCNUM, [M:YSG0]CPY(NOL), [M:YSG0]CIASEG(NOL), [M:YSG0]CONTRATO(NOL)) From YSEGCREDIT
      If mkstat Break Endif
   Next NOL
   #Si ha habido algun error sales
   If mkstat End Endif
Endif

# REFERENCIA


# --> ContrÃ´le de suppression de lignes
If status =  65
   NOL=nolign-1
   GMODIF = 1
   Gosub TEST_ANU From SUBSDHB
   If mkstat End Endif
   Call DELSTOSORW(NOL,NOL,"SDH1",[M:SDH0]STOFCY,WRET) From STKSOR
   If WRET<>0 mkstat=2 : End Endif
Endif
If status = 68 | status = 83
   GMODIF = 1
   For NOL=nolign-1 To nolign1-1
      Gosub TEST_ANU From SUBSDHB
      If mkstat Break Endif
   Next NOL
   If mkstat End Endif
   Call DELSTOSORW(nolign-1,nolign1-1,"SDH1",[M:SDH0]STOFCY,WRET) From STKSOR
   If WRET<>0 mkstat=2 : End Endif
Endif

# ---------------------------------------------------------------------------------#
#Comprobacion de errores  con el mkstat
#Si ha habido algun error sales del subrog
   If mkstat End Endif
   #Si ha habido algun error sales del bucle
   If mkstat break Endif
   #Si ha habido algun error sales del gosub
   If mkstat return Endif
# ---------------------------------------------------------------------------------#

# ---------------------------------------------------------------------------------#
# oner un campo en invisible de forma dinámica mediante codigo
Ya hemos usado alguna vez la sentencia:
# ---------------------------------------------------------------------------------#
               Chgfmt  [M:ICD2]MATLEV0    With "-k:"+num$(15)+"X"                                               
               Chgfmt  [M:ICD2]MATLEV0    With "k:"+num$(15)+"X"                                                

Para que poner un campo invisible en pantalla o bien mostrarlo. Esto se puede hacer asociado a cualquier acción de campo o rutina.

Y ahora, he visto que hay otra sentencia para cambiar el ‘Titulo’ o la ‘Etiqueta’ que se le pone a un campo en una pantalla si por ejemplo debe aparecer otra con alguna condición, la sentencia es:

               Chgtzn [M:ICD2]MATTOTLEV With mess(2,6001,1)

Indicando el campo de la pantalla y el texto que se quiere que aparezca, se puede asociar a la acción que uno quiera.

Si os miráis la ayuda del campo Chgfmt os salen las distintas sentencias Chg…que hay…

#########################################################################
#         OBTENER PRECIO NETO DE UN ARTICULO      - INICIO              #
#########################################################################
#       If  WTIPOCOSTE = 1
#          COSTE_ASOCIADO =  [WITV]AVC
#        Elsif  WTIPOCOSTE = 2
#          COSTE_ASOCIADO = [WITV]LASPURPRI
#        Elsif  WTIPOCOSTE = 3
#          COSTE_ASOCIADO = [WITV]LASRCPPRI
#        Elsif  WTIPOCOSTE = 4
#           Call GETPRISTD([WITM]ITMREF,"","",WPLANTA,date$,0,1,COSTE_ASOCIADO,WRET) From STKVAL
#       Endif
#             USOS
#           Call GETPRI(WCODCAL,"","",PLANTA,FECHA,WPESONETO,"","",4,PRECIO_ENTRADA,WRET) From STKVAL
#           Call GETPRI(WARTICULO,"","",WPLANTA,date$,UNIDAD,"","",1,COSTE_ASOCIADO,WRET) From STKVAL
#Local Decimal COSTE_STD
#           Call GETPRISTD("COMPO1FLU","","","NPE01",date$,0,1,COSTE_STD,WRET) From STKVAL
#           Infbox "ARTICULO: COMPO1FLU " + "  COST ESTD: " + num$(COSTE_STD)
#           Call GETPRISTD("COMPO2FLU","","","NPE01",date$,0,1,COSTE_STD,WRET) From STKVAL
#           Infbox "ARTICULO: COMPO2FLU " + "  COST ESTD: " + num$(COSTE_STD)
#           Call GETPRISTD("COMPO3FLU","","","NPE01",date$,0,1,COSTE_STD,WRET) From STKVAL
#           Infbox "ARTICULO: COMPO3FLU " + "  COST ESTD: " + num$(COSTE_STD)
#           Raz COSTE_STD
           Call GETPRISTD([WITM]ITMREF,"","",WPLANTA,date$,0,1,COSTE_ASOCIADO,WRET) From STKVAL
#           Infbox "1-ARTICULO: " + [WITM]ITMREF + " - Planta: " + WPLANTA + "  COST ESTD: " + num$(COSTE_STD)
#           Raz COSTE_STD
#           Call GETPRISTD2([WITM]ITMREF,"","",WPLANTA,date$,1,1,0,COSTE_STD,WRET) From STKVAL
#           Infbox "2-ARTICULO: " + [WITM]ITMREF + " - Planta: " + WPLANTA + "  COST ESTD: " + num$(COSTE_STD)
#           Raz COSTE_STD
#            Call GETPRI([WITM]ITMREF,"","",WPLANTA,date$,1,"","",1,COSTE_STD,WRET) From STKVAL
#           Infbox "3-ARTICULO: " + [WITM]ITMREF + " - Planta: " + WPLANTA + "  COST ESTD: " + num$(COSTE_STD)

#########################################################################
#         OBTENER PRECIO NETO DE UN ARTICULO - FIN                      #
#########################################################################

# ---------------------------------------------------------------------------------#
Cuando quieres modificar las lineas de algun tipo de documento con campos especificos como es el caso de KOEX
con las bobinas, debes irte al TRT correspondiente y sacar de ahi los datos correspondientes de los puntos de entrada
por ejemplo en el caso de las entregas cuando en pedidos de venta pasas de forma directa y quieres arrastrar esos campos
existe un punto de menu en el TRTVENLIV quqe se llama ALISOQSDH
# ---------------------------------------------------------------------------------#


# ---------------------------------------------------------------------------------#
El GAU_CHE contiene el numero del picking en el que estas, GAU_CHE1 , GAU_CHE2, GAU_CHE3 son cada uno de los picking de un objeto
Ejemplo de un funprog
# ---------------------------------------------------------------------------------#

Funprog PUEDE_ASIGNARSE(WPACA, WANCHO, WESPESOR, WPLIEGUE, WTUBO)
Value Char WPACA
Value Decimal WANCHO
Value Integer WESPESOR
Value Char WPLIEGUE
Value Char WTUBO

# ---------------------------------------------------------------------------------#
If clalev([F:YSTL])=0 Local File STOLOT     [YSTL]  Endif

Local Integer ASIGNABLE

Raz ASIGNABLE

Filter [F:YSTL] Where YPACA = WPACA
  Read Last
  If !fstat
    If ([F:YSTL]YANCHO <> WANCHO) or ([F:YSTL]YESPESOR <> WESPESOR) or ([F:YSTL]YPLIEGUE <> WPLIEGUE) or ([F:YSTL]YTUBO <> WTUBO)
      ASIGNABLE = 1
      Else
      ASIGNABLE = 2
    Endif
  Endif
Filter [F:YSTL]


#Infbox "PACA ASIGNABLE? : " + num$(ASIGNABLE)

End ASIGNABLE
