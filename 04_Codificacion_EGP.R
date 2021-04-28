#Esquema EGP con ELSOC
#Alejandro Plaza


# Cargar base de datos ----------------------------------------------------

load("C:/Users/Fondecyt R. Sociales/Dropbox/Proyecto Fondecyt Redes Sociales/LT Homofilia/Calculos EI/ELSOC_Wide_2016_2019_v1.00_R.RData")

#cambiar nombre
elsoc<- elsoc_wide_2016_2019

#paquetes
library(sjmisc)


#Revisión de variables y renombrar variables
frq(elsoc$cod_m03_w01) #CIUO
elsoc$isko<- elsoc$cod_m03_w01

frq(elsoc$m06_w01) #Supervisión
elsoc$sv<-elsoc$m06_w01

frq(elsoc$m07_w01) #relación de empleo
#codificacion
#1.Empleado u obrero en empresa privada = 1. Employee
#2. Empleado u obrero del sector publico (incluso empresa publica o municipalidad)=1. Employee
#3.Miembro de las Fuerzas Armadas y de Orden=NA
#4.Patron/a o empleador/a (contrata o paga a honorarios a uno/o o mas trabajadores/as)=2. Self-employed
#5.Trabaja solo, no tiene empleados/as=2. Self-employed
#6.Familiar no remunerado=NA
#7.Servicio domestico=NA
elsoc$s<-car::recode(elsoc$m07_w01, "1=1;2=1;3=NA;4=2;5=2;6:7=NA;-999:-888=NA")



# Fase 1: Codificación bruta en base a CIUO -------------------------------

#CODIGO BRUTO DE GANZEBOOM Y TREIMAN, ACÁ REQUIERE ALGUNAS MODIFICACIONES. SE SUGIERE LAS SIGUIENTES
#Profesores y Maestros (2320; 2331; 2332;2340;2351;2352;2359) = se modifican a rutina no manual (3) en vez de servicio II (2)
#Tenedores de libros (3433)=se modifican a rutina no manual (3) en vez de servicio II (2)
#vendedores ambulantes (9111; 9112;9113): Se modifican a trabajadores no calificados (9), en vez de trabajadores de rutina no manual
#

elsoc$egp<- car::recode(elsoc$isko, "1000= 1;1100= 1;1110= 1;1120= 1;1130= 2;1140= 2;
                        1141= 2;1142= 2;1143= 2;1200= 1;1210= 1;1220= 1;1221=11;
                        1222= 1;1223= 1;1224= 1;1225= 1;1226= 1;1227= 1;1228= 1;
                        1229= 1;1230= 1;1231= 1;1232= 1;1233= 1;1234= 1;1235= 1;
                        1236= 1;1237= 1;1239= 1;1240= 2;1250= 1;1251= 1;1252= 2;
                        1300= 2;1310= 2;1311=11;1312= 2;1313= 2;1314= 2;1315= 2;
                        1316= 2;1317= 2;1318= 2;1319= 2;2000= 1;2100= 1;2110= 1;
                        2111= 1;2112= 1;2113= 1;2114= 1;2120= 1;2121= 1;2122= 1;
                        2130= 1;2131= 1;2132= 2;2139= 2;2140= 1;2141= 1;2142= 1;
                        2143= 1;2144= 1;2145= 1;2146= 1;2147= 1;2148= 2;2149= 1;
                        2200= 1;2210= 1;2211= 1;2212= 1;2213= 1;2220= 1;2221= 1;
                        2222= 1;2223= 1;2224= 1;2229= 1;2230= 2;2300= 2;2310= 1;
                        2320= 3;2321= 2;2322= 2;2323= 2;2330= 2;2331= 2;2332= 2;
                        2340= 2;2350= 1;2351= 1;2352= 1;2359= 2;2400= 1;2410= 2;
                        2411= 1;2412= 2;2419= 2;2420= 1;2421= 1;2422= 1;2429= 1;
                        2430= 2;2431= 2;2432= 2;2440= 1;2441= 1;2442= 1;2443= 1;
                        2444= 2;2445= 1;2446= 2;2450= 2;2451= 2;2452= 2;2453= 2;
                        2454= 2;2455= 2;2460= 2;3000= 2;3100= 2;3110= 2;3111= 2;
                        3112= 2;3113= 2;3114= 2;3115= 2;3116= 2;3117= 2;3118= 2;
                        3119= 2;3120= 2;3121= 2;3122= 2;3123= 2;3130= 2;3131= 2;
                        3132= 2;3133= 2;3139= 2;3140= 2;3141= 2;3142= 2;3143= 1;
                        3144= 1;3145= 2;3150= 2;3151= 2;3152= 2;3200= 2;3210= 2;
                        3211= 2;3212= 2;3213= 2;3220= 2;3221= 2;3222= 2;3223= 2;
                        3224= 2;3225= 2;3226= 2;3227= 2;3228= 2;3229= 2;3230= 3;
                        3231= 3;3232= 3;3240= 2;3241= 2;3242= 2;3300= 3;3310= 3;
                        3320= 3;3330= 3;3340= 3;3400= 2;3410= 2;3411= 2;3412= 2;
                        3413= 2;3414= 2;3415= 2;3416= 2;3417= 2;3419= 2;3420= 2;
                        3421= 2;3422= 2;3423= 2;3429= 2;3430= 3;3431= 2;3432= 2;
                        3433= 3;3434= 2;3439= 3;3440= 2;3441= 2;3442= 2;3443= 2;
                        3444= 2;3449= 2;3450= 2;3451= 2;3452= 7;3460= 3;3470= 2;
                        3471= 2;3472= 2;3473= 2;3474= 2;3475= 2;3480= 3;4000= 3;
                        4100= 3;4110= 3;4111= 3;4112= 3;4113= 3;4114= 3;4115= 3;
                        4120= 3;4121= 3;4122= 3;4130= 3;4131= 3;4132= 3;4133= 3;
                        4140= 3;4141= 3;4142= 9;4143= 3;4144= 3;4190= 3;4200= 3;
                        4210= 3;4211= 3;4212= 3;4213= 3;4214= 3;4215= 3;4220= 3;
                        4221= 3;4222= 3;4223= 3;5000= 3;5100= 3;5110= 3;5111= 3;
                        5112= 3;5113= 3;5120= 3;5121= 3;5122= 8;5123= 3;5130= 3;
                        5131= 3;5132= 3;5133= 3;5139= 3;5140= 8;5141= 8;5142= 3;
                        5143= 8;5149= 3;5150= 2;5151= 2;5152= 2;5160= 9;5161= 8;
                        5162= 8;5163= 9;5164= 8;5169= 9;5200= 3;5210= 3;5220= 3;
                        5230= 3;6000=10;6100=10;6110=10;6111=10;6112=10;6113=10;
                        6114=10;6120=10;6121=10;6122=10;6123=10;6124=10;6129=10;
                        6130=10;6131=11;6132=11;6133=11;6134=10;6140=10;6141=10;
                        6142=10;6150=10;6151=10;6152=10;6153=10;6154=10;6200=11;
                        6210=11;7000= 8;7100= 8;7110= 8;7111= 8;7112= 8;7113= 8;
                        7120= 8;7121= 9;7122= 9;7123= 9;7124= 8;7129= 8;7130= 8;
                        7131= 9;7132= 8;7133= 8;7134= 8;7135= 9;7136= 8;7137= 8;
                        7140= 8;7141= 8;7142= 9;7143= 9;7200= 8;7210= 8;7211= 8;
                        7212= 8;7213= 8;7214= 8;7215= 8;7216= 8;7220= 8;7221= 8;
                        7222= 8;7223= 8;7224= 8;7230= 8;7231= 8;7232= 8;7233= 8;
                        7234= 9;7240= 8;7241= 8;7242= 8;7243= 8;7244= 8;7245= 8;
                        7300= 8;7310= 8;7311= 8;7312= 8;7313= 8;7320= 9;7321= 9;
                        7322= 9;7323= 8;7324= 8;7330= 9;7331= 9;7332= 9;7340= 8;
                        7341= 8;7342= 8;7343= 8;7344= 8;7345= 8;7346= 8;7400= 8;
                        7410= 8;7411= 8;7412= 8;7413= 8;7414= 8;7415= 8;7416= 8;
                        7420= 8;7421= 9;7422= 8;7423= 8;7424= 9;7430= 8;7431= 9;
                        7432= 9;7433= 8;7434= 8;7435= 8;7436= 8;7437= 8;7440= 8;
                        7441= 8;7442= 8;7500= 8;7510= 7;7520= 8;7530= 9;8000= 9;
                        8100= 9;8110= 8;8111= 8;8112= 8;8113= 8;8120= 8;8121= 8;
                        8122= 8;8123= 8;8124= 8;8130= 9;8131= 9;8139= 9;8140= 9;
                        8141= 9;8142= 9;8143= 9;8150= 8;8151= 8;8152= 8;8153= 8;
                        8154= 8;8155= 8;8159= 8;8160= 8;8161= 8;8162= 8;8163= 8;
                        8170= 8;8171= 8;8172= 8;8200= 9;8210= 8;8211= 8;8212= 9;
                        8220= 9;8221= 9;8222= 9;8223= 9;8224= 9;8229= 9;8230= 9;
                        8231= 9;8232= 9;8240= 9;8250= 9;8251= 9;8252= 9;8253= 9;
                        8260= 9;8261= 9;8262= 9;8263= 9;8264= 9;8265= 9;8266= 9;
                        8269= 9;8270= 9;8271= 9;8272= 9;8273= 9;8274= 9;8275= 9;
                        8276= 9;8277= 9;8278= 9;8279= 9;8280= 9;8281= 9;8282= 9;
                        8283= 9;8284= 9;8285= 9;8286= 9;8290= 9;8300= 9;8310= 9;
                        8311= 8;8312= 9;8320= 9;8321= 9;8322= 9;8323= 9;8324= 9;
                        8330= 9;8331=10;8332= 8;8333= 8;8334= 9;8340= 9;8400= 9;
                        9000= 9;9100= 3;9110= 3;9111= 3;9112= 3;9113= 3;9120= 9;
                        9130= 9;9131= 9;9132= 9;9133= 9;9140= 9;9141= 9;9142= 9;
                        9150= 9;9151= 9;9152= 9;9153= 9;9160= 9;9161= 9;9162= 9;
                        9200= 9;9210=10;9211=10;9212=10;9213=10;9300= 9;9310= 9;
                        9311= 9;9312= 9;9313= 9;9320= 9;9321= 9;9322= 9;9330= 9;
                        9331= 9;9332= 9;9333= 9 ")


# Fase 2: Codificación eb base a situación de empleo o situación d --------

#promocionalidad de ciertas ocupaciones
elsoc$p<-car::recode(elsoc$isko, "1000:9299=1; else=0")
#degradabilidad de ciertas ocupaciones
elsoc$d<-car::recode(elsoc$isko, "1300:1319=1;3400:3439=1;5000:5230=1;else=0")

#Nueva variables
elsoc$egp11<-elsoc$egp
#SI EGP="'IIIa Routine non-manuals" Y Supervisa a más de una persona, ENTONCES EGP= "II Lower controllers"
elsoc$egp11[elsoc$egp==3 & elsoc$sv>=1]<-2
#SI EGP="'IIIa Routine non-manuals" Y es autoempleado Y tiene degradez, ENTONCES EGP="'IIIb Lower sales-service'"                  
elsoc$egp11[elsoc$egp==3 & elsoc$s==2 & elsoc$d==1]<-4
#SI EGP>="V Manual supervisors" Y EGP<="VIIa Unskilled workers" Y es autoempleado Y tiene promocionalidad, ENTONCES EGP="'IVa Self-empl. with empl."
elsoc$egp11[elsoc$egp==2 & elsoc$s==2 & elsoc$d==1]<-4
#SI EGP>="V Manual supervisors" Y EGP<="VIIa Unskilled workers" Y es autoempleado Y tiene promocionalidad, ENTONCES EGP="'IVa Self-empl. with empl."
elsoc$egp11[elsoc$egp>=7 & elsoc$egp<=9  & elsoc$s==2 & elsoc$p==1]<-5
#SI EGP= VI Skilled workers' Y Supervisa a más de una persona, ENTONCES EGP= "V Manual supervisors' 
elsoc$egp11[elsoc$egp==8 & elsoc$sv>=1]<-7
#SI EGP= 'VIIb Farm labors Y es autoempleado, ENTONCES, EGP="IVc Self-empl. farmers"                   
elsoc$egp11[elsoc$egp==10 & elsoc$s==2]<-11
#SI EGP= 'IIIb Lower sales-service Y supervisa a menos de una persona, ENTONCES EGP= 'IVa Self-empl. with emp
elsoc$egp11[elsoc$egp==4 & elsoc$sv<1]<-5 #EN ORIGINAL SALE QUE DEBE SER "MENOS DE", PERO ESTO ES CONTRATEORICO
#SI EGP= IVa Self-empl. with empl.  Y supervisa desde una persona, ENTONCES, EGP= 'IIIb Lower sales-service'           
elsoc$egp11[elsoc$egp==5 & elsoc$sv>=1]<-4
#SI EGP= II Lower controllers' y supervisa a más de 10 personas, entonces EGP= I Higher controllers'                    
elsoc$egp11[elsoc$egp==2 & elsoc$sv>=10]<-1
#SI EGP= 'IIIa Routine non-manuals'' y supervisa a más de 10 personas, entonces EGP= I Higher controllers'
elsoc$egp11[elsoc$egp==3 & elsoc$sv>=10]<-1
#SI EGP= 'IIIb Lower sales-services' y supervisa a más de 10 personas, entonces EGP= I Higher controllers'                   
elsoc$egp11[elsoc$egp==4 & elsoc$sv>=10]<-1

table(elsoc$egp11)
elsoc$egp11<-as.integer(elsoc$egp11)

elsoc$egp_f<-car::recode(elsoc$egp11, "1='I Higher controllers';2='II Lower controllers';3='IIIa routine non-manuals';
                       4='IVa Self-empl. with empl.'; 5='IVb Self-empl. no empl.';7='V Manual supervisors';
                       8='VI Skilled workers'; 9='VIIa Unskilled workers';
                       10='VIIb Farm labors'; 11='IVc Self-empl. farmers';
                       100='Armed Force'; c(2470,7139,8287)=NA", as.factor=T,
                       levels=c("I Higher controllers", "II Lower controllers",
                                "IIIa routine non-manuals","IVa Self-empl. with empl.",
                                "IVb Self-empl. no empl.","V Manual supervisors","VI Skilled workers","VIIa Unskilled workers",
                                "VIIb Farm labors","IVc Self-empl. farmers", "Armed Force" ))

table(elsoc$egp_f)
prop.table(table(elsoc$egp_f))


#revision para imputación
table(elsoc$egp_f,elsoc$m02_w01)


