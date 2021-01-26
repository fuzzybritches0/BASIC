DIM monat(12)
        monat(1) = 31       'Jedes Monat wird angegeben, ohne Schaltjahr(365T)
        monat(2) = 28
        monat(3) = 31
        monat(4) = 30
        monat(5) = 31
        monat(6) = 30
        monat(7) = 31
        monat(8) = 31
        monat(9) = 30
        monat(10) = 31
        monat(11) = 30
        monat(12) = 31
PRINT "Berechnung des Wochentages mit dem Gregorianischen Kalender"
PRINT "-----------------------------------------------------------"
 INPUT "Geben Sie das Jahr ein :", Jahr
 INPUT "Geben Sie das Monat ein:", Monatt
 INPUT "Geben Sie den Tag ein  :", Tag
     Monatt = Monatt - 1     
        erg1 = Jahr / 4      
        erg0 = FIX(erg1)
        greg1 = Jahr / 100   
        greg2 = FIX(greg1)   
        greg3 = Jahr / 400  
        greg4 = FIX(greg3)   
erg2 = Jahr * 365         
 FOR month = 1 TO Monatt  
  erg3 = erg3 + monat(month)
 NEXT month
erg3 = erg3 + Tag          
erg4 = erg0 + erg2 + erg3 - greg2 + greg4      
erg5 = erg4 / 7           
erg5 = FIX(erg5)          
erg5 = erg5 * 7           
ergfin = erg4 - erg5       
IF greg1 = greg2 THEN IF NOT greg3 = greg4 THEN IF Monatt < 2 THEN erg0 = .5
IF erg1 = erg0 THEN IF Monatt < 2 THEN ergfin = ergfin - 1             
IF ergfin = -1 THEN ergfin = 6      
Tag$ = "Fehler"
IF ergfin = 0 THEN Tag$ = "Samstag"
IF ergfin = 1 THEN Tag$ = "Sonntag"
IF ergfin = 2 THEN Tag$ = "Monatg"
IF ergfin = 3 THEN Tag$ = "Dienstag"
IF ergfin = 4 THEN Tag$ = "Mittwoch"
IF ergfin = 5 THEN Tag$ = "Donnerstag"
IF ergfin = 6 THEN Tag$ = "Freitag"
Monatt = Monatt + 1                 
PRINT "Der "; Tag; Monatt; Jahr; "ist ein: "; Tag$; " (gewesen)!"

