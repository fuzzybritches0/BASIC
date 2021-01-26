DECLARE SUB robsay (saverob!)
DECLARE SUB humsay (savehum!, Name$, repuplic$)
DECLARE SUB cmin (A!, B!, c!, red$)
DECLARE SUB waitkey ()
DECLARE SUB Schuessellegen (ge)           'Unterprogramme deklarieren
DECLARE SUB win (A!, B!, c!, d!, E!)    'Ende
begame2:
COPYRIGHT$ = "Copyright by Curt Manucredo, cm-SOFT, all rights reserved!"
COLOR 0, 7                              'Farben einstellen
CLS                                     'Bildschirm l”schen
win 5, 9, 15, 60, 10
LOCATE 6, 25: PRINT "cm-SOFT's M A N C A L A V1.00"
LOCATE 7, 10: PRINT STRING$(58, "Í")
LOCATE 11, 11: PRINT "                New Game, press 'G'"
LOCATE 13, 11: PRINT "                End Game, press 'E'"

axel:
Tas$ = INKEY$
IF Tas$ = "G" OR Tas$ = "g" THEN GOTO begame
IF Tas$ = "E" OR Tas$ = "e" THEN GOTO endex
GOTO axel

begame:
CONST AL = 14                          'Dimensionieren von sb()
DIM sb(AL)                              'Dimensioniern von  sb()
win 5, 9, 15, 60, 10
LOCATE 6, 25: PRINT "cm-SOFT's M A N C A L A V1.00"
LOCATE 7, 10: PRINT STRING$(58, "Í")
LOCATE 9, 11: PRINT "               Hy, I'm Oweekoo!"
LOCATE 11, 11: PRINT "Thank you first for trying cm-SOFT's MANCALA! We'll"
LOCATE 12, 11: PRINT "have together much fun, so press a key to start!"
waitkey
COLOR 0, 7                              'Farben einstellen
CLS
win 5, 9, 15, 60, 10
LOCATE 6, 25: PRINT "cm-SOFT's M A N C A L A"
LOCATE 7, 10: PRINT STRING$(58, "Í")
LOCATE 10, 12: PRINT "Before we start I want to know your Name!"
LOCATE 11, 12: PRINT "Could you do me that favorite, and type your Name in!"
red$ = Name$
cmin 13, 28, 15, red$
Name$ = red$
LOCATE 15, 12: PRINT "From which repuplic are you"
red$ = repuplic$
cmin 17, 28, 15, red$
repuplic$ = red$
COLOR 0, 7                              'Farben einstellen
CLS
win 5, 9, 15, 60, 10
LOCATE 6, 25: PRINT "cm-SOFT's M A N C A L A"
LOCATE 7, 10: PRINT STRING$(58, "Í")
LOCATE 9, 12: PRINT "Okay "; Name$; ", the stones witch felt on the table"
LOCATE 10, 12: PRINT "will tell me now who plays first!"
RANDOMIZE TIMER
rando = INT(RND * 2)
IF NOT rando = 0 THEN starter$ = "* It looks like the stones give you the first move!"
IF rando = 0 THEN starter$ = "* It looks like the stones favore me today!"
LOCATE 12, 12: PRINT starter$
LOCATE 14, 12: PRINT "Press any key to continue!"
waitkey
COLOR 0, 7                              'Farben einstellen
CLS

NewGame:
Schuessellegen ge  'Scháeln malen
FOR A = 1 TO 14  'Allen Scháeln die richtige Menge an Steinen zuteilen
sb(A) = 4        'Scháeln
NEXT A           'Ende
sb(7) = 0        'Endscháeln mit 0 anfllen (HUMAN)
sb(14) = 0       'Endscháel mit 0 anfllen  (COMPUTER)
GOSUB ms         'Aufrufen des Scháelerstellungsprogramm

IF rando = 0 THEN GOTO robot


gol:
robsay saverob
saverob = sb(14)
LOCATE 13, 35: PRINT "You move now!"
humall = 0
FOR tog = 1 TO 6
humall = humall + sb(tog)
NEXT tog
IF humall = 0 THEN GET$ = "HUM": GOTO whowins
SOUND 220, 3
del = 2          'Cursorkorrekturstarteinstellung (Scháel 1,2)
cs = 10                         'Starteinstellung fr Cursor (Scháel 1, 10)
LOCATE 23, 1: PRINT STRING$(80, " ")
go2:                            'Kreislauf  fr Tastenerkennung
A$ = INKEY$                     'Tastenerkennung
IF cs >= 70 THEN cs = 60                'Tastentrimmung
IF cs <= 0 THEN cs = 10                 'Tastentrimmung
IF MID$(A$, 2, 1) = "M" THEN cs = cs + 10   'Tastenerkennung
IF MID$(A$, 2, 1) = "K" THEN cs = cs - 10   'Tastenerkennung
IF NOT del = cs THEN LOCATE 23, del + 5: PRINT " "  'Cursorkorrektur
LOCATE 23, cs + 5: PRINT CHR$(24)                   'Cursordarstellung
IF A$ = CHR$(27) THEN GOTO endex
cho = cs / 10
IF A$ = " " THEN human = 1: LOCATE 23, del + 5: PRINT " ": GOTO mov'Satart
del = cs
GOTO go2

mov:
stones = sb(cho)
IF human = 1 THEN IF stones = 0 THEN GOTO gol
IF human = 0 THEN IF stones = 0 THEN GOTO robot


sb(cho) = 0
FOR axi = 1 TO 50
RANDOMIZE TIMER
snd = INT(RND * 800)
IF snd < 50 THEN snd = 50
SOUND snd, .1
NEXT axi
ge = cho


DO UNTIL stones = 0
ge = ge + 1
SOUND 1000, .5
IF human = 1 THEN IF ge = 14 THEN ge = 1
IF human = 0 THEN IF ge = 7 THEN ge = 8
IF ge = 15 THEN ge = 1
sb(ge) = sb(ge) + 1
stones = stones - 1
GOSUB ms
LOCATE 23, 25: PRINT "                                           "
LOCATE 23, 22: PRINT "Status: House:"; ge; "/Stones in your Hand:"; stones
SLEEP 1
LOOP
IF human = 1 THEN IF ge = 7 THEN GOTO gol
IF human = 0 THEN IF ge = 14 THEN GOTO robot
IF sb(ge) > 1 THEN cho = ge: savehum = savehum - sb(7): saverob = saverob - sb(14): GOTO mov





IF human = 1 THEN GOTO robot
IF human = 0 THEN GOTO gol




robot:
LOCATE 23, 1: PRINT STRING$(80, " ")
humsay savehum, Name$, repuplic$
savehum = sb(7)
LOCATE 13, 35: PRINT "I move now!   "

roball = 0
FOR tog = 8 TO 13
roball = roball + sb(tog)
NEXT tog
IF roball = 0 THEN GET$ = "ROB": GOTO whowins
SOUND 440, 3
IF sb(13) = 1 THEN cho = 5: GOTO sume
IF sb(12) = 2 THEN cho = 4: GOTO sume
IF sb(11) = 3 THEN cho = 3: GOTO sume
IF sb(10) = 4 THEN cho = 2: GOTO sume
IF sb(9) = 5 THEN cho = 1: GOTO sume
IF sb(8) = 6 THEN cho = 0: GOTO sume
RANDOMIZE TIMER
cho = INT(RND * 5)
sume:
cho = cho + 8
human = 0: GOTO mov














END
ms:
Schuessellegen ge
COLOR 0, 7
FOR Zahl = 10 TO 60 STEP 10
LOCATE 4, 4 + 70 - Zahl: PRINT sb(Zahl / 10 + 7)


FOR poin = 1 TO sb(Zahl / 10 + 7)
IF Anz = 6 OR Anz = 12 OR Anz = 18 OR Anz = 24 OR Anz = 30 THEN leg = leg + 1: Anz = 0
Anz = Anz + 1
LOCATE 5 + Anz, leg + 2 + 70 - Zahl: PRINT "0"
NEXT poin
Anz = 0
leg = 0

NEXT Zahl
FOR Zahl = 10 TO 60 STEP 10
LOCATE 22, 4 + Zahl: PRINT sb(Zahl / 10)

FOR poin = 1 TO sb(Zahl / 10)
IF Anz = 6 OR Anz = 12 OR Anz = 18 OR Anz = 24 OR Anz = 30 THEN leg = leg + 1: Anz = 0
Anz = Anz + 1
LOCATE 14 + Anz, leg + 2 + Zahl: PRINT "0"
NEXT poin
Anz = 0
leg = 0

NEXT Zahl

LOCATE 5, 4: PRINT sb(14)

FOR poin = 1 TO sb(14)
IF Anz = 6 OR Anz = 12 OR Anz = 18 OR Anz = 24 OR Anz = 30 THEN leg = leg + 1: Anz = 0
Anz = Anz + 1
LOCATE 7 + Anz, leg + 3: PRINT "0"
NEXT poin
Anz = 0
leg = 0


LOCATE 5, 74: PRINT sb(7)

FOR poin = 1 TO sb(7)
IF Anz = 6 OR Anz = 12 OR Anz = 18 OR Anz = 24 OR Anz = 30 THEN leg = leg + 1: Anz = 0
Anz = Anz + 1
LOCATE 7 + Anz, leg + 2 + Zahl: PRINT "0"
NEXT poin
Anz = 0
leg = 0

RETURN

whowins:
IF GET$ = "HUM" THEN sb(7) = sb(7) + roball
IF GET$ = "ROB" THEN sb(14) = sb(14) + humall
IF sb(14) > sb(7) THEN winner$ = "Computer"
IF sb(7) > sb(14) THEN winner$ = "Human"
IF sb(7) = sb(14) THEN winner$ = "Nobody"
CLS
win 5, 9, 15, 60, 10
LOCATE 6, 25: PRINT "cm-SOFT's M A N C A L A"
LOCATE 7, 10: PRINT STRING$(58, "Í")
IF winner$ = "Computer" THEN logtext$ = "Don't be sad, everybody did now that I win!"
IF winner$ = "Human" THEN logtext$ = "You must cheated somehow, you can't be the winner!"
IF winner$ = "Nobody" THEN logtext$ = "You actually didn't win, but also didn't lose!"
LOCATE 9, 11: PRINT logtext$
LOCATE 11, 15: PRINT "Computer:            "; Name$; ":"
LOCATE 13, 17: PRINT sb(14): LOCATE 13, 38: PRINT sb(7)
waitkey
GOTO begame2:
endex:
SHELL "cls"
END

SUB cmin (A, B, c, red$)
red:
   LOCATE A, B: PRINT red$
   LOCATE A, B + LEN(red$): PRINT "_"
   LOCATE A, B + LEN(red$) + 1: PRINT " "
   A$ = INKEY$
      IF A$ = CHR$(27) THEN red$ = "": GOTO ende
      IF A$ = CHR$(3) GOTO ende
      IF A$ = CHR$(13) GOTO retry
      IF LEN(red$) = 0 GOTO asit
      IF A$ = CHR$(8) THEN red$ = LEFT$(red$, LEN(red$) - 1): A$ = ""
asit:
      IF A$ = CHR$(8) OR A$ = CHR$(9) OR LEN(A$) >= 2 OR A$ = "," OR A$ = "-" OR A$ = CHR$(59) THEN A$ = "": BEEP
   red$ = red$ + A$
   A$ = ""
      IF LEN(red$) >= c THEN BEEP: red$ = LEFT$(red$, c - 1)
   LOCATE A, B + LEN(red$): PRINT STRING$(1, 0)
   GOTO red
retry:
      IF red$ = "" GOTO red
ende:
LOCATE A, B + LEN(red$): PRINT " "
END SUB

SUB cminp (A, B, c, red$)
red2:
   LOCATE A, B: PRINT STRING$(LEN(red$), "*")
   LOCATE A, B + LEN(red$): PRINT "_"
   LOCATE A, B + LEN(red$) + 1: PRINT " "
   A$ = INKEY$
      'IF a$ = CHR$(27) GOTO ende2
      'IF a$ = CHR$(3) GOTO ende2
      IF A$ = CHR$(13) GOTO retry2
      IF LEN(red$) = 0 GOTO asit2
      IF A$ = CHR$(8) THEN red$ = LEFT$(red$, LEN(red$) - 1): A$ = ""
asit2:
      IF A$ = CHR$(8) OR A$ = CHR$(9) OR LEN(A$) >= 2 OR A$ = ":" OR A$ = "," OR A$ = "-" OR A$ = CHR$(59) THEN A$ = "": BEEP
   red$ = red$ + A$
   A$ = ""
      IF LEN(red$) >= c THEN BEEP: red$ = LEFT$(red$, c - 1)
   LOCATE A, B + LEN(red$): PRINT STRING$(1, 0)
   GOTO red2
retry2:
      IF red$ = "" GOTO red2
ende2:
LOCATE A, B + LEN(red$): PRINT " "
END SUB

SUB humsay (savehum, Name$, repuplic$)
line1$ = "Super, Duper! Go like this and you`ll win!"
line2$ = "                                          "
IF savehum = 0 THEN line1$ = "I know that you are going to become a good player!"
IF savehum = 0 THEN line2$ = "But don't be sad, everybody makes misstakes!"
IF savehum = 1 OR savehum = 2 OR savehum = 3 THEN line1$ = "Great some points for you!!!!"
IF savehum = 1 OR savehum = 2 OR savehum = 3 THEN line2$ = "You are even better than a blondie! Are you blond?"
IF savehum = 5 THEN line1$ = "You've got two points, fantastic!"
IF savehum = 5 THEN line2$ = "Congratulation, you are above a blondie " + Name$ + "!"
IF savehum = 6 THEN line1$ = "Oh good, you are soooooo great!"
IF savehum = 6 THEN line2$ = "Did you learn that in " + repuplic$ + "?"
IF savehum = 7 THEN line1$ = "Almost imposible what you did!"
IF savehum = 7 THEN line2$ = "But don't let it go to your head!"
thendo:
LOCATE 1, 5: PRINT STRING$(74, " ")
LOCATE 2, 5: PRINT STRING$(74, " ")
LOCATE 1, 5: PRINT line1$
LOCATE 2, 5: PRINT line2$
END SUB

SUB robsay (saverob)
line1$ = "Super, Duper! I'll continue like this and I`ll win!"
line2$ = "                                                   "
IF saverob = 0 THEN line1$ = "Ups! I couldn't see that coming!"
IF saverob = 0 THEN line2$ = "But don't think you are perfect, everybody makes misstakes!"
IF saverob = 1 OR saverob = 2 OR saverob = 3 THEN line1$ = "Great some points for me!!!!"
IF saverob = 1 OR saverob = 2 OR saverob = 3 THEN line2$ = "Now be ready to loose!"
IF saverob = 5 THEN line1$ = "I've got two points, fantastic!"
IF saverob = 5 THEN line2$ = "Can you see, I will be the winner!"
IF saverob = 6 THEN line1$ = "Oh god, I am soooooo perfect!"
IF saverob = 6 THEN line2$ = "I am the best, I do that since I'm a baby!"
IF saverob = 7 THEN line1$ = "Almost imposible what I did!"
IF saverob = 7 THEN line2$ = "But you know that I'm the best!!!!!!"
letrun:
LOCATE 1, 5: PRINT STRING$(74, " ")
LOCATE 2, 5: PRINT STRING$(74, " ")
LOCATE 1, 5: PRINT line1$
LOCATE 2, 5: PRINT line2$
END SUB

SUB Schuessellegen (ge)

win 6, 2, 14, 8, 4 + c

FOR R = 1 TO 60 STEP 10
win 5, 10 + R, 7, 9, 4 + c
NEXT R

win 6, 71, 14, 8, 5 + c

FOR R = 1 TO 60 STEP 10
win 14, 10 + R, 7, 9, 5 + c
NEXT R

END SUB

SUB vers (all$, verss$)
        FOR axe = 1 TO LEN(all$)
         verss$ = verss$ + STRING$(1, ASC(MID$(all$, axe, 1)) + 20)
        NEXT axe
END SUB

SUB versu (all$, verss$)
        FOR axe = 1 TO LEN(all$)
         verss$ = verss$ + STRING$(1, ASC(MID$(all$, axe, 1)) - 20)
        NEXT axe
END SUB

SUB waitkey
A:
IF NOT B$ = INKEY$ THEN GOTO endes
A$ = INKEY$
B$ = A$
GOTO A
endes:
END SUB

SUB win (A, B, c, d, E)
 COLOR F, E
   LOCATE A, B: PRINT "É"; STRING$(d - 2, "Í"); "»"
   FOR x = 1 TO c
    LOCATE A + x, B: PRINT "º"; STRING$(d - 2, " "); "º"
   NEXT x
   LOCATE A + c, B: PRINT "È"; STRING$(d - 2, "Í"); "¼"
END SUB

