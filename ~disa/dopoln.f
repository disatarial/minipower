\ наиболее используемые библиотеки и доработки их

REQUIRE  { lib\ext\locals.f
REQUIRE STR@         ~ac/lib/str5.f
REQUIRE  F. lib\include\float2.f
REQUIRE  objLocalsSupport ~day/hype3/locals.f

CLASS F:
   10 DEFS locVar
: F@ locVar F@ ;
: F! locVar F! ;
;CLASS



 : PickUpWord { \ sstr  mlen len  -- }  \  выделить одно первое слово из строчки некоторые приборы выдают много всего ненужного
 0 -> len  -> mlen ->  sstr
mlen 0 > IF  \ длинна больше 0 
   mlen 0 DO
   sstr I + C@ [CHAR] ; =
   sstr I + C@ [CHAR] , = OR
   sstr I + C@ 10 = OR
   sstr I + C@ 13 = OR
   sstr I + C@ 32 = OR  
   sstr I + C@ 0 = OR  
   len 0= AND 
   IF  I  ->  len  THEN
\    sstr I + C@ .  len .
   LOOP
   len IF len    ELSE mlen THEN    -> mlen 
ELSE 0 -> mlen THEN
  sstr mlen   \ 2DUP . . 
 ;
 

\ перевести adr u  в динамическую строку
: STR>S  { \ str  --  adr u   - s-adr } 
"" -> str  str STR+  str  ;


: prob_simv? { \ n --  } 
  C@ -> n
   n  [CHAR] ; =
   n  [CHAR] , = OR
   n  10 = OR
   n  13 = OR
   n  32 = OR  
   n 0 = OR 
\   n . 
 ;

: TYPE_BUFER { bufer } \ показать  буфер в котором 1 символ- его длинна
bufer  1+ 
bufer  C@   DUP 0 > IF TYPE ELSE 2DROP ."  no info in buffer "  THEN 
;


 : N_S_PickUpWord {  sstr n \ len_n len_k str slen   len wordnumb_k wordnumb_n -- }  \  выделить одно  N-ное слово из строчки
sstr STR@ -> slen  -> str   \ адр эталона и его длинна
0 -> len_n slen  -> len_k 
slen  0 > IF
slen  0 DO
\	ловим начало и конец слов
\ CR
	str I + prob_simv?  \ y: -1  n: 0
	I IF str I + 1 - prob_simv? 0 = AND THEN \ 
	IF \ конец
\		I . str I +  C@ . 
		I  IF
			n wordnumb_k = IF  I  -> len_k THEN
			wordnumb_k 1+ -> wordnumb_k	
		THEN
	THEN
	
	str I + prob_simv? 0=  \ y: -1  n: 0
	I IF str I + 1 - prob_simv?  AND THEN  \ 
	IF 	\ начало
\	I . str I +  C@ .
		n wordnumb_n = IF I -> len_n THEN
		wordnumb_n 1+ -> wordnumb_n
	THEN
LOOP
THEN
\ CR len_n . len_k  . CR
 str    len_n   +    len_k   len_n   -   STR>S 
sstr STRFREE 
;



 
 : -STYPE  \ { \ sstr  mlen len -- }
 DUP >R 
   STR@    PickUpWord     TYPE 
  R> STRFREE
  ;
  
  
 CREATE strLF 10 C,
 CREATE strCR 13 C,  
 CREATE str0 0 C,  
 CREATE strCRLF 13 C, 10 C,
 
: S+CR   >R strLF   1 R@ STR+ R> ;
: S+LF   >R strCR   1 R@ STR+ R> ;
: S+CRLF >R strCRLF 2 R@ STR+ R>  ;


HERE CHAR " , CONSTANT _''_  : '' _''_ 1 ; \ {''} =>  "

\ сделать число строчкой


\ недоделанные операции
: >=  < 0= ; \ 2DUP  = >R  > R> OR ;
: <= > 0= ; \  2DUP  = >R  < R> OR  ;
 \ : F>  FDUP F< F= OR 0= ;
: F> FSWAP F-  F0< ; \ 
: F>=  F< 0= ;
: F<=  F> 0= ;
 


 : >NUM S>D (D.) ; \  str.f: { 1 >NUM } => "1"



: str>num   { adr u \ numb flag ch  signum --  n true | false  }
\ ." *"  u .  ."  *" adr u TYPE ." *  " 
u 0 > IF
1 ->  signum
0 -> numb
TRUE -> flag
u 0 DO 
adr I + C@ 48 - -> ch  \ ch . \  символ в число
ch 9 > ch 0 < OR IF  ch -3 =   IF -1 -> signum 0 -> ch 0 -> numb   ELSE  ch -5 = IF 0 -> numb   0 -> ch  ELSE 0 -> flag  THEN THEN THEN
numb 10 * ch + -> numb
LOOP
signum  numb *    flag 
ELSE 0 1  THEN
;
 : s>num { s -- \ }
 s STR@ str>num
 s STRFREE
 ;

: del_probel { sadr \ adr u i } \ elfkbnm ghj,tks
sadr STR@ -> u -> adr
BEGIN
adr i  + C@   33 <  
i u  < AND

WHILE
i 1+ -> i
REPEAT
 adr i + u i -  STR>S sadr STRFREE 
-> sadr 
\ sadr STR@ TYPE ." -" 

sadr STR@ -> u -> adr
u  -> i
\ i .
BEGIN
 adr i  + C@      33  < 
i 1  > AND
WHILE
i 1- -> i
REPEAT
adr  i  1+  STR>S sadr STRFREE 
 u . i . 
;


: ONLYNUMBER { adr u \ char n }
u 0 >
IF
	u -> n
	u 0 DO
		adr I + C@  -> char
		char [CHAR] . =
		char [CHAR] e = OR
		char [CHAR] , = OR
		char [CHAR] E = OR
		char [CHAR] + = OR
		char [CHAR] - = OR
		char  47 > char 58 < AND OR 
		0=  
		IF I -> n  \ ELSE 32 adr I +  ! 
		THEN
	LOOP
	
ELSE  0 ->  n THEN
adr n 
;

: S>FLOAT {  \ str adr u  m1 m2 n_str E0 delta  flag -- flag }
\ del_probel -> str 

0 N_S_PickUpWord -> str 
str STR@ ONLYNUMBER  -> u -> adr
u 0= IF 0 EXIT THEN
-1 -> flag
 adr u  "   <" STYPE TYPE   " >   " STYPE 
u  DUP -> m1 -> m2
u 0  DO
adr I + C@  [CHAR] . = IF  I -> m1 THEN
adr I + C@  [CHAR] , = IF  I -> m1 THEN
adr I + C@  [CHAR] E = IF  I -> m2 THEN
adr I + C@  [CHAR] e = IF  I -> m2 THEN
LOOP
\ m1 . m2 .  u .
m1 m2 > IF m2 -> m1 THEN
\ m1 m2 > IF m2 -> m1 THEN
\ ."  - "   
"" -> n_str
 adr m1  n_str STR+  \ число до точки
\ m2  m1 - .
\ "  |" STYPE n_str STYPE  " |  " STYPE
m1 1-  -> E0
 m1 m2 >=  \ ТОЧКИ НЕТ
 IF m2 1- -> E0
 ELSE
 m1 1- -> E0 \ знаков до точки
 adr m1 1 +  +  \ начало числа после точки
 m2 m1 - 1 -   \ разность между точкой и началом E 
\ DUP  . DUP E0 - -> E0 \ 
 n_str STR+ \ добавил к строке
  THEN \ число после точки

\  "    |" STYPE n_str STYPE  " |    " STYPE E0 . ."        "
  n_str STR@  DUP 8 > IF   DROP 8   STR>S n_str STRFREE  -> n_str ELSE 2DROP  THEN \ отраничили значащие цыфры
\  "    |" STYPE n_str STYPE  " |    " STYPE E0 . ."        "
n_str STR@ -> delta DROP \ количество значащих символов в числе
\  "    |" STYPE n_str STR@ TYPE  " |    " STYPE E0 . ."        "
  n_str s>num    0= IF ." <number error_n>"   DROP 0  0 -> flag THEN \  ?SLITERAL1
 DS>F    \  FS. ."   " 
 m2 1+  u <= IF  \ есть что-то после "E"
  adr m2 +  1 + u m2 - 1 - \ TYPE   SPACE
 str>num   \ 2DUP DROP . 
  0= IF ." <number error_e>" DROP 0 0 -> flag  THEN
 10E DS>F F**    F* 
 THEN
\  10E E0 F**    F* 
\ E0 . delta . 
 10E E0 delta  - 1 + DS>F F**    F* 
str STRFREE
flag 

;


 : STR>FLOAT { \ ss -- } \ если цифра тогда кладе на флоат стек
 "" DUP  -> ss STR+ ss  S>FLOAT 
 ;  


REQUIRE TIME&DATE lib/include/facil.f
\ REQUIRE UseDLL ~nn/lib/usedll.f


 : F. >FNUM TYPE ."  " ;  \ 


 : FS. ( r -- )
   PrintFInf IF EXIT THEN
   (F.) IF [CHAR] - FEMIT THEN
   1 FDISPLAY 1- .EXP \  SPACE
;

 
 : >FNUM ( F: r  -- addr u )
   PAD-COUNT 0!
   ['] C-TO-PAD TO FEMIT
   ['] S-TO-PAD TO FTYPE
    FS.
   ['] EMIT TO FEMIT
   ['] TYPE TO FTYPE
   PAD PAD-COUNT @
;


:  ?STR_FILE  (  adr u -- 1 | 0 )
100 PAUSE
 R/O OPEN-FILE   0=
  IF  CLOSE-FILE  THROW -1
 ELSE DROP 0  THEN
;

: ERR-INCLUDE-PROBE
 DUP IF  ."  Include: " 2DUP TYPE CR
 ELSE 2DROP S" "  THEN  
  2DUP ?STR_FILE    IF INCLUDE-PROBE IF CR ERR-STRING TYPE CR 1 ELSE 0 THEN   ELSE  1 THEN 
  \ добавить контроль ошибок!
;

: ASCIIZ-ERR-INCLUDE-PROBE  DUP IF ASCIIZ>  ELSE  0 THEN   ERR-INCLUDE-PROBE ;


: IMMEDIATE_EVALUATE EVALUATE ; IMMEDIATE

\EOF

: IM_ORDER ORDER ; IMMEDIATE
: IM_WORDS WORDS ; IMMEDIATE


: a
" dsca" 0  N_S_PickUpWord STR@ str>num . . CR
" 1 " 0 N_S_PickUpWord STR@  str>num . . CR
" 12" 0 N_S_PickUpWord STR@ str>num . . CR
" 12 " 0 N_S_PickUpWord STR@ str>num . . CR

;
: x
" dths" S>FLOAT  F. . CR
" фвымфвым " S>FLOAT  F. . CR
 " 56" S>FLOAT  F. . CR
 " .e" S>FLOAT  F. . CR
 " e" S>FLOAT  F. .  CR
 " . " S>FLOAT  F. . CR
" 1.23" S>FLOAT  F. . CR
" 123" S>FLOAT  F. . CR
" 123e" S>FLOAT  F. . CR
" 123." S>FLOAT  F. . CR
" 321.e" S>FLOAT  F. . CR
" 1.23e2" S>FLOAT  F. . CR
" 123456789123456" S>FLOAT  F. . CR

" .01" S>FLOAT  F. DROP   CR
" .1" S>FLOAT  F. DROP   CR
" 1" S>FLOAT  F. DROP   CR
" 12" S>FLOAT  F. DROP   CR
" 123" S>FLOAT  F. DROP   CR
" 1234" S>FLOAT  F. DROP   CR
" 12345" S>FLOAT  F. DROP   CR
" 123456" S>FLOAT  F. DROP   CR
" 1234567" S>FLOAT  F. DROP   CR
" 12345678" S>FLOAT  F. DROP   CR
" 123456789" S>FLOAT  F. DROP   CR
" 1234567890" S>FLOAT  F. DROP   CR
;

: z
 " 1" 0 N_S_PickUpWord " |" STYPE  STYPE  " |" STYPE CR
 " 1234" 0 N_S_PickUpWord  " |" STYPE  STYPE  " |" STYPE CR
 "  1234" 0 N_S_PickUpWord " |" STYPE  STYPE  " |" STYPE CR
 " 1234 " 0 N_S_PickUpWord " |" STYPE  STYPE " |" STYPE CR
 "  1234 " 0 N_S_PickUpWord " |" STYPE  STYPE  " |" STYPE CR

 " 1234  789" 1 N_S_PickUpWord " |" STYPE  STYPE  " |" STYPE CR
 "  1234  789" 1 N_S_PickUpWord " |" STYPE  STYPE  " |" STYPE CR
 " 1234  789 " 1 N_S_PickUpWord " |" STYPE  STYPE  " |" STYPE CR
 "  1234  789 " 1 N_S_PickUpWord " |" STYPE  STYPE  " |" STYPE CR

 " 1234  789" 0 N_S_PickUpWord " |" STYPE  STYPE  " |" STYPE CR
 "  1234  789" 0 N_S_PickUpWord " |" STYPE  STYPE  " |" STYPE CR
 " 1234  789 " 0 N_S_PickUpWord " |" STYPE  STYPE  " |" STYPE CR
 "  1234  789 " 0 N_S_PickUpWord " |" STYPE  STYPE  " |" STYPE CR

  ; 
