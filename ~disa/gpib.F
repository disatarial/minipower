\  работа с gpib
\  все лежит в отдельном словаре
 REQUIRE  F. ~disa\dopoln.f

WINAPI: ibdev Gpib-32.dll
WINAPI: ibonl Gpib-32.dll
WINAPI: ibwrt Gpib-32.dll
WINAPI: ibrd Gpib-32.dll
WINAPI: ibclr Gpib-32.dll

WINAPI: SendDataBytes Gpib-32.dll



\ REQUIRE  { lib\ext\locals.f
\ REQUIRE STR@         ~ac/lib/str5.f
REQUIRE  HYPE ~disa\~day\hype3\hype3.f

CLASS gpib_port
32 CELLS CONSTANT num_bufer_port
 num_bufer_port DEFS bufer_port

CELL PROPERTY BoardIndex
CELL PROPERTY idport

\ : init SocketsStartup THROW ;

: open  ( adr gbib \ -- )
BoardIndex ! >R 
0 1 13 0 R>  BoardIndex @ ibdev  idport !  
idport @ ibclr DROP
;

: close \ idport @ fclose  
idport @ ibonl  
BoardIndex @  ibonl 
;
: write   { str  --  }   
str STR@ TYPE ."  "
\ " {CRLF}" str S+  
 str  STR@ SWAP idport @ ibwrt DROP
\ fputs   
str  STRFREE
;

: read \ { obj \ str --  c-adr-u  }
  num_bufer_port 0 DO bufer_port   I + 0! LOOP
  num_bufer_port bufer_port   idport @ ibrd  DROP 
  bufer_port   ASCIIZ>  \ TYPE
  STR>S 
  DUP STR@ TYPE ."  "
\  TYPE \ " www"
  \ "" DUP >R STR+ R>   

\ bufer_port  num_bufer_port idport @ READ-FILE THROW 
\    bufer_port   SWAP   "" DUP >R STR+ R>
;

: stype STYPE ;

;CLASS
(

  gpib_port NEW nrvd
  2 0 nrvd open
  " CH1;RA;TM0;DY100;DB;FA;FR0.08;OS40.00" nrvd write \ переключение каналов
  nrvd read STYPE
\ " *IDN?" nrvd write
\ " *TRG" nrvd write
\  " 192.168.0.19" 5025 smb100    open
\   "  *IDN?" smb100 write
\ " FREQ                20000000"        smb100 write
100 PAUSE
   nrvd read  STYPE
 nrvd close
 nrvd dispose
)