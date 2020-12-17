\  работа с  последовательным портом под Виндовс
  REQUIRE  F. ~disa\dopoln.f

\ REQUIRE  { lib\ext\locals.f
\ REQUIRE STR@         ~ac/lib/str5.f
 
\  VOCABULARY com_port
\ ALSO com_port  DEFINITIONS
REQUIRE  HYPE ~day\hype3\hype3.f

CLASS com_port
32 CELLS CONSTANT num_bufer_port
CELL PROPERTY idport
 num_bufer_port DEFS bufer_port

\ константы Виндовс
-2147483648 CONSTANT GENERIC_READ
1073741824 CONSTANT GENERIC_WRITE

\ внутренние переменные
\ 0 VALUE handle
VARIABLE WasRead
VARIABLE ReadBuffer
\ ф-ии Виндовс

WINAPI: TransmitCommChar KERNEL32.DLL \ ( char handle -- ior )
WINAPI: SetCommTimeouts KERNEL32.DLL
WINAPI: GetCommTimeouts KERNEL32.DLL
WINAPI: BuildCommDCBA KERNEL32.DLL
WINAPI: SetCommState KERNEL32.DLL
WINAPI: GetCommState KERNEL32.DLL
WINAPI: WaitCommEvent KERNEL32.DLL
WINAPI: SetCommMask KERNEL32.DLL

0
CELL -- Internal
CELL -- InternalHigh
CELL -- Offset
CELL -- OffsetHigh
CELL -- hEvent
CONSTANT OVELAPPED
HERE DUP >R OVELAPPED DUP ALLOT ERASE VALUE overlap
VARIABLE Event

0   
CELL -- ReadIntervalTimeout
CELL -- ReadTotalTimeoutMultiplier
CELL -- ReadTotalTimeoutConstant
CELL -- WriteTotalTimeoutMultiplier
CELL -- WriteTotalTimeoutConstant
CONSTANT COMMTIMEOUTS
 
HERE DUP COMMTIMEOUTS DUP ALLOT ERASE VALUE CommTimeouts

0
CELL -- DCBlength           \ sizeof(DCB) 
CELL -- BaudRate            \ current baud rate 
CELL -- Mode
(   биты слова Mode
    ^-- fBinary:1            \ 1 - binary mode, no EOF check 
    ^-- fParity:1            \ 0 - enable parity checking 
    ^-- fOutxCtsFlow:1       \ 0 - CTS output flow control 
    ^-- fOutxDsrFlow:1       \ 0 - DSR output flow control 
    ^-- fDtrControl:2        \ 00 - DTR flow control type 
    ^-- fDsrSensitivity:1    \ 0 - DSR sensitivity 
    ^-- fTXContinueOnXoff:1  \ 0 - XOFF continues Tx 
    ^-- fOutX:1              \ 0 - XON/XOFF out flow control 
    ^-- fInX:1               \ 0 - XON/XOFF in flow control 
    ^-- fErrorChar:1         \ 0 - enable error replacement 
    ^-- fNull:1              \ 0 - enable null stripping 
    ^-- fRtsControl:2        \ 00 - RTS flow control 
    ^-- fAbortOnError:1      \ 0 - abort reads/writes on error 
    ^-- fDummy2:17           \ reserved 
)
2 -- wReserved            \ not currently used 
2 -- XonLim               \ transmit XON threshold 
2 -- XoffLim              \ transmit XOFF threshold 
1 -- ByteSize             \ number of bits/byte, 4-8 
1 -- Parity               \ 0-4=no,odd,even,mark,space 
1 -- StopBits             \ 0,1,2 = 1, 1.5, 2 
1 -- XonChar              \ Tx and Rx XON character 
1 -- XoffChar             \ Tx and Rx XOFF character 
1 -- ErrorChar            \ error replacement character 
1 -- EofChar              \ end of input character 
1 -- EvtChar              \ received event character 
2 -- wReserved1           \ reserved; do not use 
CONSTANT DCB

HERE DUP >R DCB DUP ALLOT ERASE VALUE MyDCB

: open {  str  --  handle -1 }  \ for example string "com1" or "com2"
\ -> str 
 0 0 OPEN_EXISTING 0 0 GENERIC_READ GENERIC_WRITE OR str STR@ DROP  CreateFileA
 DUP -1 = IF DROP -1 ELSE 0 THEN  SWAP idport !
 str STRFREE
;

: close ( -- ior )
idport @ CloseHandle DROP
;
\ прочитали из порта, положили в местный буфер ит выдали "adr u" на него
: read  \ {  \ adr u --  str }
  \ obj  CELL +  -> adr 
  bufer_port  num_bufer_port idport @ READ-FILE THROW 
   bufer_port   SWAP   "" DUP >R STR+ R>
   DUP STR@ TYPE ."  "
  ; 

: write {  str   \ adr u --  }
\ STR@ -> u -> adr
\ u 0 DO  adr I + @ obj 8 + I + ! LOOP obj 8 +  u  +   0x0A SWAP !  obj 8 +  u 1 +
\ " {CRLF}" str S+  
str  STR@ TYPE ."  "
str STR@  idport @ WRITE-FILE   THROW 
str STRFREE
;


: timeouts0 ( ms -- flag ) \ установка интервалов ожидания для чтения/записи в порт

100 CommTimeouts ReadIntervalTimeout !
1 CommTimeouts ReadTotalTimeoutMultiplier !
1000 CommTimeouts ReadTotalTimeoutConstant !
100 CommTimeouts WriteTotalTimeoutMultiplier !
1 CommTimeouts WriteTotalTimeoutConstant !
CommTimeouts idport @ SetCommTimeouts DROP
;

: init ( -- ior ) \ первоначальная инициализация порта

DCB MyDCB DCBlength !
 MyDCB    R@ GetCommState DROP
 9600  MyDCB BaudRate ! 
 0x80000000 MyDCB Mode !
 0 MyDCB  StopBits C!
 8 MyDCB ByteSize C!
 0 MyDCB StopBits C!
 0 MyDCB Parity C!
 \ 21649 MyDCB Mode ! \ для ths730
 10 MyDCB EvtChar C!
 MyDCB idport @ SetCommState  DROP
 timeouts0
; 
: set_comm ( BaudRate ByteSize StopBits Parity -- ior )
\ настройка порта

MyDCB    R@ GetCommState DROP
 MyDCB Parity C!
 MyDCB StopBits C!
 MyDCB ByteSize C!
 MyDCB BaudRate !
 MyDCB idport@  SetCommState 0 <>
;

\ choose port
\ : COM1 ( -- flag ) S" COM1" Open DUP IF Init 1000 Timeouts 2DROP THEN ;
\ : COM2 ( -- flag ) S" COM2" Open DUP IF Init 1000 Timeouts 2DROP THEN ;

\ PREVIOUS DEFINITIONS  ALSO com_port 

\ CreateComPort - "создать" компорт
\  CreateComPort "name"
\ при вызове "mane"
\ до конца строки? переключить поиск на словать "ALSO com_port"
\ после окончания   сделать "PREVIOUS"
(
: CreateComPort 
NextWord  
  SFIND IF DROP
 ELSE
   CREATED  
   0 ,  \ месло для id-порта
   259 ALLOT  \ место для чтения/записи компорта
THEN
DOES>tek730  
NextWord \ 1 PARSE   \ s-adr u
ALSO com_port   EVALUATE   PREVIOUS
; IMMEDIATE)
;CLASS
\ PREVIOUS DEFINITIONS 
(
 com_port NEW smp04
 " COM3"  smp04 open
\ smp04 timeouts0   
      " *idn?"  smp04 write
\   "  FREQ  9.e08 "   smp04 write 
\      " :measu:meas1:typ rms" tek730 write
\      " :measu:meas1:val?" tek730 write
 smp04  read  STYPE
  
   smp04 close 
)
\ CreateComPort COM1
\   " COM1"   COM1 Open 
\   COM1  DROP 
\    1000 COM1  Timeouts   DROP
\     " :measu:meas1:typ rms"  COM1  Write
\    " :measu:meas1:val?"   COM1  Write 
\ COM1  Read 
   \ COM1 Close DROP
  