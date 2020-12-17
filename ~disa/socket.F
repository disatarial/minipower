\  работа с  сокетами, на основе рсокет  ~ac
\  все лежит в отдельном словаре
 
 REQUIRE  F. ~disa\dopoln.f
 
\ REQUIRE  { lib\ext\locals.f
 REQUIRE fsockopen  ~ac/lib/win/winsock/ws2/PSOCKET.F
REQUIRE  HYPE ~day\hype3\hype3.f
CLASS socket_port
CELL PROPERTY idport


: init SocketsStartup THROW ;
: open  fsockopen    idport ! ;
: close idport @ fclose  ;
: write \ { str  --  }   
DUP STR@ TYPE ."  "

>R \ " {CRLF}" R@ S+
R@ idport @ fputs   

R> STRFREE
;

: read \ { obj \ str --  adr u  }
 idport @ fgets \ >R STR@ -> str obj 8 + str  CMOVE  
 \ str  obj  4 + !  obj 8 + str   
 \ R> STRFREE
\ STYPE
  DUP STR@ TYPE ."  "
;
;CLASS

(
0 VALUE smb100
: xxx
 socket_port NewObj TO smb100
  " 192.168.0.19" 5025 smb100 ^   open
   "  *IDN?" smb100 ^ write
\ " FREQ                20000000"        smb100 write
  smb100  ^ read STYPE
 smb100 ^ close
 smb100 ^ dispose
;
)