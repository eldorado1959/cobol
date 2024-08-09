
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BANCO3.
       AUTHOR. ROGERIO-MACHADO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANCO ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS NUM-CHE
                  FILE STATUS IS FS.
       DATA DIVISION.
       FILE SECTION.
       FD BANCO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "BCO.DAT".
       01 REG-BCO.
           03 NUM-CHE          PIC X(06).
           03 CONTA-CHE        PIC X(10).
           03 DESCRI-CHE       PIC X(20).
           03 DIA-CHE          PIC 99.   
           03 MES-CHE          PIC 99.   
           03 ANO-CHE          PIC 99.   
           03 VALOR-CHE        PIC 9(06)V99.
           03 OBS-CHE          PIC X(15). 
           03 INSCR-CHE        PIC 9(06).
           03 DATA-CONF        PIC 9(06).

      ***********************************

       WORKING-STORAGE SECTION.
         
       01 WS-DATA.
           03 ANO              PIC 99.
           03 MES              PIC 99.
           03 DIA              PIC 99.
       01 WS-DATA-N.
           03 DIA-N            PIC 99.
           03 MES-N            PIC 99.
           03 ANO-N            PIC 99.
       01 WS-DATA-INSC.
           03 DIA-INSC         PIC 99.
           03 MES-INSC         PIC 99.
           03 ANO-INSC         PIC 99.
  
       77 FS                   PIC XX.
       77 WS-LIMPA             PIC X(40) VALUE SPACES.
       77 WS-CODIGO            PIC X(05) VALUE SPACES.
       77 WS-P                 PIC X VALUE SPACES.
       77 WS-SITUACAO          PIC X VALUE SPACES. 
       88 SIT-88               VALUE "D" "C" "A" "X" "B".

       77 WS-CONTA             PIC 9.
       88 CONTA-88             VALUE  1 2 3.

       77 WS-CIDADE            PIC X(15) VALUE "SAPUCAIA DO SUL".
       77 WS-CONTA-CODIGO      PIC X(05) VALUE ZEROS.
       77 WS-NUM-CODIGO        PIC 9(05) VALUE ZEROS.
       77 WS-NUM-CHE           PIC 9(06) VALUE ZEROS.

 
      ***********************************

       SCREEN SECTION.
       01 TELA-1.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 67 VALUE "DATA:  /  /  ".
           02 LINE 02 COLUMN 01 VALUE "ROGERIO FERNANDO MACHADO".
           02 LINE 03 COLUMN 25 VALUE " CONTROLE BANCARIO " BLINK. 
           02 LINE 04 COLUMN 02 VALUE " CADASTRO  CHEQUES " BLINK.
           02 LINE 04 COLUMN 57 VALUE "< ALTERACAO - CHEQUES >".
           02 LINE 06 COLUMN 01 PIC X(80) FROM ALL "_".
           02 LINE 08 COLUMN 04 VALUE "NUMERO DO CHEQUE...=".
           02 LINE 09 COLUMN 04 VALUE "CONTA CORRENTE.....=".
           02 LINE 10 COLUMN 04 VALUE "FAVORECIDO.........=".
           02 LINE 11 COLUMN 04 VALUE "VECTO DO CHEQUE....=".
           02 LINE 12 COLUMN 04 VALUE "VALOR DO CHEQUE....=".
           02 LINE 13 COLUMN 04 VALUE "CADASTRO DO CHEQUE.=".
           02 LINE 22 COLUMN 01 PIC X(80) FROM ALL "=".
           02 LINE 22 COLUMN 05 VALUE "MENSAGEM".

       PROCEDURE DIVISION.
       
       P03-ABERTURA.
           OPEN I-O BANCO.
           IF FS = "30"
               CLOSE BANCO
               OPEN OUTPUT BANCO
               CLOSE BANCO 
               GO TO P03-ABERTURA.
           PERFORM P-DATA.


       P01-TELA-1.
           DISPLAY TELA-1.
       P02-DATA.
           PERFORM P-DATA.
       P04-CODIGO.
           DISPLAY (22 04) "<CODIGO =    ,Sai da Inclusao".
           ACCEPT (08 25) WS-NUM-CHE WITH PROMPT.
           IF WS-NUM-CHE = ZEROS perform p-FIM.
           MOVE WS-NUM-CHE TO NUM-CHE.
       P-LER.
           READ BANCO
               INVALID KEY
               DISPLAY (22 04) "!! < Codigo NAO Cadastrado > !!"
               DISPLAY (23 04) " Tecle  -  <  ENTER  > "
               ACCEPT WS-P.
           PERFORM P-REC2 THRU P-REC5.
           REWRITE REG-BCO.
           DISPLAY (22 04) WS-LIMPA.
           DISPLAY (23 04) WS-LIMPA.
           GO TO P01-TELA-1.           
      *******************************************

       P-DATA.
           ACCEPT WS-DATA FROM DATE.
           DISPLAY (02 72) DIA.
           DISPLAY (02 75) MES.
           DISPLAY (02 78) ANO.
      *-----------------------------------------* 
      * P-REC1. 
      *     ACCEPT (08 25) NUM-CHE WITH PROMPT.
      *     IF NUM-CHE = SPACES perform p-FIM.
       P-REC2. 
           ACCEPT (09 25) CONTA-CHE WITH UPDATE.
       P-REC3. 
           ACCEPT (10 25) DESCRI-CHE WITH UPDATE.
           ACCEPT (10 40) OBS-CHE WITH UPDATE.
       P-REC4.
           ACCEPT (11 25) DIA-CHE WITH UPDATE.
       P-REC41.
           ACCEPT (11 28) MES-CHE WITH UPDATE. 
       P-REC42.
           ACCEPT (11 31) ANO-CHE WITH UPDATE.
       P-REC5.
           ACCEPT (12 25) VALOR-CHE WITH UPDATE.
           ACCEPT (13 25) INSCR-CHE WITH UPDATE.
           ACCEPT (14 25) DATA-CONF WITH UPDATE.
           ACCEPT (23 01) WS-P.
      *-----------------------------------------*

       P-FIM.
           CLOSE BANCO.
           DISPLAY (01 01) ERASE.
           CHAIN "C:BANCO.COM".   





