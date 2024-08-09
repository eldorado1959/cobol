
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BANCO21.
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
           03 ANO-N            PIC 99.
           03 MES-N            PIC 99.
           03 DIA-N            PIC 99.
       01 WS-DATA-C.
           03 DIA-C            PIC 99.
           03 MES-C            PIC 99.
           03 ANO-C            PIC 99.
       01 WS-DATA-INSC.
           03 DIA-INSC         PIC 99.
           03 MES-INSC         PIC 99.
           03 ANO-INSC         PIC 99.
  
       77 FS                   PIC XX.
       77 WS-LIMPA             PIC X(40) VALUE SPACES.
       77 WS-P                 PIC X VALUE SPACES.
       77 WS-SITUACAO          PIC X VALUE SPACES. 
       88 SIT-88               VALUE "D" "C" "A" "X" "B".

       77 WS-CONTA             PIC 9.
       88 CONTA-88             VALUE  0 1 2 3.

       77 WS-CIDADE            PIC X(15) VALUE "SAPUCAIA DO SUL".
       77 WS-CONTA-CODIGO      PIC X(05) VALUE ZEROS.
       77 WS-NUM               PIC 9(06) VALUE ZEROS.
       77 WS-DESCRI            PIC 99 VALUE ZEROS.
       77 WS-CONTA-CHE         PIC X(10) VALUE SPACES.

 
      ***********************************

       SCREEN SECTION.
       01 TELA-1.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 67 VALUE "DATA:  /  /  ".
           02 LINE 02 COLUMN 01 VALUE "ROGERIO FERNANDO MACHADO".
           02 LINE 03 COLUMN 25 VALUE " CONTROLE BANCARIO " BLINK. 
           02 LINE 04 COLUMN 02 VALUE " CADASTRO  TITULOS " BLINK.
           02 LINE 04 COLUMN 57 VALUE " < LANCA DE TITULOS >".
           02 LINE 05 COLUMN 57 VALUE "< AINDA NAO RECEBIDOS >".
           02 LINE 06 COLUMN 01 PIC X(80) FROM ALL "_".
           02 LINE 08 COLUMN 04 VALUE "NUMERO DO TITULO...=".
           02 LINE 09 COLUMN 04 VALUE "...................=".
           02 LINE 10 COLUMN 04 VALUE "FAVORECIDO.........=".
           02 LINE 11 COLUMN 04 VALUE "VECTO TITULO.......=".
           02 LINE 12 COLUMN 04 VALUE "VALOR DO TITULO....=".
           02 LINE 16 COLUMN 01 VALUE "Ao finalizar operacao retorna".
           02 LINE 16 COLUMN 32 VALUE "automaticamente ao CADASTRO DE".
           02 LINE 16 COLUMN 63 VALUE " PRODUTOS !!!!".
           02 LINE 22 COLUMN 01 PIC X(80) FROM ALL "=".
           02 LINE 22 COLUMN 05 VALUE "MENSAGEM".

       01 TELA-CONTA.
           02 LINE 12 COLUMN 30 VALUE "<4> TITULOS".

       01 TELA-FORN.
           02 LINE 05 COLUMN 35 VALUE "<01> MERCOSUL".
           02 LINE 06 COLUMN 35 VALUE "<02> GERMANI".
           02 LINE 07 COLUMN 35 VALUE "<03> FRANGOSUL".
           02 LINE 08 COLUMN 35 VALUE "<04> TRANSP.".
           02 LINE 09 COLUMN 35 VALUE "<05> MARISOL ".
           02 LINE 10 COLUMN 35 VALUE "<06>      ".
           02 LINE 11 COLUMN 35 VALUE "<07>        ".
           02 LINE 12 COLUMN 35 VALUE "<08>       ".
           02 LINE 13 COLUMN 35 VALUE "<09>         ".
           02 LINE 14 COLUMN 35 VALUE "<10>             ".
                     
       01 TELA-LIMPA.
           02 LINE 05 COLUMN 35 VALUE "                ".
           02 LINE 06 COLUMN 35 VALUE "                ".
           02 LINE 07 COLUMN 35 VALUE "                ".
           02 LINE 08 COLUMN 35 VALUE "                ".
           02 LINE 09 COLUMN 35 VALUE "                 ".
           02 LINE 10 COLUMN 35 VALUE "                 ".
           02 LINE 11 COLUMN 35 VALUE "                ".
           02 LINE 12 COLUMN 35 VALUE "                ".
           02 LINE 13 COLUMN 35 VALUE "                ".
           02 LINE 14 COLUMN 35 VALUE "                ".
           02 LINE 15 COLUMN 35 VALUE "                ".
           02 LINE 12 COLUMN 30 VALUE "                ".

      *
       PROCEDURE DIVISION.
       P01-TELA-1.
           DISPLAY TELA-1.
       P02-DATA.
           PERFORM P-DATA.
       
       P03-ABERTURA.
           OPEN I-O BANCO.
           IF FS = "30"
               CLOSE BANCO
               OPEN OUTPUT BANCO
               CLOSE BANCO 
               GO TO P03-ABERTURA.
           PERFORM P-DATA.
       P04-CODIGO.
           ACCEPT (08 25) WS-NUM WITH PROMPT.
           IF WS-NUM = ZEROS perform p-FIM.
           MOVE WS-NUM TO NUM-CHE.
           MOVE "TITULOS  " TO WS-CONTA-CHE.
           DISPLAY (09 25) CONTA-CHE.
           DISPLAY TELA-LIMPA.
           DISPLAY TELA-FORN.
           ACCEPT (10 25) WS-DESCRI WITH PROMPT AUTO-SKIP.
           IF WS-DESCRI = 1 MOVE "MERCOSUL" TO DESCRI-CHE.
           IF WS-DESCRI = 2 MOVE "GERMANI" TO DESCRI-CHE.
           IF WS-DESCRI = 3 MOVE "FRANGOSUL " TO DESCRI-CHE.
           IF WS-DESCRI = 4 MOVE "TRANSP." TO DESCRI-CHE.
           IF WS-DESCRI = 5 MOVE "MARISOL" TO DESCRI-CHE.
           IF WS-DESCRI = ZEROS
               ACCEPT (10 27) DESCRI-CHE WITH PROMPT.
           DISPLAY (10 25) DESCRI-CHE.
           DISPLAY TELA-LIMPA.
       P-LER.
           READ BANCO
               INVALID KEY
                   PERFORM P-REC2 THRU P-REC5
                   GO TO P01-TELA-1.
           DISPLAY (22 04) "!! < Codigo ja Cadastrado > !!".
           DISPLAY (23 04) " Tecle  -  <  ENTER  > ".
           ACCEPT WS-P.
           DISPLAY (22 04) WS-LIMPA.
           DISPLAY (23 04) WS-LIMPA.
           DISPLAY (22 04) "<CODIGO =    ,Sai da Inclusao".
           GO TO P04-CODIGO.



      *******************************************


       P-DATA.
           ACCEPT WS-DATA FROM DATE.
           DISPLAY (02 72) DIA.
           DISPLAY (02 75) MES.
           DISPLAY (02 78) ANO.
      *-----------------------------------------* 

       P-REC2.
           MOVE WS-CONTA-CHE TO CONTA-CHE. 
           DISPLAY (09 25) CONTA-CHE.
       P-REC3. 
           ACCEPT (10 25) DESCRI-CHE WITH UPDATE PROMPT.
           IF DESCRI-CHE = SPACES perform p-FIM.
       P-REC4.
           ACCEPT (11 25) DIA-CHE WITH PROMPT AUTO-SKIP.
           IF DIA-CHE > 31 GO TO P-REC4.
       P-REC41.
           ACCEPT (11 28) MES-CHE WITH PROMPT AUTO-SKIP. 
           IF MES-CHE > 12 GO TO P-REC41.
       P-REC42.
      *     ACCEPT (11 31) ANO-CHE WITH PROMPT AUTO-SKIP.
      *     IF MES-CHE < 10 ADD 1 TO ANO.
           MOVE ANO TO ANO-CHE.
           MOVE ANO-CHE TO ANO-C.
           MOVE MES-CHE TO MES-C.
           MOVE DIA-CHE TO DIA-C.
      *     IF WS-DATA-C > WS-DATA GO TO P-REC4. 
       P-REC5.
           ACCEPT (12 25) VALOR-CHE WITH PROMPT AUTO-SKIP.
            
           MOVE 88 TO ANO-INSC.
           MOVE 88 TO MES-INSC.
           MOVE 88 TO DIA-INSC.
           MOVE WS-DATA-INSC TO INSCR-CHE.
           DISPLAY (21 65) INSCR-CHE.
           MOVE ZEROS TO DATA-CONF.
           PERFORM P-GRAVA.
             

       P-FIM.
           CLOSE BANCO.
           DISPLAY (01 01) ERASE.
           CHAIN "produt01.com".
      *     CALL "produt01.COM".

       P-GRAVA.
           WRITE REG-BCO INVALID KEY
               DISPLAY (21 04) "ERRO DE GRAVACAO - FS = " FS
               CLOSE BANCO
               STOP RUN.




