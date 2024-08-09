
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BANC271.
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
  
       01 WS-DATA-CONF.
           03 DIA-C            PIC 99.
           03 MES-C            PIC 99.
           03 ANO-C            PIC 99.

       77 WS-MOSTRA-VALOR      PIC ZZZ999V99.
       77 FS                   PIC XX.
       77 WS-LIMPA             PIC X(40) VALUE SPACES.
       77 WS-CODIGO            PIC 9(06) VALUE ZEROS.
       77 WS-P                 PIC X VALUE SPACES.
       77 WS-SITUACAO          PIC X VALUE SPACES. 
       88 SIT-88               VALUE "D" "C" "A" "X" "B".

       77 WS-CONTA             PIC 9.
       88 CONTA-88             VALUE  1 2 3.

       77 WS-CIDADE            PIC X(15) VALUE "SAPUCAIA DO SUL".
       77 WS-CONTA-CODIGO      PIC X(05) VALUE ZEROS.
       77 WS-NUM-CODIGO        PIC 9(05) VALUE ZEROS.

 
      ***********************************

       SCREEN SECTION.
       01 TELA-1.
           02 LINE 21 COLUMN 02 VALUE "No.Tit.=".
           02 LINE 21 COLUMN 25 VALUE "FAVORECIDO.=".
           02 LINE 22 COLUMN 02 VALUE "VCTO...=".
           02 LINE 22 COLUMN 25 VALUE "VALOR .....=".
           02 LINE 22 COLUMN 48 VALUE "Cad...=".
           02 LINE 24 COLUMN 01 PIC X(80) FROM ALL "=".
           02 LINE 24 COLUMN 70 VALUE "banco271".
           02 LINE 24 COLUMN 05 VALUE "MENSAGEM".


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
           ACCEPT (21 09) WS-CODIGO WITH PROMPT AUTO-SKIP.
           IF WS-CODIGO = 9 perform p-FIM-2.
           IF WS-CODIGO = ZEROS perform p-FIM.
           MOVE WS-CODIGO TO NUM-CHE.
       P-LER. 
           READ BANCO
               INVALID KEY
               DISPLAY (22 04) "!! < Codigo NAO Cadastrado > !!"
               DISPLAY (23 04) " Tecle  -  <  ENTER  > "
               ACCEPT WS-P.
           PERFORM P-REC2 THRU P-REC5.
           PERFORM P-CONFIRMA.
           GO TO P01-TELA-1.           
      *******************************************

       P-DATA.
           ACCEPT WS-DATA FROM DATE.
           DISPLAY (02 72) DIA.
           DISPLAY (02 75) MES.
           DISPLAY (02 78) ANO.
      *-----------------------------------------* 
       P-REC2.
           DISPLAY (21 25) CONTA-CHE.
       P-REC3. 
           DISPLAY (21 37) DESCRI-CHE.
       P-REC4.
           DISPLAY (22 09) DIA-CHE "/".
       P-REC41.       
           DISPLAY (22 12) MES-CHE "/". 
       P-REC42.
           DISPLAY (22 15) ANO-CHE.
       P-REC5.
           MOVE VALOR-CHE TO WS-MOSTRA-VALOR.
           DISPLAY (22 37) WS-MOSTRA-VALOR.
           DISPLAY (22 58) INSCR-CHE.
           IF DATA-CONF NOT = 0
               move data-CONF to ws-data-conf
               DISPLAY (23 35) "Tit.Pago: "
               DISPLAY (23 48)  dia-c "/" mes-c "/" ano-c.
      *-----------------------------------------*

       P-FIM.
           CLOSE BANCO.
           DISPLAY (01 01) ERASE.
           CHAIN "BANCO27.COM".   
       P-FIM-2.
           CLOSE BANCO.
           DISPLAY (01 01) ERASE.
           CHAIN "BANCO.COM".   

       P-CONFIRMA.
           DISPLAY (24 15) "Confirma Exclusao < S/N >?"
           ACCEPT (24 43) WS-P WITH AUTO-SKIP.
           IF WS-P = "s" OR "S" OR "0"
               DELETE BANCO.


