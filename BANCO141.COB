

       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BANCO141.
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
  
       77 FS                   PIC XX.
       77 WS-LIMPA             PIC X(40) VALUE SPACES.
       77 WS-CODIGO            PIC X(05) VALUE SPACES.
       77 WS-P                 PIC X VALUE SPACES.
       77 WS-RECEBE            PIC X VALUE SPACES.
       77 WS-SITUACAO          PIC X VALUE SPACES. 
       88 SIT-88               VALUE "D" "C" "A" "X" "B".

       77 WS-CONTA             PIC 9.
       88 CONTA-88             VALUE  1 2 3.

       77 WS-CIDADE            PIC X(15) VALUE "SAPUCAIA DO SUL".
       77 WS-CONTA-CODIGO      PIC X(05) VALUE ZEROS.
       77 WS-NUM-CODIGO        PIC 9(05) VALUE ZEROS.
       77 WS-NUM-CHE           PIC 9(06) VALUE ZEROS.
       77 WS-MOSTRA-VALOR      PIC ZZZ,ZZ9.99 VALUE ZEROS.

 
      ***********************************

       SCREEN SECTION.
       01 TELA-1.
           02 LINE 23 COLUMN 02 VALUE "Num.Doc.".

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
           DISPLAY (23 00) ERASE.
           DISPLAY TELA-1.
       P02-DATA.
           PERFORM P-DATA.
       P04-CODIGO.
           ACCEPT (23 12) WS-NUM-CHE WITH PROMPT AUTO-SKIP.
           IF WS-NUM-CHE = ZEROS perform p-FIM.
           IF WS-NUM-CHE = 9 perform p-FIM2.
           MOVE WS-NUM-CHE TO NUM-CHE.
       P-LER. 
           READ BANCO
               INVALID KEY
               DISPLAY (22 04) "!! < TITULO NAO Cadastrado > !!"
               DISPLAY (23 04) " Tecle  -  <  ENTER  > "
               ACCEPT WS-P.
           PERFORM P-REC2 THRU P-REC42.
           PERFORM P-CONFERE.
           REWRITE REG-BCO.
           DISPLAY (21 04) WS-LIMPA.
           DISPLAY (22 04) WS-LIMPA.
           DISPLAY (23 04) WS-LIMPA.
           GO TO P01-TELA-1.           
      *******************************************

       P-DATA.
           ACCEPT WS-DATA FROM DATE.
           DISPLAY (02 72) DIA.
           DISPLAY (02 75) MES.
           DISPLAY (02 78) ANO.
           MOVE DIA TO DIA-C. 
           MOVE MES TO MES-C. 
           MOVE ANO TO ANO-C. 

      *-----------------------------------------* 
      * P-REC1. 
      *     ACCEPT (08 25) NUM-CHE WITH PROMPT.
      *     IF NUM-CHE = SPACES perform p-FIM.
       P-REC2. 
           DISPLAY (23 20) DESCRI-CHE.
       P-REC3. 
           MOVE VALOR-CHE TO WS-MOSTRA-VALOR.
           DISPLAY (23 40) WS-MOSTRA-VALOR.
       P-REC4.
           DISPLAY (23 30) DIA-CHE "/".
       P-REC41.
           DISPLAY (23 33) MES-CHE "/". 
       P-REC42.
           DISPLAY (23 36) ANO-CHE.
      *-----------------------------------------*

       P-CONFERE.
           DISPLAY (24 40) "Conf.< S/N >? ".
           ACCEPT (24 54) WS-RECEBE WITH PROMPT AUTO-SKIP.  
           IF WS-RECEBE = "S" OR "s" OR "0"
               MOVE WS-DATA-CONF TO DATA-CONF.

       P-FIM.
           CLOSE BANCO.
           DISPLAY (01 01) ERASE.
           CHAIN "BANCO14.COM".   
       P-FIM2.
           CLOSE BANCO.
           DISPLAY (01 01) ERASE.
           CHAIN "BANCO.COM".   





