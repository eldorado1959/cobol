
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BANCO05.
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
       77 WS-CODIGO            PIC 9(06) VALUE ZEROS.
       77 WS-P                 PIC X VALUE SPACES.
       77 WS-RECEBE            PIC X VALUE SPACES.
       77 WS-SITUACAO          PIC X VALUE SPACES. 
       88 SIT-88               VALUE "D" "C" "A" "X" "B".

       77 WS-CONTA             PIC 9.
       88 CONTA-88             VALUE  1 2 3.

       77 WS-CIDADE            PIC X(15) VALUE "SAPUCAIA DO SUL".
       77 WS-CONTA-CODIGO      PIC X(05) VALUE ZEROS.
       77 WS-NUM-CODIGO        PIC 9(05) VALUE ZEROS.
       77 WS-MOSTRA-VALOR      PIC ZZZ999V99.

 
      ***********************************

       SCREEN SECTION.

       01 TELA-1.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 67 VALUE "       /  /  ".
           02 LINE 03 COLUMN 25 VALUE " Controle Financeiro " BLINK. 
           02 LINE 05 COLUMN 04 VALUE "No.do Documento....=".
           02 LINE 06 COLUMN 04 VALUE "Conta..............=".
      *     02 LINE 10 COLUMN 04 VALUE "Departamento.......=".
           02 LINE 07 COLUMN 04 VALUE "Favorecido.........=".
           02 LINE 08 COLUMN 04 VALUE "Vecto Documento....=".
           02 LINE 09 COLUMN 04 VALUE "Valor Documento....=".
           02 LINE 10 COLUMN 04 VALUE "Data cad.Doc.......=".



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
           ACCEPT (05 25) WS-CODIGO WITH PROMPT AUTO-SKIP.
           IF WS-CODIGO = ZEROS perform p-FIM.
           MOVE WS-CODIGO TO NUM-CHE.
       P-LER. 
           READ BANCO
               INVALID KEY
               DISPLAY (22 04) "!! < Codigo NAO Cadastrado > !!"
               DISPLAY (23 04) " Tecle  -  <  ENTER  > "
               ACCEPT WS-P
               GO TO P04-CODIGO.
           PERFORM P-REC2 THRU P-REC5.
           IF DATA-CONF = 0
               PERFORM P-CONFERE.
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
           MOVE DIA TO DIA-C. 
           MOVE MES TO MES-C. 
           MOVE ANO TO ANO-C. 

      *-----------------------------------------* 
      * P-REC1. 
      *     ACCEPT (08 25) NUM-CHE WITH PROMPT.
      *     IF NUM-CHE = SPACES perform p-FIM.
       P-REC2. 
           DISPLAY (06 25) CONTA-CHE.
       P-REC3. 
           DISPLAY (06 38) "Depto".
           IF OBS-CHE = 01 DISPLAY (09 44) "LOJA".
           IF OBS-CHE = 02 DISPLAY (09 44) "ELETRO".
           IF OBS-CHE = 03 DISPLAY (09 44) "CALCADOS".
           IF OBS-CHE = 04 DISPLAY (09 44) "CONFEC.".
           IF OBS-CHE = 05 DISPLAY (09 44) "MERCADO".
           IF OBS-CHE = 06 DISPLAY (09 44) "FERRAGEM".
           IF OBS-CHE = 07 DISPLAY (09 44) "OUTROS".
           DISPLAY (07 25) DESCRI-CHE.
       P-REC4.
           DISPLAY (08 25) DIA-CHE "/".
       P-REC41.
           DISPLAY (08 28) MES-CHE "/". 
       P-REC42.
           DISPLAY (08 31) ANO-CHE.
       P-REC5.
           MOVE VALOR-CHE TO WS-MOSTRA-VALOR.
           DISPLAY (09 25) WS-MOSTRA-VALOR.
           DISPLAY (10 25) INSCR-CHE.
           IF DATA-CONF NOT = 0
               move data-CONF to ws-data-conf
               DISPLAY (12 25) "Lancamento Conferido: "
               DISPLAY (12 47)  dia-c "/" mes-c "/" ano-c
               STOP " ". 
      *-----------------------------------------*

       P-CONFERE.
           DISPLAY (14 10) "Conf.Conferencia do Lancamento <S/N>? ".
           ACCEPT (14 48) WS-RECEBE WITH PROMPT AUTO-SKIP.  
           IF WS-RECEBE = "S" OR "s" OR "0"
               MOVE WS-DATA-CONF TO DATA-CONF.

       P-FIM.
           CLOSE BANCO.
           DISPLAY (01 01) ERASE.
           CHAIN "BANCO.COM".   





