
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BANCO28.
       AUTHOR. ROGERIO-MACHADO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BANCO ASSIGN TO DISK
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS SEQUENTIAL
      *            ACCESS MODE IS DYNAMIC
                  RECORD KEY IS NUM-CHE
                  FILE STATUS IS FS.
       DATA DIVISION.
       FILE SECTION.


       FD BANCO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "BCO.DAT".
       01 REG-PROD.
           03 NUM-CHE          PIC X(06).
           03 CONTA-CHE        PIC X(10).
           03 DESCRI-CHE       PIC X(05).
           03 DESCRI2-CHE      PIC X(15).
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
       77 FS                   PIC XX.
       77 WS-LIMPA             PIC X(55) VALUE SPACES.
       77 WS-SETOR             PIC X(10) VALUE SPACES.
       77 WS-SENHA             PIC X(04) VALUE SPACES.
       77 WS-CONT              PIC X VALUE SPACES.
       77 WS-ANO               PIC 99 VALUE ZEROS.
      ***********************************

       SCREEN SECTION.
       01 TELA-INIC.
           02 BLANK SCREEN.
           02 LINE 10 COLUMN 24 VALUE "! !  A t e n c a o  ! !".
           02 LINE 11 COLUMN 24 VALUE "   Exclusao de Setor " blink.
       01 TELA-1.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 67 VALUE "DATA:  /  /  ".
           02 LINE 04 COLUMN 02 VALUE "CONTROLE DE ESTOQUE".
           02 LINE 04 COLUMN 57 VALUE "< EXCLUSAO  - BANCO >".
           02 LINE 06 COLUMN 01 PIC X(80) FROM ALL "_".
           02 LINE 10 COLUMN 10 VALUE "VALOR A VISTA :".
           02 LINE 11 COLUMN 10 VALUE "VALOR A PRAZO :".
           02 LINE 12 COLUMN 04 VALUE "INFORME SETOR A SER EXCLUIDO".
           02 LINE 13 COLUMN 10 VALUE "[          ]".
           02 LINE 20 COLUMN 01 PIC X(80) FROM ALL "=" BLINK.
           02 LINE 20 COLUMN 05 VALUE "Produt07" BLINK.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * 
       PROCEDURE DIVISION.
       
       P01-TELA-1.
           DISPLAY TELA-INIC.
           ACCEPT WS-CONT.
           DISPLAY TELA-1.
       P-SENHA.
           DISPLAY (20 05) "PROGRAMA DE EXCLUSAO".  
      *     ACCEPT (22 01) WS-SENHA WITH PROMPT AUTO-SKIP.
      *     IF WS-SENHA NOT = "ERNA"
      *         PERFORM P-FIM.  
           DISPLAY (20 05) "INFORME ANO A SER EXCLUIDO".  
           DISPLAY (21 05) "                    ".  
      *     ACCEPT (22 01) WS-SETOR WITH PROMPT UPDATE AUTO-SKIP.
      *     IF WS-SETOR = SPACES
      *         PERFORM P-FIM.  
        
       P03-ABERTURA.
           OPEN I-O BANCO.
           IF FS = "30"
               DISPLAY (10 10) "ERRO DE ABERTURA DE ARQUIVO FS =" FS
               STOP RUN.
       P02-DATA.
           PERFORM P-DATA.
       P04-CODIGO.
           DISPLAY (21 05) "ANO = 00  , TERMINA EXCLUSAO".  
           ACCEPT (13 11) WS-ANO WITH PROMPT UPDATE.
           IF WS-ANO = ZEROS
               PERFORM P-FIM.  
           PERFORM P05-LER UNTIL FS = "10".
       P05-LER.
           READ BANCO NEXT RECORD
               AT END
                   DISPLAY (21 04) "  !! < Exclusao  Terminada > !!"
                   DISPLAY (22 04) " Tecle  -  <  ENTER  > "
                   ACCEPT (21 37) WS-CONT
                   DISPLAY (21 04) WS-LIMPA
                   DISPLAY (22 04) WS-LIMPA
                   PERFORM P-FIM.
               IF WS-ANO = ANO-CHE
                   DISPLAY (15 10) "DESCRICAO :" DESCRI-CHE
                   " " DESCRI2-CHE
                   PERFORM P-EXCLUI 
                   GO TO P05-LER.

      *******************************************              

       P-DATA.
           ACCEPT WS-DATA FROM DATE.
           DISPLAY (02 72) DIA.
           DISPLAY (02 75) MES.
           DISPLAY (02 78) ANO.

       P-EXCLUI.
           DELETE BANCO            
               INVALID KEY
                   DISPLAY (21 05) "!!!!!  ERRO DE DELECAO !!!!!" 
                   ACCEPT (21 40) WS-CONT
                   DISPLAY (21 05) WS-LIMPA.

       P-FIM.
           CLOSE BANCO.
           CHAIN "BANCO.COM".     




