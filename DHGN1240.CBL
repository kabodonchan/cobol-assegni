IDENTIFICATION DIVISION.
PROGRAM-ID. DHGN1240.
******************************************************************
*    B.S.T. - BANKING SOLUTIONS AND TECHNOLOGIES
*----------------------------------------------------------------
*
*
***+*********+*********+*********+*********+*********+*********+**
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SPECIAL-NAMES.
    DECIMAL-POINT IS COMMA.
DATA DIVISION.
*
WORKING-STORAGE SECTION.
******************************************************************
*    SWITCH FUNZIONALI
******************************************************************
01  COMODI.
    03 IND             PIC 9(4)  VALUE ZERO.
    03 COM-VALORE      PIC X(20) VALUE SPACES.
01  COMO.
    03 COM-STATO-CON   PIC X(02) VALUE SPACES.
    03 COM-NUM-RAPP-X.
      04 COM-NUM-RAPP    PIC 9(12) VALUE ZERO.
*
    03 I-P               PIC 9(4)  VALUE ZERO.
    03 I-P-MAX           PIC 9(4)  VALUE 20.
    03 I-TBER            PIC 9(4)  VALUE ZERO.
    03 I-T               PIC 9(4)  VALUE ZERO.
    03 I-T-MAX           PIC 9(4)  VALUE 20.
    03 I-WS              PIC 9(4)  VALUE ZERO.
    03 I-WS-MAX          PIC 9(4)  VALUE 20.
*
    03 COM-WS.
      04 COM-APPL-WS     PIC X(2)  VALUE SPACES.
      04 COM-ERR-WS      PIC 9(5)  VALUE ZERO.
*
    03 FLG-GRAVITA       PIC X(1).
      88 SI-NORMAL       VALUE 'N'.
      88 SI-BLOCCANTE    VALUE 'B'.
      88 SI-FORZABILE    VALUE 'F'.
      88 SI-SEGNALAZIONE VALUE 'S'.
*
01  COM-TBER.
    03 COM-FIL-TBER       PIC X(05) VALUE SPACES.
    03 COM-NR-CONTO       PIC 9(12) VALUE ZERO.
    03 COM-NR-CONTO-TBER  PIC X(12) VALUE SPACES.
    03 COM-CONTO-TBER     PIC X(20) VALUE SPACES.
*
01  COM-K-ABI               PIC X(05) VALUE '05696'.
01  COM-TRAN                PIC X(08) VALUE SPACES.
*
01  COM-ASSE.
    03 COM-NR-RIF.
      04 COM-ABI            PIC X(05) VALUE SPACES.
      04 COM-CAB            PIC X(05) VALUE SPACES.
      04 COM-NR-ASSEGNO     PIC 9(10) VALUE ZERO.
*
01  TAB-WS-ERRORI.
  03 COM-ERR-901.
    04 COM-APPL-1           PIC X(2)  VALUE 'CC'.
    04 COM-901              PIC 9(5)  VALUE 00901.
  03 COM-ERR-902.
    04 COM-APPL-2           PIC X(2)  VALUE 'CC'.
    04 COM-902              PIC 9(5)  VALUE 00902.
  03 COM-ERR-904.
    04 COM-APPL-4           PIC X(2)  VALUE 'CC'.
    04 COM-904              PIC 9(5)  VALUE 00904.
  03 COM-ERR-905.
    04 COM-APPL-5           PIC X(2)  VALUE 'CC'.
    04 COM-905              PIC 9(5)  VALUE 00905.
  03 COM-ERR-906.
    04 COM-APPL-6           PIC X(2)  VALUE 'CC'.
    04 COM-906              PIC 9(5)  VALUE 00906.
  03 COM-ERR-909.
    04 COM-APPL-9           PIC X(2)  VALUE 'CC'.
    04 COM-909              PIC 9(5)  VALUE 00909.
  03 COM-ERR-910.
    04 COM-APPL-10          PIC X(2)  VALUE 'CC'.
    04 COM-910              PIC 9(5)  VALUE 00910.
  03 COM-ERR-911.
    04 COM-APPL-11          PIC X(2)  VALUE 'CC'.
    04 COM-911              PIC 9(5)  VALUE 00911.
  03 COM-ERR-913.
    04 COM-APPL-13          PIC X(2)  VALUE 'CC'.
    04 COM-913              PIC 9(5)  VALUE 00913.
  03 COM-ERR-915.
    04 COM-APPL-15          PIC X(2)  VALUE 'CC'.
    04 COM-915              PIC 9(5)  VALUE 00915.
*
01  COM-PRESENTE            PIC X(02).
  88 PRESENTE-SI           VALUE 'SI'.
  88 PRESENTE-NO           VALUE 'NO'.
*
01  FLAG-PRESENTE            PIC X(02).
  88 FLG-PRESENTE-SI       VALUE 'SI'.
  88 FLG-PRESENTE-NO       VALUE 'NO'.
*
01  FLAG-SARA                PIC X(02).
  88 SI-SARA               VALUE 'SI'.
  88 NO-SARA               VALUE 'NO'.
*
01  FLAG-ASSNEGO             PIC X(02).
  88 SI-ASSNEGO            VALUE 'SI'.
  88 NO-ASSNEGO            VALUE 'NO'.
*
******************************************************************
*    SWITCH FUNZIONALI
******************************************************************
01  COMO-WS.
    03 COM-NR-RAPP-X.
      04 COM-NR-RAPP     PIC 9(12)      VALUE ZERO.
    03 WS-SOGLIA              PIC 9(15)V9(3) VALUE 3000.
    03 WS-QUALIFICATORI.
      04 WS-MIA-FILIALE       PIC X(2)  VALUE 'MF'.
      04 WS-MIA-FIL-SOSP      PIC X(2)  VALUE 'MS'.
      04 WS-MIA-BANCA-CIRC    PIC X(2)  VALUE 'MC'.
      04 WS-MIA-BANCA-NO-CIRC PIC X(2)  VALUE 'IS'.
    03 WS-PROCESSO.
*    VERSAMENTO ASSEGNI BLOCCATI
      04 WS-PROC-RIF          PIC X(4).
         88 SI-PROC-RIF          VALUE 'VAB '.
******************************************************************
*    INDICI
******************************************************************
01  IND.
    03 I-TBER-MAX      PIC S9(4)  COMP VALUE 20.
******************************************************************
*    COMMAREA PER CHIAMATA SARA
******************************************************************
01  BSCAASSE-01.
    COPY BSCAASSE.
******************************************************************
*    COMMAREA PER RECUPERO STATO DEL CONTO
******************************************************************
01  GECARAPP-01.
    COPY GECARAPP.
******************************************************************
*    COMMAREA PER CHIAMATA AL LOG URP
******************************************************************
01  DHCAGURP-01.
    COPY DHCAGURP.
******************************************************************
*    COMMAREA PER DECODIFICA ERRORI
******************************************************************
01  BSCAMSER-01.
    COPY BSCAMSER.
*-----------------------------------------------------------------
*    LINKAGE SECTION
******************************************************************
LINKAGE SECTION.
01  COMMAREA-STANDARD.
    COPY BSCAXALL.
01  DHCAMAIN-01.
    COPY DHCAMAIN.
    COPY DHCATBER.
01  DHCK1240-01.
    COPY DHCK1240.
01  AREE-RICHIAMI.
    02 FILLER                PIC X(128000).
01  DHCW0214-01 REDEFINES AREE-RICHIAMI.
    COPY DHCW0214.
01  DHCW0372-01 REDEFINES AREE-RICHIAMI.
    COPY DHCW0372.
01  DHCW0373-01 REDEFINES AREE-RICHIAMI.
    COPY DHCW0373.
01  DHCW0972-01 REDEFINES AREE-RICHIAMI.
    COPY DHCW0972.
PROCEDURE DIVISION USING COMMAREA-STANDARD
                         DHCAMAIN-01
                         DHCK1240-01
                         AREE-RICHIAMI.
    COPY BSCPXALL.
    .
*
******************************************************************
*    FUNZIONI GESTIONALI
******************************************************************
PROGRAM-INIZIO.
*
    INITIALIZE     COMODI
*
*    RICHIAMO IL PROGRAMMA DHGEGURP PER REGISTRARE IL LOG DI URP
*
    IF DHCAMAIN-FLG-URP           EQUAL 'S'
       MOVE 'INPUT'
         TO DHCAGURP-ID-AZIONE
       PERFORM CHIAMA-URP          THRU CHIAMA-URP-EX
    END-IF
*
    EVALUATE TRUE
        WHEN DHCK1240-CARSOSP
              PERFORM STEP-CARSOSP  THRU STEP-CARSOSP-EX
        WHEN DHCK1240-CONTRASS
              PERFORM STEP-CONTRASS  THRU STEP-CONTRASS-EX
        WHEN DHCK1240-FINE
              PERFORM STEP-FINE  THRU STEP-FINE-EX
        WHEN DHCK1240-INIZIO
              PERFORM STEP-INIZIO  THRU STEP-INIZIO-EX
        WHEN DHCK1240-INSPREN
              PERFORM STEP-INSPREN  THRU STEP-INSPREN-EX
        WHEN DHCK1240-NEGSARA
              PERFORM STEP-NEGSARA  THRU STEP-NEGSARA-EX
        WHEN OTHER
             MOVE  1          TO DHCK1240-E-COD
             MOVE DHCK1240-STEP       TO COM-VALORE
             PERFORM DECOD-ERRORE   THRU DECOD-ERRORE-EX
    END-EVALUATE.
*
*    RICHIAMO IL PROGRAMMA DHGEGURP PER REGISTRARE IL LOG DI URP
*
    IF DHCAMAIN-FLG-URP           EQUAL 'S'
       MOVE 'OUTPUT'
         TO DHCAGURP-ID-AZIONE
       PERFORM CHIAMA-URP          THRU CHIAMA-URP-EX
    END-IF
*
    .
*
PROGRAM-FINE.
    GOBACK.
******************************************************************
*    ISTRUZIONI NODO STEP-CARSOSP
*    DOPO RICHIAMO DEL SERVIZIO S0373CCD
*
*    POSSIBILI PERCORSI: STEP-NEGSARA
******************************************************************
STEP-CARSOSP.
*
*    SEGNALO CHE E' STATA CARICATA UNA PARTITA SOSPESA
*
    MOVE 'S'                  TO DHCK1240-ID-SOSP.
    IF  DHCK1240-TIP-VAL      EQUAL WS-MIA-FILIALE
        MOVE WS-MIA-FIL-SOSP  TO DHCK1240-TIP-VAL
    END-IF.
*
    MOVE BSCAAMBI-TERM              TO DHCK1240-I-CDTERMIN .
    MOVE 'S'                        TO DHCK1240-I-CDSTATAV .
    MOVE BSCAAMBI-ABI-ISTITUTO      TO DHCK1240-I-CDABINE  .
*
    SET DHCK1240-FINE-S             TO TRUE.
*
STEP-CARSOSP-EX.
    EXIT.
******************************************************************
*    ISTRUZIONI NODO STEP-CONTRASS
*    DOPO RICHIAMO DEL SERVIZIO S0372CCD
*
*    POSSIBILI PERCORSI: STEP-CARSOSP
*                        STEP-INSPREN
******************************************************************
STEP-CONTRASS.
*
120116     IF  DHCK1240-T-TIPO-FUNZ     EQUAL 'A'
.             IF  DHCK1240-TIP-VAL      EQUAL WS-MIA-FILIALE OR
.                                             WS-MIA-FIL-SOSP
.              IF ((DHCK1240-ST-ASSE    NOT EQUAL '01') AND
.                  (DHCK1240-PR-ASSE    NOT EQUAL '11'))
.                  MOVE 871             TO DHCK1240-E-COD
.                  PERFORM DECOD-ERRORE THRU DECOD-ERRORE-EX
.                  SET DHCK1240-FINE-S            TO TRUE
.                  GO TO STEP-CONTRASS-EX
.              END-IF
.             END-IF
120116     END-IF.
*
120116     IF  DHCK1240-T-TIPO-FUNZ     EQUAL 'A'
.             IF  DHCK1240-TIP-VAL      EQUAL WS-MIA-BANCA-CIRC OR
.                                             WS-MIA-BANCA-NO-CIRC
.              IF ((DHCK1240-ST-ASSE    NOT EQUAL '01') AND
.                  (DHCK1240-ID-PRENOT  NOT EQUAL 'S'))
.                  MOVE 871             TO DHCK1240-E-COD
.                  PERFORM DECOD-ERRORE THRU DECOD-ERRORE-EX
.                  SET DHCK1240-FINE-S            TO TRUE
.                  GO TO STEP-CONTRASS-EX
.              END-IF
.             END-IF
120116     END-IF.
*
050116* IN FASE DI VAB NON ACCETTO GLI ASSEGNI NON BLOCCATI
.     * PERCHE' TALI ASSEGNI HANNO UN COMPORTAMENTO DIVERSO CHE E'
.     * GESTITO DALLA FUNZIONE SPECIFICA (V = Versamento Assegni)
.     *
.          IF DHCK1240-ST-ASSE             NOT EQUAL '01'
.                MOVE 876                  TO DHCK1240-E-COD
.                PERFORM DECOD-ERRORE      THRU DECOD-ERRORE-EX
.                PERFORM COMPONI-ERRORE    THRU COMPONI-ERRORE-EX
.                SET DHCK1240-FINE-S       TO TRUE
.                GO TO STEP-CONTRASS-EX
050116     END-IF.
*
    PERFORM SCORRI-TBER         THRU SCORRI-TBER-EX.
*
    MOVE DHCK1240-DAT-VAL-LQ-P  TO DHCK1240-I-VTLIQUID.
    MOVE DHCK1240-DAT-VAL-DS-P  TO DHCK1240-I-VTDISP.
    MOVE DHCK1240-CAUS-ASSE     TO DHCK1240-CAUSALE-P
*
    MOVE DHCK1240-IMP-ASSE      TO DHCK1240-IMP-ASSE-SEG
    COMPUTE DHCK1240-IMP-ASSE-SEG =
            DHCK1240-IMP-ASSE-SEG * -1.
*
    MOVE DHCK1240-IMP-ASSE-SEG  TO DHCK1240-IMPORTO-P
*
    MOVE 'D'                    TO DHCK1240-SEGNO-P
    MOVE 'D'                    TO DHCK1240-SEGNO-S
    MOVE DHCK1240-DIVISA-ASSE   TO DHCK1240-DIVISA-P
    MOVE DHCK1240-TIP-RIF-ASSE  TO DHCK1240-TIP-RIF-P
    MOVE DHCK1240-ABI           TO DHCK1240-NR-RIF-P (1:5)
    MOVE DHCK1240-CAB           TO DHCK1240-NR-RIF-P (6:5)
    MOVE DHCK1240-NR-ASSE       TO DHCK1240-NR-RIF-P (11:)
    MOVE DHCK1240-DAT-EMI-ASSE  TO DHCK1240-DAT-RIF-P.
*
* LA NEGOZIAZIONE NON PUO' ESSERE FORZABILE MA L'OPGE5070
* VIENE SEMPRE CHIAMATO CON FORZATURA PER GESTIRE GLI ERRORI
* DI TIPO FORZABILE (SOLO IN NEGOZIAZIONE)
*
    MOVE 'SI'                      TO DHCK1240-ID-FORZ-P.
*
    IF   DHCK1240-T-TIPO-FUNZ      EQUAL 'A'
       IF   SI-NORMAL OR SI-SEGNALAZIONE
            PERFORM PULISCI-TBER   THRU PULISCI-TBER-EX
       END-IF
    END-IF.
*
    IF  DHCK1240-PR-ASSE           EQUAL '11'
*
    IF  DHCK1240-PR-ASSE           EQUAL '11'
      IF  DHCK1240-T-TIPO-FUNZ     EQUAL 'A'
          MOVE DHCK1240-T-RIF-ESTERNO
            TO DHCK1240-NR-MOVIM-SOSP
          MOVE WS-MIA-FIL-SOSP     TO DHCK1240-TIP-VAL
PROVA            PERFORM VAL-SOSP         THRU VAL-SOSP-EX
          SET DHCK1240-CARSOSP-S   TO TRUE
          GO TO STEP-CONTRASS-EX
        ELSE
        IF   DHCK1240-T-FLG-FORZATURA  EQUAL 'S' OR
             DHCK1240-T-TIPO-FUNZ      EQUAL 'F'
             MOVE DHCK1240-T-NR-OPER-FUNZ
             TO DHCK1240-NR-MOVIM-SOSP
             MOVE WS-MIA-FIL-SOSP     TO DHCK1240-TIP-VAL
PROVA            PERFORM VAL-SOSP         THRU VAL-SOSP-EX
             SET DHCK1240-CARSOSP-S     TO TRUE
             GO TO STEP-CONTRASS-EX
          ELSE
             MOVE 668                   TO DHCK1240-E-COD
             PERFORM DECOD-ERRORE       THRU DECOD-ERRORE-EX
             PERFORM COMPONI-ERRORE     THRU COMPONI-ERRORE-EX
             SET DHCK1240-FINE-S        TO TRUE
             GO TO STEP-CONTRASS-EX
        END-IF
      END-IF
    END-IF.
*
* SE IL QUALIFICATORE E' 'MC' O 'IS', SONO IN ANNULLO E IL
* DHCK1240-ID-PRENOT = 'S' VADO DIRETTAMENTE ALLO STEP PRENOTATA
*
    IF  DHCK1240-T-TIPO-FUNZ        EQUAL 'A'
      IF  DHCK1240-IMP-ASSE         GREATER WS-SOGLIA AND
         (DHCK1240-TIP-VAL          EQUAL   WS-MIA-BANCA-CIRC OR
          DHCK1240-TIP-VAL          EQUAL   WS-MIA-BANCA-NO-CIRC)
          MOVE WS-MIA-BANCA-NO-CIRC TO      DHCK1240-TIP-VAL
      END-IF
      IF  DHCK1240-TIP-VAL          EQUAL WS-MIA-BANCA-CIRC OR
                                          WS-MIA-BANCA-NO-CIRC
        IF (DHCK1240-ID-PRENOT      EQUAL 'S')
           MOVE WS-MIA-BANCA-NO-CIRC TO DHCK1240-TIP-VAL
           SET DHCK1240-INSPREN-S      TO TRUE
*
* LA NEGOZIAZIONE NON PUO' ESSERE FORZABILE MA L'OPGE5070
* VIENE SEMPRE CHIAMATO CON FORZATURA PER GESTIRE GLI ERRORI
* DI TIPO FORZABILE (SOLO IN NEGOZIAZIONE)
*
          MOVE 'SI'                  TO DHCK1240-ID-FORZ-P
          GO TO STEP-CONTRASS-EX
         ELSE
          PERFORM REC-STATOCON THRU REC-STATOCON-EX
          IF COM-STATO-CON     EQUAL '02'
             MOVE WS-MIA-BANCA-NO-CIRC TO DHCK1240-TIP-VAL
             SET DHCK1240-INSPREN-S      TO TRUE
             MOVE 'SI'                  TO DHCK1240-ID-FORZ-P
             GO TO STEP-CONTRASS-EX
          END-IF
        END-IF
      END-IF
    END-IF.
*
* SE IL QUALIFICATORE E' 'IS' GENERO UNA PRENOTATA
*
    IF  DHCK1240-TIP-VAL          EQUAL   WS-MIA-BANCA-NO-CIRC
        MOVE 'SI'                 TO DHCK1240-ID-FORZ-P
        SET DHCK1240-INSPREN-S    TO TRUE
        GO TO STEP-CONTRASS-EX
    END-IF.
*
* SE L'IMPORTO DELL'ASSEGNO E' OLTRE SOGLIA E STO CERCANDO DI
* ADDEBITARE UN ASSEGNO MIA BANCA IN CIRCOLARITA' FORZO IL
* QUALIFICATORE DI INSERIMENTO PRENOTATA E SALTO LA CONTABILITA'
*
    IF  DHCK1240-IMP-ASSE         GREATER WS-SOGLIA AND
       (DHCK1240-TIP-VAL          EQUAL   WS-MIA-BANCA-CIRC OR
        DHCK1240-TIP-VAL          EQUAL   WS-MIA-BANCA-NO-CIRC)
        MOVE WS-MIA-BANCA-NO-CIRC TO      DHCK1240-TIP-VAL
        SET DHCK1240-INSPREN-S    TO TRUE
        GO TO STEP-CONTRASS-EX
    END-IF.
*
    IF  DHCK1240-TIP-VAL          EQUAL WS-MIA-FILIALE
        MOVE WS-MIA-FIL-SOSP      TO DHCK1240-TIP-VAL
PROVA            PERFORM VAL-SOSP         THRU VAL-SOSP-EX
        SET DHCK1240-CARSOSP-S    TO TRUE
        GO TO STEP-CONTRASS-EX
    END-IF.
*
    IF  DHCK1240-TIP-VAL          EQUAL WS-MIA-BANCA-CIRC OR
        DHCK1240-TIP-VAL          EQUAL WS-MIA-BANCA-NO-CIRC
        MOVE WS-MIA-BANCA-NO-CIRC TO DHCK1240-TIP-VAL
        SET DHCK1240-INSPREN-S    TO TRUE
        GO TO STEP-CONTRASS-EX
    END-IF.
*
    IF   SI-NORMAL OR SI-SEGNALAZIONE OR SI-FORZABILE
         SET DHCK1240-NORMAL        TO TRUE
         GO TO STEP-CONTRASS-EX
    END-IF.
*
    IF SI-BLOCCANTE
       PERFORM SCORRI-ERR-WS    THRU SCORRI-ERR-WS-EX
       IF PRESENTE-NO
          PERFORM GESTISCI-ERRORE  THRU GESTISCI-ERRORE-EX
          SET DHCK1240-FINE-S    TO TRUE
          GO TO STEP-CONTRASS-EX
       END-IF
    END-IF.
*
    SET DHCK1240-FINE-S           TO TRUE.
*
STEP-CONTRASS-EX.
    EXIT.
******************************************************************
*    ISTRUZIONI NODO STEP-FINE
******************************************************************
STEP-FINE.
*
    IF COM-TRAN                 EQUAL SPACES OR LOW-VALUE
       GO TO STEP-FINE-EX
      ELSE
       MOVE COM-TRAN            TO BSCAAMBI-TRAN
    END-IF.
*
STEP-FINE-EX.
    EXIT.
******************************************************************
*    ISTRUZIONI NODO STEP-INIZIO
*
*    POSSIBILI PERCORSI: STEP-CONTRASS
******************************************************************
STEP-INIZIO.
*
    PERFORM PULISCI-TBER        THRU PULISCI-TBER-EX.
*
    IF BSCAAMBI-CANALE          EQUAL 'NSP'
       MOVE BSCAAMBI-TRAN       TO COM-TRAN
       MOVE DHCK1240-PROC-RIF   TO BSCAAMBI-TRAN
    END-IF.
*
*    E' AMMESSA SOLO LA TIPOLOGIA DI ASSEGNO AB = BANCARIO
*
    IF  DHCK1240-TIP-ASSE       NOT EQUAL 'AB'
.                MOVE 878                  TO DHCK1240-E-COD
.                PERFORM DECOD-ERRORE      THRU DECOD-ERRORE-EX
.                PERFORM COMPONI-ERRORE    THRU COMPONI-ERRORE-EX
          SET DHCK1240-FINE-S       TO TRUE
***       MOVE 'TIPO ASSEGNO NON VALIDO (AMMESSO AB)'
***         TO BSCAERRO-DES
***       MOVE DHCK1240-DATI-INPUT TO BSCAERRO-RIF-APPL
***       PERFORM ERRORE-GENER
***       SET DHCK1240-FINE-S     TO TRUE
    END-IF.
*
* VERIFICA DI CONGRUENZA CODICE ABI CON QUALIFICATORE
*
    IF (DHCK1240-ABI            EQUAL '05696' ) AND
       (DHCK1240-TIP-ASSE       EQUAL 'AB'    ) AND
       (DHCK1240-TIP-VAL        NOT EQUAL 'MF'  AND
                                          'MC'  AND
                                          'IS'  AND
                                          'MS')
        STRING 'QUALIFICATORE ' DELIMITED BY SIZE
             DHCK1240-TIP-VAL   DELIMITED BY SIZE
             ' INCOERENTE CON ABI '
                                DELIMITED BY SIZE
             DHCK1240-ABI       DELIMITED BY SIZE
             ' E TIPO ASSEGNO ' DELIMITED BY SIZE
             DHCK1240-TIP-ASSE  DELIMITED BY SIZE
                                INTO BSCAERRO-DES
        END-STRING
        MOVE DHCK1240-T-RIF-ESTERNO
                                TO BSCAERRO-RIF-APPL
        PERFORM ERRORE-GENER
    END-IF.
*
    IF (DHCK1240-ABI        NOT EQUAL '05696' ) AND
       (DHCK1240-TIP-ASSE       EQUAL 'AB'    ) AND
       (DHCK1240-TIP-VAL        EQUAL 'MF'  OR
                                      'MC'  OR
                                      'IS'  OR
                                      'MS')
        STRING 'QUALIFICATORE ' DELIMITED BY SIZE
             DHCK1240-TIP-VAL   DELIMITED BY SIZE
             ' INCOERENTE CON ABI '
                                DELIMITED BY SIZE
             DHCK1240-ABI       DELIMITED BY SIZE
             ' E TIPO ASSEGNO ' DELIMITED BY SIZE
             DHCK1240-TIP-ASSE  DELIMITED BY SIZE
                                INTO BSCAERRO-DES
        END-STRING
        MOVE DHCK1240-T-RIF-ESTERNO
                                TO BSCAERRO-RIF-APPL
        PERFORM ERRORE-GENER
    END-IF.
*
* RICHIAMO IL SERVIZIO BSGEASSE (SERVIZIO DI INDIRIZZAMENTO SARA)
* PER SAPERE SE DEVO AGGIORNARE SARA O ASSNEGO
*
    PERFORM CHIAMA-BSGEASSE     THRU CHIAMA-BSGEASSE-EX.
*
    IF   DHCK1240-ABI             NOT EQUAL COM-K-ABI
.                MOVE 877                  TO DHCK1240-E-COD
.                PERFORM DECOD-ERRORE      THRU DECOD-ERRORE-EX
.                PERFORM COMPONI-ERRORE    THRU COMPONI-ERRORE-EX
         SET DHCK1240-FINE-S      TO TRUE
         GO TO STEP-INIZIO-01
    END-IF.
*
    SET DHCK1240-CONTRASS-S       TO TRUE.
*
STEP-INIZIO-01.
*
    MOVE ZERO                     TO I-T.
*
* LA NEGOZIAZIONE NON PUO' ESSERE FORZABILE MA L'OPGE5070
* VIENE SEMPRE CHIAMATO CON FORZATURA PER GESTIRE GLI ERRORI
* DI TIPO FORZABILE (SOLO IN NEGOZIAZIONE)
*
    MOVE 'SI'                     TO DHCK1240-ID-FORZ-P.
*
*
    IF   DHCK1240-T-TIPO-FUNZ     EQUAL 'A'
         MOVE 'ANN'               TO DHCK1240-FUNZ
         MOVE 'ANN'               TO DHCK1240-TIPO-RICH-P
         MOVE '002'               TO DHCK1240-SCELTA-ASSNEGO
       ELSE
         MOVE 'AGG'               TO DHCK1240-FUNZ
         MOVE 'INS'               TO DHCK1240-TIPO-RICH-P
         MOVE '001'               TO DHCK1240-SCELTA-ASSNEGO
    END-IF.
*
    IF NOT DHCK1240-NORMAL
       PERFORM COMPONI-ERRORE   THRU COMPONI-ERRORE-EX
    END-IF.
*
STEP-INIZIO-EX.
    EXIT.
******************************************************************
*    ISTRUZIONI NODO STEP-INSPREN
*    DOPO RICHIAMO DEL SERVIZIO S0214CCD
*
*    POSSIBILI PERCORSI: STEP-NEGSARA
******************************************************************
STEP-INSPREN.
*
    MOVE ZERO                   TO I-P.
*
    IF NOT DHCK1240-NORMAL
       PERFORM COMPONI-ERRORE   THRU COMPONI-ERRORE-EX
    END-IF.
*
    IF DHCK1240-E-DES           EQUAL SPACES OR LOW-VALUE
       GO TO STEP-INSPREN-02
    END-IF.
*
STEP-INSPREN-00.
*
    INITIALIZE               COM-TBER
    MOVE DHCW0214-NR-CONTO   TO COM-NR-CONTO
    MOVE COM-NR-CONTO        TO COM-NR-CONTO-TBER
    MOVE DHCW0214-ENTE       TO COM-FIL-TBER
*
    STRING
       ' '                DELIMITED BY SIZE
       COM-FIL-TBER       DELIMITED BY SIZE
       ' '                DELIMITED BY SIZE
       COM-NR-CONTO-TBER  DELIMITED BY SIZE
                          INTO COM-CONTO-TBER
    END-STRING.
*
STEP-INSPREN-01.
*
    ADD 1                    TO I-P.
*
    IF  I-P                  GREATER I-P-MAX OR
       (DHCATBER-E-DES (I-P)     EQUAL SPACES OR LOW-VALUE)
        IF  DHCK1240-NORMAL
            GO TO STEP-INSPREN-02
          ELSE
            SET DHCK1240-FINE-S  TO TRUE
            GO TO STEP-INSPREN-EX
        END-IF
    END-IF.
*
    MOVE COM-CONTO-TBER
         TO DHCATBER-E-DES (I-P) (41:).
*
    GO TO STEP-INSPREN-01.
*
STEP-INSPREN-02.
*
    PERFORM SCORRI-TBER             THRU SCORRI-TBER-EX.
*
    IF  SI-BLOCCANTE
        CONTINUE
       ELSE
        PERFORM COMPONI-ERRORE   THRU COMPONI-ERRORE-EX
        PERFORM PULISCI-TBER     THRU PULISCI-TBER-EX
    END-IF.
*
    MOVE BSCAAMBI-TERM              TO DHCK1240-I-CDTERMIN .
    MOVE 'E'                        TO DHCK1240-I-CDSTATAV .
    MOVE BSCAAMBI-ABI-ISTITUTO      TO DHCK1240-I-CDABINE  .
*
    SET DHCK1240-FINE-S             TO TRUE.
*
STEP-INSPREN-EX.
    EXIT.
******************************************************************
*    ISTRUZIONI NODO STEP-NEGSARA
*    DOPO RICHIAMO DEL SERVIZIO S0972SAR
*
*    POSSIBILI PERCORSI: STEP-FINE
******************************************************************
STEP-NEGSARA.
*
*
STEP-NEGSARA-EX.
    EXIT.
******************************************************************
* CHIAMATA AL SERVIZIO BSGEASSE (SERVIZIO DI INDIRIZZAMENTO SARA)
* PER SAPERE SE DEVO AGGIORNARE SARA O ASSNEGO
******************************************************************
CHIAMA-BSGEASSE.
*
    COPY BSCPCALL REPLACING PROGRAMMA BY 'BSGEASSE'
                            PARM1     BY BSCAASSE-01.
    .
*
    IF BSCAASSE-SARA-ATT
       SET SI-SARA         TO TRUE
      ELSE
       SET NO-SARA         TO TRUE
    END-IF.
*
    IF BSCAASSE-CCAS-ATT
       SET SI-ASSNEGO      TO TRUE
      ELSE
       SET NO-ASSNEGO      TO TRUE
    END-IF.
*
CHIAMA-BSGEASSE-EX.
    EXIT.
******************************************************************
*    VALORIZZAZIONE SOSPESA
******************************************************************
VAL-SOSP.
*
    MOVE DHCK1240-T-RIF-ESTERNO TO DHCK1240-NR-MOVIM-SOSP.
    MOVE 'D'                    TO DHCK1240-SEGNO-S.
    MOVE WS-MIA-FIL-SOSP        TO DHCK1240-TIP-VAL.
*
    IF   DHCK1240-TAB-ERRO-IDX  EQUAL ZERO
         GO TO VAL-SOSP-EX
    END-IF.
*
    IF   DHCK1240-RIF-ASSE-ERRO (1) EQUAL SPACES OR LOW-VALUE
         GO TO VAL-SOSP-EX
       ELSE
         MOVE DHCK1240-COD-ERRO     (1)
           TO DHCK1240-ERRORE-1 (1:5)
         MOVE DHCK1240-APPLICAZIONE (1)
           TO DHCK1240-ERRORE-1 (6:2)
    END-IF.
*
    IF   DHCK1240-RIF-ASSE-ERRO (2) EQUAL SPACES OR LOW-VALUE
         GO TO VAL-SOSP-EX
       ELSE
         MOVE DHCK1240-COD-ERRO (2)
           TO DHCK1240-ERRORE-2
         MOVE DHCK1240-APPLICAZIONE (2)
           TO DHCK1240-ERRORE-2 (6:2)
    END-IF.
*
    IF   DHCK1240-RIF-ASSE-ERRO (3) EQUAL SPACES OR LOW-VALUE
         GO TO VAL-SOSP-EX
       ELSE
         MOVE DHCK1240-COD-ERRO (3)
           TO DHCK1240-ERRORE-3
         MOVE DHCK1240-APPLICAZIONE (3)
           TO DHCK1240-ERRORE-3 (6:2)
    END-IF.
*
VAL-SOSP-EX.
    EXIT.
******************************************************************
*    VERIFICO SE IN TABELLA ERRORI CI SONO ERRORI FORZABILI
******************************************************************
SCORRI-TBER.
*
    SET  SI-NORMAL                TO TRUE.
*
SCORRI-TBER-01.
*
    ADD  1                        TO I-T.
    IF   I-T                      GREATER I-T-MAX OR
        (DHCATBER-E-DES (I-T)     EQUAL SPACES OR LOW-VALUE)
         GO TO SCORRI-TBER-EX
    END-IF.
*
    IF   DHCATBER-E-FLG-BLOC (I-T)    EQUAL 'B'
         SET SI-BLOCCANTE             TO TRUE
         GO TO SCORRI-TBER-EX
    END-IF.
*
    IF   DHCATBER-E-FLG-BLOC (I-T)    EQUAL 'F'
         SET SI-FORZABILE             TO TRUE
         GO TO SCORRI-TBER-01
    END-IF.
*
    IF   DHCATBER-E-FLG-BLOC (I-T)    EQUAL 'S' AND
         NOT SI-FORZABILE
         SET SI-SEGNALAZIONE          TO TRUE
    END-IF.
*
    GO TO SCORRI-TBER-01.
*
SCORRI-TBER-EX.
    EXIT.
******************************************************************
*    PULIZIA DHCATBER
******************************************************************
PULISCI-TBER.
*
    MOVE 0                        TO I-TBER.
    MOVE 1                        TO DHCATBER-IND-ERR.
*
PULISCI-TBER-01.
*
    ADD  1                        TO I-TBER.
    IF   I-TBER                   GREATER I-T-MAX
         GO TO PULISCI-TBER-EX
    END-IF.
*
    INITIALIZE DHCATBER-E-APPL      (I-TBER).
    INITIALIZE DHCATBER-E-SERV      (I-TBER).
    INITIALIZE DHCATBER-E-COD       (I-TBER).
    INITIALIZE DHCATBER-E-VALORE    (I-TBER).
    INITIALIZE DHCATBER-E-FLG-BLOC  (I-TBER).
    INITIALIZE DHCATBER-E-TIPO      (I-TBER).
    INITIALIZE DHCATBER-E-DES       (I-TBER).
*
    GO TO PULISCI-TBER-01.
*
PULISCI-TBER-EX.
    EXIT.
******************************************************************
*    POPOLAMENTO DHCATBER PER ERRORE BLOCCANTE
******************************************************************
GESTISCI-ERRORE.
*
    MOVE DHCATBER-E-APPL     (1)  TO DHCK1240-E-APPL.
    MOVE DHCATBER-E-SERV     (1)  TO DHCK1240-E-SERV.
    MOVE DHCATBER-E-COD      (1)  TO DHCK1240-E-COD .
    MOVE DHCATBER-E-FLG-BLOC (1)  TO DHCK1240-E-BLOC.
    MOVE DHCATBER-E-TIPO     (1)  TO DHCK1240-E-TIPO.
    MOVE DHCATBER-E-DES      (1)  TO DHCK1240-E-DES .
*
GESTISCI-ERRORE-EX.
    EXIT.
******************************************************************
*    COMPOSIZIONE DESCRIZIONE DELL'ERRORE CON AGGIUNTA DATI
*    DEL RAPPORTO
******************************************************************
COMPONI-ERRORE.
*
    INITIALIZE               COM-ASSE.
*
COMPONI-ERRORE-01.
*
    ADD  1                   TO DHCK1240-TAB-ERRO-IDX.
    IF   DHCK1240-TAB-ERRO-IDX GREATER I-TBER-MAX
         SUBTRACT 1          FROM DHCK1240-TAB-ERRO-IDX
         GO TO COMPONI-ERRORE-EX
    END-IF.
*
    IF   DHCATBER-E-DES (DHCK1240-TAB-ERRO-IDX)
                  EQUAL SPACES OR LOW-VALUE
         SUBTRACT 1          FROM DHCK1240-TAB-ERRO-IDX
0108            GO TO COMPONI-ERRORE-02
0108  *         GO TO COMPONI-ERRORE-EX
    END-IF.
*
    MOVE DHCATBER-E-APPL       (DHCK1240-TAB-ERRO-IDX)
      TO DHCK1240-APPLICAZIONE (DHCK1240-TAB-ERRO-IDX).
    MOVE DHCATBER-E-SERV       (DHCK1240-TAB-ERRO-IDX)
      TO DHCK1240-SERV-ERRO    (DHCK1240-TAB-ERRO-IDX).
    MOVE DHCATBER-E-COD        (DHCK1240-TAB-ERRO-IDX)
      TO DHCK1240-COD-ERRO     (DHCK1240-TAB-ERRO-IDX).
    MOVE DHCATBER-E-FLG-BLOC   (DHCK1240-TAB-ERRO-IDX)
      TO DHCK1240-ID-GRAVITA   (DHCK1240-TAB-ERRO-IDX).
    MOVE DHCATBER-E-DES        (DHCK1240-TAB-ERRO-IDX)
      TO DHCK1240-DES-ERRO     (DHCK1240-TAB-ERRO-IDX).
*
    MOVE DHCK1240-ABI             TO COM-ABI.
    MOVE DHCK1240-CAB             TO COM-CAB.
    MOVE DHCK1240-NR-ASSE         TO COM-NR-ASSEGNO.
    MOVE COM-NR-RIF
         TO DHCK1240-RIF-ASSE-ERRO (DHCK1240-TAB-ERRO-IDX).
*
    GO TO COMPONI-ERRORE-01.
*
COMPONI-ERRORE-02.
*
    IF   DHCK1240-NORMAL
         GO TO COMPONI-ERRORE-EX
    END-IF.
*
* SE LA TBER NON E' VALORIZZATA MA SONO IN ERRORE SIGNIFICA
* CHE L'ERRORE RESTITUITO E' ESTERNO E PERTANTO DEVO BASARMI
* SULL'ERRORE RESTITUITO DALLA DHCK1240
*
    ADD  1                     TO DHCK1240-TAB-ERRO-IDX.
*
??         IF   DHCK1240-TAB-ERRO-IDX GREATER 1
.               SUBTRACT 1            FROM DHCK1240-TAB-ERRO-IDX
.               GO TO COMPONI-ERRORE-EX
??         END-IF.
*
    IF   DHCATBER-E-DES (DHCK1240-TAB-ERRO-IDX)
         NOT EQUAL SPACES AND LOW-VALUE
         GO TO COMPONI-ERRORE-EX
    END-IF
*
    MOVE DHCK1240-E-APPL
      TO DHCK1240-APPLICAZIONE (DHCK1240-TAB-ERRO-IDX).
    MOVE DHCK1240-E-SERV
      TO DHCK1240-SERV-ERRO    (DHCK1240-TAB-ERRO-IDX).
    MOVE DHCK1240-E-COD
      TO DHCK1240-COD-ERRO     (DHCK1240-TAB-ERRO-IDX)
    MOVE DHCK1240-E-BLOC
      TO DHCK1240-ID-GRAVITA   (DHCK1240-TAB-ERRO-IDX).
    MOVE DHCK1240-E-DES
      TO DHCK1240-DES-ERRO     (DHCK1240-TAB-ERRO-IDX).
*
    MOVE DHCK1240-ABI             TO COM-ABI.
    MOVE DHCK1240-CAB             TO COM-CAB.
    MOVE DHCK1240-NR-ASSE         TO COM-NR-ASSEGNO.
    MOVE COM-NR-RIF
         TO DHCK1240-RIF-ASSE-ERRO (DHCK1240-TAB-ERRO-IDX).
*
COMPONI-ERRORE-EX.
    EXIT.
******************************************************************
*    VERIFICO SE GLI ERRORI GENERATI DAL PROCESSO SONO PRESENTI
*    NEL TABELLINO ERRORI DI WORKING. SE PRESENTI MI COMPORTO
*    COME SE L'ERRORE FOSSE FORZABILE, ALTRIMENTI VADO A FINE
******************************************************************
*    ROUTINE COMMENTATA PERCHE' NON GESTIAMO PER ORA LA FORZATURA
*    DI ERRORI BLOCCANTI...IN NESSUN CASO
SCORRI-ERR-WS.
*
    SET  PRESENTE-NO              TO TRUE.
*    MOVE ZERO                     TO I-WS.
*    SET  PRESENTE-NO              TO TRUE.
*
*SCORRI-ERR-WS-01.
*
*    ADD  1                        TO I-WS.
*    IF   I-WS                     GREATER I-WS-MAX OR
*        (DHCATBER-E-DES (I-WS)    EQUAL SPACES OR LOW-VALUE)
*         GO TO SCORRI-ERR-WS-EX
*    END-IF.
*
*    IF   DHCATBER-E-FLG-BLOC (I-WS) NOT EQUAL 'B'
*         GO TO SCORRI-ERR-WS-01
*    END-IF.
*
*    MOVE DHCATBER-E-COD (I-WS)    TO COM-ERR-WS.
*    MOVE DHCATBER-E-APPL(I-WS)    TO COM-APPL-WS.
*
*    IF   COM-WS                   EQUAL COM-ERR-901
*                                     OR COM-ERR-902
*                                     OR COM-ERR-904
*                                     OR COM-ERR-905
*                                     OR COM-ERR-906
*                                     OR COM-ERR-909
*                                     OR COM-ERR-910
*                                     OR COM-ERR-911
*                                     OR COM-ERR-913
*                                     OR COM-ERR-915
*         SET PRESENTE-SI             TO TRUE
*         GO TO SCORRI-ERR-WS-01
*      ELSE
*         SET PRESENTE-NO             TO TRUE
*         GO TO SCORRI-ERR-WS-EX
*    END-IF.
*
SCORRI-ERR-WS-EX.
    EXIT.
******************************************************************
*    RECUPERO STATO DEL CONTO
******************************************************************
REC-STATOCON.
*
    INITIALIZE                        GECARAPP-01.
    MOVE DHCK1240-T-ISTITUTO       TO GECARAPP-ISTITUTO.
    MOVE DHCK1240-SERVIZIO         TO GECARAPP-SERVIZIO.
    MOVE DHCK1240-ENTE             TO GECARAPP-ENTE.
    MOVE DHCK1240-NR-CONTO-Y       TO GECARAPP-NR-CONTO.
*
    COPY BSCPCALL REPLACING PROGRAMMA  BY 'GEGERAPP'
                            PARM1      BY GECARAPP-01.
*
    IF GECARAPP-NOT-FOUND
       MOVE 295                    TO DHCK1240-E-COD
       PERFORM DECOD-ERRORE        THRU DECOD-ERRORE-EX
       GO TO REC-STATOCON-EX
    END-IF.
*
    MOVE GECARAPP-STATO-CON        TO COM-STATO-CON.
*
REC-STATOCON-EX.
    EXIT.
******************************************************************
*    RICHIAMO AL GESTORE DEL LOG DI URP
******************************************************************
CHIAMA-URP.
*
    MOVE 'P1240CCD'
      TO DHCAGURP-ID-PROCESSO
    MOVE DHCAMAIN-COD-PROCESSO
      TO DHCAGURP-COD-PROCESSO
    MOVE DHCAMAIN-NR-OPERAZ
      TO DHCAGURP-NR-OPERAZ
    MOVE 'N'
      TO DHCAGURP-TIP-ENT
    MOVE DHCAMAIN-ISTITUTO
      TO DHCAGURP-ISTITUTO
    MOVE DHCAMAIN-STEP
      TO DHCAGURP-ID-NODO
    MOVE BSCAAMBI-DEF-CONTESTO
      TO DHCAGURP-CONTESTO
*
    MOVE 'DHCK1240'
      TO DHCAGURP-NOME-COPY
    MOVE DHCK1240-01
      TO DHCAGURP-AREA-LOG
    MOVE LENGTH OF DHCK1240-01
      TO DHCAGURP-LEN-AREA-LOG
*
    IF DHCAGURP-ID-AZIONE        EQUAL 'INPUT'
       ADD 1                     TO DHCAMAIN-LIVELLO
       MOVE DHCAMAIN-LIVELLO     TO DHCAGURP-LIVELLO
     ELSE
       ADD -1                    TO DHCAMAIN-LIVELLO
       MOVE DHCAMAIN-LIVELLO     TO DHCAGURP-LIVELLO
    END-IF
*
    SET DHCAGURP-INSERT          TO TRUE
*
    COPY BSCPCALL REPLACING PROGRAMMA BY 'DHGEGURP'
                            PARM1     BY DHCAGURP-01.
    .
*
CHIAMA-URP-EX.
    EXIT.
******************************************************************
*    DECODIFICA CODICE ERRORE APPLICATIVO
******************************************************************
DECOD-ERRORE.
*
    INITIALIZE BSCAMSER-01
    MOVE DHCAMAIN-ISTITUTO
      TO BSCAMSER-ISTITUTO
    MOVE COM-VALORE
      TO BSCAMSER-MESS-ERRORE
    SET  BSCAMSER-APPLICAZIONE-DH TO TRUE
*
    COPY BSCPMSER REPLACING I-CODERR BY DHCK1240-E-COD
         I-APPL          BY BSCAMSER-APPLICAZIONE-DH
         O-RETC          BY DHCK1240-NOT-NORMAL
         O-MESS          BY DHCK1240-E-DES
         SI-GOBACK       BY NO-GOBACK.
*
    .
*
DECOD-ERRORE-EX.
    EXIT.
*-----------------------------------------------------------------
FINEFINE.
    GOBACK.

