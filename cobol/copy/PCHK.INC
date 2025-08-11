        02  PCHK.
            03  PCHK-TITLE  PIC X(256).
            03  PCHK-POPUP-SUMMARY  PIC X(256).
            03  PCHK-POPUP-BODY PIC X(256).
            03  PCHK-POPUP-ICON PIC X(256).
            03  PCHK-POPUP-TIMEOUT  PIC S9(9)   BINARY.
      ******02  PCHK-FIXED1.
              03  PCHK-B01.
                04  PCHK-STATE  PIC S9(9)   BINARY.
              03  PCHK-CHKMSG.
                04  PCHK-TEXTDATA   PIC X(100).
