        02  K025.
            03  K02N-POPUP-STATUS.
              04  K025-POPUP-SUMMARY  PIC X(256).
              04  K025-POPUP-BODY PIC X(256).
              04  K025-POPUP-ICON PIC X(256).
              04  K025-POPUP-TIMEOUT  PIC S9(9)   BINARY.
      **    02  K025-FIXED10.
              03  K025-B01.
                04  K025-STATE  PIC S9(9)   BINARY.
              03  K025-B12.
                04  K025-STATE  PIC S9(9)   BINARY.
              03  K025-B06.
                04  K025-STATE  PIC S9(9)   BINARY.
              03  K025-B07.
                04  K025-STATE  PIC S9(9)   BINARY.
              03  K025-MIDASHI1.
                04  K025-TEXTDATA   PIC X(100).
              03  K025-SRYCD.
                04  K025-TEXTDATA   PIC X(9).
              03  K025-MIDASHI2.
                04  K025-TEXTDATA   PIC X(30).
              03  K025-SCROLLEDWINDOW2.
                04  K025-FORMALNAME.
                  05  K025-TEXTDATA   PIC X(400).
              03  K025-INPUTSTRING.
                04  K025-TEXTDATA   PIC X(100).
              03  K025-NAMELABEL.
                04  K025-TEXTDATA   PIC X(30).
              03  K025-MIDASHI3-G.
                04  K025-MIDASHI3-STYLE PIC X(20).
                04  K025-MIDASHI3   PIC X(30).
              03  K025-MIDASHI4-G.
                04  K025-MIDASHI4-STYLE PIC X(20).
                04  K025-MIDASHI4   PIC X(30).
