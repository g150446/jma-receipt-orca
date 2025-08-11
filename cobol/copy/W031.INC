        02  W031.
            03  W031-TITLE  PIC X(256).
            03  W031-POPUP-SUMMARY  PIC X(256).
            03  W031-POPUP-BODY PIC X(256).
            03  W031-POPUP-ICON PIC X(256).
            03  W031-POPUP-TIMEOUT  PIC S9(9)   BINARY.
      ****  02  W031-FIXED10.
              03  W031-B01.
                04  W031-STATE  PIC S9(9)   BINARY.
              03  W031-NEN.
                04  W031-TEXTDATA   PIC X(10).
              03  W031-B02.
                04  W031-STATE  PIC S9(9)   BINARY.
              03  W031-B06.
                04  W031-STATE  PIC S9(9)   BINARY.
              03  W031-B12.
                04  W031-STATE  PIC S9(9)   BINARY.
              03  W031-B07.
                04  W031-STATE  PIC S9(9)   BINARY.
              03  W031-MSG01.
                04  W031-TEXTDATA   PIC X(20).
              03  W031-RITU-G.
                04  W031-RITU        PIC S9(3)
                                        OCCURS   12.
