       01  CommandLine-Data.
           05  CommandLine-Length      PIC 9(4) USAGE COMP.
           05  CommandLine-String      PIC X(255).
       01  VDE-Complete-Data.
           05  Complete-Status         PIC 9(9) USAGE COMP.
               88  Complete-UnSuccessful VALUE 0.
               88  Complete-Successful   VALUE 1.
       01  VDE-Drop-Data.
           05  Drop-Data-Length        PIC 9(9) USAGE COMP.
           05  Drop-Data-String.
               10 Drop-Data-Chars      PIC X(255).
       01  VDE-Key-Pressed-Data.
           05  Character-Key           PIC X.
       01  VDE-Mouse-Data.
           05  Mouse-Button-Pressed    PIC 9(9) USAGE COMP.
               88  Mouse-NoButton      VALUE 0.
               88  Mouse-Button1       VALUE 1.
               88  Mouse-Button2       VALUE 2.
               88  Mouse-Button3       VALUE 3.
               88  Mouse-Button1and2   VALUE 4.
           05  Mouse-Position-X        PIC 9(9) USAGE COMP.
           05  Mouse-Position-Y        PIC 9(9) USAGE COMP.
       01  VDE-PopUp-Data.
           05  PopUp-Position-X        PIC 9(9) USAGE COMP.
           05  PopUp-Position-Y        PIC 9(9) USAGE COMP.
           05  PopUp-Index             PIC 9(9) USAGE COMP.
       01  VDE-Resize-Data.
           05  Resize-Height           PIC 9(9) USAGE COMP.
           05  Resize-Width            PIC 9(9) USAGE COMP.
       01  VDE-Select-Page-Data.
           05  Select-PageNumber       PIC 9(9) USAGE COMP.
       01  VDE-VKey-Pressed-Data.
           05  VCharacter-Key          PIC 9(9) USAGE COMP.
               88  VKey-Escape         VALUE 0.
               88  VKey-Tab            VALUE 1.
               88  VKey-Backtab        VALUE 2.
               88  VKey-Space          VALUE 3.
               88  VKey-Backspace      VALUE 4.
               88  VKey-Enter          VALUE 5.
               88  VKey-Newline        VALUE 6.
               88  VKey-Shift          VALUE 7.
               88  VKey-Control        VALUE 8.
               88  VKey-Alt            VALUE 9.
               88  VKey-Insert         VALUE 10.
               88  VKey-Delete         VALUE 11.
               88  VKey-Home           VALUE 12.
               88  VKey-End            VALUE 13.
               88  VKey-Pageup         VALUE 14.
               88  VKey-Pagedown       VALUE 15.
               88  VKey-Left           VALUE 16.
               88  VKey-Right          VALUE 17.
               88  VKey-Up             VALUE 18.
               88  VKey-Down           VALUE 19.
               88  VKey-Caps-Lock      VALUE 20.
               88  VKey-Num-Lock       VALUE 21.
               88  VKey-Scroll-Lock    VALUE 22.
               88  VKey-Pause          VALUE 23.
               88  VKey-SysReq         VALUE 24.
               88  VKey-Break          VALUE 25.
               88  VKey-F2             VALUE 26.
               88  VKey-F3             VALUE 27.
               88  VKey-F4             VALUE 28.
               88  VKey-F5             VALUE 29.
               88  VKey-F6             VALUE 30.
               88  VKey-F7             VALUE 31.
               88  VKey-F8             VALUE 32.
               88  VKey-F9             VALUE 33.
               88  VKey-F11            VALUE 34.
               88  VKey-F12            VALUE 35.
               88  VKey-F13            VALUE 36.
               88  VKey-F14            VALUE 37.
               88  VKey-F15            VALUE 38.
               88  VKey-F16            VALUE 39.
               88  VKey-F17            VALUE 40.
               88  VKey-F18            VALUE 41.
               88  VKey-F19            VALUE 42.
               88  VKey-F20            VALUE 43.
               88  VKey-F21            VALUE 44.
               88  VKey-F22            VALUE 45.
               88  VKey-F23            VALUE 46.
               88  VKey-F24            VALUE 47.
       01  DDE-Execute-Data.
           05  Execute-Status          PIC 9(9) USAGE COMP.
               88  Execute-UnSuccessful  VALUE 0.
               88  Execute-Successful    VALUE 1.
           05  Execute-Data-Length     PIC 9(9) USAGE COMP.
           05  Execute-Data-String.
               10 Execute-Data-Chars   PIC X(255).
       01  DDE-Poke-Data.
           05  Poke-Status             PIC 9(9) USAGE COMP.
               88  Poke-UnSuccessful   VALUE 0.
               88  Poke-Successful     VALUE 1.
           05  Poke-Data-Length        PIC 9(9) USAGE COMP.
           05  Poke-Item-Length        PIC 9(9) USAGE COMP.
           05  Poke-Data-String.
               10 Poke-Data-Chars      PIC X(255).
           05  Poke-Item-String.
               10 Poke-Item-Chars      PIC X(255).
       01  DDE-Data-Data.
           05  Data-Data-Length        PIC 9(9) USAGE COMP.
           05  Data-Item-Length        PIC 9(9) USAGE COMP.
           05  Data-Data-String.
               10 Data-Data-Chars      PIC X(255).
           05  Data-Item-String.
               10 Data-Item-Chars      PIC X(255).
