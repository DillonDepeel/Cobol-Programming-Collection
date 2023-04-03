           88  VDE-RC-OK                          VALUE  0.
           88  VDE-INSUFFICIENT-RESOURCES         VALUE  4.
           88  VDE-INVALID-HANDLE                 VALUE  8.
           88  VDE-INVALID-OBJECT                 VALUE  9.
           88  VDE-INVALID-COMPONENT              VALUE 10.
           88  VDE-INVALID-PARENT                 VALUE 11.
           88  VDE-INVALID-MESSAGE                VALUE 17.
           88  VDE-INVALID-LIBRARY                VALUE 18.
           88  VDE-INVALID-FUNCTION               VALUE 19.
           88  VDE-OBJECT-ALREADY-OPENED          VALUE 21.
           88  VDE-OBJECT-NOT-FOUND               VALUE 22.
           88  VDE-COMPONENT-ALREADY-OPENED       VALUE 23.
           88  VDE-COMPONENT-NOT-FOUND            VALUE 24.
           88  VDE-INVALID-PARENT                 VALUE 29.
           88  VDE-INVALID-TEXT                   VALUE 30.
           88  VDE-LANGUAGE-DLL-NOT-FOUND         VALUE 31.
           88  VDE-LANG-DLL-VERSION-MISMATCH      VALUE 34.
           88  VDE-MISSING-PART-REG-FILE          VALUE 41.
           88  VDE-INVALID-PART-TYPE              VALUE 42.
           88  VDE-MISSING-ODXFILE                VALUE 44.
           88  VDE-TERMINATE-REQUEST-REJECTED     VALUE 45.
           88  VDE-POPUPMENU-EMPTY                VALUE 46.
           88  VDE-GENERAL-FAILURE                VALUE 98.
           88  VDE-INTERNAL-ERROR                 VALUE 1, 2, 3, 5,
                                                  6, 7, 12, 13, 14,
                                                  15, 16, 20, 25, 26,
                                                  27, 28, 32, 33, 35,
                                                  36, 37, 38, 39, 40,
                                                  43, 99.
       01  VDE-ACTION-RC-VALUES.
           05  VDE-DEFAULT-PROCESSING      PIC 9(9) COMP VALUE 0.
           05  VDE-NO-DEFAULT-PROCESSING   PIC 9(9) COMP VALUE 1.
           05  VDE-TERMINATE-COMPONENT     PIC 9(9) COMP VALUE 2.
           05  VDE-TERMINATE-APPLICATION   PIC 9(9) COMP VALUE 3.
