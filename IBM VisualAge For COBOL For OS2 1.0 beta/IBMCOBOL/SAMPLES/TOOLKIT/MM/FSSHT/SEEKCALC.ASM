        TITLE   D:\me10\msrc\COMMON\seekcalc.c
        .386
        .387
        INCLUDELIB OS2386.LIB
        INCLUDELIB dde4mbs.lib
        INCLUDELIB dde4mbm.lib
CODE32  SEGMENT DWORD USE32 PUBLIC 'CODE'
CODE32  ENDS
DATA32  SEGMENT DWORD USE32 PUBLIC 'DATA'
DATA32  ENDS
CONST32 SEGMENT DWORD USE32 PUBLIC 'CONST'
CONST32 ENDS
BSS32   SEGMENT DWORD USE32 PUBLIC 'BSS'
BSS32   ENDS
DGROUP  GROUP CONST32, BSS32, DATA32
        ASSUME  CS:FLAT, DS:FLAT, SS:FLAT, ES:FLAT
        EXTRN   _DosFlatToSel:PROC
        EXTRN   _DosSelToFlat:PROC
        EXTRN   _dllentry:PROC
DATA32  SEGMENT
        DD      _dllentry
DATA32  ENDS
BSS32   SEGMENT
BSS32   ENDS
CONST32 SEGMENT
CONST32 ENDS
CODE32  SEGMENT

;************************************
;  Mmtime2Bytes coversion routine
;  Function - Converts an mmtime
;             value to byte value.
;************************************

        ALIGN 04H
        PUBLIC Mmtime2Bytes
Mmtime2Bytes    PROC
        PUSH    EBP
        MOV     EBP,ESP

        MOV     EAX,[EBP+0cH];  lSeekPoint
        CDQ
        IDIV    DWORD PTR [EBP+010H];   mmtimePerUnit
        MOV     ECX,[EBP+08H];  ulBytesPerUnit
        IMUL    ECX,EAX
        MOV     EAX,[EBP+014H]; lByteSeekPoint
        MOV     [EAX],ECX

   JNO   NOOVERFLOW
        MOV     EAX,01H
        JMP     FELB1

NOOVERFLOW:
        MOV     EAX,00H
        JMP     FELB1
        ALIGN 04H

FELB1:
        LEAVE
        RET
Mmtime2Bytes    ENDP

;************************************
;  Bytes2Mmtime coversion routine
;  Function - Converts a byte value
;             to an mmtime value.
;************************************

        ALIGN 04H

        PUBLIC Bytes2Mmtime
Bytes2Mmtime    PROC
        PUSH    EBP
        MOV     EBP,ESP

        MOV     ECX,[EBP+08H];  ulBytesPerUnit
        MOV     EAX,[EBP+0cH];  lSeekPoint
        CDQ
        IDIV    ECX
        MOV     ECX,EAX
        IMUL    ECX,[EBP+010H]; mmtimePerUnit
        MOV     EAX,[EBP+014H]; lByteSeekPoint
        MOV     [EAX],ECX

   JNO   NOTOVERFLOW
        MOV     EAX,01H
        JMP     FELB2

NOTOVERFLOW:
        MOV     EAX,00H
        JMP     FELB2
        ALIGN 04H

FELB2:
        LEAVE
        RET
Bytes2Mmtime    ENDP

;************************************
;  AddNCheck coversion routine
;  Function - Add two values together
;             and check for an overflow
;************************************

        ALIGN 04H

        PUBLIC AddNCheck
AddNCheck       PROC
        PUSH    EBP
        MOV     EBP,ESP

        MOV     ECX,[EBP+0cH];  lValue2
        ADD     ECX,[EBP+08H];  lValue1
        MOV     EAX,[EBP+010H]; lResult
        MOV     [EAX],ECX

   JNO  NOOVERFLOW2
        MOV     EAX,01H
        JMP     FELB3

NOOVERFLOW2:
        MOV     EAX,00H
        JMP     FELB3
        ALIGN 04H

FELB3:
        LEAVE
        RET
AddNCheck       ENDP
CODE32  ENDS
END
