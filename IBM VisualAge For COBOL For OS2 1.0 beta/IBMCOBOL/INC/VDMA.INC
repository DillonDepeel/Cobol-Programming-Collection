;       SCCSID = @(#)vdma.inc	6.1 92/03/25
;***    VDMA.INC
;
;
;       MVDM Virtual DMA Exported Definitions
;
;       For include file hierarchy, see MVDM.INC

;       Copyright (c) IBM Corporation 1987, 1992
;
;       All Rights Reserved
;
;

; VDMA defines

VDD_DMA_MASKOFF         EQU     0       ;Mask off notification to VDD
VDD_DMA_MASK            EQU     1       ;Mask on  notification to VDD

DefType PFNDMA,dd

; VDMA function prototypes

VDHENTRY VDHRegisterDMAChannel          <ULONG,PFNDMA>
VDHENTRY VDHCallOutDMA          <VOID>
