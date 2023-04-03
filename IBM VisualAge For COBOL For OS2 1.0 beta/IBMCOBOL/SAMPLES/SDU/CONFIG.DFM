; ****************************************************************************
; *      Copyright (c) IBM Corporation 1993,1995 - All Rights Reserved       *
; *                                                                          *
; * DFM/2 (program no. 5648-02012)                                           *
; * Version: 1.0                                                             *
; * Release: 1.0                                                             *
; *                                                                          *
; ****************************************************************************
; *                                                                          *
; *             IBM Distributed FileManager (DFM/2) for OS/2                 *
; *             ============================================                 *
; *                                                                          *
; *                     SAMPLE CONFIGURATION FILE                            *
; *                                                                          *
; *                                                                          *
; ****************************************************************************
; * CHANGE ACTIVITY                                                          *
; * ------------------------------------------------------------------------ *
; *                           930402 Mueller  : Initial Release              *
; *                           950803 W.Murphy : Release 1.011                *
; ****************************************************************************
; ****************************************************************************
; * Delete or comment-out the lines not needed (i.e. add a ';' in front).    *
; * Un-comment lines you want activated (i.e. delete the ';' in front).      *
; * Modify the values as required.                                           *
; ****************************************************************************

; ****************************************************************************
; * Define all DFM/2 target systems on which you want to access file data.   *
; ****************************************************************************
; The name specified in the 'remote_lu' parameter of the DFM_TARGET statement
; connects the definition to the 'partner_lu_alias' specification of a
; DEFINE_PARTNER_LU statement in the node definitions file for the
; OS/2 Communications Manager (the configuration.NDF file normally
; residing in c:\cmlib\appn).

; Definition of an AS/400 system as target:
;      DFM_TARGET (
;            remote_lu(OS400M1)
;            description(AS/400 System M1)
;            conversation(HOLD)
;            max_send_limit(32767)
;            )

; Definition of an MVS system as target:
;      DFM_TARGET (
;            remote_lu(MVS)
;            description(MVS System)
;            conversation(HOLD)
;            max_send_limit(4096)
;            )

; Definition of a CICS system as target:
;      DFM_TARGET (
;            remote_lu(DDMCICS1)
;            description(CICS/DDM MVS System DDMCICS1)
;            conversation(HOLD)
;            max_send_limit(4096)
;            )

; Definition of a 4680 Store System as target:
;      DFM_TARGET (
;            remote_lu(OS4680)
;            description(4680 Store System)
;            conversation(HOLD)
;            max_send_limit(4096)
;            )

; ****************************************************************************
; * Define the local LU alias as defined for the OS/2 Communications Mgr.    *
; ****************************************************************************
       LOCAL_LU (APPCLU)

; ****************************************************************************
; * Define the Mode Name                                                     *
; ****************************************************************************
       MODE_NAME (QPCSUPP)

; ****************************************************************************
; * Define the Default DFM Target System                                     *
; ****************************************************************************
;       DEFAULT_DFM_TARGET(OS400M1)

; ****************************************************************************
; * Define the default CCSID (Coded-Character-Set-Id, up to 5 decimal digits)*
; * for all accessed targets:                                                *
; * Some examples for CCSIDs (code pages)                                    *
; * (CEPC means EBCDIC format, PC Data means ASCII format):                  *
; * 037 = CEPC: USA, Canada (ESA), Netherlands, Portugal, Brazil, Australia  *
; * 273 = CEPC: Austria, Germany                                             *
; * 277 = CEPC: Denmark, Norway                                              *
; * 278 = CEPC: Finland, Sweden                                              *
; * 280 = CEPC: Italy                                                        *
; * 284 = CEPC: Spain, Latin America (Spanish)                               *
; * 285 = CEPC: United Kingdom                                               *
; * 297 = CEPC: France                                                       *
; * 500 = CEPC: International Latin-1, B, CH, Can(AS/400)                    *
; *             (default if no specification given)                          *
; * 437 = PC Data: USA                                                       *
; * 850 = PC Data: Latin Alphabet Number 1, Latin-1 Countries                *
; ****************************************************************************
       DEFAULT_CCSID(00500)

; ****************************************************************************
; * For each file where data conversion is required insert a                 *
; * FILE_DESCRIPTOR_MAP statement.                                           *
; ****************************************************************************
;       FILE_DESCRIPTOR_MAP(
;              REMOTE_LU(OS400M1)
;              TARGET_FILENAME(SAMPLE)
;              BASE_DDF(\SAMPLE\SAMPBASE.DDF)
;              VIEW_DDF(\SAMPLE\SAMPVIEW.DDF)
;              )

; ****************************************************************************
; * Define the shared memory size to be available for DFM/2 tracing.         *
; * Trace buffer memory to be specified in kilobytes:                        *
; *  Minimum value:   10 KB                                                  *
; *  Maximum value: 1000 KB                                                  *
; ****************************************************************************
;       TRACE_BUFFER(64)

; ****************************************************************************
; * Define space available for DFM/2 caching.                                *
; * The size (maximum space used) is specified in kilobytes,                 *
; * default value is 2000 KB.                                                *
; * The default directory is d:\cache                                        *
; * Make sure the directory specified exists and does not contain any files  *
; * you want to keep, as DFM/2 may delete all files in that directory.       *
; ****************************************************************************
;       DFM_CACHE (
;             cache_disk_size(1000)
;             cache_directory(d:\dfmcache)
;             )

; ***** end of file **********************************************************
