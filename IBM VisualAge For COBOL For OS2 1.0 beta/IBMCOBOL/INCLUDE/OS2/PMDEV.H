/****************************** Module Header ******************************\
*
* Module Name: PMDEV.H
*
* OS/2 Presentation Manager Device Context constants, types and
* function declarations
*
* Copyright (c) International Business Machines Corporation 1981, 1988-1992
*
* ===========================================================================
*
* The following symbols are used in this file for conditional sections.
*
*   #define:                To include:
*
*   INCL_DEVERRORS          error definitions
*   INCL_DEV                Dev APIs and structures
*
* There is a symbol used in this file called INCL_DDIDEFS. This is used to
* include only the definitions for the DDI. The programmer using the GPI
* can ignore this symbol
*
* There is a symbol used in this file called INCL_SAADEFS. This is used to
* include only the definitions for the SAA. The programmer using the GPI
* can ignore this symbol
*
* ===========================================================================
*
* Comments at the end of each typedef line give the name tags used in
* the assembler include version of this file.
*
* The assembler include version of this file excludes lines between NOINC
* and INC comments.
*
\***************************************************************************/

#if __IBMC__ || __IBMCPP__
   #pragma info( none )
   #ifndef __CHKHDR__
      #pragma info( none )
   #endif
   #pragma info( restore )
#endif
#ifdef __cplusplus
      extern "C" {
#endif

#define DEV_INCLUDED


#ifdef INCL_ERRORS /* if errors are required then allow DEV errors */
   #define INCL_DEVERRORS
#endif /* INCL_ERRORS */

#ifdef INCL_DDIDEFS /* if only DDI required then enable all of DEV */
   #define INCL_DEV
#endif /* INCL_DDIDEFS */

#ifdef INCL_SAADEFS /* if only SAA required then enable all of DEV */
   #define INCL_DEV
#endif /* INCL_SAADEFS */

#if (defined(INCL_DEV) || !defined(INCL_NOCOMMON))
   
   /* General DEV return values */
   #define DEV_ERROR                       0L
   #define DEV_OK                          1L
   
   /* DC type for DevOpenDC */
   #define OD_QUEUED                       2L
   #define OD_DIRECT                       5L
   #define OD_INFO                         6L
   #define OD_METAFILE                     7L
   #define OD_MEMORY                       8L
   #define OD_METAFILE_NOQUERY             9L
   
   /* codes for DevQueryCaps */
   #define CAPS_FAMILY                     0L
   #define CAPS_IO_CAPS                    1L
   #define CAPS_TECHNOLOGY                 2L
   #define CAPS_DRIVER_VERSION             3L
   #define CAPS_WIDTH                      4L      /* pels            */
   #define CAPS_HEIGHT                     5L      /* pels            */
   #define CAPS_WIDTH_IN_CHARS             6L
   #define CAPS_HEIGHT_IN_CHARS            7L
   #define CAPS_HORIZONTAL_RESOLUTION      8L      /* pels per meter  */
   #define CAPS_VERTICAL_RESOLUTION        9L      /* pels per meter  */
   #define CAPS_CHAR_WIDTH                10L      /* pels            */
   #define CAPS_CHAR_HEIGHT               11L      /* pels            */
   #define CAPS_SMALL_CHAR_WIDTH          12L      /* pels            */
   #define CAPS_SMALL_CHAR_HEIGHT         13L      /* pels            */
   #define CAPS_COLORS                    14L
   #define CAPS_COLOR_PLANES              15L
   #define CAPS_COLOR_BITCOUNT            16L
   #define CAPS_COLOR_TABLE_SUPPORT       17L
   #define CAPS_MOUSE_BUTTONS             18L
   #define CAPS_FOREGROUND_MIX_SUPPORT    19L
   #define CAPS_BACKGROUND_MIX_SUPPORT    20L
   #define CAPS_DEVICE_WINDOWING          31L
   #define CAPS_ADDITIONAL_GRAPHICS       32L
   #define CAPS_VIO_LOADABLE_FONTS        21L
   #define CAPS_WINDOW_BYTE_ALIGNMENT     22L
   #define CAPS_BITMAP_FORMATS            23L
   #define CAPS_RASTER_CAPS               24L
   #define CAPS_MARKER_HEIGHT             25L      /* pels            */
   #define CAPS_MARKER_WIDTH              26L      /* pels            */
   #define CAPS_DEVICE_FONTS              27L
   #define CAPS_GRAPHICS_SUBSET           28L
   #define CAPS_GRAPHICS_VERSION          29L
   #define CAPS_GRAPHICS_VECTOR_SUBSET    30L
   #define CAPS_PHYS_COLORS               33L
   #define CAPS_COLOR_INDEX               34L
   #define CAPS_GRAPHICS_CHAR_WIDTH       35L
   #define CAPS_GRAPHICS_CHAR_HEIGHT      36L
   #define CAPS_HORIZONTAL_FONT_RES       37L
   #define CAPS_VERTICAL_FONT_RES         38L
   #define CAPS_DEVICE_FONT_SIM           39L
   #define CAPS_LINEWIDTH_THICK           40L
   #define CAPS_DEVICE_POLYSET_POINTS     41L
   
   /* Constants for CAPS_IO_CAPS */
   #define CAPS_IO_DUMMY                   1L
   #define CAPS_IO_SUPPORTS_OP             2L
   #define CAPS_IO_SUPPORTS_IP             3L
   #define CAPS_IO_SUPPORTS_IO             4L
   
   /* Constants for CAPS_TECHNOLOGY */
   #define CAPS_TECH_UNKNOWN               0L
   #define CAPS_TECH_VECTOR_PLOTTER        1L
   #define CAPS_TECH_RASTER_DISPLAY        2L
   #define CAPS_TECH_RASTER_PRINTER        3L
   #define CAPS_TECH_RASTER_CAMERA         4L
   #define CAPS_TECH_POSTSCRIPT            5L
   
   /* Constants for CAPS_COLOR_TABLE_SUPPORT */
   #define CAPS_COLTABL_RGB_8              1L
   #define CAPS_COLTABL_RGB_8_PLUS         2L
   #define CAPS_COLTABL_TRUE_MIX           4L
   #define CAPS_COLTABL_REALIZE            8L
   
   /* Constants for CAPS_FOREGROUND_MIX_SUPPORT */
   #define CAPS_FM_OR                      1L
   #define CAPS_FM_OVERPAINT               2L
   #define CAPS_FM_XOR                     8L
   #define CAPS_FM_LEAVEALONE             16L
   #define CAPS_FM_AND                    32L
   #define CAPS_FM_GENERAL_BOOLEAN        64L
   
   /* Constants for CAPS_BACKGROUND_MIX_SUPPORT */
   #define CAPS_BM_OR                      1L
   #define CAPS_BM_OVERPAINT               2L
   #define CAPS_BM_XOR                     8L
   #define CAPS_BM_LEAVEALONE             16L
   #define CAPS_BM_AND                    32L
   #define CAPS_BM_GENERAL_BOOLEAN        64L
   #define CAPS_BM_SRCTRANSPARENT        128L
   #define CAPS_BM_DESTTRANSPARENT       256L
   
   /* Constants for CAPS_DEVICE_WINDOWING */
   #define CAPS_DEV_WINDOWING_SUPPORT      1L
   
   /* Constants for CAPS_DEV_FONT_SIM     */
   #define CAPS_DEV_FONT_SIM_BOLD          1L
   #define CAPS_DEV_FONT_SIM_ITALIC        2L
   #define CAPS_DEV_FONT_SIM_UNDERSCORE    4L
   #define CAPS_DEV_FONT_SIM_STRIKEOUT     8L
   
   /* Constants for CAPS_ADDITIONAL_GRAPHICS */
   #define CAPS_VDD_DDB_TRANSFER            1L
   #define CAPS_GRAPHICS_KERNING_SUPPORT    2L
   #define CAPS_FONT_OUTLINE_DEFAULT        4L
   #define CAPS_FONT_IMAGE_DEFAULT          8L
   /* bits represented by values 16L and 32L are reserved */
   #define CAPS_SCALED_DEFAULT_MARKERS     64L
   #define CAPS_COLOR_CURSOR_SUPPORT      128L
   #define CAPS_PALETTE_MANAGER           256L
   #define CAPS_COSMETIC_WIDELINE_SUPPORT 512L
   #define CAPS_DIRECT_FILL              1024L
   #define CAPS_REBUILD_FILLS            2048L
   #define CAPS_CLIP_FILLS               0x00001000 /* 4096L  */
   #define CAPS_ENHANCED_FONTMETRICS     0x00002000 /* 8192L  */
   #define CAPS_TRANSFORM_SUPPORT        0x00004000 /* 16384L */
   #define CAPS_EXTERNAL_16_BITCOUNT     0x00008000 /* 32768L */
   
   #ifndef INCL_SAADEFS
      
      /* Constants for CAPS_WINDOW_BYTE_ALIGNMENT */
      #define CAPS_BYTE_ALIGN_REQUIRED        0L
      #define CAPS_BYTE_ALIGN_RECOMMENDED     1L
      #define CAPS_BYTE_ALIGN_NOT_REQUIRED    2L
   #endif /* no INCL_SAADEFS */
   
   /* Constants for CAPS_RASTER_CAPS */
   #define CAPS_RASTER_BITBLT              1L
   #define CAPS_RASTER_BANDING             2L
   #define CAPS_RASTER_BITBLT_SCALING      4L
   #define CAPS_RASTER_SET_PEL            16L
   #define CAPS_RASTER_FONTS              32L
   #define CAPS_RASTER_FLOOD_FILL         64L
   
   #ifndef INCL_DDIDEFS
      
#ifdef __cplusplus
      HDC  APIENTRY DevOpenDC(HAB hab,
                              LONG lType,
                              PCSZ  pszToken,
                              LONG lCount,
                              PDEVOPENDATA pdopData,
                              HDC hdcComp);
#else
      HDC  APIENTRY DevOpenDC(HAB hab,
                              LONG lType,
                              PSZ pszToken,
                              LONG lCount,
                              PDEVOPENDATA pdopData,
                              HDC hdcComp);
#endif
      
      HMF  APIENTRY DevCloseDC(HDC hdc);
      
      BOOL  APIENTRY DevQueryCaps(HDC hdc,
                                  LONG lStart,
                                  LONG lCount,
                                  PLONG alArray);
   #endif /* no INCL_DDIDEFS */
   
#endif /* common DEV */

#ifdef INCL_DEV
   
   #ifndef INCL_SAADEFS
      
      /* structures for DEVESC_QUERYVIOCELLSIZES */
      typedef struct _VIOSIZECOUNT    /* vios */
      {
         LONG maxcount;
         LONG count;
      } VIOSIZECOUNT;
      typedef VIOSIZECOUNT *PVIOSIZECOUNT;
      
      typedef struct _VIOFONTCELLSIZE    /* viof */
      {
         LONG cx;
         LONG cy;
      } VIOFONTCELLSIZE;
      typedef VIOFONTCELLSIZE *PVIOFONTCELLSIZE;
      
      /* structure for DEVESC_GETSCALINGFACTOR */
      typedef struct _SFACTORS    /* sfactors */
      {
         LONG x;
         LONG y;
      } SFACTORS;
      typedef SFACTORS *PSFACTORS;
      
      /* structure for DEVESC_NEXTBAND */
      typedef struct _BANDRECT    /* bandrect */
      {
         LONG  xleft;
         LONG  ybottom;
         LONG  xright;
         LONG  ytop;
      } BANDRECT;
      typedef BANDRECT *PBANDRECT;
      
      /* return codes for DevEscape */
      #define DEVESC_ERROR                  (-1L)
      #define DEVESC_NOTIMPLEMENTED           0L
      
      /* codes for DevEscape */
      #define DEVESC_QUERYESCSUPPORT          0L
      #define DEVESC_GETSCALINGFACTOR         1L
      #define DEVESC_QUERYVIOCELLSIZES        2L
      #define DEVESC_GETCP                 8000L
      
      #define DEVESC_STARTDOC              8150L
      #define DEVESC_ENDDOC                8151L
      #define DEVESC_NEXTBAND              8152L
      #define DEVESC_ABORTDOC              8153L
      
      #define DEVESC_NEWFRAME             16300L
      #define DEVESC_DRAFTMODE            16301L
      #define DEVESC_FLUSHOUTPUT          16302L
      #define DEVESC_RAWDATA              16303L
      #define DEVESC_SETMODE              16304L
      
      #define DEVESC_DBE_FIRST            24450L
      #define DEVESC_DBE_LAST             24455L
      
      /* DevEscape codes for adding extra space to character strings */
      #define DEVESC_CHAR_EXTRA           16998L
      #define DEVESC_BREAK_EXTRA          16999L
      
      /* codes for DevEscape PM_Q_ESC spool files */
      #define DEVESC_STD_JOURNAL          32600L
      
      /* structure for DEVESC_SETMODE */
      typedef struct _ESCMODE     /* em */
      {
         ULONG mode;
         BYTE  modedata[1];
      } ESCMODE;
      typedef ESCMODE *PESCMODE;
      
      /* return codes for DevPostDeviceModes */
      #define DPDM_ERROR                    (-1L)
      #define DPDM_NONE                       0L
      
      /* codes for DevPostDeviceModes */
      #define DPDM_POSTJOBPROP                0L
      #define DPDM_CHANGEPROP                 1L
      #define DPDM_QUERYJOBPROP               2L
      
      /* string types for DevQueryDeviceNames */
      typedef CHAR STR16[16];     /* str16 */
      typedef STR16 *PSTR16;
      typedef CHAR STR32[32];     /* str32 */
      typedef STR32 *PSTR32;
      typedef CHAR STR64[64];     /* str64 */
      typedef STR64 *PSTR64;
      
      /* return code for DevQueryHardcopyCaps */
      #define DQHC_ERROR                    (-1L)
      
      /* codes for DevQueryHardcopyCaps */
      #define HCAPS_CURRENT                   1L
      #define HCAPS_SELECTABLE                2L
      
      /* structure for DevQueryHardcopyCaps */
      typedef struct _HCINFO           /* hci */
      {
         CHAR   szFormname[32];
         LONG   cx;
         LONG   cy;
         LONG   xLeftClip;
         LONG   yBottomClip;
         LONG   xRightClip;
         LONG   yTopClip;
         LONG   xPels;
         LONG   yPels;
         LONG   flAttributes;
      } HCINFO;
      typedef HCINFO *PHCINFO;
      
      /* Device Context Functions */
      #ifndef INCL_DDIDEFS
         
         LONG  APIENTRY DevEscape(HDC hdc,
                                  LONG lCode,
                                  LONG lInCount,
                                  PBYTE pbInData,
                                  PLONG plOutCount,
                                  PBYTE pbOutData);
         
#ifdef __cplusplus
         BOOL  APIENTRY DevQueryDeviceNames(HAB hab,
                                            PCSZ  pszDriverName,
                                            PLONG pldn,
                                            PSTR32 aDeviceName,
                                            PSTR64 aDeviceDesc,
                                            PLONG pldt,
                                            PSTR16 aDataType);
#else
         BOOL  APIENTRY DevQueryDeviceNames(HAB hab,
                                            PSZ pszDriverName,
                                            PLONG pldn,
                                            PSTR32 aDeviceName,
                                            PSTR64 aDeviceDesc,
                                            PLONG pldt,
                                            PSTR16 aDataType);
#endif
         
         LONG  APIENTRY DevQueryHardcopyCaps(HDC hdc,
                                             LONG lStartForm,
                                             LONG lForms,
                                             PHCINFO phciHcInfo);
         
#ifdef __cplusplus
         LONG  APIENTRY DevPostDeviceModes(HAB hab,
                                           PDRIVDATA pdrivDriverData ,
                                           PCSZ  pszDriverName,
                                           PCSZ  pszDeviceName,
                                           PCSZ  pszName,
                                           ULONG flOptions);
#else
         LONG  APIENTRY DevPostDeviceModes(HAB hab,
                                           PDRIVDATA pdrivDriverData ,
                                           PSZ pszDriverName,
                                           PSZ pszDeviceName,
                                           PSZ pszName,
                                           ULONG flOptions);
#endif
         
      #endif /* no INCL_DDIDEFS */
   #endif /* no INCL_SAADEFS */
#endif /* non-common DEV */

#ifdef INCL_DEVERRORS
   /* AAB error codes for the DEV - same as GPI errors at present */
#endif /* INCL_DEVERRORS */

#ifdef __cplusplus
        }
#endif
#if __IBMC__ || __IBMCPP__
   #pragma info( none )
   #ifndef __CHKHDR__
      #pragma info( restore )
   #endif
   #pragma info( restore )
#endif

/**************************** end of file **********************************/
