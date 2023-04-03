
/***************************************************************************\
*                                                                           *
*    Multimedia Mixer Software Development Kit Include File                 *
*                                                                           *
*                                                                           *
* Change History:
* DATE          DEVELOPER                 CHANGE DESCRIPTION
*                                                                           *
\***************************************************************************/
#ifndef _OS2MIXER_H_
#define _OS2MIXER_H_
typedef ULONG  HMIXER;

typedef HMIXER FAR *PHMIXER;


typedef struct _MIXERLINEINFO {
    ULONG     ulLength;              /*length of the struct */
    ULONG     ulNumChannels;     /* number of channels on input */
    ULONG     ulSupport;             /* supported functionality (MIXLINEINFO) */
    ULONG     ulConnectionsPossible; /* lines connectable to */
     ULONG    ulLine;                /* which line to operate on */
 } MIXERLINEINFO;

typedef MIXERLINEINFO FAR *PMIXERLINEINFO;

typedef struct {
        ULONG   ulLength;      /* length of the struct */
        ULONG   ulConnection;
        ULONG   ulLine;
        ULONG   ulFlags;
} LINECONNECTIONS;
typedef LINECONNECTIONS FAR *PLINECONNECTIONS;

typedef struct _MIXERCONTROL{
        ULONG   ulLength;      /* length of the struct */
        ULONG   ulLine;
        ULONG   ulControl;
        ULONG   ulSetting;
        } MIXERCONTROL;
typedef MIXERCONTROL *PMIXERCONTROL;

/* -- need masteraudio command for the following. */


/* flags for MIXERLINEINFO ulSupport field */

#define MIX_BALANCE       MCI_AMP_SET_BALANCE     /* separate balance volume control */
#define MIX_ALC           MCI_AMP_SET_ALC         /* supports Auto Level Control */
#define MIX_MONITOR       MCI_AMP_SET_MONITOR     /* supports Monitor Control */
#define MIX_CROSSOVER     MCI_AMP_SET_CROSSOVER   /* supports crossover change */
#define MIX_LOUDNESS      MCI_AMP_SET_LOUDNESS    /* supports loudness equalization */
#define MIX_MUTE          MCI_AMP_SET_MUTE        /* supports channel mute */
#define MIX_REVERB        MCI_AMP_SET_REVERB      /* supports reverb */
#define MIX_STEREOENHANCE MCI_AMP_SET_STEREOENHANC/* supports stereo enhance */
#define MIX_CUSTOM1       MCI_AMP_SET_CUSTOM1     /* supports custom effect #1 */
#define MIX_CUSTOM2       MCI_AMP_SET_CUSTOM2     /* supports custom effect #2 */
#define MIX_CUSTOM3       MCI_AMP_SET_CUSTOM3     /* supports custom effect #3 */
#define MIX_LRVOLUME      MCI_AMP_SET_LRVOLUME    /* separate left-right volume control */
#define MIX_BASS          MCI_AMP_SET_BASS        /* supports Bass */
#define MIX_MID           MCI_AMP_SET_MID         /* supports Mid */
#define MIX_TREBLE        MCI_AMP_SET_TREBLE      /* supports Treble */
#define MIX_PITCH         MCI_AMP_SET_PITCH       /* supports pitch modifications */
#define MIX_GAIN          MCI_AMP_SET_GAIN        /* supports gain modifications */
#define MIX_CHORUS        MCI_AMP_SET_CHORUS      /* supports Bass */
#define MIX_VOLUME        MCI_AMP_SET_VOLUME      /* supports volume controls */


#define MIX_STEREO_OFF            0x00000000
#define MIX_STEREO_STUDIO         0x40004000
#define MIX_STEREO_HALL           0x80008000
#define MIX_STEREO_STADIUM        0xc000c000

#define MCI_STEREO_OFF            0x00000000
#define MCI_STEREO_STUDIO         0x00000001
#define MCI_STEREO_HALL           0x00000002
#define MCI_STEREO_STADIUM        0x00000004


#define MIX_LOUDNESS_OFF          0x00000000
#define MIX_LOUDNESS_ON           0xFFFFFFFF

#define MCI_LOUDNESS_OFF          0x00000000
#define MCI_LOUDNESS_ON           0x00000001



/* ERROR MESSAGES */

#define MIXERR_BASE            512

#define    MIXERR_NOERR              0
#define    MIXERR_INVALIDHANDLE      MIXERR_BASE+0
#define    MIXERR_INVALIDINPUT       MIXERR_BASE+1
#define    MIXERR_INVALIDOUTPUT      MIXERR_BASE+2
#define    MIXERR_NOTSUPPORTED       MIXERR_BASE+7




/* INPUT LINES */
/* SOURCES */
#define   IN_SYNTHESIZER   0
#define   IN_MIXER         1
#define   IN_EXTERNAL      2
#define   IN_INTERNAL      3
#define   IN_MICROPHONE    4
#define   IN_PCM           5
#define   IN_PC_SPEAKER    6

/* SINKS */

#define   OUT_AMPLIFIER    0
#define   OUT_PCM          1


/* SOURCES */
#define   SOURCE_SYNTHESIZER          0x000000001
#define   SOURCE_LINE                 0x000000002
#define   SOURCE_INTERNAL_AUDIO       0x000000004
#define   SOURCE_MICROPHONE           0x000000008
#define   SOURCE_WAVE                 0x000000010
#define   SOURCE_PC_SPEAKER           0x000000020

/* SINKS */

#define   SINK_LINE_OUT               0x000000001
#define   SINK_SPEAKER                0x000000002
#define   SINK_HEADPHONES             0x000000004
#define   SINK_NULL                   0x000000008
#define   SINK_ALL                    0x000000010


#endif
