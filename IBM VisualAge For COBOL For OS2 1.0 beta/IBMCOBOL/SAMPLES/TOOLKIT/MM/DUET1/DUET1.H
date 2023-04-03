/*************************************************************************
 * File Name    :  duet1.h
 *
 * Description  :  This file contains the definitions used by the Duet
 *                 Player 1 sample program.
 *
 * Concepts     :  n/a
 *
 * MMPM/2 API's :  n/a
 *
 * Required
 *    Files     :  None.
 *
 *
 * Copyright (C) IBM 1991, 1992, 1993
 *************************************************************************/

#define NUM_DUETS          3
#define NUM_PARTS          2

#define INIT_VOLUME        75

#define ID_MESSAGEBOX      1

#define ID_AUDIOHELP       100

#define ID_SL_VOLUME       265
#define ID_TEXT_VOLUME     264
#define ID_GPB_STOP        263
#define ID_GPB_PAUSE       262
#define ID_GPB_PLAY        261
#define ID_TEXT_DUET       260
#define ID_LB_DUET         259
#define ID_PB_HELP         258
#define ID_PB_CANCEL       257
#define ID_DLG_MAIN        100

#define ID_ICON            100


#define ID_DUET_HELPTABLE        300
#define ID_SUBTABLE_MESSAGEBOX   301

/*
 * These are our own window messages.
 */
#define UM_PLAY_AUDIO_HELP (WM_USER+1)
#define UM_KILL_AUDIO_HELP (WM_USER+2)


#define WC_AUDIO_HELP      "DPAudioHelpWC"


/*
 * User parms for the mci operations - these will identify which
 * action generated a received notifications messages.
 */
#define UP_OPEN   1
#define UP_PLAY   2
#define UP_PAUSE  3
#define UP_RESUME 4
#define UP_STOP   5
#define UP_CLOSE  6
#define UP_VOLUME 7
#define UP_GROUP  8

/*
 * Button bitmap id's
 */

#define ID_BMP_PLAY0    101
#define ID_BMP_PLAY1    102
#define ID_BMP_PLAY2    103
#define ID_BMP_PLAY3    104
#define ID_BMP_PLAY4    105

#define ID_BMP_PAUSE    106

#define ID_BMP_STOP     107

/*
 * String defines for the string table.
 */
#define LEN_ERROR_TITLE                      60
#define LEN_ERROR_MESSAGE                    128
#define LEN_PROGRAM_TITLE                    60
#define LEN_HELP_WINDOW_TITLE                60
#define LEN_HELP_LIBRARY_NAME                13 /* DOS limit   */
#define LEN_DUET_TITLE                       40
#define LEN_AUDIO_FILENAME                   13 /* DOS limit   */

#define IDS_PROGRAM_TITLE                    1
#define IDS_HELP_CREATION_FAILED             2
#define IDS_UNKNOWN                          3
#define IDS_MCI_NOTIFY_ERROR                 4
#define IDS_DUET_PLAYER_ERROR                5
#define IDS_CANNOT_FIND_AUDIO_FILE           6
#define IDS_NO_DUET_SELECTED                 7
#define IDS_CANNOT_MAKE_GROUP                8
#define IDS_CANT_PROCESS_MESSAGE             9

#define IDS_HELP_WINDOW_TITLE                10
#define IDS_HELP_LIBRARY_NAME                11
#define IDS_HELP_WAVEFILE                    12
#define IDS_DEFAULTSIZE                      13

#define IDS_DUET_1_TITLE                     20
#define IDS_DUET_1_PART1_FILE                21
#define IDS_DUET_1_PART2_FILE                22
#define IDS_DUET_2_TITLE                     23
#define IDS_DUET_2_PART1_FILE                24
#define IDS_DUET_2_PART2_FILE                25
#define IDS_DUET_3_TITLE                     26
#define IDS_DUET_3_PART1_FILE                27
#define IDS_DUET_3_PART2_FILE                28

