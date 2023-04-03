.*==============================================================*\
.*                                                              *
.* wsfile.ipf - Information Tag Language file for the WSFILE    *
.*              Class sample application's help manager         *
.*                                                              *
.*==============================================================*/
:userdoc.

.*--------------------------------------------------------------*\
.*  Default Help for WSFILE   class                             *
.*      res = ID_HELP_DEFAULT                                   *
.*--------------------------------------------------------------*/
:h1 res=256 name=ID_HELP_DEFAULT.WSFILE Class -- Help
:i1 id=aboutMain.About WSFILE Class
:p.This Class is a subclass of WPDataFile.

.*--------------------------------------------------------------*\
.*  Open Submenu item                                           *
.*      res = ID_HELP_OPENWSFILE
.*--------------------------------------------------------------*/
:h1 res=257 name=ID_HELP_OPENWSFILE.Open Special File View
:i1 id=Open.Special View
:p.This is a special, view of a wsfile's contents.

.*--------------------------------------------------------------*\
.*  Instance variable help                                      *
.*      res = ID_HELP_LASTWORKER
.*--------------------------------------------------------------*/
:h1 res=258 name=ID_HELP_LASTWORKER.Last Worker Instance Variable
:i1 id=IV.LastWorkder
:p.Last worker is the name of the person to last work on this file.

.*--------------------------------------------------------------*\
.*  Context Menu Item help                                      *
.*      res = ID_HELP_SOUNDALARM
.*--------------------------------------------------------------*/
:h1 res=259 name=ID_HELP_SOUNDALARM.Sound an alarm
:i1 id=IV.SoundAlarm
:p.Pressing this menu option will sound several beeps.


:euserdoc.
