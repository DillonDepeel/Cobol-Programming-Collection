.*===============================================================*\
.*                                                               *
.* wsfolder.ipf - Information Tag Language file for the WSFOLDER *
.*              Class sample application's help manager          *
.*                                                               *
.*===============================================================*/
:userdoc.

.*--------------------------------------------------------------*\
.*  Default Help for WSFOLDER class                             *
.*      res = ID_HELP_DEFAULT                                   *
.*--------------------------------------------------------------*/
:h1 res=256 name=ID_HELP_DEFAULT.WSFOLDER Class -- Help
:i1 id=aboutMain.About WSFOLDER Class
:p.This Class is a subclass of WPFolder.

.*--------------------------------------------------------------*\
.*  Context Menu Item help                                      *
.*      res = ID_HELP_CLOSEVIEWS
.*--------------------------------------------------------------*/
:h1 res=257 name=ID_HELP_CLOSEVIEWS.Close Views Menu Item
:i1 id=IV.CloseViews
:p.When this context menu item is chosen, all currently open views
of this folder will be closed, except for the view that was
initially opened.  If no views are currently open, then no action will
be taken.


:euserdoc.
