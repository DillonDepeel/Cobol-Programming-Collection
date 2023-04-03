:userdoc.
:prolog.
:title.
:tline.PMREXX HELP
:etitle.
:eprolog.
:body.

:h1 res=01.Help for PMREXX
:i1.PMREXX
:i1.About PMREXX
:p.Use PMREXX to run and debug REXX procedures.  PMREXX is a windowed
application in which you can display the output from
a REXX procedure and from any programs called by the REXX procedure.  You
can use the scrollbar to view any previous output that
has not been cleared.
:p.PMREXX also provides a single line input field so that you can supply
input to the REXX procedure or to any commands called by the REXX procedure.
For more information about using REXX procedures, see :hp1.REXX
Information:ehp1..
.********************************************************************

.*Help for File       (AB)

:h1 res=02 nosearch.Help for File
:p.Use the choices on the :hp2.File:ehp2. pull-down to open and
save files and to exit PMREXX.
.********************************************************************

.*Help for Save  (PD)
:h1 res=03 nosearch.Help for Save
:p.Use :hp2.Save:ehp2. to save the REXX procedure displayed in the PMREXX
window to the same file to which it was previously saved.  If you have not
previously saved the output window, this choice has the same
effect as the :hp2.Save as:ehp2. choice.
.***********************************************************************

.*Help for Save as    (PD)

:h1 res=04 nosearch.Help for Save As
:p.Use :hp2.Save as:ehp2. to name and save a new file or to save an existing
file under a different name, in a different directory, or on a different disk.
When you choose :hp2.Save as:ehp2., a pop-up is displayed.
.**********************************************************************


.*Help for Save As Dialog box (D)

:h1 hide res=4210 scroll=none titlebar=yes group=100 id=A9004.Help for Save as
:link reftype=hd res=26500 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=26600 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.

:h1 hide res=26500 nosearch noprint.Help for Save as
:ol compact.
:li.Select the down arrow to the right of the :hp2.Drive:ehp2. list to display
all the drives on your system.
:li.Select a drive from the :hp2.Drive:ehp2. list.
:li.Select a directory from the :hp2.Directory:ehp2. list.
:li.Type the name of the file in which you want to save the contents of
the output window in the :hp2.File name:ehp2. field
and select the :hp2.Save:ehp2. pushbutton.
:eol.
:p.For specific help, select a topic below.

:h1 hide res=26600 nosearch noprint.List of Fields
:dl compact tsize=20.
:dt.:link reftype=hd res=26501 group=200.File name:elink.
:dd.:link reftype=hd res=26502 group=210.Type of file:elink.
:dt.:link reftype=hd res=26503 group=220.Drive:elink.
:dd.:link reftype=hd res=26504 group=230.File:elink.
:dt.:link reftype=hd res=26505 group=240.Directory:elink.
:dd.:link reftype=hd res=26506 group=250.Save pushbutton:elink.
:dt.:link reftype=hd res=7205  group=251.Type pushbutton:elink.
:dd.
:edl.
.***********************************************************************

.*Help for File name  (F)

:h1 hide res=26501 scroll=none titlebar=yes group=260 nosearch noprint.Help for File name
:link reftype=hd res=26550 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=26650 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.

:h1 hide res=26550 nosearch.Help for File name
:p.Type the name of the file in which you want to place the contents
of the output window in the
:hp2.File name:ehp2. field
and select the :hp2.Save:ehp2. pushbutton.
:p.For more help, select a topic below.

:h1 hide res=26650 nosearch noprint.List of Fields
:dl compact tsize=20.
:dt.:link reftype=hd res=26501 group=270.File name:elink.
:dd.:link reftype=hd res=26502 group=280.Type of file:elink.
:dt.:link reftype=hd res=26503 group=290.Drive:elink.
:dd.:link reftype=hd res=26504 group=300.File:elink.
:dt.:link reftype=hd res=26505 group=310.Directory:elink.
:dd.:link reftype=hd res=26506 group=320.Save pushbutton:elink.
:dt.:link reftype=hd res=7205  group=321.Type pushbutton:elink.
:dd.:link reftype=hd res=4210  group=330.General help:elink.
:edl.
.************************************************************************

.*Help for Type of file  (F)

:h1 hide res=26502 scroll=none titlebar=yes group=380 nosearch noprint.Help for Type of file
:link reftype=hd res=26552 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=26650 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.

:h1 hide res=26552 group=390 nosearch.Help for Type of file
:p.Select the down arrow to the right of the :hp2.Type of file:ehp2. field to
display the available file types.  PMREXX is set for all file types.
:p.For more help, select a topic below.
.***********************************************************************

.*Help for Drive     (F)

:h1 hide res=26503 scroll=none titlebar=yes group=110 nosearch noprint.Help for Drive
:link reftype=hd res=26553 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=26650 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.

:h1 hide res=26553 group=400 nosearch.Help for Drive
:p.The :hp2.Drive:ehp2. list displays the drives on your system.  Select the
drive that contains the file in which you want to place the contents
of the output window.
:p.For more help, select a topic below.
.***************************************************************************

.*Help for File    (F)

:h1 hide res=26504 scroll=none titlebar=yes group=120 nosearch noprint.Help for File
:link reftype=hd res=26554 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=26650 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.

:h1 hide res=26554 group=420 nosearch.Help for File
:p.The :hp2.File:ehp2. list displays all the files in the directory
you selected from the :hp2.Directory:ehp2. list.  Select the file in which
you want to place the contents of the output window.
:p.For more help, select a topic below.
.***************************************************************************


.*Help for Directory    (F)

:h1 hide res=26505 scroll=none titlebar=yes group=11 nosearch noprint.Help for Directory
:link reftype=hd res=26555 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=26650 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.

:h1 hide res=26555 group=430 nosearch.Help for Directory
:p.The :hp2.Directory:ehp2. list displays the directories on the selected
drive.  Select a directory to display the list of files from that directory
in the :hp2.File:ehp2. list box.
:p.For more help, select a topic below.
.*************************************************************************

.*Help for Save pushbutton  (PU)

:h1 hide res=26506 scroll=none titlebar=yes group=170 nosearch noprint.Help for Save
:link reftype=hd res=26556 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=26650 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.

:h1 hide res=26556 group=440 nosearch.Help for Save
:p.Select the :hp2.Save:ehp2. pushbutton to save the contents of the output window
to the file, drive and directory you selected and with the file name
you specified.
:p.For more help, select a topic below.
.*************************************************************************

.*Help for Type pushbutton  (PU)


:h1 hide res=7205 scroll=none titlebar=yes group=8 id=A7205 nosearch noprint.Help for Type
:link reftype=hd res=7108 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=26650 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.


.*Help for Type (PU)
:h1 hide res=7108 id=A7108 nosearch.Help for Type
:p.PMREXX is set for all file types so that this pushbutton is not active.
:p.For more help, select a topic below.
.**************************************************************************

.*Help for Exit   (PD)
:h1 res=09 nosearch.Help for Exit
:p.Use :hp2.Exit:ehp2.
to close PMREXX.
.*******************************************************************

.*Help for Edit  (AB)
:h1 res=10 nosearch.Help for Edit
:p.Use the choices on the :hp2.Edit:ehp2. pull-down to copy, move, or delete
the contents of the output window.
.*************************************************************************

.*Help for Copy from focus window
:h1 res=11 nosearch.Help for Copy
:p.Use :hp2.Copy from focus window:ehp2.
to duplicate
:link reftype=hd RES=4864 group=41 dependent.selected text:elink. from the
output window.  The
text remains in the file, but a copy of the text is moved to the
:link reftype=hd RES=4752 group=40 dependent.Clipboard:elink..
:p.You can use the :hp2.Paste to input:ehp2. choice to insert the text from
the Clipboard into the input window.
:p.To select or mark text&colon.
:ol compact.
:li.Position the mouse pointer to the left of the first character
to be selected.
:li.Hold down mouse button 1; then use the mouse to drag (move)
the cursor to the right of the last character to be selected.
:li.Release mouse button 1.
:eol.
:p.When the cursor comes to a window border while you are marking text,
PMREXX scrolls past the border.
This makes it possible to select more text than can be displayed in the window.
:p.
To deselect text or remove the highlighting, click once within the text
area of the window.
.*************************************************************************

.*Definition Selected Text
:h1 hide res=4864 x=25% y=20% width=70% height=50% scroll=vertical rules=sizeborder titlebar=sysmenu group=41 id=A4864.Selected Text&colon.definition
:i1.Selected text, definition
:p.To select text means to highlight the text that you want to delete
or move.  You delete or move selected text by using the :hp2.Clear:ehp2.,
:hp2.Copy from focus window:ehp2., or :hp2.Paste to input:ehp2. choices on the
:hp2.Edit:ehp2. pull-down.
.************************************************************************


.*Definition Clipboard
:h1 hide res=4752 x=25% y=20% width=70% height=50% scroll=vertical rules=sizeborder titlebar=sysmenu group=40 id=A4752.Clipboard&colon.definition
.*:i1.Clipboard, definition
:p.
A clipboard is an area of storage that can temporarily hold data
that is available to other programs.
.*************************************************************************


:h1 res=12 nosearch.Help for Paste to input
:p.Use :hp2.Paste to input:ehp2.
to insert the text from
the :link reftype=hd RES=4752 group=40 dependent.Clipboard:elink.
into the input window.  Press Enter to pass the text you have pasted in
the input window into the REXX program.
:p.If text is :link reftype=hd res=4864 group=41 dependent.selected:elink.,
the text from the Clipboard will replace the selected text.
:nt.Use
:hp2.Copy from focus window:ehp2. to put text on the
Clipboard.
:ent.
.*************************************************************************

.*Help for Clear
:h1 res=13 nosearch.Help for Clear
:p.Use :hp2.Clear:ehp2.
to delete
:link reftype=hd RES=4864 dependent.selected text:elink.
from the output window.
:p.To delete selected text&colon.
:ol compact.
:li.Select the text to be deleted.
:li.Select :hp2.Edit:ehp2..
:li.Select :hp2.Clear:ehp2..
:eol.
.**********************************************************************


.*Help for Select All (PD)
:h1 res=14 nosearch.Help for Select All
:p.Use :hp2.Select all:ehp2.
to
:link reftype=hd RES=4864.select:elink.
all of the text in the file.  You can then perform some action on this
text.  (For example, you could copy all the text to the clipboard.)
.**************************************************************************


.*Help for Options  (AB)
:h1 res=15 nosearch.Help for Options
:p.Use the choices on the :hp2.Options:ehp2. pull-down to restart
procedures, set interactive trace, and set fonts.
.************************************************************************


.*Help for Restart
:h1 res=16 nosearch.Help for Restart
:p.Use :hp2.Restart:ehp2. to start a PMREXX procedure again.
.**********************************************************************

.*Help for Interactive trace on
:h1 res=17.Help for Interactive trace on
:p.
Use :hp2.Interactive trace on:ehp2. to start an interactive trace.
Interactive Trace is used for debugging your program.
.***********************************************************************

.*Help for Set font
:h1 res=18.Help for Set font
:p.Use :hp2.Set font:ehp2.
to display the text in the current file in a different font.  When you
choose :hp2.Set font:ehp2., a pop-up is displayed.
.***********************************************************************


.*Help for Set font    (D)    Dialog Box
:h1 hide res=4481 scroll=none titlebar=yes group=25 id=A4481.Help for Set Font
:link reftype=hd res=7005 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=7304 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.


:h1 hide res=7005 id=A7005 nosearch.Help for Set Font
:ol compact.
:li.Select a font name from the :hp2.Font:ehp2. list.
:li.Select a font size from the :hp2.Size:ehp2. list.
:li.Select a font style from the :hp2.Typestyle:ehp2. check boxes.  Your
font selections are displayed in the :hp2.Font Sample:ehp2. field.
:li.Select :hp2.Set:ehp2. to put the Set font choices into effect.
:eol.
:p.For specific help, select a topic below.

:h1 hide res=7304 id=A7304 nosearch noprint.List of Fields
:dl compact tsize=20.
:dt.:link reftype=hd res=7217 group=26.Font:elink.
:dd.:link reftype=hd res=7218 group=27.Size:elink.
:dt.:link reftype=hd res=7219 group=28.Typestyle:elink.
:dd.:link reftype=hd res=7220 group=29.Set pushbutton:elink.
:dt.:link reftype=hd res=7221 group=30.Apply pushbutton:elink.
:dd.
:edl.
.***********************************************************************




:h1 hide res=7217 scroll=none titlebar=yes group=26 id=A7217 nosearch noprint.Help for Font
:link reftype=hd res=7120 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=7354 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.


.*Help for Font (F)
:h1 hide res=7120 group=65 id=A7120 nosearch.Help for Font
:p.This list shows the fonts available on your system.  When you select a
font, it is displayed in the :hp2.Font
Sample:ehp2. field.
:p.For more help, select a topic below.

:h1 hide res=7354 id=A7354 nosearch noprint.List of Fields
:dl compact tsize=20.
:dt.:link reftype=hd res=7217 group=26.Font:elink.
:dd.:link reftype=hd res=7218 group=27.Size:elink.
:dt.:link reftype=hd res=7219 group=28.Typestyle:elink.
:dd.:link reftype=hd res=7220 group=29.Set pushbutton:elink.
:dt.:link reftype=hd res=7221 group=30.Apply pushbutton:elink.
:dd.:link reftype=hd res=4481 group=25.General help:elink.
:edl.
.************************************************************************


.*Help for Set Font Dialog box   (D)
:h1 hide res=4482 scroll=none titlebar=yes group=25 id=A4482 nosearch noprint.Help for Set font
:link reftype=hd res=7005 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=7304 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.


:h1 hide res=7218 scroll=none titlebar=yes group=27 id=A7218 nosearch noprint.Help for Size
:link reftype=hd res=7130 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=7354 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.

.*Help for Size (F)
:h1 hide res=7130 group=63 id=A7130 nosearch.Help for Size
:p.This list contains all the sizes in which the selected font can be
displayed.  When you select a size, it is displayed in the :hp2.Font
Sample:ehp2. field.
:p.For more help, select a topic below.
.**************************************************************************


.*Help for Set Font box   (D)
:h1 hide res=4483 scroll=none titlebar=yes group=25 id=A4483 nosearch noprint.Help for Set Font
:link reftype=hd res=7005 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=7304 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.


:h1 hide res=7219 scroll=none titlebar=yes group=28 id=A7219 nosearch noprint.Help for Typestyle
:link reftype=hd res=7121 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=7354 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.


.*Help for Typestyle (F)
:h1 hide res=7121 id=A7121 nosearch.Help for Typestyle
:p.This list shows the typestyles you can choose
for the selected font.  When you select a typestyle, it is displayed in the
:hp2.Font Sample:ehp2. field.
:p.For more help, select a topic below.
.**************************************************************************


.*Help for Set Font box   (D)
:h1 hide res=4484 scroll=none titlebar=yes group=25 id=A4484 nosearch noprint.Help for Set Font
:link reftype=hd res=7005 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=7304 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.


:h1 hide res=7220 scroll=none titlebar=yes group=29 id=A7220 nosearch noprint.Help for Set
:link reftype=hd res=7122 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=7354 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.

.*Help for Set (PU)
:h1 hide res=7122 id=A7122 nosearch.Help for Set
:p.Select the :hp2.Set:ehp2. pushbutton to display the file text
in the font styles you select.
:p.For more help, select a topic below.
.**************************************************************************


.*Help for Set Font box   (D)
:h1 hide res=4485 scroll=none titlebar=yes group=25 id=A4485 nosearch noprint.Help for Set Font
:link reftype=hd res=7005 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=7304 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.


:h1 hide res=7221 scroll=none titlebar=yes group=30 id=A7221 nosearch noprint.Help for Apply
:link reftype=hd res=7123 vpx=left vpy=top vpcx=100% vpcy=70% scroll=vertical rules=border titlebar=none child auto.
:link reftype=hd res=7354 vpx=left vpy=bottom vpcx=100% vpcy=30% scroll=vertical rules=none titlebar=none child auto.

.*Help for Apply (PU)
:h1 hide res=7123 id=A7123 nosearch.Help for Apply
:p.Select the :hp2.Apply:ehp2. pushbutton to display the file text
in the font styles you select.
:p.When you use the :hp2.Apply:ehp2. pushbutton, the
Set font pop-up remains on the screen.
:p.For more help, select a topic below.
.*************************************************************************


.*Help for Halt Procedure
:h1 res=25 nosearch.Help for Halt Procedure
:p.Use :hp2.Halt procedure:ehp2. to stop a PMREXX program.
.************************************************************************


.*Help for Trace next clause
:h1 res=26 nosearch.Help for Trace next clause
:p.
Use :hp2.Trace next clause:ehp2. to step to the next clause in the PMREXX
procedure.  Using this choice is the same as entering a
blank line in the input field.
.**********************************************************************


.*Help for Redo last clause
:h1 res=27 nosearch.Help for Redo last clause
:p.
Use :hp2.Redo last clause:ehp2. to repeat the last PMREXX clause procedure.
Using this choice is the same as entering an
equal sign (=) in the input field.
.************************************************************************


.*Help for Trace off
:h1 res=28 nosearch.Help for Trace off
:p.
Use :hp2.Trace off:ehp2. to turn off the debugging procedure in your
program session.
.*********************************************************************


.*THE HELP KEY DEFINITIONS

.****** HID_KEYS_HELP_PANEL
:h1 res=9001 group=250.Help for Key Assignments
:p.
Select the key group you want help for:
:ul.
:li.
:link reftype=hd res=6375 group=251.HELP KEYS:elink. to get help
and manage help information
:li.
:link reftype=hd res=6376 group=252.SYSTEM KEYS:elink. to switch
among open programs, groups, and online documents
:li.
.*:link reftype=hd res=6377 group=253.PROGRAM WINDOW KEYS:elink.
:link reftype=hd res=6377 group=253.WINDOW MOVEMENT KEYS:elink.
to move around in a program, group, or help window, or to move
or size a window
:li.
:link reftype=hd res=6378 group=254.POP-UP KEYS:elink.
to move around in or complete a pop-up.
:eul.
:p.
When two key names are joined by a plus sign (+), use these two keys
together.  Hold down the first key and press the second key.

:h1 hide res=6375 group=251 id=A6375 nosearch.Help for Help Keys
:p.
From any active window:
:dl tsize=5 break=all compact.
:dt.:hp2.
Alt+F6:ehp2.
:dd.
Switch between the help window and the associated program window
:dt.:hp2.
F1:ehp2.
:dd.
Get help for the active area
:edl.
:p.
From any active window that has a :hp2.Help:ehp2. action bar choice:
:dl tsize=5 break=all compact.
:dt.:hp2.
F2:ehp2.
:dd.
Display general information about the program
:dt.:hp2.
F9:ehp2.
:dd.
Display a list of keys for the program
:dt.:hp2.
F11 or Shift+F1:ehp2.
:dd.
Display the help index for the program
:dt.:hp2.
Shift+F10:ehp2.
:dd.
Get help about the help facility
:edl.
:p.
From any active help window:
:dl tsize=5 break=all compact.
:dt.:hp2.
Alt+F4:ehp2.
:dd.
Close the help window
:dt.:hp2.
Esc:ehp2.
:dd.
Redisplay the previous help
.*Following added per Mark Benge 6/8am -- there will be PTR.
.*topic
topic, if there is one; if not, remove the help window
.*End change
:dt.:hp2.
Ctrl+A:ehp2.
:dd.
Copy the displayed topic and add it to the end of a temporary file
:dt.:hp2.
Ctrl+C:ehp2.
:dd.
Display the help table of contents
:dt.:hp2.
Ctrl+F:ehp2.
:dd.
Copy the displayed topic to a temporary file
:dt.:hp2.
Ctrl+H:ehp2.
:dd.
Display a list of all the topics you have viewed
:dt.:hp2.
Ctrl+L:ehp2.
:dd.
Display a list of libraries that contain help topics for one or
more programs
:dt.:hp2.
Ctrl+N:ehp2.
:dd.
Open a new help window to display any topic
.*
.*you can select from a list
.*of topics or index entries, or sometimes for topics related to words or
.*phrases that have special highlighting
that is displayed in a window where the minimize and maximize choices
in the System Menu pull-down are displayed and available
.*
:dt.:hp2.
Ctrl+S:ehp2.
:dd.
Search for a word or phrase in help topics
:dt.:hp2.
Ctrl+Ins:ehp2.
:dd.
Copy the displayed topic to the clipboard
:edl.
:p.
From any active Contents window that has multiple levels of help topics:
:dl tsize=5 break=all compact.
:dt.:hp2.
Ctrl+* (Asterisk):ehp2.
:dd.
Display all levels of the table of contents
:dt.:hp2.
Ctrl+- (Minus):ehp2.
:dd.
Remove all subtopics from the table of contents
:dt.:hp2.
+ (Plus):ehp2.
:dd.
Display one more level of topics in the table of contents
:dt.:hp2.
* (Asterisk):ehp2.
:dd.
Display all subtopics for one topic in the table of contents
:dt.:hp2.
- (Minus):ehp2.
:dd.
Remove all subtopics from one topic in the table of contents
:dt.:hp2.
Tab:ehp2.
:dd.
Move to the next word or phrase that has special highlighting
:edl.

:h1 hide res=6376 group=252 id=A6376 nosearch.Help for System Keys
:dl tsize=5 break=all compact.
:dt.:hp2.
Alt+Esc or Alt+Tab:ehp2.
:dd.
Switch to the next program (windowed or full-screen), group,
or online document
:dt.:hp2.
Ctrl+Esc:ehp2.
:dd.
Switch to the Task List
:edl.

:h1 hide res=6377 group=253 id=A6377 nosearch.Help for Window Movement Keys
:dl tsize=5 break=all compact.
:dt.:hp2.
F3:ehp2.
:dd.
Close the active program window
:dt.:hp2.
F10 or Alt:ehp2.
:dd.
Switch to or from the action bar
:dt.:hp2.
Arrow keys:ehp2.
:dd.
Move among choices
:dt.:hp2.
End:ehp2.
:dd.
Move to the last choice in a pull-down
:dt.:hp2.
Esc:ehp2.
:dd.
Remove an action bar pull-down or the System Menu pull-down but keep
the action bar choice or the System Menu icon selected
:dt.:hp2.
First letter:ehp2.
:dd.
Select from a list the next choice that starts with the
.*typed letter
letter you type
:dt.:hp2.
Home:ehp2.
:dd.
Move to the first choice in a pull-down
:dt.:hp2.
PgUp:ehp2.
:dd.
Scroll the contents of a window up one page
:dt.:hp2.
PgDn:ehp2.
:dd.
Scroll the contents of a window down one page
:dt.:hp2.
Underlined letter:ehp2.
:dd.
Select a choice on the action bar or a pull-down
:dt.:hp2.
Alt+F4:ehp2.
:dd.
Close the active program window
:dt.:hp2.
Alt+F5:ehp2.
:dd.
Restore the active program window
:dt.:hp2.
Alt+F7:ehp2.
:dd.
Move the active program window
:dt.:hp2.
Alt+F8:ehp2.
:dd.
Size the active program window
:dt.:hp2.
Alt+F9:ehp2.
:dd.
Minimize the active program window
:dt.:hp2.
Alt+F10:ehp2.
:dd.
Maximize the active program window
:dt.:hp2.
Ctrl+PgDn:ehp2.
:dd.
Scroll the contents of a window right one page
:dt.:hp2.
Ctrl+PgUp:ehp2.
:dd.
Scroll the contents of a window left one page
:dt.:hp2.
Shift+Esc or Alt+Spacebar:ehp2.
:dd.
Switch to or from the System Menu pull-down
:dt.
:hp2.Shift+Esc or Alt:ehp2.
:dd.
Switch to or from the System Menu pull-down of an OS/2 or a DOS window
:edl.

:h1 hide res=6378 group=254 id=A6378 nosearch.Help for Pop-Up Keys
:dl tsize=5 break=all compact.
:dt.:hp2.
Alt+F6:ehp2.
:dd.
Switch between the pop-up and the help window
:dt.:hp2.
Enter:ehp2.
:dd.
Send any changes to the program
:dt.:hp2.
Esc:ehp2.
:dd.
Remove the pop-up without sending any changes to the program
:dt.:hp2.
F1:ehp2.
:dd.
Get help for the active area
:dt.:hp2.
Spacebar:ehp2.
:dd.
Select a check box choice
or perform the task described in the selected pushbutton
:dt.:hp2.
Tab:ehp2.
:dd.
Move to the next field (entry field, check box, list, spin button,
first radio button, or first pushbutton)
:edl.
:p.
In an entry field:
:dl tsize=5 break=all compact.
:dt.:hp2.
Backspace
:ehp2.
:dd.
Delete the character to the left of the cursor
:dt.:hp2.
Del:ehp2.
:dd.
Delete the character to the right of the cursor
:dt.:hp2.
End or Ctrl+Right Arrow key:ehp2.
:dd.
Move to the end of a field
:dt.:hp2.
Home or Ctrl+Left Arrow key:ehp2.
:dd.
Move to the beginning of a field
:edl.

:euserdoc.
