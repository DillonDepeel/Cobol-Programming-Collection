       01  AlignCode-Attributes.
           05  AlignCode               PIC 9(9) USAGE COMP.
           05  AlignCode-Values.
               10  Align-Left          PIC 9(9) COMP VALUE 1.
               10  Align-Right         PIC 9(9) COMP VALUE 2.
               10  Align-Center        PIC 9(9) COMP VALUE 3.
       01  Allowed-Attributes.
           05  Allowed                 PIC 9(9) USAGE COMP.
           05  Allowed-Values.
               10  Is-Not-Allowed      PIC 9(9) COMP VALUE 0.
               10  Is-Allowed          PIC 9(9) COMP VALUE 1.
       01  Arrange-Attributes.
           05  Arrange                 PIC 9(9) USAGE COMP.
           05  Arrange-Values.
               10  Arrange-Unsorted    PIC 9(9) COMP VALUE 1.
               10  Arrange-Ascending   PIC 9(9) COMP VALUE 2.
               10  Arrange-Descending  PIC 9(9) COMP VALUE 3.
       01  Binding-Attributes.
           05  Binding                 PIC 9(9) USAGE COMP.
           05  Binding-Values.
               10  Binding-Spiral      PIC 9(9) COMP VALUE 1.
               10  Binding-Solid       PIC 9(9) COMP VALUE 2.
       01  Bold-Attributes.
           05  BoldFlag                PIC 9(9) USAGE COMP.
           05  BoldFlag-Values.
               10  BoldFlag-Off        PIC 9(9) COMP VALUE 0.
               10  BoldFlag-On         PIC 9(9) COMP VALUE 1.
       01  BorderFlag-Attributes.
           05  BorderFlag              PIC 9(9) USAGE COMP.
           05  BorderFlag-Values.
               10  Has-No-Border       PIC 9(9) COMP VALUE 0.
               10  Has-Border          PIC 9(9) COMP VALUE 1.
       01  BottomPosition              PIC 9(9) USAGE COMP.
       01  Checked-Attributes.
           05  CheckedFlag             PIC 9(9) USAGE COMP.
           05  CheckedFlag-Values.
               10  CheckedFlag-Off     PIC 9(9) COMP VALUE 0.
               10  CheckedFlag-On      PIC 9(9) COMP VALUE 1.
       01  Collapsed-Attributes.
           05  Collapsed               PIC 9(9) USAGE COMP.
           05  Collapsed-Values.
               10  Is-Expanded         PIC 9(9) COMP VALUE 0.
               10  Is-Collapsed        PIC 9(9) COMP VALUE 1.
       01  Color-Attributes.
           05  Color                   PIC 9(9) USAGE COMP.
           05  Color-Values.
               10  Color-White         PIC 9(9) COMP VALUE 0.
               10  Color-Black         PIC 9(9) COMP VALUE 1.
               10  Color-Blue          PIC 9(9) COMP VALUE 2.
               10  Color-Red           PIC 9(9) COMP VALUE 3.
               10  Color-Pink          PIC 9(9) COMP VALUE 4.
               10  Color-Green         PIC 9(9) COMP VALUE 5.
               10  Color-Cyan          PIC 9(9) COMP VALUE 6.
               10  Color-Yellow        PIC 9(9) COMP VALUE 7.
               10  Color-DarkGray      PIC 9(9) COMP VALUE 8.
               10  Color-DarkBlue      PIC 9(9) COMP VALUE 9.
               10  Color-DarkRed       PIC 9(9) COMP VALUE 10.
               10  Color-DarkPink      PIC 9(9) COMP VALUE 11.
               10  Color-DarkGreen     PIC 9(9) COMP VALUE 12.
               10  Color-DarkCyan      PIC 9(9) COMP VALUE 13.
               10  Color-Brown         PIC 9(9) COMP VALUE 14.
               10  Color-PaleGray      PIC 9(9) COMP VALUE 15.
       01  Command.
           05  Command-Length          PIC 9(9) USAGE COMP.
           05  Command-String.
               10  Command-Chars       PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON Command-Length.
       01  CompClose-Attributes.
           05  CompCloseFlag            PIC 9(9) USAGE COMP.
           05  CompCloseFlag-Values.
               10  CompClose-Child      PIC 9(9) COMP VALUE 0.
               10  CompClose-Parent     PIC 9(9) COMP VALUE 1.
       01  ComponentHandle             PIC X(4).
       01  ComponentHandleStruct       PIC X(4).
      * getComponentHandle returns one handle for each individual
      * object contained in the component.  Therefore,
      * ComponentHandleStruct needs to be defined as the entire
      * COBOL data structure for all the objects.  The default
      * ComponentHandleStruct is already defined in the
      * ComponentName.CPY file as VDE-ComponentName.  Since all
      * of the .CPY files use ACTION-RC as the default variable it
      * is suggessted that you ranme the ACTION-RC variable.
       01  ComponentLoad.
           05  ParentComponent-Length  PIC 9(9) USAGE COMP.
           05  ChildComponent-Length   PIC 9(9) USAGE COMP.
           05  ParentComponent-String.
               10  ParentComponent-Chars PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON ParentComponent-Length.
           05  ChildComponent-String.
               10  ChildComponent-Chars  PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON ChildComponent-Length.
       01  ComponentName.
           05  ComponentName-Length    PIC 9(9) USAGE COMP.
           05  ComponentName-String.
               10  ComponentName-Chars PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON ComponentName-Length.
       01  ComponentObject.
           05  ObjectCompName-Length   PIC 9(9) USAGE COMP.
           05  ObjectPartName-Length   PIC 9(9) USAGE COMP.
           05  ObjectCompName-String.
               10  ObjectCompName-Chars PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON ObjectCompName-Length.
           05  ObjectPartName-String.
               10  ObjectPartName-Chars  PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON ObjectPartName-Length.
      * The ContIconFile-Length field must contain the EXACT length
      * for the ICON file name ex. ALTER.ICO = 9
       01  ContainerRecord.
           05  ContRecordId            PIC 9(9) USAGE COMP.
           05  ContParentId            PIC 9(9) USAGE COMP.
           05  ContIconText-Length     PIC 9(9) USAGE COMP.
           05  ContIconFile-Length     PIC 9(9) USAGE COMP.
           05  ContIconText-String.
               10  ContIconText-Chars  PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON ContIconText-Length.
           05  ContIconFile-String.
               10  ContIconFile-Chars  PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON ContIconFile-Length.
      * ContainerParms is used for the columns in a Container part.
      * There can be a maximum of 20 columns.  Only the first 9
      * parameters are used for Code Assistant.
       01  ContainerParms.
           05  NumberOfParms           PIC 9(9) USAGE COMP.
           05  ContainerParms1-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms2-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms3-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms4-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms5-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms6-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms7-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms8-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms9-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms10-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms11-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms12-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms13-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms14-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms15-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms16-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms17-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms18-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms19-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms20-Length   PIC 9(9) USAGE COMP.
           05  ContainerParms1-String.
               10  ContainerParms1-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms1-Length.
           05  ContainerParms2-String.
               10  ContainerParms2-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms2-Length.
           05  ContainerParms3-String.
               10  ContainerParms3-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms3-Length.
           05  ContainerParms4-String.
               10  ContainerParms4-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms4-Length.
           05  ContainerParms5-String.
               10  ContainerParms5-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms5-Length.
           05  ContainerParms6-String.
               10  ContainerParms6-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms6-Length.
           05  ContainerParms7-String.
               10  ContainerParms7-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms7-Length.
           05  ContainerParms8-String.
               10  ContainerParms8-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms8-Length.
           05  ContainerParms9-String.
               10  ContainerParms9-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms9-Length.
           05  ContainerParms10-String.
               10  ContainerParms10-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms10-Length.
           05  ContainerParms11-String.
               10  ContainerParms11-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms11-Length.
           05  ContainerParms12-String.
               10  ContainerParms12-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms12-Length.
           05  ContainerParms13-String.
               10  ContainerParms13-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms13-Length.
           05  ContainerParms14-String.
               10  ContainerParms14-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms14-Length.
           05  ContainerParms15-String.
               10  ContainerParms15-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms15-Length.
           05  ContainerParms16-String.
               10  ContainerParms16-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms16-Length.
           05  ContainerParms17-String.
               10  ContainerParms17-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms17-Length.
           05  ContainerParms18-String.
               10  ContainerParms18-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms18-Length.
           05  ContainerParms19-String.
               10  ContainerParms19-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms19-Length.
           05  ContainerParms20-String.
               10  ContainerParms20-Chars  PIC X OCCURS 1 TO 255 TIMES
                                 DEPENDING ON ContainerParms20-Length.
       01  Contents.
           05  Contents-Length         PIC 9(9) USAGE COMP.
           05  Contents-String.
               10  Contents-Chars      PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON Contents-Length.
       01  DDEData.
           05  DDEData-Length          PIC 9(9) USAGE COMP.
           05  DDEData-String.
               10  DDEData-Chars       PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON DDEData-Length.
       01  DDEClient.
           05  ClientWindow-Length     PIC 9(9) USAGE COMP.
           05  ClientPart-Length       PIC 9(9) USAGE COMP.
           05  ClientWindow-String.
               10  ClientWindow-Chars  PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON ClientWindow-Length.
           05  ClientPart-String.
               10  ClientPart-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON ClientPart-Length.
       01  DDEServer.
           05  ServerAppName-Length    PIC 9(9) USAGE COMP.
           05  ServerTopic-Length      PIC 9(9) USAGE COMP.
           05  ServerItem-Length       PIC 9(9) USAGE COMP.
           05  ServerAppName-String.
               10  ServerAppName-Chars   PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON ServerAppName-Length.
           05  ServerTopic-String.
               10  ServerTopic-Chars   PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON ServerTopic-Length.
           05  ServerItem-String.
               10  ServerItem-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON ServerItem-Length.
       01  Drag-Attributes.
           05  DragFlag                PIC 9(9) USAGE COMP.
           05  DragFlag-Values.
               10  DragFlag-Off        PIC 9(9) COMP VALUE 0.
               10  DragFlag-On         PIC 9(9) COMP VALUE 1.
       01  Drop-Attributes.
           05  DropFlag                PIC 9(9) USAGE COMP.
           05  DropFlag-Values.
               10  DropFlag-Off        PIC 9(9) COMP VALUE 0.
               10  DropFlag-On         PIC 9(9) COMP VALUE 1.
       01  Enable-Attributes.
           05  EnableFlag              PIC 9(9) USAGE COMP.
           05  EnableFlag-Values.
               10  EnableFlag-Off      PIC 9(9) COMP VALUE 0.
               10  EnableFlag-On       PIC 9(9) COMP VALUE 1.
       01  ExtSelect-Attributes.
           05  ExtSelectFlag           PIC 9(9) USAGE COMP.
           05  ExtSelectFlag-Values.
               10  ExtSelectFlag-Off   PIC 9(9) COMP VALUE 0.
               10  ExtSelectFlag-On    PIC 9(9) COMP VALUE 1.
       01  FileName.
           05  FileName-Length         PIC 9(9) USAGE COMP.
           05  FileName-String.
               10  FileName-Chars      PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON FileName-Length.
       01  Focus-Attributes.
           05  FocusFlag               PIC 9(9) USAGE COMP.
           05  FocusFlag-Values.
               10  FocusFlag-Off       PIC 9(9) COMP VALUE 0.
               10  FocusFlag-On        PIC 9(9) COMP VALUE 1.
       01  FontName.
           05  FontName-Length         PIC 9(9) USAGE COMP.
           05  FontName-String.
               10  FontName-Chars      PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON FontName-Length.
       01  FontSize                    PIC 9(9) USAGE COMP.
       01  Height                      PIC 9(9) USAGE COMP.
       01  Help-Attributes.
           05  HelpFlag                PIC 9(9) USAGE COMP.
           05  HelpFlag-Values.
               10  HelpFlag-Off        PIC 9(9) COMP VALUE 0.
               10  HelpFlag-On         PIC 9(9) COMP VALUE 1.
       01  HighLight-Attributes.
           05  HighLightFlag           PIC 9(9) USAGE COMP.
           05  HighLightFlag-Values.
               10  HighLightFlag-Off   PIC 9(9) COMP VALUE 0.
               10  HighLightFlag-On    PIC 9(9) COMP VALUE 1.
       01  HorizontalSpacing           PIC 9(9) USAGE COMP.
       01  InfoLabel.
           05  InfoLabel-Length        PIC 9(9) USAGE COMP.
           05  InfoLabel-String.
               10  InfoLabel-Chars     PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON InfoLabel-Length.
       01  Insert-Attributes.
           05  InsertFlag              PIC 9(9) USAGE COMP.
           05  InsertFlag-Values.
               10  InsertFlag-Off      PIC 9(9) COMP VALUE 0.
               10  InsertFlag-On       PIC 9(9) COMP VALUE 1.
       01  Interval                    PIC 9(9) USAGE COMP.
       01  InUse-Attributes.
           05  InUse                   PIC 9(9) USAGE COMP.
           05  InUse-Values.
               10  InUse-Off           PIC 9(9) COMP VALUE 0.
               10  InUse-On            PIC 9(9) COMP VALUE 1.
       01  Italics-Attributes.
           05  ItalicsFlag             PIC 9(9) USAGE COMP.
           05  ItalicsFlag-Values.
               10  ItalicsFlag-Off     PIC 9(9) COMP VALUE 0.
               10  ItalicsFlag-On      PIC 9(9) COMP VALUE 1.
       01  Item.
           05  Item-Length             PIC 9(9) USAGE COMP.
           05  Item-String.
               10  Item-Chars          PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON Item-Length.
       01  ItemCount                   PIC 9(9) USAGE COMP.
       01  ItemIndex                   PIC 9(9) USAGE COMP.
       01  LeftPosition                PIC 9(9) USAGE COMP.
       01  LineNumber                  PIC 9(9) USAGE COMP.
       01  MagPanel-Attributes.
           05  MagPanelFlag            PIC 9(9) USAGE COMP.
           05  MagPanelFlag-Values.
               10  MagPanelFlag-Off    PIC 9(9) COMP VALUE 0.
               10  MagPanelFlag-On     PIC 9(9) COMP VALUE 1.
       01  Mask-Attributes.
           05  MaskFlag                PIC 9(9) USAGE COMP.
           05  MaskFlag-Values.
               10  MaskFlag-Off        PIC 9(9) COMP VALUE 0.
               10  MaskFlag-On         PIC 9(9) COMP VALUE 1.
       01  Max                         PIC S9(9) USAGE COMP.
       01  MediaButton-Attributes.
           05  MediaButton             PIC 9(9) USAGE COMP.
           05  MediaButton-Values.
               10  No-MediaButton          PIC 9(9) COMP VALUE 0.
               10  MediaButton-Pause       PIC 9(9) COMP VALUE 1.
               10  MediaButton-Play        PIC 9(9) COMP VALUE 2.
               10  MediaButton-Record      PIC 9(9) COMP VALUE 3.
               10  MediaButton-Stop        PIC 9(9) COMP VALUE 4.
               10  MediaButton-Next-Track  PIC 9(9) COMP VALUE 5.
               10  MediaButton-Last-Track  PIC 9(9) COMP VALUE 6.
               10  MediaButton-Next-Index  PIC 9(9) COMP VALUE 7.
               10  MediaButton-Last-Index  PIC 9(9) COMP VALUE 8.
               10  MediaButton-Volume      PIC 9(9) COMP VALUE 9.
               10  MediaButton-Position    PIC 9(9) COMP VALUE 10.
       01  MediaMode-Attributes.
           05  MediaMode               PIC 9(9) USAGE COMP.
           05  MediaMode-Values.
               10  Media-Pause         PIC 9(9) COMP VALUE 1.
               10  Media-Play          PIC 9(9) COMP VALUE 2.
               10  Media-Record        PIC 9(9) COMP VALUE 3.
               10  Media-Stop          PIC 9(9) COMP VALUE 4.
       01  MessageButtonReturned       PIC 9(9) USAGE COMP.
           88  MessageButton-OK        VALUE 1.
           88  MessageButton-Cancel    VALUE 2.
           88  MessageButton-Retry     VALUE 4.
           88  MessageButton-Abort     VALUE 8.
           88  MessageButton-Ignore    VALUE 16.
           88  MessageButton-Yes       VALUE 32.
           88  MessageButton-No        VALUE 64.
           88  MessageButton-Enter     VALUE 128.
       01  MsgFileRecord.
           05  MessageNumber           PIC 9(9) USAGE COMP.
           05  NumberOfSubStrings      PIC 9(9) USAGE COMP.
           05  MsgSubStr1-Length       PIC 9(9) USAGE COMP.
           05  MsgSubStr2-Length       PIC 9(9) USAGE COMP.
           05  MsgSubStr3-Length       PIC 9(9) USAGE COMP.
           05  MsgSubStr4-Length       PIC 9(9) USAGE COMP.
           05  MsgSubStr5-Length       PIC 9(9) USAGE COMP.
           05  MsgSubStr6-Length       PIC 9(9) USAGE COMP.
           05  MsgSubStr7-Length       PIC 9(9) USAGE COMP.
           05  MsgSubStr8-Length       PIC 9(9) USAGE COMP.
           05  MsgSubStr9-Length       PIC 9(9) USAGE COMP.
           05  MsgSubStr1-String.
               10  MsgSubStr1-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON MsgSubStr1-Length.
           05  MsgSubStr2-String.
               10  MsgSubStr2-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON MsgSubStr2-Length.
           05  MsgSubStr3-String.
               10  MsgSubStr3-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON MsgSubStr3-Length.
           05  MsgSubStr4-String.
               10  MsgSubStr4-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON MsgSubStr4-Length.
           05  MsgSubStr5-String.
               10  MsgSubStr5-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON MsgSubStr5-Length.
           05  MsgSubStr6-String.
               10  MsgSubStr6-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON MsgSubStr6-Length.
           05  MsgSubStr7-String.
               10  MsgSubStr7-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON MsgSubStr7-Length.
           05  MsgSubStr8-String.
               10  MsgSubStr8-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON MsgSubStr8-Length.
           05  MsgSubStr9-String.
               10  MsgSubStr9-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON MsgSubStr9-Length.
       01  MsgTextRecord.
           05  MessageType             PIC 9(9) USAGE COMP.
           05  MessageButtons          PIC 9(9) USAGE COMP.
           05  MessageText-Length      PIC 9(9) USAGE COMP.
           05  MessageText-String.
               10  MessageText-Chars   PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON MessageText-Length.
       01  MessageType-Values.
           05  MsgType-Information     PIC 9(9) USAGE COMP VALUE 1.
           05  MsgType-Warning         PIC 9(9) USAGE COMP VALUE 2.
           05  MsgType-Error           PIC 9(9) USAGE COMP VALUE 3.
       01  MessageButton-Values.
      * Buttons determined by sum of values
           05  MsgButton-OK            PIC 9(9) USAGE COMP VALUE 1.
           05  MsgButton-Cancel        PIC 9(9) USAGE COMP VALUE 2.
           05  MsgButton-Retry         PIC 9(9) USAGE COMP VALUE 4.
           05  MsgButton-Abort         PIC 9(9) USAGE COMP VALUE 8.
           05  MsgButton-Ignore        PIC 9(9) USAGE COMP VALUE 16.
           05  MsgButton-Yes           PIC 9(9) USAGE COMP VALUE 32.
           05  MsgButton-No            PIC 9(9) USAGE COMP VALUE 64.
           05  MsgButton-Enter         PIC 9(9) USAGE COMP VALUE 128.
       01  Min                         PIC S9(9) USAGE COMP.
       01  Mix.
           05  Red                     PIC 9(9) USAGE COMP.
           05  Green                   PIC 9(9) USAGE COMP.
           05  Blue                    PIC 9(9) USAGE COMP.
       01  MouseFocus-Attributes.
           05  MouseFocusFlag          PIC 9(9) USAGE COMP.
           05  MouseFocusFlag-Values.
               10  MouseFocusFlag-Off  PIC 9(9) COMP VALUE 0.
               10  MouseFocusFlag-On   PIC 9(9) COMP VALUE 1.
       01  MultiSelect-Attributes.
           05  MultiSelectFlag         PIC 9(9) USAGE COMP.
           05  MultiSelectFlag-Values.
               10  MultiSelectFlag-Off   PIC 9(9) COMP VALUE 0.
               10  MultiSelectFlag-On    PIC 9(9) COMP VALUE 1.
       01  NewLine.
           05  NewLine-Length          PIC 9(9) USAGE COMP.
           05  NewLine-String.
               10  NewLine-Chars       PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON newLine-Length.
       01  NewText.
           05  NewText-Length          PIC 9(9) USAGE COMP.
           05  NewText-String.
               10  NewText-Chars       PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON NewText-Length.
       01  NumberOfIntervals           PIC 9(9) USAGE COMP.
       01  NumberOfLines               PIC 9(9) USAGE COMP.
       01  NumberOfTicks               PIC 9(9) USAGE COMP.
       01  NumberSelected              PIC 9(9) USAGE COMP.
       01  ObjectHndl                  PIC X(4).
       01  ObjectWidth                 PIC 9(9) USAGE COMP.
       01  Offset                      PIC 9(9) USAGE COMP.
       01  PageNumber                  PIC 9(9) USAGE COMP.
       01  ParentName.
           05  ParentName-Length       PIC 9(9) USAGE COMP.
           05  ParentName-String.
               10  ParentName-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON ParentName-Length.
       01  PartName.
           05  PartName-Length         PIC 9(9) USAGE COMP.
           05  PartName-String.
               10  PartName-Chars      PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON PartName-Length.
       01  PartType.
           05  PartType-Length         PIC 9(9) USAGE COMP.
           05  PartType-String.
               10  PartType-Chars      PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON PartType-Length.
       01  ReadOnly-Attributes.
           05  ReadOnlyFlag            PIC 9(9) USAGE COMP.
           05  ReadOnlyFlag-Values.
               10  ReadOnlyFlag-Off    PIC 9(9) COMP VALUE 0.
               10  ReadOnlyFlag-On     PIC 9(9) COMP VALUE 1.
       01  RecField.
           05  RecField-RecordId       PIC 9(9) USAGE COMP.
           05  RecField-ColNumber      PIC 9(9) USAGE COMP.
       01  RecordDetails.
           05  RecordDetails-Length    PIC 9(9) USAGE COMP.
           05  RecordDetails-String.
               10  RecordDetails-Chars   PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON RecordDetails-Length.
       01  RecordIcon.
           05  RecordIcon-Length       PIC 9(9) USAGE COMP.
           05  RecordIcon-String.
               10  RecordIcon-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON RecordIcon-Length.
       01  RecordId                    PIC 9(9) USAGE COMP.
       01  RecordText.
           05  RecordText-Length       PIC 9(9) USAGE COMP.
           05  RecordText-String.
               10  RecordText-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON RecordText-Length.
       01  RuntimeParms.
           05  RuntimeParms-Length      PIC 9(4) USAGE COMP.
           05  RuntimeParms-String      PIC X(255).
       01  Selected-Attributes.
           05  SelectedFlag            PIC 9(9) USAGE COMP.
           05  SelectedFlag-Values.
               10  SelectedFlag-Off    PIC 9(9) COMP VALUE 0.
               10  SelectedFlag-On     PIC 9(9) COMP VALUE 1.
       01  SelectedText.
           05  SelectedText-Length     PIC 9(9) USAGE COMP.
           05  SelectedText-String.
               10  SelectedText-Chars  PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON SelectedText-Length.
       01  SelectionRange.
           05  SelectionStart          PIC 9(9) USAGE COMP.
           05  SelectionEnd            PIC 9(9) USAGE COMP.
       01  SequenceCode-Attributes.
           05  SequenceCode            PIC 9(9) USAGE COMP.
           05  SequenceCode-Values.
               10  Sequence-Ascending    PIC 9(9) COMP VALUE 1.
               10  Sequence-Descending   PIC 9(9) COMP VALUE 2.
               10  Sequence-At-Index     PIC 9(9) COMP VALUE 3.
               10  Sequence-At-End       PIC 9(9) COMP VALUE 4.
       01  SliderPosition              PIC 9(9) USAGE COMP.
       01  SliderValue                 PIC S9(9) USAGE COMP.
       01  Strikeout-Attributes.
           05  StrikeoutFlag           PIC 9(9) USAGE COMP.
           05  StrikeoutFlag-Values.
               10  StrikeoutFlag-Off   PIC 9(9) COMP VALUE 0.
               10  StrikeoutFlag-On    PIC 9(9) COMP VALUE 1.
       01  TabLabel.
           05  TabLabel-Length         PIC 9(9) USAGE COMP.
           05  TabLabel-String.
               10  TabLabel-Chars      PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON TabLabel-Length.
       01  TabShape-Attributes.
           05  TabShape                PIC 9(9) USAGE COMP.
           05  TabShape-Values.
               10  TabShape-Squared    PIC 9(9) COMP VALUE 1.
               10  TabShape-Rounded    PIC 9(9) COMP VALUE 2.
               10  TabShape-Polygon    PIC 9(9) COMP VALUE 3.
       01  TextEnd                     PIC 9(9) USAGE COMP.
       01  TextLength                  PIC 9(9) USAGE COMP.
       01  TextStart                   PIC 9(9) USAGE COMP.
       01  TickNumber                  PIC 9(9) USAGE COMP.
       01  Time-Out                    PIC 9(9) USAGE COMP.
       01  TimerMode-Attributes.
           05  TimerMode               PIC 9(9) USAGE COMP.
           05  TimerMode-Values.
               10  Timer-Start         PIC 9(9) COMP VALUE 1.
               10  Timer-Stop          PIC 9(9) COMP VALUE 2.
               10  Timer-Pause         PIC 9(9) COMP VALUE 3.
       01  TitleSeparator-Attributes.
           05  TitleSeparator          PIC 9(9) USAGE COMP.
           05  TitleSeparator-Values.
               10  TitleSeparator-Off  PIC 9(9) COMP VALUE 0.
               10  TitleSeparator-On   PIC 9(9) COMP VALUE 1.
       01  TitleVisibility-Attributes.
           05  TitleVisibility         PIC 9(9) USAGE COMP.
           05  TitleVisibility-Values.
               10  TitleVisibility-Off     PIC 9(9) COMP VALUE 0.
               10  TitleVisibility-On      PIC 9(9) COMP VALUE 1.
       01  TreeLine-Attributes.
           05  TreeLine                PIC 9(9) USAGE COMP.
           05  TreeLine-Values.
               10  TreeLine-Off        PIC 9(9) COMP VALUE 0.
               10  TreeLine-On         PIC 9(9) COMP VALUE 1.
       01  Underline-Attributes.
           05  UnderlineFlag           PIC 9(9) USAGE COMP.
           05  UnderlineFlag-Values.
               10  UnderlineFlag-Off   PIC 9(9) COMP VALUE 0.
               10  UnderlineFlag-On    PIC 9(9) COMP VALUE 1.
       01  UserData.
           05  UserData-Length         PIC 9(9) USAGE COMP.
           05  UserData-String.
               10  UserData-Chars      PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON UserData-Length.
       01  Validation-Attributes.
           05  ValidationFlag          PIC 9(9) USAGE COMP.
           05  ValidationFlag-Values.
               10  ValidationFlag-Off  PIC 9(9) COMP VALUE 0.
               10  ValidationFlag-On   PIC 9(9) COMP VALUE 1.
       01  VerticalSpacing             PIC 9(9) USAGE COMP.
       01  View-Attributes.
           05  View                    PIC 9(9) USAGE COMP.
           05  View-Values.
               10  View-Icon           PIC 9(9) COMP VALUE 1.
               10  View-Tree           PIC 9(9) COMP VALUE 2.
               10  View-Details        PIC 9(9) COMP VALUE 3.
       01  Visible-Attributes.
           05  VisibleFlag             PIC 9(9) USAGE COMP.
           05  VisibleFlag-Values.
               10  VisibleFlag-Off     PIC 9(9) COMP VALUE 0.
               10  VisibleFlag-On      PIC 9(9) COMP VALUE 1.
       01  Volume                      PIC 9(9) USAGE COMP.
       01  WindowName.
           05  WindowName-Length       PIC 9(9) USAGE COMP.
           05  WindowName-String.
               10  WindowName-Chars    PIC X OCCURS 1 TO 255 TIMES
                                   DEPENDING ON WindowName-Length.
       01  WindowMode-Attributes.
           05  WindowMode              PIC 9(9) USAGE COMP.
           05  WindowMode-Values.
               10  Window-Maximized    PIC 9(9) COMP VALUE 1.
               10  Window-Minimized    PIC 9(9) COMP VALUE 2.
               10  Window-Restored     PIC 9(9) COMP VALUE 3.
       01  WordWrap-Attributes.
           05  WordWrapFlag            PIC 9(9) USAGE COMP.
           05  WordWrapFlag-Values.
               10  WordWrapFlag-Off    PIC 9(9) COMP VALUE 0.
               10  WordWrapFlag-On     PIC 9(9) COMP VALUE 1.

