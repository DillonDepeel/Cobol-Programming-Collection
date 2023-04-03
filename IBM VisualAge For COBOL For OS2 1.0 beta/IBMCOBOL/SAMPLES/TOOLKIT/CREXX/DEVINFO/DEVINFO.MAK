# NMAKE-compatible MAKE file for the REXX sample program DEVINFO.DLL.

devinfo.dll: devinfo.def devinfo.obj
  ilink /nofree devinfo.obj,devinfo.dll,,REXX,devinfo.def

devinfo.obj:
  ICC -C -Ge-  DEVINFO.C




