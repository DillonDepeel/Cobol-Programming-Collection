#*                                                                       *
#*-----------------------------------------------------------------------*
#* DISCLAIMER OF WARRANTIES:                                             *
#*                                                                       *
#*   The following enclosed code is sample code created by IBM           *
#*   Corporation.  This sample code is not part of any standard IBM      *
#*   product and is provided to you solely for the purpose of assisting  *
#*   you in the development of your applications.  The code is provided  *
#*   "AS IS", without warranty of any kind.  IBM shall not be liable     *
#*   for any damages arising out of your use of the sample code, even    *
#*   if they have been advised of the possibility of such damages.       *
#*                                                                       *
#*-----------------------------------------------------------------------*
#*                                                                       *
# MAKE file for LXMIN2.DLL.
#
# Note:  Add /Tdp option to the icc command (and remove /Ss)
#        in order to compile as C++.

LXMIN2.DLL: LXMIN2.OBJ LPEXEXT.DEF
  ILINK /NOFREE /NOI /NOL /NOD LXMIN2, LXMIN2.DLL,, LPEXAPI cppom30i OS2386 ,LPEXEXT

LXMIN2.OBJ: LXMIN2.C
  icc /C /Q /Sp1 /Gm /Gn /Fd /Gd /W3 /Wpro /Ss /Sn /DIBM32 LXMIN2.C
