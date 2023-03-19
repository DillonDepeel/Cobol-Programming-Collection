@echo off
REM
REM un-install script for SEE4VB.
REM
if (%windir%) == () goto WIN31

if not exist %windir%\SEE16.DLL goto TEST2
echo Delete %windir%\SEE16.DLL ?
pause
del %windir%\SEE16.DLL

:TEST2
if not exist %windir%\SEE32.DLL goto DONE
echo Delete %windir%\SEE32.DLL ?
pause
del %windir%\SEE32.DLL
goto DONE

:WIN31

if not exist C:\WINDOWS\SEE16.DLL goto TEST4
echo Delete C:\WINDOWS\SEE16.DLL ?
pause
del C:\WINDOWS\SEE16.DLL

:TEST4
if not exist C:\WINDOWS\SEE32.DLL goto DONE
echo Delete C:\WINDOWS\SEE32.DLL ?
pause
del C:\WINDOWS\SEE32.DLL

:DONE


 