doit: test.exe

test.exe: test.obj
        link386 test.obj /stack:32000 /co /noe,test.exe,,d:\pli\lib\ibmlink.lib d:\pli\lib\ceelink,test.def

test.obj: test.pli test.mak
        pli test.pli
