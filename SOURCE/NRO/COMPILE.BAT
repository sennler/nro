@echo off
cc mkcmds.c -fop _MICROC_=1
if errorlevel 1 goto err
cc mkcsinc.c -fop _MICROC_=1
if errorlevel 1 goto err
cc interprete.c -fop _MICROC_=1
if errorlevel 1 goto err
mkcmds <nro.h >cmds.inc
if errorlevel 1 goto err
mkcsinc <nro.h
if errorlevel 1 goto err
cc nro.c -fop _MICROC_=1
if errorlevel 1 goto err
cc 1tonro.c -fop
echo Making MAN pages...
nro -b -u -man nro.nro >nro.man
echo Making plain text file...
interprete <nro.man >nro.txt
echo Done.
echo.
echo.
echo To use NRO in future just copy the files NRO.COM, and AN to the proper
echo place. The documentation is available in NRO format (NRO.NRO), as a
echo MAN page (NRO.MAN), and as a plain text file (NRO.TXT).
echo Probably INTERPRE.COM is helpful, too.
goto ende

:err
echo an error occured while compiling...

:ende
