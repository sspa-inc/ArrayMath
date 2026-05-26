@echo off
set ff=gfortran -cpp -fbacktrace -ffree-line-length-none -O2 -static -s
set cc=cl /O2 /fp:except /nologo /c
set lk=link -static 


if not exist build mkdir build
if not exist bin mkdir bin
set modflags=-Jbuild -Ibuild
echo %ff% | findstr /B /I "gfortran" >nul
if not errorlevel 1 set modflags=-Jbuild -Ibuild

set src=^
  src\str_upper.f90  ^
  src\f90getopt.F90  ^
  src\mrgref.f90    ^
  src\ArrayMathStats.f90  ^
  src\ArrayMathTable.f90  ^
  src\ArrayMath.f90
echo %ff% %modflags% -o bin\ArrayMath.exe %src%
%ff% %modflags% -o bin\ArrayMath.exe %src%
   

pause
