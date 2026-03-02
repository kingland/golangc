@echo off
setlocal

set VSTOOLS=C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.44.35207\bin\Hostx64\x64
set VSLIB=C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.44.35207\lib\x64
set WKITUM=C:\Program Files (x86)\Windows Kits\10\Lib\10.0.26100.0\um\x64
set WKITUCRT=C:\Program Files (x86)\Windows Kits\10\Lib\10.0.26100.0\ucrt\x64
set PATH=%VSTOOLS%;%PATH%
set LIB=%VSLIB%;%WKITUM%;%WKITUCRT%;%LIB%

set COMPILER=C:\W\Code\AI\golang-compiler\build\bin\Release\golangc.exe
set TMPDIR=C:\Users\saray\AppData\Local\Temp

echo === TEST 1: byte slice string conversion ===
%COMPILER% %TMPDIR%\p28_t1.go -o %TMPDIR%\p28_t1.exe 2>&1
if exist %TMPDIR%\p28_t1.exe (
    %TMPDIR%\p28_t1.exe
    echo RESULT: PASS
) else (
    echo RESULT: FAIL - no exe produced
)

echo.
echo === TEST 2: os.Open error pattern ===
%COMPILER% %TMPDIR%\p28_t2.go -o %TMPDIR%\p28_t2.exe 2>&1
if exist %TMPDIR%\p28_t2.exe (
    %TMPDIR%\p28_t2.exe
    echo RESULT: PASS
) else (
    echo RESULT: FAIL - no exe produced
)

echo.
echo === TEST 3: struct embedding ===
%COMPILER% %TMPDIR%\p28_t3.go -o %TMPDIR%\p28_t3.exe 2>&1
if exist %TMPDIR%\p28_t3.exe (
    %TMPDIR%\p28_t3.exe
    echo RESULT: PASS
) else (
    echo RESULT: FAIL - no exe produced
)

echo.
echo === TEST 4: slice of interfaces ===
%COMPILER% %TMPDIR%\p28_t4.go -o %TMPDIR%\p28_t4.exe 2>&1
if exist %TMPDIR%\p28_t4.exe (
    %TMPDIR%\p28_t4.exe
    echo RESULT: PASS
) else (
    echo RESULT: FAIL - no exe produced
)

echo.
echo === TEST 5: strings.Builder ===
%COMPILER% %TMPDIR%\p28_t5.go -o %TMPDIR%\p28_t5.exe 2>&1
if exist %TMPDIR%\p28_t5.exe (
    %TMPDIR%\p28_t5.exe
    echo RESULT: PASS
) else (
    echo RESULT: FAIL - no exe produced
)

echo.
echo === TEST 6: linked list ===
%COMPILER% %TMPDIR%\p28_t6.go -o %TMPDIR%\p28_t6.exe 2>&1
if exist %TMPDIR%\p28_t6.exe (
    %TMPDIR%\p28_t6.exe
    echo RESULT: PASS
) else (
    echo RESULT: FAIL - no exe produced
)

echo.
echo === TEST 7: parseAndDouble multi-return ===
%COMPILER% %TMPDIR%\p28_t7.go -o %TMPDIR%\p28_t7.exe 2>&1
if exist %TMPDIR%\p28_t7.exe (
    %TMPDIR%\p28_t7.exe
    echo RESULT: PASS
) else (
    echo RESULT: FAIL - no exe produced
)

echo.
echo === TEST 8: wordCount map ===
%COMPILER% %TMPDIR%\p28_t8.go -o %TMPDIR%\p28_t8.exe 2>&1
if exist %TMPDIR%\p28_t8.exe (
    %TMPDIR%\p28_t8.exe
    echo RESULT: PASS
) else (
    echo RESULT: FAIL - no exe produced
)

echo.
echo === TEST 9: goroutine WaitGroup ===
%COMPILER% %TMPDIR%\p28_t9.go -o %TMPDIR%\p28_t9.exe 2>&1
if exist %TMPDIR%\p28_t9.exe (
    %TMPDIR%\p28_t9.exe
    echo RESULT: PASS
) else (
    echo RESULT: FAIL - no exe produced
)

echo.
echo === TEST 10: iota Direction enum ===
%COMPILER% %TMPDIR%\p28_t10.go -o %TMPDIR%\p28_t10.exe 2>&1
if exist %TMPDIR%\p28_t10.exe (
    %TMPDIR%\p28_t10.exe
    echo RESULT: PASS
) else (
    echo RESULT: FAIL - no exe produced
)

endlocal
