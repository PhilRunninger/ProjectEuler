@echo off
setlocal enabledelayedexpansion
set limit=999
for /l %%a in (0,3,%limit%) do set /a sum=!sum!+%%a
for /l %%a in (0,5,%limit%) do set /a sum=!sum!+%%a
for /l %%a in (0,15,%limit%) do set /a sum=!sum!-%%a
echo The answer is: %sum%
