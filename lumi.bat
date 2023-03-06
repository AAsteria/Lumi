@REM Instructions on how to edit this file
@REM Change the 10th line to "cd /d directory/you/have/to/Lumi"
@REM Instructions on how to add the lumi folder into your environment variables:
@REM 1. Search "environment" in the Taskbar, and click "Edit the system environment variables"
@REM 2. Under "Advanced", click "Environment Variables"
@REM 3. Select the "Path" variable under "System Variables", click "Edit"
@REM 4. Click "New", then copy and paste your Lumi folder's path into system paths

@echo off
cd /d %USERPROFILE%\Desktop\CS 498\Lumi
@REM Ignore for now: "call npm.cmd start"
call stack install
cd /d %USERPROFILE%\AppData\Roaming\local\bin
call lumi.exe