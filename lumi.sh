#!/bin/bash
cd ~/Desktop/Lumi
npm start
stack install
cd ~/.local/bin
./lumi

# Instruction:

# Put this file under ~/Lumi/
# 1. Revise path in 2nd line to Lumi directory path on your local machine.
# 2. Revise path in 5th line if necessary. If unsure, under Lumi directory, check manually using `stack install`. It will show where Lumi executable is on your machine.
# 3. Make the script executable by typing `chmod +x lumi.sh` and pressing Enter.
# 4. Run `echo $SHELL` in terminal
# 5. Do either `vim ~/.bash_profile` or `vim ~/.zshrc`
# 6. Add a line: export PATH="$HOME/.local/bin:$PATH
# 7. Now you can run `lumi' command to use Lumi Language under any path!

# Attention:
# If you are not on a Unix computer, use `lumi.bat` file instead.
# This file is for MacOS and Linux users. Thank you!

# Enjoy!
