#!/bin/bash
cd ~/Desktop/Lumi
curl -sSL https://get.haskellstack.org/ | sh
npm start
stack install
cd ~/.local/bin
./lumi

# Instruction:

# Put this file under ~/Lumi/
# 1. Revise path in 2nd line to Lumi directory path on your local machine.
# 2. Remove 3rd line if you already have haskell on your local machine.
# 3. Revise path in 6th line if necessary. If unsure, under Lumi directory, check manually using `stack install`. It will show where Lumi executable is on your machine. Copy that path here.
# 4. Make the script executable by typing `chmod +x lumi.sh` and pressing Enter.
# 5. Run `echo $SHELL` in terminal
# 6. Do either `vim ~/.bash_profile` or `vim ~/.zshrc`
# 7. Do `source ~/.bash_profile` or `source ~/.zshrc` correspondingly.
# 8. Add a line: export PATH="$HOME/.local/bin:$PATH"
# 9. Now you can run `lumi' command to use Lumi Language under any path!

# Attention:

# If you are not on a Unix computer, use `lumi.bat` file instead.
# This file is for MacOS and Linux users.
# For China-based users, since as of 24 February 2020, the download link in 3rd line has limited connectivity from within mainland China. Please review the "China-based users" section within this link: docs.haskellstack.org/en/stable/install_and_upgrade/

# Thank you for using Lumi. Enjoy! :)
