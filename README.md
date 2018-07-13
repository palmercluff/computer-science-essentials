# computer-science-essentials
Lists of packages, softwares, and other tools that every computer scientist, software engineer, web developer, or IT specialist should have.

## Languages, Compilers, and Interpreters
- Fortran
- g++
- gcc
- java jdk
- JavaScript
- lua
- Objective-C
- perl
- php
- python
- ruby

## SSH Clients
- PuTTY (Linux, Windows)

## Networking Tools
- OpenVPN
- Squid
- Wireshark

## Emulation
- QEMU
- VirtualBox

## Disk Tools
- ChkFlsh (Windows)
- furiusisomount (Linux)
- htop (Linux)
- ImgBurn (Windows)
- MagicISO (Windows)
- quota (Linux)
- quotatool (Linux)
- SDFormatter (Windows)
- TestDisk
- Win32DiskImager (Windows)
- WinCDEmu (Windows)

## Integrity Checks
- IgorWare Hasher

## Useful Commands
- `repquota -a` (Linux) Returns a detailed report of all users that have been assigned a quota. Must be root or have sudo priveledges.
- `netstat -tulpn` (Linux) Returns a list of active internet connections and their associated ports. Use `netstat -tulpn | grep LISTEN` to return of the list of listening ports.
- `apt-get install default-jre` (Linux) Installs the Java runtime environment (java).
- `apt-get install default-jdk` (Linux) Installs the Java development kit (javac).
- Emacs `M-x` command can be executed by: `Alt-x` or `Esc-x` or `Esc x`
- Emacs text-mode: mark whole buffer with `C-x h` (or `M-x mark-whole-buffer`).
- Emacs copy and paste: Press `Ctrl-Space` to mark start of block. Move cursor until end of block. Press `Alt-w` to copy. Move to insert position. Press `Ctrl-y` to paste.
- Emacs set Japanese input: `Ctrl-\` then enter the string `japanese`. Toggle between Japanese and English with the same command.
- Add new user to sudo group: `usermod -a -G sudo user_account`.
- `cat /dev/null > ~/.bash_history && history -c && exit` This clears the history saved in the history file as well as the history in the current session (so that it's not saved to file when bash exits). It then exits the shell. The next shell session will have no history.
