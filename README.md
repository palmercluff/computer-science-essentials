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
- Emacs `M-x` command can be executed by: `Alt-x` or `Esc-x` or `Esc x`.
- Emacs `C-g` cancel command.
- Emacs text-mode: mark whole buffer with `C-x h` (or `M-x mark-whole-buffer`).
- Emacs copy and paste: Press `Ctrl-Space` to mark start of block. Move cursor until end of block. Press `M-w` to copy (or `C-w` to cut). Move to insert position. Press `Ctrl-y` to paste.
- Emacs set Japanese input: `Ctrl-\` then enter the string `japanese` (or `japanese-katakana`). Toggle between Japanese and English with the same command.
- For Windows, to find where root directory is, just do: `C-x C-f ~/`.
- Open a new shell buffer: `C-u M-x shell`.
- Emacs insert text (put in init file) then run using:`M-x` `insert-p-tag`:
  ```
  (defun insert-p-tag ()
    "Insert <p></p> at cursor point."
    (interactive)
    (insert "<p></p>")
    (backward-char 4))
  ```
- Emacs windows commands:
  - `C-x 2` - Split window horizontally
  - `C-x 3` - Split window vertically
  - `C-x o` - Go to different window
  - `C-x 0` - Close current window
  - `C-x 1` - Close all but current window
- Emacs buffer commands:
  - `C-x b` - Create new or select different buffer
  - `C-x C-b` - List all bufferes
  - `C-x k` - Kill buffer
  - `C-x left-arrow-key` - Back a buffer
  - `C-x right-arrow-key` - Forward a buffer
- Emacs create macro:
  - `C-x (` or `<f3>` to start recording macro
  - `C-x )` or `<f4>` to stop recording macro
  - `C-x e` or `<f4>` to execute last macro you defined
  - `M-x name-last-kbd-macro` to name last defined macro
  - `M-x insert-kbd-macro` to save named macro (copy result to .emacs file)
  - `M-x macro_name` to run your macro
- Add new user to sudo group: `usermod -a -G sudo user_account`.
- `cat /dev/null > ~/.bash_history && history -c && exit` This clears the history saved in the history file as well as the history in the current session (so that it's not saved to file when bash exits). It then exits the shell. The next shell session will have no history.
