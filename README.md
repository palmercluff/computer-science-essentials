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
- atop (Linux)
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
- `du -sh file_path` or `du -sh *` Size of file(s).
- `df -h` size left on filesystem.
- `repquota -a` (Linux) Returns a detailed report of all users that have been assigned a quota. Must be root or have sudo priveledges.
- `netstat -tulpn` (Linux) Returns a list of active internet connections and their associated ports. Use `netstat -tulpn | grep LISTEN` to return of the list of listening ports.
- `apt-get install default-jre` (Linux) Installs the Java runtime environment (java).
- `apt-get install default-jdk` (Linux) Installs the Java development kit (javac).
- Emacs `M-x` command can be executed by: `Alt-x` or `Esc-x` or `Esc x`.
- Emacs `C-g` cancel command.
- Emacs `Esc Esc Esc` go back to just one window (by deleting all but the selected window).
- Emacs `C-z` suspends Emacs temporarily and puts it in background (do `fg %emacs` in terminal to bring it back)
- Emacs `C-h ?` or `C-h C-h` will take you to the help menu.
- Emacs `C-x C-h` will show you all keybindings that start with `C-x`.
- Emacs `M-g M-g` or `M-g g` goes to line #.
- Emacs `C-y M-y M-y M-y ...` loops through the kill ring.
- Emacs text-mode: mark whole buffer with `C-x h` (or `M-x mark-whole-buffer`).
- Emacs copy and paste: Press `Ctrl-Space` to mark start of block. Move cursor until end of block. Press `M-w` to copy (or `C-w` to cut). Move to insert position. Press `Ctrl-y` to paste.
- Emacs set Japanese input: `Ctrl-\` then enter the string `japanese` (or `japanese-katakana`). Toggle between Japanese and English with the same command.
- For Windows, to find where root directory is, just do: `C-x C-f ~/`.
- Open a new shell buffer: `C-u M-x shell`.
- Undo in Emacs:
  - `C-/`
  - `C-x u`
  - `C-_`
  - `C--` (ssh)
- Evaluating Elisp:
  - `C-x C-e` evaluates lisp code before point and prints value in the echo area
  - `C-j` evaluates lisp code before point and inserts value at point
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
- Manually install an emacs package:
  ```
  ;; Tell emacs where is your personal elisp lib dir
  (add-to-list 'load-path "~/.emacs.d/lisp/")

  ;; load the packaged named xyz.
  (load "xyz") ;; best not to include the ending ".el" or ".elc"
  ```
- Install auto-complete package in emacs:
  - `sudo apt-get install auto-complete-el`
  - In .emacs file:
    ```
    (require 'auto-complete-config)
    (require 'auto-complete)
    (ac-config-default)
    (global-auto-complete-mode t)
    ```
  - Manual can be found here: `/usr/share/doc/auto-complete-el/doc/manual.txt`
  - Source files can be found here: `/usr/share/emacs24/site-lisp/auto-complete/`
  - To use flyspell-mode with auto-complete, include: `(ac-flyspell-workaround)` in .emacs file
- Use Ditaa in org-mode for diagram-making
  - `sudo apt-get install ditaa`
  - `cp /usr/share/ditaa/ditaa.jar /usr/share/emacs/24.5/lisp/contrib/scripts/ditaa.jar`
  - In .emacs file:
    ```
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ditaa . t))) ; this line activates ditaa
    ```
  - Use `C-c C-c` to evaluate code block
    ```
    #+BEGIN_SRC ditaa :file foo.png
        +-------+
        | Hello |
        +-------+
    #+END_SRC
    ```
  - To change the default path Emacs uses for ditaa, you can use `(setq org-ditaa-jar-path "/path/to/ditaa_file.jar")`
- Turn on `flyspell` for all programming languages: `(add-hook 'prog-mode-hook 'flyspell-prog-mode)`
- Install php-mode for emacs:
  - `sudo apt-get install php-elisp`
  - In .emacs file: `(require 'php-mode)`
- Install lua-mode for emacs:
  - `sudo apt-get install lua-mode`
  - In .emacs file: `(require 'lua-mode)`
- Convert Latex to PDF when exporting to PDF in ORG mode
  - `sudo apt-get install texlive-latex-base`
  - `sudo apt-get install texlive-latex-extra`
- Emacs Encryption (Emacs 23 or later):
  - To enable file encryption when saving a .gpg file (i.e. test.org.gpg) in .emacs:
    ```
    (require 'epa-file)
    (epa-file-enable)
    ```
  - To encrypt a region: `M-x epa-encrypt-region`
  - To decrypt a region: `M-x epa-decrypt-region`
  - For windows users:
    - For EPA file/region encryption, simply download Gpg4win and it should work out of the box
    - To enable passphrase everytime when decrypting
    - Open Kleopatra -> "Settings" -> "Configure Kleopatra" -> "GnuPG System" -> "Private Keys" -> Set "Expire cached PINS after N seconds" to 0
- Use abbreviations:
  - Make a `abbrevs.el` or whatever named file and put something like this in it:
    ```
    (define-abbrev-table 'global-abbrev-table '(
    ;; Greek letters
    ("Alpha"     "Α") ;A
    ("alpha"     "α") ;a
    ("Beta"      "Β") ;B
    ("beta"      "β") ;b
    ("Gamma"     "Γ") ;G
    ("gamma"     "γ") ;g
    ("Delta"     "Δ") ;D
    ("delta"     "δ") ;d
    ("Epsilon"   "Ε") ;E
    ("epsilon"   "ε") ;e
    ("Zeta"      "Ζ") ;Z
    ("zeta"      "ζ") ;z
    ("Eta"       "Η") ;H
    ("eta"       "η") ;h
    ("Theta"     "Θ") ;Th
    ("theta"     "θ") ;th
    ("Iota"      "Ι") ;I
    ("iota"      "ι") ;i
    ("Kappa"     "Κ") ;K
    ("kappa"     "κ") ;k
    ("Lambda"    "Λ") ;L
    ("lambda"    "λ") ;l
    ("Mu"        "Μ") ;M
    ("mu"        "μ") ;m
    ("Nu"        "Ν") ;N
    ("nu"        "ν") ;n
    ("Xi"        "Ξ") ;X
    ("xi"        "ξ") ;x
    ("Omicron"   "Ο") ;O
    ("omicron"   "ο") ;o
    ("Pi"        "Π") ;P
    ("pi"        "π") ;p
    ("Rho"       "Ρ") ;R
    ("rho"       "ρ") ;r
    ("Sigma"     "Σ") ;S
    ("sigma"     "σ") ;s
    ("Tau"       "Τ") ;T
    ("tau"       "τ") ;t
    ("Upsilon"   "Υ") ;U
    ("upsilon"   "υ") ;u
    ("Phi"       "Φ") ;Ph
    ("phi"       "φ") ;ph
    ("Chi"       "Χ") ;Ch
    ("chi"       "χ") ;ch
    ("Psi"       "Ψ") ;Ps
    ("psi"       "ψ") ;ps
    ("Omega"     "Ω") ;O
    ("omega"     "ω") ;o

    ;; Temperatures
    ("degrees" "" degrees)
    ("C"         "Celsius")
    ("F"         "Fahrenheit")

    ;; Other
    ("inf"       "∞") ;Infinity
    ("nospace"   "no space after expansion" dont-add-space)
    ))

    (defun degrees()
      (backward-char)
    (insert "°"))

    (defun dont-add-space()
      t)
    (put 'dont-add-space 'no-self-insert t)

    ```
  - Then in .emacs put:
    ```
    (setq abbrev-file-name       ;; tell emacs where to read abbrev
          "~/abbrevs.el")        ;; definitions from...
    (setq save-abbrevs t)        ;; (ask) save abbrevs when files are saved
    (setq-default abbrev-mode t) ;; turn it on for all modes
    ```
  - Then whenever you type the key, then space or . it will convert
  - `C-q` prevents word from expanding if you do it before space or punctuation
- Evil Mode
  - In .emacs:
    ```
    (add-to-list 'load-path "~/evil")
    (require 'evil)
    (evil-mode 1)
    (setq evil-default-state 'emacs) ;; changes default state to emacs
    ```
  - When using evil-mode, `C-z` is used by default to switch between Vi and Emacs modes
  - As an alternative to EVIL, Emacs has `viper-mode` installed by default
- NeoTree
  - In .emacs:
    ```
    (add-to-list 'load-path "~/.emacs.d/lisp/emacs-neotree-master/emacs-neotree-master")
    (require 'neotree)
    (global-set-key [f8] 'neotree-toggle)
    ```
- Sublimity
  - In .emacs:
    ```
    (add-to-list 'load-path "~/.emacs.d/lisp/sublimity-master/sublimity-master")
    (require 'sublimity)
    (require 'sublimity-map)
    (sublimity-mode 1)
    ```
- Minimap
  - In .emacs:
    ```
    (add-to-list 'load-path "~/.emacs.d/lisp/minimap-master/minimap-master")
    (require 'minimap)
    ```
- Access Org manual from within Emacs
    1. Start the info system `C-h i`
    2. Open the menu `m`
    3. Enter `org <RET>`

  Or with: `M-x org-info`

  Navigating within documentation
    - `n` - Go forward, but only within the level you are in (will not go to next section)
    - `]` - Go forward and go into next section if you have finished the level

    - `p` - Go backward, but only within the level you are in (will not go to previous section)
    - `[` - Go backward and go into previous section if you have finished the level

    - `^` - Go up a level
- Ispell
  - `M-$` Check and correct spelling of the word at point (`ispell-word`). If the region is active, do it for all words in the region instead
  - `M-x ispell` Check and correct spelling of all words in the buffer. If the region is active, do it for all words in the region instead
  - While changing a word, pressing `<space>` will skip the word, but will still consider it incorrect for now
  - While changing a word, pressing `i` will insert that selected word into your own personal dictionary file. It appears that the dictionary file is called `.aspell.en.pws` on Debian-based systems
  - For Windows users
    - In .emacs:
      ```
      ;; Getting spell checker to work under newer versions of Emacs on Windows
      ;; (aspell is pretty old and v0.50 wont work on Emacs v26+) (add this to github later)
      (add-to-list 'exec-path "~/.emacs.d/binaries/hunspell-1.3.2-3-w32-bin/bin/") ;Path to where binary is located (no install required)
      (setq ispell-program-name "hunspell") ;Personal dictionary file will probably be saved as "hunspell_default"
      ```
- Add new user to sudo group: `usermod -a -G sudo user_account`.
- `cat /dev/null > ~/.bash_history && history -c && exit` This clears the history saved in the history file as well as the history in the current session (so that it's not saved to file when bash exits). It then exits the shell. The next shell session will have no history.
