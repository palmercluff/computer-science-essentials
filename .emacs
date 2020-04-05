;; Disable startup screen
(setq inhibit-startup-screen t)

;; For older versions of Emacs (24 and below (I think)), use:
(setq inhibit-splash-screen t)   ;; Alias for inhibit-startup-screen
(setq inhibit-startup-message t) ;; Alias for inhibit-startup-screen

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Set default starting directory
(setq default-directory "C:/")

;; Replace highlighted text when you type
(delete-selection-mode 1)

;; Load theme
(load-theme 'manoj-dark)

;; Automatically open .emacs file with <f5>
(global-set-key (kbd "<f5>") (lambda() (interactive)(find-file "~/.emacs")))

;; Add line numbers to all open windows
(global-linum-mode)

;; The following adds a space after the line-number
(setq linum-format "%d ")

;; You can also add a solid line separator
(setq linum-format "%4d \u2502 ")

;; Wrap lines at the word border
(global-visual-line-mode t)

;; Show column number
(column-number-mode 1)

;; Disable warning bell
(setq ring-bell-function 'ignore)

;; Or change bell sound with this (supports .wav and .au files)
(setq ring-bell-function (lambda ()
			                     (play-sound-file "/path/to/sound.wav")))

;; Prevent re-centering when going up and down document with arrow-keys
(setq scroll-conservatively 101)

;; Auto close bracket insertion. New in emacs 24. Includes brackets, parenthesis, etc
(electric-pair-mode 1)

;; Create closing tags automatically
(defun my-sgml-insert-gt ()
  "Inserts a '>' character and calls 'my-sgml-close-tag-if-necessary', leaving point where it is."
  (interactive)
  (insert ">")
  (save-excursion (my-sgml-close-tag-if-necessary)))

(defun my-sgml-close-tag-if-necessary ()
  "Calls sgml-close-tag if the tag immediately before point is
an opening tag that is not followed by a matching closing tag."
  (when (looking-back "<\\s-*\\([^</> \t\r\n]+\\)[^</>]*>")
    (let ((tag (match-string 1)))
      (unless (and (not (sgml-unclosed-tag-p tag))
		   (looking-at (concat "\\s-*<\\s-*/\\s-*" tag "\\s-*>")))
	(sgml-close-tag)))))

(eval-after-load "sgml-mode"
  '(define-key sgml-mode-map ">" 'my-sgml-insert-gt))
;; End closing tags automatically

;; Enable terminal mouse
(xterm-mouse-mode 1)

;; Set custom global key binding that calls a function
(global-set-key (kbd "C-c C-c") 'my-function)

;; Enable the display of time in the mode line
(display-time-mode 1)

;; Turn off confirmation requests for all code blocks
(setq org-confirm-babel-evaluate nil)

;; Turn off confirmation requests for certain languages
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("python" "sh"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
;; End Turn off confirmation requests for certain languages

;; Make external command and insert output into current buffer
;; Start
(defun put-the-date ()
  (interactive)
  (insert (shell-command-to-string "date")))

(global-set-key
 (kbd "C-c C-d")
 'put-the-date
 )
;; Stop

;; Or use an anonymous function instead
(global-set-key (kbd "C-c C-d") (lambda () (interactive) (insert (shell-command-to-string "date"))))
;; This is the equivelent of basically doing C-u M-! date. M-! date will do the same, but use a different buffer for output

;; Start interactive and prompt examples
(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(defun prompt-user-n-times (n)
  "Prompt user n times for strings and append strings to list"
  (interactive "nHow many strings: ")
  (let ((newlist ()))
    (while (> n 0)
      (setq newlist (append newlist (list (read-string "Give me input: "))))
      (setq n (- n 1)))
    (print-elements-of-list newlist)
    newlist))

;; Doesn't show up with M-x! But can be called like (say-hello)
(defun say-hello ()
  (message "Hello %s" (user-full-name)))

;; Now M-x works, and can still be called like (say-hello2)
(defun say-hello2 ()
  (interactive)
  (message "Hello %s" (user-full-name)))

;; You can still invoke this with (say-hello-to-me "Palmer" 7), or just call it with M-x
(defun say-hello-to-me (username favorite-number)
  (interactive "sWhat's your name?
nWhat's your favorite number? ")
  (message "Why, hello there %s! Your favorite number times 7 is %d"
           username
           (* 7 favorite-number)))

;; Chose a number and display string
(defvar valeriy-alist '((?1 "Cow" (lambda () (message "I am a cow") 'cow))
                        (?2 "Rabbit" (lambda () (message "I am a rabbit") 'rabbit))
                        (?3 "Dog" (lambda () (message "I am a dog") 'dog)))
  "List that associates number letters to descriptions and actions.")

(defun valeriy-choose ()
  "Lets the user choose the animal and takes the corresponding action.
Returns whatever the action returns."
  (interactive)
  (let ((choice (read-char-choice (mapconcat (lambda (item) (format "%c: %s" (car item) (cadr item))) valeriy-alist "; ")
                                  (mapcar #'car valeriy-alist))))
    (funcall (nth 2 (assoc choice valeriy-alist)))))

;; Another way to do it
(defun choice1 ()
  (let ((completions '(("1" "Cow") ("2" "Rabbit") ("3" "Dog"))))
    (cadr (assoc (completing-read "Choose: " completions) completions))))

;; Another way
(defun choice2 ()
  (let ((lst '(("1" . "Cow") ("2" . "Rabbit") ("3" . "Dog")))
        (select nil))
    (setq select (completing-read "Choose: " lst nil t ""))
    (cdr (assoc select lst))))

;; Basic autocomplete options
(defun choice3 (choice)
  "..."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Choose: " '("Cow" "Rabbit" "Dog") nil t))))
  (message "You chose `%s'" choice)
  choice)

;; Query user for yes/no
(defun yesorno ()
  (if (y-or-n-p "Do it?")
      (progn
        ;; code to do something here
        (message "You chose yes")
        )
    (progn
      ;; code if user answered no.
      (message "You chose no")
      )
    ))
;; End interactive and prompt examples

;; Save all generated backups in one place
(setq backup-directory-alist '(("." . "~/emacsFilesBackups")))

;; Backup remotely edited files on SERVER when using TRAMP
(setq tramp-backup-directory-alist '(("." . "~/trampEmacsFilesBackups")))

;; DON'T backup any files on the SERVER if accessed through TRAMP
;; (but only if backup-directory-alist is enabled as well (as seen above), otherwise default emacs backups will occur on the server)
;; (If both backup-directory-alist is set and tramp-backup-directory-alist is null, then remote backups will be saved on the local machine)
(setq tramp-backup-directory-alist nil)

;; Disk space is cheap, backup the same file(s) very often!
(setq version-control t)      ;; Use version numbers for backups
(setq kept-new-versions 10)   ;; Number of newest versions to keep
(setq kept-old-versions 0)    ;; Number of oldest versions to keep
(setq delete-old-versions t)  ;; Don't ask to delete excess backup versions
(setq backup-by-copying t)    ;; Copy all files, don't rename them
(setq vc-make-backup-files t) ;; Backup versioned files

;; Backup on every save, not just a buffer session
(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))

(add-hook 'before-save-hook  'force-backup-of-buffer)

;; Or don't create any backups whatsoever
(setq make-backup-files nil)

;; Save various histories (i.e. minibuffer, kill ring, searches, etc)
(setq kill-ring-max 100)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

;; New HTML5 Template
(defun newHTML5Template ()
  "Insert a template for an empty HTML page"
  (interactive)
  (insert "<!DOCTYPE html>\n"
          "<html>\n"
          "  <head>\n"
          "    <title></title>\n"
          "  </head>\n"
          "  <body>\n"
          "  </body>\n"
          "</html>\n")
  (forward-line -5)
  (forward-char 11)
  )

;; Use new HTML template if the file is empty
(add-hook 'html-mode-hook
          (lambda ()
            (if (= (buffer-size) 0)
                (progn
                  (newHTML5Template)
                  (message "Used HTML template"))
              (message "Did not use HTML template"))
            ))

;; If you press C-z by mistake then you can add this to your .emacs to make C-z do nothing
(global-unset-key (kbd "C-z"))

;; Example Org project + export
;;
;; Publish with (org-publish-current-project) while having a file in your project open
;; OR
;; M-x org-publish-project RET project-name RET
;;
;; Force publish cached files: M-: (org-publish "project-name" t)
;; OR
;; C-u M-x org-publish
(setq org-publish-project-alist
      '(("Example Org project"
         :base-directory "~/example_org_project"
         :recursive "t"
         :publishing-function org-html-publish-to-html
         :publishing-directory "/var/www/html/exported_example_org_project")))

;; Skeletons with abbreviation
(define-skeleton hello-world-skeleton
  "Write a greeting"
  "Type name of user: "
  "hello, " str "!")

(define-abbrev global-abbrev-table "hws" "" 'hello-world-skeleton)

;; Ask for repeated input until an empty string is entered
(define-skeleton hello-class
  "Example for repeated input."
  "this prompt is ignored"
  ("Enter name of student: " "hello, " str \n))

(define-skeleton add-tags
  "Enter tags in any case and the output will be upcased."
  nil
  "Tags: "
  ((upcase (skeleton-read "Tag: ")) ":"  str) ":" \n)

(define-skeleton skeleton-skel
  "Interactive skeleton for writing skeletons."
  > "(define-skeleton " (skeleton-read "Skeleton name: ") \n
  > "\"" (skeleton-read "Docstring: ") "\""
  > ("Content line: " \n str) _ ")")

(define-skeleton read-two-vars
  "Prompt the user for two variables, and use them in a skeleton."
  ""
  > "variable A is " (setq v1 (skeleton-read "Variable A? ")) \n
  > "variable B is " (setq v2 (skeleton-read "Variable B? ")) \n
  > "A: " v1 "    B: " v2 \n)

(define-skeleton vote
  "Electoral motion results"
  nil
  > "|----------------+----------+--------------|" \n
  > "|Vote:           |For: "
  (setq v1 (skeleton-read "How many for? "))
  "    |Against: "
  (setq v2 (skeleton-read "How many against? "))
  "    |" \n
  > "|----------------+----------+--------------|" \n
  "|" (if (< (string-to-number v1)(string-to-number v2))
	  "Not Carried                               |"
	"Carried                                    |")
  \n
  > "|------------------------------------------|" \n)

(define-skeleton sexpr-example
  "Insert a silly example."
  ""
  > "Emacs version is: " emacs-version \n
  > "And time is: " (current-time-string))

(define-skeleton insert-c-comment-header
  "Inserts a c comment in a rectangle into current buffer."
  ""
  '(setq str (skeleton-read "Comment: "))
  ;; `str' is set explicitly here, because otherwise the skeleton
  ;; program would set it, only when it is going to insert it into the
  ;; buffer. But we need to determine the length of the string
  ;; beforehand, with `(length str)' below.
  '(when (string= str "") (setq str " - "))
  '(setq v1 (make-string (- fill-column 6) ?*))
  '(setq v2 (- fill-column 10 (length str)))
  "/* " v1 " */" \n
  "/* **"
  (make-string (floor v2 2) ?\ )
  str
  (make-string (ceiling v2 2) ?\ )
  "** */" \n
  "/* " v1 " */")

(define-skeleton xhtml-trans-skeleton
  "Inserts a skeletal XHTML file with the DOCTYPE declaration
    for the XHTML 1.0 Transitional DTD"
  "Title: "
  "<?xml version=\"1.0\""
  (if buffer-file-coding-system
      (concat " encoding=\""
	      (setq v1
		    (symbol-name
		     (coding-system-get buffer-file-coding-system
                                        'mime-charset))) "\""))
  "?>\n"
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n"
  > "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
  > "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
  > "<head>\n"
  (when buffer-file-coding-system
    (indent-according-to-mode)
    (concat
     "<meta http-equiv=\"Content-type\" content=\"text/html; charset="
     v1 "\" />\n"))
  > "<meta name=\"Author\" content=\"" (user-full-name) "\" />\n"
  > "<title>" - str
  "</title>\n"
  "</head>" > \n
  > "<body>\n"
  > "<h1>" - str
  "</h1>\n"
  > - \n
  "</body>" > \n
  "</html>" > \n \n)

(define-skeleton html-redirect-page
  "Inserts an Web page that can redirect to STR."
  "URL: "
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
  "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
  "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
  "<head>\n"
  "<meta http-equiv=\"Refresh\" content=\"1;url=" str "\" />\n" 
  "<title>Redirecting to " (setq v1 (skeleton-read "Title: ")) "</title>\n"
  "</head>\n"
  "<body>\n"
  "<p>Redirecting to\n"
  "<a href=\"" str "\">" v1 "</a>.\n"
  "&hellip;</p>\n"
  "</body>\n"
  "</html>")

(define-skeleton skel-defun
  "Insert a defun template."
  "Name: "
  "(defun " str " (" @ - ")" \n
  "(" @ _ ")" \n)

;; Simply bind skeleton-next-position to some convenient key and you'll be able to cycle through the completion points within the template (The template directly above).
(defvar *skeleton-markers* nil
  "Markers for locations saved in skeleton-positions")

(add-hook 'skeleton-end-hook 'skeleton-make-markers)

(defun skeleton-make-markers ()
  (while *skeleton-markers*
    (set-marker (pop *skeleton-markers*) nil))
  (setq *skeleton-markers*
	(mapcar 'copy-marker (reverse skeleton-positions))))

(defun skeleton-next-position (&optional reverse)
  "Jump to next position in skeleton.
         REVERSE - Jump to previous position in skeleton"
  (interactive "P")
  (let* ((positions (mapcar 'marker-position *skeleton-markers*))
	 (positions (if reverse (reverse positions) positions))
	 (comp (if reverse '> '<))
	 pos)
    (when positions
      (if (catch 'break
	    (while (setq pos (pop positions))
	      (when (funcall comp (point) pos)
		(throw 'break t))))
	  (goto-char pos)
	(goto-char (marker-position
		    (car *skeleton-markers*)))))))
;; More docs and examples can be found in skeleton.el and sh-script.el

;; To delete IDO completion history, delete ido.last file
;; To change where ido.last is saved, change ido-save-directory-list-file (setq ido-save-directory-list-file "/some/file/name")
;; C-l - Re-reads contents of directory in minibuffer
;; C-f - Fall back to find-file
;; C-j - Create a new file with the text you typed, not the closest auto-completed match if it exists
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Spacemacs themes
(add-to-list 'load-path "~/.emacs.d/lisp/spacemacs-theme-master/spacemacs-theme-master")
(require 'spacemacs-common)
(load-theme 'spacemacs-dark t)

;; Doom themes
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-doom-themes-master/emacs-doom-themes-master")
(require 'doom-themes)
(load-theme 'doom-one t)

;; which-key
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-which-key-master/emacs-which-key-master")
(require 'which-key)
(which-key-mode)
;; While which-key is toggled, you can press ? instead of C-h for pagination and such

;; EVIL leader keys

;; Evil-leader
(add-to-list 'load-path "~/.emacs.d/lisp/evil-leader-master/evil-leader-master")
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer)

;; General.el (Use SPC in normal and visual states, and use M-SPC in emacs and insert state)
(add-to-list 'load-path "~/.emacs.d/lisp/general.el-master/general.el-master")
(require 'general)
(general-define-key
 :keymaps '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "e" 'find-file
 "b" 'switch-to-buffer
 "k" 'kill-buffer)

;; Save cursor position

;; Emacs 25.1 and above
(save-place-mode 1)

;; Emacs 24.5 and below
(require 'saveplace)
(setq-default save-place t)

;; Keyboard-driven solution for mc/add-cursor-on-click
(require 'multiple-cursors)

(defun mc/toggle-cursor-at-point ()
  "Add or remove a cursor at point."
  (interactive)
  (if multiple-cursors-mode
      (message "Cannot toggle cursor at point while `multiple-cursors-mode' is active.")
    (let ((existing (mc/fake-cursor-at-point)))
      (if existing
          (mc/remove-fake-cursor existing)
        (mc/create-fake-cursor-at-point)))))

(add-to-list 'mc/cmds-to-run-once 'mc/toggle-cursor-at-point)
(add-to-list 'mc/cmds-to-run-once 'multiple-cursors-mode)

(global-set-key (kbd "C-S-SPC") 'mc/toggle-cursor-at-point)
(global-set-key (kbd "<C-S-return>") 'multiple-cursors-mode)

;; For external programs you need to run as async (as well as add custom commands to Eshell)

;; To disable popup Async Shell Command window for when using (shell-command "putty.exe&") or (async-shell-command "putty.exe")
;;(add-to-list 'display-buffer-alist
;;	     (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))
(defsubst eshell/putty ()
  (interactive)
  ;;(shell-command "putty.exe&")
  (call-process-shell-command "putty" nil 0)
  ;;(async-shell-command "putty.exe")
  )
