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

;; Enable the display of time in the modeline
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

;; Disk space is cheap, backup the same file(s) very often!
(setq version-control t)      ;; Use version numbers for backups
(setq kept-new-versions 10)   ;; Number of newest versions to keep
(setq kept-old-versions 0)    ;; Number of oldest versions to keep
(setq delete-old-versions t)  ;; Don't ask to delete excess backup versions
(setq backup-by-copying t)    ;; Copy all files, don't rename them
(setq vc-make-backup-files t) ;; Backup versioned files

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
