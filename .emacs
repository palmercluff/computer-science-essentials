;; Add line numbers to all open windows
(global-linum-mode)

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
