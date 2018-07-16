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
