;; Run command in *shell* buffer. Unlike "M-x compile", we can see the compile messages in the *shell* buffer.
;; http://stackoverflow.com/questions/6286579/emacs-shell-mode-how-to-send-region-to-shell
(defun shell-send-command (command display_command)
  "Send and run command in shell buffer
   If you set display_command argument to true, it prints the command in the shell buffer"
  (setq shproc (get-process "shell"))

  ;; Start *shell* if it is not running
  (unless shproc
    (let ((currbuff (current-buffer)))
      (shell)
      (switch-to-buffer currbuff)
      (setq shproc (get-process "shell"))
      ))

  (setq shbuff (process-buffer shproc))

  (setq cmd_n (concat command "\n"))

  ;; Send string of command for sake of display
  (when display_command
    (with-current-buffer shbuff
      (goto-char (process-mark shproc))
      (insert cmd_n)
      (move-marker (process-mark shproc) (point))
      (insert "hoge")
      ))

  (process-send-string  shproc cmd_n ) ;run command

  (display-buffer (process-buffer shproc)))

(defun shell-send-cd-curdir (display_command)
  ;; get directory of buffer
  (defun current-dir-path ()
    (defun rm_last (l)
      (reverse (cdr (reverse l))))
    (defun fold_right (f l init)
      (if (null l)
          init
        (funcall f (car l) ( fold_right f (cdr l) init ))))
    (setq file_path (buffer-file-name (current-buffer)))
    (fold_right
     '(lambda (s1 s2) (concat s1 (concat "/" s2)) )
     (rm_last (split-string file_path "/"))
     "" ))

    (shell-send-command (concat "cd " (current-dir-path)) display_command)

  ;; Resync the buffer's idea of the current directory stack
  (setq shproc (get-process "shell"))
  (setq shbuff (process-buffer shproc))
  (with-current-buffer shbuff
    ;; For some reason, sync only happens when I do it twice...
    (shell-resync-dirs)
    (shell-resync-dirs))
  )

(defun shell-send-cd ()
    (interactive ())
    (shell-send-cd-curdir t))

(defun shell-send-make ()
  (interactive ())
  (shell-send-command "make" t))
