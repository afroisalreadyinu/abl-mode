;;; abl-mode.el --- Python TDD minor mode

;;
;; Author: Ulas Tuerkmen <ulas.tuerkmen at gmail dot com>
;; URL: http://github.com/afroisalreadyinu/abl-mode
;; Version: 0.9.0
;;
;; Copyright (C) 2011 Ulas Tuerkmen
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; The aim of this mode is to make editing Python code in a
;; version-controlled project easier, and enable the execution of
;; repetitive tasks --such as running tests or scripts-- in emacs
;; shell buffers. Please see README.rst for details.

;; <<--------- The necessary minor-mode stuff  ---------->>

(defvar abl-mode nil
  "Mode variable for abl-mode")
(make-variable-buffer-local 'abl-mode)

(defun abl-mode (&optional arg)
  "abl minor mode"
  (interactive "P")
  (setq abl-mode (if (null arg) (not abl-mode)
		   (> (prefix-numeric-value arg) 0)))
  (if abl-mode
      (let ((project-base (abl-mode-find-base-dir)))
	(if (not project-base)
	    (setq abl-mode nil)
	  (setq abl-mode-branch-base project-base)
	  (setq abl-mode-branch (abl-mode-branch-name abl-mode-branch-base))
	  (setq abl-mode-project-name (abl-mode-get-project-name abl-mode-branch-base))
	  (setq abl-mode-vem-name (abl-mode-get-vem-name))
	  (setq abl-mode-shell-name (abl-mode-shell-name-for-branch
				abl-mode-project-name
				abl-mode-branch))))))

(defun abl-mode-hook ()
  (abl-mode))

(if (not (assq 'abl-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(abl-mode " abl-mode")
		minor-mode-alist)))

(defvar abl-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t") 'abl-mode-run-test-at-point)
    (define-key map (kbd "C-c u") 'abl-mode-rerun-last-test)
    (define-key map (kbd "C-c o") 'abl-mode-open-python-path-at-point)
    (define-key map (kbd "C-c w") 'abl-mode-display-branch)
    (define-key map (kbd "C-c s") 'abl-mode-start-vem-python)
    map)
  "The keymap for abl-mode")

(or (assoc 'abl-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'abl-mode abl-mode-keymap)
                minor-mode-map-alist)))

;; <<------------  Customization options  -------------->>

(defcustom abl-mode-vem-activate-command "workon %s"
  "The command for activating a virtual environment")

(defcustom abl-mode-vem-create-command "mkvirtualenv %s"
  "The command for activating a virtual environment")

(defcustom abl-mode-test-command "nosetests -s %s"
  "The command for running tests")

(defcustom abl-mode-branch-shell-prefix "ABL-SHELL:"
  "Prefix for the shell buffers opened")

(defcustom abl-mode-vems-base-dir "~/.virtualenvs"
  "base directory for virtual environments")

(defcustom abl-mode-install-command "python setup.py develop"
  "The command to install a package.")

;; <<----------------  Here ends the customization -------------->>

(defvar abl-mode-branch-base ""
  "Base directory of the current branch")
(make-variable-buffer-local 'abl-mode-branch-base)

(defvar abl-mode-vem-name ""
  "Name of the virtual env")
(make-variable-buffer-local 'abl-mode-vem-name)

(defvar abl-mode-etags-command-base "find %s -name '*.py' -print | etags - -o %s/TAGS"
  "command run to create a tags file for emacs")

(defvar abl-mode-branch "master"
  "The branch you are working on.When abl-mode is started, it is
  set to the name of the directory in which you are for svn, the
  git branch if you're on git.")
(make-variable-buffer-local 'abl-mode-branch)

(defvar abl-mode-shell-name "ABL-SHELL")
(make-variable-buffer-local 'abl-mode-shell-name)

(defvar abl-mode-project-name "web"
  "The name of the project. ")
(make-variable-buffer-local 'abl-mode-project-name)

(defvar abl-mode-last-test-run nil
  "Last test run and which branch it was")

(defvar abl-mode-replacement-vems '())

;; <<------------- Helpers  ------------->>

(defun abl-mode-starts-with (str1 str2)
  (and (> (length str1) 0)
       (string= str2
		(substring str1 0 (length str2)))))

(defun abl-mode-ends-with (str1 str2)
  (let ((str1-length (length str1)))
    (and (> str1-length 0)
	 (string= (substring str1 (- str1-length (length str2)) str1-length) str2))))

(defun abl-mode-remove-last (lst)
  (if (not (cdr lst))
      '()
    (cons (car lst) (abl-mode-remove-last (cdr lst)))))

(defun abl-mode-index-of (substr str1)
  (cond ((< (length str1) (length substr)) nil)
	((string= substr (substring str1 0 (length substr))) 0)
	(t (let ((rest-return (abl-mode-index-of substr (substring str1 1 (length str1)))))
	     (if (null rest-return) nil
	       (+ rest-return 1))))))

(defun abl-mode-concat-paths (base &rest paths)
  "join a list of path components into a path"
  (if (equal paths '())
      base
    (apply 'abl-mode-concat-paths
	   (concat (file-name-as-directory base) (car paths))
	   (cdr paths))))

(defun abl-mode-remove-last-slash (path)
  (if (abl-mode-ends-with path "/")
      (substring path 0 (- (length path) 1))
    path))

(defun abl-mode-higher-dir (path)
  "Return one higher directory of a given path"
  (assert (abl-mode-starts-with path "/"))
  (if (string-equal "/" path)
      nil
    (let* ((true-path (abl-mode-remove-last-slash path))
	   (components (split-string true-path "/" )))
      (apply 'abl-mode-concat-paths
	     (concat "/" (car components))
	     (abl-mode-remove-last (cdr components))))))

(defun abl-mode-last-path-comp (path)
  "Get the last path components, whether it's a file name or directory"
  (and (< 0 (length path))
       (car (last (split-string (abl-mode-remove-last-slash path) "/")))))

(defun abl-mode-find-base-dir (&optional dir-path)
  (let* ((path (or dir-path (buffer-file-name))))
    (if (and (file-exists-p (abl-mode-concat-paths path "setup.py"))
	     (not (file-exists-p (abl-mode-concat-paths path "__init__.py"))))
	path
      (let ((higher (abl-mode-higher-dir path)))
	(if (not higher)
	    nil
	  (abl-mode-find-base-dir higher))))))


(defun abl-mode-string-in-buffer (string)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward string nil t)
	t
      nil)))

(defun abl-mode-join-string (string-list joiner)
  (cond ((not (cdr string-list)) (car string-list))
	 (t (concat
	     (car string-list)
	     joiner
	     (abl-mode-join-string (cdr string-list) joiner)))))

(defun abl-mode-starts-uppercase? (strng)
  (let ((y (substring strng 0 1))) (string= y (upcase y))))



;; ------------------------------------

(defun abl-mode-git-or-svn (base-dir)
  (cond ((file-exists-p (abl-mode-concat-paths base-dir ".git")) "git")
	((file-exists-p (abl-mode-concat-paths base-dir ".svn")) "svn")
	(t nil)))

(defun abl-mode-set-config (name value)
  (setq (intern name) value))

(defun parse-abl-options (file-path)
  (let ((config-lines (with-temp-buffer
			(insert-file-contents file-path)
			(split-string (buffer-string) "\n" t))))
    (loop for config-line in config-lines
	  do (apply 'abl-mode-set-config (split-string config-line)))))


(defun abl-mode-local-options (base-dir)
  (let ((file-path (abl-mode-concat-paths base-dir ".abl")))
    (if (file-exists-p file-path)
	 (parse-abl-options file-path)
      nil)))

(defun abl-mode-get-git-branch-name (base-dir)
  (let* ((command (concat "cd " base-dir " && git branch"))
	(git-output (shell-command-to-string command)))
    (if (string-equal git-output "")
	(progn
	  (message "Looks like your git repository is empty (the output of git branch was empty). Calling it 'none'.")
	  "none")
      (string-match "\\* \\(.*\\)" git-output)
      (substring git-output (match-beginning 1) (match-end 1)))))


(defun abl-mode-branch-name (path)
  (if (string= path "/")
      nil
    (let ((vcs (abl-mode-git-or-svn path)))
      (cond ((or (not vcs) (string-equal vcs "svn"))
	     (abl-mode-last-path-comp path))
	    ((string-equal vcs "git")
	     (abl-mode-get-git-branch-name path))
	    (t nil)))))


(defun abl-mode-get-project-name (path)
  "Returns the name of the project; higher directory for no vcs or svn,
   directory name for git."
  (if (string= path "/")
      nil
    (let ((vcs (abl-mode-git-or-svn path)))
      (cond ((or (not vcs) (string-equal vcs "svn"))
	     (abl-mode-last-path-comp (abl-mode-higher-dir path)))
	    ((string-equal vcs "git")
	     (abl-mode-last-path-comp path))
	    (t nil)))))

(defun abl-mode-get-vem-name (&optional branch project)
  (let ((branch-name (or branch abl-mode-branch))
	(prjct-name (or project abl-mode-project-name)))
    (or
     (cdr (assoc branch-name abl-mode-replacement-vems))
     (concat prjct-name "_" branch-name))))

;;<< ---------------  Shell stuff  ----------------->>

(defun abl-mode-shell-name-for-branch (project-name branch-name)
  (concat abl-mode-branch-shell-prefix project-name "_" branch-name))

(defun abl-mode-shell-busy (&optional shell-buffer)
  (let* ((real-buffer (or shell-buffer (current-buffer)))
	 (shell-process-id (process-id (get-buffer-process real-buffer)))
	 (command (format "ps --ppid %d  h | wc -l" shell-process-id))
	 (output (shell-command-to-string command)))
    (/= (string-to-number output) 0)))

(defun abl-shell-busy ()
  (let ((abl-shell-buffer (get-buffer abl-mode-shell-name)))
    (if (not abl-shell-buffer)
	nil
      (abl-mode-shell-busy abl-shell-buffer))))

(defun abl-mode-exec-command (command)
  "This function should be used from inside a non-shell buffer"
  (let* ((new-or-name (abl-mode-vem-name-or-create abl-mode-vem-name))
	 (create-vem (cdr new-or-name))
	 (new-vem-name (car new-or-name))
	 (commands (if create-vem (list
				   (concat "cd " abl-mode-branch-base)
				   (format abl-mode-vem-create-command new-vem-name)
				   (format abl-mode-vem-activate-command new-vem-name)
				   abl-mode-install-command
				   command)
		     (list
		      (concat "cd " abl-mode-branch-base)
		      (format abl-mode-vem-activate-command new-vem-name)
		      command)))
	 (shell-name abl-mode-shell-name)
	 (open-shell-buffer (get-buffer shell-name))
	 (open-shell-window (if open-shell-buffer
				(get-buffer-window-list shell-name nil t)
			      nil))
	 (code-window (selected-window)))

    (if open-shell-window
	(select-window (car open-shell-window))
      (if open-shell-buffer
	  (switch-to-buffer open-shell-buffer)
	(shell shell-name)
	(sleep-for 2)))
    (abl-mode-run-command (abl-mode-join-string commands " && "))
    (select-window code-window)))


(defun abl-mode-run-command (command)
  "This function should be used when inside a shell"
  (goto-char (point-max))
  (insert command)
  (comint-send-input))

(defun abl-mode-vem-name-or-create (name)
  (let ((replacement-vem (cdr (assoc name abl-mode-replacement-vems))))
    (if replacement-vem
	(cons replacement-vem nil)
      (let ((vem-path (expand-file-name name abl-mode-vems-base-dir)))
	(if (file-exists-p vem-path)
	    (cons name nil)
	  (let*
	      ((command-string (format "No vem %s; y to create it, or name of existing to use instead: "
				       name))
	       (vem-or-y (read-from-minibuffer command-string))
	       (create-new (or (string-equal vem-or-y "y") (string-equal vem-or-y "Y"))))
	    (if create-new
		(cons name create-new)
	      (abl-mode-vem-name-or-create vem-or-y))))))))

;; <<------------  Running the server and tests  -------->>

(defun abl-mode-determine-test-function-name ()
  (save-excursion
    (end-of-line)
    (if (not (re-search-backward "^ *def test_*" nil t))
	(error "Looks like you are not even in a function definiton! Bad girl!"))
    (let* ((start (re-search-forward "^ *def *"))
	   (end (re-search-forward "test_[^\(]*" (line-end-position) t)))
      (if (not end)
	  (error "Looks like you are not inside a test function. Go to a test function! Now!")
	(buffer-substring-no-properties start (point))))))


(defun abl-mode-determine-test-class-name ()
  (save-excursion
    (if (not (re-search-backward "^class *" nil t))
	(error "Looks like there is a problem with your python code (functions is indented
but not in a class). Sorry, can't do anything")
    (let* ((start (re-search-forward "^class *"))
	   (end (re-search-forward "[^\(:]*" (line-end-position) t)))
      (if (not end)
	  (error "Looks like there is a problem with you python code (keyword class not
followed by a proper class name).")
	(buffer-substring-no-properties start (point)))))))


;;this function assumes that you are already in a test function (see
;;the function above)
(defun abl-mode-test-in-class ()
  (save-excursion
    (end-of-line)
    (let* ((start (re-search-backward "^ *def *"))
	   (end (re-search-forward "[^ ]")))
      (> (- end start 1) 0))))


(defun abl-mode-get-test-file-path ()
  (let ((buffer-name (buffer-file-name)))
    (if (not (abl-mode-ends-with buffer-name ".py"))
	(error "You do not appear to be in a python file. Now open a python file!"))
    (let ((relative-path (substring
			  buffer-file-name
			  (+ (length abl-mode-branch-base) 1)
			  (- (length buffer-name) 3))))
      (replace-regexp-in-string "/" "." relative-path))))


(defun abl-mode-get-test-function-path (file-path)
  (let ((function-name (abl-mode-determine-test-function-name)))
    (if (not (abl-mode-test-in-class))
	(concat file-path ":" function-name)
      (let ((class-name (abl-mode-determine-test-class-name)))
	(concat file-path ":" class-name "." function-name)))))


(defun abl-mode-run-test (test-path &optional branch-name)
  (if (abl-shell-busy)
      (message "The shell is busy; please end the process before running a test")
    (let* ((shell-command (format abl-mode-test-command test-path))
	   (real-branch-name (or branch-name abl-mode-branch)))
      (message (format "Running test(s) %s on branch %s" test-path real-branch-name))
      (abl-mode-exec-command shell-command)
      (setq abl-mode-last-test-run (cons test-path abl-mode-branch)))))


;This returns the python destination on point, depending on
;whether it is a test function, class, or whole file
(defun abl-mode-get-test-entity ()
  (let ((file-path (abl-mode-get-test-file-path)))
    (if (= (line-number-at-pos) 1)
	file-path
      (let* ((test-func-pos
	      (save-excursion
		(re-search-backward "^ *def test*" nil t)))
	     (test-class-pos
	      (save-excursion
		(re-search-backward "^class *" nil t))))
	(cond
	 ((not (or test-func-pos test-class-pos))
	  (error "You are neither in a test class nor a test function."))
	 ((and test-func-pos
	       (and test-class-pos (< test-class-pos test-func-pos)))
	  (abl-mode-get-test-function-path file-path))
	 (test-class-pos (concat file-path ":" (abl-mode-determine-test-class-name))))))))

(defun abl-mode-run-test-at-point ()
  (interactive)
  (let* ((test-path (abl-mode-get-test-entity)))
    (abl-mode-run-test test-path)))

(defun abl-mode-rerun-last-test ()
  (interactive)
  (if (not abl-mode-last-test-run)
      (message "You haven't run any tests yet.")
    (abl-mode-run-test (car abl-mode-last-test-run) (cdr abl-mode-last-test-run))))

;the command: "find . -name *.py -print | etags -"
(defun abl-mode-create-etags-file ()
  (interactive)
  (let ((tag-file-path (abl-mode-concat-paths abl-mode-branch-base "TAGS")))
    (if (or (not (file-exists-p tag-file-path))
	    (if (y-or-n-p "Tags file alread exists, recreate?")
		(progn
		  (delete-file tag-file-path)
		  t)))
	(let ((etags-command (format abl-mode-etags-command-base
				     abl-mode-branch-base
				     abl-mode-branch-base)))
	  (shell-command-to-string etags-command)))))


(defun abl-mode-parse-python-path (python-path)
  (let* ((colon-index (string-match ":" python-path))
	 (file-part (if colon-index
			(substring python-path 0 colon-index)
		      python-path))
	 (file-path (expand-file-name
		     (concat (abl-mode-join-string (split-string file-part "\\.") "/") ".py")
		     abl-mode-branch-base))
	 (internal-part (if colon-index
			    (substring python-path (+ colon-index 1) (length python-path))
			  nil))
	 (internal-part-dot-index (if internal-part (string-match "\\." internal-part) nil)))
    (let ((class-and-func-name
	   (cond (internal-part-dot-index
		  (cons (substring internal-part 0 internal-part-dot-index)
			(substring internal-part (+ internal-part-dot-index 1)
				   (length internal-part))))
		 ((and internal-part (not internal-part-dot-index) (abl-mode-starts-uppercase? internal-part))
		  (cons internal-part nil))
		 (t (cons nil internal-part)))))
      (list file-path (car class-and-func-name) (cdr class-and-func-name)))))


(defun abl-mode-open-python-path-at-point ()
  (interactive)
  (save-excursion
    (re-search-backward "[^a-zA-Z0-9:_\.]" nil t)
    (forward-char)
    (let ((start (point))
	  (end (- (re-search-forward "[^a-zA-Z0-9:_\.]" nil t) 1)))
      (let ((python-path-info (abl-mode-parse-python-path (buffer-substring-no-properties start end))))
	(let ((file-path (car python-path-info))
	      (class-name (cadr python-path-info))
	      (func-name (caddr python-path-info)))
	  (if (not (file-exists-p file-path))
	      (error (concat "File coud not be found: " file-path)))
	  (find-file file-path)
	  (goto-char (point-min))
	  (if class-name (search-forward (concat "class " class-name)))
	  (if func-name (search-forward (concat "def " func-name))))))))


(defun abl-mode-display-branch()
  "Displays the name of the branch on which the current buffer is"
  (interactive)
  (message (concat "Current branch: " abl-mode-branch)))

(defun abl-mode-start-vem-python ()
  (interactive)
  (ansi-term
   (expand-file-name "python" (abl-mode-concat-paths abl-mode-vems-base-dir  abl-mode-vem-name "bin"))
   (concat "Python " abl-mode-vem-name)))

;; Sample custom command

(defun run-current-branch ()
  (interactive)
  (if (abl-shell-busy)
      (message "The shell is busy; please end the process before running a test")
    (progn
      (abl-mode-exec-command "runit")
      (message (format "Started local server for branch %s" abl-mode-branch)))))


(provide 'abl-mode)


;; <<------------  TODOS -------------->>

;; - per-project customization through a .abl file at the base of project
;; - C-c f looks for definition and not just import
;; - import something from one of the open files (or repeat existing import)
;; - run file as script if it has a name == main at the end
;; - bash script for going to base of project
;; - navigating to definitions of methods etc. should not be that difficult

;;; abl-mode.el ends here
