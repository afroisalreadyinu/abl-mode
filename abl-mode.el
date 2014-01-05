;;; abl-mode.el --- Python TDD minor mode

;;
;; Author: Ulas Tuerkmen <ulas.tuerkmen at gmail dot com>
;; URL: http://github.com/afroisalreadyinu/abl-mode
;; Version: 0.9.2
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
	  (setq abl-mode-ve-name (abl-mode-get-vem-name))
	  (setq abl-mode-shell-name (abl-mode-shell-name-for-branch
				     abl-mode-project-name
				     abl-mode-branch))
	  (abl-mode-local-options project-base)))))

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
    (define-key map (kbd "C-c s") 'abl-mode-start-python)
    map)
  "The keymap for abl-mode")

(or (assoc 'abl-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'abl-mode abl-mode-keymap)
                minor-mode-map-alist)))

;; <<------------  Customization options  -------------->>

(defcustom abl-mode-ve-activate-command "workon %s"
  "The command for activating a virtual environment")
(make-variable-buffer-local 'abl-mode-ve-activate-command)

(defcustom abl-mode-ve-create-command "mkvirtualenv %s"
  "The command for activating a virtual environment")
(make-variable-buffer-local 'abl-mode-ve-create-command)

(defcustom abl-mode-test-command "nosetests -s %s"
  "The command for running tests")
(make-variable-buffer-local 'abl-mode-test-command)

(defcustom abl-mode-branch-shell-prefix "ABL-SHELL:"
  "Prefix for the shell buffers opened")
(make-variable-buffer-local 'abl-mode-branch-shell-prefix)

(defcustom abl-mode-check-and-activate-ve t
  "Check existence of virtualenv, and activate it when a command is run")
(make-variable-buffer-local 'abl-mode-check-and-activate-ve)

(defcustom abl-mode-ve-base-dir "~/.virtualenvs"
  "base directory for virtual environments")
(make-variable-buffer-local 'abl-mode-ve-base-dir)

(defcustom abl-mode-install-command "python setup.py develop"
  "The command to install a package.")
(make-variable-buffer-local 'abl-mode-install-command)

(defcustom abl-mode-test-file-regexp ".*_tests.py"
"regexp used to check whether a file is a test file")
(make-variable-buffer-local 'abl-mode-test-file-regexp)

(defcustom abl-mode-code-file-tests-regexps
  '("^\"\"\"[^(\"\"\")]*\\(^tests:\\)" "^'''[^(''')]*\\(^tests:\\)")
"list of regexps used to search for corresponding test files in a code file")
(make-variable-buffer-local 'abl-mode-code-file-tests-regexps)

;; <<----------------  Here ends the customization -------------->>

(defvar abl-mode-branch-base ""
  "Base directory of the current branch")
(make-variable-buffer-local 'abl-mode-branch-base)

(defvar abl-mode-ve-name ""
  "Name of the virtual env")
(make-variable-buffer-local 'abl-mode-ve-name)

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


(defvar abl-mode-shell-child-cmd
  (if (eq system-type 'darwin)
      "ps -j | grep %d | grep -v grep | grep -v \"/bin/bash\" | wc -l"
    "ps --ppid %d  h | wc -l"))
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

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)

;; ------------------------------------

(defun abl-mode-git-or-svn (base-dir)
  (cond ((locate-dominating-file (abl-mode-concat-paths base-dir) ".git") "git")
	((locate-dominating-file (abl-mode-concat-paths base-dir) ".svn") "svn")
	(t nil)))

(defun abl-mode-set-config (name value)
  (set (intern name) (eval (read value))))

(defun parse-abl-options (file-path)
  (let ((config-lines (with-temp-buffer
			(insert-file-contents file-path)
			(split-string (buffer-string) "\n" t))))
    (loop for config-line in config-lines
	  do (let* ((parts (split-string config-line))
		    (command-part (car parts))
		    (rest-part (abl-mode-join-string (cdr parts) " ")))
	       (abl-mode-set-config command-part rest-part)))))


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
     (concat prjct-name "_"
	     (replace-regexp-in-string "/" "-" branch-name)))))

;;<< ---------------  Shell stuff  ----------------->>

(defun abl-mode-shell-name-for-branch (project-name branch-name)
  (concat abl-mode-branch-shell-prefix project-name "_" branch-name))


(defun abl-shell-busy ()
  "Find out whether the shell has any child processes
running using ps."
  (let ((abl-shell-buffer (get-buffer abl-mode-shell-name)))
    (if (not abl-shell-buffer)
	nil
      (let* ((shell-process-id (process-id (get-buffer-process abl-shell-buffer)))
	     (command (format abl-mode-shell-child-cmd shell-process-id))
	     (output (shell-command-to-string command)))
	(/= (string-to-number output) 0)))))


(defun abl-mode-exec-command (command)
  (let* ((new-or-name (abl-mode-ve-name-or-create abl-mode-ve-name))
	 (ve-name (car new-or-name))
	 (create-vem (cdr new-or-name))
	 (commands
	  (cond (create-vem (list (concat "cd " abl-mode-branch-base)
				  (format abl-mode-ve-create-command ve-name)
				  (format abl-mode-ve-activate-command ve-name)
				  abl-mode-install-command
				  command))
		((not ve-name) (list (concat "cd " abl-mode-branch-base)
					  command))
		(t (list (concat "cd " abl-mode-branch-base)
			 (format abl-mode-ve-activate-command ve-name)
			 command))))
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
    (goto-char (point-max))
    (insert (abl-mode-join-string commands " && "))
    (comint-send-input)
    (select-window code-window)))


(defun abl-mode-ve-name-or-create (name)
  (if (not abl-mode-check-and-activate-ve)
      (cons nil nil)
    (let ((replacement-vem (cdr (assoc name abl-mode-replacement-vems))))
      (if replacement-vem
	  (cons replacement-vem nil)
	(let ((vem-path (expand-file-name name abl-mode-ve-base-dir)))
	  (if (file-exists-p vem-path)
	      (cons name nil)
	    (let*
		((command-string
		  (format
		   "No virtualenv %s; y to create it, or name of existing to use instead: "
		   name))
		 (vem-or-y (read-from-minibuffer command-string))
		 (create-new (or (string-equal vem-or-y "y") (string-equal vem-or-y "Y"))))
	      (if create-new
		  (cons name create-new)
		(abl-mode-ve-name-or-create vem-or-y)))))))))

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

(defun abl-mode-test-for-code-file ()
  "Look for a 'tests: ' header in a python code file. This
function is a bit convoluted because I prefer a longish function
to a mind-bending regular expression. Especially in elisp."
  (save-excursion
    (goto-char (point-min))
    (let ((tests-list-start nil)
	  (regexp-list abl-mode-code-file-tests-regexps))
      (while (and regexp-list (not tests-list-start))
	(setq tests-list-start (re-search-forward (car regexp-list) nil t))
	(setq regexp-list (cdr regexp-list)))
      (if (not tests-list-start)
	  nil
	(goto-char tests-list-start)
	(chomp (buffer-substring tests-list-start (line-end-position)))))))


(defun abl-mode-get-test-entity ()
  "Which tests should be run? If this is a test file, depending
on where the cursor is, test whole file, class, or test
method. Otherwise, look for a header with 'tests:' and run
that. In the last case, return whatever follows 'tests: '. Error
if none of these is true."
  (let* ((file-path (abl-mode-get-test-file-path))
	 (is-test-file (eql (string-match abl-mode-test-file-regexp
					  (buffer-file-name)) 0)))
    (if (not is-test-file)
	(let ((test-file-paths (abl-mode-test-for-code-file)))
	  (if test-file-paths
	      (abl-mode-join-string test-file-paths " ")
	    (error "You are not in a test file, and there are no tests in header.")))
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
	 (test-class-pos (concat file-path ":" (abl-mode-determine-test-class-name)))))))))


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


(defun abl-mode-start-python ()
  (interactive)
  (ansi-term
   (expand-file-name "python"
		     (abl-mode-concat-paths
		      abl-mode-ve-base-dir
		      abl-mode-ve-name
		      "bin"))
   (concat "Python " abl-mode-ve-name)))

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

;; - import something from one of the open files (or repeat existing import)
;;      - when abl-mode is initialized on a file, find the imports, add to list if new
;;      - add command to insert an import
;; - C-c f looks for definition and not just import
;; - navigating to definitions of methods etc. should not be that difficult
;; - add option to return to code window if test runs, is great success,
;;   and window was swtiched from code window.
;; - maybe: parsing output? listing failed tests?

;;; abl-mode.el ends here
