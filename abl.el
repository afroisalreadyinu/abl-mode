;;; abl-mode.el --- Python TDD minor mode

;; Author: Ulas Tuerkmen
;; URL: http://github.com/afroisalreadyinu/abl-mode
;; Version: 0.9.0

;; The aim of this mode is to make editing Python code in a
;; version-controlled project easier, and make the repetitive tasks
;; such as running tests and bits of code easier. Please see
;; README.rst for details.

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
      (let ((project-base (find-base-dir)))
	(if (not project-base)
	    (setq abl-mode nil)
	  (setq abl-branch-base project-base)
	  (setq abl-branch (branch-name abl-branch-base))
	  (setq project-name (get-project-name abl-branch-base))
	  (setq vem-name (get-vem-name))
	  (setq abl-shell-name (shell-name-for-branch
				project-name
				abl-branch))))))

(defun abl-mode-hook ()
  (abl-mode))

(if (not (assq 'abl-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(abl-mode " abl-mode")
		minor-mode-alist)))

(defvar abl-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r") 'run-current-branch)
    (define-key map (kbd "C-c t") 'run-test-at-point)
    (define-key map (kbd "C-c u") 'rerun-last-test)
    (define-key map (kbd "C-c o") 'open-python-path-at-point)
    (define-key map (kbd "C-c a") 'revert-all-buffers)
    (define-key map (kbd "C-c w") 'display-branch)
    (define-key map (kbd "C-c s") 'start-vem-python)
    map)
  "The keymap for abl-mode")

(or (assoc 'abl-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'abl-mode abl-mode-keymap)
                minor-mode-map-alist)))

;; <<------------  Customization options  -------------->>

(defcustom vem-activate-command "vem activate %s"
  "The command for activating a virtual environment")

(defcustom vem-create-command "vem create %s"
  "The command for activating a virtual environment")

(defcustom test-command "nosetests -s %s"
  "The command for running tests")

(defcustom branch-shell-prefix "ABL-SHELL:"
  "Prefix for the shell buffers opened")

(defcustom vems-base-dir "~/.virtualenvs"
  "base directory for virtual environments")

(defcustom start-server-command "scripts/run.sh"
  "command executed for starting the local server of a web
  application.")

(defcustom abl-python-executable "python"
  "The executable used to install a package with.")

;; <<----------------  Here ends the customization -------------->>

(defvar abl-branch-base ""
  "Base directory of the current branch")
(make-variable-buffer-local 'abl-branch-base)

(defvar vem-name ""
  "Name of the virtual env")
(make-variable-buffer-local 'vem-name)

(defvar etags-command-base "find %s -name '*.py' -print | etags - -o %s/TAGS"
  "command run to create a tags file for emacs")

(defvar abl-branch "master"
  "The branch you are working on.When abl-mode is started, it is
  set to the name of the directory in which you are for svn, the
  git branch if you're on git.")
(make-variable-buffer-local 'abl-branch)

(defvar abl-shell-name "ABL-SHELL")
(make-variable-buffer-local 'abl-shell-name)

(defvar project-name "web"
  "The name of the project. ")
(make-variable-buffer-local 'project-name)

(defvar last-test-run nil
  "Last test run and which branch it was")

(defvar existing-shells '())

(defvar replacement-vems '())

;; <<------------- Helpers  ------------->>

(defun starts-with (str1 str2)
  (and (> (length str1) 0)
       (string= str2
		(substring str1 0 (length str2)))))

(defun ends-with (str1 str2)
  (let ((str1-length (length str1)))
    (and (> str1-length 0)
	 (string= (substring str1 (- str1-length (length str2)) str1-length) str2))))

(defun remove-last (lst)
  (if (not (cdr lst))
      '()
    (cons (car lst) (remove-last (cdr lst)))))

(defun index-of (substr str1)
  (cond ((< (length str1) (length substr)) nil)
	((string= substr (substring str1 0 (length substr))) 0)
	(t (let ((rest-return (index-of substr (substring str1 1 (length str1)))))
	     (if (null rest-return) nil
	       (+ rest-return 1))))))

(defun concat-paths (base &rest paths)
  "join a list of path components into a path"
  (if (equal paths '())
      base
    (apply 'concat-paths
	   (concat (file-name-as-directory base) (car paths))
	   (cdr paths))))

(defun remove-last-slash (path)
  (if (ends-with path "/")
      (substring path 0 (- (length path) 1))
    path))

(defun higher-dir (path)
  "Return one higher directory of a given path"
  (assert (starts-with path "/"))
  (if (string-equal "/" path)
      nil
    (let* ((true-path (remove-last-slash path))
	   (components (split-string true-path "/" )))
      (apply 'concat-paths
	     (concat "/" (car components))
	     (remove-last (cdr components))))))

(defun last-path-comp (path)
  "Get the last path components, whether it's a file name or directory"
  (and (< 0 (length path))
       (car (last (split-string (remove-last-slash path) "/")))))

(defun find-base-dir (&optional dir-path)
  (let* ((path (or dir-path (buffer-file-name))))
    (if (and (file-exists-p (concat-paths path "setup.py"))
	     (not (file-exists-p (concat-paths path "__init__.py"))))
	path
      (let ((higher (higher-dir path)))
	(if (not higher)
	    nil
	  (find-base-dir higher))))))

(defun git-or-svn (base-dir)
  (cond ((file-exists-p (concat-paths base-dir ".git")) "git")
	((file-exists-p (concat-paths base-dir ".svn")) "svn")
	(t nil)))

(defun get-git-branch-name (base-dir)
  (let* ((command (concat "cd " base-dir " && git branch"))
	(git-output (shell-command-to-string command)))
    (if (string-equal git-output "")
	(progn
	  (message "Looks like your git repository is empty (the output of git branch was empty). Calling it 'none'.")
	  "none")
      (string-match "\\* \\(.*\\)" git-output)
      (substring git-output (match-beginning 1) (match-end 1)))))


(defun branch-name (path)
  (if (string= path "/")
      nil
    (let ((vcs (git-or-svn path)))
      (cond ((or (not vcs) (string-equal vcs "svn"))
	     (last-path-comp path))
	    ((string-equal vcs "git")
	     (get-git-branch-name path))
	    (t nil)))))


(defun get-project-name (path)
  "Returns the name of the project; higher directory for no vcs or svn,
   directory name for git."
  (if (string= path "/")
      nil
    (let ((vcs (git-or-svn path)))
      (cond ((or (not vcs) (string-equal vcs "svn"))
	     (last-path-comp (higher-dir path)))
	    ((string-equal vcs "git")
	     (last-path-comp path))
	    (t nil)))))

(defun get-vem-name (&optional branch project)
  (let ((branch-name (or branch abl-branch))
	(prjct-name (or project project-name)))
    (or
     (cdr (assoc branch-name replacement-vems))
     (concat prjct-name "_" branch-name))))

;;<< ---------------  Shell stuff  ----------------->>

(defun shell-name-for-branch (project-name branch-name)
  (concat branch-shell-prefix project-name "_" branch-name))

(defun shell-busy (&optional shell-buffer)
  (let* ((real-buffer (or shell-buffer (current-buffer)))
	 (shell-process-id (process-id (get-buffer-process real-buffer)))
	 (command (format "ps --ppid %d  h | wc -l" shell-process-id))
	 (output (shell-command-to-string command)))
    (/= (string-to-number output) 0)))

(defun abl-shell-busy ()
  (let ((abl-shell-buffer (get-buffer abl-shell-name)))
    (if (not abl-shell-buffer)
	nil
      (shell-busy abl-shell-buffer))))

(defun exec-command (command)
  "This function should be used from inside a non-shell buffer"
  (let* ((new-or-name (vem-name-or-create vem-name))
	 (create-vem (cdr new-or-name))
	 (new-vem-name (car new-or-name))
	 (commands (if create-vem (list
				   (concat "cd " abl-branch-base)
				   (format vem-create-command new-vem-name)
				   (format vem-activate-command new-vem-name)
				   (format "%s setup.py develop" abl-python-executable)
				   command)
		     (list
		      (concat "cd " abl-branch-base)
		      (format vem-activate-command new-vem-name)
		      command))))

  (shell abl-shell-name)
  (unless (member abl-shell-name existing-shells) (sleep-for 2))
  (setf existing-shells (append existing-shells '(abl-shell-name)))
  (run-command (join-string commands " && "))
  (if (> (length (get-buffer-window-list abl-shell-name nil t)) 1)
      (delete-window))))

(defun run-command (command)
  "This function should be used when inside a shell"
  (goto-char (point-max))
  (insert command)
  (comint-send-input))

(defun vem-name-or-create (name)
  (let ((replacement-vem (cdr (assoc name replacement-vems))))
    (if replacement-vem
	(cons replacement-vem nil)
      (let ((vem-path (expand-file-name name vems-base-dir)))
	(if (file-exists-p vem-path)
	    (cons name nil)
	  (let*
	      ((command-string (format "No vem %s; y to create it, or name of existing to use instead: "
				       name))
	       (vem-or-y (read-from-minibuffer command-string))
	       (create-new (or (string-equal vem-or-y "y") (string-equal vem-or-y "Y"))))
	    (if create-new
		(cons name create-new)
	      (vem-name-or-create vem-or-y))))))))

;; <<------------  Running the server and tests  -------->>

(defun run-current-branch ()
  (interactive)
  (if (abl-shell-busy)
      (message "The shell is busy; please end the process before running a test")
    (progn
      (exec-command start-server-command)
      (message (format "Started local server for branch %s" abl-branch)))))

(defun determine-test-function-name ()
  (save-excursion
    (end-of-line)
    (if (not (re-search-backward "^ *def test_*" nil t))
	(error "Looks like you are not even in a function definiton! Bad girl!"))
    (let* ((start (re-search-forward "^ *def *"))
	   (end (re-search-forward "test_[^\(]*" (line-end-position) t)))
      (if (not end)
	  (error "Looks like you are not inside a test function. Go to a test function! Now!")
	(buffer-substring-no-properties start (point))))))


(defun determine-test-class-name ()
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
(defun test-in-class ()
  (save-excursion
    (end-of-line)
    (let* ((start (re-search-backward "^ *def *"))
	   (end (re-search-forward "[^ ]")))
      (> (- end start 1) 0))))


(defun get-test-file-path ()
  (let ((buffer-name (buffer-file-name)))
    (if (not (ends-with buffer-name ".py"))
	(error "You do not appear to be in a python file. Now open a python file!"))
    (let ((relative-path (substring
			  buffer-file-name
			  (+ (length abl-branch-base) 1)
			  (- (length buffer-name) 3))))
      (replace-regexp-in-string "/" "." relative-path))))


(defun get-test-function-path (file-path)
  (let ((function-name (determine-test-function-name)))
    (if (not (test-in-class))
	(concat file-path ":" function-name)
      (let ((class-name (determine-test-class-name)))
	(concat file-path ":" class-name "." function-name)))))


(defun run-test (test-path &optional branch-name)
  (if (abl-shell-busy)
      (message "The shell is busy; please end the process before running a test")
    (let* ((shell-command (format test-command test-path))
	   (real-branch-name (or branch-name abl-branch)))
      (message (format "Running test(s) %s on branch %s" test-path real-branch-name))
      (exec-command shell-command)
      (setq last-test-run (cons test-path abl-branch)))))


;This returns the python destination on point, depending on
;whether it is a test function, class, or whole file
(defun get-test-entity ()
  (let ((file-path (get-test-file-path)))
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
	  (get-test-function-path file-path))
	 (test-class-pos (concat file-path ":" (determine-test-class-name))))))))

(defun run-test-at-point ()
  (interactive)
  (let* ((test-path (get-test-entity)))
    (run-test test-path)))

(defun rerun-last-test ()
  (interactive)
  (if (not last-test-run)
      (message "You haven't run any tests yet.")
    (run-test (car last-test-run) (cdr last-test-run))))

;the command: "find . -name *.py -print | etags -"
(defun create-etags-file ()
  (interactive)
  (let ((tag-file-path (concat-paths abl-branch-base "TAGS")))
    (if (or (not (file-exists-p tag-file-path))
	    (if (y-or-n-p "Tags file alread exists, recreate?")
		(progn
		  (delete-file tag-file-path)
		  t)))
	(let ((etags-command (format etags-command-base
				     abl-branch-base
				     abl-branch-base)))
	  (shell-command-to-string etags-command)))))


(defun string-in-buffer (string)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward string nil t)
	t
      nil)))


(defun join-string (string-list joiner)
  (cond ((not (cdr string-list)) (car string-list))
	 (t (concat
	     (car string-list)
	     joiner
	     (join-string (cdr string-list) joiner)))))

(defun starts-uppercase? (strng)
  (let ((y (substring strng 0 1))) (string= y (upcase y))))


(defun parse-python-path (python-path)
  (let* ((colon-index (string-match ":" python-path))
	 (file-part (if colon-index
			(substring python-path 0 colon-index)
		      python-path))
	 (file-path (expand-file-name
		     (concat (join-string (split-string file-part "\\.") "/") ".py")
		     abl-branch-base))
	 (internal-part (if colon-index
			    (substring python-path (+ colon-index 1) (length python-path))
			  nil))
	 (internal-part-dot-index (if internal-part (string-match "\\." internal-part) nil)))
    (let ((class-and-func-name
	   (cond (internal-part-dot-index
		  (cons (substring internal-part 0 internal-part-dot-index)
			(substring internal-part (+ internal-part-dot-index 1)
				   (length internal-part))))
		 ((and internal-part (not internal-part-dot-index) (starts-uppercase? internal-part))
		  (cons internal-part nil))
		 (t (cons nil internal-part)))))
      (list file-path (car class-and-func-name) (cdr class-and-func-name)))))


(defun open-python-path-at-point ()
  (interactive)
  (save-excursion
    (re-search-backward "[^a-zA-Z0-9:_\.]" nil t)
    (forward-char)
    (let ((start (point))
	  (end (- (re-search-forward "[^a-zA-Z0-9:_\.]" nil t) 1)))
      (let ((python-path-info (parse-python-path (buffer-substring-no-properties start end))))
	(let ((file-path (car python-path-info))
	      (class-name (cadr python-path-info))
	      (func-name (caddr python-path-info)))
	  (if (not (file-exists-p file-path))
	      (error (concat "File coud not be found: " file-path)))
	  (find-file file-path)
	  (goto-char (point-min))
	  (if class-name (search-forward (concat "class " class-name)))
	  (if func-name (search-forward (concat "def " func-name))))))))


(defun revert-or-skip (&optional buff)
  "Reverts a buffer in abl-mode if it is not modified"
  (let ((buffer (or buff (current-buffer))))
    (when (and (local-variable-p 'abl-mode)
	       (buffer-local-value 'abl-mode buffer))
      (message (buffer-name buffer))
      (if (buffer-modified-p buffer)
	  (progn (message (concat "Buffer " (buffer-name buffer) " is modified"))
		 (buffer-name buffer))
      (progn
	(set-buffer buffer)
	(revert-buffer t t t)
	(abl-mode t)
	nil
	)))))

(defun revert-all-buffers()
  "Refreshs all non-modified open buffers with the function above"
  (interactive)
  (let ((modified-files (delq nil (mapcar 'revert-or-skip (buffer-list)))))
    (if modified-files
	(message (concat "Following buffers have modifications: " (join-string modified-files " , ")))
      (message "Refreshed open files"))))

(defun display-branch()
  "Displays the name of the branch on which the current buffer is"
  (interactive)
  (message (concat "Current branch: " abl-branch)))

(defun start-vem-python ()
  (interactive)
  (ansi-term
   (expand-file-name "python" (concat-paths vems-base-dir  vem-name "bin"))
   (concat "Python " vem-name)))

(provide 'abl)


;; <<------------  TODOS -------------->>

;; - recreate virtual env
;; - present status of test by reading stdout from proc or something
;; - take to location in stack trace
;; - create tags file and find files in project.
;; - create etags file progressively by first finding all the code files in the dir
;; - don't do git branch for every new buffer; make an assoc-list of bases (works now, as an improvement)


;; Features:
;; - add import (looks at other files, maybe in the order of last
;;   visited, and finds suitable imports to insert
;; - show the import line for an entity

;;; abl-mode.el ends here