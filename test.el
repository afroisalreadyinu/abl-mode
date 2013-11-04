;; to run the tests:
;; emacs -q -L . -L /path/to/dir/where/ert/resides -l test.el --batch

(require 'abl-mode)
(require 'ert)

(defun write-to-file (file-path string)
  (with-temp-buffer (insert string)
		    (write-region (point-min) (point-max) file-path)))

(defvar project-subdir "aproject")
(defvar test-file-name "test.py")
(defvar output-file-path "/tmp/tc.txt")
(defvar output-content "ABL MODE WAS HERE")
(defvar test-file-content
  (concat "class AblTest(object):\n"
	   "    def test_abl_mode():\n"
   (format "        f = open('%s')\n" output-file-path)
   (format "        f.write('%s')\n" output-content)
	   "        f.close()"))

(defun setup-git-tests (&optional base)
  ;;create git repo with setup.py and a test file. the folder
  ;;structure will look something like this (the temp directory name
  ;;starting with abltest will be different):
  ;; /tmp
  ;;   |
  ;;   - abltest18945
  ;;        |
  ;;        - .git
  ;;        - setup.py (contents: blah)
  ;;        - _proof (dir)
  ;;        - aproject
  ;;             |
  ;;             - test.py (contents: test-file-content)
  ;;             - __init__.py (contents: #nothing)

  (let* ((base-dir (or base (make-temp-file "abltest" 't)))
	 (project-dir (abl-mode-concat-paths base-dir project-subdir))
	 (proof-dir (abl-mode-concat-paths base-dir "_proof")))
    (if (not (file-exists-p base-dir)) (make-directory base-dir))
    (assert (abl-mode-index-of "Initialized empty Git repository"
		      (shell-command-to-string
		       (concat "git init " base-dir))))
    (make-directory project-dir)
    (make-directory proof-dir)
    (write-to-file (abl-mode-concat-paths base-dir "setup.py") "blah")
    (write-to-file (abl-mode-concat-paths project-dir test-file-name) test-file-content)
    (write-to-file (abl-mode-concat-paths project-dir "__init__.py") "#nothing")
    base-dir))

(defun commit-git (base-path)
    (shell-command-to-string
     (format
      "cd %s && git add setup.py && git add %s/%s && git commit -am 'haha'"
      base-path
      project-subdir
      test-file-name)))

(defun branch-git (base-path branch-name)
  (shell-command-to-string (format
			    "cd %s && git branch %s && git checkout %s"
			    base-path branch-name branch-name)))

(defun cleanup (path)
  ;; rm -rf's a folder which begins with /tmp. you shouldn't put
  ;; important stuff into /tmp.
  (unless (or (abl-mode-starts-with path "/tmp") (abl-mode-starts-with path "/var/folders/"))
    (error
     (format "Tried to cleanup a path (%s) not in /tmp; refusing to do so."
	     path)))
  (shell-command-to-string
   (concat "rm -rf " path)))

(defun create-dummy-project ()
  (let* ((base-dir (make-temp-file "yada" 't))
	 (next-dir (abl-mode-concat-paths base-dir "etc"))
	 (another-dir (abl-mode-concat-paths next-dir "blah")))
    (make-directory next-dir)
    (make-directory another-dir)
    (write-to-file (abl-mode-concat-paths base-dir "setup.py") "blah")
    another-dir))


(ert-deftest test-abl-utils ()
  (should (string-equal (abl-mode-concat-paths "/tmp/blah" "yada" "etc")
			"/tmp/blah/yada/etc"))
  (should (equal (abl-mode-remove-last '(1 2 3 4)) '(1 2 3)))
  (should (equal (abl-mode-remove-last '(1)) '()))
  (should (string-equal (abl-mode-higher-dir "/home/username/temp") "/home/username"))
  (should (string-equal (abl-mode-higher-dir "/home/username/") "/home"))
  (should (string-equal (abl-mode-higher-dir "/home") "/"))
  (should (not(abl-mode-higher-dir "/")))
  (should (string-equal (abl-mode-remove-last-slash "/hehe/haha") "/hehe/haha"))
  (should (string-equal (abl-mode-remove-last-slash "/hehe/haha/") "/hehe/haha"))
  (should (string-equal (abl-mode-remove-last-slash "") ""))
  (should (string-equal (abl-mode-last-path-comp "/hehe/haha") "haha"))
  (should (string-equal (abl-mode-last-path-comp "/hehe/haha/") "haha"))
  (should (string-equal (abl-mode-last-path-comp "/hehe/haha.py") "haha.py"))
  (should (not (abl-mode-last-path-comp "")))
  )

(ert-deftest test-path-funcs ()
  (should (not (abl-mode-find-base-dir "/home")))
  (let ((path (create-dummy-project)))
    (should (string-equal (abl-mode-find-base-dir path)
			  (abl-mode-higher-dir (abl-mode-higher-dir path))))
    (should (string-equal (abl-mode-find-base-dir (abl-mode-higher-dir (abl-mode-higher-dir path)))
			  (abl-mode-higher-dir (abl-mode-higher-dir path))))
    (let* ((base-path (abl-mode-find-base-dir path))
	   (git-path (abl-mode-concat-paths base-path ".git"))
	   (svn-path (abl-mode-concat-paths base-path ".svn")))
      (should (not (abl-mode-git-or-svn base-path)))
      (make-directory git-path)
      (should (string-equal (abl-mode-git-or-svn base-path) "git"))
      (cleanup git-path)
      (make-directory svn-path)
      (should (string-equal (abl-mode-git-or-svn base-path) "svn"))
      (cleanup base-path)
    )))


(ert-deftest test-project-name-etc ()
  (should (string-equal (abl-mode-branch-name "/home") "home"))
  (let* ((top-dir (make-temp-file "blah" 't))
	 (top-dir-name (abl-mode-last-path-comp top-dir))
	 (project-path (abl-mode-concat-paths top-dir "project")))
    (setup-git-tests project-path)
    (commit-git project-path)
    (should (string-equal (abl-mode-branch-name project-path) "master"))
    (should (string-equal (abl-mode-get-project-name project-path) "project"))

    (should (string-equal (abl-mode-get-vem-name "master" "project")
			  "project_master"))

    (cleanup (abl-mode-concat-paths project-path ".git"))
    (make-directory (abl-mode-concat-paths project-path ".svn"))
    (should (string-equal (abl-mode-branch-name project-path) "project"))
    (should (string-equal (abl-mode-get-project-name project-path) top-dir-name))
    (cleanup top-dir)
    ))


(defun abl-values-for-path (path)
  (let ((buffer (find-file path)))
    (list
     (buffer-local-value 'abl-mode buffer)
     (buffer-local-value 'abl-mode-branch buffer)
     (buffer-local-value 'abl-mode-branch-base buffer)
     (buffer-local-value 'abl-mode-project-name buffer)
     (buffer-local-value 'abl-mode-vem-name buffer)
     (buffer-local-value 'abl-mode-shell-name buffer))))


(defmacro abl-git-test (&rest tests-etc)
  "Macro for tests. The first argument determines whether a dummy
vem is created."
  `(let* ((base-dir (setup-git-tests))
	  (project-name (abl-mode-last-path-comp base-dir))
	  (test-file-path (abl-mode-concat-paths base-dir "aproject" "test.py"))
	  (vem-proof-file-path (format "%s/_proof/proveit.txt" base-dir))
	  (test-proof-file-path (format "%s/_proof/prove_test.txt" base-dir))
	  (run-proof-file-path (format "%s/_proof/prove_run.txt" base-dir)))
     (unwind-protect
	 (progn
	   ,@tests-etc)
	 ;;(cleanup base-dir)
	 )))


(ert-deftest test-empty-git-abl ()
  (abl-git-test
    (let ((abl-values (abl-values-for-path test-file-path)))
      (should (car abl-values))
      (should (string-equal "none" (nth 1 abl-values)))
      (should (string-equal base-dir (nth 2 abl-values)))
      (should (string-equal project-name (nth 3 abl-values)))
      (should (string-equal (concat project-name "_" "none") (nth 4 abl-values))))))


(ert-deftest test-git-abl ()
  (abl-git-test
    (commit-git base-dir)
    (let ((abl-values (abl-values-for-path test-file-path)))
      (should (car abl-values))
      (should (string-equal "master" (nth 1 abl-values)))
      (should (string-equal base-dir (nth 2 abl-values)))
      (should (string-equal project-name (nth 3 abl-values)))
      (should (string-equal (concat project-name "_" "master")
			    (nth 4 abl-values)))
      (should (string-equal (concat "ABL-SHELL:" project-name "_" "master")
			    (nth 5 abl-values))))))


(ert-deftest test-branched-git-abl ()
  (abl-git-test
    (commit-git base-dir)
    (branch-git base-dir "gitbranch")
    (let ((abl-values (abl-values-for-path test-file-path)))
      (should (car abl-values))
      (should (string-equal "gitbranch" (nth 1 abl-values)))
      (should (string-equal base-dir (nth 2 abl-values)))
      (should (string-equal project-name (nth 3 abl-values)))
      (should (string-equal (concat project-name "_" "gitbranch")
			    (nth 4 abl-values)))
      (should (string-equal (concat "ABL-SHELL:" project-name "_" "gitbranch")
			    (nth 5 abl-values))))))


(ert-deftest test-git-abl-functionality ()
  ;;this test checks whether the two main functionalities of running
  ;;tests and running a server work
  (abl-git-test
    (commit-git base-dir)
    (find-file test-file-path)
    (goto-char (point-max))
    (let* ((abl-values (abl-values-for-path test-file-path))
	   (test-path (abl-mode-get-test-entity))
	   (vemname (nth 4 abl-values)))
      (setq abl-mode-vem-activate-command (concat "echo '%s' > " vem-proof-file-path))
      (setq abl-mode-test-command (concat "echo '%s' > " test-proof-file-path))
      (setq abl-mode-vems-base-dir (make-temp-file "vems" 't))
      (shell-command-to-string (format "virtualenv %s"
				       (abl-mode-concat-paths abl-mode-vems-base-dir vemname)))
      (should (string-equal test-path "aproject.test:AblTest.test_abl_mode"))
      (abl-mode-run-test-at-point)
      (sleep-for 1)
      (should (file-exists-p vem-proof-file-path))

      (save-excursion
	(find-file vem-proof-file-path)
	(should (string= (buffer-substring (point-min) (- (point-max) 1)) vemname)))

      (should (file-exists-p test-proof-file-path))

      (save-excursion
	(find-file test-proof-file-path)
	(should (string= (buffer-substring (point-min) (- (point-max) 1)) test-path)))

      (find-file test-file-path)
      (setq start-server-command (format "echo `pwd` > %s" run-proof-file-path))
      (cleanup abl-mode-vems-base-dir))))

(ert-deftest test-replacement-vem ()
  (abl-git-test
    (commit-git base-dir)
    (let* ((abl-values (abl-values-for-path test-file-path))
	   (master-vemname (nth 4 abl-values))
	   (new-branch "gitbranch")
	   (branch-vemname (concat project-name "_" new-branch))
	   (test-buff (find-file test-file-path)))
      (goto-char (point-max))
      (setq abl-mode-vems-base-dir (make-temp-file "vems" 't))
      (shell-command-to-string (format "virtualenv %s"
				       (abl-mode-concat-paths abl-mode-vems-base-dir master-vemname)))
      (should (= 0 (length abl-mode-replacement-vems)))

      (branch-git base-dir new-branch)
      (setq abl-mode-replacement-vems (list (cons branch-vemname master-vemname)))
      (setq abl-mode-vem-activate-command (concat "echo '%s' > " vem-proof-file-path))

      (revert-buffer test-buff t nil)
      (goto-char (point-max))
      (abl-mode-run-test-at-point)
      (sleep-for 1)
      (should (file-exists-p vem-proof-file-path))
      (save-excursion
	(find-file vem-proof-file-path)
	(should (string= (buffer-substring (point-min) (- (point-max) 1))
			 master-vemname)))

      (cleanup abl-mode-vems-base-dir)
      )))


(add-hook 'find-file-hooks 'abl-mode-hook)
(ert t)
