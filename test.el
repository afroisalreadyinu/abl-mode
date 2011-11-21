;; to run the tests:
;; emacs -q -L . -L /path/to/dir/where/ert/resides -l test.el --batch

(require 'abl)
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
	 (project-dir (concat-paths base-dir project-subdir))
	 (proof-dir (concat-paths base-dir "_proof")))
    (if (not (file-exists-p base-dir)) (make-directory base-dir))
    (assert (index-of "Initialized empty Git repository"
		      (shell-command-to-string
		       (concat "git init " base-dir))))
    (make-directory project-dir)
    (make-directory proof-dir)
    (write-to-file (concat-paths base-dir "setup.py") "blah")
    (write-to-file (concat-paths project-dir test-file-name) test-file-content)
    (write-to-file (concat-paths project-dir "__init__.py") "#nothing")
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
  (unless (starts-with path "/tmp")
    (error
     (format "Tried to cleanup a path (%s) not in /tmp; refusing to do so."
	     path)))
  (shell-command-to-string
   (concat "rm -rf " path)))

(defun create-dummy-project ()
  (let* ((base-dir (make-temp-file "yada" 't))
	 (next-dir (concat-paths base-dir "etc"))
	 (another-dir (concat-paths next-dir "blah")))
    (make-directory next-dir)
    (make-directory another-dir)
    (write-to-file (concat-paths base-dir "setup.py") "blah")
    another-dir))


(ert-deftest test-abl-utils ()
  (should (string-equal (concat-paths "/tmp/blah" "yada" "etc")
			"/tmp/blah/yada/etc"))
  (should (equal (remove-last '(1 2 3 4)) '(1 2 3)))
  (should (equal (remove-last '(1)) '()))
  (should (string-equal (higher-dir "/home/username/temp") "/home/username"))
  (should (string-equal (higher-dir "/home/username/") "/home"))
  (should (string-equal (higher-dir "/home") "/"))
  (should (not(higher-dir "/")))
  (should (string-equal (remove-last-slash "/hehe/haha") "/hehe/haha"))
  (should (string-equal (remove-last-slash "/hehe/haha/") "/hehe/haha"))
  (should (string-equal (remove-last-slash "") ""))
  (should (string-equal (last-path-comp "/hehe/haha") "haha"))
  (should (string-equal (last-path-comp "/hehe/haha/") "haha"))
  (should (string-equal (last-path-comp "/hehe/haha.py") "haha.py"))
  (should (not (last-path-comp "")))
  )

(ert-deftest test-path-funcs ()
  (should (not (find-base-dir "/home")))
  (let ((path (create-dummy-project)))
    (should (string-equal (find-base-dir path)
			  (higher-dir (higher-dir path))))
    (should (string-equal (find-base-dir (higher-dir (higher-dir path)))
			  (higher-dir (higher-dir path))))
    (let* ((base-path (find-base-dir path))
	   (git-path (concat-paths base-path ".git"))
	   (svn-path (concat-paths base-path ".svn")))
      (should (not (git-or-svn base-path)))
      (make-directory git-path)
      (should (string-equal (git-or-svn base-path) "git"))
      (cleanup git-path)
      (make-directory svn-path)
      (should (string-equal (git-or-svn base-path) "svn"))
      (cleanup base-path)
    )))


(ert-deftest test-project-name-etc ()
  (should (string-equal (branch-name "/home") "home"))
  (let* ((top-dir (make-temp-file "blah" 't))
	 (top-dir-name (last-path-comp top-dir))
	 (project-path (concat-paths top-dir "project")))
    (setup-git-tests project-path)
    (commit-git project-path)
    (should (string-equal (branch-name project-path) "master"))
    (should (string-equal (get-project-name project-path) "project"))

    (should (string-equal (get-vem-name "master" "project")
			  "project_master"))

    (cleanup (concat-paths project-path ".git"))
    (make-directory (concat-paths project-path ".svn"))
    (should (string-equal (branch-name project-path) "project"))
    (should (string-equal (get-project-name project-path) top-dir-name))
    (cleanup top-dir)
    ))


(defun abl-values-for-path (path)
  (let ((buffer (find-file path)))
    (list
     (buffer-local-value 'abl-mode buffer)
     (buffer-local-value 'abl-branch buffer)
     (buffer-local-value 'abl-branch-base buffer)
     (buffer-local-value 'project-name buffer)
     (buffer-local-value 'vem-name buffer)
     (buffer-local-value 'abl-shell-name buffer))))


(defmacro abl-git-test (&rest tests-etc)
  "Macro for tests. The first argument determines whether a dummy
vem is created."
  `(let* ((base-dir (setup-git-tests))
	  (project-name (last-path-comp base-dir))
	  (test-file-path (concat-paths base-dir "aproject" "test.py")))
     (unwind-protect
	 (progn
	   ,@tests-etc)
	 (cleanup base-dir))))


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
	   (test-path (get-test-entity))
	   (vem-proof-file-path (format "%s/_proof/proveit.txt" base-dir))
	   (test-proof-file-path (format "%s/_proof/prove_test.txt" base-dir))
	   (run-proof-file-path (format "%s/_proof/prove_run.txt" base-dir))
	   (vemname (nth 4 abl-values)))
      (setq vem-activate-command (concat "echo '%s' > " vem-proof-file-path))
      (setq test-command (concat "echo '%s' > " test-proof-file-path))
      (setq vems-base-dir (make-temp-file "vems" 't))
      (shell-command-to-string (format "virtualenv %s"
				       (concat-paths vems-base-dir vemname)))
      (should (string-equal test-path "aproject.test:AblTest.test_abl_mode"))
      (run-test-at-point)
      (sleep-for 1)
      (should (file-exists-p vem-proof-file-path))
      (save-excursion
	(find-file vem-proof-file-path)
	(should (search-forward vemname)))
      (should (file-exists-p test-proof-file-path))
      (save-excursion
	(find-file test-proof-file-path)
	(should (search-forward test-path)))

      (find-file test-file-path)
      (setq start-server-command (format "echo `pwd` > %s" run-proof-file-path))
      (run-current-branch)
      (sleep-for 1)
      (should (file-exists-p run-proof-file-path))
      (save-excursion
	(find-file run-proof-file-path)
	(should (string= (buffer-substring (point-min) (- (point-max) 1)) base-dir)))
      (cleanup vems-base-dir))))

(ert-deftest test-replacement-vem ()
  (abl-git-test
    (commit-git base-dir)
    (find-file test-file-path)
    (goto-char (point-max))
    (let* ((abl-values (abl-values-for-path test-file-path))
	  (vemname (nth 4 abl-values)))
      (shell-command-to-string (format "virtualenv %s"
				       (concat-paths vems-base-dir vemname)))
      (should (= 0 (length replacement-vems)))
      (cleanup vems-base-dir))))


(add-hook 'find-file-hooks 'abl-mode-hook)
(ert t)