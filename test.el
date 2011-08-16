(require 'abl)
(require 'ert)

(defun write-to-file (file-path string)
  (with-temp-buffer (insert string)
		    (write-region (point-min) (point-max) file-path)))

(defun setup-git-tests (&optional base)
  ;create git repo with setup.py and a test file
  (let* ((project-name "aproject")
	 (base-dir (or base (make-temp-file "blah" 't)))
	 (project-dir (concat-paths base-dir "aproject")))
    (assert (index-of "Initialized empty Git repository"
		      (shell-command-to-string
		       (concat "git init " base-dir))))
    (if (not (file-exists-p base-dir)) (make-directory base-dir))
    (make-directory project-dir)
    (write-to-file (concat-paths base-dir "setup.py") "blah")
    (write-to-file (concat-paths project-dir "test.py") "blah")
    base-dir))

(defun commit-git (base-path)
  (shell-command-to-string (format
			    "cd %s && git add setup.py && git commit -am 'haha'"
			    base-path)))

(defun branch-git (base-path branch-name)
  (shell-command-to-string (format
			    "cd %s && git branch %s && git checkout %s"
			    base-path branch-name branch-name)))

(defun cleanup (path)
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
     (buffer-local-value 'vem-name buffer))))


(ert-deftest test-empty-git-abl ()
  (let* ((base-dir (setup-git-tests))
	 (project-name (last-path-comp base-dir))
	 (test-file-path (concat-paths base-dir "aproject" "test.py")))
    (let ((abl-values (abl-values-for-path test-file-path)))
      (should (car abl-values))
      (should (string-equal "none" (nth 1 abl-values)))
      (should (string-equal base-dir (nth 2 abl-values)))
      (should (string-equal project-name (nth 3 abl-values)))
    (cleanup base-dir))))


(ert-deftest test-git-abl ()
  (let* ((base-dir (setup-git-tests))
	 (project-name (last-path-comp base-dir))
	 (test-file-path (concat-paths base-dir "aproject" "test.py")))
    (commit-git base-dir)
    (let ((abl-values (abl-values-for-path test-file-path)))
      (should (car abl-values))
      (should (string-equal "master" (nth 1 abl-values)))
      (should (string-equal base-dir (nth 2 abl-values)))
      (should (string-equal project-name (nth 3 abl-values)))
    (cleanup base-dir))))


(ert-deftest test-branched-git-abl ()
  (let* ((base-dir (setup-git-tests))
	 (project-name (last-path-comp base-dir))
	 (test-file-path (concat-paths base-dir "aproject" "test.py")))
    (commit-git base-dir)
    (branch-git base-dir "gitbranch")
    (let ((abl-values (abl-values-for-path test-file-path)))
      (should (car abl-values))
      (should (string-equal "gitbranch" (nth 1 abl-values)))
      (should (string-equal base-dir (nth 2 abl-values)))
      (should (string-equal project-name (nth 3 abl-values))))
    (cleanup base-dir)))
