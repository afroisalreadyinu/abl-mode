;; See `Running the tests` section in the readme.

(require 'abl-mode)
(require 'ert)
(require 'cl-lib)
(require 'f)

(toggle-debug-on-error)

(add-hook 'find-file-hooks 'abl-mode-hook)

(defun write-to-file (file-path string)
  (with-temp-buffer (insert string)
		    (write-region (point-min) (point-max) file-path)))

(defun read-from-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defconst test-file-content
  (concat "import unittest\n"
	  "\n"
	  "def test_free_standing_one():\n"
	  "#markerone\n"
	  "    pass\n"
	  "\n"
	  "class AblTest(unittest.TestCase):\n"
	  "#markertwo\n"
	  "    def test_abl_mode(self):\n"
	  "        self.fail('A FAILING ' + 'TEST')\n"
	  "\n"
	  "    def test_other_thing(self):\n"
	  "        pass"
	  "\n"
	  "    def test_one_more_thing(self):\n"
	  "        self.fail()"
	  "\n"
	  "def test_free_standing_two():\n"
	  "#markerthree\n"
	  "    pass\n"
	  ))

(defconst setup-content
  (concat "from setuptools import setup\n"
	  "setup(name='test')\n"))


(defun character-count (path char)
  (let ((count 0))
    (save-excursion
      (find-file path)
      (goto-char (point-min))
      (setq count (count-matches "t"))
      (kill-buffer))
    count))
(character-count "/Users/ulas/temp/yada.txt" "t")

(cl-defstruct (testenv
	       (:constructor new-testenv
			     (base-dir
			      &optional
			      (project-dir (f-join base-dir "aproject"))
			      (package-dir (f-join project-dir "apackage"))
			      (proof-dir (f-join base-dir "_proof"))
			      (setup-py-path (f-join project-dir "setup.py"))
			      (test-file-path (f-join project-dir "project_tests.py"))
			      (init-file-path (f-join package-dir "__init__.py")))))
  base-dir project-dir package-dir proof-dir setup-py-path test-file-path init-file-path)

(defun random-testenv ()
  (new-testenv (make-temp-file "abltest" 't)))

(defun testenv-init (env)
  ;; create git repo with setup.py and a test file. the folder structure will
  ;; look something like this (the temp directory name starting with abltest
  ;; will be different):
  ;;
  ;; /tmp
  ;;   |
  ;;   - abltest18945
  ;;        |
  ;;        - .git
  ;;        - _proof (dir)
  ;;        - aproject
  ;;           |
  ;;           - setup.py
  ;;           - project_tests.py (contents: test-file-content)
  ;;           - apackage
  ;;               |
  ;;               - __init__.py (contents: #nothing)
    (make-directory (testenv-base-dir env) 't)
    (cl-assert (s-contains? "Initialized empty Git repository"
			    (shell-command-to-string
			     (concat "git init -b trunk " (testenv-base-dir env)))))
    (make-directory (testenv-project-dir env))
    (make-directory (testenv-package-dir env))
    (make-directory (testenv-proof-dir env))
    (write-to-file (testenv-setup-py-path env) setup-content)
    (write-to-file (testenv-test-file-path env) test-file-content)
    (write-to-file (testenv-init-file-path env) "#nothing")
    (shell-command-to-string
     (format
      "cd %s && git add setup.py && git add %s && git commit -am 'haha'"
      (testenv-base-dir env)
      (testenv-project-dir env)))
    env)

(defun testenv-project-name (env)
  (abl-last-path-comp (testenv-project-dir env)))

(defun testenv-base-dirname (env)
  (abl-last-path-comp (directory-file-name (testenv-base-dir env))))

(defun testenv-proof-file (env)
  (f-join (testenv-proof-dir env) "out"))

(defun testenv-branch-git (env branch-name)
  (shell-command-to-string (format
			    "cd %s && git checkout -b %s"
			    (testenv-base-dir env) branch-name)))

(defun cleanup (path)
  ;; rm -rf's a folder which begins with /tmp. you shouldn't put
  ;; important stuff into /tmp.
  (unless (or (s-starts-with? "/tmp" path) (s-starts-with? "/var/folders/" path))
    (error
     (format "Tried to cleanup a path (%s) not in /tmp; refusing to do so."
	     path)))
   (if (file-directory-p path) (delete-directory path 't)))

(defmacro abl-git-test (&rest tests-etc)
  "Macro for tests. The first argument determines whether a dummy
vem is created."
  `(let* ((env (testenv-init (random-testenv)))
	  ;; (vem-proof-file-path (format "%s/_proof/proveit.txt" base-dir))
	  ;; (test-proof-file-path (format "%s/_proof/prove_test.txt" base-dir))
	  ;; (run-proof-file-path (format "%s/_proof/prove_run.txt" base-dir))
	  )
     (unwind-protect
	 (progn
	   ,@tests-etc)
       (cleanup (testenv-base-dir env))
       (dolist (buffer (buffer-list))
	 (if (s-starts-with? abl-mode-branch-shell-prefix (buffer-name buffer))
	     (let ((kill-buffer-query-functions nil))
	       (kill-buffer buffer))))
       )))

(defun abl-values-for-path (path)
  (let ((buffer (find-file path)))
    (list
     (buffer-local-value 'abl-mode buffer)
     (buffer-local-value 'abl-mode-branch buffer)
     (buffer-local-value 'abl-mode-project-name buffer)
     (buffer-local-value 'abl-mode-vem-name buffer)
     (buffer-local-value 'abl-mode-shell-name buffer))))

;; -----------------------------------------------------------------------------------------
;; Tests start here

(ert-deftest test-abl-capitalized ()
  (should (abl-capitalized? "Hello"))
  (should-not (abl-capitalized? "hello"))
  )


(ert-deftest test-git-utils ()
  (abl-git-test
   (should (string-equal (abl-git-branch (testenv-project-dir env)) "trunk"))
   (testenv-branch-git env "yello")
   (should (string-equal (abl-git-branch (testenv-project-dir env)) "yello"))
   (cleanup (concat (testenv-base-dir env) "/.git"))
   (should-not (abl-git-branch (testenv-project-dir env)))
   ))


(ert-deftest test-project-vars ()
  (abl-git-test
    (should (string-equal (abl-git-branch (testenv-project-dir env)) "trunk"))
    (should (string-equal (abl-get-project-name (testenv-project-dir env)) "aproject"))
    (should (string-equal (abl-make-ve-name "trunk" "project")
     			  "project_trunk"))
))


(ert-deftest test-no-vc-abl ()
  (abl-git-test
   (cleanup (f-join (testenv-base-dir env) ".git"))
   (let ((test-buffer (find-file (testenv-test-file-path env))))
     (should (buffer-local-value 'abl-mode test-buffer))
     (should (string-equal (buffer-local-value 'abl-mode-project-name test-buffer) "aproject"))
     (should-not (buffer-local-value 'abl-mode-branch test-buffer))
     (should (string-equal (buffer-local-value 'abl-ve-name test-buffer) "aproject"))
)))


(ert-deftest test-git-abl ()
  (abl-git-test
   (let ((test-buffer (find-file (testenv-test-file-path env))))
     (should (buffer-local-value 'abl-mode test-buffer))
     (should (string-equal (buffer-local-value 'abl-mode-branch test-buffer)
			   "trunk"))
     (should (string-equal (buffer-local-value 'abl-mode-project-name test-buffer) "aproject"))
     (should (string-equal (buffer-local-value 'abl-ve-name test-buffer) "aproject_trunk"))
     (should (string-equal (buffer-local-value 'abl-mode-shell-name test-buffer) "ABL-SHELL:aproject_trunk"))
)))


(ert-deftest test-branched-git-abl ()
  (abl-git-test
   (testenv-branch-git env "gitbranch")
   (let ((test-buffer (find-file (testenv-test-file-path env))))
     (should (buffer-local-value 'abl-mode test-buffer))
     (should (string-equal (buffer-local-value 'abl-mode-branch test-buffer)
			   "gitbranch"))
     (should (string-equal (buffer-local-value 'abl-mode-project-name test-buffer) "aproject"))
     (should (string-equal (buffer-local-value 'abl-ve-name test-buffer) "aproject_gitbranch"))
     (should (string-equal (buffer-local-value 'abl-mode-shell-name test-buffer) "ABL-SHELL:aproject_gitbranch"))
)))

(ert-deftest test-test-at-point ()
  (abl-git-test
   (find-file (testenv-test-file-path env))
   (goto-char (point-min))
   (should (string-equal (abl-mode-get-test-entity) "project_tests.py"))
   (search-forward "markerone")
   (should (string-equal (abl-mode-get-test-entity) "project_tests.py::test_free_standing_one"))
   (search-forward "markertwo")
   (should (string-equal (abl-mode-get-test-entity)
			 "project_tests.py::AblTest"))
   (search-forward "self.fail")
   (should (string-equal (abl-mode-get-test-entity)
			 "project_tests.py::AblTest::test_abl_mode"))
   (search-forward "pass")
   (should (string-equal (abl-mode-get-test-entity)
			 "project_tests.py::AblTest::test_other_thing"))
   (search-forward "markerthree")
   (should (string-equal (abl-mode-get-test-entity) "project_tests.py::test_free_standing_two"))
))

(ert-deftest test-dot-file ()
  (abl-git-test
   (write-to-file (f-join (testenv-project-dir env) ".abl")
		  "abl-ve-name \"VENAME\"\nabl-mode-shell-name \"SHELLNAME\"")
   (find-file (testenv-test-file-path env))
   (should (string-equal abl-ve-name "VENAME"))
   (should (string-equal abl-mode-shell-name "SHELLNAME"))))

(ert-deftest test-ve-check-and-activation ()
  (let ((collected-msgs '())
	(output-dir (make-temp-file "testout" 't)))
    ;; mock the read-from-minibuffer used for getting user input
    (cl-letf (((symbol-function 'read-from-minibuffer)
	       (lambda (msg)
	       (setq collected-msgs (append collected-msgs (list msg)))
	       (if (= (length collected-msgs) 1)
		   "test-ve"
		 "y"))))
      (abl-git-test
       (find-file (testenv-test-file-path env))
       (setq abl-mode-ve-create-command (concat "cd " output-dir " && touch %s"))
       (abl-mode-exec-command "ls")
       (sleep-for 1)
       (file-exists-p (f-join output-dir "test-ve"))
       ))))

(ert-deftest test-running-tests ()
  (abl-git-test
   (find-file (testenv-test-file-path env))
   (let ((shell-name abl-mode-shell-name))
     (setq abl-mode-check-and-activate-ve nil)
     (abl-mode-run-test-at-point)
     (let ((shell-buffer (get-buffer shell-name)))
       (should shell-buffer)
       (switch-to-buffer shell-buffer)
       ;; f**k it, we'll do it with sleep. I'll fix it later.
       (sleep-for 2)
       (goto-char (point-min))
       (should (search-forward "A FAILING TEST" nil t))
       (goto-char (point-min))
       (should (= (count-matches "A FAILING TEST") 1))
     ))))

(ert-deftest test-rerun-last ()
  (abl-git-test
   (find-file (testenv-test-file-path env))
   (let ((shell-name abl-mode-shell-name)
	 (new-test-file-path
	      (replace-regexp-in-string
	       "project_tests" "other_tests" (testenv-test-file-path env))))
     (setq abl-mode-check-and-activate-ve nil)
     (abl-mode-run-test-at-point)
     (sleep-for 1)
     (while (abl-shell-busy shell-name) (sleep-for 1))
     (should (gethash shell-name abl-mode-last-tests-run))
     (switch-to-buffer shell-name)
     (goto-char (point-min))
     (should (= (count-matches "A FAILING TEST") 1))

     ;; we want to make sure the test from project_tests.py is executed
     (copy-file (testenv-test-file-path env) new-test-file-path)
     (find-file new-test-file-path)
     (setq abl-mode-check-and-activate-ve nil)
     (abl-mode-rerun-last-test)
     (sleep-for 1)
     (while (abl-shell-busy shell-name) (sleep-for 1))
     (switch-to-buffer shell-name)
     (goto-char (point-min))
     (should (= (count-matches "A FAILING TEST") 2))
)))


(ert-deftest test-replacement-ve ()
  (let* ((output-dir (make-temp-file "testout" 't))
	 (ve-dir (make-temp-file "ves" 't))
	 (collected-msgs '())
	 (replacement-ve-name "test-ve")
	 (ve-out-file (f-join output-dir replacement-ve-name)))
    (write-to-file (f-join ve-dir replacement-ve-name) "blah")

    (cl-letf (((symbol-function 'read-from-minibuffer)
	       (lambda (msg)
		 (setq collected-msgs (append collected-msgs (list msg))) "test-ve")))
	      (abl-git-test
	       (find-file (testenv-test-file-path env))
	       (setq abl-mode-ve-activate-command
		     (concat "cd " output-dir " && echo 't' >> %s"))
	       (setq abl-mode-ve-base-dir ve-dir)
	       (save-excursion (abl-mode-exec-command "ls"))
	       (sleep-for 1)
	       (should (file-exists-p ve-out-file))
	       (should (= (character-count ve-out-file "t") 1))
	       (should (= (length collected-msgs) 1))
	       (should (string-equal (gethash abl-mode-shell-name
					      abl-mode-replacement-vems nil)
				     replacement-ve-name))
	       (should (string-equal abl-ve-name replacement-ve-name))

	       (let ((new-test-file-path
		      (replace-regexp-in-string
		       "project_tests" "other_tests" (testenv-test-file-path env))))
		 (copy-file (testenv-test-file-path env) new-test-file-path)
		 (find-file new-test-file-path)
		 (setq abl-mode-ve-activate-command
		       (concat "cd " output-dir " && echo 't' >> %s"))
		 (setq abl-mode-ve-base-dir ve-dir)
		 (abl-mode-exec-command "ls")
		 (sleep-for 1)
		 (should (= (character-count ve-out-file "t") 2))
		 (should (= (length collected-msgs) 1))
		 ))
)))
