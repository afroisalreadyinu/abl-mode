========
abl-mode
========

The abl-mode is for python programmers who develop using virtual
environments, version control branches and unit tests. To install
abl-mode, put abl.el on your elisp path, and include the following in
your emacs configuration file:

(require 'abl)

If you want to activate abl-mode automatically, add the following to
your configuration:

(add-hook 'find-file-hooks 'abl-mode-hook)

You can replace find-file-hooks with python-mode-hook if you want to
activate abl-mode only for python files.

=======================================
Commands and their default key bindings
=======================================

+----------+-------------------------------------+
|M-x       |Toggle abl-mode                      |
|abl-mode  |                                     |
+----------+-------------------------------------+
|C-c w     |Which branch am I on?  (If           |
|          |you're in a git repository, the      |
|          |outcome of the 'git branch'          |
|          |command; if you're in a              |
|          |subversion repository, the name      |
|          |of the directory)                    |
|          |                                     |
|          |                                     |
|          |                                     |
|          |                                     |
|          |                                     |
|          |                                     |
+----------+-------------------------------------+
|C-c t     |Run test at point (function if       |
|          |cursor is inside a function,         |
|          |class if cursor is inside a          |
|          |class but not in a function,         |
|          |file if cursor is at the first       |
|          |line of a file)                      |
|          |                                     |
|          |                                     |
+----------+-------------------------------------+
|C-c u     |Rerun last test entity               |
|          |(whatever was run the last time      |
|          |with C-c t)                          |
+----------+-------------------------------------+
|C-c r     |Run the web server for the           |
|          |branch you're on (or whatever        |
|          |else you have customized it to       |
|          |be)                                  |
+----------+-------------------------------------+
|C-c s     |Run the python shell for the         |
|          |virtualenv of this branch            |
|          |                                     |
+----------+-------------------------------------+
|C-c o     |Open the python entity at point. The |
|          |python entity must be in the format  |
|          |package.name:ClassName.function_name.|
|          |                                     |
|          |                                     |
+----------+-------------------------------------+
|C-c a     |Reload all buffers that have no      |
|          |unsaved changes (useful if you       |
|          |switch branches on git;              |
|          |notifies you in case there were      |
|          |buffers with unsaved changes)        |
+----------+-------------------------------------+

=============
Customization
=============

You can set any of the following options by including a line such as
the following in your .emacs file:

(setq option-name new-value)

String formatting is similar to the way it is done in Python; %s gets
replaced by the relevant value when the option gets used.

+---------------------+--------------------------------------+
|nose-command         |The command which is used to run unit |
|                     |tests. Default: nose -s %s            |
|                     |                                      |
|                     |                                      |
|                     |                                      |
+---------------------+--------------------------------------+
|vem-activate-command |The command used for activating a     |
|                     |python virtual environment. Default:  |
|                     |vem_activate %s                       |
+---------------------+--------------------------------------+
|vem-create-command   |The command used for activating a     |
|                     |python virtual environment.  Default: |
|                     |vem create %s                         |
+---------------------+--------------------------------------+
|vems-base-dir        |The directory in which new virtual    |
|                     |environments are stored. This should  |
|                     |be the same directory in which the    |
|                     |above mentioned vem commands create   |
|                     |the environments. Default:            |
|                     |~/.virtualenvs                        |
+---------------------+--------------------------------------+
|branch-shell-prefix  |The string appended to the name of the|
|                     |shell buffer created to run tests or  |
|                     |wsgi server. Default: ABL-SHELL:      |
+---------------------+--------------------------------------+
|abl-python-executable|The python executable used for        |
|                     |installing a python package (i.e. the |
|                     |python in “python setup.py develop”)  |
+---------------------+--------------------------------------+









