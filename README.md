# abl-mode

The abl-mode is a minor mode for python programmers who develop using virtual
environments, version control branches and unit tests.

## What does it do?

Abl-mode is ideal for developers who switch development branches frequently, and
create python virtual environments for these individual branches. When you run a
test with ``C-c t`` or start a server with ``C-c r``, abl-mode checks the
availability of a virtual environment for the branch you're on. The name of the
virtual environment is determined according to the name of the project and the
VCS branch. If there is no such virtual environment, the user is prompted either
for the name of a replacement virtual environment, or ``y`` for creating a new
virtual environment. Once you've run a test, and made some changes depending on
the results, you can rerun the same test with ``C-c u``.

## Installing

To install abl-mode, put abl.el on your elisp path, and include the following in
your emacs configuration file::

    (require 'abl-mode)

If you want to activate abl-mode automatically for python files, add the
following to your Emacs configuration::

    (add-hook 'find-file-hooks 'abl-mode-hook)

You can replace find-file-hooks with python-mode-hook if you want to activate
abl-mode only for python files.

## Commands and their default key bindings

+----------+-------------------------------------------+
|M-x       |Toggle abl-mode                            |
|abl-mode  |                                           |
+----------+-------------------------------------------+
|C-c t     |Run test at point. See below for detailed  |
|          |explanation.                               |
|          |                                           |
|          |                                           |
+----------+-------------------------------------------+
|C-c u     |Rerun last test entity (whatever was run   |
|          |the last time with C-c t)                  |
|          |                                           |
+----------+-------------------------------------------+
|C-c s     |Run the python shell for the virtualenv of |
|          |this branch                                |
|          |                                           |
+----------+-------------------------------------------+
|C-c o     |Open the python entity at point. The python|
|          |entity must be in the format               |
|          |``package.name:ClassName.function_name``.  |
|          |                                           |
|          |                                           |
+----------+-------------------------------------------+

## Running Tests

Here is how the tests to run are determined:

If the name of the file does not match ``abl-mode-test-file-regexp``, it is
assumed that this is not a test file, but a code file. In that case, the
beginning of the file is searched (using the regular expressions in
``abl-mode-code-file-tests-regexps``) for a line that lists the tests for that
file. The format for this line is ``tests: wherever/the_tests_are.py``. If no
such header is found, an error is shown.

If the file is a test file, which test is run depends on the location of the
cursor. If it is at the start of the file, the whole file is run. If it is in an
individual test, that test is run. If it is in a test class but not in a test,
that test class is run.

## Customization

You can set any of the following options either by including a line such as the
following in your .emacs file::

    (setq option-name new-value)

Or by listing project-local customization options in a file named `.abl` at the
base level of a project, i.e. where the `setup.py` file is located. The
customization line in such a file should start with the option name, followed by
the value, such as::

    abl-mode-test-command "ENV_VAR=\"env var value\"" pytest -x %s"

As you can see here, the value should be as you would include it in your
configuration; it is read and evaluated, so that logical values such as nil or t
can also be used. String formatting is similar to the way it is done in Python;
%s gets replaced by the relevant value when the option gets used.


+-------------------------------------+------------------------------------------+
|abl-mode-check-and-activate-ve       |Whether a the existence of a suitable     |
|                                     |virtual environment should be checked, and|
|                                     |the virtualenv activation command should  |
|                                     |be included with each command             |
|                                     |run. Default: t                           |
+-------------------------------------+------------------------------------------+
|abl-mode-ve-activate-command         |The command used for activating a python  |
|                                     |virtual environment. Default: workon %s   |
|                                     |                                          |
|                                     |                                          |
|                                     |                                          |
+-------------------------------------+------------------------------------------+
|abl-mode-ve-create-command           |The command used for activating a python  |
|                                     |virtual environment.  Default:            |
|                                     |mkvirtualenv create %s                    |
|                                     |                                          |
+-------------------------------------+------------------------------------------+
|abl-mode-test-command                |The command used to run unit tests.       |
|                                     |Default: nose -s %s                       |
|                                     |                                          |
|                                     |                                          |
|                                     |                                          |
+-------------------------------------+------------------------------------------+
|abl-mode-ve-base-dir                 |The directory in which new virtual        |
|                                     |environments are stored. This should be   |
|                                     |the same directory in which the above     |
|                                     |mentioned virtualenv commands create the  |
|                                     |environments. Default: ~/.virtualenvs     |
|                                     |                                          |
+-------------------------------------+------------------------------------------+
|abl-mode-install-command             |The command used to install a development |
|                                     |package. This command is run to install   |
|                                     |dependencies when a virtualenv is created.|
|                                     |Default is "python setup.py develop".     |
|                                     |                                          |
+-------------------------------------+------------------------------------------+
|abl-mode-test-file-regexp            |Regular expression used to determine      |
|                                     |whether a file contains tests.  Default:  |
|                                     |".*_tests.py"                             |
|                                     |                                          |
+-------------------------------------+------------------------------------------+
|abl-mode-code-file-tests-regexps     |List of regular expressions used to search|
|                                     |for a test files list in a code file.     |
|                                     |                                          |
+-------------------------------------+------------------------------------------+
|abl-mode-branch-shell-prefix         |The string appended to the name of the    |
|                                     |shell buffer created to run tests or wsgi |
|                                     |server. Default: ``ABL-SHELL:``           |
+-------------------------------------+------------------------------------------+
