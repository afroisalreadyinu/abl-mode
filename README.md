# abl-mode

The abl-mode is a minor mode for python programmers who develop using virtual
environments, version control branches and unit tests.

## What does it do?

Abl-mode is ideal for developers who switch development branches frequently, and
create python virtual environments for these individual branches. When you run a
test with `C-c t` or start a server with `C-c r`, abl-mode checks the
availability of a virtual environment for the branch you're on. The name of the
virtual environment is determined according to the name of the project and the
VCS branch. If there is no such virtual environment, the user is prompted either
for the name of a replacement virtual environment, or `y` for creating a new
virtual environment. Once you've run a test, and made some changes depending on
the results, you can rerun the same test with `C-c u`.

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

<table>
<tr><th width="25%">Key</th><th>Function</th></tr>
<tr><td><code>M-x abl-mode</code></td><td>Toggle abl-mode</td></tr>
<tr><td><code>C-c t</code></td><td>Run test at point. See below for detailed explanation</td></tr>
<tr><td><code>C-c u</code></td><td>Rerun last test entity (whatever was run the last time with C-c t)</td></tr>
<tr><td><code>C-c s</code></td><td>Run the python shell for the virtualenv of this branch</td></tr>
<tr><td><code>C-c o</code></td><td>Open the python entity at point. The python entity must be in the format <code>package.name:ClassName.function_name</code></td></tr>
</table>

## Running Tests

Here is how the tests to run are determined:

If the name of the file does not match `abl-mode-test-file-regexp`, it is
assumed that this is not a test file, but a code file. In that case, the
beginning of the file is searched (using the regular expressions in
`abl-mode-code-file-tests-regexps`) for a line that lists the tests for that
file. The format for this line is `tests: wherever/the_tests_are.py`. If no
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

<table>
<tr><th width="40%">Option</th><th>Function</th></tr>
<tr><td><code>abl-mode-check-and-activate-ve</code></td><td>Whether a the existence of a suitable virtual environment should be checked, and the virtualenv activation command should be included with each command run. Default: <code>t</code></td></tr>

<tr><td><code>abl-mode-ve-activate-command</code></td><td> The command used for activating a python virtual environment. Default: <code>workon %s</code></td></tr>

<tr><td><code>abl-mode-ve-create-command</code></td><td> The command used for activating a python virtual environment. Default: <code>mkvirtualenv create %s</code></td></tr>

<tr><td><code>abl-mode-test-command</code></td><td>The command used to run unit tests. Default: <code>python -m unittest %s</code></td></tr>

<tr><td><code>abl-mode-ve-base-dir</code></td><td>The directory in which new virtual environments are stored. This should be the same directory in which the above mentioned virtualenv commands create the environments. Default: </codde>~/.virtualenvs</code></td></tr>

<tr><td><code>abl-mode-install-command</code></td><td> The command used to install a development package. This command is run to install dependencies when a virtualenv is created. Default: <code>python setup.py develop</code></td></tr>

<tr><td><code>abl-mode-test-file-regexp</code></td><td>Regular expression used to determine whether a file contains tests.  Default: <code>.*_tests.py</code></td></tr>

<tr><td><code>abl-mode-code-file-tests-regexps</code></td><td>List of regular expressions used to search for a test files list in a code file.</td></tr>

<tr><td><code>abl-mode-branch-shell-prefix</code></td><td> The string appended to the name of the shell buffer created to run tests. Default: <code>ABL-SHELL:</code></td></tr>
</table>

## Running the tests

```
export HOME=/tmp/temporaryhome
mkdir -p $HOME/.emacs.d
cp testing-files/init.el $HOME/.emacs.d
emacs -L . -l ert -l $HOME/.emacs.d/init.el  -l test.el --batch -f ert-run-tests-batch-and-exit
```

This will run all the tests in `test.el`. If you want to run a single test, use
the following, more complicated command:

```
emacs -L . -l ert -l $HOME/.emacs.d/init.el  -l test.el --batch --eval '(let ((ert-quiet t)) (ert-run-tests-batch-and-exit "test-git-abl"))'
```

## TODOs

[ ] Fix failing tests & improve testing
[ ] abl-mode for elisp
[ ] Option to add an argument to test run with C-u (e.g. -x for pytest)
