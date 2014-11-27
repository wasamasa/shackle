shackle
=========

.. image:: https://raw.githubusercontent.com/wasamasa/shackle/master/img/shackle.gif

About
-----

``shackle`` gives you the means to put an end to popped up buffers not
behaving they way you'd like them to.  By setting up simple rules you
can for instance make Emacs always select help buffers for you or make
everything reuse your currently selected window.

Installation
------------

Install via `quelpa <https://github.com/quelpa/quelpa>`_ with ``M-:
(quelpa '(shackle :fetcher github :repo "wasamasa/shackle"))`` for the
time being or download ``shackle.el``, place it into a suitable
location such as ``~/.emacs.d/vendor/`` and add the following to your
init file:

.. code:: cl

    (add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/"))

Usage
--------------

First you need to customize ``shackle-rules``, this can be done via
``M-x customize-group RET shackle`` or in your init file.

As the name of the variable suggests, it's a list of rules.  Each rule
consists of a condition and a set of key-value combinations that tell
what to do with the buffer in question.

The condition can be either a symbol, a string or ``t``.  A symbol is
interpreted as the major mode of the buffer to match, a string as the
name of the buffer (which can be turned into regexp matching by using
the ``:regexp`` key with a value of ``t`` in the key-value part) and
``t`` as the fallback rule to follow when no other match succeeds.  If
you set up a fallback rule, make sure it's the last rule in
``shackle-rules``, otherwise it will always be used.

The following key-value pairs are available:

- ``:select`` and ``t``:

  Select the popped up window.  Can be used standalone or in
  combination with ``:reuse`` to make reused windows selected.  The
  ``shackle-select-reused-windows`` option makes this the default for
  reused windows.

- ``:same`` and ``t``:

  Reuse the current window.

- ``:reuse`` and ``t``:

  Attempt reusing a window that's already displaying the buffer to be
  displayed.  This option only makes sense with
  ``shackle-preserve-emacs-defaults`` set to ``nil``. It can be used
  this way to have the described behaviour on a case-by-case basis
  instead for everything by default.  Another way of using it may be
  in the fallback rule to only alter ``switch-to-buffer`` to pop up
  windows instead while keeping this Emacs default.

- ``:align`` and ``'above``, ``'below``, ``'left``, ``'right``, or
  ``t``:

  Align a new window at the respective side of the current frame or
  with the default alignment (customizable with
  ``shackle-default-alignment``) by deleting every other window than
  the currently selected one, then wait for the window to be "dealt"
  with.  This can either happen by burying its buffer with ``q`` or by
  deleting its window with ``C-x 0``.

- ``:ratio`` and a floating point value between 0 and 1:

  Aligned window use a default ratio of 0.5 to split up the original
  window in half (customizable with ``shackle-default-ratio``), the
  ratio can be changed on a per-case basis by providing a different
  floating point value like 0.33 to make it occupy a third of the
  original window's size.

- ``:defer`` and ``t``:

  Not all aligned windows behave equally and can be expected to be
  "dealt" with immediately.  An example for such an exception would be
  `helm <https://github.com/emacs-helm/helm>`_ which pops up a window
  and cleans up its buffers and finally the window itself.  To
  accomodate for this behaviour, use this key-value pair.  To detect
  whether it's needed at all, use the ``:align`` key with the ``t``
  value on its own and check whether the window in question throws
  errors.

- ``:frame`` and ``t``:

  Pop buffer to a frame instead of a window.

To have an exception to a fallback rule, use the condition of your
choice and either don't list the key-value pair, use a different value
or use a placeholder key with any value.

The following example configuration enables the rather radical
behaviour of always reusing the current window in order to avoid
unwanted window splitting:

.. code:: cl

    (setq shackle-rules
          '((t :reuse t)))

This one on the other hand provides a less intrusive user experience
and demonstrates how to use exceptions:

.. code:: cl

    (setq shackle-rules
          '((compilation-mode :noselect t)
            (t :select t)))

Once you're done customizing ``shackle-rules``, use ``M-x
shackle-mode`` to enable ``shackle`` interactively.  To enable it
automatically on startup, add ``(shackle-mode -1)`` to your init file.

Internals
---------

``shackle`` adds an extra entry to ``display-buffer-alist``, a
customizable variable in Emacs that specifies what to do with buffers
displayed with the ``display-buffer`` function.  It's used by quite a
lot of Emacs packages, including very essential ones like the built-in
help and compilation package.  There is a
``shackle-preserve-emacs-defaults`` option you can set to ``nil`` to
make ``shackle`` completely ignore the defaults Emacs is using for
``display-buffer``, such as reusing windows already displaying the
target buffer or making ``switch-to-buffer`` not reuse the currently
selected window.

This means other Emacs packages that neither use the
``display-buffer`` function directly nor indirectly won't be
influenced by ``shackle``.  If you should ever come across a package
that ought to use it, but doesn't conform, chances are you'll have to
speak with upstream instead of me to have it fixed.  Another thing to
be aware of is that if you've set up a fallback rule, it may take over
the Emacs defaults which can play less well with packages (such as
`Magit <http://github.com/magit/magit>`_).  Once you find out what's
causing the problem, you can add an exception rule to fix it.

Contributing
------------

If you find bugs, have suggestions or any other problems, feel free to
report an issue on the issue tracker or hit me up on IRC, I'm always on
``#emacs``.  Patches are welcome, too, just fork, work on a separate
branch and open a pull request with it.

Alternatives
------------

This package is heavily inspired by `popwin
<https://github.com/m2ym/popwin-el>`_ and was hacked together after
discovering it being hard to debug, creating overly many timers and
exposing rather baffling bugs.  ``shackle`` being intentionally
simpler and easier to understand is considered a debugging-friendly
feature, not a bug.  However if you prefer less rough edges, a
sensible default configuration and having more options for
customizing, give ``popwin`` a try.
