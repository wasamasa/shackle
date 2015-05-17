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

Install via Marmalade or MELPA via ``M-x package-install RET shackle``
or download ``shackle.el``, place it into a suitable location such as
``~/.emacs.d/vendor/`` and add the following to your init file:

.. code:: elisp

    (add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/"))

Usage
-----

First you need to customize ``shackle-rules``, this can be done via
``M-x customize-group RET shackle`` or in your init file.

As the name of the variable suggests, it's a list of rules.  Each rule
consists of a condition and a set of key-value combinations that tell
what to do with the buffer in question.

The condition can be either a symbol, a string, a list of either or
``t``.  A symbol is interpreted as the major mode of the buffer to
match, a string as the name of the buffer (which can be turned into
regexp matching by using the ``:regexp`` key with a value of ``t`` in
the key-value part), a list groups either symbols or strings (as
described earlier) while requiring at least one element to match and
``t`` as the fallback rule to follow when no other match succeeds.  If
you set up a fallback rule, make sure it's the last rule in
``shackle-rules``, otherwise it will always be used.

The following key-value pairs are available:

- ``:select`` and ``t``:

  Select the popped up window.  The ``shackle-select-reused-windows``
  option makes this the default for windows already displaying the
  buffer.

- ``:same`` and ``t``:

  Display buffer in the current window.

- ``:popup`` and ``t``:

  Pop up a new window instead of displaying the buffer in the current
  one.

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

- ``:frame`` and ``t``:

  Pop buffer to a frame instead of a window.

To have an exception to a fallback rule, use the condition of your
choice and either don't list the key-value pair, use a different value
or use a placeholder key with any value.

The following example configuration enables the rather radical
behaviour of always reusing the current window in order to avoid
unwanted window splitting:

.. code:: elisp

    (setq shackle-rules
          '((t :same t)))

This one on the other hand provides a less intrusive user experience
to select all windows by default unless they are spawned by
``compilation-mode`` and demonstrates how to use exceptions:

.. code:: elisp

    (setq shackle-rules
          '((compilation-mode :noselect t)
            (t :select t)))

My final example tames `helm <https://github.com/emacs-helm/helm>`_
windows by aligning them at the bottom with a ratio of 40%:

.. code:: elisp

    (setq shackle-rules
          '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)))

Once you're done customizing ``shackle-rules``, use ``M-x
shackle-mode`` to enable ``shackle`` interactively.  To enable it
automatically on startup, add ``(shackle-mode)`` to your init file.

Internals
---------

``shackle`` adds an extra entry to ``display-buffer-alist``, a
customizable variable in Emacs that specifies what to do with buffers
displayed with the ``display-buffer`` function.  It's used by quite a
lot of Emacs packages, including very essential ones like the built-in
help and compilation package.

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
