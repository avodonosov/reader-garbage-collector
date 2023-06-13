[Just an idea for now. Comments are welcome.
Known issue [#2](https://github.com/avodonosov/reader-garbage-collector/issues/2).]

People tend to use uninterned symbols in Common Lisp package definitions:

```common-lisp
    (defpackage #:my-pkg  (:export #:func-1 #:func-2) (:use #:cl))
    (in-package #:my-pkg)
```

But Common Lisp supports usual symbols:

```common-lisp
    (defpackage my-pkg (:export func-1 func-2) (:use cl))
    (in-package my-pkg)
```
and that is more readable, in my opinion.

An added bonus of usual symbol names in defpackage is that at GitHub,
when you click a symbol, GitHub pops-up on the right of the screen
all mentions of that symbol in the repo, so reader can quickly find
the definition for the symbol. This does not work for uninterned
symbols, because the search fails to match `#:some-function` in defpackage
with `some-function` in other files - the `#:` confuses the search.

The reason people use uninterned symbols is to avoid pollution of their
current package, or keyword package, with the symbols interned
when reader reads the defpacakge / in-package forms.

We could avoid this pollution while still keeping the usual (interned)
symbols by switching to a temporary package while
reading the forms:

```common-lisp

    ;; a garbage package where all pollution
    ;; symbols will be interned
    (cl:defpackage #:my-pkg-reader-garbage (:use))
    (cl:in-package #:my-pkg-reader-garbage)

    (cl:defpackage my-pkg
      (:export func-1
               func-2)
      (:use cl))

    ;; reader GC
    (cl:in-package #:cl-user)
    (eval-when (:load-toplevel :compile-toplevel :execute)
      (delete-package '#:my-pkg-reader-garbage)))

```

Creation and deletion of such a temporary garbage package
can further be hidden in a function specified for the `:around-compile`
ASDF option.

This library provides such a function.

How to use it:

```common-lisp

    ;;;; my-system.asd

    (defsystem "my-system"
                :depends-on ("reader-garbage-collector" ... )
                :around-compile "reader-gc:call-with-garbage-package"
                  ...
                )


    ;;;; package.lisp

    (defpackage my-pkg
      (:export func-1
               func-2)
      (:use cl))


```

For a runnable example see test-systems/good-system.asd
which prevents the pollution, in contrast to test-systems/bad-system.asd.
Also see how they are tested in test.lisp.

What is the most appropriate name for this functionality?
Is it really a "reader garbage collector",
or maybe it's a "symbol pollution sink"? Something else?

Further Thoughts.

Common Lisp could do that automatically. If symbol does not have a
binding (function binding, value binding, or other), and is not
reachable from any GC root other than a package object,
the symbol can be garbage collected, uninterning if necessary.

In other words, simply being interned into a package should
not prevent symbol from being garbage collected.

A special function (gc-symbols &optional package) could be provided
for user to invoke this process at any time, without waiting for garbage
collector to be fired automatically.

The (gc-symbols *package*) could be invoked at every iteration
of read/eval/print loop. So it will become an read/eval/print/gc-symbols loop.
(Or better read/eval/gc-symbols/print loop?)

The compile-file function could also call (gc-symbols *package*) in the end.

I may be missing something. Maybe there are good reasons to not have
it in the language.

More practically, ASDF could probably include such a call-with-garbage-package
functionality out of box, and probably even use it around compilation
by default.

Another though. Maybe readability is so more important than pollution,
that it's better to stick to usual symbols even without bothering
for any anti-pollution measures. Especially that
this pollution only happens when the system is compiled,
i.e. loaded first time. Next time, when existing .fasl files
are just loaded without compilation, the pollution does not happen,
if lisp implementation follows the CLHS reccomendation:

> The macroexpansion of defpackage could usefully canonicalize
  the names into strings, so that even if a source file has
  random symbols in the defpackage form, the compiled file would
  only contain strings.

http://clhs.lisp.se/Body/m_defpkg.htm