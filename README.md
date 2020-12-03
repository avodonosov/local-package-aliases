Local Package Aliases
======================

Allows to define inside of a package aliases to refer other packages.
Provides a reader macro `$` to use the aliases (active only in packages having
alias mapping, and does not affect other code).
Portable.
  
``` common-lisp
(ql:quickload :local-package-aliases)

(defpackage com.my-company.some-library
  (:use cl)
  (:export #:func))

(in-package #:com.my-company.some-library)

(defun func () "hello")



(defpackage some-application (:use cl))
(in-package #:some-application)
(local-package-aliases:set #:com.my-company.some-library #:lib
                           #:some.other.library #:olib)

(read-from-string "$lib:func")
;; => COM.MY-COMPANY.SOME-LIBRARY:FUNC

($lib:func)
;; => "hello"

;; The aliases are not global; they are scoped only to the package
;; where they are defined:

(in-package #:cl-user)
(read-from-string "$lib:func")
;; => ERROR There is no package named "$LIB" .

;; The $ reader macro is non-terminating,
;; therefore it is only activated when $
;; is on the beginning of a token.
;; $ in the middle of a token has no special
;; effect.
(in-package #:some-application)

(read-from-string "just-a-$-symbol")
;; => JUST-A-$-SYMBOL

;; The $ designates alias reference only if the
;; current package has defined mappings. If there
;; is no alias mapping in the current package, then
;; $ is interpreted as usual:

(local-package-aliases:set) ; installs an empty alias mapping
(read-from-string "$-a-sybmol")
;; => $-A-SYMBOL

(in-package #:cl-user)
(read-from-string "$-a-sybmol")
;; => $-A-SYMBOL

;; However, CCL and CLISP signal an error when read
;; uninterned symbols whose name starts with a
;; macro-character, in our case $. For example #:$abcd.
;; See https://lists.clozure.com/pipermail/openmcl-devel/2013-February/009911.html
;; Therefore we do not recommend enabling the
;; $ macro globally, from your lisp implementation
;; init file - libraries that have such uninterned symbols
;; in their code will fail to parse.
;;
;; But for your own code, don't use symbols like #:$abcd
;;;and local-package-aliases will work.
```

To enable the `$` macro in your lisp session:
``` common-lisp
(local-package-aliases:set-aliasing-reader *readtable*)
```

A macro character other than `$` may be used.
See the docstring for `local-package-aliases:set-aliasing-reader`
for parameters description.

To return to the standard syntax:
``` common-lisp
(set-syntax-from-char #\$ #\$ *readtable* (copy-readtable nil))
```

To enable the `$` macro during compilation of ASDF systems,
use the `:around-compile` argument:

``` common-lisp
(asdf:defsystem #:some-application
  :depends-on (#:local-package-aliases
               #:com.my-company.some-library)
  :around-compile "local-package-aliases:call-with-aliasing-readtable"
  :components ((:file "some-application")))
```

SLIME support
-------------

SLIME uses the standard readtable rules for tokens such as `pkg:symb`
and doesn't undersdand our aliases; thus, symbol completion,
slime-edit-definition, and argument hints do not work out of
the box for aliased tokens like `$lib:func`.

The solution we found is to hook into swank, and wrap evaluation
of every SLIME request with temporary binding of the aliases defined
in the current package as nicknames for their corresponding packages.
So, during dynamic extent of every slime request, the aliases become
real package nicknames and SLIME can handle them as usually.

It must be noted that this solution is not entirely transparent:
when working from SLIME not only the reader follows the aliases,
but also `(find-package :$lib)` will find the package. During
normal run-time, only the reader knows about the aliases.

Functions `hook-into-swank` and `unhook-from-swank` enable/disable
this SLIME support.

To have the SLIME support enabled automatically add the following
to your _~/.swank.lisp_:
``` common-lisp
(when (find-package :local-package-aliases)
  (funcall (read-from-string "local-package-aliases:hook-into-swank")))
```
Or this in _~/.emacs_:
``` common-lisp
(add-hook 'slime-connected-hook
          (lambda ()
            (slime-eval '(cl:when (cl:find-package :local-package-aliases)
                            (cl:funcall (cl:read-from-string "local-package-aliases:hook-into-swank"))))))
```


Other Package Aliasing Approaches
=================================

Here is some information, solutions and ideas I encountered recently
related to package aliases.
  
The solutions vary in whether:
 - the aliases are only honored by the reader, or they
   also affect functions like `cl:find-package`, `cl:find-symbol`;
 - the solution is portable Common Lisp, or relies on patches
   or language extensions;
 - the aliases are scoped to a package, or somehow else;
 - the solution is specific about aliasing scheme,
   or is a lower-level tool allowing to build various
   aliasing approaches.

cl-package-aliases - http://www.cliki.net/cl-package-aliases
------------------------------------------------------------

Provides patches for 5 lisp implementations to introduce
aliases. Aliases are scoped to package and visible both
for reader and for standard functions like `cl:find-symbol`.

package-renaming - http://common-lisp.net/gitweb?p=users/frideau/package-renaming.git;a=tree
--------------------------------------------------------------------------------------------

Tools based on `cl:rename-package` to temporary give packages
desired short names/nicknames. Portable. To make the renaming
local it is expected to be used with the ASDF's `:around-compile` argument.

CL language extensions
----------------------

There were discussions to develop a CL language extension
and propose it to CL vendors. The extension might be
a hook called by CL to resolve package prefix, _i.e._ when
encountering a token like `pkg:symbol`, call the hook with
string "pkg", and the hook should return a package object,
or maybe just a string designating real package name.
Such hook may be called `*package-prefix-resolver*`.

Alternatively the hook may be passed the full token "pkg:symbol"
and resolve names for both packages and symbols.
Such hook may be named `*parse-token-hook*`.

There were considerations whether these hooks should be called only
from the reader, or also by `cl:find-sybmol` and other functions.
Sketch for a CDR: http://paste.lisp.org/display/133561
Discussions on the #lisp irc channel: 
http://ccl.clozure.com/irc-logs/lisp/2012-11/lisp-2012.11.05.txt
http://ccl.clozure.com/irc-logs/lisp/2013-01/lisp-2013.01.06.txt

One more possible language extension would be to allow to
fully substitute the lisp reader. In this case there might
be a public library implementing fully compliant CL reader.
Lisp implementations will delegate functions like `cl:read`,
`cl:read-delimeted-list`, `cl:set-syntax-from-char`
and others to the pluggable reader. The reader by default
honors `cl:*readtable*`, `cl:*package*` and other variables,
but also allows any custom hooks we need, such as described
above `parse-token-hook` and/or `package-prefix-resolver`.
It must be noted that interface between CL and such a pluggable
reader will consist of many functions. Also, the reader should
come with it's own implementations for all the reader macros,
because standard reader macros are not implemented in terms
of public `cl:*` functions, but use functions internal to the
CL reader's implementation, such as `read-token`.

If speaking about pluggable reader, it's necessary to mention
the reader-interception project:
http://common-lisp.net/cgi-bin/gitweb.cgi?p=users/frideau/reader-interception.git;a=tree;js=1

It's a portable solution allowing to plug-in your own reader.
It relies on the trick to look at the first character of input,
configure this character temporarily as a reader macro, and
then this reader macro may read the full input stream according
to any rules.

Conclusion
----------

The local-package-aliases approach with a reader macro seems
to be a decent approach.

It is comparable by convenience with package-renaming.

In my opinion a form of package aliasing deserves to be
introduced as a CL extension into all implementations.

The language extension may be specific, targeting only package
aliases (like cl-package-aliases project proposes).
It will encourage consistent coding practice across
all the CL programs.

To simplify adoption of the extension by the CL implementations,
I believe it should be enough to have aliasing only in reader.
Calls to `cl:find-package` are rare, and we can pass full package
names to it; on the other hand, if `cl:find-package` is unaware
of aliases, it may complicate support by SLIME.

As for more low-level language extensions, like pluggable
reader or various hooks, I would welcome them too,
just to make Lisp more programmable and allow programmers
to solve their needs simply.

Author
------
  Anton Vodonosov, avodonosov@yandex.ru

License
-------
MIT
