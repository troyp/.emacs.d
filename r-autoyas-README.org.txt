#+TITLE: R-autoyas
#+AUTHOR: Sven  Hartenstein & Matthew L. Fidler
* Library Information
 *r-autoyas.el* --- Provides automatically created yasnippets for R function argument lists.

 - Filename :: [[file:r-autoyas.el][r-autoyas.el]]
 - Description :: r-autoyas is a small ESS complement. It provides automatically created yasnippets for R function argument lists.
 - Author :: Sven Hartenstein & Matthew Fidler
 - Maintainer :: Matthew Fidler
 - Created :: Fri Mar 25 10:36:08 2011 (-0500)

 - Version :: 0.28
 - Last-Updated :: Mon Jun 25 15:12:20 2012 (-0500)
 -           By :: Matthew L. Fidler
 -     Update # :: 873

 - URL :: https://github.com/mlf176f2/r-autoyas.el
 - Keywords :: R yasnippet
Compatibility:

* About
  r-autoyas is a small ESS complement. It provides automatically created
  yasnippets for R function argument lists.

* Requirements
  The following are needed:
- yasnippet https://github.com/capitaomorte/yasnippet
- R & ESS
- R process must be running.
* Usage
- To expand the snipped type the function name and them press =TAB=.
- To jump from field to feild press =TAB=.  If you did not change the
  field, the parameter will be deleted from this list
- To exit the snipped and delete remaining arguments, press =C-g=
* Options
  This is an incomplete list of user definable options.  The complete
  list can be retrieved by 
  =M-x customize-group r-autoyas=
** Debugging
   Debugging messages can be put on-screen.  This is done by
   #+BEGIN_SRC emacs-lisp
(setq r-autoyas-debug t)
   #+END_SRC
** Sending a ... replacement to R via emacs instead of by a global options statement
   Uses Lisp-based dot-replacement defined by
   `r-autoyas-r-based-dot-replacement' instead of specifying through
   options in R startup.  This is on by default but can be turned off by
   #+BEGIN_SRC emacs-lisp
(setq r-autoyas-use-r-based-dot-replacement nil)
   #+END_SRC
** Specifying the `...' replacement via the R options() statement
   Emacs can change the functions `...' replacement through lisp.  The
   easiest way to change this is, typing:

   =M-x customize-variable r-autoyas-r-based-dot-replacement=
** Using functions within a namespace only
   By default, R-autoyas only expands predefined functions in
   namespaces/package that are loaded in R.  This ignores any
   user-defined functions.  However, R-autoyas may be used to expand
   user-defined functions as well.  This is done with the
   =r-autoyas-expand-package-functions-only= variable.  To turn on
   r-autoyas's expansion of user-defined functions, the following code
   may be used:
   #+BEGIN_SRC emacs-lisp
(setq r-autoyas-expand-package-functions-only nil)
   #+END_SRC

   This variable may also be customized.

* Limitations
- No nice error handling when no R process is found
- Partial nested support -- not perfected
* Loading r-autoyas in ~/.emacs
  You may use marmalade-repo and ELPA to install r-autoyas
  (http://marmalade-repo.org/), or put it into your load-path and put
  the following in ~/.emacs

  #+BEGIN_SRC emacs-lisp :results silent
(require 'r-autoyas)
(add hook 'ess-mode-hook 'r-autoyas-ess-activate)
  #+END_SRC


#  LocalWords:  yasnippets autoyas ESS Hartenstein
* Wish-List/To-Do
* Functions
** Interactive Functions

*** r-autoyas-defined-p
=(r-autoyas-defined-p &optional WITH-PAREN)=

Is the current function defined (like plot )

*** r-autoyas-ess-activate
=(r-autoyas-ess-activate)=

R autoyas ESS hook

*** r-autoyas-exit-snippet-delete-remaining
=(r-autoyas-exit-snippet-delete-remaining)=

Exit yas snippet and delete the remaining argument list.

*** r-autoyas-expand
=(r-autoyas-expand &optional RM-PAREN)=

Insert argument list for R function before the point as intelligent yas snippets and
expand the snippets.
RM-PAREN removes the inserted parenthesis

*** r-autoyas-expand-maybe
=(r-autoyas-expand-maybe &rest IGNORE)=

Might auto-expand snippet.

*** r-autoyas-paren
=(r-autoyas-paren)=

Function to allow Auto-yas to insert parenthesis

*** r-autoyas-wrap
=(r-autoyas-wrap)=

Wrap code

** Internal Functions

*** autopair-r-autoyas-paren-action
=(autopair-r-autoyas-paren-action ACTION PAIR POS-BEFORE)=

Autopair R autoyas paren-action

*** r-autoyas-active-field-number
=(r-autoyas-active-field-number &optional ARG)=

Get the active field position

*** r-autoyas-editing-field-num-p
=(r-autoyas-editing-field-num-p &optional ARG)=

Which field is active?

*** r-autoyas-generate-dotreplace-list
=(r-autoyas-generate-dotreplace-list)=

Generates dot-replace R-code

*** r-autoyas-generte-dotreplace-list-lisp
=(r-autoyas-generte-dotreplace-list-lisp FUNC)=

Generates dot-replacement yasnippet based on lisp options

*** r-autoyas-m
=(r-autoyas-m &rest OBJECTS)=

Message when debugging is on.

*** r-autoyas-namespace
=(r-autoyas-namespace FUNCTION-NAME)=

Returns the namespace for FUNCTION-NAME, or nil if it cannot be determined.

*** r-autoyas-preloaded-namespace-p
=(r-autoyas-preloaded-namespace-p NAMESPACE)=

Determines if NAMESPACE is preloaded in R.  It is based on the variable =r-autoyas-preloaded-packages=

*** r-autoyas-text-on-moving-away
=(r-autoyas-text-on-moving-away DEFAULT-TEXT &optional ORIG-TEXT)=

 - Changes text when moving away AND original text has not changed

*** r-autoyas-update
=(r-autoyas-update)=

Update fields

*** rayas-comma
=(rayas-comma FIELD NUM)=

Inserts comma and field number if needed

*** rayas-require-explicit-p
=(rayas-require-explicit-p NUM)=

Should the explicit x= be required?

*** rayas-space
=(rayas-space FIELD-NUMBER)=

Adds a dummy space so that reducing the yasnippet field to zero doesn't cause strange errors.
* Variables
** Customizable Variables

*** r-autoyas-auto-expand-with-paren
 - When true will automatically expand with the ( key.

*** r-autoyas-debug
Add debugging comments for=r-autoyas=

*** r-autoyas-echo-inject-commands
 - When true use =ess-eval-linewise= to echo the commands to the R process.  Otherwise use =ess-command= to quietly add the lines to the R process.

*** r-autoyas-expand-package-functions-only
Automatically expand only functions defined in a package/library.

*** r-autoyas-lisp-based-dot-replacement
Defines default the ... replacement using lisp.

*** r-autoyas-number-of-commas-before-return
Defines the number of commas before the snippet is inserted as:

plot(x= ,
     y=NULL,
     type='p',
     xlim=NULL,
     ylim=NULL,
     log='',
     main=NULL,
     sub=NULL,
     xlab=NULL,
     ylab=NULL,
     ann=par("ann"),
     axes=TRUE,
     frame.plot= ,
     panel.first=NULL,
     panel.last=NULL,
     asp=NA, ...)

insetad of:

plot(x= , y=NULL, type='p', xlim=NULL, ylim=NULL, log='', main=NULL, sub=NULL, xlab=NULL, ylab=NULL, ann=par("ann"), axes=TRUE, frame.plot= , panel.first=NULL, panel.last=NULL, asp=NA, ...)

If this number is zero or below, always insert as a single line.

*** r-autoyas-paren-ignored-functions
List of functions to ignore when creating auto-snippets by inserting a parenthesis

*** r-autoyas-preloaded-packages
List of preloaded packages in R.  All other packages need to be included by a require(package) or library(package) statement.

*** r-autoyas-r-based-dot-replacement
Defines default the ... replacement sent to the options() statement in R. quote() is used to keep the expression instead of evaluating it.

*** r-autoyas-remove-explicit-assignments
 - Remove explicit assignments when appropriate.

This option removes explicit assignments after tabbing away.  For example

write.table(x= ,
            file="",
            append=FALSE,
            quote=TRUE,
            sep=" ",
            eol="\n",
            na="NA",
            dec=".",
            row.names=TRUE,
            col.names=TRUE,
            qmethod=c("escape", "double"))

Becomes

write.table(d,
            "file-name.csv",
            append=FALSE,
            quote=TRUE,
            sep=" ",
            eol="\n",
            na="NA",
            dec=".",
            row.names=TRUE,
            col.names=TRUE,
            qmethod=c("escape", "double"));

*** r-autoyas-save-expression-to-memory
Defines if r-autoyas should save the snippet to memory instead of calling the R communcation again.

*** r-autoyas-use-lisp-based-dot-replacement
Uses Lisp-based dot-replacement defined by =r-autoyas-lisp-based-dot-replacement= instead of specifying through options in R startup.

*** r-autoyas-use-r-based-dot-replacement
Uses Lisp-based dot-replacement defined by =r-autoyas-r-based-dot-replacement= instead of specifying through options in R startup.

*** r-autoyas-wrap-on-exit
Defines if R-autoyas attempts to wrap end of lines.

** Internal Variables

*** r-autoyas-backward
R-autoyas use backward compatability?

*** r-autoyas-backward-compatability
Yasnippet backward compatability functions used in r-autoyas.el

Value: ((yas/expand-snippet yas-expand-snippet)
 (yas/active-field-overlay yas--active-field-overlay)
 (yas/wrap-around-region yas-wrap-around-region)
 (yas/moving-away-p yas-moving-away-p)
 (yas/expand yas-expand)
 (yas/modified-p yas-modified-p)
 (yas/moving-away-p yas-moving-away-p)
 (yas/text yas-text)
 (yas/skip-and-clear-or-delete-char yas-skip-and-clear-or-delete-char)
 (yas/snippet-fields yas--snippet-fields)
 (yas/snippets-at-point yas--snippets-at-point)
 (yas/update-mirrors yas--update-mirrors)
 (yas/fallback-behavior yas-fallback-behavior)
 (yas/minor-mode yas-minor-mode)
 (yas/field-probably-deleted-p yas--field-probably-deleted-p)
 (yas/field yas--field)
 (yas/field-text-for-display yas--field-text-for-display)
 (yas/snippet-control-overlay yas--snippet-control-overlay)
 (yas/exit-snippet yas-exit-snippet)
 (yas/check-commit-snippet yas--check-commit-snippet)
 (yas/define-snippets yas-define-snippets)
 (yas/after-exit-snippet-hook yas-after-exit-snippet-hook))



*** r-autoyas-cache
Cache of complex language statments for R-autoyas
* History

 - 18-Sep-2012 ::  Should be compatible with 0.6 and 0.8 versions of Yasnippet and fix issue #4 ()
 - 17-Sep-2012 ::  Should now work with yasnippet 0.8 -- Mostly fixes issue #4, but needs to confirm backward compatability with 0.6. ()
 - 17-Sep-2012 ::  Added some more fixes to conform to the 0.8 style variables. ()
 - 13-Sep-2012 ::  Did not catch yas--update-mirrors. Need to fix this. ()
 - 12-Sep-2012 ::  Have attempted to make r-autoyas compatible with yasnippet 0.8. This will possibly address github issue #4 ()
 - 04-Jun-2012 ::  Bug fix for autopair-mode (Matthew L. Fidler)
 - 04-Jun-2012 ::  Changed syntax table for yas/expand so that write.csv will expand if you have a snippet named csv. (Matthew L. Fidler)
 - 04-Jun-2012 ::  Bug fix for autopair. (Matthew L. Fidler)
 - 07-May-2012 ::  Changed the syntax table for =r-autoyas-expand= so that when a snippet =csv= is defined and you expand at write.csv, write.csv will be expanded instead of =csv= (Matthew L. Fidler)
 - 02-Feb-2012 ::  This package no longer auto-loads. (Matthew L. Fidler)
 - 29-Nov-2011 ::  Change the *r-autoyas* buffer to be hidden (ie " *r-autoyas*") (Matthew L. Fidler)
 - 18-Nov-2011 ::  Added gihub URL (Matthew L. Fidler)
 - 17-Nov-2011 ::  Fixed =called-interactively-p= to have a single argument. (Matthew L. Fidler)
 - 17-Nov-2011 ::  Added which to the default ignored parenthetical statements (Matthew L. Fidler)
 - 17-Nov-2011 ::  Fixed =r-autoyas-defined-p= (Matthew L. Fidler)
 - 17-Nov-2011 ::  Added Forward compatablilty for (interactive-p) (Matthew L. Fidler)
 - 17-Nov-2011 ::  Changed the order of r-autoyas alais of old (Matthew L. Fidler)
 - 16-Nov-2011 ::  Changed ignored expressions to only be ignore when using a parenthesis, and added more ignored expressions (Matthew L. Fidler)
 - 16-Nov-2011 ::  Updated to have better wrapping after exiting a snippet. (Matthew L. Fidler)
 - 08-Jun-2011 ::  A partial fix for noweb (Rnw) (Matthew L. Fidler)
 - 06-Jun-2011 ::  Small update to fix lisp-based replacements. (Matthew L. Fidler)
 - 06-Jun-2011 ::  Added a bug-fix for complex language statements like reshape. (Matthew L. Fidler)
 - 16-May-2011 ::  Bug Fixes for cached snippets. (Matthew L. Fidler)
 - 16-May-2011 ::  Added wrapping capaibilites to code. Currently only works on Ctl-G. (Matthew L. Fidler)
 - 16-May-2011 ::  Added option to remove explicit parameter names for functions if not needed. (Matthew L. Fidler)
 - 16-May-2011 ::  Allow autopair backspace to delete autostarted template. (Matthew L. Fidler)
 - 16-May-2011 ::  Changed language constructs to make sure its not a default text. (Matthew L. Fidler)
 - 16-May-2011 ::  Changed quoting method to fix read.table() (Matthew L. Fidler)
 - 16-May-2011 ::  Removed if (grepl(', ', str, fixed=TRUE)) str <- sub(', ', '', str); from R code to fix write.table()  (Matthew L. Fidler)
 - 26-Apr-2011 ::  Now when using Control-G to exit snippets, it will not delete anything inside the snippet. For example, using ls(name=".txt|",...) where the cursor is at |, pressing Cntrl-G (Matthew L. Fidler)
 - 26-Apr-2011 ::  Added a space to try to fix the strange duplication issues. (Matthew L. Fidler)
 - 25-Apr-2011 ::  Bug fix for nested auto-expansion using (. (Matthew L. Fidler)
 - 21-Apr-2011 ::  Tried to fix the autobrackets in r-auotyas. (Matthew L. Fidler)
 - 15-Apr-2011 ::  Bugfix for ess-eval-linewise option (Matthew L. Fidler)
 - 15-Apr-2011 ::  Fixed autopair bug. (Matthew L. Fidler)
 - 15-Apr-2011 ::  Changed =r-autoyas-inject-commnads= to use =ess-eval-linewise= in mering with Svens' version. (Its an option) (Matthew L. Fidler)
 - 11-Apr-2011 ::   (Matthew L. Fidler)
 - 10-Apr-2011 :: 09-Apr-2011 Added autoload. ()
 - 09-Apr-2011 :: 30-Mar-2011 Matthew L. Fidler Attempted to allow nested expansion, as well as changing the mechanism of Yasnippet expansion.  ()
 - 25-Mar-2011 ::   (Matthew L. Fidler)
