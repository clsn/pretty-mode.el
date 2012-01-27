Pretty Mode
===========

My own modification of `pretty-mode.el` from http://www.emacswiki.org/emacs/pretty-mode.el for my own amusement.  What can I say, I like nifty-looking Unicode characters?  I've mostly added more stupid characters, and also made it possible to specify more kinds of replacement; the basic idea behind the file and how it works is from the original author.

The premise behind pretty-mode is that emacs will replace a keyword or symbol or some such and _display_ in its place a _single_ character instead.  Note both emphasized words: the actual content of the file is unaffected, so the program runs normally, can be edited by other people, and nobody has to know about your sick desire to mess with the presentation.  Also, each substitution can only consist of a _single_ Unicode character, owing to limitations in how the whole thing works and my limited understanding of what it's doing.  Maybe someday we can change that.

Languages
---------

Pretty-mode is language-sensitive, and replaces things according to what language you're writing in, since different substitutions make different kinds of sense of different languages.  So in some languages, the string `<>` should be replaced by `≠` because it means "not equal", but in perl it's used to indicate reading from the "standard" file handle (standard input or file arguments).  Most of my own changes have been in languages I myself know and use, obviously, and I'm sure there are some mistakes or missing specifications.  The changes I chose to make or not make depend on the languages I know and use, but also on considerations like how the character looks on my own personal system when it renders it with whatever font emacs seems to like to use.  Your mileage may vary.

So make sure you're in the right language-mode, and then do `M-x pretty-mode` to toggle prettiness, after loading in the file of course.

Example
-------

There's a dummy python program (rotten programming, barely grammatical, but shows off a lot of the substitions) in `python-sample.py`, which when rendered on my computer looks like this:

![Image](http://github.com/clsn/pretty-mode.el/blob/master/python-sample.png?raw=true)