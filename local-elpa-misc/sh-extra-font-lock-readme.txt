`sh-extra-font-lock' provides syntax highlighting of variables inside
strings for `sh-mode'. Written by Anders Lindgren and packaged up
(including some docstring and naming fixes) by Mattias Bengtsson.  To
activate this just add the following to your `init.el':

(add-hook 'sh-mode-hook 'sh-extra-font-lock-activate)

Below are the original notes from the author on Stack Overflow:

	The code below use a font-lock rule with a function instead of a
	regexp, the function search for occurrences of $VAR but only when they
	are inside a double-quoted string. The function (syntax-ppss) is used
	to determine this.

	The font-lock rule use the prepend flag to add itself on top of the
	existing string highlighting. (Note that many packages use t for
	this. Unfortunately, this overwrites all aspects of the existing
	highlighting. For example, using prepend will retain a string
	background color (if there is one) while replacing the foreground
	color.)

	You can use this by adding the last function to a suitable hook,
	for example:

	(add-hook 'sh-mode-hook 'sh-extra-font-lock-activate)
