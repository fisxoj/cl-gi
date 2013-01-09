# Cl-GI
This is my attempt at building automatic binding generation based on the .gir files from [Gobject introspection](http://live.gnome.org/GObjectIntrospection/).  It is currently by no means complete, it doesn't even attempt to load object information yet.  But I wanted to share it with anyone who finds it, and have a backup available in the event my computer explodes.

## Requirements

You can peek at cl-gi.asd for the most up-to-date information, but as of now, it depends on
* cffi
* split-sequence

which you can install by running

	(loop for package in '("cffi" "split-sequence")
		do (ql:quickload package))

at your REPL.  You'll also need some typelibs, which should be installed along with most modern gobject libraries.

## How to use it

Right now, fire up your lisp and follow me!  I'll show a few short things

	(require 'cl-gi)
	
	(cl-gir:load-typelib "GLib" "2.0")

As things currently stand, this will result in your lisp telling you about all the things it doesn't know how to parse, and a bunch of things about constants, aliases, and functions, which it does.  When it finishes, you should be able to do things like this:

	CL-USER> (GLib:ascii-strup "cart" -1)
	"CART"
	CL-USER> (GLib:random-double)
	0.9164995134550389d0
	CL-USER> (format nil "This code calls glib version ~d.~d" GLib:+MAJOR-VERSION+ GLib:+MINOR-VERSION+)
	"This code calls glib version 2.29"
	CL-USER> (cffi:with-foreign-pointer-as-string (s 128)
	   (GLib:sprintf s "Here's a number %g, and a char %c" :double 34.21d3 :char 78))
	"Here's a number 34210, and a char N"
	CL-USER> (cffi:with-foreign-pointer-as-string (s 128)
	   (GLib:sprintf s "Here's a number %g, and a char %c" :double 34.21d3 :char 78))
	"Here's a number 34210, and a char N"



Ok, now if you'll excuse me, I have a lot more code to write for this...
