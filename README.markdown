# Emacs Starter Kit

From Phil Hagelberg's emacs-starter-kit.

## Current customisations by James Stradling (jstr)

* Currently used with Cocoa Emacs (http://atomized.org/wp-content/cocoa-emacs-nightly/)
* May have partially functional or half-implemented features. Try forking [the original from Phil](http://github.com/technomancy/emacs-starter-kit/tree/master) if you experience problems or want a fresh start.

### Some current additions

* defunkt's [TextMate](http://github.com/defunkt/textmate.el/tree/master) minor-mode.
* Textile, Haml, Sass, etc.
* Color themes.
* Snippets via yasnippet
* Centered cursor mode
* Nav
* Some additional useful functions such as outdent (bound to shift-tab), smart-tab (tab or autocomplete depending on context), etc.

# Description

This should provide a saner set of defaults than you get normally with
Emacs. It's intended for beginners, but it should provide a reasonable
working environment for anyone using Emacs for dynamic languages.

## Installation

1. Install Emacs from http://atomized.org/wp-content/cocoa-emacs-nightly/
2. Move the directory containing this file to ~/.emacs.d
3. Launch Emacs!

If you are missing some autoloads after an update (should manifest
itself as "void function: foobar" errors) try M-x regen-autoloads.

If you want to keep your regular ~/.emacs.d in place and just launch a
single instance using the starter kit, try the following invocation:

  $ emacs -q -l ~/src/emacs-starter-kit/init.el

## ELPA

Libraries from ELPA (http://tromey.com/elpa) are preferred when
available since dependencies are handled automatically, and the burden
to update them is removed from the user.

See starter-kit-elpa.el for a list of libraries that are pending
submission to ELPA.

## Contributing

If you know your way around Emacs, please try out the starter kit as a
replacement for your regular dotfiles for a while. If there's anything
you just can't live without, add it or let me know so I can add
it. Take a look at what happens in init.el to get started.

Also: see the file TODO.

The latest version is at http://github.com/technomancy/emacs-starter-kit/
