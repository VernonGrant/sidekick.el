# Sidekick.el

```text
------------------------------
------------ ----    ----
************ ****   ****
----         ----  ----
************ *********       SIDEKICK
------------ ---------    --------------
       ***** ****  ****
------------ ----   ----
************ ****    ****
------------------------------
```

Sidekick is an **Emacs extension** that's aim is to provide useful information
about a symbol inside a single window. It's still in it's infancy, and at this
point in time only provides basic reference information. It will however be in
active development and I hope to extend it in the near future to support things
such as getting a symbols documentation and extracting other time saving
information.

If you experience any issues or there's a feature you would like to recommend,
please open a [new issue](https://github.com/VernonGrant/sidekick.el/issues/new).

![Screenshot of sidekick](/assets/images/sidekick.png?raw=true "Sidekick for Emacs")

#### Current feature list

- Finds matching symbols in the current buffer.
- Finds matching symbols across your [projects](#how-it-finds-your-project-root-directory) code base.
- Lists the files containing the symbol.

## Installation

Important: **ripgrep (rg) is required!** Please insure that you have `ripgrep`
installed and available in your system path, see [Installing
ripgrep](https://github.com/BurntSushi/ripgrep#installation).

#### Melpa

#### Use package

## Usage

## Configuration

#### Project directory

#### Options & settings

- **sidekick-min-symbol-length:** The minimum symbol length required in order
  for Sidekick to update. Be careful making this value to small in larger
  projects as it will result in a lot more matches and this might make things slow.
  - **Type:** integer
  - **Default:** 2
  - **Example**:
	```lisp
	;; Should set inside your emacs configuration.
	(seq sidekick-min-symbol-length 3)
	```

#### Adding modes and customizing globs

## Overview

#### What is a symbol?

#### How it finds your project root directory

#### Adding additional modes and file patterns

## Credits

I feel obliged to tank the people behind some of the tools and educational
resources that was used to complete this extension.

- Xah Lee, author of [Xah Emacs Tutorial](http://xahlee.info/emacs/index.html)
- Mickey Petersen, author of [Mastering Emacs](https://www.masteringemacs.org/)
- Andrew Gallant (BurntSushi), creator of [Ripgrep](https://github.com/BurntSushi/ripgrep)
