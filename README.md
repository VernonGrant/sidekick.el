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

<!-- TODO: change this to a raw path once published -->
![Screenshot of sidekick](/assets/images/sidekick.png?raw=true "Sidekick for Emacs")

## Installation

Important: **ripgrep (rg) is required!** Please make sure you have `ripgrep`
installed and available in your system path, see [Installing
ripgrep](https://github.com/BurntSushi/ripgrep#installation).

#### Basic configuration

#### Melpa

#### Use package

## Usage

#### Key bindings

## Configuration

#### How a project's root directory is detected

Sidekick performs operations relative to your projects root directory. The root
directory is determined by having one of the following files or folders.

- A file named `.sidekick` *(highest priority)*
- A file named `.projectile`
- A git repository `.git`

#### Current available settings

- **sidekick-min-symbol-length:** The minimum symbol length required in order
  for Sidekick to update. It will not allow values less then 2.
  - **Type:** integer
  - **Default:** 2
  - **Example**:
	```lisp
	(seq sidekick-min-symbol-length 3)
	```

- **sidekick-take-focus-post-update:** If non-nil automatically focus on the
  sidekick window after each update.
  - **Type:** boolean
  - **Default:** nil
  - **Example**:
	```lisp
	(seq sidekick-take-focus-post-update t)
	```
- **(Not Implemented Yet) sidekick-window-width:** The width of the sidekick window in fractional percentage.
  - **Type:** double
  - **Default:** 0.25
  - **Example**:
	```lisp
	(seq sidekick-window-width 0.2)
	```
- **(Not Implemented Yet) sidekick-window-side:** The Sidekick window's position, left or right.
  - **Type:** string
  - **Default:** right
  - **Example**:
	```lisp
	(seq sidekick-window-side "left")
	```

#### Modifying a mode's associated files

The blow list specifies what file types should be search depending on the major
mode of the active buffer. If a mode's glob pattern is an empty string, Sidekick will just search for files
with the same extension as the active buffer. Here's the current default glob patterns for each
mode.

| Mode            | File Globs             |
|-----------------|------------------------|
| cperl-mode      | `"*.{pl,PL}"`          |
| python-mode     | `"*.py"`               |
| php-mode        | `"*.{php,phtml,twig}"` |
| js-mode         | `"*.{js,es,es6}"`      |
| typescript-mode | `"*.ts"`               |
| java-mode       | `"*.java"`             |
| json-mode       | `"*.json"`             |
| yaml-mode       | `"*.yml"`              |
| xml-mode        | `"*.xml"`              |
| c-mode          | `"*.{c,cc,h,hh}"`      |
| c++-mode        | `"*.{cpp,h,hh}"`       |
| css-mode        | `"*.{css,sass,scss}"`  |
| web-mode        | `""`                   |
| markdown-mode   | `"*.md"`               |
| emacs-lisp-mode | `"*.{el,emacs}"`       |


**(Not Implemented Yet)** To add a new mode or update an existing mode, please
use the following helper function.

```lisp
(sidekick-set-mode-file-associations "example-mode" "*.{ex,exx,EX}")
```

## Credits

I feel obliged to tank the people behind some of the tools and educational
resources that was used to complete this extension.

- Xah Lee, author of [Xah Emacs Tutorial](http://xahlee.info/emacs/index.html)
- Mickey Petersen, author of [Mastering Emacs](https://www.masteringemacs.org/)
- Andrew Gallant (BurntSushi), creator of [Ripgrep](https://github.com/BurntSushi/ripgrep)
