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

Sidekick is an **Emacs package** that's aim is to provide information about a
symbol inside a single window. **It's still in its infancy**, and at this point in
time only searches for references using `grep`. It will however be in active
development and I hope to extend it in the near future to support things such as
getting a symbols documentation and extracting other time saving information.

If you experience any issues or there's a feature you would like to recommend,
please open a [new issue](https://github.com/VernonGrant/sidekick.el/issues/new).

![Screenshot of sidekick](https://raw.githubusercontent.com/VernonGrant/sidekick.el/main/assets/images/sidekick.png "Sidekick for Emacs")

## Installation

Important: **grep is required!** Please make sure you have it installed
and available in your system path.

#### Manual installation

Clone this repository locally, and add the load path to your `.emacs`:

```lisp
(add-to-list 'load-path "/path/to/sidekick.el/folder/")
(require 'sidekick)

;; If your part of the dark side (Evil User), uncomment this:
;; (with-eval-after-load 'sidekick (require 'sidekick-evil))

;; Set some default bindings.
(global-set-key (kbd "C-c k") 'sidekick-at-point)
(global-set-key (kbd "C-c C-k") 'sidekick-search-for-literal)

;; Optional:
;; Maybe customize a mode's file assosiations?
(sidekick-set-file-associations "web-mode" '("php" "twig" "blade" "phtml"))

;; Notes:
;; On Windows, use this path format:
;; (add-to-list 'load-path "C:\\Users\\your-name\\path\\to\\sidekick.el\\folder\\")
```

## Usage

#### Default key bindings

The following keybindings are available inside the Sidekick window/mode.

| Key   | Command                          | Description                                              |
|-------|----------------------------------|----------------------------------------------------------|
| `q`   | **sidekick-quit**                | Closes the Sidekick window and kills it's buffer.        |
| `g`   | **sidekick-refresh**             | Re-runs the previous operations, refreshing the results. |
| `n`   | **sidekick-open-next-match**     | Displays the next match.                                 |
| `p`   | **sidekick-open-previous-match** | Displays the previous match.                             |
| `RET` | **sidekick-open-match**          | Go's directly to the matched symbol.                     |

You can customize keybindings that are local to the Sidekick window, like this:

```lisp
;; Adding custom Sidekick window local bindings.
(add-hook 'sidekick-mode-hook
  (lambda ()
   (local-set-key (kbd "<up>") 'sidekick-open-previous-match)
   (local-set-key (kbd "<down>") 'sidekick-open-next-match)))
```

#### Unbound commands

| Command                         | Description                                                  |
|---------------------------------|--------------------------------------------------------------|
| **sidekick-at-point**           | Takes the symbol at point and triggers the update call.      |
| **sidekick-focus**              | Focuses on the Sidekick window, if visible.                  |
| **sidekick-search-for-literal** | Input a literal string and triggers the update call.         |

You can define custom key bindings for any of the above commands. See the below
example:

```lisp
(global-set-key (kbd "C-c k") 'sidekick-at-point)
(global-set-key (kbd "C-c K") 'sidekick-focus-toggle)
(global-set-key (kbd "C-c C-k") 'sidekick-search-for-literal)
```

#### Evil key bindings

| Key   | Command                          | Description                                              |
|-------|----------------------------------|----------------------------------------------------------|
| `q`   | **sidekick-quit**                | Closes the Sidekick window and kills it's buffer.        |
| `g`   | **sidekick-refresh**             | Re-runs the previous operations, refreshing the results. |
| `n`   | **sidekick-open-next-match**     | Displays the next match.                                 |
| `N`   | **sidekick-open-previous-match** | Displays the previous match.                             |
| `RET` | **sidekick-open-match**          | Go's directly to the matched symbol.                     |

[Evil-mode](https://github.com/emacs-evil/evil) integration is provided by
`sidekick-evil`. Use the following to load it:

``` lisp
;; Add this line after (require 'sidekick).
(with-eval-after-load 'sidekick (require 'sidekick-evil))
```

## Configuration

#### How the projects root directory is determined

A projects root directory is determined by `project.el`. If not in use, the following additional checks are made.

- A file named `.sidekick` *(highest priority)*
- A file named `.projectile`
- A git repository `.git`

#### Available customizations

- **sidekick-search-minimum-symbol-length:** The minimum symbol / term length in
	order for Sidekick to update. The lowest number possible is 2.
  - Type: integer
  - Default: `2`
  - Example:
	```lisp
	(setq sidekick-search-minimum-symbol-length 3)
	```

- **sidekick-window-hide-footer:** Remove the Sidekick footer branding.
  - Type: boolean
  - Default: `nil`
  - Example:
	```lisp
	(setq sidekick-window-hide-footer t)
	```

#### Modifying a mode's associated files

The below list specifies the default mode - file associations. These
associations allow you to search within specific file types on a per mode basis.
For example, if your using `web-mode` for HTML files, it might also be useful to
search for a symbol inside `SCSS` and `Javascirpt` files.

```lisp
(defvar sidekick--mode-file-associations
    `(
         ("c++-mode"        . ("cpp" "h" "hh"))
         ("c-mode"          . ("c" "cc" "h" "hh"))
         ("cperl-mode"      . ("pl" "PL"))
         ("css-mode"        . ("css"))
         ("scss-mode"       . ("css" "sass" "scss"))
         ("emacs-lisp-mode" . ("el" "emacs"))
         ("go-mode"         . ("go"))
         ("java-mode"       . ("java"))
         ("js-mode"         . ("js" "es" "es6"))
         ("json-mode"       . ("json"))
         ("markdown-mode"   . ("md"))
         ("text-mode"       . ("txt"))
         ("php-mode"        . ("php" "phtml" "twig" "blade"))
         ("phps-mode"       . ("php" "phtml" "twig" "blade"))
         ("python-mode"     . ("py"))
         ("ruby-mode"       . ("rb"))
         ("rust-mode"       . ("rs"))
         ("restclient-mode" . ("el"))
         ("org-mode"        . ("org"))
         ("typescript-mode" . ("ts"))
         ("web-mode"        . ("html" "blade" "twig"))
         ("nxml-mode"       . ("xml" "xml.dist"))
         ("yaml-mode"       . ("yml")))
    "Association List holding major mode file associations.")
```

To add or update an existing mode's file associations, the following helper
function has been provided. See the below usage example:

```lisp
;; Adds a file associations for a new mode.
(sidekick-set-file-associations "example-mode" '("html" "php" "py"))

;; Replace an existing mode's file associations.
(sidekick-set-file-associations "web-mode" '("html" "scss" "sass" "php"))
```

## Credits

The following tools and educational resources where invaluable during the
development of this package.

- [Contributors](https://github.com/VernonGrant/sidekick.el/blob/main/CONTRIBUTORS.md)
- [Free Software Foundation](https://www.fsf.org/)
- Xah Lee, author of [Xah Emacs Tutorial](http://xahlee.info/emacs/index.html)
- Mickey Petersen, author of [Mastering Emacs](https://www.masteringemacs.org/)
