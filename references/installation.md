# Installation and Setup

Complete installation guide for org-roam-skill.

## Prerequisites

1. **Emacs with org-roam installed and configured**
2. **Emacs daemon running**: Start with `emacs --daemon`
3. **emacsclient available**: Should be installed with Emacs
4. **org-roam directory set up**: Your notes directory (e.g., `~/org-roam/` or `~/Documents/org/roam/`)
5. **org-roam database initialized**

## Step 1: Load the Package in Emacs

### For Doom Emacs

Add to `~/.doom.d/config.el`:

```elisp
(use-package! org-roam-skill
  :load-path "~/.claude/skills/org-roam-skill")
```

### For Vanilla Emacs

Add to `~/.emacs.d/init.el`:

```elisp
(add-to-list 'load-path (expand-file-name "~/.claude/skills/org-roam-skill"))
(require 'org-roam-skill)
```

After adding, **restart Emacs** or evaluate the configuration.

## Step 2: Verify Installation

### Verify package is loaded

```bash
emacsclient --eval "(featurep 'org-roam-skill)"
```

Should return `t`. If it returns `nil`, the package isn't loaded yet.

### Run diagnostic

```bash
emacsclient --eval "(org-roam-doctor)"
```

This checks your org-roam configuration, database, and templates.

## Optional: Recommended Configuration

For cleaner filenames, configure org-roam to use timestamp-only format:

### For Doom Emacs

```elisp
(setq org-roam-directory "~/Documents/org/roam/")

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>.org" "${title}")
           :unnarrowed t))))
```

### For Vanilla Emacs

```elisp
(setq org-roam-directory "~/Documents/org/roam/")

(with-eval-after-load 'org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>.org" "${title}")
           :unnarrowed t))))
```

### Why This is Optional

- Creates files like `20251019193157.org` (clean)
- Instead of `20251019193157-title-slug.org` (default)
- **The skill auto-detects your template format**
- Works with both formats automatically

### Note on Title Duplication

The `"${title}"` in the template prevents #+title duplication, as org-roam adds it automatically.

## Finding Your Org-roam Directory

Ask the skill to detect it:

```bash
emacsclient --eval "org-roam-directory"
```

Returns your configured org-roam directory path.

## Common Setup Issues

### Daemon not running

```bash
emacs --daemon
```

### org-roam not loaded

Ensure org-roam loads on startup:

```elisp
(require 'org-roam)
(org-roam-db-autosync-mode)
```

### Database not syncing

Manually sync:

```bash
emacsclient --eval "(org-roam-db-sync)"
```

### Package not found

Verify the load-path is correct:

```bash
ls ~/.claude/skills/org-roam-skill/org-roam-skill.el
```

Should exist. If not, check installation location.
