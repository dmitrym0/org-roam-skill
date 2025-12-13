# CLAUDE.md

## What This Is

A Claude Code skill enabling interaction with org-roam note-taking systems through emacsclient. Communicates with a running Emacs daemon to create, query, and manage org-roam notes.

## Architecture

**Core components:**
- `SKILL.md` - Main skill instructions (307 lines, follows skill-creator best practices)
- `org-roam-skill.el` - Main package loading all modules
- `org-roam-skill-*.el` - Modular implementations (create, search, links, tags, attach, utils, doctor)
- `scripts/*.el` - Legacy helper scripts (deprecated, backward compatibility only)
- `references/` - Detailed documentation loaded as needed by Claude

**References (progressive disclosure):**
- `references/functions.md` - Complete function documentation with parameters and examples
- `references/installation.md` - Setup and configuration guide
- `references/troubleshooting.md` - Common issues and solutions
- `references/org-roam-api.md` - Org-roam API reference
- `references/emacsclient-usage.md` - Detailed emacsclient patterns

**Package loading:**
Users must load `org-roam-skill` in their Emacs config (see `agent_docs/setup.md` for details). All functions use `org-roam-skill-` prefix except diagnostics (`org-roam-doctor*`).

## Key Implementation Details

**Tag sanitization:**
- Org tags cannot contain hyphens - functions auto-replace with underscores (`my-tag` → `my_tag`)

**Node access:**
- Use `org-roam-node-from-title-or-alias` for title search (see `org-roam-skill-search.el:15`)
- Use `org-roam-node-from-id` when you have the ID
- Always use `org-roam-node-*` accessor functions
- Use node IDs for linking (stable across file moves)

**Database operations:**
- Sync before queries if data might be stale: `(org-roam-db-sync)`
- Prefer org-roam query functions over direct SQL

**Diagnostics:**
- Full check: `emacsclient --eval "(org-roam-doctor)"`
- Quick check: `emacsclient --eval "(org-roam-doctor-quick)"`

**Temp file handling:**
- External `:content-file` parameters are automatically deleted after processing
- Only deletes files in temp directories (`/tmp/`, `/var/tmp/`, and `temporary-file-directory`)
- Use `:keep-file t` to prevent deletion (useful for debugging)
- Internal temp files (in elisp) use `make-temp-file` + `unwind-protect` for guaranteed cleanup
- Implementation: `org-roam-skill-create.el:55-110` (unwind-protect cleanup logic)
- Validation function: `org-roam-skill--looks-like-temp-file` in `org-roam-skill-core.el:35-42`

For detailed implementation patterns, see `agent_docs/implementation.md`.

## Testing & Development

Uses [Buttercup](https://github.com/jorgenschaefer/emacs-buttercup) for testing and [Eldev](https://github.com/doublep/eldev) for test execution.

**IMPORTANT**: All new functions and significant code changes require tests.

**Quick commands:**
```bash
make test     # Run all tests
make lint     # Run linting checks
make prepare  # Install dependencies
```

See `agent_docs/testing.md` for detailed patterns, test structure, and examples.

## Git Workflow

**Branch-based workflow required:**
1. Create feature branch (never commit to `master`)
2. Make changes with tests (`make test` before commit)
3. Push branch and create PR
4. Wait for approval before merge

**Commit format:**
```
<conventional type>: <summary>

Co-Authored-By: Claude <noreply@anthropic.com>
```

## Skill Packaging

**Files included in .skill package:**
- `SKILL.md` (required)
- `*.el` files (Emacs Lisp code)
- `references/` (documentation for AI)

**Files excluded from .skill package** (see `.skillignore`):
- `README.md` - Developer/user documentation (not needed for AI agent)
- `CLAUDE.md` - Project-specific development instructions
- `agent_docs/` - Developer documentation
- `test/` - Test suite (developer resources)
- `.git/`, `.github/`, `Makefile`, `Eldev` - Development tools

**Packaging command:**
```bash
cd ~/dev/majorgreys && zip -r org-roam-skill.skill org-roam-skill/ -x @org-roam-skill/.skillignore
```

## Additional Documentation

**For AI (included in package):**
- `SKILL.md` - Quick reference and core workflows
- `references/functions.md` - Complete function documentation
- `references/installation.md` - Setup guide
- `references/troubleshooting.md` - Common issues

**For developers (excluded from package):**
- `README.md` - User-facing documentation
- `agent_docs/setup.md` - Package loading and Emacs configuration
- `agent_docs/implementation.md` - Detailed implementation patterns
- `agent_docs/testing.md` - Testing patterns and examples
