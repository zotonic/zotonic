# Live code deployment and schema upgrades

Use the running node to establish an update barrier **before** changing a Git
working tree:

```sh
bin/zotonic deploy
# Or pull a site/module repository and rebuild the containing Zotonic umbrella:
bin/zotonic deploy /path/to/site-repository
```

Before pulling, the command restores `rebar.lock` from `HEAD` if that file is
tracked in the target repository. This discards both staged and unstaged lock-file
changes, including dependencies added by compiling `apps_user` applications.
Other local changes and untracked lock files are preserved. A restore failure
aborts deployment; a failed pull does not undo the lock-file reset.

The command then runs `git pull --ff-only`. When deploying the Zotonic umbrella
itself, it refreshes Cotonic with `apps/zotonic_mod_base/cotonic-update.sh` before
compilation. Site/module repository deployments do not refresh the shared
Cotonic runtime. A failed Cotonic download fails the deployment without replacing
the installed Cotonic files.

Deployments use the committed `priv/lib` assets. Build and commit those assets
during development; do not run their Makefiles or Taskfiles during deployment.
Buffered deployment watcher events still refresh file indexes and caches, but
do not rebuild library assets (including Sass, Less and CoffeeScript). This also
applies when a full build recovers a failed deployment. Ordinary development
watcher events continue to build assets. Erlang compilation and native runtime
build hooks, such as the sandbox launcher, still run.

The command compiles the umbrella, captures the BEAM
binaries and activates changed code atomically. The site-update UI and webhook
use the same barrier (including the existing Mercurial update path). Install this
feature with a full build and node restart first, so the new supervised
coordinator and migration guards are present before the first live deployment.

Deploy and update compile with `rebar3` from the running node's `PATH`, falling
back to `./rebar3` in the Zotonic checkout when no installed executable is found.
Ensure the service's environment includes the installed Rebar3 location; an
interactive shell can have a different `PATH`. A compiler failure still aborts
activation and keeps a failed deployment blocked until a successful rebuild.

The admin/status **Deploy latest** buttons first open a preview dialog showing the
current commit and a scrollable remote change log. Opening the dialog fetches
remote metadata without modifying the checkout or its lock file. **Cancel**
closes the dialog; **Deploy** runs the protected deployment. The preview shows up
to 100 incoming commits and disables deployment for an up-to-date or divergent
Git branch. Permissions are checked again when confirming deployment.

On **System > Modules**, administrators also get a **Deploy latest** button for
each known user module maintained in an independent Git repository, whether or
not it is active. Source symlinks, nested applications and Git worktrees are
supported. Bundled modules in Zotonic's own repository have no separate button.
A module deployment pulls its repository root, so other applications in that
same repository are updated together. Repository paths are resolved again on the
server when opening the preview and confirming deployment.

A plain external `git pull` followed by `make` or `bin/zotonic update` cannot
provide the same guarantee: the watcher may already have compiled and loaded
part of the pull. Watcher debounce and Git's index lock do not establish a
completed-build boundary. Do not run external compilers, concurrent pulls, or
manual `code:load_*` calls during a managed deployment. For an offline deployment,
stop the node, pull and build completely, then start it.

## Ordering

1. Acquire the node-wide update barrier, waiting for active schema migrations.
2. Drain executing file-handler actions. Buffer subsequent watcher events,
   including templates, translations, dispatch files and application files.
3. Restore the tracked Git lock file, pull, refresh Cotonic for an umbrella
   deployment, and compile. A failure does not activate code.
4. Discover new applications; capture changed loaded modules and unloaded project
   modules from the active code paths. Compare BEAM MD5 hashes, not timestamps.
5. Prepare the binaries with `code:prepare_loading/1`, safely purge unused old
   code, and activate the entire batch with `code:finish_loading/1`.
6. Notify site managers only after activation.
7. Flush queued watcher events into the barrier, then release it, replay buffered
   events without rebuilding committed library assets, and perform any deferred periodic
   mtime cache clearing. Flush caches and reindex files under a read lease, which
   prevents a subsequent deployment from overlapping that refresh.
   Migrations acquire read leases
   before reading `mod_schema`, and retain them through the schema callback,
   datamodel/data changes and recording the schema version. New code activation
   cannot interleave with a migration. Startup, restart and reinstall use this
   guard too.

`bin/zotonic update` now waits for its full compile and code activation and returns
an error exit status on failure. Site restarts/migrations initiated by the batch
notification continue asynchronously through the normal module-manager queue.
Ordinary watcher and shell reloads also use atomic batches and the same migration
barrier. Single-file compilation and full compilation share a job queue.

This protects migration code consistency; it does not make HTTP requests or
reads of files in an in-place working tree transactional. Use maintenance mode
or an offline deployment if visitors must not see files changing during a pull.
Migration code should remain compatible with requests already executing old code.

## Failures and recovery

A failed pull, compile or load retains buffered events and prevents schema
upgrades. Fix the working tree and run:

```sh
bin/zotonic update
```

A successful full rebuild reopens the barrier. A single-module reload cannot
clear a failed deployment. An interrupted update process also closes the barrier.
If the coordinator itself restarts, restart the node after completing a build;
its lost migration leases cannot safely be reconstructed.

If a module's old code is still executing, activation reports `old_code_in_use`
instead of killing that process. Retry after those processes finish. Modules
with `-on_load` cannot participate in an atomic load; deploy those changes with
a full node restart. An atomic-load failure never falls back to per-file loading.

The barrier is local to one Erlang node. Multiple nodes sharing a database need
deployment orchestration and database migration serialization separately.
Previously skipped migrations require an explicit repair migration; this change
does not lower or reset stored schema versions.

## Regression tests

```sh
./rebar3 compile
bin/zotonic runtests zotonic_filehandler_update_tests \
    zotonic_filehandler_deploy_tests z_module_manager_schema_tests
```

The barrier tests run in an isolated VM, including when invoked by the full
Zotonic test runner. They cover a delayed helper, two competing migration callers,
failed/interrupted updates, active migration exclusion, atomic-load rejection,
safe purging and retained non-code watcher events. The deployment integration test
uses disposable local Git repositories, runs a real umbrella compile on the
sandbox node, and verifies failed-pull recovery without changing repository history.
