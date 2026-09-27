# Contributing

Thanks for helping improve Extreme Carpaccio. This document covers the conventions that
apply across the repository. The kata itself is documented in
[clients/README.md](clients/README.md), and running a session is covered by
[server/README.md](server/README.md).

## The client baseline

Every client under `clients/` is a deliberately minimal starting point. Participants build
the order and feedback endpoints themselves, so a client that already implements them
defeats the exercise. Every client therefore meets the same contract:

- `POST /ping` responds with the body `pong` on port 3000.
- Exactly one test, asserting that endpoint.
- No `/order` and no `/feedback` route.
- No leftover models, serializers, controllers or routes for those endpoints.
- A package manager and lock file, so `install` and `test` are reproducible.
- A README with the sections `Prerequisites`, `Install & Run`, `Endpoint` and `Tests`,
  where `Endpoint` shows the `curl -X POST http://localhost:3000/ping` call.
- A `.gitignore` covering the build output of that ecosystem.

Where a client is a single implementation it lives directly in `clients/<language>/`. Where
a language has more than one flavour, each variant is a subdirectory, as with
`clients/java/java-httpserver` or `clients/ruby/sinatra`.

### Adding a client

1. Create `clients/<language>/` with the implementation, its one test, and a README
   following the shape above.
2. Add a test workflow at `.github/workflows/test-client-<language>.yml`. It must run only
   on pull requests, and its `paths` filter must list the client folder plus
   `.github/workflows/**` and `.github/actions/**`.
3. Register the language in `.github/dependabot.yml` if Dependabot supports its ecosystem.
   See the dependency section below for the ones it does not.

Editor and IDE files must not be committed. Participants bring their own editor, and
`.vscode/`, `.idea/`, `.vs/` and similar are already ignored at the root.

## Continuous integration

Each test target is its own workflow, filtered with GitHub's built-in
`on.pull_request.paths`. A pull request therefore runs only the workflows whose folders it
touched, and a change to `.github/workflows/**` runs all of them.

There is no aggregate `All Tests Passed` job. A workflow cannot declare `needs` on a job in
a different workflow, so the gate could not survive the split.

Two consequences worth knowing before you trust a green pull request:

- **A single-folder pull request runs a single workflow.** A change to `clients/go` runs the
  Go workflow and nothing else. Green means Go passed, not that the repository is healthy.
- **A pull request that matches no filter runs no workflow at all.** Editing a top-level
  file such as this one runs zero tests, and the pull request looks green because nothing
  reported a failure. That is expected, not a malfunction.

Workflow files must sit directly in `.github/workflows/`. GitHub does not discover
workflows in subdirectories, so a workflow placed under a folder is silently ignored and
never runs. File names follow `test-server.yml` and `test-client-<language>.yml`.

Branch protection currently requires no status checks. Review the diff and the checks that
did run; do not read a green pull request as full coverage.

## Dependencies

Dependabot runs monthly against the server and most clients, and its pull requests are
squash-merged automatically.

Five clients have no Dependabot ecosystem, so their dependencies are **not** updated
automatically and must be reviewed by hand:

| Client | Manifest | Why |
| --- | --- | --- |
| `clients/clojure` | `project.clj` | No Clojure ecosystem |
| `clients/haskell` | `carpaccio.cabal` | No Haskell ecosystem |
| `clients/erlang` | `rebar.config` | No Erlang ecosystem |
| `clients/d` | `dub.json` | No D ecosystem |
| `clients/racket` | none | The client has no dependencies to update |

Renovate was evaluated as a replacement and does not close this gap: it supports Clojure,
but has no manager for Haskell, Erlang or D. Its `regexManagers` could parse those
manifests by hand, at the cost of regexes that break silently when an upstream format
changes, which is worse than a version reviewed once a year. When upgrading a client in
this list, check the current release rather than waiting for a bot.

## Pull requests and commits

- One issue per pull request, and conventional commit messages.
- Do not commit to `main`; open a pull request and let a maintainer merge it.
- Say in the description how the change was verified, and be explicit when a change is
  validated only by CI because the toolchain was not available locally.
