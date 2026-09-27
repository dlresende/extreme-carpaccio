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
3. Register the language in `.github/dependabot.yml`.

Editor and IDE files must not be committed. Participants bring their own editor, and
`.vscode/`, `.idea/`, `.vs/` and similar are already ignored at the root.

## Continuous integration

Each test target is its own workflow, filtered with GitHub's built-in
`on.pull_request.paths`, so a pull request runs only the workflows whose folders it
touched. A change to `.github/workflows/**` runs all of them.

There is no aggregate job. A workflow cannot declare `needs` on a job in a different
workflow, so a gate could not survive the split. Branch protection requires no status
checks, which means a green pull request is not evidence that the whole repository is
healthy: a change to one client folder runs only that client's workflow, and a change
matching no filter runs none at all. Review the diff and the checks that did run.

Workflow files must sit directly in `.github/workflows/`. GitHub does not discover
workflows in subdirectories, so a workflow placed under a folder is silently ignored and
never runs. File names follow `test-server.yml` and `test-client-<language>.yml`.

## Dependencies

Dependabot runs monthly against the server and the clients, and its pull requests are
squash-merged automatically.

## Pull requests and commits

- One issue per pull request, and conventional commit messages.
- Say in the description how the change was verified, and be explicit when a change is
  validated only by CI because the toolchain was not available locally.
