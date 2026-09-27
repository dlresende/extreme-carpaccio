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
2. Add a job to `.github/workflows/test.yml` that runs the client's test command in its
   folder, and add that job to the `ci-gate` job's `needs` list. The gate fails unless
   every job it depends on succeeded, so a job left out of `needs` is silently unchecked.
3. Register the language in `.github/dependabot.yml`.

Editor and IDE files must not be committed. Participants bring their own editor, and
`.vscode/`, `.idea/`, `.vs/` and similar are already ignored at the root.

## Continuous integration

All tests run from a single workflow, `.github/workflows/test.yml`, on pull requests only.
It holds one job per client and per server platform, plus an `All Tests Passed` gate that
fails unless every one of those jobs succeeded.

The gate is why the suite lives in one workflow. A workflow can only depend on jobs inside
itself, and `workflow_run` fires once per completed workflow, so a single aggregate check
cannot be built across separate per-folder workflows. The cost is that a pull request waits
for the slowest job, currently the Haskell build at around six minutes.

Because the gate covers every job, a green pull request is meaningful: all of them ran and
passed. Branch protection currently requires no status checks, so nothing blocks a merge on
that basis — the gate is there to be read, not enforced.

## Dependencies

Dependabot runs monthly against the server and the clients, and its pull requests are
squash-merged automatically.

## Pull requests and commits

- One issue per pull request, and conventional commit messages.
- Say in the description how the change was verified, and be explicit when a change is
  validated only by CI because the toolchain was not available locally.
