# My Advent of Code 2023 Solutions

**Language of Choice of the Year:** Haskell! It's just really fun, powerful and flexible now that I know what I'm doing

## Setting up

### Nix

Run `nix develop` in the folder to get into the dev shell. If you want to be able to download inputs from https://adventofcode.com, you also have to have your session token stored in the `TOKEN` environment variable.

If you use `direnv`, optionally create a `.env` file containing your token, and finally run `direnv allow` — you should be all set.
The `.env` file should look like:

```env
TOKEN=<your token here>
```

### Others

Install [Cabal](https://www.haskell.org/cabal/) and [Just](https://just.systems), and optionally set your session token (see Nix instructions).

## Running
