# hlesspass

Alternative CLI application for [LessPass](https://lesspass.com).

## Installation

Clone the repository and build the project either via cabal (`cabal build`,
see also `cabal --help`) or nix.

## Usage

```console
$ hlesspass [OPTIONS]
```

See `hlesspass --help`.

The app has its own configuration file in `$XDG_CONFIG_HOME/hlesspass/config.cfg`
(where `$XDG_CONFIG_HOME` defaults to `~/.config`). The config looks like this:

```ini
lowercase=yes
uppercase=yes
digits=yes
symbols=yes
length=16
counter=1
copy=no
```

It can also locally store hash[^1] of the master password in the
`$XDG_CONFIG_HOME/hlesspass/pwd` file to prevent typos. Launch `hlesspass` with
the `--save-hash` option and enter the password. On subsequent runs it'll check
that you wrote a correct password. `--no-check` can be used to disable this
check.

[^1]: the first 24 bits of pbkdf2 (hmac sha256, 2 iterations) hash are used

The main LessPass algorithm uses pbkdf2_sha256 with 100'000 iterations.

## License

Copyright © 2019 eilvelia, [MIT License](./LICENSE).
