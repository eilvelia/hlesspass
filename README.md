# hlesspass

Alternative CLI application for [LessPass](https://lesspass.com).

## Build & install

```console
$ stack install
```

Run tests:

```console
$ stack test
```

## Usage

```console
$ hlesspass [options]
$ hlesspass --help
```

The app has its own configuration file in `~/.hlesspass/config.cfg`.\
The config looks like this:

```ini
lowercase=yes
uppercase=yes
digits=yes
symbols=yes
length=16
counter=1
copy=no
```

It can also store hash[*](#note1) of the password in the `~/.hlesspass/pwd` file so that you can't make a mistake when entering your password.\
Launch the program with the `--save-hash` option and enter the password. On next launch it will always check if you wrote a correct password. `--no-check` can be used to disable the check.

<a name="note1">*</a> it uses the first 3 bytes of pbkdf2 (hmac sha256, 2 iterations) hash

The main LessPass algorithm uses pbkdf2_sha256 with 100'000 iterations.
