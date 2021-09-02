## dprox

[![CircleCI](https://circleci.com/gh/bjin/dprox.svg?style=shield)](https://circleci.com/gh/bjin/dprox)
[![Depends](https://img.shields.io/hackage-deps/v/dprox.svg)](https://packdeps.haskellers.com/feed?needle=dprox)
[![Release](https://img.shields.io/github/release/bjin/dprox.svg)](https://github.com/bjin/dprox/releases)
[![Hackage](https://img.shields.io/hackage/v/dprox.svg)](https://hackage.haskell.org/package/dprox)
[![AUR](https://img.shields.io/aur/version/dprox.svg)](https://aur.archlinux.org/packages/dprox/)
[![License](https://img.shields.io/github/license/bjin/dprox.svg)](https://github.com/bjin/dprox/blob/master/LICENSE)

`dprox` is a lightweight DNS proxy server. It's created as a drop-in replacement
of [dnsmasq](http://www.thekelleys.org.uk/dnsmasq/doc.html) to work with
[dnsmasq-china-list](https://github.com/felixonmars/dnsmasq-china-list),
while improving the overall lookup performance over large domain list.

### Installation

`dprox` should build and work on all unix-like OS with [ghc](https://www.haskell.org/ghc/) support, but it's only
been tested on Linux and macOS.

While `dprox` can be built with [cabal](https://www.haskell.org/cabal/) like any other Hackage packages, for a
reliable compilation with pinned dependencies, [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) is generally recommended.

```sh
stack setup
stack install
```

For Arch Linux users, an [AUR package](https://aur.archlinux.org/packages/dprox/) is provided.
Alternatively, you also can use the statically linked binary for the [latest release](https://github.com/bjin/dprox/releases).

### Usage

Only a small subset of dnsmasq options are implemented at the moment, just barely enough to work with `dnsmasq-china-list` and [hosts-blocklists](https://github.com/notracking/hosts-blocklists).

Here is the list of implemented dnsmasq options (with `server`, `local`, `address` and `bogus-nxdomain` options allowed in configuration file):

```
-u, --user=<username>
-p, --port=<port>
-a, --listen-address=<ipaddr>
-C, --conf-file=<file>
-h, --no-hosts
-H, --addn-hosts=<file>
-S, --local, --server=[/<domain>/]<ipaddr>[#<port>]
-A, --address=[/<domain>/]<ipaddr>
-B, --bogus-nxdomain=<ipaddr>
```

Use `dprox --help` or [dnsmasq manpage](http://www.thekelleys.org.uk/dnsmasq/docs/dnsmasq-man.html) for further details about these options.
But be aware that there might be minor differences on some options like `--server`.

To use `dprox` with `dnsmasq-china-list`, with "8.8.4.4" as the remote DNS server
(systemd user can also use [this unit file](https://github.com/bjin/dprox/blob/master/systemd/dprox.service)):

```sh
dprox -C /etc/dnsmasq.d/accelerated-domains.china.conf -C /etc/dnsmasq.d/bogus-nxdomain.china.conf -S 8.8.4.4
```

To use `dprox` with `hosts-blocklists` and the default remote DNS server ("8.8.8.8"), without loading system hosts file:

```sh
dprox -C /opt/hosts-blocklists/domains.txt -H /opt/hosts-blocklists/hostnames.txt -h
```

### Known Issue

* `dprox` has fairly large memory footprint at the moment. About 85MB for current `dnsmasq-china-list`.

### License

`dprox` is licensed under the BSD3 license. See LICENSE file for details.
