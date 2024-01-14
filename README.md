## dprox

[![CircleCI](https://circleci.com/gh/bjin/dprox.svg?style=shield)](https://circleci.com/gh/bjin/dprox)
[![CirrusCI](https://api.cirrus-ci.com/github/bjin/dprox.svg)](https://cirrus-ci.com/github/bjin/dprox)
[![Depends](https://img.shields.io/hackage-deps/v/dprox.svg)](https://packdeps.haskellers.com/feed?needle=dprox)
[![Release](https://img.shields.io/github/release/bjin/dprox.svg)](https://github.com/bjin/dprox/releases)
[![Hackage](https://img.shields.io/hackage/v/dprox.svg)](https://hackage.haskell.org/package/dprox)
[![AUR](https://img.shields.io/aur/version/dprox.svg)](https://aur.archlinux.org/packages/dprox/)
[![License](https://img.shields.io/github/license/bjin/dprox.svg)](https://github.com/bjin/dprox/blob/master/LICENSE)

`dprox` is a lightweight DNS proxy server. It's created as a drop-in replacement
of [dnsmasq](http://www.thekelleys.org.uk/dnsmasq/doc.html) to work with
[dnsmasq-china-list](https://github.com/felixonmars/dnsmasq-china-list),
enhancing overall lookup performance over extensive domain lists.

### Installation

`dprox` should build and work on all unix-like OS with [ghc](https://www.haskell.org/ghc/) support, as well as Windows.

Although `dprox` can be built using [cabal](https://www.haskell.org/cabal/) like any other Hackage package,
we recommend using [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) for a more reliable compilation with pinned dependencies.

```sh
stack setup
stack install
```

Arch Linux users can install provided [AUR package](https://aur.archlinux.org/packages/dprox/).

Alternatively, you can opt to use the statically linked binary available in the [latest release](https://github.com/bjin/dprox/releases).

### Usage

Currently, only a small subset of dnsmasq options is implemented: just enough to seamlessly work with `dnsmasq-china-list` and [hosts-blocklists](https://github.com/notracking/hosts-blocklists).

Here is the list of implemented dnsmasq options (with `server`, `local`, `address` and `bogus-nxdomain` options allowed in configuration file):

```
-p, --port=<port>
-a, --listen-address=<ipaddr>
-C, --conf-file=<file>
-h, --no-hosts
-H, --addn-hosts=<file>
-S, --local, --server=[/<domain>/]<ipaddr>[#<port>]
-A, --address=[/<domain>/]<ipaddr>
-B, --bogus-nxdomain=<ipaddr>
```

For more details about these options, use `dprox --help` or refer to the [dnsmasq manpage](http://www.thekelleys.org.uk/dnsmasq/docs/dnsmasq-man.html).
However, be aware that there might be minor differences in some options, such as `--server`.

To use `dprox` with `dnsmasq-china-list` and set "8.8.4.4" as the remote DNS server:

```sh
dprox -C /etc/dnsmasq.d/accelerated-domains.china.conf -C /etc/dnsmasq.d/bogus-nxdomain.china.conf -S 8.8.4.4
```

To use `dprox` with `hosts-blocklists` and use the default remote DNS server ("8.8.8.8"), excluding the loading of the system hosts file:

```sh
dprox -C /opt/hosts-blocklists/domains.txt -H /opt/hosts-blocklists/hostnames.txt -h
```

Additionally, a customized `--ipset` option is available (distinct from `dnsmasq`).
With these options configured, in case a DNS request somehow matches with the specified `ipset`, `dprox` will switch to an alternative upstream
DNS server (with the [FakeDNS](https://www.v2fly.org/config/fakedns.html) feature, for example), specified by `ipset-server`.
The exact matching policy can be configured using the `ipset-match`. These options can be used to enable IP-based routing for DNS requests.

```
--ipset <ipmask>
--ipset-match <none|all|any|notall>
--ipset-server <ipaddr>[#port]
--ipset-file <file>
```

### Known Issue

* `dprox` currently has a relatively large memory footprint, approximately 85MB for the current `dnsmasq-china-list`.

### License

`dprox` is licensed under the BSD3 license. Refer to the LICENSE file for comprehensive details.
