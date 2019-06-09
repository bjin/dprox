## dprox

[![CircleCI](https://circleci.com/gh/bjin/dprox.svg?style=shield)](https://circleci.com/gh/bjin/dprox)
[![Depends](https://img.shields.io/hackage-deps/v/dprox.svg)](https://packdeps.haskellers.com/feed?needle=dprox)
[![Release](https://img.shields.io/github/release/bjin/dprox.svg)](https://github.com/bjin/dprox/releases)
[![Hackage](https://img.shields.io/hackage/v/dprox.svg)](https://hackage.haskell.org/package/dprox)
[![AUR](https://img.shields.io/aur/version/dprox.svg)](https://aur.archlinux.org/packages/dprox/)
[![License](https://img.shields.io/github/license/bjin/dprox.svg)](https://github.com/bjin/dprox/blob/master/LICENSE)

dprox is a lightweight DNS proxy server. It's written as a drop-in replacement
of dnsmasq to work with [dnsmasq-china-list](https://github.com/felixonmars/dnsmasq-china-list),
while improving the overall lookup performance over large domain list.

### Installation

Only Linux and macOS are supported. [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) is required to build `dprox`.

```sh
stack install
```

### Usage

Only a small subset of dnsmasq options are implemented at the moment, just barely enough to work with `dnsmasq-china-list`.

Use `dprox --help` to list those options. A [systemd unit file](https://github.com/bjin/dprox/blob/master/systemd/dprox.service) is also provided for Linux user.

### Known Issue

* `dprox` has fairly large memory footprint at the moment. Over 100MB for current `dnsmasq-china-list`.

### License

`dprox` is licensed under the BSD3 license. See LICENSE file for details.
