# ruhaskell.herokuapp.com

Full source code for website. Attempt to make copy of ruhaskell.org using Yesod.

### System requirements
You must have Haskell, Yesod and Postgresql installed.
Instructions for installing Haskell and Yesod are available at http://www.yesodweb.com/page/quickstart

### Installation
```sh
$ git clone https://github.com/b0oh/ruhaskell.git
$ cd ruhaskell
$ cabal sandbox init
$ cabal install --only-dependencies --reorder-goals --max-backjumps=-1
$ cp config/settings.yml.example config/settings.yml
$ open config/settings.yml
$ yesod devel
