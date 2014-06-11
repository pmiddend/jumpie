jumpie
======

Jumpie is a simple platformer, a toy project to teach myself Haskell in a "visual" setting.

There is no real game concept yet. This section will be expanded if that happens.

In the meantime, here's a screenshot!

![Screenshot](https://raw.githubusercontent.com/pmiddend/jumpie/master/doc/screenie0_0_1_0.png)

Installation
------------

Some general notes about the installation procedure:

  - It's best to use a cabal sandbox: `cabal sandbox init` (this needs cabal >= 1.18)
  - On Debian-based distros, you need libsdl2-dev and libsdl2-image-dev. You also need to download the current version of SDL2 for Haskell (found [here|http://hackage.haskell.org/package/sdl2-1.1.0/sdl2-1.1.0.tar.gz]), extract it somewhere, open the sdl2.cabal file and change the `pkgconfig-depends` to `sdl2 >= 2.0.2`. Then go to the jumpie directory and add the SDL2 source directory to the sandbox via `cabal sandbox add-source ../sdl2-1.1.0`
  - On all other distros, you need the latest version (currently 0.1.2.1) of sdl2-image (the Haskell package). Clone it from [github|https://github.com/ccll/hs-sdl2-image], then add the source to the sandbox: `cabal sandbox add-source hs-sdl2-image`.

