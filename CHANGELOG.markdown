5.1 [????.??.??]
----------------
* Remove `Pointed` instance for `Option`, which was removed in `base-4.16`.

5.0.2 [2020.09.30]
------------------
* Drop unused `generic-deriving` dependency.

5.0.1
-----
* Add `Pointed` and `Copointed` instances for various types from `GHC.Generics`.
* Do not attempt to support Safe Haskell anymore.

5
-
* Require `kan-extensions-5`.

4.2.1
-----
* Added (largely unsupported) options to build without almost all of the dependencies for advanced users

4.2.0.2
-------
* Added the `CHANGELOG.markdown` to `extra-source-files`, so that `hackage` can display it.

4.2.0.1
-------
* Allow `semigroupoids` 5.

4.2
---
* Add dependency on kan-extensions for instances

4.1.1
---
* Added transformers-compat dependency to loosen transformers dependency

4.1
---
* Added missing instances for `transformers` 0.4 types

4.0
---
* Updated to use the 4.0 versions of `comonad` and `semigroupoids` packages
* Updated to use just `data-default-class`. Beware of orphans.

3.1
---
* Added `Tagged` and `Proxy` instances.

3.0.3
-----
* Claim to be Trustworthy

3.0.2
-----
* Removed upper bounds on my other packages
* Refactored directory layout
