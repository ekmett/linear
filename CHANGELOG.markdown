1.23 [2024.04.15]
-----------------
* The direction of interpolation of `lerp` has been reversed;
  now `lerp 0 a b == a` and `lerp 1 a b == b`.
  This brings `lerp` in line not only with its implementation
  in other languages and frameworks, but also with `slerp` in this package.

1.22 [2022.11.30]
-----------------
* The types of `_Point` and `lensP` have been generalized:

  ```diff
  -_Point :: Iso' (Point f a) (f a)
  +_Point :: Iso (Point f a) (Point g b) (f a) (g b)

  -lensP :: Lens' (Point g a) (g a)
  +lensP :: Lens (Point f a) (Point g b) (f a) (g b)
  ```

  There is a chance that existing uses of `_Point` or `lensP` will fail to
  typecheck due to their more general types. You can use `_Point.simple` or
  `lensP.simple` to restore their old, more restricted types (where `simple`
  comes from `Control.Lens` in the `lens` library).

1.21.10 [2022.06.21]
--------------------
* Allow building with `vector-0.13.*`.

1.21.9 [2022.05.18]
-------------------
* Allow building with `transformers-0.6.*`.

1.21.8 [2021.11.15]
-------------------
* Allow building with `hashable-1.4.*`.
* Drop support for pre-8.0 versions of GHC.

1.21.7 [2021.09.20]
-------------------
* Fix a build error when using `random-1.2.1` or later.

1.21.6 [2021.07.05]
-------------------
* Fix a build error when configured with `-template-haskell`.

1.21.5 [2021.02.18]
-------------------
* Allow building with `lens-5.*`.

1.21.4 [2021.01.29]
-------------------
* Allow building with `vector-0.12.2` or later.
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.

1.21.3 [2020.10.03]
-------------------
* Allow building with GHC 9.0.

1.21.2 [2020.09.30]
-------------------
* Use `base-orphans-0.8.3` or later. This means that the `Linear.Instances`
  module no longer defines any orphan instances of its own, and the module is
  now a simple shim on top of `Data.Orphans` from `base-orphans`.

1.21.1 [2020.06.25]
-------------------
* Allow building with `random-1.2.*`.

1.21 [2020.02.03]
-----------------
* Add instances for direct sums (`Product`) and tensor products (`Compose`) of
  other vector spaces. This makes is much more convenient to do things like treat
  a matrix temporarily as a vector through Compose, or to consider things like
  Gauss-Jordan elimination, which wants augmented structures.
* Add `frobenius` for computing the Frobenius norm of a matrix.
* Added `Random` instances for `System.Random`. We had an indirect dependency
  through `vector` anyways.
* Add "obvious" zipping `Semigroup` and `Monoid` instances to all the
  representable vector spaces.
* Add `R1`..`R4` instances to `Quaternion`. `_w` is the scalar component so that
  `_x`,`_y`,`_z` can be directional.
* Add more solvers to `Linear.Matrix`, available with `base-4.8` or later.
* Add `unangle` function to `Linear.V2`.

1.20.9 [2019.05.02]
-------------------
* Derive `Lift` instances for `Plucker`, `Quaternion`, and `V{0,1,2,3,4}`.

1.20.8 [2018.07.03]
-------------------
* Add instances of the `Field` classes from `lens`.
* Add `Epsilon` instance for `Complex`.
* Use specialized implementations of the `null` and `length` methods in
  `Foldable` instances.
* Add `Hashable1` instances for data types in `linear`. Also add a
  `Hashable` instance for `V`.
* Fix a bug in which `Quaternion`s were incorrectly exponentiated.

1.20.7
------
* Support `semigroupoids-5.2.1` and `doctest-0.12`

1.20.6
------
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.
* Make `(1 / x)` and `recip x` agree in the `Fractional` instance for `Quaternion`
* Use newtype instances for `Point` vectors in `Linear.Affine`
* Enable `PolyKinds` in `Linear.Trace`. Also enable `PolyKinds` when GHC 7.6 or
  later is used (previously, it was GHC 7.8 or later).
* Fix a segfault arising from the `MVector` instance for `V`
* Add `Finite` class for conversion between `V` and fixed-size vector types

1.20.5
------
* GHC 8 compatibility
* Fixed the `perspective` calculation.

1.20.4
------
* Compatibility with `base-orphans` 0.5

1.20.3
------
* Support `vector` 0.11.0.0.
* Support `cereal` 0.5
* You can now unboxed vectors of `V n` vectors.

1.20.2
------
* Modified the `doctest` machinery to work with `stack` and builds to non-standard locations.
* Removed the local `.ghci` file.
* Various numerical stability improvements were made to the quaternion and projection functions.

1.20.1
------
* Fixed doctests broken by the previous change.
* Unboxed vector instances for various linear data types now use unpacked integers even on older GHCs.

1.20
----
* `inv22`, `inv33` and `inv44` no longer attempt an epsilon check. They no longer return a `Maybe` result as a consequence.
  You should filter for the 0 determinant case yourself.

1.19.1.3
--------
* `vector` 0.11.0.0 support

1.19.1.2
--------
* Fix GHC 7.4.

1.19.1.1
--------
* Proper `reflection` 2 support

1.19.1
------
* `reflection` 2 support

1.19
----
* Change the Ixed instance for `Linear.V` to use `Int` as the index type. This makes `V n` a _lot_ easier to use.

1.18.3
------
* Compile warning-free on GHC 7.10.


1.18.2
------
* Added `NFData` instance for `Point`

1.18.1
------
* Added an `-f-template-haskell` option to allow disabling `template-haskell` support. This is an unsupported configuration but may be useful for expert users in sandbox configurations.
* Added lenses for extracting corner various sub-matrices e.g. `_m22`, `_m33`

1.18.0.2
--------
* Fixed builds on even older GHCs.

1.18.0.1
--------
* Fixed the test suite.
* Fixed builds on older GHCs.

1.18
----
* Consolidated `eye2` .. `eye4` into a single `identity` combinator.
* Fixed the `Data` instance `V n a` for GHC 7.10-RC3.

1.17.1.1
--------
* `filepath` 1.4 support

1.17.1
------
* Added support for `Data.Functor.Classes` from `transformers` 0.5 via `transformers-compat`.
* Added missing support for `binary`, `bytes` and `cereal` for `Point`

1.17
----
* Better support for `binary`. Added support for `bytes` and `cereal`

1.16.4
------
* `ortho` and `inverseOrtho` now only require a `Fractional` constraint.
* Added missing `Floating` instances.

1.16.3
----
* Improve the performance of `fromQuaternion`, `mkTransformation`,
  `mkTransformationMat`, `basisFor`, `scaled` by using implementations
  that inline well for functions that were previously reference
  implementations.

1.16.2
----
* Added `NFData` instances for the various vector types.
* Added `!!/` operator for matrix division by scalar.

1.16.1
----
* Added `Trace` instance for `V1`.

1.16
----
* Renamed `kronecker` to `scaled`.

1.15.5
------
* Added `Metric` instances for `[]`, `ZipList`, `Maybe`
* Added `det44` and `inv44` to `Linear.Matrix`
* Added `Data` instance for `Point`

1.15.4
------
* Added Typeable and Data instances for V

1.15.3
------
* Added missing `FunctorWithIndex`, `FoldableWithIndex` and `TraversableWithIndex Int (V n)` instances for `V`

1.15.2
------
* Added `frustum`, analogous to the old `glFrustum` call.
* Added `inverseInfinitePerspective`, `inverseOrtho`, `inverseFrustum`.

1.15.1
------
* Added `inversePerspective`. It is much more accurate to compute it directly than to compute an inverse.

1.15.0.1
--------
* Fixed build failures caused by `Linear` re-exporting the old name.

1.15
----
* Renamed `Linear.Perspective` to `Linear.Projection`.
* Fixed a build issue with GHC HEAD.

1.14.0.1
--------
* Fixed test failures caused by 1.14

1.14
----
* Moved `Coincides` to `Linear.Plucker.Coincides`. The constructors `Line` and `Ray` oft collided with user code.

1.13
----
* Switched 'ortho' to follow the OpenGL handedness.

1.12.1
------
* Added "swizzle" lenses **e.g.** `_yzx`, which are useful for working with libraries like `gl`.

1.12
------
* Added 'transpose'
* Added missing 'Mxy' matrices up to 4 dimensions -- they were commonly reimplemented by users.

1.11.3
------
* Fixed an issue with `UndecidableInstances` on GHC 7.6.3

1.11.2
------
* Added `Linear.Perspective`.

1.11.1
------
* Added `_Point`, `relative` and a few instances for `Point`.

1.11
----
* Changed the 'representation' of `V n` from `E (V n)`, which was hard to use, to `Int`, which is a bit too permissive, but is easy to use.

1.10.1
------
* Added `Linear.V2.angle`.

1.10
----
* Added `Hashable` instances.

1.9.1
-----
* Added a role annotation to `V n a` to prevent users from using GHC 7.8's `Coercible` machinery to violate invariants.

1.9.0.1
-----
* Fixed a broken build

1.9
---
* Added `MonadZip` instances.
* Added `MonadFix` instances.
* Added `Control.Lens.Each.Each` instances

1.8.1
-----
* Bugfixed `slerp`

1.8
---
* Added missing `Unbox` instances for working with unboxed vectors of `linear` data types.

1.7
---
* Fixed `axisAngle`
* `unit` now has a rank 1 type.

1.5
---
* `lens` 4 compatibility

1.4
---
* Renamed `incore` to `column` and added an example.

1.3.1.1
-------
* Build bugfix

1.3.1
---
* Better implementations of `basis` and `basisFor`.
* Derived Generic instances.

1.2
---
* Improved matrix multiplication to properly support the sparse/sparse case.

1.1.4
-----
* Marked modules `Trustworthy` as necessary.

1.1.2
-----
* Dependency bump for `reflection` compatibility

1.1.1
-----
* Fixed an infinite loop in the default definition of `liftI2`.

1.1
---
* Added `Additive` instances for `[]`, `Maybe` and `Vector`.

1.0
---
* Strict vectors
* Exported `mkTransformationMat`
* Bumped dependency bounds

0.9.1 [bug fix]
-----
* Exported `Linear.V0`!

0.9
---
* Added sparse vector support.

0.8
---
* Added `Linear.V0`

0.7
---
* Added `Linear.Instances`
* More documentation

0.6
---
* Removed the direct dependency on `lens`.
* Added `Linear.Core` to cover vector spaces as corepresentable functors.

0.5
-------
* Added `Ix` instances for `V2`, `V3`, and `V4`

0.4.2.2
-------
* Removed the upper bound on `distributive`

0.2
---
* Initial hackage release
