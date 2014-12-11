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
