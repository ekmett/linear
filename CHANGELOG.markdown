1.9
---
* Added `MonadZip` instances.
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
