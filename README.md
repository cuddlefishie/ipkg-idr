<!--
SPDX-FileCopyrightText: 2021 The ipkg-idr developers

SPDX-License-Identifier: CC0-1.0
-->

# `ipkg-idr`

An Idris 2 package description parser for [Idris 2](https://github.com/idris-lang/Idris2).

## Installation

At least version `0.3.0` of the Idris 2 compiler is required.

```sh
idris2 --install ipkg.ipkg
```

## Usage

After installing the package, add `ipkg` to the `depends` section in the `.ipkg` file.

To parse an iPKG file, the `parseIPKG` function in the `Language.IPKG` module
can be used.

A package description is represented with the `PkgDesc` type.

## License

All code is licensed under the [MPL-2.0](LICENSES/MPL-2.0.txt).

All files that are not properly copyrightable are in the public domain, using
the [CC0 license](LICENSES/CC0-1.0.txt).

This project aims to be [REUSE compliant](https://reuse.software/).