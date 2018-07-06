# Talos
[![NuGet](https://img.shields.io/nuget/dt/Talos.svg)](https://www.nuget.org/packages/Talos/)

This libary is an F# port of the Haskell library [aeson-diff](https://github.com/thsutton/aeson-diff)
(and its dependency [edit-distance-vector](https://github.com/thsutton/edit-distance-vector))
for generating and applying [RFC 6902](http://tools.ietf.org/html/rfc6902) JSON patches.
[chiron](https://github.com/xyncro/chiron) serves as an aeson replacement.

Reasonable attempts are made for the core `Talos` library to be C# friendly, but C# users will likely
prefer to use `Talos.Dynamic` instead of `Talos` directly (in large part because `chiron` is not very
C# friendly).
