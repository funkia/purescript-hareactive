# purescript-hareactive

[![Hareactive on Pursuit](https://pursuit.purescript.org/packages/purescript-hareactive/badge)](https://pursuit.purescript.org/packages/purescript-hareactive)
[![Build status](https://travis-ci.org/funkia/purescript-hareactive.svg?branch=master)](https://travis-ci.org/funkia/purescript-hareactive)

Hareactive is a practical, powerful, and simple FRP library.

## Highlights

* Implements classic FRP with behaviors and streams.
* Supports continuous time and integration.
* Higher-order monadic behaviors.
* Efficient push and pull implementation.
* Simple precise semantics.
* Supports circular dependencies i.e. recursively defined behaviors.
* Easy integration with both `Effect` and `Aff` for side-effects.
* Suitable for building UI.

## Background

Hareactive is highly inspired by the papers [Push-Pull Functional Reactive
Programming](http://conal.net/papers/push-pull-frp/) by Conal Elliott and
[Practical principled FPP](https://dl.acm.org/citation.cfm?id=2784752) by Atze
van der Ploeg and Koen Claessen.

Hareactive implements _classic_ FRP. This means that it makes a distinction
between _behaviors_ and _streams_ (what Hareactive calls streams is often called
event). As for why that distinction is useful see the blogpost [Behaviors and
streams, why both?](http://vindum.io/blog/behaviors-and-streams-why-both/).

Behaviors in Hareactive are _monads_. Monadic behaviors are essential for many
practical purposes. Behaviors in Hareactive can be _continuous_, i.e. change
infinetly often. Among other things this means that Hareactive support continous
time and integration.

Hareactive supports stateful behaviors (i.e. behaviors that depends on the
past). It does so while avoiding the problems with space and time leaks that
FRP are notorius for.

A key focus is to implement a simple precise semantic model following in the
spirit of Conal Elliott. The semantics serve as a specification for determining
the correctness of the implementation. Additionally, it is a mental model which
can be used to reason about the library.

Hareactive is implemented in carefully optimized TypeScript.

## Install

```
npm i @funkia/hareactive
bower install --save purescript-hareactive
```

## Documentation

Module documentation is published on
[Pursuit](https://pursuit.purescript.org/packages/purescript-hareactive).
