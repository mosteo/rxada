[![Alire CI](https://img.shields.io/endpoint?url=https://alire-crate-ci.ada.dev/badges/rxada.json)
[![Alire indexed](https://img.shields.io/badge/alire-0.1.0-blue.svg)](https://alire.ada.dev)
[![LGPL licensed](https://img.shields.io/badge/license-LGPL%203.0-blue.svg)](./LICENSE)


# README #

RxAda, an experiment on porting [ReactiveX](http://reactivex.io/) to Ada. 

[Article](https://authors.elsevier.com/a/1b5sx4weOvebZl) at the Journal of
Systems Architecture with details about internals and package organization.

Quick example. Suppose a Java 8 case where we print the length of a string:

```java

Observable
   .just("Hello, world!")
   .map(s -> s.length())
   .subscribe(len -> System.out.println(len));
```

With RxAda this becomes (tested with gnat GPL 2015/2016 and gnat from Ubuntu 16.10):

```Ada

declare
   S : Rx.Subscriptions.Subscription;
begin
   S := -- We can't ignore the resulting subscription in Ada
     Just ("Hello, world!")
     & Map (Length'Access)
     & Subscribe (Put_Line'Access);
```
Type checks are performed at compile time.

For a working example check the file [rx-examples-basic.adb](https://github.com/mosteo/rxada/blob/master/src/main/rx-examples-basic.adb) and the other examples in the [src/main](https://github.com/mosteo/rxada/blob/master/src/main/) folder

For even more exhaustive examples, check the main test suite body: [rx-tests.adb](https://github.com/mosteo/rxada/blob/master/src/utests/rx-tests.adb)

### Quick start ###

You need to instantiate a package for each type you want to use in a Rx chain, and a transformation package for each pair of types involved in some operation. The basic String and Integer types are already available in Rx.Std. Supposing you didn't know about this, you would do:

```Ada

package Strings  is new Rx.Indefinites (String);  -- Comes pre-instantiated in Rx.Std
package Integers is new Rx.Definites (Integer);   -- Comes pre-instantiated in Rx.Std
package StrToInt is new Rx.Operators (Strings.Observables, Integers.Observables);
```

Otherwise, it is enough to do:

```Ada

package StrToInt is new Rx.Operators (Std.Strings, Std.Integers);
```

Then, you have to "use" them so their "&" function becomes visible. As a side effect, Rx operators become visible too, although they can be prefixed with their package for clarity, if preferred.

Functions are passed to operators via their 'Access, so standard accessibility checks are applied.

### Rationale and design goals ###

I became acquainted with RxJava recently, and soon appreciated its power for tidy code and sane concurrency in event-driven systems (like Android). This is particularly true when using Java 8 with lambda functions.

Given Ada lack of lambda functions and inline generics, increased verbosity is inevitable: functions have to be declared in advance (and at library level), and the types involved require generic instantiations in advance too.

Design goals: 

* Make the user code as tidy and understandable as possible, following the Java 8 example (i.e., avoid RxC++ cryptosyntax).
* Make defaults as simple as possible for a new user to ease the learning curve.

Some highlights about the library implementation:

* The library makes extensive use of "signature packages" aka "traits" (thanks go to Emmanuel Briot for his traits-based container library). Check [rx-types.ads](https://github.com/mosteo/rxada/blob/master/src/rx-types.ads) and [rx-operators.ads](https://github.com/mosteo/rxada/blob/master/src/rx-operators.ads) for examples, and the Rx.Traits.* hierarchy. However, this is mostly hidden from the user. Beginners can use simple instantiations of Rx.Definites or Rx.Indefinites with their types of choice.
* There is very little explicit dynamic memory management for now in the library code, thanks to Ada use of unconstrained types and the Ada.Containers.Holders 2012 package. 

### What's next ###

I intend to continue experimenting with this library, which is providing me lots of fun. I had to make several attempts to arrive at this design, which was not clear in my mind as a goal, so it kind of emerged from the various attempts. In this regard, heartfelt thanks to all contributors from [comp.language.ada](https://groups.google.com/forum/#!forum/comp.lang.ada) to the several discussions I started there (e.g., [1](https://groups.google.com/forum/#!topic/comp.lang.ada/v0ZXkaG8rek), [2](https://groups.google.com/forum/#!searchin/comp.lang.ada/rxada/comp.lang.ada/QvjReeJKfXQ/w9y6NE4PCgAJ)).

### LICENSE ###

This project is licensed under [LGPL v3](http://choosealicense.com/licenses/lgpl-3.0/).
