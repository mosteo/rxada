# README #

RxAda, an experiment on porting [ReactiveX](http://reactivex.io/) to Ada. At this time there is only a proof-of-concept example.

Quick example. Suppose a Java 8 case where we print the length of a string:

```
#!java

Observable
   .just("Hello, world!")
   .map(s -> s.length())
   .subscribe(len -> System.out.println(len));
```

With RxAda this becomes (tested with gnat GPL 2015/2016 and gnat from Ubuntu 16.10):

```
#!Ada

begin
   Strings.Chain :=
     Just ("Hello, world!") &
     Map (Length'Access) &
     Subscribe (Put_Line'Access);
```

Not the tersest but for Ada I think its a-OK. And we keep compile time type checks and the rest of marvels of Ada.

For a working example check the file [rx-examples-basic.adb](https://bitbucket.org/amosteo/rxada/src/9292a420676fdc8b6e2f572f5c89114551ead664/src/rx-examples-basic.adb?at=default)

### Quick start ###

You need to instantiate a package for each type you want to use in a Rx chain, and a transformation package for each pair of types involved in some operation:

```
#!Ada

package Strings  is new Rx.Observable (String);
package Integers is new Rx.Observable (Integer);
package StrToInt is new Rx.Operators (String, Integer);
```

And you have to "use" them so its "&" function becomes visible. As a side effect, Rx operators become visible too, although they can be prefixed with their package for clarity, if one preferred.

Functions are passed to operators via their 'Access. This has the advantage that Ada accessibility checks are applied.

In the short term I plan to have pre-instantiated packages for common Ada types: String, Integer mostly (Long_Long_Integer?). I'd also like to provide tailored facilities for enumerations.

### Rationale and design goals ###

I became acquainted with RxJava recently, and soon appreciated its power for tidy code and sane concurrency in event driven systems (like Android). This is particularly true when using Java 8 with lambda functions.

I soon started to think on how something similar could be achieved in Ada, which lacks lambda functions and implicit generics/templates. I suspected that increased verbosity would be inevitable. I was also unsure about what was the way to go: generics or class-wide types. So I started tinkering.

I settled for a driving goal: make the user code as tidy and understandable as possible, following the Java 8 example (I've seen RxC++ monstrosities that defy sanity).

Ada syntax has limitations that prevent effective use of dot notation in this problem: only primitive operations can use it. Once you introduce generics to bind transformations from a type to another you cannot use it anymore, since these operations are no longer primitive.

So I saw two possibilities that could work: force the user to work with types derived from a common root, and lose compile time checks (since all value passing would be as this abstract root type), or try to come up with a solution using generics. The first approach could be made more palatable using marshalling functions so in practice the user only uses its own types. This would require instantiating these marshalling functions, so in the end it gains not much over using generics directly. All this to be able to use dot notation and keep the flow of transformations. An attempt in this direction is buried somewhere in the early revisions of the repository.

Fortunately, I saw something in the RxC++ version that RxJava does not: the use of the pipe operator for chaining. Ada has no pipe operator but you can redefine the language operators, such is the "&" used for string concatenation. Either this or "+" would be a good fit familiar for Ada users. Still, the important thing here is that, by having different "&" for each type conversion involved, the code can look as in the above example, where the flow of transformations is kept, which was my main concern.

The price of this approach is an unholy amount of copies made during chain setup; but since this is done once per chain I think it is a fair price to pay.

So, in the end, the library has the following properties:

* It seems simple for a new user, without complex marshallings (I'm looking at you, GtkAda and Booch components). I like the look of the result and find it understandable. The package names can be used for additional in-code documentation.
* Alas, this requires to "use" every Rx type package involved so its "&" is in view. I plan to provide a renamed "connect" or such function, but that will require either lots of intermediate variables or a LISP-like construct that kinda defeats the purpose of clarity.
* Each type requires a library-level instantiation, and each transformation from one type to another requires another. Not what I wanted but seems inevitable to allow for non-library level chain creation (there is another buried design in the early commits in this direction).
* Transforming operators take type instances as a generic parameter. I had never used generic packages as formals of another generic; I never saw the need but here it fits naturally somehow. This is at the root of the design: the Observable takes a user type and creates a Typed package which contains the whole class hierarchy for the type. Later, transformations use Observable instances as formals. Check [rx-observable.ads](https://bitbucket.org/amosteo/rxada/src/9292a420676fdc8b6e2f572f5c89114551ead664/src/rx-observable.ads?at=default), [rx-transform.ads](https://bitbucket.org/amosteo/rxada/src/9292a420676fdc8b6e2f572f5c89114551ead664/src/rx-transform.ads?at=default) and [rx-operators.ads](https://bitbucket.org/amosteo/rxada/src/9292a420676fdc8b6e2f572f5c89114551ead664/src/rx-operators.ads?at=default) for details.
* There is no dynamic memory involved for now in the library code, thanks to Ada use of unconstrained types and the Ada.Containers.Holders 2012 package. I so much love not to have to deal with pointers/access types if possible...
* BIG CON: gnat gets very slow compiling these chains of generics.

### What's next ###

I intend to continue experimenting with this idea, which is providing me lots of fun. I had to make several attempts to arrive at this design, which was not clear in my mind as a goal, so it kind of emerged from the various attempts. In this regard, heartfelt thanks to all contributors from [comp.language.ada](https://groups.google.com/forum/#!forum/comp.lang.ada) to the several discussions I started there (e.g., [1](https://groups.google.com/forum/#!topic/comp.lang.ada/v0ZXkaG8rek), [2](https://groups.google.com/forum/#!searchin/comp.lang.ada/rxada/comp.lang.ada/QvjReeJKfXQ/w9y6NE4PCgAJ)).

Who knows, although Ada has always (well, since '95) had good concurrency support, miles above any other I have used in other languages, RxJava is fabulous for the kind of uncoupled or loosely coupled event streams that appear in Android. So I'd love to have Rx available in Ada, just in case.

Perhaps it can become a stress test for the Gnat generics engine too ;-) It's really slow compiling this project. Yeah, I said that already. And I already hit a compiler bug likely involving interfaces+generics.

### LICENSE ###

This project is licensed under [LGPL v3](http://choosealicense.com/licenses/lgpl-3.0/).