# README #

RxAda, an experiment on porting ReactiveX to Ada. At this time, there are but a few operators available and nothing really fancy yet. This attempt is more the result of my curiosity on how it could be done than of a real need.

Quick example. Suppose a Java 8 case where we print the length of a string:

```
#!java

Observable
   .just("Hello, world!")
   .map(s -> s.length())
   .subscribe(len -> System.out.println(len));
```

With RxAda this becomes (tested with gnat GPL 2015 and gnat from Ubuntu 16.10):

```
#!Ada

declare
   package  Just    is new Rx.Just ("Hello, world");
   function Length (S : String) return Natural is (S'Length);
   package  Str2Len is new Rx.Map (Just.Output, Integer, Length);
   package  Subs    is new Rx.Subscribe (Str2Len.Output, Put_Line);
```

Not the tersest but for Ada I think its a-OK. And we keep compile time type checks and the rest of marvels of Ada.

For more examples of what is implemented check the file [rx-examples.adb](https://bitbucket.org/amosteo/rxada/src/bde706ee0b3906abb94a88548870eddf86d452cf/src/rx-examples.adb?at=default&fileviewer=file-view-default)

### Rationale and design goals ###

I became acquainted with RxJava recently, and soon appreciated its power for tidy code and sane concurrency in event driven systems (like Android). This is particularly true when using Java 8 with lambda functions.

I soon started to think how something similar could be achieved in Ada, which lacks lambda functions and implicit generics/templates. I suspected that increased verbosity would be inevitable. I was also unsure about what was the way to go: generics or class-wide types. So I started tinkering.

After some tries, I settled for a driving goal: make the code as tiny and similar to java 8 as possible. That meant no pre-instantiations, class-overridings, and out-of-order declarations-bindings. What emerged was a series of generic instantiations. The result for now has been surprisingly reasonable in my opinion, since I expected much more verbose Ada code. A collateral of this approach is that the Rx streams are set up during elaboration code, which makes me wary but, at the same time, makes me want to push it forward to see what happens and where is the limit to this approach, which is new to me. So, basically:

* It seems simple from the point of view of a new user, no complex marshallings/pre-instantiations (I'm looking at you, GtkAda and Booch components). I like the look of the result and find it understandable. The package names can be used for additional in-code documentation.
* Each operator chaining, which in Java is a method call, here is a generic instantiation.
* Each generic takes the resulting package of the previous instantiation. I had never used generic packages as formals of another generic; I never saw the need but here it fits naturally somehow. Check [rx-map.ads](https://bitbucket.org/amosteo/rxada/src/bde706ee0b3906abb94a88548870eddf86d452cf/src/operators/rx-map.ads?at=default&fileviewer=file-view-default) for example.
* There is no dynamic memory involved, so no possibility of leaks. But also, everything is generated at compile time, so I'm unsure about the full implications yet. Also, it is (or will be) probably horribly broken when I introduce Schedulers. Or when using a same stream from concurrent threads. I guess at some point I will need some kind of smart pointer.
* BIG CON: gnat gets very slow compiling these chains of generics.

### What's next ###

I intend to continue experimenting with this idea, which is providing me lots of fun. I had to make several attempts to arrive at this design, which was not clear in my mind as a goal, so it kind of emerged from the various attempts. Who knows, although Ada has always (well, since '95) had good concurrency support, miles above any other I have used in other languages, RxJava is fabulous for the kind of uncoupled or loosely coupled event streams that appear in Android. So I'd love to have Rx available in Ada, just in case.

Perhaps it can become a stress test for the Gnat generics engine too ;-) It's really slow compiling this project. Yeah, I said that already.

### LICENSE ###

Until I decide where this thing goes, this project is GPL v3.