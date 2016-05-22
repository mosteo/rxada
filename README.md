# README #

RxAda, an experiment on porting ReactiveX to Ada. At this time, there are but a few operators available and nothing really fancy yet. This attempt is more the result of my curiosity on how it could be done than of a real need.

### Rationale and design goals ###

I became acquainted with RxJava recently, and soon appreciated its power for tidy code and sane concurrency in event driven systems (like Android). This is particularly true when using Java 8 with lambda functions.

I soon started to think how something similar could be achieved in Ada, which lacks lambda functions and implicit generics/templates. I suspected that increased verbosity would be inevitable. I was also unsure about what was the way to go: generics or class-wide types. So I started tinkering.

After some tries, I settled for a driving goal: make the code as tiny and similar to java 8 as possible. That meant no pre-instantiations, class-overridings, and out-of-order declarations-bindings. That meant a series of generic instantiations. The result for now has been surprisingly reasonable in my opinion, since I expected much more verbose Ada code. A collateral of this approach is that the Rx streams are set-up during elaboration code, which makes me wary but, at the same time, makes me want to push it forward to see what happens and where is the limit to this approach, which is new to me. So, basically:

* Each operator invocation, which in Java is a method call, here is a generic instantiation.
* Each generic takes the resulting package of the previous instantiation. I had never used generics as formals of another generic; frankly it was over my head, but here it fits naturally somehow so I see it very clearly. Perhaps I never had had the need before.
* It seems simple from the point of view of a new user, no complex marshallings/pre-instantiations (I'm looking at GtkAda and the Booch components now).
* This, for maps, filters, and so on requires declaring between two such instances the procedure/function to be applied. That's OK to me, since it maintains the top-bottom logic order.
* There is no dynamic memory involved, so no possibility of leaks. But also, everything is generated at compile time, so I'm unsure about the implications. Also, it is (or it will be) probably horribly broken when I introduce Schedulers. Or when using a same stream from concurrent threads. I guess at some point I will need some kind of smart pointer.
* CON: gnat gets very slow compiling these chains of generics

### What's next ###

I intend to continue experimenting with this idea, which is providing me lots of fun. I had to make several attempts to arrive at this design, which was not clear in my mind as a goal, so it kind of emerged from the various attempts. Who knows, although Ada has always (well, since '95) had good concurrency support, miles above any other I have used in other languages, for the kind of uncoupled or loosely coupled event streams I have used in Android RxJava is fabulous. I'd love to have it available in Ada, just in case.

Perhaps it can become a test suite for Gnat generics engine too ;-) It's really slow compiling this.

### LICENSE ###

Until I decide where this goes, this project is strictly GPL v3.