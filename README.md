# composition-kit

composition-kit is a hopelessly idiosyncratic pile of clojure code which I use for music composition. If you are looking at this README
then let me explain what it does a bit, but again, hopelessly idiosyncratic is the mindset you should be in!

Think about, for a moment, the difference between a sheet music representation of composed music and the actual performance. The information
content difference is huge. I thought: Can I write something which is part way between an expanded notation and a declarative sequencer
which would let me compose music in a way which had the entire composition and all aspects of the performance in the notated score (if that
score were to cooperate with a collection of configured electronic instruments)?

There's hope because modern midi instruments are very very good, and control of midi instruments is easy. So I didn't have to worry about
synthesis - I could just set up Logic Pro tracks and configure Alchemy or use sample packs.

So the problem became: Can I write a pile of declaration which results in a midi stream
that generates music which sounds the way I want.

And the music I wanted to write I wanted to sound organic and performed.

So I constructed the clojure constructs I wanted, and started composing. As I found things I couldn't do, I added to the library. The first few
weeks were mostly library. The last few weeks were 99% composing.

The first collection of music I wrote with the tool were my "four folk dances", available [on my soundcloud](http://www.soundcloud.com/baconpaul)

## Design principles

I had two overarching design principles

1. The end result of the project is music, not software.

2. I'm not interested in randomness or solvers or "find the counterpoint for this theme" or "machine learn jazz". Those are all cool
projects but I wanted to carefully pick notes and articulations to find the music I wanted.

That's why the project feels more like "expanded notation" or "functional programming sequencer" than "music machine".

There were a few other princpiples as well

1. Test the software
2. Have the result be readable if you know lilypond and lisp
3. Make the code opensource (Apache 2), and make the music creative commons (CC-BY-4.0)

## How do I use it?

Well it's a library idiosyncratically written by this one dude with wierd music ideas and posted on github. Are you sure you want to?

But if you do, it should be easy enough to get running if you have a Mac and Logic Pro. Make sure your midi control has IAC turned on and
has a bus named "Bus 1" and you should be able to open the logic files for the folk dances and run the scripts.

On other platforms, you will need to figure out the inter-bus reciever architecture that works for midi and the endpoint. That's mostly
in midi_util.


## Other projects

You may be asking yourself "why on earth did you write this when XXX exists" or "what about the music of YYY". I happily acknowledge that
this project has a lot of overlap with lots of XXX and YYY.

Haskore and Overtone are two that come to mind. Haskore is a very formal logical approach to music in Haskell and is cool. Overtone is
an amazing tool for live performances and allows users to make some really great shows.

But I wasn't looking for those things. I wanted something which felt like notation. Lilypond was actually a bigger inflluence on me than
either of those.

## Thanks to

1. A long history of electronic musicians
2. A longer history of composers
3. The folks who wrote the excellent clojure tooling, especially CIDER, Instaparse, clojure.test
4. The super helpful folks on the google group for clojure
5. Various music teachers and so on.


## License

Copyright © 2016 Paul Walker

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.


