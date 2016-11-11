* "visualization" tools of note and volume density

* instrument names to allow above
  - really instrument creation neads to be into a map with names. So rather than
    (def piano (midi/midi-instrument 2))
    have
    (def instruments 
      (-> (??/midi-instrument-map)
          (assoc :piano (midi/port 1))
  	  (assoc :bass (midi/port 2))))

    then use (ls/on-instrument (:piano instruments)) (or of course you can alias that out with def).

---
* "markers" in all the sequences which let you make symbolic references to a point in time
* split instruments are tough to notate
* loops and alternate endings are a clunky idiom. Want a method for this
* 'per-note' things are clunky

the solution to this is to have an 'expanded' lilypond notation with meta-data and hold and velocity info. So imagine

"c4 f16 d e f" becoming

"[^inst v1 ^hold 0.99] c4 [^inst v2 ^hold 0.7] f16 d e f" can avoid the lily -> hold for -> instrument idiom

for instruments and holds. And then we could do velocities in metadata or on notes

"c4 d e f*92 g" turns the volume up to 92 at f and keeps it there

"[^vel-style line] c4*70 d e f g*90" is a crescendo

Pedal is the same obviously.

