* "debug and visualize"
  * Render a phrase as a lily PNG


* Logical Sequence ToDo
  * "markers" in all the sequences which let you make symbolic references to a point in time
  * loops and alternate endings are a clunky idiom. Think about this
  * Chord 'splatting' in the core library


* extended-lily branch to do
  - ^pedal is the same obviously as are markers
  - ^cc
  - have a curve syntax so you can do
    	 ^cc=(sigmoid blah blim)
  - apply that to velocities also so you have
    	  ^crescendo=70-90@4 to have a 4 beat creschendo from 70 to 90
  * test per-note chord velocity
  * hold doesn't apply to chords properly

;; TURNS OUT THIS ONE IS HARD since you have to look ahead or do two passes
"^vel-style=line c4*70 d e f g*90" is a crescendo

* Control curve idioms better than meddley
* Curves applied to mulitple instruments (or really "apply to playing instrument" but that's super tough)


* Cleanup ready for GITHUB upload



