\ Float / Fixed abstraction for Zeptoforth S31.32 fixed-point math

\ The idea is we make Zeptoforth fixed point look like the Flashforth floating support 

\ [inlined] gives us faster performance for these aliases.

\ -floatcompat
\ marker -floatcompat

: fconstant 2constant ; 
: f+ [inlined] d+ ;
: f- [inlined] d- ;
: fvariable 2variable ;
: fdup [inlined] 2dup ;
: fswap [inlined] 2swap ;
: f! [inlined] 2! ;
: f@ [inlined] 2@ ;
: fvalue 2value ;
: f2dup [inlined] 4dup ;
: ei [inlined] enable-int ;
: di [inlined] disable-int ;
: fnegate [inlined] dnegate ;
: fmin [inlined] dmin ;
: fmax [inlined] dmin ;
: f+! [inlined] ( f.inc addr -- ) dup >r f@ f+ r> f! ;
: frot [inlined] 2rot ;
: fdrop [inlined] 2drop ;
: f-rot [inlined] ( f1 f2 f3 -- f3 f1 f2 ) frot frot ;

\ Flashforth uses floating constants like this: 1e10 -1e10 1.234e10 -1.234e10
\ unless you use this: float? which is Interpreter defer for parsing floating-point values. ' >float is float? PIC24-30-33 only
\
\ NOTE: >float  Convert a string into a float. ( c-addr u — flt f ) Note that it works for decimal base only.Examples: 1e10 -1e10 1.234e10 -1.234e10
\ 
\ Zeptoforth uses fix-point S31.32 constants like this: 19,540 for 19.54
\
\ We switch float?  
\ ' ,>float is float?
\ 

\ For the ray tracer
: f0< [inlined] 0,0 d< ;
: f0> [inlined] 0,0 d> ;
: fover [inlined] 2over ;
: fsqrt [inlined] sqrt ;
: 1/f [inlined] 1,0 fswap f/ ;
