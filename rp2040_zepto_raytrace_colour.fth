\ Ray trace for Zeptoforth, output to 0.96" display on a RP2040-LCD-0.96 board by Waveshare
\ By Rob Probin, Feb 2024
\ Loosely Based on BBC BASIC verison
\ Conversion (C) 2024 Rob Probin.
\
\ Conversion itself licensed under the MIT License - however since this is 
\ a direct conversion of the original code, the original license
\ would apply.  I have not been able to find the original license - so
\ that means that copyright applies.  Contact the original author for
\ permission to use the original code - for this file only. If you use the
\ maths *concepts* only to create a simple renderer, then I think that is
\ fair use and/or covered by known in the art.  If you use the original code,
\ then you need to get permission.
\
\ NOTE: BMP converter is original code - so that is fully MIT licensed.
\
\ Originally found here https://github.com/robogeek42/agon_basic_mycode/blob/main/rtdemo/HowDoesItWork.md
\ And here https://github.com/robogeek42/agon_basic_mycode/tree/main/rtdemo
\ But also see https://news.ycombinator.com/item?id=39023056
\
\ Zeptoforth version based on my gForth version
\ Differences:
\   - gForth floats are like 1e0 and 0e and -0.1e10, whereas Zeptforth floats are like 1, and 1,2 and 10,1234
\   - gForth float support, Zeptoforth has S31.32 fixed point support. (There is 
\     also a S15.16 in Zeptoforth - need to check if it's good enough, and what the speed improvement will be.)
\   - There is only one data stack on Zeptoforth - but there is a seperate float stack on gForth
decimal

\ same output size as the LCD display
lcd_width constant screen_width 
lcd_height constant screen_height
\ Scaling factor is set below (it's set as half the width at the moment)

24 constant bits_per_pixel
3 constant bytes_per_pixel

\ image buffer - make it match the 16 bit LCD screen
lcd_buffer constant image_buffer

: pixel_address ( x y -- addr )  screen_width * + 2* image_buffer + ;

0 value plot_colour

: fill_image_buffer ( rgb_colour -- ) screen_height 0 DO
    screen_width 0 DO
        dup I J pixel_address h!
    LOOP
LOOP drop ;

: byte_swap ( n -- n ) dup $FF and 8 << swap $FF00 and 8 >> or ;

\ plot (0,0)=bottom left, (319,255)=top right
: plot ( x y -- ) screen_height swap - 1- pixel_address plot_colour byte_swap swap h! ;

\ lcd has 5 bit red, 6 bit green, 5 bit blue. 
\ The LCD itself is byte reversed so it's 0bGGGBBBBB RRRRRGGG where those lowest green are the MSBits
\ The buffer we make 0bRRRRRGGG GGGBBBBB
\ so we need to convert 8 bit to 5/6 bit
: set_blue_colour ( n -- ) $F8 and 3 >> to plot_colour ;
: set_green_colour ( n -- ) $FC and 3 << to plot_colour ;
: set_red_colour ( n -- ) $F8 and 8 << to plot_colour ;
: set_cyan_colour ( n -- ) $F8 and dup 3 << swap 3 >> + to plot_colour ;
: set_white_cyan_blue_colour ( n -- ) 
    dup 128 >= if
        \ from cyan to white  ( add red )
        128 - 2* $F8 and 8 << $07FF + 
    else
        \ from blue to cyan ( add green )
        2* $FC and 3 << $001F +
    then
    to plot_colour
;


\ I would have used fvalue, but it is not implemented in gForth 0.7.3
\ only on later versions.
\ This is a super quick conversion, so I just used fvariable. 
\
\ @TODO: check for later version and use fvalue
\ @TODO: Consider using local variables
\ @TODO: Consider using a struct for the camera, ray and sphere
\ @TODO: Consider passing as parameter as per Agon code

\ camera position
fvariable X
fvariable Y
fvariable Z
\ ray vector
fvariable U
fvariable V
fvariable W

: show_ray_position ( -- )  \ show the current ray position
    ." XYZ " X f@ f. Y f@ f. Z f@ f. CR
    ." UVW " U f@ f. V f@ f. W f@ f. CR
    ." Stack " .s CR 
\    ." FP " f.s CR CR
;

: skyfloor ( -- )  \ Inputs: X,Y,Z,U,V,W
                   \ Outputs: Setting the colour. 
    \ V<0 means we are looking at the floor (Y=-2)
    \ V>0 means we are looking at the sky
    v f@ f0< IF \ V<0
        \ plane is at Y=-2
        Y f@ 2,0 f+ v f@ f/ \ P=(Y+2)/V ( -- 'P' ) Previous 'P' is on stack
        \ checkerboard pattern
        \ (INT(X-U*P)+INT(Z-W*P)AND1
        X f@ fover U f@ f* f- f>s
        Z f@ fswap W f@ f* f- f>s
        + 1 and

        \ Could add different colours here for alternate squares of the floor
        \ Also could make the floor higher contrast - so that the reflections have more contrast 

        \ V=-V*(checkerboard)/2+.3)+.2
        \ 0.3 affects the colour of the dark squares
        \ the 0.2 gives a fade to the floor as it gets further away
        s>f 2,0 f/ 0,1 f+ v f@ fnegate f* 0,2 f+ v f!
        \ B=B+3*SQR V
        \ V is used here to be floor gradient, sqrt is gradual shortening 
        V f@ fsqrt 250,0 f* \ ." green=" fdup f. 
        f>s set_green_colour
    else
        \ V is used here to be sky gradient, sqrt is gradual shortening 
        \ V F@ ." V=" fdup f.
        V f@ fsqrt 256,0 f* \ ." white-cyan-blue=" fdup f. 
        f>s 256 swap - set_white_cyan_blue_colour
        \ maybe we should have more contrast in the sky?
        
    then
;


\ x and y coordinate of reflective spheres
fvariable sphere_coords

\ these are the trace_ray internal variables
fvariable E
fvariable F
fvariable P
fvariable D
fvariable T
fvariable G

: trace_ray_internal ( -- )
    1,0 G f!
    begin
        \ vector from sphere to ray start (e.g. camera)
        x f@ sphere_coords f@ f- e f!   \ E=X-I
        y f@ sphere_coords f@ f- f f!   \ F=Y-I
        \ Dot product
        \ P=U*E+V*F-W*Z
        U f@ E f@ f* 
        V f@ F f@ f* 
        f+
        W f@ Z f@ f* 
        f- 
        P f!
        \ discriminant - find roots via equation of a sphere 
        \ D=P*P-E*E-F*F-Z*Z+1
        P f@ fdup f* 
        E f@ fdup f* f- 
        F f@ fdup f* f- 
        Z f@ fdup f* f- 
        1,0 f+         \ 1 is the radius of the sphere
        D f!
        \ d>0 means ray intersects sphere
        \ d=0 means ray is tangent to sphere
        \ d<0 means ray misses sphere
        D f@ f0> if 
            \ ray intersects sphere
            P f@ fnegate D f@ fsqrt f- T f!  \ T=-P-SQR D
            T f@ f0> IF \ T>0
                T f@ U f@ f* X f@ f+ X f!   \ X=X+T*U
                T f@ V f@ f* Y f@ f+ Y f!   \ Y=Y+T*V
                Z f@ T f@ W f@ f* f- Z f!   \ Z=Z-T*W

                X f@ sphere_coords f@ f- E f!   \ E=X-sphere_coords
                Y f@ sphere_coords f@ f- F f!   \ F=Y-sphere_coords
                Z f@ G f!                       \ G=Z

                \ P=2*(U*E+V*F-W*G)
                U f@ E f@ f*
                V f@ F f@ f*
                f+
                W f@ G f@ f*
                f-
                2,0 f* P f!

                U f@ P f@ E f@ f* f- U f!   \ U=U-P*E
                V f@ P f@ F f@ f* f- V f!   \ V=V-P*F
                W f@ P f@ G f@ f* f+ W f!   \ W=W+P*G

                sphere_coords f@ fnegate sphere_coords f!
            else
                exit
            then
        else
            exit
        then
    again   \ hard coded recursion just in case tail-end recursion not implemented
            \ since no splitting of ray (single ray) looping is easy option.
;

: trace_ray
    \ show_ray_position
    trace_ray_internal
    \ eventually all rays will hit the sky/floor
    \ show_ray_position
    skyfloor
    \ show_ray_position
;


\ Sign of a float: -1 for negative number, 0 for zero, +1 for positive number
: fsgn ( F: n -- -1|0|+1 ) fdup f0< -rot f0> - s>f ;
: 1/fsqrt1 ( F: x y -- result ) fdup f* fswap fdup f* f+ 1,0 f+ fsqrt 1/f ;

screen_width 2/ s>f 0,5 f- fconstant screen_width_offset
screen_height 2/ s>f 0,5 f- fconstant screen_height_offset
screen_width 2/ s>f fconstant screen_scale


: ray_trace_line ( screen_y -- )
    screen_width 0 do \ m in original code, x coords
        \ camera position (not hex, floats!)
        0,0 X f!
        -0,1 Y f!
        3,0 Z f!

        \ calculate the ray vector
        I s>f screen_width_offset f- screen_scale f/ U f!    \ ray_x = (screen_x-159.5)/160
        dup s>f screen_height_offset f- screen_scale f/ V f!     \ ray_y = (screen_y-127.5)/160
    
        U f@ V f@ 1/fsqrt1 W f!         \ ray_z = 1/sqrt(x^2+y^2+1)
    
        \ normalise the ray vector
        U f@ W f@ f* U f! \ U=U*W
        V f@ W f@ f* V f! \ V=V*W

        \ set the mirror sphere coordinates, based on the position on the screen
        U f@ fsgn sphere_coords f!
        trace_ray
        \ unloop exit

        \ now plot the pixel on the screen
        I over PLOT    \ original was 4*M,4*N because virtual coords
    loop
    \ drop both copies of the screen_y coordinate
    drop
;

: ray_trace_all ( -- )
    screen_height 0 do \ n in original code
        \ ." Line = " I . CR
        I ray_trace_line
        lcd.display
        \ unloop exit
    loop
;


systick import 
: main
    systick-counter 
    CR ." Ray trace" CR
    RED fill_image_buffer
    lcd.init
    lcd.display
    ray_trace_all
    ." Done - took" systick-counter swap - 10000 / . ." seconds" CR
;


main


