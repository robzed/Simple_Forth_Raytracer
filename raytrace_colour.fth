\ Ray trace for gForth, output to a BMP file
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
\ And https://bbcmic.ro/#%7B%22v%22%3A1%2C%22program%22%3A%22MODE1%3AVDU5%3AB%3D0%5CnFORN%3D8TO247%3AFORM%3D0TO319%5CnX%3D0%3AY%3D-.1%3AZ%3D3%3AU%3D%28M-159.5%29%2F160%3AV%3D%28N-127.5%29%2F160%3AW%3D1%2FSQR%28U*U%2BV*V%2B1%29%3AU%3DU*W%3AV%3DV*W%3AI%3DSGNU%3AG%3D1%5CnE%3DX-I%3AF%3DY-I%3AP%3DU*E%2BV*F-W*Z%3AD%3DP*P-E*E-F*F-Z*Z%2B1%3AIFD%3E0T%3D-P-SQRD%3AIFT%3E0X%3DX%2BT*U%3AY%3DY%2BT*V%3AZ%3DZ-T*W%3AE%3DX-I%3AF%3DY-I%3AG%3DZ%3AP%3D2*%28U*E%2BV*F-W*G%29%3AU%3DU-P*E%3AV%3DV-P*F%3AW%3DW%2BP*G%3AI%3D-I%3AGOTO40%5CnIFV%3C0P%3D%28Y%2B2%29%2FV%3AV%3D-V*%28%28INT%28X-U*P%29%2BINT%28Z-W*P%29AND1%29%2F2%2B.3%29%2B.2%5CnB%3DB%2B3*SQRV%3AGCOL0%2C3-INTB%3AB%3DB-INTB%5CnPLOT69%2C4*M%2C4*N%3ANEXT%2C%22%7D
\

decimal
320 value screen_width 
256 value screen_height
24 value bits_per_pixel
3 value bytes_per_pixel

\ image buffer
create image_buffer screen_height screen_width * cells allot

: pixel_address ( x y -- addr )  screen_width * + cells image_buffer + ;

0 value plot_colour

: fill_image_buffer ( rgb_colour -- ) screen_height 0 DO
    screen_width 0 DO
        dup I J pixel_address !
    LOOP
LOOP drop ;

\ plot (0,0)=bottom left, (319,255)=top right
: plot ( x y -- ) screen_height swap - 1- pixel_address plot_colour swap ! ;

\ top_plot (0,0)=top left, (319,255)=bottom right
: top_plot ( x y -- ) pixel_address plot_colour swap ! ;

: draw_horizontal ( y -- )
    screen_width 0 DO
    I over top_plot
LOOP drop ;
: draw_vertical ( x -- ) 
    screen_height 0 DO
    dup I top_plot
LOOP drop ;

: set_blue_colour ( n -- ) $FF and to plot_colour ;
: set_green_colour ( n -- ) $FF and 8 lshift to plot_colour ;
: set_cyan_colour ( n -- ) $FF and dup 8 lshift + to plot_colour ;
: set_white_cyan_blue_colour ( n -- ) 
    dup 128 >= if
        \ from cyan to white
        128 - 2* 16 lshift $00FFFF + 
    else
        \ from blue to cyan
        2* 8 lshift $0000FF +
    then
    to plot_colour
;

\ Set colours
$FF0000 constant red
$0000FF constant blue
$00FF00 constant green
$FFFF00 constant yellow
$FFFFFF constant white

red constant dark
yellow constant light
white constant bright

\ BBC basic, in Mode 1 is 320x256, 4 colour display	
\ It has the following colours: 0=black, 1=red, 2=yellow, 3=white
: gcol ( n -- ) 
    dup 1 = if drop dark then
    dup 2 = if drop light then
    dup 3 = if drop bright then
    to plot_colour ;

0 Value fd-out
: open-output ( addr u -- )  w/o bin create-file throw to fd-out ;
: close-output ( -- )  fd-out close-file throw ;
: write>f ( addr u -- )  fd-out write-file throw ;
: emit>f ( c -- )  fd-out emit-file throw ;
: emit_u8 ( n -- )  255 and emit>f ;
: write_u32_le ( n -- )  dup emit_u8 8 rshift 
                         dup emit_u8 8 rshift 
                         dup emit_u8 8 rshift 
                             emit_u8 ;
: write_u16_le ( n -- )  dup emit_u8 8 rshift 
                             emit_u8 ;
: write_u24_le ( n -- )  dup emit_u8 8 rshift 
                         dup emit_u8 8 rshift 
                             emit_u8 ;
: calc_bmp_size ( -- n )  14 40 3 * + image_buffer screen_width * screen_height * + ;
: write_bmp ( addr u -- ) open-output
    \ bitmap file header
    S" BM" write>f
    calc_bmp_size write_u32_le \ file size
    0 write_u32_le  \ reserved
    14 40 + write_u32_le \ offset to image data

    \ bitmap info header
    40 write_u32_le \ header size
    screen_width write_u32_le \ image width
    screen_height write_u32_le \ image height
    1 write_u16_le \ planes
    bits_per_pixel write_u16_le \ bits per pixel
    0 write_u32_le \ compression, 0 for BI_RGB
    bytes_per_pixel screen_width * screen_height * write_u32_le \ image size, can be 0 for BI_RGB
    0 write_u32_le \ x pixels per meter
    0 write_u32_le \ y pixels per meter
    0 write_u32_le \ colours used
    0 write_u32_le \ important colours

    \ image data
    \ 24 bits per pixel, 3 bytes per pixel
    \ bottom to top, left to right
    \ 3 bytes per pixel, blue, green, red (BGR) - little endian (effectively)
    \ 0,0 is bottom left. 319,255 is top right

    \ calculate the bottom left corner of the image
    0 screen_height 1- pixel_address \ bottom left
    screen_height 0 DO
        screen_width 0 DO
            dup @ write_u24_le
            cell+ \ next pixel
        LOOP
        screen_width 2* cells - \ next line, up for .bmp
    LOOP
    drop \ drop the address

    close-output
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
    ." FP " f.s CR CR
    ;

: skyfloor ( -- )  \ Inputs: X,Y,Z,U,V,W
                   \ Outputs: Setting the colour. 
    \ V<0 means we are looking at the floor (Y=-2)
    \ V>0 means we are looking at the sky
    v f@ f0< IF \ V<0
        \ plane is at Y=-2
        Y f@ 2e0 f+ v f@ f/ \ P=(Y+2)/V ( -- 'P' ) Previous 'P' is on stack
        \ checkerboard pattern
        \ (INT(X-U*P)+INT(Z-W*P)AND1
        X f@ fover U f@ f* f- f>s
        Z f@ fswap W f@ f* f- f>s
        + 1 and
        \ V=-V*(checkerboard)/2+.3)+.2
        \ 0.3 affects the colour of the dark squares
        \ the 0.2 gives a fade to the floor as it gets further away
        s>f 2e0 f/ 0.1e0 f+ v f@ fnegate f* 0.2e0 f+ v f!
        \ B=B+3*SQR V
        \ V is used here to be floor gradient, sqrt is gradual shortening 
        V f@ fsqrt 250e0 f* 
        f>s set_green_colour
    else
        \ V is used here to be sky gradient, sqrt is gradual shortening 
        V f@ fsqrt 256e0 f* 
        f>s 256 swap - set_white_cyan_blue_colour
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
    1e0 G f!
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
        1e f+         \ 1 is the radius of the sphere
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
                2e f* P f!

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
: fsgn ( F: n -- -1|0|+1 ) fdup f0< f0> - s>f ;
: 1/fsqrt1 ( F: x y -- result ) fdup f* fswap fdup f* f+ 1e f+ fsqrt 1/f ;

: ray_trace_line ( screen_y -- ) ( F: screen_y -- )
    screen_width 0 do \ m in original code, x coords
        \ camera position (not hex, floats!)
        0e X f!
        -.1e Y f!
        3e Z f!

        \ calculate the ray vector
        I s>f 159.5e f- 160e f/ U f!    \ ray_x = (screen_x-159.5)/160
        fdup 127.5e f- 160e f/ V f!     \ ray_y = (screen_y-127.5)/160
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
    drop fdrop
;

: ray_trace_all ( -- )
    screen_height 0 do \ n in original code
        \ ." Line = " I . CR
        I s>f I ray_trace_line
        \ unloop exit
    loop
;

: _bitmap_test
    $80 fill_image_buffer
    $ff0000 gcol
    10 draw_horizontal
    $ff00ff gcol
    20 draw_horizontal
    $8000FF gcol         
    120 draw_horizontal

    20 draw_vertical
    1 gcol
    100 draw_vertical
    2 gcol
    200 draw_vertical
    0 gcol
    300 draw_vertical
    3 gcol
    310 draw_vertical
    S" raytrace.bmp" write_bmp
;

: main
    CR ." Ray trace" CR
    $000080 fill_image_buffer
    ray_trace_all
    ." Writing BMP file..." CR
    S" raytrace_colour.bmp" write_bmp
    ." Done" CR
;


main
\ .s cr
\ f.s cr
bye



