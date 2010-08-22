; $B%G%P%C%0%W%j%s%H(B
(define (intensity-debug-print i j mean)
  (let* (
          (prefix
            (if (= i j)
              (number->string i)
              (string-append
                (number->string i) "-" (number->string j))
            ))
        )
    (gimp-message
      (string-append prefix " => " (number->string mean)))
  )
)

(define (the-image)
  (aref (car (cdr (gimp-image-list))) 0)
)
(define (the-layer)
  (car (gimp-image-get-active-layer (the-image)))
)

(define (selection-grow-and-shrink img params)
  (if (not (null? params))
    (let* (
            (param (car params))
            (rest (cdr params))
          )
      (cond
        ((< param 0) (gimp-selection-shrink img (abs param)))
        ((> param 0) (gimp-selection-grow img param))
      )
      (selection-grow-and-shrink img rest)
    )
  )
)

; $BHO0O$NA0$N$[$&$+$i$J$a$F$$$C$F(B0$B$h$jBg$-$$CM$,=P$k$H$3$m$r8!=P$9$k(B
(define (detect-positive-intensity layer start-range end-range step)
  (let* (
          (i start-range)
          (result -1)
        )
    (while (and (<= (+ i step) end-range) (< result 0))
      (let* (
              (j (+ i step -1))
              (histogram (gimp-histogram layer 0 i j))
              (mean (car (cdr (cddddr histogram))))
            )
        (intensity-debug-print i j mean)
        (if (> mean 0.0) (set! result i))
        (set! i (+ i step))
      )
    )
    result
  )
)

; $BHO0O$N8e$N$[$&$+$i$J$a$F$$$C$F%T!<%/$r8!=P$9$k(B
(define (detect-peak-of-intensity layer start-range end-range step)
  (let* (
          (i (- end-range step))
          (prev -1)
          (result -1)
        )
    (while (and (<= start-range i) (< result 0))
      (let* (
              (j (+ i step -1))
              (histogram (gimp-histogram layer 0 i j))
              (mean (car (cdr (cddddr histogram))))
            )
        (intensity-debug-print i j mean)
        (if (>= mean prev)
          (set! prev mean)
          (set! result (+ i step)) ; $B0l$DA0$N%(%j%"$K%T!<%/$,$"$k(B
        )
      )
      (set! i (- i step))
    )
    result
  )
)

(define (detect-x-percent-of-peak-of-intensity layer x start step)
  (let *(
          (i (- start step))
          (value -1)
          (result -1)
        )
    (while (and (< 0 i) (< result 0))
      (let* (
              (j (+ i step -1))
              (histogram (gimp-histogram layer 0 i j))
              (mean (car (cdr (cddddr histogram))))
            )
        (if (< value 0)
          (set! value (* mean x))
        )
        (if (<= mean value)
          (set! result (+ i step))
        )
      )
      (set! i (- i step))
    )
    result
  )
)

; $B%0%l!<%9%1!<%k2hA|$NGr$H$P$7$r$9$k$?$a$N%l%Y%k$r?dDj$9$k(B
(define (estimate-levels-of-grayscale-image layer)
  (let* (
          (rough-step 10)
          (rough-low-input (detect-positive-intensity layer 0 100 rough-step))
          (rough-high-input (detect-peak-of-intensity layer 155 255 rough-step))
          (low-input -1)
          (high-input -1)
        )
    (if (>= rough-low-input 0)
      (set! low-input
        (detect-positive-intensity layer rough-low-input (+ rough-low-input rough-step) 1)
      )
    )
    (if (>= rough-high-input 0)
      (set! high-input
        (detect-peak-of-intensity layer rough-high-input (+ rough-high-input rough-step) 1)
      )
      (if (< high-input 0) ; $B$A$g$&$I6-3&$K%T!<%/$,$"$C$?>l9g(B
        (set! high-input rough-high-input)
      )
      (set! high-input
        (detect-x-percent-of-peak-of-intensity layer 0.95 high-input 1))
    )
    (list low-input high-input)
  )
)

(define (script-fu-estimate-and-set-levels img layer)
  (let* (
          (levels (estimate-levels-of-grayscale-image layer))
          (low-input (car levels))
          (high-input (car (cdr levels)))
        )
    (if (or (< low-input 0) (< high-input 0))
      (gimp-message
        (string-append
          "Failed to estimate levels (low: " (number->string low-input)
          ", high: " (number->string high-input) ")"
        )
      )
      (begin
        (gimp-levels layer HISTOGRAM-VALUE low-input high-input 1.0 0 255)
        (gimp-displays-flush)
      )
    )
  )
)

(script-fu-register "script-fu-estimate-and-set-levels"
                    "Adjust levels..."
                    "Estimates levels and set it to the drawable"
                    "ay"
                    "ay"
                    "2010-08-14"
                    ""
                    SF-IMAGE "Input Input" 0
                    SF-DRAWABLE "Input Layer" 0
                    )

(script-fu-menu-register "script-fu-estimate-and-set-levels"
                         "<Image>/Filters/Test")

(define (script-fu-jisui-grayscale img layer)
  (if (= 0 (car (gimp-drawable-is-gray layer)))
    (begin
      (gimp-image-convert-grayscale img)
      (gimp-displays-flush)))
  )

(script-fu-register "script-fu-jisui-grayscale"
                    "Make it grayscale..."
                    "Changes image mode to grayscale"
                    "ay"
                    "ay"
                    "2010-08-13"
                    ""
                    SF-IMAGE "Input Image" 0
                    SF-DRAWABLE "Input Layer" 0
                    )
(script-fu-menu-register "script-fu-jisui-grayscale"
                         "<Image>/Filters/Test")

(define (script-fu-jisui-over-exposure img layer)
  (gimp-image-undo-group-start img)
  (let* (
          (copied (car (gimp-layer-copy layer FALSE)))
        )
    (gimp-selection-clear img)
    (gimp-image-add-layer img copied -1)
    (gimp-image-set-active-layer img copied)

    (plug-in-unsharp-mask 1 img copied 1.0 5.0 37); 500%, 1.0px, 24
    (gimp-by-color-select copied '(255 255 255) 70 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)

    (gimp-image-remove-layer img copied)
    (selection-grow-and-shrink img '(-2 1 -1))
    (gimp-levels layer HISTOGRAM-VALUE 0 230 1.0 0 255)

    (gimp-selection-clear img)
  )
  (gimp-image-undo-group-end img)
  (gimp-displays-flush)
)

(script-fu-register "script-fu-jisui-over-exposure"
                    "Make it over exposure..."
                    "Clean ups the image by making exposure over (300dpi)"
                    "ay"
                    "ay"
                    "2010-08-13"
                    ""
                    SF-IMAGE "Input Input" 0
                    SF-DRAWABLE "Input Layer" 0
                    )

(script-fu-menu-register "script-fu-jisui-over-exposure"
                         "<Image>/Filters/Test")

(define (script-fu-jisui-over-exposure-paranoid img layer)
  (gimp-image-undo-group-start img)
  (let* (
          (copied (car (gimp-layer-copy layer FALSE)))
        )
    (gimp-selection-clear img)
    (gimp-image-add-layer img copied -1)
    (gimp-image-set-active-layer img copied)

    (plug-in-unsharp-mask 1 img copied 0.5 5.0 45); 500%, 0.5px, 24
    (plug-in-sel-gauss 1 img copied 20 20) ; 20 25
    (plug-in-sel-gauss 1 img copied 20 20) ; 20 25

    (gimp-by-color-select copied '(255 255 255) 20 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
    (selection-grow-and-shrink img '(-2 -2))
    (script-fu-distress-selection img layer 127 0 2 5 TRUE TRUE)
    (selection-grow-and-shrink img '(1 1 1 1 -2 -2 -2))

    (plug-in-gauss 1 img copied 2 2 0)
    (gimp-selection-clear img)
    (plug-in-sel-gauss 1 img copied 20 20) ; 20 25
    (gimp-by-color-select copied '(255 255 255) 40 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)

    (selection-grow-and-shrink img '(-1 -1 -1))
    (gimp-image-remove-layer img copied)
    (gimp-levels layer HISTOGRAM-VALUE 0 225 1.0 0 255)
    (gimp-levels layer HISTOGRAM-VALUE 0 225 1.0 0 255)
    (gimp-selection-clear img)
  )
  (gimp-image-undo-group-end img)
  (gimp-displays-flush)
)

(script-fu-register "script-fu-jisui-over-exposure-paranoid"
                    "Make it over exposure (paranoid)..."
                    "Clean ups the image by making exposure over (300dpi/paranoid)"
                    "ay"
                    "ay"
                    "2010-08-13"
                    ""
                    SF-IMAGE "Input Input" 0
                    SF-DRAWABLE "Input Layer" 0
                    )

(script-fu-menu-register "script-fu-jisui-over-exposure-paranoid"
                         "<Image>/Filters/Test")

(define (script-fu-jisui-over-exposure-600dpi img layer)
  (gimp-image-undo-group-start img)
  (let* (
          (copied (car (gimp-layer-copy layer FALSE)))
        )
    (gimp-selection-clear img)
    (gimp-image-add-layer img copied -1)
    (gimp-image-set-active-layer img copied)

    (plug-in-unsharp-mask 1 img copied 2.0 5.0 50); 500%, 2.0px, 40

    (gimp-by-color-select copied '(255 255 255) 80 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
    (selection-grow-and-shrink img '(-4 -4))
    (script-fu-distress-selection img layer 127 0 2 8 TRUE TRUE); $B3j$i$+(B8
    (selection-grow-and-shrink img '(8 -4))
    (plug-in-gauss 1 img copied 2 2 0)
    (plug-in-gauss 1 img copied 2 2 0)

    (gimp-selection-clear img)
    (plug-in-sel-gauss 1 img copied 25 25); 85 60
    (gimp-by-color-select copied '(255 255 255) 80 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
    (selection-grow-and-shrink img '(-2 -1))
    (gimp-selection-feather img 2)

    (gimp-image-remove-layer img copied)
    (let* ((bg (car (gimp-context-get-background))))
      (gimp-context-set-background '(255 255 255))
      (gimp-edit-fill layer BACKGROUND-FILL)
      (gimp-context-set-background bg)
    )

    (gimp-selection-clear img)
  )
  (gimp-image-undo-group-end img)
  (gimp-displays-flush)
)

(script-fu-register "script-fu-jisui-over-exposure-600dpi"
                    "Make it over exposure (600dpi)..."
                    "Clean ups the image by making exposure over (600dpi)"
                    "ay"
                    "ay"
                    "2010-08-13"
                    ""
                    SF-IMAGE "Input Input" 0
                    SF-DRAWABLE "Input Layer" 0
                    )

(script-fu-menu-register "script-fu-jisui-over-exposure-600dpi"
                         "<Image>/Filters/Test")

; $BNX3T:Y@~2=%7%c!<%W(B
;
; 2010-08-15$B%a%b(B
; $B$"$^$j$&$^$/$$$+$J$$!#5U$KJ8;z$J$I$OB@$/$J$j!"%H!<%s$bG;$/$J$C$F$7$^$&!#(B
; $B%*!<%P%l%$$G$J$/%9%/%j!<%s$GE}9g$9$k$H$$$/$i$+$=$l$C$]$$7k2L$K$J$k$,!"$3$A$i$O%H!<%s$,Gv$/$J$k$h$&$J!#(B
; Elements$B$G$O$J$$(BPhotoshop$B$NF0:n$HHf$Y$J$$$HDI5a$OFq$7$=$&!#(B
; $B!V$h$/J,$+$i$J$1$l$PHt$P$7$F$b$h$$!W$H$"$k$7!"$H$j$"$($:$"$-$i$a!#(B
;
; 2010-0822$B%a%b(B
; $BC19TK\$J$I%=!<%9$,$-$l$$$@$H8z2L$,$"$k$h$&$@!#(B
; $B;(;o$+$i$G$b;vA0$N=hM}$,==J,$K$G$-$F$$$l$P$&$^$/8z2L$,=P$k$N$+$b$7$l$J$$!#(B
(define (script-fu-jisui-narrow-and-sharpen img layer)
  (gimp-image-undo-group-start img)

  (let* (
          (copied (car (gimp-layer-copy layer FALSE)))
          (sel1)
          (sel2)
          (sel3)
          (merged-layer)
        )
    (gimp-selection-clear img)
    (gimp-image-add-layer img copied -1)

    (gimp-by-color-select copied '(255 255 255) 20 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
    (selection-grow-and-shrink img '(-2 10))
    (script-fu-distress-selection img layer 127 0 2 10 TRUE TRUE)
    (gimp-selection-grow img 8)

    (selection-grow-and-shrink img '(-10 -10 -5 -5))
    (set! sel1 (car (gimp-selection-save img)))

    (gimp-selection-clear img)
    (gimp-image-set-active-layer img copied)
    (gimp-levels-auto copied)
    (gimp-by-color-select copied '(0 0 0) 150 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)

    (gimp-selection-grow img 2)
    (gimp-image-remove-layer img copied)
    (set! sel2 (car (gimp-selection-save img)))
    (gimp-selection-clear img)

    (set! copied (car (gimp-layer-copy layer FALSE)))
    (gimp-image-add-layer img copied -1)
    (gimp-image-set-active-layer img copied)
    (plug-in-edge 1 img copied 1.0 1 3); 1.0/SMEAR/ROBERTS
    (gimp-invert copied)
    (gimp-by-color-select copied '(0 0 0) 20 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
    (gimp-image-remove-layer img copied)

    (set! sel3 (car (gimp-selection-save img)))
    (gimp-selection-clear img)
    (gimp-selection-combine sel1 CHANNEL-OP-REPLACE)
    (gimp-selection-combine sel2 CHANNEL-OP-INTERSECT)
    (gimp-selection-combine sel3 CHANNEL-OP-INTERSECT)
    (gimp-edit-copy layer)
    (let* (
            (width (car (gimp-drawable-width layer)))
            (height (car (gimp-drawable-height layer)))
            (tmp-layer (car (gimp-layer-new img width height GRAY-IMAGE "tmp" 100 OVERLAY-MODE)))
          )
      (gimp-image-add-layer img tmp-layer -1)
      (gimp-floating-sel-anchor
        (car (gimp-edit-paste tmp-layer TRUE)))
      (gimp-brightness-contrast tmp-layer 0 -50)
      (set! merged-layer (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY))
    )
    (gimp-image-remove-channel img sel1)
    (gimp-image-remove-channel img sel2)
    (gimp-image-remove-channel img sel3)

    (gimp-selection-clear img)

    (gimp-image-undo-group-end img)
    (gimp-displays-flush)

    merged-layer
  )


)

(script-fu-register "script-fu-jisui-narrow-and-sharpen"
                    "Narrows and sharpens outlines..."
                    "Narrows and sharpens outlines"
                    "ay"
                    "ay"
                    "2010-08-15"
                    ""
                    SF-IMAGE "Input Input" 0
                    SF-DRAWABLE "Input Layer" 0
                    )

(script-fu-menu-register "script-fu-jisui-narrow-and-sharpen"
                         "<Image>/Filters/Test")

; $BL@3%?'2<$2!#(B($BCf4V?'$+$iGr$KEO$C$FGr$K$h$j$9$.$?$N$r9uB&$KLa$9!#(B)
(define (script-fu-jisui-gammadown-lightgray img layer)
  (gimp-image-undo-group-start img)

  (gimp-by-color-select layer '(255 255 255) 200 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
  (gimp-levels layer HISTOGRAM-VALUE 0 255 0.7 0 255)
  (gimp-selection-clear img)

  (gimp-image-undo-group-end img)
  (gimp-displays-flush)
)

(script-fu-register "script-fu-jisui-gammadown-lightgray"
                    "Downs gamma value of lightgray..."
                    "Downs gamma value of lightgray..."
                    "ay"
                    "ay"
                    "2010-08-15"
                    ""
                    SF-IMAGE "Input Input" 0
                    SF-DRAWABLE "Input Layer" 0
                    )

(script-fu-menu-register "script-fu-jisui-gammadown-lightgray"
                         "<Image>/Filters/Test")

; $B=L>.(B
(define (reduce-image-size-by-two-stages img layer height)
  (gimp-image-undo-group-start img)

  (let* (
          (orig-height (car (gimp-drawable-height layer)))
          (orig-width (car (gimp-drawable-width layer)))
          (tmp-height (- orig-height (modulo orig-height height)))
          (tmp-width (* orig-width (/ tmp-height orig-height)))
        )
    (if (> orig-height height)
      (begin
        (if (not (= orig-height tmp-height))
          (gimp-image-scale-full
            img tmp-width tmp-height INTERPOLATION-LANCZOS))
        (gimp-image-scale-full
          img (* tmp-width (/ height tmp-height)) height INTERPOLATION-LANCZOS)
      )
    )
  )
  
  (gimp-image-undo-group-end img)
)

(define (script-fu-jisui-reduce-image-size img layer)
  (let* (
          (height 2000)
        )
    (reduce-image-size-by-two-stages img layer height)
    (gimp-displays-flush)
  )
)

(script-fu-register "script-fu-jisui-reduce-image-size"
                    "Reduce image size to 2000 by two stages..."
                    "Reduce image size to 2000 by two stages..."
                    "ay"
                    "ay"
                    "2010-08-15"
                    ""
                    SF-IMAGE "Input Input" 0
                    SF-DRAWABLE "Input Layer" 0
                    )

(script-fu-menu-register "script-fu-jisui-reduce-image-size"
                         "<Image>/Filters/Test")

; $BNX3T6/D4(B
(define (script-fu-jisui-edge-emphasis img layer)
  (gimp-image-undo-group-start img)

  (let* (
          (copied (car (gimp-layer-copy layer FALSE)))
        )
    (gimp-image-add-layer img copied -1)
    (gimp-image-set-active-layer img copied)
    (plug-in-edge 1 img copied 1.0 1 3); 1.0/SMEAR/ROBERTS
    (gimp-invert copied)
    (gimp-by-color-select copied '(0 0 0) 100 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
    (gimp-image-remove-layer img copied)
    (plug-in-unsharp-mask 1 img layer 1.0 0.8 30); 80%, 1px, 20

    (gimp-selection-clear img)
  )

  (gimp-image-undo-group-end img)
  (gimp-displays-flush)
)

(script-fu-register "script-fu-jisui-edge-emphasis"
                    "Emphasizes edges..."
                    "Emphasizes edges..."
                    "ay"
                    "ay"
                    "2010-08-15"
                    ""
                    SF-IMAGE "Input Input" 0
                    SF-DRAWABLE "Input Layer" 0
                    )

(script-fu-menu-register "script-fu-jisui-edge-emphasis"
                         "<Image>/Filters/Test")

; $B:G8e$K7Z$/GrHt$P$7(B
(define (script-fu-jisui-fixup-over-exposure img layer)
  (gimp-image-undo-group-start img)

  (gimp-by-color-select layer '(255 255 255) 25 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
  (selection-grow-and-shrink img '(-1 -1))
  (gimp-levels layer HISTOGRAM-VALUE 0 240 1.0 0 255)
  (gimp-selection-clear img)

  (gimp-image-undo-group-end img)
  (gimp-displays-flush)
)

(script-fu-register "script-fu-jisui-fixup-over-exposure"
                    "Make it over exposure (fixup)..."
                    "Make it over exposure (fixup)..."
                    "ay"
                    "ay"
                    "2010-08-15"
                    ""
                    SF-IMAGE "Input Input" 0
                    SF-DRAWABLE "Input Layer" 0
                    )

(script-fu-menu-register "script-fu-jisui-fixup-over-exposure"
                         "<Image>/Filters/Test")

; $BA4It(B
; $B%=!<%9$,;(;o$N>l9g8~$1!#(B
; $BC19TK\%=!<%9$KBP$7$FE,MQ$9$k$H$d$j$9$.$K$J$k$N$GCm0U!#(B
; ($B$-$l$$$K%9%-%c%s$G$-$F$$$k$J$i=L>.$H6/D4$/$i$$$G$$$$$N$+$b!#(B)
(define (script-fu-jisui-do-all img layer)
  (let* ((merged-layer layer))
    (gimp-message "[1] BEGIN")
    (script-fu-jisui-grayscale img layer)
    (gimp-message "[2]")
;    (reduce-image-size-by-two-stages img layer 4800) ; $B$3$3$G(B3200$B$K$7$F$7$^$&$HGrHt$P$7$7$-$l$J$$$h$&$@(B($B%=!<%9$K$h$k$+$b(B)$B!#(BPC$B%9%Z%C%/$,==J,$J$i85%5%$%:$N$^$^$G$b9=$o$J$$!#(B
    (gimp-message "[3]")
    (script-fu-estimate-and-set-levels img layer)
    (gimp-message "[4]")
    (script-fu-jisui-over-exposure-600dpi img layer)
    (gimp-message "[5]")
; $B$3$N;~E@$G$"$kDxEY$-$l$$$J;E>e$,$j$K$J$C$F$$$J$$$H$3$m$K(B
; $B$3$l$rE,MQ$9$k$H;E>e$,$j$,0-$/$J$k(B...
;    (reduce-image-size-by-two-stages img layer 2800)
;    (set! merged-layer
;      (car (script-fu-jisui-narrow-and-sharpen img layer)))
    (gimp-message "[6]")
    (script-fu-jisui-gammadown-lightgray img merged-layer)
    (gimp-message "[7]")
;    (reduce-image-size-by-two-stages img merged-layer 1600)
    (gimp-message "[8]")
    (script-fu-jisui-edge-emphasis img merged-layer)
    (gimp-message "[9]")
    (script-fu-jisui-fixup-over-exposure img merged-layer)
    (gimp-message "[10] FINISH")
  )
)

(script-fu-register "script-fu-jisui-do-all"
                    "Jisui do all..."
                    "Jisui do all..."
                    "ay"
                    "ay"
                    "2010-08-15"
                    ""
                    SF-IMAGE "Input Input" 0
                    SF-DRAWABLE "Input Layer" 0
                    )

(script-fu-menu-register "script-fu-jisui-do-all"
                         "<Image>/Filters/Test")

; $BA4It(B

