; デバッグプリント
(define (intensity-debug-print i j value)
  (let* (
          (prefix
            (if (= i j)
              (number->string i)
              (string-append
                (number->string i) "-" (number->string j))
            ))
        )
    (gimp-message
      (string-append prefix " => " (number->string value)))
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

; 範囲の前のほうからなめていって0より大きい値が出るところを検出する
(define (detect-positive-intensity layer start-range end-range step)
  (let* (
          (i start-range)
          (result -1)
        )
    (while (and (<= (+ i step) end-range) (< result 0))
      (let* (
              (j (+ i step -1))
              (histogram (gimp-histogram layer 0 i j))
              (pct (car (cdr (cddddr histogram))))
            )
        (intensity-debug-print i j pct)
        (if (> pct 0.0) (set! result i))
        (set! i (+ i step))
      )
    )
    result
  )
)

; 範囲の後のほうからなめていってピークを検出する
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
              (pct (car (cdr (cddddr histogram))))
            )
        (intensity-debug-print i j pct)
        (if (>= pct prev)
          (set! prev pct)
          (set! result (+ i step)) ; 一つ前のエリアにピークがある
        )
      )
      (set! i (- i step))
    )
    result
  )
)

(define (detect-x-percent-of-peak-of-intensity layer x start step)
  (let* (
          (i (- start step))
          (value -1)
          (result -1)
        )
    (while (and (< 0 i) (< result 0))
      (let* (
              (j (+ i step -1))
              (histogram (gimp-histogram layer 0 i j))
              (pct (car (cdr (cddddr histogram))))
            )
        (if (< value 0)
          (set! value (* pct x))
        )
        (if (<= pct value)
          (set! result (+ i step))
        )
      )
      (set! i (- i step))
    )
    result
  )
)

; グレースケール画像の白とばしをするためのレベルを推定する
(define (estimate-levels-of-grayscale-image layer)
  (let* (
          (low-input -1)
          (high-input -1)
        )
    (set! low-input
      (detect-positive-intensity layer 0 100 1)
    )
    (set! high-input
      (detect-peak-of-intensity layer 155 255 1)
    )
    (set! high-input
      (detect-x-percent-of-peak-of-intensity layer 0.55 high-input 1))
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

    (plug-in-unsharp-mask 1 img copied 1.0 2.0 24); 500%, 1.0px, 24
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

    (plug-in-unsharp-mask 1 img copied 0.5 5.0 24); 500%, 0.5px, 24
    (plug-in-sel-gauss 1 img copied 20 20) ; 20 25
    (plug-in-sel-gauss 1 img copied 20 20) ; 20 25

    (gimp-by-color-select copied '(255 255 255) 20 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
    (selection-grow-and-shrink img '(-2 -2))
    (gimp-selection-feather img 5)(gimp-selection-sharpen img); 滑らか5
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

    (plug-in-unsharp-mask 1 img copied 2.0 5.0 40); 500%, 2.0px, 40

    (gimp-by-color-select copied '(255 255 255) 80 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
    (selection-grow-and-shrink img '(-4 -4))
    (gimp-selection-feather img 8)(gimp-selection-sharpen img); 滑らか8
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

; 輪郭細線化シャープ
;
; 2010-08-15メモ
; あまりうまくいかない。逆に文字などは太くなり、トーンも濃くなってしまう。
; オーバレイでなくスクリーンで統合するといくらかそれっぽい結果になるが、こちらはトーンが薄くなるような。
; ElementsではないPhotoshopの動作と比べないと追求は難しそう。
; 「よく分からなければ飛ばしてもよい」とあるし、とりあえずあきらめ。
;
; 2010-0822メモ
; 単行本などソースがきれいだと効果があるようだ。
; 雑誌からでも事前の処理が十分にできていればうまく効果が出るのかもしれない。
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
    (gimp-selection-feather img 10)(gimp-selection-sharpen img); 滑らか10
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

; 明灰色下げ。(中間色から白に渡って白によりすぎたのを黒側に戻す。)
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

; 縮小
(define (reduce-image-size-by-two-stages img layer height)
  (gimp-image-undo-group-start img)

  (let* (
          (orig-height (car (gimp-drawable-height layer)))
          (orig-width (car (gimp-drawable-width layer)))
          (tmp-height (- orig-height (modulo orig-height height)))
          (tmp-width (round (* orig-width (/ tmp-height orig-height))))
        )
    (if (> orig-height height)
      (begin
        (if (not (= orig-height tmp-height))
          (gimp-image-scale-full
            img tmp-width tmp-height INTERPOLATION-LANCZOS))
        (gimp-image-scale-full
          img (round (* tmp-width (/ height tmp-height))) height INTERPOLATION-LANCZOS)
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

; 輪郭強調
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
    (plug-in-unsharp-mask 1 img layer 1.0 0.8 20); 80%, 1px, 20

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

; 最後に軽く白飛ばし
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

; 全部
; ソースが雑誌の場合向け。
; 単行本ソースに対して適用するとやりすぎになるので注意。
; (きれいにスキャンできているなら縮小と強調くらいでいいのかも。)
(define (script-fu-jisui-do-all img layer)
  (let* ((merged-layer layer))
    (gimp-message "[1] BEGIN")
    (script-fu-jisui-grayscale img layer)
    (gimp-message "[2]")
;    (reduce-image-size-by-two-stages img layer 4800) ; ここで3200にしてしまうと白飛ばししきれないようだ(ソースによるかも)。PCスペックが十分なら元サイズのままでも構わない。
    (gimp-message "[3]")
    (script-fu-estimate-and-set-levels img layer)
    (gimp-message "[4]")
    (script-fu-jisui-over-exposure-600dpi img layer)
    (gimp-message "[5]")
; この時点である程度きれいな仕上がりになっていないところに
; これを適用すると仕上がりが悪くなる...
;    (reduce-image-size-by-two-stages img layer 2800)
;    (set! merged-layer
;      (car (script-fu-jisui-narrow-and-sharpen img layer)))
    (gimp-message "[6]")
    (script-fu-jisui-gammadown-lightgray img merged-layer)
    (gimp-message "[7]")
;    (reduce-image-size-by-two-stages img merged-layer 2000)
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

; 全部

