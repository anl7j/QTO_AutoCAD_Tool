;;; ==========================================================================
;;;  AutoCAD Quantity Takeoff (QTO) Tool - أداة حصر الكميات للمخططات الهندسية
;;;  Version: 1.7 (Engineering BOQ & Multi-Sheet Metraj Excel Engine)
;;;  Commands:
;;;    QTO         - تشغيل حصر الكميات التفاعلي (أطوال، مساحات، بلوكات)
;;;    HASR        - اختصار لنفس الأداة
;;;    QTO-INSTALL - التثبيت الدائم ليعمل تلقائياً في كل مرة يفتح فيها أوتوكاد
;;;    QTO-UPDATE  - التحديث التلقائي للإضافة مباشرة من GitHub
;;; ==========================================================================

(vl-load-com)

;;; رقم الإصدار الحالي
(setq *qto-version* "1.7")

;;; --------------------------------------------------------------------------
;;; دالة التحقق من أن القيمة رقم (صحيح أو عشري)
;;; --------------------------------------------------------------------------
(defun qto:numberp (x)
  (and x (or (= (type x) 'REAL) (= (type x) 'INT)))
)

;;; --------------------------------------------------------------------------
;;; أمر التثبيت الدائم بنقرة واحدة: QTO-INSTALL
;;; يقوم بإضافة الإضافة إلى acaddoc.lsp لتعمل تلقائياً في كل مخطط يفتحه المستخدم
;;; --------------------------------------------------------------------------
(defun c:QTO-INSTALL ( / local-path sup-folder acaddoc-path f clean-path)
  (princ "\nجاري إعداد التثبيت الدائم للإضافة داخل أوتوكاد ...")
  (setq local-path (findfile "QTO.lsp"))
  (if (null local-path)
    (setq local-path (getfiled "حدد مسار ملف QTO.lsp لتثبيته الدائم" "QTO.lsp" "lsp" 0))
  )
  (if local-path
    (progn
      (setq sup-folder (strcat (getvar "ROAMABLEROOTPREFIX") "Support\\"))
      (setq acaddoc-path (strcat sup-folder "acaddoc.lsp"))
      (setq f (open acaddoc-path "a"))
      (if (null f)
        (setq f (open acaddoc-path "w"))
      )
      (if f
        (progn
          (setq clean-path (vl-string-translate "\\" "/" local-path))
          (write-line "\n;;; AutoCAD Quantity Takeoff Auto-Loader" f)
          (write-line "(vl-load-com)" f)
          (write-line (strcat "(if (findfile \"" clean-path "\") (load \"" clean-path "\"))") f)
          (close f)
          (alert (strcat "تم تثبيت الإضافة بنجاح في أوتوكاد!\n\n"
                         "من الآن فصاعداً ستعمل الإضافة تلقائياً في كل مرة تفتح فيها أي مخطط أوتوكاد\n"
                         "دون الحاجة لسحبها أو تحميلها يدوياً نهائياً."))
          (princ "\n[✓] تم بنجاح ضبط التحميل التلقائي الدائم عبر ملف acaddoc.lsp.")
        )
        (alert "تعذر الكتابة في مجلد الدعم. يمكنك استخدام أمر APPLOAD وقسم Startup Suite بدلاً من ذلك.")
      )
    )
    (alert "لم يتم تحديد مسار ملف QTO.lsp.")
  )
  (princ)
)

(defun c:HASR-INSTALL () (c:QTO-INSTALL))

;;; --------------------------------------------------------------------------
;;; دوال كتابة بايتات UTF-8 صريحة لضمان ظهور النصوص العربية بدون أي تشوه
;;; --------------------------------------------------------------------------

(defun qto:write-bytes (lst f)
  (foreach b lst (write-char b f))
)

;; علامة البداية UTF-8 BOM
(defun qto:write-utf8-bom (f)
  (qto:write-bytes '(239 187 191) f)
)

;; تحويل 4 خانات Hex إلى عدد صحيح
(defun qto:hex4->int (h / i val c digits pos)
  (setq digits "0123456789ABCDEF")
  (setq val 0)
  (setq i 1)
  (setq h (strcase h))
  (while (<= i 4)
    (setq c (substr h i 1))
    (setq pos (vl-string-search c digits))
    (if pos
      (setq val (+ (* val 16) pos))
    )
    (setq i (1+ i))
  )
  val
)

;; كتابة Code Point كـ بايتات UTF-8
(defun qto:write-code-point (code f / b1 b2 b3)
  (cond
    ((< code 128)
     (write-char code f))
    ((< code 2048)
     (setq b1 (+ 192 (/ code 64)))
     (setq b2 (+ 128 (rem code 64)))
     (write-char b1 f)
     (write-char b2 f))
    (t
     (setq b1 (+ 224 (/ code 4096)))
     (setq b2 (+ 128 (rem (/ code 64) 64)))
     (setq b3 (+ 128 (rem code 64)))
     (write-char b1 f)
     (write-char b2 f)
     (write-char b3 f))
  )
)

;; كتابة نص ديناميكي وفك أي شفرة يونيكود من أوتوكاد مثل \U+0131 أو \U+062A وتحويلها لكيانات HTML آمنة
(defun qto:write-utf8-escaped (str f / len i hex code c-ascii)
  (setq len (strlen str))
  (setq i 1)
  (while (<= i len)
    (if (and (<= (+ i 6) len)
             (= (strcase (substr str i 3)) "\\U+"))
      (progn
        (setq hex (substr str (+ i 3) 4))
        (setq code (qto:hex4->int hex))
        (if (> code 127)
          (princ (strcat "&#" (itoa code) ";") f)
          (write-char code f)
        )
        (setq i (+ i 7))
      )
      (progn
        (setq c-ascii (ascii (substr str i 1)))
        (cond
          ((= c-ascii 38) (princ "&amp;" f))
          ((= c-ascii 60) (princ "&lt;" f))
          ((= c-ascii 62) (princ "&gt;" f))
          ((= c-ascii 34) (princ "&quot;" f))
          ((> c-ascii 127) (princ (strcat "&#" (itoa c-ascii) ";") f))
          (t (write-char c-ascii f))
        )
        (setq i (1+ i))
      )
    )
  )
)

(defun qto:write-line-crlf (str f)
  (qto:write-utf8-escaped str f)
  (write-char 13 f)
  (write-char 10 f)
)

;;; نصوص باللغة العربية كبايتات UTF-8 نقية وثابتة
(defun qto:txt-hdr-title (f) (qto:write-bytes '(216 170 217 130 216 177 217 138 216 177 32 216 173 216 181 216 177 32 216 167 217 132 217 131 217 133 217 138 216 167 216 170 32 45 32 65 117 116 111 67 65 68 32 81 117 97 110 116 105 116 121 32 84 97 107 101 111 102 102) f))
(defun qto:txt-col-cat (f)   (qto:write-bytes '(216 167 217 132 217 134 217 136 216 185 32 40 67 97 116 101 103 111 114 121 41) f))
(defun qto:txt-col-desc (f)  (qto:write-bytes '(216 167 216 179 217 133 32 216 167 217 132 216 183 216 168 217 130 216 169 32 47 32 216 167 217 132 216 168 217 132 217 136 217 131 32 40 68 101 115 99 114 105 112 116 105 111 110 41) f))
(defun qto:txt-col-qty (f)   (qto:write-bytes '(216 167 217 132 217 131 217 133 217 138 216 169 32 40 81 117 97 110 116 105 116 121 41) f))
(defun qto:txt-col-unit (f)  (qto:write-bytes '(216 167 217 132 217 136 216 173 216 175 216 169 32 40 85 110 105 116 41) f))
(defun qto:txt-sec-len (f)   (qto:write-bytes '(45 45 45 32 91 49 93 32 216 173 216 181 216 177 32 216 167 217 132 216 163 216 183 217 136 216 167 217 132 32 40 76 105 110 101 97 114 32 84 97 107 101 111 102 102 41 32 45 45 45) f))
(defun qto:txt-sec-area (f)  (qto:write-bytes '(45 45 45 32 91 50 93 32 216 173 216 181 216 177 32 216 167 217 132 217 133 216 179 216 167 216 173 216 167 216 170 32 40 65 114 101 97 32 84 97 107 101 111 102 102 41 32 45 45 45) f))
(defun qto:txt-sec-blk (f)   (qto:write-bytes '(45 45 45 32 91 51 93 32 216 173 216 181 216 177 32 216 167 217 132 216 168 217 132 217 136 217 131 216 167 216 170 32 217 136 216 167 217 132 216 185 217 134 216 167 216 181 216 177 32 40 66 108 111 99 107 32 67 111 117 110 116 115 41 32 45 45 45) f))
(defun qto:txt-cat-len (f)   (qto:write-bytes '(216 163 216 183 217 136 216 167 217 132 32 40 76 101 110 103 116 104 41) f))
(defun qto:txt-cat-area (f)  (qto:write-bytes '(217 133 216 179 216 167 216 173 216 167 216 170 32 40 65 114 101 97 41) f))
(defun qto:txt-cat-blk (f)   (qto:write-bytes '(216 168 217 132 217 136 217 131 216 167 216 170 32 40 66 108 111 99 107 41) f))
(defun qto:txt-unit-m (f)    (qto:write-bytes '(217 133 216 170 216 177 32 216 183 217 136 217 132 217 138 32 40 109 41) f))
(defun qto:txt-unit-m2 (f)   (qto:write-bytes '(217 133 216 170 216 177 32 217 133 216 177 216 168 216 185 32 40 109 194 178 41) f))
(defun qto:txt-unit-pcs (f)  (qto:write-bytes '(216 185 216 175 216 175 32 40 112 99 115 41) f))

;;; --------------------------------------------------------------------------
;;; دالة جلب محتوى رابط عبر الإنترنت (HTTP GET) لدعم التحديث التلقائي
;;; --------------------------------------------------------------------------
(defun qto:http-get (url / http res status)
  (setq http (vlax-create-object "MSXML2.ServerXMLHTTP.6.0"))
  (if (null http)
    (setq http (vlax-create-object "MSXML2.XMLHTTP"))
  )
  (if (null http)
    (setq http (vlax-create-object "WinHttp.WinHttpRequest.5.1"))
  )
  (if http
    (progn
      (vl-catch-all-apply
        '(lambda ()
           (vlax-invoke-method http 'open "GET" url :vlax-false)
           (vlax-invoke-method http 'setRequestHeader "Cache-Control" "no-cache")
           (vlax-invoke-method http 'setRequestHeader "Pragma" "no-cache")
           (vlax-invoke-method http 'send)
         )
      )
      (setq status (vl-catch-all-apply 'vlax-get-property (list http 'status)))
      (if (and (qto:numberp status) (= status 200))
        (setq res (vl-catch-all-apply 'vlax-get-property (list http 'responseText)))
      )
      (vlax-release-object http)
    )
  )
  (if (and res (not (vl-catch-all-error-p res)) (= (type res) 'STR))
    res
    nil
  )
)

;;; --------------------------------------------------------------------------
;;; أمر التحديث التلقائي: QTO-UPDATE
;;; --------------------------------------------------------------------------
(defun c:QTO-UPDATE ( / local-path remote-ver new-code f ans)
  (princ "\nجاري التحقق من وجود تحديثات جديدة عبر GitHub ...")
  (setq remote-ver (qto:http-get "https://raw.githubusercontent.com/anl7j/QTO_AutoCAD_Tool/main/version.txt"))
  (if remote-ver
    (setq remote-ver (vl-string-trim " \t\r\n" remote-ver))
  )
  (if (and remote-ver (> (strlen remote-ver) 0))
    (if (/= remote-ver *qto-version*)
      (progn
        (princ (strcat "\nتم العثور على إصدار جديد: v" remote-ver " (الإصدار الحالي: v" *qto-version* ")"))
        (initget "Yes No نعم لا")
        (setq ans (getkword (strcat "\nهل ترغب في تثبيت التحديث v" remote-ver " الآن؟ [نعم(Yes)/لا(No)] <نعم>: ")))
        (if (or (= ans "Yes") (= ans "نعم") (null ans))
          (progn
            (setq local-path (findfile "QTO.lsp"))
            (if (null local-path)
              (setq local-path (getfiled "حدد مسار حفظ ملف QTO.lsp" "QTO.lsp" "lsp" 1))
            )
            (if local-path
              (progn
                (princ "\nجاري تحميل أحدث كود من GitHub ...")
                (setq new-code (qto:http-get "https://raw.githubusercontent.com/anl7j/QTO_AutoCAD_Tool/main/QTO.lsp"))
                (if (and new-code (> (strlen new-code) 100))
                  (progn
                    (setq f (open local-path "w"))
                    (if f
                      (progn
                        (write-line new-code f)
                        (close f)
                        (load local-path)
                        (princ (strcat "\n[✓] تم بنجاح تحديث الإضافة إلى الإصدار v" remote-ver " وتم إعادة تحميلها!"))
                        (alert (strcat "تم بنجاح تحديث إضافة حصر الكميات إلى الإصدار v" remote-ver "!\nتم تطبيق التحديث فوراً في أوتوكاد."))
                      )
                      (alert "تعذر الكتابة على الملف. يرجى التأكد من صلاحيات المجلد.")
                    )
                  )
                  (alert "تعذر تنزيل كود التحديث من GitHub. يرجى التحقق من اتصال الإنترنت.")
                )
              )
            )
          )
        )
      )
      (progn
        (princ (strcat "\nأنت تستخدم بالفعل أحدث إصدار (v" *qto-version* ")."))
        (alert (strcat "أنت تستخدم بالفعل أحدث إصدار من الإضافة (v" *qto-version* ").\nلا توجد تحديثات جديدة حالياً."))
      )
    )
    (progn
      (princ "\nتعذر الاتصال بـ GitHub للتحقق من التحديثات.")
      (alert "تعذر الاتصال بموقع GitHub للتحقق من التحديثات.\nيرجى التأكد من اتصال الإنترنت.")
    )
  )
  (princ)
)

(defun c:HASR-UPDATE () (c:QTO-UPDATE))

;;; --------------------------------------------------------------------------
;;; دالة جلب الاسم الحقيقي للبلوك (حتى لو كان بلوك ديناميكي Dynamic Block)
;;; --------------------------------------------------------------------------
(defun qto:get-block-name (vla-obj / blk-name)
  (cond
    ((and (vlax-property-available-p vla-obj 'EffectiveName)
          (setq blk-name (vl-catch-all-apply 'vla-get-EffectiveName (list vla-obj)))
          (not (vl-catch-all-error-p blk-name))
          blk-name)
     blk-name)
    ((and (vlax-property-available-p vla-obj 'Name)
          (setq blk-name (vl-catch-all-apply 'vla-get-Name (list vla-obj)))
          (not (vl-catch-all-error-p blk-name))
          blk-name)
     blk-name)
    (t "Unknown_Block")
  )
)

;;; --------------------------------------------------------------------------
;;; دالة حساب طول العنصر (خطوط، بولي لاين، أقواس، سبلين)
;;; --------------------------------------------------------------------------
(defun qto:get-curve-length (vla-obj / res)
  (setq res (vl-catch-all-apply
              '(lambda ()
                 (vlax-curve-getDistAtParam
                   vla-obj
                   (vlax-curve-getEndParam vla-obj)
                 )
               )
            ))
  (if (and (not (vl-catch-all-error-p res)) (qto:numberp res))
    res
    0.0
  )
)

;;; --------------------------------------------------------------------------
;;; دالة التحقق مما إذا كان المنحنى مغلقاً وله مساحة
;;; --------------------------------------------------------------------------
(defun qto:is-closed-curve (vla-obj / cl)
  (and
    (vlax-property-available-p vla-obj 'Closed)
    (not (vl-catch-all-error-p (setq cl (vl-catch-all-apply 'vlax-get-property (list vla-obj 'Closed)))))
    (= cl :vlax-true)
    (vlax-property-available-p vla-obj 'Area)
  )
)

;;; --------------------------------------------------------------------------
;;; دالة إضافة قيمة إلى قائمة التجميع التراكمي (Key-Value Accumulator)
;;; متوافقة تماماً مع AutoLISP القياسي
;;; --------------------------------------------------------------------------
(defun qto:accumulate (key val alist / found new-val)
  (setq found (assoc key alist))
  (if found
    (progn
      (setq new-val (+ (cdr found) val))
      (subst (cons key new-val) found alist)
    )
    (cons (cons key val) alist)
  )
)

;;; --------------------------------------------------------------------------
;;; المحرك الرئيسي لحصر العناصر من مجموعة الاختيار (Selection Set)
;;; --------------------------------------------------------------------------
(defun qto:process-selection (ss / i total-count ent vla-obj obj-name layer-name
                                  blk-name h-area c-area len-val p-area c-len)
  (setq *qto-lengths* nil)
  (setq *qto-areas* nil)
  (setq *qto-blocks* nil)

  (setq i 0)
  (setq total-count (sslength ss))

  (while (< i total-count)
    (if (and (> total-count 1000) (= (rem i 1000) 0) (> i 0))
      (princ (strcat "\n... تم فحص " (itoa i) " من " (itoa total-count) " عنصراً"))
    )

    (setq ent (ssname ss i))
    (setq vla-obj (vl-catch-all-apply 'vlax-ename->vla-object (list ent)))

    (if (and vla-obj (not (vl-catch-all-error-p vla-obj)))
      (progn
        (setq obj-name (vla-get-ObjectName vla-obj))
        (setq layer-name (vla-get-Layer vla-obj))
        (if (or (null layer-name) (= layer-name "")) (setq layer-name "0"))

        (cond
          ;; 1. حصر البلوكات (Block References)
          ((or (= obj-name "AcDbBlockReference") (= obj-name "AcDbMInsertBlock"))
           (setq blk-name (qto:get-block-name vla-obj))
           (if (not (wcmatch blk-name "`**"))
             (setq *qto-blocks* (qto:accumulate (cons blk-name layer-name) 1 *qto-blocks*))
           )
          )

          ;; 2. حصر المساحات من التهشير (Hatches)
          ((= obj-name "AcDbHatch")
           (if (vlax-property-available-p vla-obj 'Area)
             (setq h-area (vl-catch-all-apply 'vla-get-Area (list vla-obj)))
             (setq h-area 0.0)
           )
           (if (and (qto:numberp h-area) (> h-area 0.0))
             (setq *qto-areas* (qto:accumulate layer-name h-area *qto-areas*))
           )
          )

          ;; 3. حصر المساحات من الأشكال المغلقة (Regions / Circles)
          ((or (= obj-name "AcDbRegion") (= obj-name "AcDbCircle"))
           (if (vlax-property-available-p vla-obj 'Area)
             (progn
               (setq c-area (vl-catch-all-apply 'vla-get-Area (list vla-obj)))
               (if (and (qto:numberp c-area) (> c-area 0.0))
                 (setq *qto-areas* (qto:accumulate layer-name c-area *qto-areas*))
               )
             )
           )
          )

          ;; 4. المنحنيات والخطوط (Lines, Polylines, Arcs, Splines)
          ((vlax-property-available-p vla-obj 'Length)
           (setq len-val (vl-catch-all-apply 'vla-get-Length (list vla-obj)))
           (if (and (qto:numberp len-val) (> len-val 0.0))
             (setq *qto-lengths* (qto:accumulate layer-name len-val *qto-lengths*))
           )
           (if (qto:is-closed-curve vla-obj)
             (progn
               (setq p-area (vl-catch-all-apply 'vla-get-Area (list vla-obj)))
               (if (and (qto:numberp p-area) (> p-area 0.0))
                 (setq *qto-areas* (qto:accumulate layer-name p-area *qto-areas*))
               )
             )
           )
          )

          ;; فحص بديل للأطوال عبر vlax-curve
          ((and (vlax-method-applicable-p vla-obj 'IntersectWith)
                (wcmatch obj-name "*Line*,*Arc*,*Spline*"))
           (setq c-len (qto:get-curve-length vla-obj))
           (if (and (qto:numberp c-len) (> c-len 0.0))
             (setq *qto-lengths* (qto:accumulate layer-name c-len *qto-lengths*))
           )
           (if (qto:is-closed-curve vla-obj)
             (progn
               (setq p-area (vl-catch-all-apply 'vla-get-Area (list vla-obj)))
               (if (and (qto:numberp p-area) (> p-area 0.0))
                 (setq *qto-areas* (qto:accumulate layer-name p-area *qto-areas*))
               )
             )
           )
          )
        )
      )
    )

    (setq i (1+ i))
  )
)

;;; --------------------------------------------------------------------------
;;; دالة طباعة النتائج في سطر الأوامر (Command Line Display)
;;; --------------------------------------------------------------------------
(defun qto:print-summary ()
  (princ "\n==================================================================")
  (princ (strcat "\n        ملخص حصر الكميات للمخطط (الإصدار v" *qto-version* ")        "))
  (princ "\n==================================================================")

  ;; 1. الأطوال
  (princ "\n\n--- [1] حصر الأطوال (Linear Quantities):")
  (if *qto-lengths*
    (progn
      (princ (strcat "\n" (qto:pad "اسم الطبقة (Layer)" 30) " | " "المجموع (م.ط / وحدة)"))
      (princ "\n-------------------------------------------------------------")
      (foreach item (vl-sort *qto-lengths* '(lambda (a b) (< (car a) (car b))))
        (princ (strcat "\n" (qto:pad (car item) 30) " | " (rtos (cdr item) 2 3)))
      )
    )
    (princ "\n  لا توجد خطوط أو مسارات أطوال في التحديد.")
  )

  ;; 2. المساحات
  (princ "\n\n--- [2] حصر المساحات (Area Quantities):")
  (if *qto-areas*
    (progn
      (princ (strcat "\n" (qto:pad "اسم الطبقة (Layer)" 30) " | " "إجمالي المساحة (م² / وحدة)"))
      (princ "\n-------------------------------------------------------------")
      (foreach item (vl-sort *qto-areas* '(lambda (a b) (< (car a) (car b))))
        (princ (strcat "\n" (qto:pad (car item) 30) " | " (rtos (cdr item) 2 3)))
      )
    )
    (princ "\n  لا توجد أشكال مغلقة أو تهشيرات (Hatches) في التحديد.")
  )

  ;; 3. البلوكات
  (princ "\n\n--- [3] حصر الكتل والبلوكات (Block Counts):")
  (if *qto-blocks*
    (progn
      (princ (strcat "\n" (qto:pad "اسم البلوك (Block Name)" 25) " | " (qto:pad "الطبقة" 20) " | العدد"))
      (princ "\n-------------------------------------------------------------")
      (foreach item (vl-sort *qto-blocks* '(lambda (a b) (< (caar a) (caar b))))
        (princ (strcat "\n" (qto:pad (caar item) 25) " | " (qto:pad (cdar item) 20) " | " (itoa (fix (cdr item)))))
      )
    )
    (princ "\n  لا توجد بلوكات في التحديد.")
  )

  (princ "\n\n==================================================================\n")
  (princ)
)

;;; مساعدة لتنسيق النصوص في الطباعة
(defun qto:pad (str width)
  (setq str (vl-princ-to-string str))
  (while (< (strlen str) width)
    (setq str (strcat str " "))
  )
  str
)

;;; --------------------------------------------------------------------------
;;; دالة التصدير المباشر لملف Excel احترافي متوافق 100% مع المقايسات ودفاتر الحصر
;;; يحتوي على: 1. المقايسة التقديرية والتكاليف  2. دفتر حصر الأطوال والمساحات  3. حصر الأبواب والشبابيك
;;; تم ترميز النصوص العربية بكيانات HTML القياسية لضمان عدم حدوث أي تشوه أو أخطاء تحميل
;;; --------------------------------------------------------------------------
(defun qto:export-to-excel ( / dwg-path dwg-name default-xls default-xlsx filepath f date-str xl wb
                               c-idx m-idx b-idx tot-cost tot-blocks item)
  (setq dwg-path (getvar "DWGPREFIX"))
  (setq dwg-name (vl-filename-base (getvar "DWGNAME")))
  (if (or (null dwg-path) (= dwg-path "")) (setq dwg-path "C:\\"))
  (if (or (null dwg-name) (= dwg-name "")) (setq dwg-name "Drawing"))
  (setq default-xls (strcat dwg-path dwg-name "_حصر_الكميات_والمقايسة.xls"))

  (setq filepath (getfiled "حفظ تقرير المقايسة وحصر الكميات بصيغة Excel" default-xls "xls" 1))
  (if filepath
    (progn
      (setq f (open filepath "w"))
      (if f
        (progn
          (qto:write-utf8-bom f)
          (setq date-str (vl-catch-all-apply 'menucmd (list "M=$(edtime,$(getvar,date),YYYY-MO-DD HH:MM)")))
          (if (vl-catch-all-error-p date-str) (setq date-str "N/A"))

          ;; 1. ترويسة HTML Excel مع دعم RTL والتنسيق الأخضر الرسمي
          (qto:write-line-crlf "<html xmlns:o=\"urn:schemas-microsoft-com:office:office\" xmlns:x=\"urn:schemas-microsoft-com:office:excel\" xmlns=\"http://www.w3.org/TR/REC-html40\">" f)
          (qto:write-line-crlf "<head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">" f)
          (qto:write-line-crlf "<!--[if gte mso 9]><xml><x:ExcelWorkbook><x:ExcelWorksheets><x:ExcelWorksheet>" f)
          (qto:write-line-crlf "<x:Name>QTO_BOQ</x:Name><x:WorksheetOptions><x:DisplayRightToLeft/><x:Selected/></x:WorksheetOptions>" f)
          (qto:write-line-crlf "</x:ExcelWorksheet></x:ExcelWorksheets></x:ExcelWorkbook></xml><![endif]-->" f)
          (qto:write-line-crlf "<style>" f)
          (qto:write-line-crlf "body { font-family: Segoe UI, Tahoma, Arial, sans-serif; direction: rtl; }" f)
          (qto:write-line-crlf "table { border-collapse: collapse; width: 100%; margin-bottom: 25px; }" f)
          (qto:write-line-crlf "th { background-color: #107C41; color: #ffffff; font-weight: bold; font-size: 11pt; padding: 10px; border: 1px solid #0D6535; text-align: center; }" f)
          (qto:write-line-crlf "td { padding: 8px 12px; border: 1px solid #d4d4d4; font-size: 10pt; text-align: center; }" f)
          (qto:write-line-crlf ".right { text-align: right; }" f)
          (qto:write-line-crlf ".total { background-color: #E8F5E9; color: #107C41; font-weight: bold; font-size: 11pt; border-top: 1px solid #107C41; border-bottom: 3px double #107C41; }" f)
          (qto:write-line-crlf ".sec { background-color: #E8F5E9; color: #1B5E20; font-weight: bold; font-size: 12pt; text-align: right; padding: 8px 12px; border: 1px solid #C8E6C9; margin-top: 20px; margin-bottom: 8px; }" f)
          (qto:write-line-crlf ".title { font-size: 16pt; font-weight: bold; color: #107C41; padding: 5px 0; }" f)
          (qto:write-line-crlf ".info { font-size: 10pt; color: #555555; margin-bottom: 15px; }" f)
          (qto:write-line-crlf "</style></head><body>" f)

          (qto:write-line-crlf "<div class=\"title\">&#1580;&#1583;&#1608;&#1604; &#1575;&#1604;&#1605;&#1602;&#1575;&#1610;&#1587;&#1577; &#1608;&#1581;&#1589;&#1585; &#1575;&#1604;&#1603;&#1605;&#1610;&#1575;&#1578; - QUANTITY TAKEOFF &amp; BOQ</div>" f)
          (qto:write-line-crlf (strcat "<div class=\"info\"><strong>Drawing:</strong> " (getvar "DWGNAME") " | <strong>Date:</strong> " date-str " | <strong>Version:</strong> v" *qto-version* "</div>") f)

          ;; -----------------------------------------------------------------
          ;; جدول 1: المقايسة التقديرية والتكاليف (YAKLAŞIK MALİYET)
          ;; -----------------------------------------------------------------
          (qto:write-line-crlf "<div class=\"sec\">--- [1] &#1575;&#1604;&#1605;&#1602;&#1575;&#1610;&#1587;&#1577; &#1575;&#1604;&#1578;&#1602;&#1583;&#1610;&#1585;&#1610;&#1577; &#1608;&#1575;&#1604;&#1578;&#1603;&#1575;&#1604;&#1610;&#1601; (YAKLA&#350;IK MAL&#304;YET) ---</div>" f)
          (qto:write-line-crlf "<table><thead><tr>" f)
          (qto:write-line-crlf "<th style=\"width:5%;\">&#1605; (S&#305;ra No)</th>" f)
          (qto:write-line-crlf "<th style=\"width:15%;\">&#1603;&#1608;&#1583; &#1575;&#1604;&#1576;&#1606;&#1583; (&#304;&#351; Kalemi No)</th>" f)
          (qto:write-line-crlf "<th style=\"width:40%;\">&#1576;&#1610;&#1575;&#1606; &#1608;&#1578;&#1608;&#1589;&#1610;&#1601; &#1575;&#1604;&#1571;&#1593;&#1605;&#1575;&#1604; (&#304;&#351; Kaleminin Ad&#305; ve A&#231;&#305;klamas&#305;)</th>" f)
          (qto:write-line-crlf "<th style=\"width:10%;\">&#1575;&#1604;&#1608;&#1581;&#1583;&#1577; (&#214;l&#231;&#252; Birimi)</th>" f)
          (qto:write-line-crlf "<th style=\"width:10%;\">&#1575;&#1604;&#1603;&#1605;&#1610;&#1577; (Miktar)</th>" f)
          (qto:write-line-crlf "<th style=\"width:10%;\">&#1587;&#1593;&#1585; &#1575;&#1604;&#1608;&#1581;&#1583;&#1577; (Birim Fiyat TL)</th>" f)
          (qto:write-line-crlf "<th style=\"width:10%;\">&#1575;&#1604;&#1573;&#1580;&#1605;&#1575;&#1604;&#1610; (Tutar TL)</th>" f)
          (qto:write-line-crlf "</tr></thead><tbody>" f)

          (setq c-idx 1)
          ;; صفوف الأطوال
          (if *qto-lengths*
            (foreach item (vl-sort *qto-lengths* '(lambda (a b) (< (car a) (car b))))
              (qto:write-line-crlf (strcat "<tr><td>" (itoa c-idx) "</td>"
                                           "<td>15.120." (itoa (+ 1000 c-idx)) "</td>"
                                           "<td class=\"right\">&#1571;&#1593;&#1605;&#1575;&#1604; &#1605;&#1587;&#1575;&#1585;&#1575;&#1578; &#1608;&#1571;&#1591;&#1608;&#1575;&#1604; - " (car item) "</td>"
                                           "<td>&#1605;.&#1591;</td>"
                                           "<td>" (rtos (cdr item) 2 3) "</td>"
                                           "<td>0</td>"
                                           "<td x:num=\"0\" x:fmla=\"=E" (itoa (1+ c-idx)) "*F" (itoa (1+ c-idx)) "\">0</td></tr>") f)
              (setq c-idx (1+ c-idx))
            )
          )

          ;; صفوف المساحات
          (if *qto-areas*
            (foreach item (vl-sort *qto-areas* '(lambda (a b) (< (car a) (car b))))
              (qto:write-line-crlf (strcat "<tr><td>" (itoa c-idx) "</td>"
                                           "<td>15.220." (itoa (+ 1000 c-idx)) "</td>"
                                           "<td class=\"right\">&#1571;&#1593;&#1605;&#1575;&#1604; &#1605;&#1587;&#1575;&#1581;&#1575;&#1578; &#1608;&#1578;&#1588;&#1591;&#1610;&#1576;&#1575;&#1578; - " (car item) "</td>"
                                           "<td>&#1605;&#178;</td>"
                                           "<td>" (rtos (cdr item) 2 3) "</td>"
                                           "<td>0</td>"
                                           "<td x:num=\"0\" x:fmla=\"=E" (itoa (1+ c-idx)) "*F" (itoa (1+ c-idx)) "\">0</td></tr>") f)
              (setq c-idx (1+ c-idx))
            )
          )

          ;; صفوف البلوكات
          (if *qto-blocks*
            (foreach item (vl-sort *qto-blocks* '(lambda (a b) (< (caar a) (caar b))))
              (qto:write-line-crlf (strcat "<tr><td>" (itoa c-idx) "</td>"
                                           "<td>15.350." (itoa (+ 1000 c-idx)) "</td>"
                                           "<td class=\"right\">&#1578;&#1608;&#1585;&#1610;&#1583; &#1608;&#1578;&#1585;&#1603;&#1610;&#1576; " (caar item) " [" (cdar item) "]</td>"
                                           "<td>&#1593;&#1583;&#1583;</td>"
                                           "<td>" (itoa (fix (cdr item))) "</td>"
                                           "<td>0</td>"
                                           "<td x:num=\"0\" x:fmla=\"=E" (itoa (1+ c-idx)) "*F" (itoa (1+ c-idx)) "\">0</td></tr>") f)
              (setq c-idx (1+ c-idx))
            )
          )

          ;; صف الإجمالي العام للمقايسة
          (setq tot-cost (1- c-idx))
          (if (> tot-cost 0)
            (qto:write-line-crlf (strcat "<tr><td colspan=\"6\" class=\"total\">&#1575;&#1604;&#1605;&#1580;&#1605;&#1608;&#1593; &#1575;&#1604;&#1603;&#1604;&#1610; &#1575;&#1604;&#1578;&#1602;&#1583;&#1610;&#1585;&#1610; &#1604;&#1604;&#1605;&#1588;&#1585;&#1608;&#1593; (TOPLAM TUTAR):</td>"
                                         "<td class=\"total\" x:num=\"0\" x:fmla=\"=SUM(G2:G" (itoa (1+ tot-cost)) ")\">0</td></tr>") f)
          )
          (qto:write-line-crlf "</tbody></table>" f)

          ;; -----------------------------------------------------------------
          ;; جدول 2: دفتر حصر الأطوال والمساحات (METRAJ CETVELİ)
          ;; -----------------------------------------------------------------
          (qto:write-line-crlf "<div class=\"sec\">--- [2] &#1583;&#1601;&#1578;&#1585; &#1581;&#1589;&#1585; &#1575;&#1604;&#1571;&#1591;&#1608;&#1575;&#1604; &#1608;&#1575;&#1604;&#1605;&#1587;&#1575;&#1581;&#1575;&#1578; (METRAJ CETVEL&#304;) ---</div>" f)
          (qto:write-line-crlf "<table><thead><tr>" f)
          (qto:write-line-crlf "<th style=\"width:5%;\">NO (&#1605;)</th>" f)
          (qto:write-line-crlf "<th style=\"width:35%;\">&#1576;&#1610;&#1575;&#1606; &#1575;&#1604;&#1593;&#1606;&#1589;&#1585; / &#1575;&#1604;&#1591;&#1576;&#1602;&#1577; (&#304;&#351;in A&#231;&#305;klanmas&#305; / Layer)</th>" f)
          (qto:write-line-crlf "<th style=\"width:15%;\">&#1606;&#1608;&#1593; &#1575;&#1604;&#1581;&#1589;&#1585; (Kategori)</th>" f)
          (qto:write-line-crlf "<th style=\"width:8%;\">&#1575;&#1604;&#1605;&#1578;&#1588;&#1575;&#1576;&#1607; (Benzer)</th>" f)
          (qto:write-line-crlf "<th style=\"width:12%;\">&#1575;&#1604;&#1605;&#1602;&#1575;&#1587; (Boy / Alan)</th>" f)
          (qto:write-line-crlf "<th style=\"width:8%;\">&#1575;&#1604;&#1582;&#1589;&#1605;&#1610;&#1575;&#1578; (M&#304;NHA)</th>" f)
          (qto:write-line-crlf "<th style=\"width:12%;\">&#1575;&#1604;&#1573;&#1580;&#1605;&#1575;&#1604;&#1610; &#1575;&#1604;&#1589;&#1575;&#1601;&#1610; (TOPLAM)</th>" f)
          (qto:write-line-crlf "<th style=\"width:5%;\">&#1575;&#1604;&#1608;&#1581;&#1583;&#1577; (Birim)</th>" f)
          (qto:write-line-crlf "</tr></thead><tbody>" f)

          (setq m-idx 1)
          (if *qto-lengths*
            (foreach item (vl-sort *qto-lengths* '(lambda (a b) (< (car a) (car b))))
              (qto:write-line-crlf (strcat "<tr><td>" (itoa m-idx) "</td>"
                                           "<td class=\"right\">" (car item) "</td>"
                                           "<td>&#1571;&#1593;&#1605;&#1575;&#1604; &#1605;&#1587;&#1575;&#1585;&#1575;&#1578; (Linear)</td>"
                                           "<td>1</td>"
                                           "<td>" (rtos (cdr item) 2 3) "</td>"
                                           "<td>0</td>"
                                           "<td>" (rtos (cdr item) 2 3) "</td>"
                                           "<td>&#1605;.&#1591;</td></tr>") f)
              (setq m-idx (1+ m-idx))
            )
          )

          (if *qto-areas*
            (foreach item (vl-sort *qto-areas* '(lambda (a b) (< (car a) (car b))))
              (qto:write-line-crlf (strcat "<tr><td>" (itoa m-idx) "</td>"
                                           "<td class=\"right\">" (car item) "</td>"
                                           "<td>&#1571;&#1593;&#1605;&#1575;&#1604; &#1605;&#1587;&#1575;&#1581;&#1575;&#1578; (Area)</td>"
                                           "<td>1</td>"
                                           "<td>" (rtos (cdr item) 2 3) "</td>"
                                           "<td>0</td>"
                                           "<td>" (rtos (cdr item) 2 3) "</td>"
                                           "<td>&#1605;&#178;</td></tr>") f)
              (setq m-idx (1+ m-idx))
            )
          )
          (qto:write-line-crlf "</tbody></table>" f)

          ;; -----------------------------------------------------------------
          ;; جدول 3: حصر الأبواب والشبابيك والبلوكات (KAPI, PENCERE VE BLOKLAR)
          ;; -----------------------------------------------------------------
          (qto:write-line-crlf "<div class=\"sec\">--- [3] &#1581;&#1589;&#1585; &#1575;&#1604;&#1571;&#1576;&#1608;&#1575;&#1576; &#1608;&#1575;&#1604;&#1588;&#1576;&#1575;&#1576;&#1610;&#1603; &#1608;&#1575;&#1604;&#1576;&#1604;&#1608;&#1603;&#1575;&#1578; (KAPI, PENCERE VE BLOKLAR) ---</div>" f)
          (qto:write-line-crlf "<table><thead><tr>" f)
          (qto:write-line-crlf "<th style=\"width:5%;\">NO (&#1605;)</th>" f)
          (qto:write-line-crlf "<th style=\"width:35%;\">&#1575;&#1587;&#1605; &#1575;&#1604;&#1576;&#1604;&#1608;&#1603; / &#1575;&#1604;&#1585;&#1605;&#1586; (&#304;&#351;in A&#231;&#305;klanmas&#305; / Blok Ad&#305;)</th>" f)
          (qto:write-line-crlf "<th style=\"width:25%;\">&#1575;&#1604;&#1591;&#1576;&#1602;&#1577; (Layer)</th>" f)
          (qto:write-line-crlf "<th style=\"width:10%;\">&#1575;&#1604;&#1605;&#1578;&#1588;&#1575;&#1576;&#1607; (Benzer)</th>" f)
          (qto:write-line-crlf "<th style=\"width:10%;\">&#1575;&#1604;&#1593;&#1583;&#1583; &#1575;&#1604;&#1605;&#1581;&#1589;&#1608;&#1585; (Adet)</th>" f)
          (qto:write-line-crlf "<th style=\"width:10%;\">&#1575;&#1604;&#1573;&#1580;&#1605;&#1575;&#1604;&#1610; (TOPLAM)</th>" f)
          (qto:write-line-crlf "<th style=\"width:5%;\">&#1575;&#1604;&#1608;&#1581;&#1583;&#1577; (Birim)</th>" f)
          (qto:write-line-crlf "</tr></thead><tbody>" f)

          (setq b-idx 1)
          (if *qto-blocks*
            (foreach item (vl-sort *qto-blocks* '(lambda (a b) (< (caar a) (caar b))))
              (qto:write-line-crlf (strcat "<tr><td>" (itoa b-idx) "</td>"
                                           "<td class=\"right\">" (caar item) "</td>"
                                           "<td>" (cdar item) "</td>"
                                           "<td>1</td>"
                                           "<td>" (itoa (fix (cdr item))) "</td>"
                                           "<td>" (itoa (fix (cdr item))) "</td>"
                                           "<td>&#1593;&#1583;&#1583; (adet)</td></tr>") f)
              (setq b-idx (1+ b-idx))
            )
          )
          (qto:write-line-crlf "</tbody></table>" f)

          (qto:write-line-crlf "</body></html>" f)
          (close f)

          (princ (strcat "\n[✓] تم إنشاء وتصدير ملف Excel بنجاح: " filepath))

          ;; تشغيل Excel وفتح الملف أمامه وحفظه بصيغة .xlsx الحقيقية
          (setq xl (vl-catch-all-apply 'vlax-get-or-create-object (list "Excel.Application")))
          (if (and xl (not (vl-catch-all-error-p xl)))
            (progn
              (vla-put-Visible xl :vlax-true)
              (setq wb (vl-catch-all-apply 'vlax-invoke-method (list (vlax-get-property xl 'Workbooks) 'Open filepath)))
              (if (and wb (not (vl-catch-all-error-p wb)))
                (progn
                  (setq default-xlsx (strcat (vl-filename-directory filepath) "\\" (vl-filename-base filepath) ".xlsx"))
                  (vl-catch-all-apply 'vlax-invoke-method (list wb 'SaveAs default-xlsx 51))
                )
              )
            )
            (startapp "explorer.exe" filepath)
          )

          (alert (strcat "تم بنجاح تصدير تقرير المقايسة التقديرية وحصر الكميات إلى Excel!\n\n"
                         "الملف يحتوي على 3 جداول هندسية منظمة ومطابقة للمقايسة:\n"
                         "1. المقايسة التقديرية والتكاليف (BOQ & Cost Estimate)\n"
                         "2. دفتر حصر الأطوال والمساحات (Detailed Takeoff)\n"
                         "3. حصر الأبواب والشبابيك والبلوكات (Block Counts)\n\n"
                         "المسار:\n" filepath))
        )
        (alert "تعذر فتح الملف للكتابة!")
      )
    )
  )
  (princ)
)

;;; --------------------------------------------------------------------------
;;; دالة إدراج جدول كميات (AutoCAD Table) مباشرة داخل لوحة الرسم
;;; --------------------------------------------------------------------------
(defun qto:insert-autocad-table ( / opt pt doc mspace num-rows num-cols pt-arr tbl cur-row)
  (initget "Yes No نعم لا")
  (setq opt (getkword "\nهل ترغب في إدراج جدول كميات داخل الرسم؟ [نعم(Yes)/لا(No)] <نعم>: "))
  (if (or (= opt "Yes") (= opt "نعم") (null opt))
    (progn
      (setq pt (getpoint "\nانقر نقطة في الرسم لإدراج الجدول: "))
      (if pt
        (progn
          (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
          (setq mspace (vla-get-ModelSpace doc))

          (setq num-rows (+ 2 (length *qto-lengths*) (length *qto-areas*) (length *qto-blocks*)))
          (setq num-cols 4)

          (if (> num-rows 2)
            (progn
              (setq pt-arr (vlax-make-safearray vlax-vbDouble '(0 . 2)))
              (vlax-safearray-fill pt-arr pt)

              (setq tbl (vl-catch-all-apply 'vla-AddTable (list mspace pt-arr num-rows num-cols 2.0 20.0)))
              (if (and tbl (not (vl-catch-all-error-p tbl)))
                (progn
                  (vla-put-TitleSuppressed tbl :vlax-false)
                  (vla-put-HeaderSuppressed tbl :vlax-false)

                  (vla-SetText tbl 0 0 "جدول حصر الكميات - QUANTITY TAKEOFF")

                  (vla-SetText tbl 1 0 "النوع (Category)")
                  (vla-SetText tbl 1 1 "الوصف / الطبقة (Description)")
                  (vla-SetText tbl 1 2 "الكمية (Quantity)")
                  (vla-SetText tbl 1 3 "الوحدة (Unit)")

                  (setq cur-row 2)

                  ;; تعبئة الأطوال
                  (foreach item (vl-sort *qto-lengths* '(lambda (a b) (< (car a) (car b))))
                    (vla-SetText tbl cur-row 0 "أطوال (Length)")
                    (vla-SetText tbl cur-row 1 (car item))
                    (vla-SetText tbl cur-row 2 (rtos (cdr item) 2 2))
                    (vla-SetText tbl cur-row 3 "م.ط")
                    (setq cur-row (1+ cur-row))
                  )

                  ;; تعبئة المساحات
                  (foreach item (vl-sort *qto-areas* '(lambda (a b) (< (car a) (car b))))
                    (vla-SetText tbl cur-row 0 "مساحات (Area)")
                    (vla-SetText tbl cur-row 1 (car item))
                    (vla-SetText tbl cur-row 2 (rtos (cdr item) 2 2))
                    (vla-SetText tbl cur-row 3 "م²")
                    (setq cur-row (1+ cur-row))
                  )

                  ;; تعبئة البلوكات
                  (foreach item (vl-sort *qto-blocks* '(lambda (a b) (< (caar a) (caar b))))
                    (vla-SetText tbl cur-row 0 "بلوكات (Block)")
                    (vla-SetText tbl cur-row 1 (strcat (caar item) " [" (cdar item) "]"))
                    (vla-SetText tbl cur-row 2 (itoa (fix (cdr item))))
                    (vla-SetText tbl cur-row 3 "عدد")
                    (setq cur-row (1+ cur-row))
                  )

                  (princ "\nتم إدراج جدول الكميات بنجاح داخل الرسم.")
                )
                (princ "\nتعذر إنشاء جدول أوتوكاد داخل لوحة الرسم.")
              )
            )
            (princ "\nلا توجد عناصر كافية لإنشاء جدول.")
          )
        )
      )
    )
  )
)

;;; --------------------------------------------------------------------------
;;; الأمر الأساسي: QTO (أو HASR)
;;; يدعم الآن التحديد المسبق (Pickfirst) والتحديد التفاعلي بالماوس، مع حماية ضد الحصر الشامل غير المقصود
;;; --------------------------------------------------------------------------
(defun c:QTO ( / ss exp-opt all-opt)
  (setvar "CMDECHO" 0)
  (princ "\n=======================================================")
  (princ (strcat "\n  أداة حصر الكميات لأوتوكاد (v" *qto-version* ") "))
  (princ "\n=======================================================")

  ;; 1. التحقق أولاً من وجود عناصر محددة مسبقاً بالماوس (Pickfirst / Pre-selection)
  (setq ss (ssget "_I" '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE,CIRCLE,HATCH,REGION,INSERT"))))
  (if (and ss (> (sslength ss) 0))
    (progn
      (sssetfirst nil nil) ; إلغاء التحديد المسبق لإيقاف الـ Grips
      (princ (strcat "\n[✓] تم التقاط العناصر المحددة مسبقاً (" (itoa (sslength ss)) " عنصراً)."))
    )
    ;; 2. إذا لم يكن هناك تحديد مسبق، نطلب من المستخدم التحديد بالماوس الآن
    (progn
      (princ "\nحدد العناصر أو الغرفة بالماوس لحصرها ثم اضغط [Enter]: ")
      (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE,CIRCLE,HATCH,REGION,INSERT"))))
    )
  )

  ;; 3. إذا لم يقم المستخدم بتحديد عناصر وضغط Enter مباشرة
  (if (null ss)
    (progn
      (initget "Yes No نعم لا")
      (setq all-opt (getkword "\nلم تقم بتحديد عناصر بالماوس. هل ترغب فعلاً في حصر كامل عناصر المخطط؟ [نعم(Yes)/لا(No)] <لا>: "))
      (if (or (= all-opt "Yes") (= all-opt "نعم"))
        (progn
          (princ "\nجاري حصر كامل عناصر المخطط أوتوماتيكياً ...")
          (setq ss (ssget "X" '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE,CIRCLE,HATCH,REGION,INSERT"))))
        )
        (princ "\n[i] تم إلغاء العملية، لم يتم اختيار أي عناصر.")
      )
    )
    (princ (strcat "\nتم تحديد " (itoa (sslength ss)) " عنصراً للحصر."))
  )

  (if (and ss (> (sslength ss) 0))
    (progn
      (princ (strcat "\nجاري فحص وحصر " (itoa (sslength ss)) " عنصراً ..."))
      (qto:process-selection ss)

      ;; 1. طباعة الملخص في سطر الأوامر
      (qto:print-summary)

      ;; 2. خيار التصدير إلى Excel
      (initget "Yes No نعم لا")
      (setq exp-opt (getkword "\nهل ترغب في تصدير تقرير الكميات إلى ملف Excel؟ [نعم(Yes)/لا(No)] <نعم>: "))
      (if (or (= exp-opt "Yes") (= exp-opt "نعم") (null exp-opt))
        (qto:export-to-excel)
      )

      ;; 3. خيار إدراج جدول داخل أوتوكاد
      (qto:insert-autocad-table)
    )
    (princ "\nلم يتم العثور على أي عناصر قابلة للحصر!")
  )

  (setvar "CMDECHO" 1)
  (princ "\nتم الانتهاء.")
  (princ)
)

;; اسم مرادف بالأحرف اللاتينية والعربية
(defun c:HASR () (c:QTO))
(defun c:QTAKEOFF () (c:QTO))

(princ (strcat "\n>> تم تحميل إضافة حصر الكميات بنجاح (الإصدار v" *qto-version* ")!"))
(princ "\n>> اكتب الأمر QTO أو HASR لبدء الحصر.")
(princ "\n>> اكتب الأمر QTO-INSTALL للتثبيت الدائم لتعمل تلقائياً عند فتح أوتوكاد.")
(princ "\n>> اكتب الأمر QTO-UPDATE للتحقق من التحديثات من GitHub.\n")
(princ)
