;;; ==========================================================================
;;;  AutoCAD Quantity Takeoff (QTO) Tool - أداة حصر الكميات للمخططات الهندسية
;;;  Version: 1.1
;;;  Commands:
;;;    QTO        - تشغيل حصر الكميات التفاعلي (أطوال، مساحات، بلوكات)
;;;    HASR       - اختصار لنفس الأداة
;;;    QTO-UPDATE - التحديث التلقائي للإضافة مباشرة من GitHub
;;; ==========================================================================

(vl-load-com)

;;; رقم الإصدار الحالي
(setq *qto-version* "1.1")

;;; --------------------------------------------------------------------------
;;; دالة التحقق من أن القيمة رقم (صحيح أو عشري)
;;; --------------------------------------------------------------------------
(defun qto:numberp (x)
  (and x (or (= (type x) 'REAL) (= (type x) 'INT)))
)

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
;;; دالة كتابة رأس ملف CSV مع علامة UTF-8 BOM لدعم اللغة العربية في Excel
;;; --------------------------------------------------------------------------
(defun qto:write-utf8-bom (file-handle)
  (write-char 239 file-handle)
  (write-char 187 file-handle)
  (write-char 191 file-handle)
)

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
  (setq *qto-lengths* nil)   ; قائمة الأطوال حسب الطبقة: '((layer . total_length) ...)
  (setq *qto-areas* nil)     ; قائمة المساحات حسب الطبقة: '((layer . total_area) ...)
  (setq *qto-blocks* nil)    ; قائمة أعداد البلوكات: '(((block_name . layer) . count) ...)

  (setq i 0)
  (setq total-count (sslength ss))

  (while (< i total-count)
    ;; إظهار تقدم الحصر في المخططات الكبيرة
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
           ;; استبعاد البلوكات الخاصة بالنظام إذا بدأت بـ *
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
           ;; إذا كان بولي لاين مغلق نحسب مساحته أيضاً
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
;;; دالة تصدير النتائج إلى ملف CSV مهيأ للإكسل بترميز UTF-8
;;; --------------------------------------------------------------------------
(defun qto:export-to-csv ( / dwg-path dwg-name default-csv filepath f date-str)
  (setq dwg-path (getvar "DWGPREFIX"))
  (setq dwg-name (vl-filename-base (getvar "DWGNAME")))
  (if (or (null dwg-path) (= dwg-path ""))
    (setq dwg-path "C:\\")
  )
  (if (or (null dwg-name) (= dwg-name ""))
    (setq dwg-name "Drawing")
  )
  (setq default-csv (strcat dwg-path dwg-name "_حصر_الكميات.csv"))

  (setq filepath (getfiled "حفظ تقرير حصر الكميات (Excel CSV)" default-csv "csv" 1))

  (if filepath
    (progn
      (setq f (open filepath "w"))
      (if f
        (progn
          ;; كتابة BOM لفتح الملف باللغة العربية في إكسل بدون رموز غريبة
          (qto:write-utf8-bom f)

          (setq date-str (vl-catch-all-apply 'menucmd (list "M=$(edtime,$(getvar,date),YYYY-MO-DD HH:MM)")))
          (if (vl-catch-all-error-p date-str) (setq date-str "N/A"))

          ;; ترويسة الملف
          (write-line "تقرير حصر الكميات - AutoCAD Quantity Takeoff Report" f)
          (write-line (strcat "المخطط: ," (getvar "DWGNAME")) f)
          (write-line (strcat "التاريخ: ," date-str) f)
          (write-line (strcat "الإصدار: ,v" *qto-version*) f)
          (write-line "" f)

          ;; جدول الأطوال
          (write-line "--- [1] حصر الأطوال (Linear Takeoff) ---" f)
          (write-line "نوع الحصر,اسم الطبقة (Layer),الكمية الإجمالية,الوحدة المقدرة" f)
          (if *qto-lengths*
            (foreach item (vl-sort *qto-lengths* '(lambda (a b) (< (car a) (car b))))
              (write-line (strcat "أطوال," (car item) "," (rtos (cdr item) 2 3) ",متر طولي / وحدة") f)
            )
            (write-line "أطوال,لا يوجد,0,-" f)
          )
          (write-line "" f)

          ;; جدول المساحات
          (write-line "--- [2] حصر المساحات (Area Takeoff) ---" f)
          (write-line "نوع الحصر,اسم الطبقة (Layer),المساحة الإجمالية,الوحدة المقدرة" f)
          (if *qto-areas*
            (foreach item (vl-sort *qto-areas* '(lambda (a b) (< (car a) (car b))))
              (write-line (strcat "مساحات," (car item) "," (rtos (cdr item) 2 3) ",متر مربع / وحدة²") f)
            )
            (write-line "مساحات,لا يوجد,0,-" f)
          )
          (write-line "" f)

          ;; جدول البلوكات
          (write-line "--- [3] حصر البلوكات والعناصر (Block Counts) ---" f)
          (write-line "نوع الحصر,اسم البلوك (Block Name),الطبقة (Layer),العدد,الوحدة" f)
          (if *qto-blocks*
            (foreach item (vl-sort *qto-blocks* '(lambda (a b) (< (caar a) (caar b))))
              (write-line (strcat "بلوكات," (caar item) "," (cdar item) "," (itoa (fix (cdr item))) ",عدد") f)
            )
            (write-line "بلوكات,لا يوجد,-,0,عدد" f)
          )

          (close f)
          (princ (strcat "\nتم بنجاح تصدير تقرير الكميات إلى: " filepath))
          (alert (strcat "تم تصدير تقرير الكميات بنجاح إلى:\n" filepath "\n\nيمكنك فتحه مباشرة في برنامج Excel."))
        )
        (alert "تعذر فتح الملف للكتابة!")
      )
    )
  )
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

          ;; حساب عدد الصفوف المطلوبة
          ;; ترويسة رئيسية (1) + عناوين أعمدة (1) + الأطوال + المساحات + البلوكات
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

                  ;; عنوان الجدول
                  (vla-SetText tbl 0 0 "جدول حصر الكميات - QUANTITY TAKEOFF")

                  ;; عناوين الأعمدة
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
;;; --------------------------------------------------------------------------
(defun c:QTO ( / choice ss exp-opt)
  (setvar "CMDECHO" 0)
  (princ "\n=======================================================")
  (princ (strcat "\n  أداة حصر الكميات لأوتوكاد (v" *qto-version* ") "))
  (princ "\n=======================================================")

  (initget "All Selection كامل تحديد")
  (setq choice (getkword "\nاختر نطاق الحصر [كامل المخطط(All) / تحديد عناصر بالماوس(Selection)] <كامل>: "))

  (if (or (= choice "All") (= choice "كامل") (null choice))
    ;; اختيار كامل المخطط مع استبعاد الكيانات في الـ Defpoints أو الطبقات المجمدة
    (setq ss (ssget "X" '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE,CIRCLE,HATCH,REGION,INSERT"))))
    ;; اختيار يدوي بالماوس
    (progn
      (princ "\nحدد العناصر المطلوب حصرها في المخطط (أو اضغط Enter لإنهاء التحديد): ")
      (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE,CIRCLE,HATCH,REGION,INSERT"))))
    )
  )

  (if (and ss (> (sslength ss) 0))
    (progn
      (princ (strcat "\nجاري فحص وحصر " (itoa (sslength ss)) " عنصراً ..."))
      ;; المعالجة والحصر
      (qto:process-selection ss)

      ;; 1. طباعة الملخص في الـ Command Line
      (qto:print-summary)

      ;; 2. خيار التصدير إلى Excel / CSV
      (initget "Yes No نعم لا")
      (setq exp-opt (getkword "\nهل ترغب في تصدير تقرير الكميات إلى ملف Excel (CSV)؟ [نعم(Yes)/لا(No)] <نعم>: "))
      (if (or (= exp-opt "Yes") (= exp-opt "نعم") (null exp-opt))
        (qto:export-to-csv)
      )

      ;; 3. خيار إدراج جدول داخل أوتوكاد
      (qto:insert-autocad-table)
    )
    (princ "\nلم يتم تحديد أي عناصر قابلة للحصر!")
  )

  (setvar "CMDECHO" 1)
  (princ "\nتم الانتهاء.")
  (princ)
)

;; اسم مرادف بالأحرف اللاتينية والعربية
(defun c:HASR () (c:QTO))
(defun c:QTAKEOFF () (c:QTO))

(princ (strcat "\n>> تم تحميل إضافة حصر الكميات بنجاح (الإصدار v" *qto-version* ")!"))
(princ "\n>> اكتب الأمر QTO أو HASR لبدء الحصر، أو QTO-UPDATE للتحقق من التحديثات.\n")
(princ)
