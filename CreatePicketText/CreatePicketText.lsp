; =======================================
; кнопка (^C^C_ПК)
; Создание пикетажной выноски на трассе (с выбором формата текста)
; =======================================
; Удачной работы!
; LarinPV, 2025г. (Обновлено)

(defun c:ПК (/ *error* oldcmdecho oldosmode olddimzin oldclayer oldtextstyle pline pt param stationRaw station basePt linePt textHeight textOffset stationStr km m continue decimalPlaces roundingPlaces userHeight textPt lineEndPt nearestPt pt2d nearestPt2d formatChoice mStr zeroChoice decChoice padChoice lineLength txtList)

  (defun *error* (msg)
    (if oldcmdecho (setvar 'CMDECHO oldcmdecho))
    (if oldosmode (setvar 'OSMODE oldosmode))
    (if olddimzin (setvar 'DIMZIN olddimzin))
    (if oldclayer (setvar 'CLAYER oldclayer))
    (if oldtextstyle (setvar 'TEXTSTYLE oldtextstyle))
    (if (not (wcmatch (strcase msg) "*CANCEL*,*EXIT*"))
      (princ (strcat "\nОшибка: " msg))
    )
    (princ)
  )

  ; Функция для преобразования 3D точки в 2D (игнорируя Z)
  (defun 2dpoint (pt)
    (list (car pt) (cadr pt))
  )
  
  ; Функция создания текста
  (defun CreateText (point height rotation text / txtList)
    (setq txtList
      (list
        '(0 . "TEXT")
        (cons 8 (getvar 'CLAYER))
        (cons 10 point)
        (cons 40 height)
        (cons 1 text)
        (cons 7 (getvar 'TEXTSTYLE))
        (cons 50 rotation)
        (if (eq (getvar 'TEXTSTYLE) "rn3") (cons 41 0.7))
        (if (eq (getvar 'TEXTSTYLE) "rn3") (cons 51 (cvunit 15 "degree" "radian")))
      )
    )
    (entmake (vl-remove nil txtList))
  )

  (vl-load-com)
  
  (setq oldcmdecho (getvar 'CMDECHO))
  (setq oldosmode (getvar 'OSMODE))
  (setq olddimzin (getvar 'DIMZIN))
  (setq oldclayer (getvar 'CLAYER))
  (setq oldtextstyle (getvar 'TEXTSTYLE))
  
  (setvar 'CMDECHO 0)
  (setvar 'OSMODE 63)

  (if (not (tblsearch "LAYER" "Пикетаж"))
    (progn
      (command "_.LAYER" "_M" "Пикетаж" "_C" "7" "" "")
      (setvar 'CLAYER "Пикетаж")
    )
    (setvar 'CLAYER "Пикетаж")
  )

  (if (tblsearch "STYLE" "rn3")
    (progn
      (setvar 'TEXTSTYLE "rn3")
      (setq textHeight 0.875)
    )
    (progn
      (setvar 'TEXTSTYLE "Standard")
      (setq textHeight 0.88)
    )
  )

  ;; ---------- НАГЛЯДНОЕ МЕНЮ ФОРМАТА ----------
  (initget "1 2")
  (setq formatChoice (getkword "\nВыберите формат [1 (ПК1+23.45) / 2 (ПК1 +23.45)] <1>: "))
  (if (not formatChoice) (setq formatChoice "1"))

  (setq userHeight (getreal (strcat "\nВведите высоту текста <" (rtos textHeight 2 2) ">: ")))
  (if userHeight (setq textHeight userHeight))

  ;; ---------- НАСТРОЙКА ОКРУГЛЕНИЯ ----------
  (initget "1 2 3")
  (setq decChoice (getkword "\nКоличество знаков после запятой [1 / 2 / 3] <2>: "))
  (if (not decChoice) (setq decChoice "2"))
  
  (setq roundingPlaces (atoi decChoice))
  (setq decimalPlaces roundingPlaces) ; По умолчанию выводим столько же знаков, сколько округляем

  (initget "Yes No Да Нет")
  (setq zeroChoice (getkword "\nОтображать незначащие нули в конце? [Да/Нет] <Да>: "))
  
  (if (or (= zeroChoice "No") (= zeroChoice "Нет"))
    (setvar 'DIMZIN 8) ; Обрезать лишние нули (31.1)
    (progn
      (setvar 'DIMZIN 0) ; Форсировать отображение конечных нулей
      
      ; Если выбран 1 знак и включены нули, предлагаем дополнить до 2-х знаков
      (if (= roundingPlaces 1)
        (progn
          (initget "Yes No Да Нет")
          (setq padChoice (getkword "\nДополнить до 2-х знаков нулем (например, 31.10 вместо 31.1)? [Да/Нет] <Да>: "))
          (if (or (not padChoice) (= padChoice "Yes") (= padChoice "Да"))
            (setq decimalPlaces 2) ; Визуально дополняем до 2 знаков, хотя математика округлена до 1
          )
        )
      )
    )
  )

  (setq textOffset (* textHeight 1.1))

  ;; ---------- ВЫБОР ТРАССЫ ----------
  (setq pline nil)
  (while (not pline)
    (setq pline (car (entsel "\nВыберите трассу (линию, полилинию, сплайн): ")))
    (if (not pline)
      (princ "\nОбъект не выбран. Попробуйте еще раз.")
      (if (not (wcmatch (cdr (assoc 0 (entget pline))) "*POLYLINE,LINE,ARC,SPLINE"))
        (progn
          (princ "\nВыбранный объект не поддерживается. Выберите линию, полилинию, дугу или сплайн.")
          (setq pline nil)
        )
      )
    )
  )

  ;; ---------- РАССТАНОВКА ПИКЕТОВ ----------
  (setq continue T)
  (while continue
    (initget "Exit")
    (setq pt (getpoint "\nУкажите точку (или Enter/Exit для завершения): "))
    
    (cond
      ((or (eq pt "Exit") (null pt)) (setq continue nil))
      
      (T
        ; Преобразуем точки в 2D
        (setq pt2d (2dpoint pt))
        (setq nearestPt (vlax-curve-getClosestPointTo pline pt))
        (setq nearestPt2d (2dpoint nearestPt))
        
        ; Проверяем расстояние в 2D пространстве
        (if (> (distance pt2d nearestPt2d) (* textHeight 0.1))
          (princ "\nТочка слишком далеко от трассы.")
          (progn
            (setq param (vlax-curve-getParamAtPoint pline nearestPt))
            (setq textPt (getpoint nearestPt "\nУкажите точку вставки текста: "))
            (if (null textPt)
              (setq textPt (polar nearestPt 0 (* textHeight 10)))
            )
            
            ; Вычисляем длину дополнительного отрезка
            (setq lineLength (* (/ 6 0.88) textHeight))
            (setq lineEndPt (polar textPt 0 lineLength))
            
            ; Вычисляем станцию
            (setq stationRaw (vlax-curve-getDistAtParam pline param))
            
            ; Строго округляем общую длину перед разбиением (исключает ошибку ПК1+100.0)
            (setq station (atof (rtos stationRaw 2 roundingPlaces)))
            
            (setq km (fix (/ station 100)))
            (setq m (rem station 100))

            ;; ЛОГИКА ФОРМАТИРОВАНИЯ
            (setq mStr (rtos m 2 decimalPlaces))
            (if (= formatChoice "1")
              (setq stationStr (strcat "ПК" (itoa km) "+" mStr))      ; Формат 1
              (setq stationStr (strcat "ПК" (itoa km) " +" mStr))     ; Формат 2
            )

            ; Создаем выноску
            (command "_.LINE" "_non" nearestPt "_non" textPt "")
            (command "_.LINE" "_non" textPt "_non" lineEndPt "")

            ; Создаем текстовые элементы
            (CreateText textPt textHeight 0 "ТЕКСТ")
            (CreateText (polar textPt (* pi -0.5) textOffset) textHeight 0 stationStr)
            (CreateText (polar textPt (* pi -0.5) (* 2 textOffset)) textHeight 0 "з.000.00")
            (CreateText (polar textPt (* pi -0.5) (* 3 textOffset)) textHeight 0 "г.000.00")

            (princ (strcat "\nСоздана выноска на станции " stationStr))
          )
        )
      )
    )
  )

  (setvar 'CMDECHO oldcmdecho)
  (setvar 'OSMODE oldosmode)
  (setvar 'DIMZIN olddimzin) ; Восстанавливаем оригинальные настройки нулей
  (setvar 'CLAYER oldclayer)
  (setvar 'TEXTSTYLE oldtextstyle)
  
  (princ "\nЗавершение работы команды.")
  (princ)
)

(princ "\nКоманда ПК загружена. Введите ПК для запуска.")
(princ)