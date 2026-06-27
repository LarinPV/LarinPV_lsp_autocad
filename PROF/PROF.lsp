; ==================================================
; PROF.LSP
; Команда: PROF
; Макрос: (^C^C_PROF)
; Назначение: Автоматическое построение продольного 
; профиля трассы с таблицей по ГОСТ, планом и пересечками
; ==================================================
; Удачной работы!
; LarinPV, 2026г.

(defun c:Prof (/ *error* doc pline obj points_list elev_ground_list elev_tr_list pt_ins
               total_length dist_list ground_list tr_list ground_pts tr_pts
               i pt1 pt2 min_elev max_elev elev_range crane_elev text_ent text_content
               h_scale v_scale text_height pt closest_pt dist elev_ground elev_tr
               sorted_data min_elev_rounded vertices idx found dist1 dist2
               elev_ground1 elev_ground2 elev_tr1 elev_tr2 ratio elev_ground_curr elev_tr_curr
               x_scaled y_ground_scaled y_tr_scaled pt_ground pt_tr last_tr_pt last_dist
               crane_pt elev_step elev y_pos left_margin gap_x axis_x x_pos dist_remainder
               ground_elev tr_elev depth txt_x cross_item cross_label pt_cross_top
               end_x y_levels y cell_width combined_str table_bottom crossings_list 
               ent_data ent_type layer_name ltype_name ent_obj ent_len tmp_len 
               temp_elev_ground n curr_elev prev_elev prev_dist k next_idx next_elev 
               next_dist is_anomaly total_dist weight expected_elev existing_cross diff
               intervals int d1 d2 e1 e2 len sl slope_segments current_seg seg
               slope_promille total_L x1 x2 x_mid L_val cross_ltype plan_cross
               is_crossing ent_gr ent_tr text_content_gr text_content_tr txt_up)

  (vl-load-com)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  
  (defun *error* (msg)
    (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
      (princ (strcat "\nОшибка: " msg))
    )
    (vla-endundomark doc)
    (princ)
  )
  
  (vla-startundomark doc)
  
  ; Масштабы и высота текста
  (setq h_scale 2.0   ; 1:500
        v_scale 10.0  ; 1:100
        text_height 1.8) ; Высота текста профиля
  
  (princ "\nВыберите полилинию трассы: ")
  (setq pline (car (entsel)))
  (if (not pline)
    (progn (princ "\nНе выбрана полилиния!") (vla-endundomark doc) (exit))
  )
  
  (setq obj (vlax-ename->vla-object pline))
  (setq total_length (vlax-curve-getdistatparam obj (vlax-curve-getendparam obj)))
  
  (princ "\nВыберите точки на полилинии для выбора отметок (Enter для завершения):")
  (setq points_list '() elev_ground_list '() elev_tr_list '() crossings_list '())
  
  (while (setq pt (getpoint "\nУкажите точку на полилинии: "))
    (setq closest_pt (vlax-curve-getclosestpointto obj pt)
          dist (vlax-curve-getdistatpoint obj closest_pt))
    
    (setq elev_ground nil elev_tr nil cross_label nil cross_ltype nil is_crossing nil)
    
    ; Умный запрос: Пересечка или Земля
    (initget "Пропустить")
    (setq text_ent (entsel (strcat "\nПересечка ИЛИ отметка земли для " (rtos dist 2 2) " м [Пропустить]: ")))
    
    (cond
      ((eq text_ent "Пропустить") nil)
      (text_ent
       (setq ent_data (entget (car text_ent)))
       (setq ent_type (cdr (assoc 0 ent_data)))
       
       (cond
         ; 1. ВЫБРАНА ЛИНИЯ (Полилиния пересечения)
         ((or (= ent_type "LWPOLYLINE") (= ent_type "POLYLINE") (= ent_type "LINE"))
          (setq is_crossing t)
          (setq layer_name (cdr (assoc 8 ent_data)))
          (setq ltype_name (cdr (assoc 6 ent_data)))
          (if (or (null ltype_name) (= (strcase ltype_name) "BYLAYER"))
            (setq ltype_name (cdr (assoc 6 (tblsearch "LAYER" layer_name))))
          )
          (if (null ltype_name) (setq ltype_name "Continuous"))

          (setq ent_obj (vlax-ename->vla-object (car text_ent)) ent_len 0.0)
          (if (not (vl-catch-all-error-p (setq tmp_len (vl-catch-all-apply 'vlax-curve-getdistatparam (list ent_obj (vlax-curve-getendparam ent_obj))))))
            (setq ent_len tmp_len)
          )

          (cond
            ((and (wcmatch (strcase layer_name) "*ГАЗОСНАБЖЕНИЕ*") (equal ent_len 2.0 0.01))
             (setq cross_label "смена материала"))
            ((or (wcmatch (strcase ltype_name) "*500_1.5X1.5*") (wcmatch (strcase ltype_name) "*500_8X1*") (wcmatch (strcase ltype_name) "*500_5X2*"))
             (setq cross_label "пересечение с дорогой"))
            ((wcmatch (strcase ltype_name) "*500_133*")
             (setq cross_label "пересечение с кабелем связи"))
            ((or (wcmatch (strcase ltype_name) "*500_119_3*") (wcmatch (strcase ltype_name) "*500_119_1*"))
             (setq cross_label "пересечение с электрическим кабелем"))
            ((wcmatch (strcase layer_name) "*ТЕПЛОСНАБЖЕНИЕ*") (setq cross_label "пересечение с теплотрассой"))
            ((wcmatch (strcase layer_name) "*ГАЗОСНАБЖЕНИЕ*")  (setq cross_label "пересечение с газопроводом"))
            ((wcmatch (strcase layer_name) "*КАНАЛИЗАЦИЯ*")    (setq cross_label "пересечение с канализацией"))
            ((wcmatch (strcase layer_name) "*ВОДОСНАБЖЕНИЕ*")  (setq cross_label "пересечение с водопроводом"))
            (T (setq cross_label "пересечка"))
          )
          (setq cross_ltype ltype_name)
         )
         
         ; 2. ВЫБРАН ТЕКСТ (Умный парсинг с жесткой иерархией)
         ((or (= ent_type "TEXT") (= ent_type "MTEXT"))
          (setq text_content (cdr (assoc 1 ent_data)))
          (setq txt_up (strcase text_content))
          
          ; Определение типа по приоритетам
          (cond
            ((eq (vl-string-trim " 0123456789. " text_content) "") 
             (setq is_crossing nil)) ; Только цифры -> Земля
            ((wcmatch txt_up "*З.Г.М*") 
             (setq is_crossing nil)) ; Точно земля
            ((wcmatch txt_up "*ВЫХОД*") 
             (setq is_crossing t))   ; Выход из земли -> Пересечка
            ((wcmatch txt_up "*УП*") 
             (setq is_crossing t))   ; УП -> Пересечка
            ((wcmatch txt_up "*В.ТР.*") 
             (setq is_crossing t))   ; Верх трубы -> Пересечка
            ((wcmatch txt_up "*ТР.*") 
             (setq is_crossing t))   ; Труба -> Пересечка
            ((wcmatch txt_up "*К.*") 
             (setq is_crossing t))   ; Кабель -> Пересечка
            ((wcmatch txt_up "*Г.*") 
             (setq is_crossing t))   ; Газ -> Пересечка
            ((wcmatch txt_up "*СМЕНА*") 
             (setq is_crossing t))   ; Смена материала -> Пересечка
            ((wcmatch txt_up "*ОПОРА*") 
             (setq is_crossing t))   ; Опора -> Пересечка
            ((wcmatch txt_up "*ПРОВ*") 
             (setq is_crossing t))   ; Провод -> Пересечка
            ((wcmatch txt_up "*З.*") 
             (setq is_crossing nil)) ; Отметка земли -> Земля
            (T 
             (setq is_crossing t))   ; Любой нестандартный текст (например "дорога") -> Пересечка
          )
          
          (if is_crossing
            (progn
              ; Это ПЕРЕСЕЧКА
              (setq cross_label text_content)
              (if (wcmatch txt_up "УП*")
                (setq cross_label (strcat "угол поворота " text_content))
              )
              (setq cross_ltype nil) 
            )
            (progn
              ; Это ЗЕМЛЯ
              (setq elev_ground (atof (vl-string-trim " з.г.мЗГМ" text_content)))
            )
          )
         )
       )
       
       ; Запрос недостающих данных
       (if is_crossing
         (progn
           (initget "Пропустить")
           (setq ent_gr (entsel "\nВыберите отметку ЗЕМЛИ [Пропустить]: "))
           (if (and ent_gr (not (eq ent_gr "Пропустить")))
             (progn
               (setq text_content_gr (cdr (assoc 1 (entget (car ent_gr)))))
               (setq elev_ground (atof (vl-string-trim " з.г.мЗГМ" text_content_gr)))
             )
           )
           
           (initget "Пропустить")
           (setq ent_tr (entsel "\nВыберите отметку ГАЗА [Пропустить]: "))
           (if (and ent_tr (not (eq ent_tr "Пропустить")))
             (progn
               (setq text_content_tr (cdr (assoc 1 (entget (car ent_tr)))))
               (setq elev_tr (atof (vl-string-trim " тр.к.з.г.мТРКЗГМвВ" text_content_tr)))
             )
           )
         )
         (progn
           (initget "Пропустить")
           (setq ent_tr (entsel "\nВыберите отметку ГАЗА [Пропустить]: "))
           (if (and ent_tr (not (eq ent_tr "Пропустить")))
             (progn
               (setq text_content_tr (cdr (assoc 1 (entget (car ent_tr)))))
               (setq elev_tr (atof (vl-string-trim " тр.к.з.г.мТРКЗГМвВ" text_content_tr)))
             )
           )
         )
       )
      )
    )
    
    (if cross_label
      (setq crossings_list (cons (list dist cross_label cross_ltype) crossings_list))
    )
    (setq points_list (cons dist points_list)
          elev_ground_list (cons elev_ground elev_ground_list)
          elev_tr_list (cons elev_tr elev_tr_list))
    (princ (strcat "\nДобавлено. Расстояние: " (rtos dist 2 2) " м"))
  )
  
  (if (null points_list)
    (progn (princ "\nНет точек!") (vla-endundomark doc) (exit))
  )
  
  ; Кран
  (initget "Пропустить")
  (setq text_ent (entsel "\nВыберите отметку крана или [Пропустить]: "))
  (cond
    ((eq text_ent "Пропустить") (setq crane_elev nil))
    (text_ent
     (setq text_content (cdr (assoc 1 (entget (car text_ent)))))
     (cond
       ((wcmatch (strcase text_content) "КР.*") (setq text_content (substr text_content 4))) 
       ((wcmatch (strcase text_content) "КРАН*") (setq text_content (substr text_content 6))) 
     )
     (setq crane_elev (atof (vl-string-trim " з.г.м" text_content))))
    (T (setq crane_elev nil))
  )
  
  ; Сортировка
  (setq sorted_data (vl-sort (mapcar 'list points_list elev_ground_list elev_tr_list)
                             (function (lambda (a b) (< (car a) (car b))))))
  (setq points_list (mapcar 'car sorted_data)
        elev_ground_list (mapcar 'cadr sorted_data)
        elev_tr_list (mapcar 'caddr sorted_data))
        
  ; ================= АНАЛИЗ АНОМАЛИЙ ЗЕМЛИ =================
  (setq temp_elev_ground '() i 0 n (length points_list))
  (while (< i n)
    (setq curr_elev (nth i elev_ground_list) dist (nth i points_list))
    
    (if curr_elev
      (progn
        (setq prev_elev nil prev_dist nil k 0)
        (while (and (< k (length temp_elev_ground)) (null prev_elev))
          (setq prev_elev (nth k temp_elev_ground))
          (if prev_elev (setq prev_dist (nth (- i 1 k) points_list)))
          (setq k (1+ k))
        )
        
        (setq next_idx (1+ i) next_elev nil next_dist nil)
        (while (and (< next_idx n) (null next_elev))
          (setq next_elev (nth next_idx elev_ground_list))
          (if next_elev (setq next_dist (nth next_idx points_list)))
          (setq next_idx (1+ next_idx))
        )
        
        (setq is_anomaly nil diff 0.0)
        
        (if (and prev_elev next_elev)
          (progn
            (setq total_dist (- next_dist prev_dist))
            (if (> total_dist 0.0)
              (setq weight (/ (- dist prev_dist) total_dist)
                    expected_elev (+ (* prev_elev (- 1.0 weight)) (* next_elev weight)))
              (setq expected_elev (/ (+ prev_elev next_elev) 2.0))
            )
            (setq diff (abs (- curr_elev expected_elev)))
          )
          (if prev_elev
            (setq diff (abs (- curr_elev prev_elev)))
            (if next_elev
              (setq diff (abs (- curr_elev next_elev)))
            )
          )
        )
        
        (if (and (equal curr_elev 0.0 0.001) (>= diff 20.0))
          (setq is_anomaly t)
        )
        
        (if is_anomaly
          (progn
            (setq temp_elev_ground (cons nil temp_elev_ground)) 
            (setq existing_cross (assoc dist crossings_list))
            (if existing_cross
              (setq crossings_list (subst (list dist (strcat (cadr existing_cross) " (перепроверить отметку)") (caddr existing_cross)) existing_cross crossings_list))
              (setq crossings_list (cons (list dist "перепроверить отметку" nil) crossings_list))
            )
          )
          (setq temp_elev_ground (cons curr_elev temp_elev_ground)) 
        )
      )
      (setq temp_elev_ground (cons nil temp_elev_ground))
    )
    (setq i (1+ i))
  )
  (setq elev_ground_list (reverse temp_elev_ground))
        
  ; Интерполяция пустых отметок
  (setq elev_ground_list (FillMissingWithDistances elev_ground_list points_list)
        elev_tr_list (FillMissingWithDistances elev_tr_list points_list))
  
  ; Определение диапазона профиля по высоте
  (setq min_elev (min (apply 'min elev_ground_list) (apply 'min elev_tr_list))
        max_elev (max (apply 'max elev_ground_list) (apply 'max elev_tr_list)))
  (if crane_elev (setq min_elev (min min_elev crane_elev) max_elev (max max_elev crane_elev)))
  (setq min_elev_rounded (fix min_elev)
        min_elev (- min_elev_rounded 2.0)
        max_elev (+ max_elev 2.0)
        elev_range (- max_elev min_elev))
  
  (setq pt_ins (getpoint "\nВыберите точку вставки профиля: "))
  (if (not pt_ins) (progn (vla-endundomark doc) (exit)))
  
  ; СОЗДАНИЕ СЛОЕВ
  (CreateLayer "PROF_GROUND" 7 50) 
  (CreateLayer "PROF_TR"     7 60) 
  (CreateLayer "PROF_TEXT"   7  0) 
  (CreateLayer "PROF_AXIS"   7  0) 
  (CreateLayer "PROF_GRID"   7  0) 
  (CreateLayer "PROF_TABLE"  7 30) 
  (CreateLayer "PROF_CROSS"  7  0) 
  
  ; Генерация точек для полилиний профиля
  (setq dist_list '() ground_list '() tr_list '() ground_pts '() tr_pts '())
  (setq vertices (GetPolylineVertices obj))
  
  (foreach vertex vertices
    (setq dist (vlax-curve-getdistatpoint obj vertex) pt vertex idx 0 found nil)
    (while (and (< idx (1- (length points_list))) (not found))
      (if (and (>= dist (nth idx points_list)) (<= dist (nth (1+ idx) points_list)))
        (setq found t) (setq idx (1+ idx)))
    )
    (if found
      (progn
        (setq dist1 (nth idx points_list) dist2 (nth (1+ idx) points_list)
              elev_ground1 (nth idx elev_ground_list) elev_ground2 (nth (1+ idx) elev_ground_list)
              elev_tr1 (nth idx elev_tr_list) elev_tr2 (nth (1+ idx) elev_tr_list))
        (setq ratio (if (equal dist1 dist2 1e-6) 0.0 (/ (- dist dist1) (- dist2 dist1))))
        (setq elev_ground_curr (+ elev_ground1 (* (- elev_ground2 elev_ground1) ratio))
              elev_tr_curr (+ elev_tr1 (* (- elev_tr2 elev_tr1) ratio)))
      )
      (if (< dist (car points_list))
        (setq elev_ground_curr (car elev_ground_list) elev_tr_curr (car elev_tr_list))
        (setq elev_ground_curr (last elev_ground_list) elev_tr_curr (last elev_tr_list))
      )
    )
    (setq dist_list (cons dist dist_list)
          x_scaled (* dist h_scale)
          y_ground_scaled (* (- elev_ground_curr min_elev) v_scale)
          y_tr_scaled (* (- elev_tr_curr min_elev) v_scale)
          pt_ground (list (+ (car pt_ins) x_scaled) (+ (cadr pt_ins) y_ground_scaled))
          pt_tr (list (+ (car pt_ins) x_scaled) (+ (cadr pt_ins) y_tr_scaled))
          ground_pts (cons pt_ground ground_pts) tr_pts (cons pt_tr tr_pts))
  )
  
  (setq dist_list (reverse dist_list) ground_pts (reverse ground_pts) tr_pts (reverse tr_pts))
  
  ; Добавление крана в массив точек трубы
  (if crane_elev
    (setq tr_pts (append tr_pts (list (list (car (last tr_pts)) (+ (cadr pt_ins) (* (- crane_elev min_elev) v_scale))))))
  )

  (DrawPolyline ground_pts "PROF_GROUND")
  (DrawPolyline tr_pts "PROF_TR")
  
  (if crane_elev
    (progn
      (setq crane_pt (last tr_pts))
      (AddTextEx (strcat "Кран " (rtos crane_elev 2 2)) (list (+ (car crane_pt) 1) (cadr crane_pt)) text_height 0 "PROF_TEXT" 9)
    )
  )
  
  ; ================= ГЕОМЕТРИЯ ЗАЗОРА =================
  (setq left_margin (- (car pt_ins) 44) 
        gap_x       (- (car pt_ins) 4)
        axis_x      (- (car pt_ins) 4)
        table_bottom -80)
        
  (setq end_x (+ (car pt_ins) (* total_length h_scale)))
  
  ; Отрисовка осей профиля (линии начинаются от зазора)
  (DrawLine (list axis_x (cadr pt_ins)) (list end_x (cadr pt_ins)) "PROF_AXIS")
  (DrawLine (list axis_x (cadr pt_ins)) (list axis_x (+ (cadr pt_ins) (* elev_range v_scale))) "PROF_AXIS")
  
  ; Вертикальная шкала высот с засечками (перемещена на axis_x = gap_x)
  (setq elev_step 1 elev (fix min_elev))
  (while (<= elev (fix max_elev))
    (setq y_pos (+ (cadr pt_ins) (* (- elev min_elev) v_scale)))
    (AddTextEx (rtos elev 2 0) (list (- axis_x 1.5) y_pos) text_height 0 "PROF_TEXT" 11)
    (DrawLine (list (- axis_x 0.75) y_pos) (list (+ axis_x 0.75) y_pos) "PROF_AXIS")
    (setq elev (+ elev elev_step))
  )
  
  ; Штрихи по оси X
  (foreach dist dist_list
    (if (= (rem dist 10) 0)
      (DrawLine (list (+ (car pt_ins) (* dist h_scale)) (- (cadr pt_ins) 0.75)) 
                (list (+ (car pt_ins) (* dist h_scale)) (+ (cadr pt_ins) 0.75)) "PROF_GRID")
    )
  )
  
  ; Вертикальные линии сетки через профиль
  (foreach dist dist_list
    (if (= (rem dist 10) 0)
      (DrawLine (list (+ (car pt_ins) (* dist h_scale)) (cadr pt_ins)) 
                (list (+ (car pt_ins) (* dist h_scale)) (+ (cadr pt_ins) (* elev_range v_scale))) "PROF_GRID")
    )
  )
  
  ; ================= ПОСТРОЕНИЕ ТАБЛИЦЫ С ПЛАНОМ =================
  ; Все 6 первых строк высотой 10 мм. План высотой 20 (-60 до -80)
  (setq y_levels '(0 -10 -20 -30 -40 -50 -60 -80))
  (foreach y y_levels
    ; Левый боковик (до зазора)
    (DrawLine (list left_margin (+ (cadr pt_ins) y)) (list gap_x (+ (cadr pt_ins) y)) "PROF_TABLE")
    ; Правая таблица (после зазора)
    (DrawLine (list (car pt_ins) (+ (cadr pt_ins) y)) (list end_x (+ (cadr pt_ins) y)) "PROF_TABLE")
  )
  
  ; Вертикальные линии рамок с учетом зазора
  (DrawLine (list left_margin (cadr pt_ins)) (list left_margin (+ (cadr pt_ins) table_bottom)) "PROF_TABLE")
  (DrawLine (list gap_x (cadr pt_ins)) (list gap_x (+ (cadr pt_ins) table_bottom)) "PROF_TABLE")
  (DrawLine pt_ins (list (car pt_ins) (+ (cadr pt_ins) table_bottom)) "PROF_TABLE")
  (DrawLine (list end_x (cadr pt_ins)) (list end_x (+ (cadr pt_ins) table_bottom)) "PROF_TABLE")
  
  ; Осевая линия развернутого плана (середина от -60 до -80)
  (DrawLine (list (car pt_ins) (- (cadr pt_ins) 70)) (list end_x (- (cadr pt_ins) 70)) "PROF_TR")
  
  ; Названия строк 
  (AddTextEx "Отметка земли, м" (list (+ left_margin 2) (- (cadr pt_ins) 5)) text_height 0 "PROF_TEXT" 9)
  (AddTextEx "Отметка верха трубы, м" (list (+ left_margin 2) (- (cadr pt_ins) 15)) text_height 0 "PROF_TEXT" 9)
  (AddTextEx "Глубина траншеи, м" (list (+ left_margin 2) (- (cadr pt_ins) 25)) text_height 0 "PROF_TEXT" 9)
  (AddTextEx "Уклон, \\U+2030 / Длина, м" (list (+ left_margin 2) (- (cadr pt_ins) 35)) text_height 0 "PROF_TEXT" 9)
  (AddTextEx "Расстояния, м" (list (+ left_margin 2) (- (cadr pt_ins) 45)) text_height 0 "PROF_TEXT" 9)
  (AddTextEx "Диаметр" (list (+ left_margin 2) (- (cadr pt_ins) 55)) text_height 0 "PROF_TEXT" 9)
  (AddTextEx "Развернутый план трассы" (list (+ left_margin 2) (- (cadr pt_ins) 70)) text_height 0 "PROF_TEXT" 9)
  
  
  (setq i 0)
  (foreach dist points_list
    (setq x_pos (+ (car pt_ins) (* dist h_scale))
          ground_elev (nth i elev_ground_list)
          tr_elev (nth i elev_tr_list)
          depth (- ground_elev tr_elev))
    
    ; Вертикальные линии останавливаются на -50, не пересекая "Диаметр"
    (DrawLine (list x_pos (cadr pt_ins)) (list x_pos (- (cadr pt_ins) 30)) "PROF_TABLE")
    (DrawLine (list x_pos (- (cadr pt_ins) 40)) (list x_pos (- (cadr pt_ins) 50)) "PROF_TABLE")
    
    (setq txt_x (+ x_pos 1.2))
    (AddTextEx (rtos ground_elev 2 2) (list txt_x (- (cadr pt_ins) 5)) text_height (* pi 0.5) "PROF_TEXT" 10)
    (AddTextEx (rtos tr_elev 2 2) (list txt_x (- (cadr pt_ins) 15)) text_height (* pi 0.5) "PROF_TEXT" 10)
    (AddTextEx (rtos depth 2 2) (list txt_x (- (cadr pt_ins) 25)) text_height (* pi 0.5) "PROF_TEXT" 10)
    
    (setq cross_item (assoc dist crossings_list))
    (if cross_item
      (progn
        (setq cross_label (cadr cross_item)
              cross_ltype (caddr cross_item)
              pt_cross_top (list x_pos (+ (cadr pt_ins) (* elev_range v_scale))))
              
        ; Отрисовка вертикальной выноски на профиле
        (DrawLine (list x_pos (cadr pt_ins)) pt_cross_top "PROF_CROSS")
        (AddTextEx cross_label (list (+ x_pos 0.00) (+ (cadr pt_ins) 1.0)) text_height (* pi 0.5) "PROF_TEXT" 0)
        
        ; Отрисовка на Развернутом плане (БЕЗ ПОДПИСЕЙ)
        (cond
          ((wcmatch (strcase cross_label) "*УГОЛ ПОВОРОТА*")
            ; УП - двойная галочка (стрелка от оси)
            (DrawPolyline (list (list x_pos (- (cadr pt_ins) 70))
                                (list x_pos (- (cadr pt_ins) 75))) "PROF_TR")
            (DrawPolyline (list (list (- x_pos 0.5) (- (cadr pt_ins) 74))
                                (list x_pos (- (cadr pt_ins) 75))
                                (list (+ x_pos 0.5) (- (cadr pt_ins) 74))) "PROF_TR")
          )
          ((wcmatch (strcase cross_label) "*СМЕНА МАТЕРИАЛА*")
            ; Смена материала - знак в виде скобки [ (направлена влево)
            (DrawPolyline (list (list (- x_pos 1.0) (- (cadr pt_ins) 68.5))
                                (list x_pos (- (cadr pt_ins) 68.5))
                                (list x_pos (- (cadr pt_ins) 71.5))
                                (list (- x_pos 1.0) (- (cadr pt_ins) 71.5))) "PROF_TR")
          )
          (T
            ; Обычное пересечение - полилинией на всю высоту плана (-60 до -80)
            (setq plan_cross (DrawPolyline (list (list x_pos (- (cadr pt_ins) 60)) 
                                                 (list x_pos (- (cadr pt_ins) 80))) "PROF_CROSS"))
            (if (and cross_ltype (not (wcmatch (strcase cross_ltype) "BYLAYER,BYBLOCK")))
                (vl-catch-all-apply 'vla-put-linetype (list plan_cross cross_ltype))
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  
  (setq i 0)
  (while (< i (1- (length points_list)))
    (setq dist1 (nth i points_list)
          dist2 (nth (1+ i) points_list)
          L_val (- dist2 dist1)
          x1 (+ (car pt_ins) (* dist1 h_scale))
          x2 (+ (car pt_ins) (* dist2 h_scale))
          x_mid (/ (+ x1 x2) 2.0))
    (if (> L_val 0.01)
      (if (> (- x2 x1) 8.0)
        (AddTextEx (rtos L_val 2 2) (list x_mid (- (cadr pt_ins) 45)) text_height 0 "PROF_TEXT" 10)
        (AddTextEx (rtos L_val 2 2) (list x_mid (- (cadr pt_ins) 45)) text_height (* pi 0.5) "PROF_TEXT" 10)
      )
    )
    (setq i (1+ i))
  )
  
  ; ================= АЛГОРИТМ ОБЪЕДИНЕНИЯ ОДНОРОДНЫХ УКЛОНОВ =================
  (setq intervals '() i 0)
  (while (< i (1- (length points_list)))
    (setq intervals (cons (list (nth i points_list) (nth (1+ i) points_list) (nth i elev_tr_list) (nth (1+ i) elev_tr_list)) intervals))
    (setq i (1+ i))
  )
  (setq intervals (reverse intervals))

  (setq slope_segments '() current_seg nil)
  (foreach int intervals
    (setq d1 (car int) d2 (cadr int) e1 (caddr int) e2 (nth 3 int))
    (setq len (- d2 d1))
    (setq sl (if (> len 0.001) (/ (- e2 e1) len) 0.0))
    
    (if (null current_seg)
      (setq current_seg (list d1 d2 e1 e2 sl))
      (if (equal sl (nth 4 current_seg) 1e-4) 
        (setq current_seg (list (car current_seg) d2 (caddr current_seg) e2 sl))
        (progn
          (setq slope_segments (cons current_seg slope_segments))
          (setq current_seg (list d1 d2 e1 e2 sl))
        )
      )
    )
  )
  (if current_seg (setq slope_segments (cons current_seg slope_segments)))
  (setq slope_segments (reverse slope_segments))
  
  (foreach seg slope_segments
    (setq d1 (car seg) d2 (cadr seg) e1 (caddr seg) e2 (nth 3 seg) sl (nth 4 seg))
    (setq x1 (+ (car pt_ins) (* d1 h_scale)))
    (setq x2 (+ (car pt_ins) (* d2 h_scale)))
    (setq x_mid (/ (+ x1 x2) 2.0))
    (setq total_L (- d2 d1))
    (setq slope_promille (* sl 1000.0))
    
    (DrawLine (list x1 (- (cadr pt_ins) 30)) (list x1 (- (cadr pt_ins) 40)) "PROF_TABLE")
    (DrawLine (list x2 (- (cadr pt_ins) 30)) (list x2 (- (cadr pt_ins) 40)) "PROF_TABLE")
    
    (if (equal e1 e2 0.001)
      (DrawLine (list x1 (- (cadr pt_ins) 35)) (list x2 (- (cadr pt_ins) 35)) "PROF_TABLE")
      (if (> e2 e1)
        (DrawLine (list x1 (- (cadr pt_ins) 40)) (list x2 (- (cadr pt_ins) 30)) "PROF_TABLE") 
        (DrawLine (list x1 (- (cadr pt_ins) 30)) (list x2 (- (cadr pt_ins) 40)) "PROF_TABLE") 
      )
    )
    
    (setq cell_width (- x2 x1))
    (if (> cell_width 15.0)
      (progn
        (AddTextEx (strcat (rtos (abs slope_promille) 2 1) "\\U+2030") (list x_mid (- (cadr pt_ins) 32.5)) text_height 0 "PROF_TEXT" 10)
        (AddTextEx (rtos total_L 2 2) (list x_mid (- (cadr pt_ins) 37.5)) text_height 0 "PROF_TEXT" 10)
      )
      (progn
        (setq combined_str (strcat (rtos (abs slope_promille) 2 1) "\\U+2030 / " (rtos total_L 2 1)))
        (AddTextEx combined_str (list x_mid (- (cadr pt_ins) 35)) text_height (* pi 0.5) "PROF_TEXT" 10)
      )
    )
  )
  
  (AddTextEx "ПРОДОЛЬНЫЙ ПРОФИЛЬ" (list (+ (car pt_ins) (* total_length h_scale 0.5)) (+ (cadr pt_ins) (* elev_range v_scale) 15)) (* text_height 1.5) 0 "PROF_TEXT" 10)
  
  (vla-endundomark doc)
  (princ "\nПостроение профиля успешно завершено!")
  (princ)
)

;;; ================= Вспомогательные функции =================

(defun AddTextEx (content insertion height rotation layer align / modelspace text)
  (setq modelspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
        text (vla-addtext modelspace content (vlax-3d-point insertion) height))
  (vla-put-layer text layer)
  (vla-put-alignment text align)
  (if (/= align 0) 
    (vla-put-textalignmentpoint text (vlax-3d-point insertion))
    (vla-put-insertionpoint text (vlax-3d-point insertion))
  )
  (vla-put-rotation text rotation)
)

(defun GetPolylineVertices (obj / vertices dist total_length step pt)
  (setq vertices '() dist 0.0 step 0.1
        total_length (vlax-curve-getdistatparam obj (vlax-curve-getendparam obj)))
  (while (<= dist (+ total_length 1e-5))
    (setq pt (vlax-curve-getpointatdist obj dist)
          vertices (cons pt vertices)
          dist (+ dist step))
  )
  (reverse vertices)
)

(defun sublist (lst start end / res)
  (setq res '())
  (repeat (- end start) (setq res (cons (nth start lst) res) start (1+ start)))
  (reverse res)
)

(defun FillMissingWithDistances (values distances / result n i prev next prev_val next_val prev_dist next_dist total_dist dist_to_prev weight new_val)
  (setq result (mapcar '(lambda (x) x) values) n (length result) i 0)
  (while (< i n)
    (if (null (nth i result))
      (progn
        (setq prev i) (while (and (>= (setq prev (1- prev)) 0) (null (nth prev result))))
        (setq prev_val (if (>= prev 0) (nth prev result) nil) prev_dist (if (>= prev 0) (nth prev distances) 0.0))
        (setq next i) (while (and (< (setq next (1+ next)) n) (null (nth next result))))
        (setq next_val (if (< next n) (nth next result) nil) next_dist (if (< next n) (nth next distances) 0.0))
        (cond
          ((and prev_val next_val)
           (setq dist_to_prev (- (nth i distances) prev_dist) total_dist (- next_dist prev_dist))
           (if (> total_dist 0.0)
             (setq weight (/ dist_to_prev total_dist) new_val (+ (* prev_val (- 1.0 weight)) (* next_val weight)))
             (setq new_val (/ (+ prev_val next_val) 2.0)))
           (setq result (append (sublist result 0 i) (list new_val) (sublist result (1+ i) n))))
          (prev_val (setq result (append (sublist result 0 i) (list prev_val) (sublist result (1+ i) n))))
          (next_val (setq result (append (sublist result 0 i) (list next_val) (sublist result (1+ i) n))))
          (T (setq result (append (sublist result 0 i) (list 0.0) (sublist result (1+ i) n))))
        )
      )
    )
    (setq i (1+ i))
  )
  result
)

(defun CalculateAllSlopes (pts_list elevs_list / slopes i dist1 dist2 elev1 elev2 slope dist_diff elev_diff)
  (setq slopes '() i 0)
  (while (< i (length pts_list))
    (if (= i 0)
      (setq slopes (cons "" slopes))
      (progn
        (setq dist1 (nth (1- i) pts_list) dist2 (nth i pts_list)
              elev1 (nth (1- i) elevs_list) elev2 (nth i elevs_list)
              dist_diff (- dist2 dist1) elev_diff (- elev2 elev1))
        (if (and dist_diff (> (abs dist_diff) 0.001))
          (setq slope (/ elev_diff dist_diff))
          (setq slope 0.0))
        (setq slopes (cons (rtos slope 2 4) slopes))
      )
    )
    (setq i (1+ i))
  )
  (reverse slopes)
)

(defun DrawPolyline (pts layer / points poly)
  (setq points (apply 'append pts))
  (setq poly (vlax-invoke (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))) 'addLightWeightPolyline points))
  (vla-put-layer poly layer)
  poly
)

(defun DrawLine (pt1 pt2 layer / modelspace line)
  (setq modelspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
        line (vla-addline modelspace (vlax-3d-point (list (car pt1) (cadr pt1) 0.0)) (vlax-3d-point (list (car pt2) (cadr pt2) 0.0))))
  (vla-put-layer line layer)
  line
)

(defun DrawCircle (center radius layer / modelspace circle)
  (setq modelspace (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
        circle (vla-addcircle modelspace (vlax-3d-point (list (car center) (cadr center) 0.0)) radius))
  (vla-put-layer circle layer)
  circle
)

(defun CreateLayer (name color weight / layers layer)
  (setq layers (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
  (if (not (tblsearch "LAYER" name))
    (setq layer (vla-add layers name))
    (setq layer (vla-item layers name))
  )
  (vla-put-color layer color)
  (vl-catch-all-apply 'vla-put-lineweight (list layer weight))
)

(princ "\nКоманда PROF перезагружена. Введите PROF для запуска.")
(princ)