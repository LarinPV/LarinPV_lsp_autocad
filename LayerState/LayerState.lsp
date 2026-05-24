; =======================================
; LayerState.lsp
; кнопка (^C^C_LS_SAVE) (сохранить)
; кнопка (^C^C_LS_RESTORE) восстановить
; Сохранение и восстановление состояний слоёв (Вкл/Откл, Заморозка)
; =======================================
; Удачной работы!
; LarinPV, 2026г.


(vl-load-com)
(setq *LS_SavedStates* nil)

(defun c:LS_SAVE (/ doc layers layer lst name on frz lock col ltype lwt transp)
  (setq doc    (vla-get-activedocument (vlax-get-acad-object))
        layers (vla-get-layers doc)
        lst    nil)
  
  (vlax-for layer layers
    (setq name   (vla-get-name layer)
          on     (vla-get-layeron layer)
          frz    (vla-get-freeze layer)
          lock   (vla-get-lock layer)
          col    (vla-get-color layer)
          ltype  (vla-get-linetype layer)
          lwt    (vla-get-lineweight layer)
          transp (if (vlax-property-available-p layer 'entitytransparency)
                   (vl-princ-to-string (vla-get-entitytransparency layer))
                   "0")
          lst    (cons (list name on frz lock col ltype lwt transp) lst))
  )
  
  (setq *LS_SavedStates* lst)
  (princ (strcat "\n[OK] Сохранено состояний слоев: " (itoa (length lst))))
  (princ)
)

(defun c:LS_RESTORE (/ doc layers cur-layer item name on frz lock col ltype lwt transp layer err)
  (if (not *LS_SavedStates*)
    (progn (princ "\n[ERROR] Нет сохраненных состояний. Сначала выполните LS_SAVE.") (princ))
    (progn
      (setq doc       (vla-get-activedocument (vlax-get-acad-object))
            layers    (vla-get-layers doc)
            cur-layer (vla-get-name (vla-get-activelayer doc)))
      
      (foreach item *LS_SavedStates*
        (setq name   (car item)
              on     (cadr item)
              frz    (caddr item)
              lock   (cadddr item)
              col    (nth 4 item)
              ltype  (nth 5 item)
              lwt    (nth 6 item)
              transp (nth 7 item))
        
        ;; Защита от старых сохранений без прозрачности
        (if (not transp) (setq transp "0"))
        
        (if (tblsearch "LAYER" name)
          (progn
            (setq layer (vla-item layers name)
                  err   nil)
            
            ;; 1. Цвет (ACI)
            (setq err (vl-catch-all-apply 'vla-put-color (list layer col)))
            (if (vl-catch-all-error-p err)
              (princ (strcat "\n[ERROR] Цвет \"" name "\": " (vl-catch-all-error-message err))))
            
            ;; 2. Тип линии
            (setq err (vl-catch-all-apply 'vla-put-linetype (list layer ltype)))
            (if (vl-catch-all-error-p err)
              (princ (strcat "\n[WARN] Тип линии \"" name "\" не загружен в чертеж: " ltype)))
            
            ;; 3. Вес линии
            (setq err (vl-catch-all-apply 'vla-put-lineweight (list layer lwt)))
            (if (vl-catch-all-error-p err)
              (princ (strcat "\n[ERROR] Вес \"" name "\": " (vl-catch-all-error-message err))))
            
            ;; 4. Прозрачность (AutoCAD 2011+)
            (if (vlax-property-available-p layer 'entitytransparency)
              (progn
                (setq err (vl-catch-all-apply 'vla-put-entitytransparency (list layer transp)))
                (if (vl-catch-all-error-p err)
                  (princ (strcat "\n[ERROR] Прозрачность \"" name "\": " (vl-catch-all-error-message err))))))
            
            ;; 5. Блокировка (нельзя блокировать текущий слой)
            (if (and (eq lock :vlax-true) (eq name cur-layer))
              (princ (strcat "\n[WARN] Пропущена блокировка текущего слоя: " name))
              (progn
                (setq err (vl-catch-all-apply 'vla-put-lock (list layer lock)))
                (if (vl-catch-all-error-p err)
                  (princ (strcat "\n[ERROR] Блокировка \"" name "\": " (vl-catch-all-error-message err))))))
            
            ;; 6. Заморозка (нельзя морозить текущий слой)
            (if (and (eq frz :vlax-true) (eq name cur-layer))
              (princ (strcat "\n[WARN] Пропущена заморозка текущего слоя: " name))
              (progn
                (setq err (vl-catch-all-apply 'vla-put-freeze (list layer frz)))
                (if (vl-catch-all-error-p err)
                  (princ (strcat "\n[ERROR] Заморозка \"" name "\": " (vl-catch-all-error-message err))))))
            
            ;; 7. Вкл/Откл (применяется в последнюю очередь)
            (setq err (vl-catch-all-apply 'vla-put-layeron (list layer on)))
            (if (vl-catch-all-error-p err)
              (princ (strcat "\n[ERROR] Вкл/Откл \"" name "\": " (vl-catch-all-error-message err))))
          )
          (princ (strcat "\n[WARN] Слой не найден в чертеже: " name))
        )
      )
      
      (vla-regen doc acAllViewports)
      (princ "\n[OK] Состояния слоев восстановлены.")
      (princ)
    )
  )
)

(princ "\nЗагружены команды: LS_SAVE (сохранить), LS_RESTORE (восстановить)")
(princ)