;;; workgroups-win.el --- Emacs WINDOW
;;; Commentary:
;;
;; Emacs Window has a Buffer in it (see workgroups-buf.el). Window also
;; has got a "history" of opened buffers and more parameters.
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Windows-and-Frames.html#Windows-and-Frames
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Windows.html#Windows
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-History.html
;;
;;; Code:

(require 'workgroups-buf)

(defun wg-min-size (dir)
  "Return the minimum window size in split direction DIR."
  (if dir wg-window-min-height wg-window-min-width))

(defun wg-actual-min-size (dir)
  "Return the actual minimum window size in split direction DIR."
  (if dir wg-actual-min-height wg-actual-min-width))

(defmacro wg-with-edges (w spec &rest body)
  "Bind W's edge list to SPEC and eval BODY."
  (declare (indent 2))
  `(wg-dbind ,spec (wg-w-edges ,w) ,@body))

(defmacro wg-with-bounds (wtree dir spec &rest body)
  "Bind SPEC to W's bounds in DIR, and eval BODY.
\"bounds\" are a direction-independent way of dealing with edge lists."
  (declare (indent 3))
  (wg-with-gensyms (dir-sym l1 t1 r1 b1)
    (wg-dbind (ls1 hs1 lb1 hb1) spec
      `(wg-with-edges ,wtree (,l1 ,t1 ,r1 ,b1)
         (cond (,dir (let ((,ls1 ,l1) (,hs1 ,r1) (,lb1 ,t1) (,hb1 ,b1))
                       ,@body))
               (t    (let ((,ls1 ,t1) (,hs1 ,b1) (,lb1 ,l1) (,hb1 ,r1))
                       ,@body)))))))

(defun wg-set-bounds (w dir ls hs lb hb)
  "Set W's edges in DIR with bounds LS HS LB and HB."
  (wg-set-edges w (if dir (list ls lb hs hb) (list lb ls hb hs))))

(defun wg-step-edges (edges1 edges2 hstep vstep)
  "Return W1's edges stepped once toward W2's by HSTEP and VSTEP."
  (wg-dbind (l1 t1 r1 b1) edges1
    (wg-dbind (l2 t2 r2 b2) edges2
      (let ((left (wg-step-to l1 l2 hstep))
            (top  (wg-step-to t1 t2 vstep)))
        (list left top
              (+ left (wg-step-to (- r1 l1) (- r2 l2) hstep))
              (+ top  (wg-step-to (- b1 t1) (- b2 t2) vstep)))))))

(defun wg-w-edge-operation (w edges op)
  "Return a copy of W with its edges mapped against EDGES through OP."
  (wg-set-edges w (cl-mapcar op (wg-w-edges w) edges)))

(defun wg-first-win (w)
  "Return the first actual window in W."
  (if (wg-win-p w) w
    (wg-first-win (car (wg-wtree-wlist w)))))

(defun wg-last-win (w)
  "Return the last actual window in W."
  (if (wg-win-p w) w
    (wg-last-win (wg-last1 (wg-wtree-wlist w)))))

(defun wg-minify-win (w)
  "Set W's edges to the smallest allowable."
  (let* ((edges (wg-w-edges w))
         (left (car edges))
         (top (cadr edges)))
    (wg-set-edges w (list left top
                          (+ left wg-actual-min-width)
                          (+ top  wg-actual-min-height)))))

(defun wg-minified-copy-of-last-win (w)
  "Minify a copy of the last actual window in W."
  (wg-minify-win (wg-copy-win (wg-last-win w))))

(defun wg-w-size (w &optional height)
  "Return the width or height of W, calculated from its edge list."
  (wg-with-edges w (l1 t1 r1 b1)
    (if height (- b1 t1) (- r1 l1))))

(defun wg-adjust-w-size (w width-fn height-fn &optional new-left new-top)
  "Adjust W's width and height with WIDTH-FN and HEIGHT-FN."
  (wg-with-edges w (left top right bottom)
    (let ((left (or new-left left)) (top (or new-top top)))
      (wg-set-edges (wg-copy-w w)
                    (list left
                          top
                          (+ left (funcall width-fn  (- right  left)))
                          (+ top  (funcall height-fn (- bottom top))))))))

(defun wg-scale-w-size (w width-scale height-scale)
  "Scale W's size by WIDTH-SCALE and HEIGHT-SCALE."
  (cl-labels
      ((wscale (width)  (truncate (* width  width-scale)))
       (hscale (height) (truncate (* height height-scale))))
    (wg-adjust-w-size w #'wscale #'hscale)))



(defun wg-restore-window-positions (win &optional window)
  "Restore various positions in WINDOW from their values in WIN."
  (let ((window (or window (selected-window))))
    (wg-with-slots win
        ((win-point wg-win-point)
         (win-start wg-win-start)
         (win-hscroll wg-win-hscroll))
      (set-window-start window win-start t)
      (set-window-hscroll window win-hscroll)
      (set-window-point
       window
       (cond ((not wg-restore-point) win-start)
             ((eq win-point :max) (point-max))
             (t win-point)))
      (when (>= win-start (point-max)) (recenter)))))

(defun wg-restore-window (win)
  "Restore WIN in `selected-window'."
  (let ((selwin (selected-window))
        (buf (wg-find-buf-by-uid (wg-win-buf-uid win))))
    (if (not buf)
        (wg-restore-default-buffer)
      (when (wg-restore-buffer buf)
        (wg-restore-window-positions win selwin)
        (when wg-restore-window-dedicated-p
          (set-window-dedicated-p selwin (wg-win-dedicated win)))))
    (ignore-errors
      (set-window-prev-buffers
       selwin (wg-unpickel (wg-win-parameter win 'prev-buffers)))
      (set-window-next-buffers
       selwin (wg-unpickel (wg-win-parameter win 'next-buffers)))
      )))


(defun wg-window-point (ewin)
  "Return `point' or :max.  See `wg-restore-point-max'.
EWIN should be an Emacs window object."
  (let ((p (window-point ewin)))
    (if (and wg-restore-point-max (= p (point-max))) :max p)))

(defun wg-win-parameter (win parameter &optional default)
  "Return WIN's value for PARAMETER.
If PARAMETER is not found, return DEFAULT which defaults to nil.
SESSION nil defaults to the current session."
  (wg-aget (wg-win-parameters win) parameter default))

(defun wg-set-win-parameter (win parameter value)
  "Set WIN's value of PARAMETER to VALUE.
SESSION nil means use the current session.
Return value."
  (wg-set-parameter (wg-win-parameters win) parameter value)
  value)
;; (wg-win-parameters (wg-window-to-win (selected-window)))

(defun wg-remove-win-parameter (win parameter)
  "Remove parameter PARAMETER from WIN's parameters."
  (wg-asetf (wg-win-parameters win) (wg-aremove it parameter)))

;; (wg-window-to-win (selected-window))
(defun wg-window-to-win (window)
  "Return the serialization (a wg-win) of Emacs window WINDOW."
  (let ((selected (eq window (selected-window)))
        win)
    (with-selected-window window
      (setq win
            (wg-make-win
             :edges              (window-edges window)
             :point              (wg-window-point window)
             :start              (window-start window)
             :hscroll            (window-hscroll window)
             :selected           selected
             :minibuffer-scroll  (eq window minibuffer-scroll-window)
             :dedicated          (window-dedicated-p window)
             :buf-uid            (wg-buffer-uid-or-add (window-buffer window))))
      ;; To solve: https://github.com/pashinin/workgroups2/issues/51
      ;; shouldn't ignore here
      (ignore-errors
        (wg-set-win-parameter
         win 'next-buffers (wg-pickel (remove nil (cl-subseq (window-next-buffers window) 0 4))))
        (wg-set-win-parameter
         win 'prev-buffers (wg-pickel (remove nil (cl-subseq (window-prev-buffers window) 0 4))))
        ))
    win))

(defun wg-toggle-window-dedicated-p ()
  "Toggle `window-dedicated-p' in `selected-window'."
  (interactive)
  (set-window-dedicated-p nil (not (window-dedicated-p)))
  (force-mode-line-update t)
  (wg-fontified-message
    (:cmd "Window:")
    (:cur (concat (unless (window-dedicated-p) " not") " dedicated"))))

(provide 'workgroups-win)
;;; workgroups-win.el ends here
