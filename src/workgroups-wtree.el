;;; workgroups-wtree.el --- window-tree "class"
;;; Commentary:
;;
;; `window-tree' - several opened windows, their sizes and position is
;; what you want to save.
;;
;;; Code:

(require 'workgroups-win)

(defun wg-w-edges (w)
  "Return W's edge list."
  (cl-etypecase w
    (wg-win (wg-win-edges w))
    (wg-wtree (wg-wtree-edges w))))

(defun wg-copy-w (w)
  "Return a copy of W.  W should be a wg-win or a wg-wtree."
  (cl-etypecase w
    (wg-win (wg-copy-win w))
    (wg-wtree (wg-copy-wtree w))))

(defun wg-set-edges (w edges)
  "Set W's edge list, and return W."
  (cl-etypecase w
    (wg-win (setf (wg-win-edges w) edges))
    (wg-wtree (setf (wg-wtree-edges w) edges)))
  w)


(defun wg-equal-wtrees (w1 w2)
  "Return t when W1 and W2 have equal structure."
  (cond ((and (wg-win-p w1) (wg-win-p w2))
         (equal (wg-w-edges w1) (wg-w-edges w2)))
        ((and (wg-wtree-p w1) (wg-wtree-p w2))
         (and (eq (wg-wtree-dir w1) (wg-wtree-dir w2))
              (equal (wg-wtree-edges w1) (wg-wtree-edges w2))
              (cl-every #'wg-equal-wtrees
                        (wg-wtree-wlist w1)
                        (wg-wtree-wlist w2))))))

(defun wg-normalize-wtree (wtree)
  "Clean up and return a new wtree from WTREE.
Recalculate the edge lists of all subwins, and remove subwins
outside of WTREE's bounds.  If there's only one element in the
new wlist, return it instead of a new wtree."
  (if (wg-win-p wtree) wtree
    (wg-with-slots wtree ((dir wg-wtree-dir)
                          (wlist wg-wtree-wlist))
      (wg-with-bounds wtree dir (ls1 hs1 lb1 hb1)
        (let* ((min-size (wg-min-size dir))
               (max (- hb1 1 min-size))
               (lastw (-last-item wlist)))
          (cl-labels
              ((mapwl
                (wl)
                (wg-dbind (sw . rest) wl
                  (cons (wg-normalize-wtree
                         (wg-set-bounds
                          sw dir ls1 hs1 lb1
                          (setq lb1 (if (eq sw lastw) hb1
                                      (let ((hb2 (+ lb1 (wg-w-size sw dir))))
                                        (if (>= hb2 max) hb1 hb2))))))
                        (when (< lb1 max) (mapwl rest))))))
            (let ((new (mapwl wlist)))
              (if (not (cdr new)) (car new)
                (setf (wg-wtree-wlist wtree) new)
                wtree))))))))

(defun wg-scale-wtree (wtree wscale hscale)
  "Return a copy of WTREE with its dimensions scaled by WSCALE and HSCALE.
All WTREE's subwins are scaled as well."
  (let ((scaled (wg-scale-w-size wtree wscale hscale)))
    (if (wg-win-p wtree) scaled
      (wg-asetf (wg-wtree-wlist scaled)
                (wg-docar (sw it) (wg-scale-wtree sw wscale hscale)))
      scaled)))


(defun wg-resize-frame-scale-wtree (wconfig)
  "Set FRAME's size to WCONFIG's, returning a possibly scaled wtree.
If the frame size was set correctly, return WCONFIG's wtree
unchanged.  If it wasn't, return a copy of WCONFIG's wtree scaled
with `wg-scale-wconfigs-wtree' to fit the frame as it exists."
  (let ((frame (selected-frame)))
    (wg-with-slots wconfig ((wcwidth wg-wconfig-width)
                            (wcheight wg-wconfig-height))
      (when window-system (set-frame-size frame wcwidth wcheight))
      (let ((fwidth  (frame-parameter frame 'width))
            (fheight (frame-parameter frame 'height)))
        (if (and (= wcwidth fwidth) (= wcheight fheight))
            (wg-wconfig-wtree wconfig)
          (wg-scale-wconfigs-wtree wconfig fwidth fheight))))))


(defun wg-wtree-buf-uids (wtree)
  "Return a new list of the buf uids of all wins in WTREE."
  (if (not wtree)
      (error "WTREE is nil in `wg-wtree-buf-uids'!"))
  (wg-flatten-wtree wtree 'wg-win-buf-uid))

(defun wg-wtree-unique-buf-uids (wtree)
  "Return a list of the unique buf uids of all wins in WTREE."
  (cl-remove-duplicates (wg-wtree-buf-uids wtree) :test 'string=))




(defun wg-reset-window-tree ()
  "Delete all but one window in `selected-frame', and reset
various parameters of that window in preparation for restoring
a wtree."
  (delete-other-windows)
  (set-window-dedicated-p nil nil))

(defun wg-restore-window-tree-helper (w)
  "Recursion helper for `wg-restore-window-tree'."
  (if (wg-wtree-p w)
      (cl-loop with dir = (wg-wtree-dir w)
               for (win . rest) on (wg-wtree-wlist w)
               do (when rest (split-window nil (wg-w-size win dir) (not dir)))
               do (wg-restore-window-tree-helper win))
    (wg-restore-window w)
    (when (wg-win-selected w)
      (setq wg-window-tree-selected-window (selected-window)))
    (when (wg-win-minibuffer-scroll w)
      (setq minibuffer-scroll-window (selected-window)))
    (other-window 1)))

(defun wg-restore-window-tree (wtree)
  "Restore WTREE in `selected-frame'."
  (let ((window-min-width wg-window-min-width)
        (window-min-height wg-window-min-height)
        (wg-window-tree-selected-window nil))
    (wg-reset-window-tree)
    (wg-restore-window-tree-helper wtree)
    (awhen wg-window-tree-selected-window (select-window it))))


;; (wg-window-tree-to-wtree (window-tree))
(defun wg-window-tree-to-wtree (window-tree)
  "Return the serialization (a wg-wtree) of Emacs window tree WINDOW-TREE."
  (wg-barf-on-active-minibuffer)
  (cl-labels
      ((inner (w) (if (windowp w) (wg-window-to-win w)
                    (wg-dbind (dir edges . wins) w
                      (wg-make-wtree
                       :dir    dir
                       :edges  edges
                       :wlist  (mapcar #'inner wins))))))
    (let ((w (car window-tree)))
      (when (and (windowp w) (window-minibuffer-p w))
        (error "Workgroups can't operate on minibuffer-only frames."))
      (inner w))))


(defun wg-flatten-wtree (wtree &optional key)
  "Return a new list by flattening WTREE.
KEY non returns returns a list of WTREE's wins.
KEY non-nil returns a list of the results of calling KEY on each win."
  (cl-labels
      ((inner (w) (if (wg-win-p w) (list (if key (funcall key w) w))
                    (cl-mapcan #'inner (wg-wtree-wlist w)))))
    (inner wtree)))

(defun wg-reverse-wlist (w &optional dir)
  "Reverse W's wlist and those of all its sub-wtrees in direction DIR.
If DIR is nil, reverse WTREE horizontally.
If DIR is 'both, reverse WTREE both horizontally and vertically.
Otherwise, reverse WTREE vertically."
  (cl-labels
      ((inner (w) (if (wg-win-p w) w
                    (wg-with-slots w ((d1 wg-wtree-dir))
                      (wg-make-wtree
                       :dir d1
                       :edges (wg-wtree-edges w)
                       :wlist (let ((wl2 (mapcar #'inner (wg-wtree-wlist w))))
                                (if (or (eq dir 'both) (eq dir d1))
                                    (nreverse wl2)
                                  wl2)))))))
    (wg-normalize-wtree (inner w))))

(defun wg-wtree-move-window (wtree offset)
  "Offset `selected-window' OFFSET places in WTREE."
  (cl-labels
      ((inner (w) (if (wg-win-p w) w
                    (wg-with-slots w ((wlist wg-wtree-wlist))
                      (wg-make-wtree
                       :dir (wg-wtree-dir w)
                       :edges (wg-wtree-edges w)
                       :wlist (aif (cl-find t wlist :key 'wg-win-selected)
                                  (wg-cyclic-offset-elt it wlist offset)
                                (mapcar #'inner wlist)))))))
    (wg-normalize-wtree (inner wtree))))



(provide 'workgroups-wtree)
;;; workgroups-wtree.el ends here
