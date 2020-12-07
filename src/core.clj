(ns core.clj)

(defn get-inverse [x p]
  (inc (.indexOf
        (mapv #(mod (* x %) p)
              (range 1 p)) 1)))

(defn double-and-add [k P curve]
  ;; curve = (y^2 = x^3 + 4x + 20 mod 29) => [1 4 20 29]
  (let [a (second curve)
        p (last curve)
        fs (flatten
            (map
             (fn [c] (if (= c \1) [double-point add-point] double-point))
             (subs (Integer/toString k 2) 1)))]
    (loop [[f1 & f-rest] fs
           coord P
           cur-k 1]
      (let [print-kP (fn [] (println (str cur-k "P") coord))]
        (if (not f1)
          (do (print-kP)
              coord)
          (do (print-kP)
              (recur f-rest (apply
                             f1
                             (filter identity
                                     [coord
                                      (if (= f1 double-point) nil P)
                                      (calc-s f1 coord (if (= f1 double-point)
                                                         coord P) a p)
                                      p]))
                     (if (= f1 double-point) (* 2 cur-k) (inc cur-k)))))))))

(defn calc-s [f [x1 y1] [x2 y2] a p]
  (mod
   (if (= f double-point)
     (* (+ (* 3 x1 x1) a) (get-inverse (* 2 y2) p))
     (* (- y2 y1) (get-inverse (- x2 x1) p)))
   p))

(defn add-point [[x1 y1] [x2 y2] s p]
  (let [x3 (mod
            (- (* s s) x1 x2)
            p)
        y3 (mod
            (- (* s (- x1 x3)) y1)
            p)]
    [x3 y3]))

(defn double-point [[x1 y1] s p]
  (add-point [x1 y1] [x1 y1] s p))