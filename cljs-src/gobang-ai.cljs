(ns AI
  (:require [clojure.browser.repl :as repl]))
;(repl/connect "http://localhost:9000/repl")

; A simple Gobang AI

(defn l [args] 
  (.log js/console args))

(defonce max-list
  (take 8 (iterate #(/ % 8) (inc 0x7fffffff))))
;(l (str max-list))

(defn pp [n] (l (str n)) n)

(defn switch-player [crt-player] 
  (- 1 crt-player))

(defn -gen-rules [crt-player]
  (let [crt crt-player
        otr (switch-player crt)]
    {[crt 5 0] (nth max-list 0)
     [crt 5 1] (nth max-list 0)
     [crt 5 2] (nth max-list 0)
     [otr 5 0] (- (nth max-list 1)) 
     [otr 5 1] (- (nth max-list 1)) 
     [otr 5 2] (- (nth max-list 1)) 
     [crt 4 1] (nth max-list 2)
     [crt 4 2] (nth max-list 2)
     [otr 4 2] (- (nth max-list 3))
     [crt 3 2] (nth max-list 4)
     [otr 3 2] (- (nth max-list 5))
     [otr 4 1] (- (nth max-list 5))
     [crt 2 2] (nth max-list 6)
     [otr 2 2] (- (nth max-list 6))
     [crt 2 1] (nth max-list 7)
     [otr 2 1] (- (nth max-list 7))}))
(def gen-rules (memoize -gen-rules))

(defn gen-cells [grid]
  (let [row (count grid)
        col (count (first grid))
        rrange (range 0 row)
        crange (range 0 col)
        dirs [[1 -1] [0 1] [1 0] [1 1]
              [-1 -1] [0 -1] [-1 0] [-1 1]]
        move #(map + %1 %2)
        all-nbs (fn [v] 
                  (map 
                    #(get-in grid (move v %)) 
                    dirs))]
    (->>
      (for [i rrange j crange]
        [i j])
      (filter #(not-every? 
                 (fn [v] 
                   (or (= -1 v) (nil? v))) 
                 (all-nbs %))))))

(defn get-row [grid dir pos]
  (if-let [v (get-in grid pos)]
    (cons v (get-row grid dir (map + pos dir)))
    [])) 

(defn create-or-inc [m ks]
  (if (contains? m ks)
    (update-in m ks inc)
    (assoc-in m ks 1)))

(defn scan-row [grid dir org]
  (let [row (get-row grid dir org)
        row-groups (vec (map vec (partition-by identity row)))]
    (loop [idx 0
           ret {}]
      (let [grp (get row-groups idx)
            n (count grp)
            crt (first grp)
            neib [(inc idx) (dec idx)]
            head (count 
                   (filter #(= -1 (get-in row-groups [% 0])) neib))]
        (cond 
          (nil? grp)
          ret
          (or (= -1 crt) (< n 2))
          (recur (inc idx) ret)
          :else
          (recur (inc idx) 
                 (create-or-inc ret [crt n head])))))))

(defn deep [m1 m2]
  (if (and (map? m1) (map? m2))
    (merge-with deep m1 m2)
    (+ m1 m2)))

(defn -gen-orgs+dirs [row col]
  (concat 
    (map #(vector [0 %] [[1 0] [1 1] [1 -1]]) 
         (range 0 col))
    (map #(vector [(dec row) %] [[-1 1] [-1 -1]]) (range 0 col))
    (map #(vector [% 0] [[0 1]]) (range 0 row))))
(def gen-orgs+dirs (memoize -gen-orgs+dirs))

(defn calc-state [grid]
  (let [row (count grid)
        col (count (first grid))
        orgs+dirs (gen-orgs+dirs row col)]
    (->>
      (for [[org dirs] orgs+dirs dir dirs]
        (scan-row grid dir org))
      (apply merge-with deep))))

(defn chance [grid crt-player]
  (let [state (calc-state grid)
        rules (gen-rules crt-player)]
    (->> 
      (map 
        (fn [[rule v]]
          (if-let [n (get-in state rule)]
            (* n v)
            0)) 
        rules)
      (apply +))))

; Return the max of min chance and corresponding movement
; Note if depth == 0 then movement will be undefined
(defn think [grid crt-player depth max-chance hot-move]
  (if (zero? depth)
    [(chance grid crt-player) nil]
    (loop [[cell & rest-cells] (filter #(= -1 (get-in grid %)) 
                                       (if hot-move 
                                         (cons hot-move (gen-cells grid))
                                         (gen-cells grid)))
           [cs move :as ret] [-0x7fffffff nil]
           key-move nil] 
      (cond
        (nil? cell) ret
        (and max-chance (>= cs (* 0.75 max-chance)))
        (do
          (l "Go prunning")
          [0x7fffffff cell])
        :else
        (let [newgrid (assoc-in grid cell crt-player)
              enm-ret (think newgrid (switch-player crt-player) (dec depth) (- cs) key-move)
              enm-cs ((comp - first) enm-ret)
              enm-mv (second enm-ret)]
          (recur rest-cells
            (max-key first ret 
              [enm-cs cell])
            enm-mv))))))

(defn vec->array [v] 
  (apply array v))

(defn ^:export play 
  ([grid crt-player]
   (play grid crt-player 2))
  ([grid crt-player depth] 
   (let [vec-grid (vec (map vec (vec grid)))
         ret->js #(vec->array (vector %1 (vec->array %2)))]
     (apply ret->js (think vec-grid crt-player depth nil nil)))))
