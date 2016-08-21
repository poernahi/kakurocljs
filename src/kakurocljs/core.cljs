(ns kakurocljs.core
  (:require [reagent.core :as rg]))

;; 1. Solver

(def all-numbers
  "Allowable numbers in kakuro"
  #{1 2 3 4 5 6 7 8 9})

(def empty-board
  "Empty 10 by 10 board"
  (vec
   (repeat 10 (vec (repeat 10 0)))))

(def sum-smallest
  "Find the smallest possible sum of n unique [1-9] numbers.
  Memoized for performance."
  (memoize
   (fn [n]
     (reduce + (range (+ 1 n))))))

(def sum-largest
  "Find the largest possible sum of n unique [1-9] numbers.
  Memoized for performance."
  (memoize
   (fn [n]
     (reduce + (drop (- 10 n) (range 10))))))

(defn fast-contradiction?
  "Check for obvious contradictions."
  [box sum avail]
  (or (> (sum-smallest box) sum)
      (< (sum-largest box) sum)))

(defn count-open
  "Count number of open cells."
  [r b]
  (->> (:coord r)
       (filter #(zero? (get-in b %)))
       (count)))

(defn best-rule
  "Find rule with the least unfilled cell."
  ([rules board]
   (let [r (first rules)
         n-open (count-open r board)]
     (best-rule (disj rules r) board r n-open)))
  ([rules board best best-open]
   (if (or (empty? rules) (= best-open 0))
     best
     (let [r (first rules)
           n-open (count-open r board)]
       (if (< n-open best-open)
         (recur (disj rules r) board r n-open)
         (recur (disj rules r) board best best-open))))))

(defn best-coord
  "Given a set of coordinates, return a coordinate.
  Priority given to those who already has a number in the board."
  [coords board]
  (reduce #(if (zero? (get-in board %1))
             %2
             %1)
          coords))

(defn valid-last-empty-cell? [coord avail sum n]
  (and (= 1 (count coord))
       (contains? avail n)
       (= sum n)))

(defn valid-non-last-entry? [coord avail sum n]
  (and (> (count coord) 1)
       (contains? avail n)
       (> sum n)
       (not (fast-contradiction? (dec (count coord))
                                 (- sum n)
                                 (disj avail n)))))

(defn update-rule-after-insert
  "Update a rule after cell c is filled with number n.
  If rule is violated, returns :fail. If rule is completed, returns nil."
  [c n {:keys [coord sum avail] :as r}]
  (if (contains? coord c)
    (cond
      ;; Last empty box, match remaining sum, number available -> drop rule
      (valid-last-empty-cell? coord avail sum n) nil
      ;; Not the last box, remainder not 0, number available -> update rule
      (valid-non-last-entry? coord avail sum n) {:coord (disj coord c)
                                                 :sum (- sum n)
                                                 :avail (disj avail n)}
      :else :fail)
    ;; Change doesn't affect this rule
    r))

(defn enter-number
  "If an entry fits all rules, returns updated rules. Otherwise returns :fail."
  [rules c n]
  (let [upd-rules
        (->> rules
             (map (partial update-rule-after-insert c n))
             (filter (complement nil?)))]
    (if (some #{:fail} upd-rules)
      :fail
      (set upd-rules))))

(defn solve
  "Attempts to find one possible solution for a set of rules
  by searching through all possibilities. If a rule is broken, it will backtrack."
  ([rules]
   (trampoline solve rules nil empty-board all-numbers #(keyword :fail)))
  ([rules board fail]
   (trampoline solve rules nil board all-numbers fail))
  ([rules r board untried backtrack]
   (if (and (nil? r) (empty? rules))
     ;; All rules fulfilled!
     board
     (let [{:keys [coord sum avail] :as r} (or r (best-rule rules board))
           c (best-coord coord board)
           untried (or untried avail)]
       (if (= 1 (count coord))
         ;; Last box
         (if (contains? untried sum)
           (let [rules (enter-number rules c sum)]
             (if (= :fail rules)
               #(backtrack)
               (recur rules nil (assoc-in board c sum) nil backtrack)))
           #(backtrack))
         ;; Not last box
         (if-let [n (first untried)]
           (if (> sum n)
             (let [try-rules (enter-number rules c n)]
               (if (= :fail try-rules)
                 ;; Try another number
                 (recur rules r board (disj untried n) backtrack)
                 ;; Number fits, do next box.
                 (recur try-rules nil (assoc-in board c n) nil
                        ;; Backtrack, try next number
                        #(solve rules r board (disj untried n) backtrack))))
             ;; Number too big, try next number.
             (recur rules r board (disj untried n) backtrack))
           ;; Run out of numbers.
           #(backtrack)))))))

;; 2. User Interface

(def first-row-template
  (vec (repeat 10 {})))

(def row-template
  (-> (repeat 9 0)
      (conj {})
      (vec)))

(def board-template
  (-> (repeat 9 row-template)
      (conj first-row-template)
      (vec)))

(defonce ui-board
  (rg/atom board-template))

(defn transpose [m]
  (apply mapv vector m))

(def slash-background
  "SVG background. This should be in the CSS."
  ;; Yeah, embedding SVG inside url..
  (let [svg
        (-> (str "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'>"
                 "<line x1='0' y1='0' x2='100' y2='100' style='stroke:black;stroke-width:0.25em'/>"
                 "</svg>")
            ;; To have a valid URL, must escape characters.
            (clojure.string/replace "<" "%3C")
            (clojure.string/replace ">" "%3E"))]
    {:background (str "url(\"data:image/svg+xml," svg "\")")
     :backgroundColor "#aaa"}))

(defn sanitize-input
  "Hints entered should not be too large."
  [n]
  (let [num (js/parseInt (clojure.string/trim n))
        max-num (reduce + (range 10))]
    (if (and (int? num)
             (> num 0)
             (< num max-num))
      num
      nil)))

(defn clear-hint-left
  "Clear a hint cell to the left of current cell."
  [board rownum colnum]
  (if (number? (get-in board [rownum (dec colnum)]))
    board
    (assoc-in board [rownum (dec colnum)] {})))

(defn clear-hint-top
  "Clear a hint cell above current cell."
  [board rownum colnum]
  ; Not sure if doing double transpose cause performance hit?
  ; Don't care for now..
  (-> (transpose board)
      (clear-hint-left colnum rownum)
      (transpose)))

(defn update-hint-fn
  "Put a new value in a cell. Map represents a hit cell."
  [rownum colnum newval]
  (fn [board]
    (let [board (assoc-in board [rownum colnum] newval)]
      (if (number? newval)
        board
        (-> board
            (clear-hint-top rownum colnum)
            (clear-hint-left rownum colnum))))))

(defn cell-right-click-fn
  "Creates event listener function that assigns new value."
  [board rownum colnum newval]
  (fn [e]
    (.preventDefault e)
    (when-not (or (zero? rownum) (zero? colnum))
      (swap! board (update-hint-fn rownum colnum newval)))))

(defn show-hint? [board rownum colnum dir]
  (if (= :V dir)
    (recur (transpose board) colnum rownum :H)
    (number? (get-in board [rownum (inc colnum)]))))

(defn hint-change-fn
  "Creates event listener for when hint numbers are changed."
  [board rownum colnum dir]
  (fn [e]
    (let [newval (-> e .-target .-value sanitize-input)]
      (swap! board assoc-in [rownum colnum dir] newval))))

(defn render-hint [board cell rownum colnum style]
  (let [input-style {:height "100%" :width "2.5rem"
                     :margin "0" :border "solid 0 white"
                     :background "transparent" :font-size "1rem"
                     :padding-left "0.2rem" :padding-right "0.2rem"}]
    [:td {:style (conj style slash-background)
          :on-context-menu (cell-right-click-fn board rownum colnum 0)}
     [:div.top-hint {:style {:height "50%"}}
      (when (show-hint? @board rownum colnum :H)
        [:input {:size 2
                 :value (:H cell)
                 :style (assoc input-style :text-align "right")
                 :on-change (hint-change-fn board rownum colnum :H)}])]
     [:div.bot-hint {:style {:height "50%"}}
      (when (show-hint? @board rownum colnum :V)
        [:input {:size 2
                 :value (:V cell)
                 :style input-style
                 :on-change (hint-change-fn board rownum colnum :V)}])]]))

(defn render-cell [board cell rownum colnum]
  (let [style {:width "3rem" :height "3rem" :border "1px solid black" :text-align "center"}]
    (if (number? cell)
      [:td {:key (str rownum "." colnum)
            :style (assoc style :font-size "2.5em")
            :on-context-menu (cell-right-click-fn board rownum colnum {})}
       (if (zero? cell) " " cell)]
      ^{:key (str rownum "." colnum)} [render-hint board cell rownum colnum style])))

(defn render-row [board row rownum]
  [:tr {:key (str rownum)} (map render-cell (repeat board) row (repeat rownum) (range))])

(defn render-board [board]
  [:table {:cellPadding 0 :cellSpacing 0 :style {:border "1px solid black" :margin "0 auto"}}
   [:tbody (map render-row (repeat board) @board (range))]])

(defn on-js-reload []
  nil)

;; 3. UI -> Rules

(defn insert-coords [b]
  (mapv (fn [row rownum]
          (mapv (fn [cell colnum]
                  (if (number? cell)
                    [rownum colnum]
                    cell))
                row (range)))
        b (range)))

(defn segment->rule-fn [dir]
  (fn [segment]
    (let [sum (dir (last (first segment)))
          coords (set (last segment))]
      {:coord coords
       :sum sum
       :avail all-numbers})))

(defn row->rule
  ([row]
   (row->rule row :H))
  ([row dir]
   (->> row
        (partition-by map?)
        (partition 2)
        (map (segment->rule-fn dir)))))

(defn col->rule [col]
  (row->rule col :V))

(defn board->rule [b]
  (->> (concat
        (mapv row->rule (insert-coords b))
        (mapv col->rule (transpose (insert-coords b))))
       (flatten)
       (remove empty?)
       (set)))

;; 4. App

(defn click->solve []
  (let [puzzle-board @ui-board
        rules (board->rule puzzle-board)
        solved (solve rules puzzle-board #(puzzle-board))]
    (reset! ui-board solved)))

(defn render-app []
  [:div
   [:h1 {:style {:textAlign "center"}} "Kakuro Solver"]
   [render-board ui-board]
   [:button {:style {:display "block" :margin "2rem auto"}
             :on-click click->solve} "SOLVE"]])

(rg/render-component [render-app]
                     (. js/document (getElementById "app")))


;; 5. Tests

#_(defn make-rule
  "Convert rule from [sum :H/:V row col len] into coordinates."
  [r]
  (set (map
        (fn [[sum dir row col len]]
          {:coord (set (map (fn [n]
                              (cond
                                (= dir :H) [row (+ n col)]
                                (= dir :V) [(+ n row) col]))
                            (range len)))
           :sum sum
           :avail all-numbers})
        r)))

#_(defn solve-kakuro [r]
  (solve (make-rule r)))

#_(def kakuroconquest-28551
  [[10 :H 0 0 3]
   [15 :H 0 5 2]
   [14 :H 1 0 3]
   [12 :H 1 4 4]
   [35 :H 2 0 5]
   [8 :H 2 6 2]
   [16 :H 3 1 5]
   [32 :H 4 2 5]
   [15 :H 5 0 2]
   [29 :H 5 3 5]
   [12 :H 6 0 4]
   [19 :H 6 5 3]
   [10 :H 7 1 2]
   [9 :H 7 5 3]

   [9 :V 0 0 3]
   [9 :V 5 0 2]
   [30 :V 0 1 4]
   [10 :V 5 1 3]
   [27 :V 0 2 5]
   [12 :V 6 2 2]
   [33 :V 2 3 5]
   [15 :V 1 4 5]
   [13 :V 0 5 2]
   [32 :V 3 5 5]
   [14 :V 0 6 3]
   [11 :V 4 6 4]
   [4 :V 1 7 2]
   [17 :V 5 7 3]])

#_(def solution-28551
  [[2 7 1 0 0 8 7 0 0 0]
   [1 8 5 0 4 5 2 1 0 0]
   [6 9 8 7 5 0 5 3 0 0]
   [0 6 4 3 1 2 0 0 0 0]
   [0 0 9 8 3 7 5 0 0 0]
   [8 7 0 9 2 8 3 7 0 0]
   [1 2 3 6 0 9 2 8 0 0]
   [0 1 9 0 0 6 1 2 0 0]
   [0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0]])

#_(defn test-kakuro []
  (= (solve-kakuro kakuroconquest-28551) solution-28551))
