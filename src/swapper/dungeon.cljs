(ns swapper.dungeon)

(defrecord Tile [char blocks color])
(defrecord GameMap [tiles])
(defrecord Rect [x1 y1 x2 y2])

(def map-width 101)
(def map-height 51)

(def max-rooms 500)
(def room-min-height 5)
(def room-max-height 7)
(def room-min-width 7)
(def room-max-width 13)

(def cardinal-dirs [{:x 1 :y 0}
                    {:x 0 :y 1}
                    {:x -1 :y 0}
                    {:x 0 :y -1}])

(defn- room-overlaps? [room1 room2]
  (let [overlap (and (<= (:x1 room1) (:x2 room2))
                     (>= (:x2 room1) (:x1 room2))
                     (<= (:y1 room1) (:y2 room2))
                     (>= (:y2 room1) (:y1 room2)))]
    overlap))

(defn- can-place-room? [rooms room]
  (let [overlapping (filter (fn [other-room] (room-overlaps? room other-room)) rooms)]
    (= (count overlapping) 0)))

(defn- all-walls [width height]
  (vec (repeat height (vec (repeat width (->Tile "#" true "#777777"))))))

(defn- generate-random-room []
  (let [size (inc (* (inc (rand-int 10)) 2))
        rectangularity (* (rand-int (+ 1 (int (/ size 2)))) 2)
        extend-width? (< (rand) 0.5)
        width (if extend-width? (+ size rectangularity) size)
        height (if extend-width? size (+ size rectangularity))
        pos-x (inc (* (rand-int (/ (- map-width width) 2)) 2))
        pos-y (inc (* (rand-int (/ (- map-height height) 2)) 2))
        room (->Rect pos-x pos-y (+ pos-x width) (+ pos-y height))]
    room))

(defn carve [dungeon {x :x y :y} region-index]
  (assoc-in dungeon [:tiles y x] (->Tile "." false "#333333")))

(defn- carve-room [dungeon rect region-index]
  (reduce (fn [acc y]
                (reduce (fn [acc x]
                          (carve acc {:x x :y y} region-index))
                        acc
                        (range (:x1 rect) (:x2 rect))))
              dungeon
              (range (:y1 rect) (:y2 rect))))

(defn place-rooms [dungeon]
  (loop [count 0
         rooms []
         region-index (inc (:region-index dungeon))
         dungeon dungeon]
    (let [room (generate-random-room)
          can-place? (can-place-room? rooms room)
          region-index (if can-place? (inc region-index) region-index)
          dungeon (if can-place?
                    (carve-room dungeon room region-index)
                    dungeon)
          tiles (:tiles dungeon)]
      (if (= count max-rooms)
        (-> dungeon
            (assoc :tiles tiles)
            (assoc :rooms rooms)
            (assoc :region-index region-index))
        (recur (inc count)
               (if can-place? (conj rooms room) rooms)
               region-index
               (assoc dungeon :tiles tiles))))))

(defn add-pos [pos dir]
  {:x (+ (:x pos) (:x dir))
   :y (+ (:y pos) (:y dir))})

(defn carve-dir [tiles pos dir]
  (let [{bounds-x :x bounds-y :y} (add-pos (add-pos (add-pos pos dir) dir) dir)
        {x :x y :y} (add-pos (add-pos pos dir) dir)]
    (if (and (> bounds-x 0)
             (> bounds-y 0)
             (< bounds-x map-width)
             (< bounds-y map-height)
             (get-in tiles [y x :blocks]))
      dir
      nil)))

(defn grow-maze [{:keys [region-index] :as dungeon} x y]
  (loop [dungeon (carve dungeon {:x x :y y} region-index)
         cells [{:x x :y y}]]
    (if (empty? cells)
      dungeon
      (let [cell (peek cells)
            tiles (:tiles dungeon)
            unmade-cell-dirs (into [] (remove #(nil? (carve-dir tiles cell %)) cardinal-dirs))]
        (if (empty? unmade-cell-dirs)
          (recur dungeon (pop cells))
          (let [dir (rand-nth unmade-cell-dirs)
                next (add-pos cell dir)
                next-2 (add-pos next dir)
                dungeon (-> dungeon
                            (carve next region-index)
                            (carve next-2 region-index))
                cells (conj cells next-2)]
            (recur dungeon cells)))))))

(defn generate-maze [{tiles :tiles :as dungeon}]
  (let [range-x (filter odd? (range 1 map-width))
        range-y (filter odd? (range 1 map-height))]
    (reduce (fn [dungeon y]
              (reduce (fn [dungeon x]
                        (if (get-in dungeon [:tiles y x :blocks])
                          (grow-maze (assoc dungeon :region-index (inc (:region-index dungeon)))
                                     x y)
                          dungeon))
                      dungeon
                      range-x))
            dungeon
            range-y)))

(defn init-map [state]
  (let [dungeon (-> (hash-map :region-index -1 :region-vec [] :rooms [] :tiles (all-walls map-width map-height))
                    (place-rooms)
                    (generate-maze))]
    (-> state
        (assoc :game-map (->GameMap (:tiles dungeon)))
        (assoc :rooms (:rooms dungeon))
        (assoc :region-index (:region-index dungeon)))))
