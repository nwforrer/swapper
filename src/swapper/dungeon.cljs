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
  (vec (repeat height (vec (repeat width (->Tile "#" true "#333333"))))))

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

(defn- carve-room [tiles rect]
  (reduce (fn [acc y]
                (reduce (fn [acc x]
                          (assoc-in acc [y x] (->Tile "." false "#FFFFFF")))
                        acc
                        (range (:x1 rect) (:x2 rect))))
              tiles
              (range (:y1 rect) (:y2 rect))))

(defn place-rooms [tiles]
  (loop [count 0
         rooms []
         connecting-points []
         tiles tiles]
    (let [room (generate-random-room)
          tiles (if (can-place-room? rooms room)
                  (carve-room tiles room)
                  tiles)]
      (if (= count max-rooms)
        ;[tiles rooms]
        tiles
        (recur (inc count)
               (conj rooms room)
               connecting-points
               tiles)))))

(defn add-pos [pos dir]
  {:x (+ (:x pos) (:x dir))
   :y (+ (:y pos) (:y dir))})

(defn can-carve [tiles pos dir]
  (let [{bounds-x :x bounds-y :y} (add-pos (add-pos (add-pos pos dir) dir) dir)
        {x :x y :y} (add-pos (add-pos pos dir) dir)]
    (if (and (> bounds-x 0)
             (> bounds-y 0)
             (< bounds-x map-width)
             (< bounds-y map-height)
             (get-in tiles [y x :blocks]))
      dir
      nil)))

(defn carve [tiles {x :x y :y}]
  (assoc-in tiles [y x] (->Tile "." false "#FF0000")))

(defn grow-maze [tiles x y]
  (loop [tiles (carve tiles {:x x :y y})
         cells [{:x x :y y}]]
    (if (empty? cells)
      tiles
      (let [cell (peek cells)
            unmade-cell-dirs (into [] (remove #(nil? (can-carve tiles cell %)) cardinal-dirs))]
        (if (empty? unmade-cell-dirs)
          (recur tiles (pop cells))
          (let [dir (rand-nth unmade-cell-dirs)
                next (add-pos cell dir)
                next-2 (add-pos next dir)
                tiles (-> tiles
                          (carve next)
                          (carve next-2))
                cells (conj cells next-2)]
            (recur tiles cells)))))))

(defn generate-maze [tiles]
  (let [new-tiles (atom tiles)]
    (doseq [row (range 1 (/ (count tiles) 2))
            col (range 1 (/ (count (get tiles row)) 2))
            :let [x (dec (* col 2))
                  y (dec (* row 2))]]
      (when (get-in @new-tiles [y x :blocks])
        (swap! new-tiles grow-maze x y)))
    @new-tiles))

(defn init-map [state]
  (let [tiles (-> (all-walls map-width map-height)
                  (place-rooms)
                  (generate-maze))]
    (-> state
        (assoc :game-map (->GameMap tiles))
        ;; (assoc :rooms rooms)
        )))
