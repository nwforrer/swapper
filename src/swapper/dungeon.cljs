(ns swapper.dungeon)

(defrecord Tile [char blocks])
(defrecord GameMap [tiles])
(defrecord Rect [x1 y1 x2 y2])

(def map-size 20)

(def max-rooms 50)
(def room-min-size 10)
(def room-max-size 15)

(defn- room-overlaps? [room1 room2]
  (let [overlap (and (< (:x1 room1) (:x2 room2))
                     (> (:x2 room1) (:x1 room2))
                     (< (:y1 room1) (:y2 room2))
                     (> (:y2 room1) (:y1 room2)))]
    overlap))

(defn- can-place-room? [rooms room]
  (let [overlapping (filter (fn [other-room] (room-overlaps? room other-room)) rooms)]
    (= (count overlapping) 0)))

(defn- all-walls [size]
  (vec (repeat size (vec (repeat size (->Tile "#" true))))))

(defn- generate-random-room []
  (let [width (+ (rand-int (- room-max-size room-min-size)) room-min-size)
        height (+ (rand-int (- room-max-size room-min-size)) room-min-size)
        pos-x (rand-int (- map-size width))
        pos-y (rand-int (- map-size height))
        room (->Rect pos-x pos-y (+ pos-x width) (+ pos-y height))]
    room))

(defn- carve-room [tiles rect]
  (reduce (fn [acc y]
                (reduce (fn [acc x]
                          (assoc-in acc [y x] (->Tile "." false)))
                        acc
                        (range (inc (:x1 rect)) (dec (:x2 rect)))))
              tiles
              (range (inc (:y1 rect)) (dec (:y2 rect)))))

(defn init-map [state]
  (loop [count 0
         rooms []
         connecting-points []
         tiles (all-walls map-size)]
    (let [room (generate-random-room)
          tiles (if (can-place-room? rooms room)
                  (carve-room tiles room)
                  tiles)]
      (if (= count max-rooms)
        (-> state
            (assoc :game-map (->GameMap tiles))
            (assoc :rooms rooms))
        (recur (inc count)
               (conj rooms room)
               connecting-points
               tiles)))))
