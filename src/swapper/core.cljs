(ns swapper.core
  (:require [brute.entity :as e]
            [cljsjs.howler]
            [cljs-time.core :as dt]
            [cljs-time.coerce :as dtc]))

(enable-console-print!)

(defonce *state* (atom nil))
(defonce *key-state* (atom nil))

(def tile-size 25)
(def map-size 40)

(defrecord Name [name])
(defrecord Position [x y])
(defrecord Visible [char color])
(defrecord Controlled [])
(defrecord LastActionTimer [delay last-time])
(defrecord Health [health max-health])

(defrecord Tile [char blocks])
(defrecord GameMap [tiles])

(defn init-ecs [state]
  (e/create-system))

(defn init-render-context [state]
  (let* [canvas (.getElementById js/document "game-canvas")
         ctx (.getContext canvas "2d")]
    (set! (.-font ctx) "25px courier, inconsolata, monospace")
    (-> state
        (assoc-in [:renderer :ctx] ctx)
        (assoc-in [:renderer :canvas] canvas))))

(defn init-sounds [state]
  (let [sound (js/Howl. (clj->js {:src ["sound/Subterranians.mp3"]
                                  ;:autoplay true
                                  }))]
    (assoc state :music sound)))

(defn init-player [state]
  (let [player (e/create-entity)]
    (-> state
        (e/add-component player (->Controlled))
        (e/add-component player (->LastActionTimer 200 (atom 0)))
        (e/add-component player (->Position (atom 10) (atom 10)))
        (e/add-component player (->Health (atom 3) (atom 3)))
        (e/add-component player (->Name "Player"))
        (e/add-component player (->Visible "@" "#FF0000")))))

(defn init-enemy [state name char color x y]
  (let [enemy (e/create-entity)]
    (-> state
        (e/add-component enemy (->Position (atom x) (atom y)))
        (e/add-component enemy (->Name name))
        (e/add-component enemy (->Health (atom 2) (atom 2)))
        (e/add-component enemy (->Visible char color)))))

(defn init-map [state]
  (let [tiles (vec (repeat map-size (vec (repeat map-size (->Tile "#" true)))))]
    (as-> tiles tiles
      (reduce (fn [acc y]
                (reduce (fn [acc x]
                          (assoc-in acc [y x] (->Tile "." false)))
                        acc
                        (range 5 15)))
              tiles
              (range 5 15))
      (assoc state :game-map (->GameMap tiles)))))

(defn clear-screen [state]
  (let [ctx (get-in state [:renderer :ctx])
        canvas (get-in state [:renderer :canvas])]
    (set! (.-fillStyle ctx) "#000")
    (.fillRect ctx 0 0 (.-width canvas) (.-height canvas)))
  state)

(defn render-map [state]
  (let [ctx (get-in state [:renderer :ctx])]
    (set! (.-fillStyle ctx) "#333333")
    (doseq [y (range map-size)
            x (range map-size)]
      (let [tiles (get-in state [:game-map :tiles])
            tile (get-in tiles [y x])
            x-pos (* x tile-size)
            y-pos (* y tile-size)]
        (.fillText ctx (:char tile) x-pos y-pos))))
  state)

(defn render-entities [state]
  (let [ctx (get-in state [:renderer :ctx])]
    (set! (.-textBaseline ctx) "top")
    (doseq [entity (e/get-all-entities-with-component state Visible)]
      (let* [glyph (e/get-component state entity Visible)
             position (e/get-component state entity Position)
             x (* @(:x position) tile-size)
             y (* @(:y position) tile-size)]
        (set! (.-fillStyle ctx) "#000")
        (.fillRect ctx x y tile-size tile-size)
        (set! (.-fillStyle ctx) (:color glyph))
        (.fillText ctx (:char glyph) x y))))
  state)

(defn move-position! [state entity delta]
  (let* [position (e/get-component state entity Position)
         x (:x position)
         y (:y position)]
    (swap! x + (:x delta))
    (swap! y + (:y delta)))
  state)

(defn attack! [state attacker defender]
  (let [defender-health (e/get-component state defender Health)
        attacker-name (e/get-component state attacker Name)
        defender-name (e/get-component state defender Name)]
    (swap! (:health defender-health) dec)
    (println (:name attacker-name) "attacks" (:name defender-name))
    (cond
      (= @(:health defender-health) 0)
      (do
        (println (:name defender-name) "has died.")
        (e/kill-entity state defender))

      :else
      (do
        (println "remaining health:" @(:health defender-health))
        state))))

(defn move-or-attack [state entity delta]
  (let* [position (e/get-component state entity Position)
         dest-x (+ @(:x position) (:x delta))
         dest-y (+ @(:y position) (:y delta))
         attack-entity (atom nil)
         can-move? (atom true)]
    (doseq [other-entity (e/get-all-entities-with-component state Visible)]
      (when (not= other-entity entity)
        (let [other-pos (e/get-component state other-entity Position)]
          (when (and (= @(:x other-pos) dest-x) (= @(:y other-pos) dest-y))
            (reset! can-move? false)
            (reset! attack-entity other-entity)))))

    ;; if we didn't collide with an entity, check for collisions on map
    (when (nil? @attack-entity)
      (let* [tiles (get-in state [:game-map :tiles])
             tile-at-pos (get-in tiles [dest-y dest-x])]
        (if (:blocks tile-at-pos)
          (reset! can-move? false))))

    (cond
      @attack-entity
      (attack! state entity @attack-entity)
      
      @can-move?
      (move-position! state entity delta)

      :else
      state)))

(defn handle-input [state]
  (let [new-state (atom state)]
    (doseq [entity (e/get-all-entities-with-component state Controlled)]
      (let [last-action-timer (e/get-component state entity LastActionTimer)]
        (when (> (- (dtc/to-long (dt/now)) @(:last-time last-action-timer)) (:delay last-action-timer))
          (reset! (:last-time last-action-timer) (dtc/to-long (dt/now)))
          (cond
            (get @*key-state* 37)
            (reset! new-state (move-or-attack state entity {:x -1 :y 0}))

            (get @*key-state* 39)
            (reset! new-state (move-or-attack state entity {:x 1 :y 0}))

            (get @*key-state* 40)
            (reset! new-state (move-or-attack state entity {:x 0 :y 1}))

            (get @*key-state* 38)
            (reset! new-state (move-or-attack state entity {:x 0 :y -1}))))))
    @new-state))

(defn keydown [event]
  (swap! *key-state* assoc (.-keyCode event) true))

(defn keyup [event]
  (swap! *key-state* dissoc (.-keyCode event)))

(defn update-game [state]
  (-> state
      (handle-input)
      (clear-screen)
      (render-map)
      (render-entities)))

(defn game-loop [state]
  (as-> state state
    (update-game state)
    (reset! *state* state)
    (.requestAnimationFrame js/window #(game-loop state))))

(defn startup []
  (-> {}
      (init-ecs)
      (init-render-context)
      (init-sounds)
      (init-map)
      (init-player)
      (init-enemy "Kobold" "k" "#00FF00" 7 7)
      (game-loop))
  (set! (.-onkeydown js/document) keydown)
  (set! (.-onkeyup js/document) keyup))

(set! (.-onload js/window) startup)


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
