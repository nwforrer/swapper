(ns swapper.core
  (:require [brute.entity :as e]
            [cljsjs.howler]
            [cljs-time.core :as dt]
            [cljs-time.coerce :as dtc]))

(enable-console-print!)

(defonce *state* (atom nil))
(defonce *key-state* (atom nil))
(defonce *input-queue* (atom {}))

(def tile-width 16)
(def tile-height 25)
(def map-size 40)

(defrecord Name [name])
(defrecord Position [x y])
(defrecord Visible [char base-color color])
(defrecord Controlled [input-state])
(defrecord LastActionTimer [delay last-time])
(defrecord Health [health max-health])
(defrecord AbilitiesState [state])
(defrecord SwapAttack [])
(defrecord MeleeAttack [damage])
(defrecord AbilitiesToSteal [abilities])

(defrecord Tile [char blocks])
(defrecord GameMap [tiles])

(defn remove-component [system entity instance]
  (let [type (e/get-component-type instance)
        system (transient system)
        entity-components (:entity-components system)
        entity-component-types (:entity-component-types system)]
    (-> system
        (assoc! :entity-components (assoc entity-components type (-> entity-components (get type) (dissoc entity))))
        (assoc! :entity-component-types (assoc entity-component-types entity (as-> entity-component-types types (get types entity) (remove #{type} types))))
        persistent!)))

(defn replace-component [state entity type fn & args]
  (if-let [update (apply fn (e/get-component state entity type) args)]
    (let [old-component (e/get-component state entity type)]
      (-> state
          (remove-component entity old-component)
          (e/add-component entity update)))))

(defn get-tile-pos-from-pixel [pixel-pos]
  (hash-map :x (.floor js/Math (/ (:x pixel-pos) tile-width))
            :y (.floor js/Math (/ (:y pixel-pos) tile-height))))

(defn get-cursor-pos [canvas event]
  (let [rect (.getBoundingClientRect canvas)
        x (- (.-clientX event) (.-left rect))
        y (- (.-clientY event) (.-top rect))]
    (hash-map :x x :y y)))

(defn get-entities-at-pos [state pos]
  (filter (fn [entity]
            (let [position (e/get-component state entity Position)]
              (and (= (:x position) (:x pos)) (= (:y position) (:y pos)))))
          (e/get-all-entities-with-component state Visible)))

(defn init-ecs [state]
  (e/create-system))

(defn init-render-context [state]
  (let* [canvas (.getElementById js/document "game-canvas")
         ctx (.getContext canvas "2d")]
    (set! (.-font ctx) "25px courier, inconsolata, monospace")
    (set! (.-tabIndex canvas) 1)
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
        (e/add-component player (->Controlled :normal))
        (e/add-component player (->LastActionTimer 100 0))
        (e/add-component player (->Position 10 10))
        (e/add-component player (->Health 3 3))
        (e/add-component player (->AbilitiesState :none))
        (e/add-component player (->Name "Player"))
        (e/add-component player (->Visible "@" "#FF0000" "#FF0000"))
        (e/add-component player (->SwapAttack)))))

(defn init-enemy [state name char color x y]
  (let [enemy (e/create-entity)]
    (-> state
        (e/add-component enemy (->Position x y))
        (e/add-component enemy (->Name name))
        (e/add-component enemy (->Health 2 2))
        (e/add-component enemy (->MeleeAttack 1))
        (e/add-component enemy (->AbilitiesToSteal [MeleeAttack]))
        (e/add-component enemy (->Visible char color color)))))

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
            x-pos (* x tile-width)
            y-pos (* y tile-height)]
        (.fillText ctx (:char tile) x-pos y-pos))))
  state)

(defn render-entities [state]
  (let [ctx (get-in state [:renderer :ctx])]
    (set! (.-textBaseline ctx) "top")
    (doseq [entity (e/get-all-entities-with-component state Visible)]
      (let* [glyph (e/get-component state entity Visible)
             position (e/get-component state entity Position)
             x (* (:x position) tile-width)
             y (* (:y position) tile-height)]
        (set! (.-fillStyle ctx) "#000")
        (.fillRect ctx x y tile-width tile-height)
        (set! (.-fillStyle ctx) (:color glyph))
        (.fillText ctx (:char glyph) x y))))
  state)

(defn set-abilities-state [state entity new-state]
  (println "set abilities state" new-state)
  (let [controlled (e/get-component state entity Controlled)]
    (replace-component state entity AbilitiesState #(assoc % :state new-state))))

(defn move-position [state entity delta]
  (let* [position (e/get-component state entity Position)
         x (:x position)
         y (:y position)]
    (replace-component state entity Position
                       (fn [position & args]
                         (-> position (assoc :x (+ x (:x delta))) (assoc :y (+ y (:y delta))))))))

(defn attack [state attacker defender]
  (let [attacker-name (e/get-component state attacker Name)
        defender-name (e/get-component state defender Name)
        attack (e/get-component state attacker MeleeAttack)]
    (println (:name attacker-name) "attacks" (:name defender-name) "for" (:damage attack) "damage")
    (as-> state state
      (replace-component state defender Health (fn [health] (assoc health :health
                                                                   (- (:health health) (:damage attack)))))
        
      (let [defender-health (e/get-component state defender Health)]
        (cond
          (<= (:health defender-health) 0)
          (do
            (println (:name defender-name) "has died.")
            (e/kill-entity state defender))
          
          :else
          (do
            (println "remaining health:" (:health defender-health))
            state))))))

(defn swap [state initiator target]
  (let [abilities (e/get-component state target AbilitiesToSteal)
        new-state (atom state)]
    (println "swapping" abilities "abilities from" target)
    (doseq [ability-component (:abilities abilities)]
      (let [ability (e/get-component @new-state target ability-component)]
        (reset! new-state (-> @new-state
                              (remove-component target ability)
                              (e/add-component initiator ability)))))
    @new-state))

(defn move-or-attack [state entity delta]
  (let* [position (e/get-component state entity Position)
         dest-x (+ (:x position) (:x delta))
         dest-y (+ (:y position) (:y delta))
         abilities-state (e/get-component state entity AbilitiesState)
         attack-entity (atom nil)
         can-move? (atom true)]
    (doseq [other-entity (get-entities-at-pos state {:x dest-x :y dest-y})]
      (when (not= other-entity entity)
        (let [other-pos (e/get-component state other-entity Position)]
          (reset! can-move? false)
          (reset! attack-entity other-entity))))

    ;; if we didn't collide with an entity, check for collisions on map
    (when (nil? @attack-entity)
      (let* [tiles (get-in state [:game-map :tiles])
             tile-at-pos (get-in tiles [dest-y dest-x])]
        (if (:blocks tile-at-pos)
          (reset! can-move? false))))

    (cond
      @attack-entity
      (if (= (:state abilities-state) :swap)
        (swap state entity @attack-entity)
        (attack state entity @attack-entity))
      
      @can-move?
      (move-position state entity delta)

      :else
      state)))

(defn process-input-queue-actions [state entity abilities-state]
  (let [new-state (atom state)]
    (cond
      (= (get @*input-queue* 83) :down)
      (reset! new-state
              (if (= (:state abilities-state) :swap)
                (set-abilities-state @new-state entity :none)
                (set-abilities-state @new-state entity :swap)))
      
      (= (get @*input-queue* 37) :down)
      (reset! new-state (move-or-attack @new-state entity {:x -1 :y 0}))

      (= (get @*input-queue* 39) :down)
      (reset! new-state (move-or-attack @new-state entity {:x 1 :y 0}))

      (= (get @*input-queue* 40) :down)
      (reset! new-state (move-or-attack @new-state entity {:x 0 :y 1}))

      (= (get @*input-queue* 38) :down)
      (reset! new-state (move-or-attack @new-state entity {:x 0 :y -1})))

    @new-state))

(defn default-input [state entity]
  (let [new-state (atom state)]
    (let [last-action-timer (e/get-component state entity LastActionTimer)
          abilities-state (e/get-component state entity AbilitiesState)]
      (when (>= (- (dtc/to-long (dt/now)) (:last-time last-action-timer)) (:delay last-action-timer))
        (reset! new-state (replace-component state entity LastActionTimer
                                             (fn [last-action-timer]
                                               (assoc last-action-timer :last-time (dtc/to-long (dt/now))))))
        (cond
          (get @*key-state* 83)
          (reset! new-state
                  (if (= (:state abilities-state) :swap)
                    (set-abilities-state @new-state entity :none)
                    (set-abilities-state @new-state entity :swap)))
          
          (get @*key-state* 37)
          (reset! new-state (move-or-attack @new-state entity {:x -1 :y 0}))

          (get @*key-state* 39)
          (reset! new-state (move-or-attack @new-state entity {:x 1 :y 0}))

          (get @*key-state* 40)
          (reset! new-state (move-or-attack @new-state entity {:x 0 :y 1}))

          (get @*key-state* 38)
          (reset! new-state (move-or-attack @new-state entity {:x 0 :y -1}))

          :else
          (reset! new-state (process-input-queue-actions @new-state entity abilities-state)))
        (reset! *input-queue* {})))
    @new-state))

(defn swap-input [state entity]
  state)

(defn handle-input [state]
  (let [new-state (atom state)]
    (doseq [entity (e/get-all-entities-with-component state Controlled)]
      (let [controlled (e/get-component state entity Controlled)]
        (case (:input-state controlled)
          :normal (reset! new-state (default-input state entity))

          :swap (reset! new-state (swap-input state entity)))))
    @new-state))

(defn keydown [event]
  (swap! *key-state* assoc (.-keyCode event) true)
  (swap! *input-queue* assoc (.-keyCode event) :down)
  (.preventDefault event))

(defn keyup [event]
  (swap! *key-state* dissoc (.-keyCode event)))

(defn click-event [event]
  (println "clicked" ))

(defn update-game [state]
  (-> state
      (handle-input)
      (clear-screen)
      (render-map)
      (render-entities)))

(defn game-loop [state timestamp]
  (as-> state state
    (update-game state)
    (reset! *state* state)
    (.requestAnimationFrame js/window #(game-loop state %))))

(defn startup []
  (let* [state (-> {}
                   (init-ecs)
                   (init-render-context)
                   (init-sounds)
                   (init-map)
                   (init-player)
                   (init-enemy "Kobold" "k" "#00FF00" 7 7)
                   (init-enemy "Goblin" "g" "#0000FF" 11 13))
         canvas (get-in state [:renderer :canvas])]
    (game-loop state 0)
    (.addEventListener canvas "click" click-event))
  (set! (.-onkeydown js/document) keydown)
  (set! (.-onkeyup js/document) keyup))

(set! (.-onload js/window) startup)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
