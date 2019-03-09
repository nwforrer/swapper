(ns swapper.core
  (:require [brute.entity :as e]
            [cljsjs.howler]
            [cljs-time.core :as dt]
            [cljs-time.coerce :as dtc])
  (:use [swapper.dungeon :only [init-map map-width map-height]]))

(enable-console-print!)

;;
;; Entire state of game stored in the following structure:
;; {
;;   :entity-components
;;   :entity-component-types
;;   :current-entity                ; entity that is currently taking it's turn
;;   :renderer                      ; stores canvas and context for rendering
;;   :music                         ; stores Howler object for music
;;   :game-map                      ; stores the map tiles as a 2d vector in :tiles
;;   :rooms                         ; a vector of Rects representing the generated rooms
;;   :messages                      ; a vector of messages to be displayed
;;   :entity-list                   ; a vector of entities to display in the ui
;; }
;;

(defonce *state* (atom nil))
(defonce *key-state* (atom nil))
(defonce *input-queue* (atom {}))

(defonce *tile-width* (atom nil))
(def tile-height 14)

(def max-messages 7)

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
(defrecord BasicAI [target-component])

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
  (hash-map :x (.floor js/Math (/ (:x pixel-pos) @*tile-width*))
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
    (set! (.-font ctx) (str tile-height "px courier, inconsolata, monospace"))
    (set! (.-tabIndex canvas) 1)
    (reset! *tile-width* (.ceil js/Math (.-width (.measureText ctx "m"))))
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
        (e/add-component enemy (->BasicAI Controlled))
        (e/add-component enemy (->Health 2 2))
        (e/add-component enemy (->MeleeAttack 1))
        (e/add-component enemy (->AbilitiesToSteal [MeleeAttack]))
        (e/add-component enemy (->Visible char color color)))))

(defn clear-screen [state]
  (let [ctx (get-in state [:renderer :ctx])
        canvas (get-in state [:renderer :canvas])]
    (set! (.-fillStyle ctx) "#000")
    (.fillRect ctx 0 0 (.-width canvas) (.-height canvas)))
  state)

(defn render-map [state]
  (let [ctx (get-in state [:renderer :ctx])]
    (doseq [y (range map-height)
            x (range map-width)]
      (let [tiles (get-in state [:game-map :tiles])
            tile (get-in tiles [y x])
            x-pos (* x @*tile-width*)
            y-pos (* y tile-height)]
        (set! (.-fillStyle ctx) (:color tile))
        (.fillText ctx (:char tile) x-pos y-pos))))
  state)

(defn render-entities [state]
  (let [ctx (get-in state [:renderer :ctx])]
    (set! (.-textBaseline ctx) "top")
    (doseq [entity (e/get-all-entities-with-component state Visible)]
      (let* [glyph (e/get-component state entity Visible)
             position (e/get-component state entity Position)
             x (* (:x position) @*tile-width*)
             y (* (:y position) tile-height)]
        (set! (.-fillStyle ctx) "#000")
        (.fillRect ctx x y @*tile-width* tile-height)
        (set! (.-fillStyle ctx) (:color glyph))
        (.fillText ctx (:char glyph) x y))))
  state)

(defn render-entity-list [state]
  (let [ctx (get-in state [:renderer :ctx])
        entities (e/get-all-entities-with-component state Name)]
    (set! (.-fillStyle ctx) "#FFF")
    (doseq [[entity index] (map vector entities (range))
            :let [name (:name (e/get-component state entity Name))]]
      (.fillText ctx name 840 (+ 200 (* index (+ tile-height 5))))))
  state)

(defn render-ability-status [state]
  (let [ctx (get-in state [:renderer :ctx])
        entity (first (e/get-all-entities-with-component state AbilitiesState))
        abilities-state (e/get-component state entity AbilitiesState)]
    (set! (.-fillStyle ctx) "#FFF")
    (.fillText ctx (str "Swap state " (:state abilities-state)) 450 650))
  state)

(defn render-messages [{:keys [messages] :as state}]
  (let [left 40
        top 650
        ctx (get-in state [:renderer :ctx])]
    (set! (.-fillStyle ctx) "#FFF")
    (doseq [[message index] (map vector messages (range))]
      (.fillText ctx message left (+ top (* index tile-height)))))
  state)

(defn add-message [state message]
  (let [state (assoc state :messages (into [] (conj (:messages state) message)))]
    (if (> (count (:messages state)) max-messages)
      (assoc state :messages (into [] (rest (:messages state))))
      state)))

(defn set-abilities-state [state entity new-state]
  (let [controlled (e/get-component state entity Controlled)]
    (-> state
     (replace-component entity AbilitiesState #(assoc % :state new-state))
     (add-message (str "set abilities state" new-state)))))

(defn move-position [state entity delta]
  (let* [position (e/get-component state entity Position)
         name (e/get-component state entity Name)
         x (:x position)
         y (:y position)]
    (-> state
        (replace-component entity Position
                           (fn [position & args]
                             (-> position (assoc :x (+ x (:x delta))) (assoc :y (+ y (:y delta))))))
        (add-message (str (:name name) " moved " delta)))))

(defn attack [state attacker defender]
  (let [attacker-name (e/get-component state attacker Name)
        defender-name (e/get-component state defender Name)
        attack (e/get-component state attacker MeleeAttack)]
    (as-> state state
      (replace-component state defender Health (fn [health] (assoc health :health
                                                                   (- (:health health) (:damage attack)))))
      (add-message state (str (:name attacker-name) " attacks " (:name defender-name) " for " (:damage attack) " damage"))
        
      (let [defender-health (e/get-component state defender Health)]
        (cond
          (<= (:health defender-health) 0)
          (do
            (-> state
                (e/kill-entity defender)
                (add-message (str (:name defender-name) " has died"))))
          
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
  (let [state (cond
                (= (get @*input-queue* 83) :down)
                (if (= (:state abilities-state) :swap)
                  (set-abilities-state state entity :none)
                  (set-abilities-state state entity :swap))
                
                (= (get @*input-queue* 37) :down)
                (move-or-attack state entity {:x -1 :y 0})

                (= (get @*input-queue* 39) :down)
                (move-or-attack state entity {:x 1 :y 0})

                (= (get @*input-queue* 40) :down)
                (move-or-attack state entity {:x 0 :y 1})

                (= (get @*input-queue* 38) :down)
                (move-or-attack state entity {:x 0 :y -1}))]
    
    state))

(defn default-input [state entity]
  (let [last-action-timer (e/get-component state entity LastActionTimer)
        abilities-state (e/get-component state entity AbilitiesState)]
    (when (>= (- (dtc/to-long (dt/now)) (:last-time last-action-timer)) (:delay last-action-timer))
      (let [state (as-> state state
                    (replace-component state entity LastActionTimer
                                       (fn [last-action-timer]
                                         (assoc last-action-timer :last-time (dtc/to-long (dt/now)))))
                    (cond
                      (get @*key-state* 83)
                      (if (= (:state abilities-state) :swap)
                        (set-abilities-state state entity :none)
                        (set-abilities-state state entity :swap))
                      
                      (get @*key-state* 37)
                      (move-or-attack state entity {:x -1 :y 0})

                      (get @*key-state* 39)
                      (move-or-attack state entity {:x 1 :y 0})

                      (get @*key-state* 40)
                      (move-or-attack state entity {:x 0 :y 1})

                      (get @*key-state* 38)
                      (move-or-attack state entity {:x 0 :y -1})

                      (not (empty? @*input-queue*))
                      (process-input-queue-actions state entity abilities-state)))]
        (reset! *input-queue* {})
        state))))

(defn swap-input [state entity]
  state)

(defn keydown [event]
  (swap! *key-state* assoc (.-keyCode event) true)
  (swap! *input-queue* assoc (.-keyCode event) :down)
  (.preventDefault event))

(defn keyup [event]
  (swap! *key-state* dissoc (.-keyCode event)))

(defn click-event [event]
  (let [cursor-pos (get-cursor-pos (get-in @*state* [:renderer :canvas]) event)
        tile-pos (get-tile-pos-from-pixel cursor-pos)]
    (println "clicked" cursor-pos "tile" tile-pos)))

(defn handle-ai [state entity]
  (let [name-component (e/get-component state entity Name)
        name (:name name-component)]
    (add-message state (str name " took a turn."))))

(defn entity-turn [state entity]
  (let [controlled (e/get-component state entity Controlled)
        ai (e/get-component state entity BasicAI)]
    (cond
      (not (nil? controlled))
      (default-input state entity)

      (not (nil? ai))
      (handle-ai state entity))))

(defn update-entities [state]
  (loop [state state
         num-loops 1]
    (let [current-entity (:current-entity state)
          all-entities (e/get-all-entities state)
          entity (nth all-entities current-entity)]
      (if (>= num-loops (count all-entities))
        state
        (let [result (entity-turn state entity)]
          (if (nil? result)
            state
            (recur
             (assoc result
                    :current-entity (mod (inc current-entity) (count all-entities)))
             (inc num-loops))))))))

(defn update-game [state]
  (-> state
      (update-entities)
      (clear-screen)
      (render-map)
      (render-entities)
      (render-messages)
      (render-entity-list)
      (render-ability-status)))

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
                   (init-enemy "Goblin" "g" "#0000FF" 11 13)
                   (assoc :current-entity 0))
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
