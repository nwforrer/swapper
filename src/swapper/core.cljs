(ns swapper.core
  (:require [brute.entity :as e]
            [cljsjs.howler]))

(enable-console-print!)

(defonce *state* (atom nil))
(defonce *key-state* (atom nil))

(defrecord Position [x y])
(defrecord Visible [char color])
(defrecord Controlled [])

(defn init-ecs [state]
  (e/create-system))

(defn init-render-context [state]
  (let* [canvas (.getElementById js/document "game-canvas")
         ctx (.getContext canvas "2d")]
    (-> state
        (assoc-in [:renderer :ctx] ctx)
        (assoc-in [:renderer :canvas] canvas))))

(defn init-sounds [state]
  (let [sound (js/Howl. (clj->js {:src ["sound/Subterranians.mp3"]
                                  :autoplay true
                                  }))]
    (assoc state :music sound)))

(defn init-player [state]
  (let [player (e/create-entity)]
    (-> state
        (e/add-component player (->Controlled))
        (e/add-component player (->Position (atom 50) (atom 50)))
        (e/add-component player (->Visible "@" "#FF0000")))))

(defn render [state]
  (let [ctx (get-in state [:renderer :ctx])
        canvas (get-in state [:renderer :canvas])]
    (set! (.-fillStyle ctx) "#000")
    (.fillRect ctx 0 0 (.-width canvas) (.-height canvas))
    (set! (.-font ctx) "23px courier, inconsolata, monospace")
    (set! (.-textBaseline ctx) "top")
    (doseq [entity (e/get-all-entities-with-component state Visible)]
      (let* [glyph (e/get-component state entity Visible)
             position (e/get-component state entity Position)
             x (:x position)
             y (:y position)]
        (set! (.-fillStyle ctx) (:color glyph))
        (.fillText ctx (:char glyph) @x @y))))
  state)

(defn move-position! [state entity delta]
  (let* [position (e/get-component state entity Position)
         x (:x position)
         y (:y position)]
    (swap! x + (:x delta))
    (swap! y + (:y delta))))

(defn handle-input [state]
  (doseq [entity (e/get-all-entities-with-component state Controlled)]
    (if (get @*key-state* 37)
      (move-position! state entity {:x -1 :y 0}))
    (if (get @*key-state* 39)
      (move-position! state entity {:x 1 :y 0}))
    (if (get @*key-state* 40)
      (move-position! state entity {:x 0 :y 1}))
    (if (get @*key-state* 38)
      (move-position! state entity {:x 0 :y -1})))
  state)

(defn keydown [event]
  (swap! *key-state* assoc (.-keyCode event) true))

(defn keyup [event]
  (swap! *key-state* dissoc (.-keyCode event)))

(defn update-game [state]
  (-> state
      (handle-input)
      (render)))

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
      (init-player)
      (game-loop))
  (set! (.-onkeydown js/document) keydown)
  (set! (.-onkeyup js/document) keyup))

(set! (.-onload js/window) startup)


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
