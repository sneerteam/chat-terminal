(ns sneerteam.chat-terminal.main
  (:require [lanterna.screen :as s]
            [amalloy.ring-buffer :refer [ring-buffer]]
            [clojure.core.async :as async :refer [go go-loop alt! alt!! <! >! >!! <!!]]
            [sneerteam.chat-terminal.udp :as udp])
  (:gen-class))

(def max-messages 200)

(def messages (atom (ring-buffer max-messages)))

(defn conj-message [msg]
  (swap! messages conj msg))

(defn minutes->msec [m] (* m 60 1000))

(defn timeout-minutes
  "creates a channel that closes in `m minutes"
  [m]
  (async/timeout (long (minutes->msec m))))

(defn async-client-loop
  "Sits in a loop waiting for messages from server-in.
  If it doesn't hear from server-in in 3 minutes it will request new messages using server-out.
  Quits the loop when server-in is closed."
  [server-in server-out]
  (let [server-timeout #(timeout-minutes 3)]
    (go-loop [timeout (server-timeout)]
      (alt!
        server-in ([msg] (when-not (nil? msg)
                           (conj-message msg)
                           (recur (server-timeout))))
        timeout ([_] (do
                       (>! server-out {:type :refresh})
                       ; try again in 1 minute
                       (recur (timeout-minutes 1))))))))

(def user-name (System/getProperty "user.name"))

(defn send-message [out contents]
  (go (>! out {:type :msg :sender user-name :contents contents})))

(def input-state (atom {:visible true :value ""}))

(defn blink-cursor []
  (swap! input-state #(assoc % :visible (not (:visible %)))))

(defn cursor-visible? []
  (:visible @input-state))

(defn set-input-value [value]
  (swap! input-state assoc :value value))

(defn input-value []
  (:value @input-state))

(defn screen-rows-1 [scr]
  (let [[_ rows] (s/get-size scr)]
    (dec rows)))

(defn set-cursor-visible [scr visible]
  (-> scr .getTerminal (.setCursorVisible visible)))

(def sender-colors [:red :green :yellow :blue :magenta :cyan])

(defn color-for [sender]
  (if (= user-name sender)
    :white
    (nth sender-colors (mod (hash sender) (count sender-colors)))))

(defn draw-message [scr row {:keys [sender contents]}]
  (s/put-string scr 0 row (str sender "> " contents) {:fg (color-for sender)}))

(defn draw-input-prompt [scr row]
  (let [value (input-value)]
    (s/put-string scr 0 row value)
    (s/move-cursor scr (.length value) row)))

(defn redraw [scr]
  (let [rows (screen-rows-1 scr)
        msgs @messages
        scroll-pos (max 0 (- (count msgs) rows))
        msgs (drop scroll-pos msgs)
        set-cursor-visibility (partial set-cursor-visible scr (cursor-visible?))]

    (s/clear scr)
    (doall (map-indexed (partial draw-message scr) msgs))
    (draw-input-prompt scr rows)
    (set-cursor-visibility)
    (s/redraw scr)
    (set-cursor-visibility)))

(defmulti handle-key
  "handles the key and returns the new input value"
  (fn [key ctx] (if (char? key) :char key)))

(defmethod handle-key :char [key ctx]
  (str (input-value) key))

(defmethod handle-key :enter [key ctx]
  (let [value (input-value)]
    (when (pos? (.length value))
      (send-message ctx value)
      "")))

(defmethod handle-key :backspace [key ctx]
  (let [value (input-value)
        length (.length value)]
    (when (pos? length)
      (subs value 0 (dec length)))))

(defmethod handle-key :default [key ctx])

(defn handle-keys [scr ctx]
  (loop []
    (when-let [key (s/get-key-blocking scr)]
      (if (= :escape key)
        :escape
        (do
          (when-let [new-input-value (handle-key key ctx)]
            (set-input-value new-input-value))
          (recur))))))

(defn screen-loop [scr ctx]

  (let [blink-timeout #(async/timeout 500)
        input-thread (async/thread (handle-keys scr ctx))
        next-redraw (async/chan (async/sliding-buffer 1))
        add-redraw-watch #(add-watch % :screen (fn [& _] (>!! next-redraw :redraw)))]

    (add-redraw-watch messages)
    (add-redraw-watch input-state)

    (loop [next-blink (blink-timeout)]
      (alt!!
        next-redraw ([_] (do
                           (redraw scr)
                           (recur next-blink)))

        next-blink ([_] (do
                          (blink-cursor)
                          (recur (blink-timeout))))

        input-thread ([_] _)))))

(defn enter-screen-loop
  "runs the screen loop sending messages to the `sout` channel."
  [sout]
  (let [scr (s/get-screen :text)]
    (s/in-screen scr
       (screen-loop scr sout))))

(defn -main
  [& [host port]]
  (let [host (or host "dynamic.sneer.me")
        port (if port (Long/parseLong port) 55555)
        {:keys [in out]} (udp/connect host port)]
    (async-client-loop in out)
    (enter-screen-loop out)))
