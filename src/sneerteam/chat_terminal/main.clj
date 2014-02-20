(ns sneerteam.chat-terminal.main
  (:require [lanterna.screen :as s]
            [amalloy.ring-buffer :refer [ring-buffer]]
            [clojure.core.async :as async :refer [go go-loop alt! alt!! <! >! >!! <!!]])
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

(defn client-loop
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
                       (>! server-out {:type :get-last-msgs})
                       ; try again in 1 minute
                       (recur (timeout-minutes 1))))))))

(def user-name (System/getProperty "user.name"))

(defn send-message [out body]
  (go (>! out {:type :create-msg :sender user-name :body body})))

(def input-state (atom {:visible true :value ""}))

(defn set-input-value [value]
  (swap! input-state assoc :value value))

(defn input-value []
  (:value @input-state))

(defn screen-rows-1 [scr]
  (let [[_ rows] (s/get-size scr)]
    (dec rows)))

(defn draw-message [scr row msg]
  (s/put-string scr 0 row (str (:sender msg) "> " (:body msg))))

(defn set-cursor-visible [scr visible]
  (-> scr .getTerminal (.setCursorVisible visible)))

(defn redraw [scr]
  (let [rows (screen-rows-1 scr)
        msgs @messages
        scroll-pos (max 0 (- (count msgs) rows))
        msgs (drop scroll-pos msgs)]
    (s/clear scr)

    (doall (map-indexed (partial draw-message scr) msgs))

    (let [{:keys [visible value]} @input-state]
      (s/put-string scr 0 rows value)
      (when visible (s/move-cursor scr (.length value) rows))
      (set-cursor-visible scr visible))

    (s/redraw scr)))

(defmulti handle-key (fn [key ctx] (if (char? key) :char key)))

(defmethod handle-key :char [key ctx]
  (set-input-value (str (input-value) key)))

(defmethod handle-key :enter [key ctx]
  (let [value (input-value)]
    (when (pos? (.length value))
      (send-message ctx value)
      (set-input-value ""))))

(defmethod handle-key :backspace [key ctx]
  (let [value (input-value)
        length (.length value)]
    (when (pos? length)
      (set-input-value (subs value 0 (dec length))))))

(defmethod handle-key :default [key ctx])

(defn handle-keys [scr ctx]
  (loop []
    (when-let [key (s/get-key scr)]
      (if (= :escape key)
        :escape
        (do (handle-key key ctx)
            (recur))))))

(defn blink-cursor []
  (swap! input-state #(assoc % :visible (not (:visible %)))))

(defn screen-loop [scr ctx]

  (let [blink-timeout #(async/timeout 500)
        key-timeout #(async/timeout 100)
        next-redraw (async/chan (async/sliding-buffer 1))
        add-redraw-watch #(add-watch % :screen (fn [key a old new] (>!! next-redraw :redraw)))]

    (add-redraw-watch messages)
    (add-redraw-watch input-state)

    (loop [next-handle-keys (key-timeout)
           next-blink (blink-timeout)]
      (alt!!
        next-redraw ([_] (do
                           (redraw scr)
                           (recur next-handle-keys next-blink)))

        next-handle-keys ([_] (when-not (= :escape (handle-keys scr ctx))
                                (recur (key-timeout) next-blink)))

        next-blink ([_] (do
                          (blink-cursor)
                          (recur next-handle-keys (blink-timeout))))))))

(defn run-screen-loop
  "runs the screen loop sending messages to the `sout` channel."
  [sout]
  (let [scr (s/get-screen :text)]
    (s/start scr)
    (screen-loop scr sout)
    (s/stop scr)))

(defn feed-msg [sender body]
  {:type :feed-msg :timestamp (System/currentTimeMillis) :sender sender :body body})

(defn feed-every [msecs server sender messages]
  (go-loop []
    (<! (async/timeout msecs))
    (>! server (feed-msg sender (rand-nth messages)))
    (recur)))

(defn create-msg->feed-msg [{:keys [sender body]}]
  (feed-msg sender body))

(defn start-fake-clients [sin sout]
  (feed-every 2500 sin "klaus" ["sir" "atualizei lá" "blz?"])
  (feed-every 5000 sin "bamboo"["opa!" "e aí?" "blz." "agora sim!"])
  (feed-every 4000 sin "fabio" ["tri!" "sabe o que seria tri?" "ok"])
  (async/pipe (async/map< create-msg->feed-msg sout) sin))

(defn -main
  [& args]
  (let [sin (async/chan 1)
        sout (async/chan 1)]
    (start-fake-clients sin sout)
    (client-loop sin sout)
    (run-screen-loop sout)))
