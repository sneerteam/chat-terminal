(ns sneerteam.chat-terminal.udp
  (:require [clojure.core.async :as async :refer [go-loop <! >!!]]
            [clojure.edn :as edn])
  (:import [java.net DatagramPacket DatagramSocket InetAddress]))

(defrecord UdpConnection [socket in out])

(defn- read-bytes [bytes length]
  (edn/read-string (String. bytes 0 length "UTF8")))

(defn- ->bytes [data]
  (. (pr-str data) getBytes "UTF8"))

(defn- receive-loop [socket channel]
  (let [buffer (byte-array 512)
        packet (DatagramPacket. buffer (alength buffer))]
    (while true
      (. socket receive packet)
      (>!! channel (read-bytes buffer (.getLength packet))))))

(defn- async-receive-loop [socket channel]
  (async/thread (receive-loop socket channel)))

(defn- async-send-loop [channel socket addressed-packet]
  (go-loop []
    (when-let [msg (<! channel)]
      (let [bytes (->bytes msg)]
        (. socket send (doto addressed-packet
                         (.setData bytes)
                         (.setLength (alength bytes)))))
      (recur))))

(defn connect [host port]
  (let [address (InetAddress/getByName host)
        addressed-packet (DatagramPacket. (byte-array 0) 0 0 address port)
        socket (DatagramSocket.)
        in (async/chan 1)
        out (async/chan 1)]
    (async-receive-loop socket in)
    (async-send-loop out socket addressed-packet)
    (UdpConnection. socket in out)))
