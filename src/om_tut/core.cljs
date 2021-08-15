(ns om-tut.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om
             :include-macros true]
            [om.dom :as dom
             :include-macros true]
            [cljs.core.async :refer [<! chan timeout put!]]
            [cljs.core.async :refer [chan pub]]
            [cljs-http.client :as http]
            [cljs.pprint :as pp]
            [om-tut.localstorage :as local]
            [cljs.repl :as rep]
            [clojure.string :as string]))


(enable-console-print!)

(defn on-js-reload [])

;; --- global utility ---

(defmacro dbg [x]
  `(let [x# ~x]
     (print "dbg-key" '~x ":")
     (console/log "%cdbg-value: %o" "color:hsl(208, 53%, 58%)" x#)
     x#))


(defn console
  "This is a wrapper function to call js console methods
   https://developer.mozilla.org/en-US/docs/Web/API/Console"
  ([whatever]
   (js/console.log (console whatever "log")))
  ([whatever method]
   ((aget js/console method) (clj->js whatever))))

(defn index-of [x coll]
  (let [idx? (fn [i a] (when (= x a) i))]
    (first (keep-indexed idx? coll))))


; --- local storage ---
(defonce letters-key "letters")
(defonce words-key "words")
(defn get-local-words [] (local/get-item words-key))
(defn get-local-letter-words [letter]
  (get-in (get-local-words) [(keyword letter)]))
(defn set-local-letter-words [letter words]
  (js/console.log "got here")
  (let [local-words (get-local-words)]
    (if local-words
      (do
        (as-> local-words $
          (assoc $ (keyword letter) words)
          (local/set-item! words-key $))
        #_(->>
           local-words
           (merge {(keyword letter) words})
           (local/set-item! words-key)))
      (do
        (println "got here 2")
        (local/set-item! words-key {(keyword letter) words})))))



;; --- state ---


(defonce app-state (atom {:letters nil :current-letter nil :words {}}))

(defn get-chars []
  (to-array "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defn init-char-state []
  (into (sorted-map) (for [char (get-chars)
                           opts [{:title char :selected false}]
                           key [(keyword (clojure.string/lower-case char))]]
                       {key opts})))

(do
  (def letters (if
                (not= (local/get-item letters-key) "null")
                 (local/get-item ["letters"])
                 (init-char-state)))
  ;TODO use letters, there is a bug here works only after refresh
  ; (swap! app-state assoc :letters letters)
  (swap! app-state assoc :letters (init-char-state))
  (local/set-item! letters-key letters))

;; --- Watch the state ---

(add-watch app-state :app-state
           (fn [key _atom old-state new-state]
             (if false
               (if (not= old-state new-state) (do
                                                ;(println "---" key "atom changed ---")
                                                ;(println "---" key "old state ---")
                                                ;(pp/pprint old-state)
                                                ;(println "---" key "new state ---")
                                                (pp/pprint (:words new-state)))))))
;; --- utility ---

(defn add-order [coll]
  (vec (map #(assoc %1 :order %2) coll (iterate inc 0))))

(def word-api-url "https://api.datamuse.com/words")

(defn get-words! [letter data]
  ;check if local storage has words for this letter else get from api
  (let [letter-words (get-local-letter-words letter)]
    (if letter-words
      (om/update! data :words letter-words)
      (let [url (str word-api-url)
            options {:with-credentials? false
                     :query-params      {:sp (str (name letter) "*") :max 100}}]
        (go
          (let [words (mapv :word (:body (<! (http/get url options))))]
            (om/update! data :words words)))))))

;; --- render ---


(defn select-letter! [letter current-letter data]
  (if current-letter
    (if (not= (name letter) current-letter)
      (do
        (om/update! data [:letters (keyword current-letter) :selected] false))))
  (om/update! data [:current-letter] (name letter))
  (om/update! data [:letters letter :selected] true))

(defn letter-component [data owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [set-letter-chan]}]
      (let [key (first data)
            properties (second data)
            title (properties :title)
            selected (properties :selected)]
        (dom/button #js {:onClick   (fn [e] (put! set-letter-chan key))
                         :className (str "letter_item " (if selected "selected"))} title)))))

(defn move-word! [word func word-cursor]
  (let [order (index-of word word-cursor)
        current (get word-cursor order)
        sub-with (get word-cursor (func order))]
    (om/update! word-cursor (assoc word-cursor (func order) current order sub-with))))

(defn move-button [data]
  (reify
    om/IRenderState
    (render-state [_ _]
      (let [{:keys [word-chan direction disable word]} data
            up (if (= direction "up") true false)
            icon (dom/img #js {:className "up-image"
                               :src       "/images/up.svg"})
            func (if up dec inc)]
        (if disable (dom/button #js {:onClick   (fn [e] (put! word-chan [word func]))
                                     :className (str "move_word push" direction)
                                     :disabled  true} icon)
            (dom/button #js {:onClick   (fn [e] (put! word-chan [word func]))
                             :className (str "move_word push" direction)} icon))))))


(defn word-component [data]
  (reify
    om/IRenderState
    (render-state [this {:keys [word-chan]}]
      (let [word-cursor (om/ref-cursor (:words (om/root-cursor app-state)))
            first (< (index-of data word-cursor) 1)
            last (= (index-of data word-cursor) (dec (count word-cursor)))]
        (dom/div #js {:className "word_wrapper"}
                 (om/build move-button {:word-chan word-chan :direction "down" :disable last :word data})
                 (dom/div #js {:className (str "word_item ")} data)
                 (om/build move-button {:word-chan word-chan :direction "up" :disable first :word data}))))))

(defn widget "main app component" [data owner]
  (reify
    om/ICheckState
    om/IInitState
    (init-state [_]
      {:set-letter-chan (chan) :word-chan (chan) :current-letter nil :words {}})
    om/IWillUpdate
    (will-update [this next-props next-state]
      (let [letter (:current-letter next-props) words (:words next-props)]
        (if (= (get (first words) 0) letter)
          (set-local-letter-words letter words))))

    om/IWillMount
    (will-mount [_]
      (let [set-letter (om/get-state owner :set-letter-chan)]
        (go (loop []
              (let [letter (<! set-letter) current-letter (get @app-state :current-letter)]
                (select-letter! letter current-letter data)
                (get-words! letter data))
              (recur))))
      (let [word-reorder (om/get-state owner :word-chan)]
        (go (loop []
              (let [word-util (<! word-reorder) word-cursor (om/ref-cursor (:words (om/root-cursor app-state)))]
                (move-word! (first word-util) (second word-util) word-cursor))
              (recur)))))
    om/IRenderState
    (render-state [this {:keys [set-letter-chan word-chan]}]
      (let [{:keys [letters words]} data]
        (dom/div nil
                 (dom/div #js {:className "letter-section"}
                          (dom/h2 nil "letters")
                          (om/build-all letter-component letters
                                        {:init-state {:set-letter-chan set-letter-chan}}))
                 (dom/div #js {:className "word-section"} (dom/h2 nil "words")
                          (dom/div #js {:className "words-wrapper"}
                                   (om/build-all word-component words
                                                 {:init-state {:word-chan word-chan}}))))))))

(om/root widget app-state
         {:target (.getElementById js/document "app")})
