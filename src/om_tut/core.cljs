(ns om-tut.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! chan timeout put!]]
            [cljs.core.async :refer [chan pub]]
            [cljs-http.client :as http] [clojure.string :as string]))

(enable-console-print!)

(defn console
  "This is a wrapper function to call js console methods  
   https://developer.mozilla.org/en-US/docs/Web/API/Console  
   "
  ([whatever]
   (js/console.log (clj->js whatever)))
  ([method whatever]
   ((aget js/console method) (clj->js whatever))))

(defn on-js-reload [])

(defonce app-state (atom {:page 1 :table-data nil}))

(defn reset [owner]
  (om/set-state! owner {:search-term "" :table-data nil}))


(def url "https://api.github.com/search/code")
(def auth-header {"Authorization" "Basic Y2hlbWl1LWtvYWx5czpnaHBfNmVRWjF6dnBEVmdVTk1BbVJoNk80d2htbFhEZUYzMk9vakx2"})

(defn getResaults [owner search-term page]
  (go (let [response (<! (http/get url
                                   {:with-credentials? false
                                    :headers           auth-header
                                    :query-params      {:q (str search-term " extension:cljs") :page (if (> page 1) page 1)}}))]
        ;; (fn [] (put! (result-channel)  (response)))
        (om/set-state! owner {:table-data (conj (:body response) {:pages (Math/ceil (/ (:total_count (:body response)) 30))})})
        (console "state changed")
        (console (om/get-state owner))
      ;;   (fn [] (put! (result-channel)  (conj data {:pages (Math/ceil (/ (:total_count data) 30))})))
        ;; (console "result-channel")
        ;; (console result-channel)
        )))


(defn handle-search-state [owner val]
  (reset owner)
  (om/set-state! owner {:search-term val}))

;; how can this be stateless and how can I incorporate the state method inside the component
(defn search-component "Search component" [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:search-term ""})
    om/IRenderState
    (render-state [_ {:keys [search-term]}]
      (dom/div nil
               (dom/label #js {:className "label1" :htmlFor (:field-name data)} (:field-label data))
               (dom/input #js {:className (:class data) :id (:field-name data)
                               :onChange  (fn [e] (handle-search-state owner e.target.value) )})
               (dom/button #js {:onClick
                                (fn [] (put! (:search-channel data) search-term))}
                           "Execute search!")))))

(defn search-results-component "This component renders the search results" [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:page 1 :table-data [] :result-channel (chan)})
    om/IWillMount
    (will-mount [_]
      (getResaults owner (:search-term data) (om/get-state owner :page))
      (console "table" data))
    om/IRenderState
    (render-state [_ {:keys [table-data]}]
      (dom/div nil
               (dom/h2 #js {:className "results-header"} (str "Search results for " (:search-term data)))
               (if (empty? table-data)
                 (dom/div #js {:className "loading-message"} (str "Loading ..."))
                 (dom/div nil 
                          (dom/h3 #js {:className "results-count"} (str "Results Count " (:total_count table-data)))
                          (map (dom/div #js {:className "row-item"} (str "file name " ((:name %))))(:items table-data))
                          ))))))


(defn widget "main app component" [data owner]
  (reify
    om/ICheckState
    om/IInitState
    (init-state [_]
      {:count 0 :search-channel (chan) :search-term ""})
    om/IWillMount
    (will-mount [_]
      (let [search-term (om/get-state owner :search-channel)]
        (go-loop []
          (let [term (<! search-term)]
            (console (str "from chan " term))
            (om/set-state! owner :search-term term)
            ;; (getResaults owner term (om/get-state owner :page))
            (recur)))))
    om/IWillUnmount
    (will-unmount [_]
      (println "Hello widget unmounting"))
    om/IRenderState
    (render-state [_ {:keys [count search-channel search-term]}]

      (console "Render")
      (dom/div nil
               (dom/h2 nil "Search github for cljs exetnsions")
               (om/build search-component {:field-name "searchTerm" :field-label "Search Term" :class "input1" :search-channel search-channel})
               (when-not (= search-term "") (om/build search-results-component  {:search-term search-term}))))))


(om/root widget app-state
         {:target (.getElementById js/document "app")})
