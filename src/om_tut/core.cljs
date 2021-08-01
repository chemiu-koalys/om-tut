(ns om-tut.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as     om
             :include-macros true]
            [om.dom :as      dom
             :include-macros true]
            [cljs.core.async :refer [<! chan timeout put!]]
            [cljs.core.async :refer [chan pub]]
            [cljs-http.client :as http]
            [cljs.pprint :as pp]
            [clojure.string :as string]))


(enable-console-print!)


(defmacro dbg [x]
  `(let [x# ~x]
    (when (audyx.app.config-logic/get-in [:general :enable-debugging])
      (print "dbg-key" '~x ":")
      (console/log "%cdbg-value: %o" "color:hsl(208, 53%, 58%)" x#))
    x#))

(def results-per-page 30)

(defn console
  "This is a wrapper function to call js console methods  
   https://developer.mozilla.org/en-US/docs/Web/API/Console"
  ([whatever]
   (js/console.log (console whatever "log")))
  ([whatever method]
   ((aget js/console method) (clj->js whatever))))

(defn on-js-reload [])

(defonce app-state (atom {:page 1 :table-data nil :search-term "" :text "" :change ""}))

;; --- Watch the state ---

(add-watch app-state :app-state
           (fn [key _atom old-state new-state]
            ;;  (if (not= old-state new-state) (do
            ;;                                   (println "---" key "atom changed ---")
            ;;                                   (println "---" key "old state ---")
            ;;                                   (pp/pprint old-state)
            ;;                                   (println "---" key "new state ---")
            ;;                                   (pp/pprint new-state)))
             ))
             


(defn set-table-data! [table-data]
  (swap!
   app-state
   update-in [:table-data] table-data))

(defn reset-state! [owner]
  (om/set-state! owner {:search-term "" :table-data nil}))

(defn update-app-state! [atom state-data]
  (swap! atom assoc state-data))

(defn count-pages [total-count] (Math/ceil (/ total-count results-per-page)))

(defn fetch-results [search-term page owner]
  (let [url         "https://api.github.com/search/code"
        auth-header {"Authorization" "Basic Y2hlbWl1LWtvYWx5czpnaHBfNmVRWjF6dnBEVmdVTk1BbVJoNk80d2htbFhEZUYzMk9vakx2"}
        q           (str search-term " extension:cljs")
        page        (if (> page 1) page 1)
        options     {:with-credentials? false
                     :headers           auth-header
                     :query-params      {:q q :page page}}]

    (go
      (let [data  (:body (<! (http/get url options)))]
        (om/set-state! owner  {:search-term search-term
                               :table-data  (conj data {:pages (count-pages (:total_count data))})})))))

(defn row-item [item owner]
  (reify
    om/IWillUpdate (will-update [this next-props next-state]
                                (console "update item widget")
                                (console next-props))
    om/IRenderState
    (render-state [this state]
      ;; (dom/div #js {:className "row-item"} (str "file name " ((:name item))))
      (dom/div nil (:name item))

      )))


(defn table [data owner]
  (reify
    om/ICheckState
    om/IInitState
    (init-state [_])

    om/IWillMount
    (will-mount [_])

    om/IWillUnmount
    (will-unmount [_])
    
    om/IWillUpdate (will-update [this next-props next-state]
                                (console "update table widget")
                                (console next-props))

    om/IRenderState
    (render-state [this state]
                  (console "hello1")

                  (console data "table")
                  (let [table-data (data :table-data) items (table-data :items)]
                    (console "checking items")
                    (console (count items))
                    (console (> 1 (count items)))
                    )
                    

                  (let [table-data (data :table-data) items (table-data :items)]
                    (dom/div nil
                             (dom/h2 #js {:className "results-header"} (str "Search results for " (:search-term data)))
                             (if (> 1 (count items))
                               (dom/div #js {:className "loading-message"} (str "Loading ..."))
                               (dom/div nil
                                        (dom/h3 #js {:className "results-count"} (str "Results Count " (:total_count table-data)))
                                        (om/build-all row-item items)
                                        ;; (map (dom/div #js {:className "row-item"} (str "file name " ((:name %)))) items)
                                        )))))))

(defn handle-change [e owner]
  (om/set-state! owner :search-term (.. e -target -value)))

(defn search! [data owner]
  (console data)
  (swap! app-state assoc :search-term data)
  (fetch-results data 1 owner)
  (console owner)
  )


(defn widget "main app component" [data owner]
  (reify
   om/ICheckState
   om/IInitState
   (init-state [_]
     {:table-data {:items []} :search-term ""})

   om/IWillMount
   (will-mount [_])

   om/IWillUnmount
   (will-unmount [_]
                 (println "Hello widget unmounting"))
   
   om/IRenderState
   (render-state [this state]
                 (dom/div nil
                          (dom/h2 nil "Search github for cljs exetnsions")
                          (dom/h2 nil (str "Search term" (:search-term data)))
                          (dom/label nil "Search for")
                          (dom/input
                           #js {:type     "text"
                                :ref      "search-term"
                                :value    (:search-term state)
                                :onChange #(handle-change % owner state)})
                          (dom/button #js {:onClick #(let [search-term (state :search-term)]
                                                       (search! search-term owner))} "Search!")
                          (om/build table {:table-data (state :table-data) :search-term  (state :search-term)})))))


(om/root widget app-state
         {:target (.getElementById js/document "app")})
