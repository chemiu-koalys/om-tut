(ns om-tut.helper)

(defn console
  "This is a wrapper function to call js console methods  
   https://developer.mozilla.org/en-US/docs/Web/API/Console  
   "
  ([whatever]
   (js/console.log (clj->js whatever)))
  ([method whatever]
   ((aget js/console method)(clj->js whatever))
   ))