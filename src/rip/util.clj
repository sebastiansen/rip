(ns rip.util
  (:use rip.core))

(defn in?
  "Check if in one of the giving actions inside a handler.
  Usage:
    (defscope api
      \"/api\"
      (index [] (if (in? :index) \"ok\")))"
  [& actions]
  (or (empty? actions)
      (contains? (set actions) *current-action*)))
