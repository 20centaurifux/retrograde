(ns retrograde.jdbc.sqlite.core
  (:require [retrograde.jdbc.core :as jdbc]))

(defmethod jdbc/insert-mem-rep-if-absent-query "sqlite"
  [_ relation id data]
  {:insert-into relation
   :values [{:id id :data data}]
   :on-conflict :id
   :do-nothing true})