(ns todomvc.constant)

(def enter-keycode 13)
(def escape-keycode 27)

(def initial-db
  {:next-todo-item-id 0
   :comp.input/title ""
   :todo-items {}})

(def todo-item-local-storage-key "todo-items")
