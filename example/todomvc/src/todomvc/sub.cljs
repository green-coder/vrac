(ns todomvc.sub
  (:require [re-frame.core :as rf]))

(rf/reg-sub
  :router/route-match
  (fn [db _]
    (:router/route-match db)))

(rf/reg-sub
  :comp.input/title
  (fn [db _]
    (:comp.input/title db)))

(rf/reg-sub
  :comp.input/any-todo-item
  (fn [db _]
    (pos? (count (:todo-items db)))))

(rf/reg-sub
  :all-todo-items-completed
  (fn [db _]
    (->> (vals (:todo-items db))
         (every? :completed))))

(rf/reg-sub
  :todo-item-ids
  (fn [db _]
    (keys (:todo-items db))))

(rf/reg-sub
  :todo-item
  (fn [db [_ todo-item-id]]
    (-> db :todo-items (get todo-item-id))))

(rf/reg-sub
  :comp/display-type
  :<- [:router/route-match]
  (fn [router-match _]
    (-> router-match :data :name {:page/all-todo-items :all
                                  :page/active-todo-items :active
                                  :page/completed-todo-items :completed})))

(rf/reg-sub
  :show-todo-item
  (fn [[_ todo-item-id]]
    [(rf/subscribe [:comp/display-type])
     (rf/subscribe [:todo-item todo-item-id])])
  (fn [[display-type todo-item] _]
    (let [completed (:completed todo-item)]
      (case display-type
        :completed completed
        :active (not completed)
        #_:all true))))

(rf/reg-sub
  :active-todo-items
  (fn [db _]
    (->> (vals (:todo-items db))
         (filterv (complement :completed)))))

(rf/reg-sub
  :completed-todo-items
  (fn [db _]
    (->> (vals (:todo-items db))
         (filterv :completed))))

(rf/reg-sub
  :active-todo-items-count
  :<- [:active-todo-items]
  (fn [active-todo-items _]
    (count active-todo-items)))

(rf/reg-sub
  :complemented-todo-items-count
  :<- [:completed-todo-items]
  (fn [completed-todo-items _]
    (count completed-todo-items)))

(rf/reg-sub
  :complemented-todo-items-to-clear
  :<- [:complemented-todo-items-count]
  (fn [complemented-todo-items-count _]
    (pos? complemented-todo-items-count)))
