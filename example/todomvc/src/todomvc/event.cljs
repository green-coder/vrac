(ns todomvc.event
  (:require [re-frame.core :as rf]
            [todomvc.constant :as const]
            [todomvc.interceptor :as interceptor]))

(rf/reg-event-db
  :initialize-db
  (fn [db _]
    const/initial-db))

(rf/reg-event-db
  :router/set-route-match
  (fn [db [_ route-match]]
    (assoc db :router/route-match route-match)))

(rf/reg-event-fx
  :local-storage/load-todo-items
  [(rf/inject-cofx :todo-items-from-local-store)]
  (fn [{:keys [db todo-items-from-local-store]} _]
    {:db (-> db
             (assoc :todo-items todo-items-from-local-store
                    :next-todo-item-id (inc (apply max -1 (keys todo-items-from-local-store)))))}))

(rf/reg-event-db
  :comp.input/on-title-changed
  (fn [db [_ title]]
    (assoc db :comp.input/title title)))

(rf/reg-event-db
  :comp.input/add-todo-item
  [interceptor/save-todo-items-in-local-storage]
  (fn [db [_ title]]
    (let [todo-item-id (:next-todo-item-id db)]
      (-> db
          (update :next-todo-item-id inc)
          (assoc-in [:todo-items todo-item-id] {:id todo-item-id
                                                :title title
                                                :completed false})
          (assoc :comp.input/title "")))))

(rf/reg-event-db
  :toggle-all-todo-items
  [interceptor/save-todo-items-in-local-storage]
  (fn [db [_ all-completed]]
    (-> db
        (update :todo-items
                update-vals
                (fn [todo-item]
                  (assoc todo-item :completed (not all-completed)))))))

(rf/reg-event-db
  :toggle-todo-item
  [interceptor/save-todo-items-in-local-storage]
  (fn [db [_ todo-item-id]]
    (-> db
        (update-in [:todo-items todo-item-id :completed] not))))

(rf/reg-event-db
  :delete-todo-item
  [interceptor/save-todo-items-in-local-storage]
  (fn [db [_ todo-item-id]]
    (-> db
        (update :todo-items dissoc todo-item-id))))

(rf/reg-event-db
  :set-todo-item-title
  [interceptor/save-todo-items-in-local-storage]
  (fn [db [_ todo-item-id title]]
    (-> db
        (assoc-in [:todo-items todo-item-id :title] title))))

(rf/reg-event-db
  :clean-completed-todo-items
  [interceptor/save-todo-items-in-local-storage]
  (fn [db _]
    (-> db
        (update :todo-items
                (fn [todo-items]
                  (into {}
                        (remove (comp :completed val))
                        todo-items))))))
