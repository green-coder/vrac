(ns todomvc.view
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [reitit.frontend.easy :as rtfe]
            [todomvc.constant :as const]
            [signaali.reactive :as sr]
            [vrac.web :as vw :refer [$]]))

(defn- focus-on-create [ref]
  (sr/create-effect
    (fn []
      (when-some [^js input-element @ref]
        (.focus input-element)))))

(defn todo-item-creation-input []
  (let [title (rf/subscribe [:comp.input/title])
        ref (sr/create-signal nil)]
    ($ :<>
       (vw/use-effects [(focus-on-create ref)])
       ($ :input.new-todo
          (vw/attributes-effect
            (fn []
              {:value @title}))
          {:ref         ref
           :type        "text"
           :placeholder "What needs to be done?"
           :on-input    (fn [^js event]
                          (rf/dispatch [:comp.input/on-title-changed (-> event .-target .-value)]))
           :on-keyDown  (fn [^js event]
                          (let [key-pressed   (.-which event)
                                trimmed-title (str/trim @title)]
                            (when (and (= key-pressed const/enter-keycode)
                                       (not (str/blank? trimmed-title)))
                              (rf/dispatch [:comp.input/add-todo-item trimmed-title]))))}))))

(defn toggle-items-button []
  (let [all-completed (rf/subscribe [:all-todo-items-completed])]
    ($ :<>
       ($ :input#toggle-all.toggle-all
          {:type "checkbox"
           :on-change (fn []
                        (rf/dispatch [:toggle-all-todo-items @all-completed]))}
          (vw/attributes-effect
            (fn []
              {:checked @all-completed})))
       ($ :label {:for "toggle-all"} "Mark all as complete"))))

(defn todo-edit [todo-item editing]
  (let [ref (sr/create-signal nil)
        default (:title @todo-item)
        edit-title (sr/create-state default)]
    (vw/when-fragment editing
      ($ :<>
         (vw/use-effects [(focus-on-create ref)])
         ($ :input.edit
            (vw/attributes-effect
              (fn []
                {:value @edit-title}))
            {:ref        ref
             :type       "text"
             :on-input  (fn [^js event]
                          (reset! edit-title (-> event .-target .-value)))
             :on-blur    (fn [_]
                           (reset! editing false)
                           (rf/dispatch [:set-todo-item-title (:id @todo-item) @edit-title]))
             :on-keyDown (fn [^js event]
                           (let [key-pressed (.-which event)]
                             (cond
                               (= key-pressed const/enter-keycode)
                               (do (reset! editing false)
                                   (rf/dispatch [:set-todo-item-title (:id @todo-item) @edit-title]))

                               (= key-pressed const/escape-keycode)
                               (do (reset! editing false)
                                   (reset! edit-title default)))))})))))

(defn todo-item [todo-item-id]
  (let [editing (sr/create-state false)
        todo (rf/subscribe [:todo-item todo-item-id])
        show-todo-item (rf/subscribe [:show-todo-item todo-item-id])
        title (sr/create-memo (fn [] (:title @todo)))
        completed (sr/create-memo (fn [] (:completed @todo)))]
    ($ :li
       (vw/attributes-effect
         (fn []
           {:class [(when @completed "completed")
                    (when @editing "editing")]
            :style {:display (if @show-todo-item
                               "list-item"
                               "none")}}))
       ($ :div.view
          ($ :input.toggle
             {:type "checkbox"
              :on-change (fn [_]
                           (rf/dispatch [:toggle-todo-item todo-item-id]))}
             (vw/attributes-effect
               (fn []
                 {:checked @completed})))
          ($ :label {:on-dblclick (fn [_]
                                    (reset! editing true))}
             title)
          ($ :button.destroy {:on-click (fn [_]
                                          (rf/dispatch [:delete-todo-item todo-item-id]))}))
       ($ todo-edit todo editing))))

(defn todo-items-list []
  (let [todo-item-ids (rf/subscribe [:todo-item-ids])]
    ($ :ul.todo-list
       (vw/for-fragment* todo-item-ids identity
         (fn [todo-item-id]
           ($ todo-item todo-item-id))))))

(defn todo-items-count []
  (let [active-todo-item-count (rf/subscribe [:active-todo-items-count])]
    ($ :span.todo-count
       ($ :strong
          (sr/create-derived (fn []
                               (str @active-todo-item-count
                                    (if (= @active-todo-item-count 1) " item " " items ")
                                    "left")))))))

(defn todo-items-filters []
  (let [display-type (rf/subscribe [:comp/display-type])]
    ($ :ul.filters
       ($ :li ($ :a
                 (vw/attributes-effect
                   (fn []
                     {:class [(when (= @display-type :all) "selected")]}))
                 {:href (rtfe/href :page/all-todo-items)}
                 "All"))
       ($ :li ($ :a
                 (vw/attributes-effect
                   (fn []
                     {:class [(when (= @display-type :active) "selected")]}))
                 {:href (rtfe/href :page/active-todo-items)}
                 "Active"))
       ($ :li ($ :a
                 (vw/attributes-effect
                   (fn []
                     {:class [(when (= @display-type :completed) "selected")]}))
                 {:href (rtfe/href :page/completed-todo-items)}
                 "Completed")))))

(defn clear-completed-button []
  (let [complemented-todo-items-to-clear (rf/subscribe [:complemented-todo-items-to-clear])]
    ($ :button.clear-completed
       {:on-click (fn [_]
                    (rf/dispatch [:clean-completed-todo-items]))}
       (vw/attributes-effect
          (fn []
            {:style {:display (if @complemented-todo-items-to-clear "inline" "none")}}))
       "Clear completed")))

(defn main-page []
  (let [any-todo-item (rf/subscribe [:comp.input/any-todo-item])]
    ($ :div
       ($ :section.todoapp
          ($ :header.header
             ($ :h1 "todos")
             ($ todo-item-creation-input))
          ($ :div (vw/attributes-effect
                    (fn []
                      {:style {:display (if @any-todo-item "inline" "none")}}))
             ($ :section.main
                ($ toggle-items-button)
                ($ todo-items-list))
             ($ :footer.footer
                ($ todo-items-count)
                ($ todo-items-filters)
                ($ clear-completed-button))))
       ($ :footer.info
          ($ :p "Double-click to edit a todo")))))
