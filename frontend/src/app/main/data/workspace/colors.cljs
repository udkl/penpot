;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) UXBOX Labs SL

(ns app.main.data.workspace.colors
  (:require
   [app.common.colors :as colors]
   [app.common.data :as d]
   [app.common.pages.helpers :as cph]
   [app.main.data.modal :as md]
   [app.main.data.workspace.changes :as dch]
   [app.main.data.workspace.layout :as layout]
   [app.main.data.workspace.libraries :as dwl]
   [app.main.data.workspace.state-helpers :as wsh]
   [app.main.data.workspace.texts :as dwt]
   [app.util.color :as uc]
   [beicon.core :as rx]
   [potok.core :as ptk]))

(defn change-palette-selected
  "Change the library used by the general palette tool"
  [selected]
  (ptk/reify ::change-palette-selected
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-global :selected-palette] selected))

    ptk/EffectEvent
    (effect [_ state _]
      (let [wglobal (:workspace-global state)]
        (layout/persist-layout-state! wglobal)))))

(defn show-palette
  "Show the palette tool and change the library it uses"
  [selected]
  (ptk/reify ::show-palette
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-global :selected-palette] selected))

    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (layout/toggle-layout-flag :colorpalette :force? true)))

    ptk/EffectEvent
    (effect [_ state _]
      (let [wglobal (:workspace-global state)]
        (layout/persist-layout-state! wglobal)))))

(defn start-picker
  []
  (ptk/reify ::start-picker
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:workspace-global :picking-color?] true)))))

(defn stop-picker
  []
  (ptk/reify ::stop-picker
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (update :workspace-global dissoc :picked-color-select :picked-shift?)
          (assoc-in [:workspace-global :picking-color?] false)))))

(defn pick-color
  [rgba]
  (ptk/reify ::pick-color
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:workspace-global :picked-color] rgba)))))

(defn pick-color-select
  [value shift?]
  (ptk/reify ::pick-color-select
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:workspace-global :picked-color-select] value)
          (assoc-in [:workspace-global :picked-shift?] shift?)))))

(defn transform-fill
  [state ids color transform]
  (let [objects   (wsh/lookup-page-objects state)

        is-text?  #(= :text (:type (get objects %)))
        text-ids  (filter is-text? ids)
        shape-ids (remove is-text? ids)

        attrs
        (cond-> {}
          (contains? color :color)
          (assoc :fill-color (:color color))

          (contains? color :id)
          (assoc :fill-color-ref-id (:id color))

          (contains? color :file-id)
          (assoc :fill-color-ref-file (:file-id color))

          (contains? color :gradient)
          (assoc :fill-color-gradient (:gradient color))

          (contains? color :opacity)
          (assoc :fill-opacity (:opacity color))

          :always
          (d/without-nils))

        transform-attrs #(transform % attrs)]

    (rx/concat
     (rx/from (map #(dwt/update-text-with-function % transform-attrs) text-ids))
     (rx/of (dch/update-shapes shape-ids transform-attrs)))))

(defn swap-attrs [shape attr index new-index]
  (let [first (get-in shape [attr index])
        second (get-in shape [attr new-index])]
    (-> shape
        (assoc-in [attr index] second)
        (assoc-in [attr new-index] first))))

(defn reorder-fills
  [ids index new-index]
  (ptk/reify ::reorder-fills
    ptk/WatchEvent
    (watch [_ state _]
      (let [objects   (wsh/lookup-page-objects state)

            is-text?  #(= :text (:type (get objects %)))
            text-ids  (filter is-text? ids)
            shape-ids (remove is-text? ids)
            transform-attrs #(swap-attrs % :fills index new-index)]

        (rx/concat
         (rx/from (map #(dwt/update-text-with-function % transform-attrs) text-ids))
         (rx/of (dch/update-shapes shape-ids transform-attrs)))))))

(defn change-fill
  [ids color position]
  (ptk/reify ::change-fill
    ptk/WatchEvent
    (watch [_ state _]
      (let [change-fn (fn [shape attrs]
                        (-> shape
                            (cond-> (not (contains? shape :fills))
                              (assoc :fills []))
                            (assoc-in [:fills position] (into {} attrs))))]
        (transform-fill state ids color change-fn)))))

(defn change-fill-and-clear
  [ids color]
  (ptk/reify ::change-fill-and-clear
    ptk/WatchEvent
    (watch [_ state _]
      (let [set (fn [shape attrs] (assoc shape :fills [attrs]))]
        (transform-fill state ids color set)))))

(defn add-fill
  [ids color]
  (ptk/reify ::add-fill
    ptk/WatchEvent
    (watch [_ state _]
      (let [add (fn [shape attrs]
                  (-> shape
                      (update :fills #(into [attrs] %))))]
        (transform-fill state ids color add)))))

(defn remove-fill
  [ids color position]
  (ptk/reify ::remove-fill
    ptk/WatchEvent
    (watch [_ state _]
      (let [remove-fill-by-index (fn [values index] (->> (d/enumerate values)
                                                         (filterv (fn [[idx _]] (not= idx index)))
                                                         (mapv second)))

            remove (fn [shape _] (update shape :fills remove-fill-by-index position))]
        (transform-fill state ids color remove)))))

(defn remove-all-fills
  [ids color]
  (ptk/reify ::remove-all-fills
    ptk/WatchEvent
    (watch [_ state _]
      (let [remove-all (fn [shape _] (assoc shape :fills []))]
        (transform-fill state ids color remove-all)))))


(defn change-hide-fill-on-export
  [ids hide-fill-on-export]
  (ptk/reify ::change-hide-fill-on-export
    ptk/WatchEvent
    (watch [_ state _]
      (let [page-id   (:current-page-id state)
            objects   (wsh/lookup-page-objects state page-id)
            is-text?  #(= :text (:type (get objects %)))
            shape-ids (filter (complement is-text?) ids)
            attrs {:hide-fill-on-export hide-fill-on-export}]
        (rx/of (dch/update-shapes shape-ids (fn [shape]
                                              (if (= (:type shape) :frame)
                                                (d/merge shape attrs)
                                                shape))))))))

(defn change-stroke
  [ids attrs index]
  (ptk/reify ::change-stroke
    ptk/WatchEvent
    (watch [_ _ _]
      (let [color-attrs (cond-> {}
                          (contains? attrs :color)
                          (assoc :stroke-color (:color attrs))

                          (contains? attrs :id)
                          (assoc :stroke-color-ref-id (:id attrs))

                          (contains? attrs :file-id)
                          (assoc :stroke-color-ref-file (:file-id attrs))

                          (contains? attrs :gradient)
                          (assoc :stroke-color-gradient (:gradient attrs))

                          (contains? attrs :opacity)
                          (assoc :stroke-opacity (:opacity attrs)))

            attrs (merge attrs color-attrs)]

        (rx/of (dch/update-shapes ids (fn [shape]
                                        (let [new-attrs (merge (get-in shape [:strokes index]) attrs)
                                              new-attrs (cond-> new-attrs
                                                          (not (contains? new-attrs :stroke-width))
                                                          (assoc :stroke-width 1)

                                                          (not (contains? new-attrs :stroke-style))
                                                          (assoc :stroke-style :solid)

                                                          (not (contains? new-attrs :stroke-alignment))
                                                          (assoc :stroke-alignment :center)

                                                          :always
                                                          (d/without-nils))]
                                          (-> shape
                                              (cond-> (not (contains? shape :strokes))
                                                (assoc :strokes []))
                                              (assoc-in [:strokes index] new-attrs))))))))))

(defn change-shadow
  [ids attrs index]
  (ptk/reify ::change-shadow
    ptk/WatchEvent
    (watch [_ _ _]
      (rx/of (dch/update-shapes ids (fn [shape]
                                      (let [new-attrs (merge (get-in shape [:shadow index :color]) attrs)]
                                        (assoc-in shape [:shadow index :color] new-attrs))))))))

(defn add-stroke
  [ids stroke]
  (ptk/reify ::add-stroke
    ptk/WatchEvent
    (watch [_ _ _]
      (let [add (fn [shape attrs] (assoc shape :strokes (into [attrs] (:strokes shape))))]
        (rx/of (dch/update-shapes
                ids
                #(add % stroke)))))))

(defn remove-stroke
  [ids position]
  (ptk/reify ::remove-stroke
    ptk/WatchEvent
    (watch [_ _ _]
      (let [remove-fill-by-index (fn [values index] (->> (d/enumerate values)
                                                         (filterv (fn [[idx _]] (not= idx index)))
                                                         (mapv second)))

            remove (fn [shape] (update shape :strokes remove-fill-by-index position))]
        (rx/of (dch/update-shapes
                ids
                #(remove %)))))))

(defn remove-all-strokes
  [ids]
  (ptk/reify ::remove-all-strokes
    ptk/WatchEvent
    (watch [_ _ _]
      (let [remove-all (fn [shape] (assoc shape :strokes []))]
        (rx/of (dch/update-shapes
                ids
                #(remove-all %)))))))

(defn reorder-strokes
  [ids index new-index]
  (ptk/reify ::reorder-strokes
    ptk/WatchEvent
    (watch [_ _ _]
           (rx/of (dch/update-shapes
                   ids
                   #(swap-attrs % :strokes index new-index))))))

(defn picker-for-selected-shape
  []
  (let [sub (rx/subject)]
    (ptk/reify ::picker-for-selected-shape
      ptk/WatchEvent
      (watch [_ state stream]
        (let [ids   (wsh/lookup-selected state)
              stop? (rx/filter (ptk/type? ::stop-picker) stream)

              update-events
              (fn [color]
                (rx/of (change-fill ids color 0)))]

          (rx/merge
           ;; Stream that updates the stroke/width and stops if `esc` pressed
           (->> sub
                (rx/take-until stop?)
                (rx/flat-map update-events))

           ;; Hide the modal if the stop event is emitted
           (->> stop?
                (rx/take 1)
                (rx/map #(md/hide))))))

      ptk/UpdateEvent
      (update [_ state]
        (let [handle-change-color (fn [color] (rx/push! sub color))]
          (-> state
              (assoc-in [:workspace-global :picking-color?] true)
              (assoc ::md/modal {:id (random-uuid)
                                 :data {:color colors/black :opacity 1}
                                 :type :colorpicker
                                 :props {:on-change handle-change-color}
                                 :allow-click-outside true})))))))

(defn color-att->text
  [color]
  {:fill-color (:color color)
   :fill-opacity (:opacity color)
   :fill-color-ref-id (:id color)
   :fill-color-ref-file (:file-id color)
   :fill-color-gradient (:gradient color)})

(defn change-text-color
  [old-color new-color index node]
  (let [fills (:fills node)
        parsed-color (d/without-nils (color-att->text old-color))
        parsed-new-color (d/without-nils (color-att->text new-color))
        has-color? (d/index-of fills parsed-color)]
    (cond-> node
      (some? has-color?)
      (assoc-in [:fills index] parsed-new-color))))

(defn change-color-in-selected
  [new-color shapes-by-color old-color]
  (ptk/reify ::change-color-in-selected
    ptk/WatchEvent
    (watch [_ _ _]
      (->> (rx/from shapes-by-color)
           (rx/map (fn [shape] (case (:prop shape)
                                 :fill (change-fill [(:shape-id shape)] new-color (:index shape))
                                 :stroke (change-stroke [(:shape-id shape)] new-color (:index shape))
                                 :shadow (change-shadow [(:shape-id shape)] new-color (:index shape))
                                 :content (dwt/update-text-with-function
                                           (:shape-id shape)
                                           (partial change-text-color old-color new-color (:index shape))))))))))

(defn apply-color-from-palette
  [color is-alt?]
  (ptk/reify ::apply-color-from-palette
    ptk/WatchEvent
    (watch [_ state _]
      (let [objects  (wsh/lookup-page-objects state)
            selected (->> (wsh/lookup-selected state)
                          (cph/clean-loops objects))
            selected-obj (keep (d/getf objects) selected)
            select-shapes-for-color (fn [shape objects]
                                      (let [shapes (case (:type shape)
                                                     :group (cph/get-children objects (:id shape))
                                                     [shape])]
                                        (->> shapes
                                             (remove cph/group-shape?)
                                             (map :id))))
            ids (mapcat #(select-shapes-for-color % objects) selected-obj)]
        (if is-alt?
          (rx/of (change-stroke ids (merge uc/empty-color color) 0))
          (rx/of (change-fill ids (merge uc/empty-color color) 0)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COLORPICKER STATE MANAGEMENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn split-color-components
  [{:keys [color opacity] :as data}]
  (let [value (if (uc/hex? color) color colors/black)
        [r g b] (uc/hex->rgb value)
        [h s v] (uc/hex->hsv value)]
    (merge data
           {:hex (or value "000000")
            :alpha (or opacity 1)
            :r r :g g :b b
            :h h :s s :v v})))

(defn materialize-color-components
  [{:keys [hex alpha] :as data}]
  (-> data
      (assoc :color hex)
      (assoc :opacity alpha)))

(defn clear-color-components
  [data]
  (dissoc data :hex :alpha :r :g :b :h :s :v))

(defn- create-gradient
  [type]
  {:start-x 0.5
   :start-y (if (= type :linear-gradient) 0.0 0.5)
   :end-x   0.5
   :end-y   1
   :width  1.0})

(defn get-color-from-colorpicker-state
  [{:keys [type current-color stops gradient] :as state}]
  (if (= type :color)
    (clear-color-components current-color)
    {:gradient (-> gradient
                   (assoc :type (case type
                                  :linear-gradient :linear
                                  :radial-gradient :radial))
                   (assoc :stops (mapv clear-color-components stops))
                   (dissoc :shape-id))}))

(defn- colorpicker-onchange-runner
  "Effect event that runs the on-change callback with the latest
  colorpicker state converted to color object."
  [on-change]
  (ptk/reify ::colorpicker-onchange-runner
    ptk/WatchEvent
    (watch [_ state _]
      (when-let [color (some-> state :colorpicker get-color-from-colorpicker-state)]
        (on-change color)
        (rx/of (dwl/add-recent-color color))))))

(defn initialize-colorpicker
  [on-change]
  (ptk/reify ::initialize-colorpicker
    ptk/WatchEvent
    (watch [_ _ stream]
      (let [stoper (rx/merge
                    (rx/filter (ptk/type? ::finalize-colorpicker) stream)
                    (rx/filter (ptk/type? ::initialize-colorpicker) stream))]

        (->> (rx/merge
              (->> stream
                   (rx/filter (ptk/type? ::update-colorpicker-gradient))
                   (rx/debounce 200))
              (rx/filter (ptk/type? ::update-colorpicker-color) stream)
              (rx/filter (ptk/type? ::activate-colorpicker-gradient) stream))
             (rx/map (constantly (colorpicker-onchange-runner on-change)))
             (rx/take-until stoper))))))

(defn finalize-colorpicker
  []
  (ptk/reify ::finalize-colorpicker
    ptk/UpdateEvent
    (update [_ state]
      (dissoc state :colorpicker))))

(defn update-colorpicker
  [{:keys [gradient] :as data}]
  (ptk/reify ::update-colorpicker
    ptk/UpdateEvent
    (update [_ state]
      (let [shape-id (-> state wsh/lookup-selected first)]
        (update state :colorpicker
                (fn [state]
                  (if (some? gradient)
                    (let [stop  (or (:editing-stop state) 0)
                          stops (mapv split-color-components (:stops gradient))
                          type  (case (:type gradient)
                                  :linear :linear-gradient
                                  :radial :radial-gradient)]
                      (-> state
                          (assoc :type type)
                          (assoc :current-color (nth stops stop))
                          (assoc :stops stops)
                          (assoc :gradient (-> gradient
                                               (dissoc :stops)
                                               (assoc :shape-id shape-id)))
                          (assoc :editing-stop stop)))

                    (-> state
                        (assoc :type :color)
                        (assoc :current-color (split-color-components (dissoc data :gradient)))
                        (dissoc :editing-stop)
                        (dissoc :gradient)
                        (dissoc :stops)))))))))

(defn update-colorpicker-color
  [changes]
  (ptk/reify ::update-colorpicker-color
    ptk/UpdateEvent
    (update [_ state]
      (update state :colorpicker
              (fn [state]
                (let [state (-> state
                                (update :current-color merge changes)
                                (update :current-color materialize-color-components))]
                  (if-let [stop (:editing-stop state)]
                    (update-in state [:stops stop] (fn [data] (->> changes
                                                                   (merge data)
                                                                   (materialize-color-components))))
                    (-> state
                        (assoc :type :color)
                        (dissoc :gradient :stops :editing-stop)))))))))

(defn update-colorpicker-gradient
  [changes]
  (ptk/reify ::update-colorpicker-gradient
    ptk/UpdateEvent
    (update [_ state]
      (update-in state [:colorpicker :gradient] merge changes))))

(defn select-colorpicker-gradient-stop
  [stop]
  (ptk/reify ::select-colorpicket-gradient-stop
    ptk/UpdateEvent
    (update [_ state]
      (update state :colorpicker
              (fn [state]
                (if-let [color (get-in state [:stops stop])]
                  (assoc state
                         :current-color color
                         :editing-stop stop)
                  state))))))

(defn activate-colorpicker-gradient
  [type]
  (ptk/reify ::activate-colorpicker-gradient
    ptk/UpdateEvent
    (update [_ state]
      (update state :colorpicker
              (fn [state]
                (if (= type (:type state))
                  (do
                    (-> state
                        (assoc :type :color)
                        (dissoc :editing-stop :stops :gradient)))
                  (let [gradient (create-gradient type)]
                    (-> state
                        (assoc :type type)
                        (assoc :gradient gradient)
                        (cond-> (not (:stops state))
                          (assoc :editing-stop 0
                                 :stops  {0 (:current-color state)
                                          1 (-> (:current-color state)
                                                (assoc :alpha 0))}))))))))))
