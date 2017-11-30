(ns columnsforsteem.client
  (:require [cljsjs.material-ui]
            [cljs-react-material-ui.core :refer [get-mui-theme color]]
            [cljs-react-material-ui.reagent :as ui]
            [cljs-react-material-ui.icons :as ic]
            [reagent.core :as r]))

(enable-console-print!)

(defonce column-index (atom 0))

(defonce
  app-state
  (r/atom {:columns [(r/atom {:path "trending"})
                     (r/atom {:path "hot"})]}))

(defn parseImageUrl [post]
  (if (empty? (get post "json_metadata"))
    nil
    (let [parsed (js/JSON.parse (get post "json_metadata"))
          meta (js->clj parsed)
          images (get meta "image")]
      (if-not (or (nil? images)
                  (empty? images)
                  (empty? (first images)))
        (first images)
        nil))))

(defn avatar-url [user]
  (str "https://steemitimages.com/u/" user "/avatar/small"))

(defn cached-image [url]
  (str "https://steemitimages.com/640x480/" url))

(defn format-time [time]
  (.toLocaleString (js/Date. (str time "Z"))))

(defn is-post-active [post]
  (let [cashout (js/Date. (get post "cashout_time"))
        now (js/Date.)]
    (> (- cashout now) 0)))

(defn getDiscussions [path tag & {:keys [callback]}]
  (.then
    (js/steem.database.getDiscussions
      path
      (clj->js {:limit 25
                :tag tag}))
    (fn [result]
      (js/console.log result)
      (if callback (callback result)))
    (fn [e]
      (js/console.log "getDiscussions error")
      (js/console.log e)
      (if callback (callback nil)))))

(defn scroll-element [el duration]
  (let [scroll-step-temp (/ (- (.-scrollTop el)) (/ duration 15))
        scroll-step (if (< scroll-step-temp -1)
                      scroll-step-temp
                      -1)
        scroll-interval (atom nil)]
    (reset! scroll-interval (js/setInterval
                              (fn []
                                (if (> (.-scrollTop el) 0)
                                  (.scrollBy el 0 scroll-step)
                                  (do
                                    (js/console.log "scroll finished")
                                    (js/clearInterval @scroll-interval))))
                              15))))

(defn all-images [posts]
  (filter #(not (empty? %))
    (map (fn [post]
           (let [metadata (js->clj (js/JSON.parse (get post "json_metadata")))
                 image (first (get metadata "image"))]
             (if-not (empty? image)
               (cached-image image)
               nil)))
      posts)))

(defn preload-images [images callback]
  (let [left (atom (count images))
        preloaded (atom [])]
    (doseq [image-link images]
      (let [image (.createElement js/document "img")]
        (set! (.-onerror image) (fn []
                                  (swap! left dec)
                                  (if (= 0 @left) (callback preloaded))))
        (set! (.-onload image) (fn []
                                 (swap! left dec)
                                 (if (= 0 @left) (callback preloaded))))
        (set! (.-src image) image-link)
        (swap! preloaded conj image)))))

(defn load-column [column]
  (getDiscussions
    (:path @column)
    (if-let [tag (:tag @column)]
      tag
      "")
    :callback
    (fn [result]
      (println "Callback called")
      (let [parsed (js->clj result)
            first-parsed (first parsed)
            last-top (if (first (:data @column))
                       (get (first (:data @column)) "id")
                       nil)]
        (preload-images
          (all-images parsed)
          (fn [preloaded]
            (swap! column assoc :images preloaded)
            (swap! column assoc :data parsed)
            (if (and last-top
                     (not (= last-top (get first-parsed "id"))))
              (js/setTimeout
                (fn []
                  (let [column-element (.querySelector js/document (str "#" (:element @column)))]
                    (js/console.log column-element)
                    (.scrollIntoView
                      (.querySelector column-element (str "#post-" last-top)))
                    (scroll-element (.querySelector column-element ".scroll-view") 500)))
                100))))))))

(defn column-component [column remove-fn]
  (r/create-class
    {:component-will-mount
     (fn [this]
       (let [column (first (next (r/argv this)))]
         (swap! column-index inc)
         (swap! column assoc :element (str "column-" @column-index))))
     :component-did-mount
     (fn [this]
       (let [column (first (next (r/argv this)))]
         (when (empty? (:data @column))
           (.scrollIntoView (r/dom-node this))
           (load-column column))))
     :reagent-render
     (let [scroll-view (atom nil)
           header (atom nil)
           header-wrapper (atom nil)]
       (fn [column remove-fn]
         [ui/paper {:id (:element @column)
                    :z-depth 2
                    :style {:margin 10
                            :flex 1
                            :display "flex"
                            :flex-direction "column"
                            :overflow "hidden"
                            :min-width 300
                            :max-width 500}}
          [:div {:ref (fn [el] (reset! header el))
                 :style {:background (color :blue500)
                         :color "white"
                         :padding 10
                         :display "flex"
                         :align-items "center"}
                 :on-click (fn [e]
                             (when @scroll-view
                               (when (or (= (.-target e) @header)
                                         (= (.-target e) @header-wrapper))
                                 (scroll-element @scroll-view 500))))}
           [:div {:ref (fn [el] (reset! header-wrapper el))
                  :style {:flex 1
                          :display "flex"
                          :align-items "center"
                          :overflow "hidden"}}
            (if (= "blog" (:path @column))
              [ui/drop-down-menu {:value "created"
                                  :style {:background (color :blue300)
                                          :height 28}
                                  :underline-style {:display "none"}
                                  :icon-style {:display "none"}
                                  :label-style {:padding-left 24
                                                :padding-right 24
                                                :height 28
                                                :line-height "28px"}}
               [ui/menu-item {:value "created"
                              :primary-text "New"}]]
              [ui/drop-down-menu {:value (:path @column)
                                  :on-change (fn [e key value]
                                               (swap! column assoc :path value)
                                               (load-column column))
                                  :style {:background (color :blue300)
                                          :height 28}
                                  :underline-style {:display "none"}
                                  :icon-style {:display "none"}
                                  :label-style {:padding-left 24
                                                :padding-right 24
                                                :height 28
                                                :line-height "28px"}}
               [ui/menu-item {:value "trending"
                              :primary-text "Trending"}]
               [ui/menu-item {:value "hot"
                              :primary-text "Hot"}]
               [ui/menu-item {:value "created"
                              :primary-text "New"}]])
            (if-not (empty? (:tag @column))
              [ui/chip {:label-style {:line-height "24px"
                                      :overflow "hidden"
                                      :text-overflow "ellipsis"}
                        :label-color (color :white)
                        :background-color (color :blue300)
                        :style {:margin-left 10}}
               (if (= "blog" (:path @column))
                 [ui/avatar {:src (avatar-url (:tag @column))
                             :style {:width 24
                                     :height 24}}]
                 [ui/avatar {:icon (r/as-element [ui/font-icon "#"])
                             :size 24
                             :color (color :blue500)
                             :style {:width 24
                                     :height 24
                                     :line-height "24px"
                                     :background (color :blue200)}}])
               (:tag @column)])]
           [ui/icon-button {:tooltip "Close this column"
                            :tooltip-position "bottom-left"
                            :style {:padding 0
                                    :width "auto"
                                    :height "auto"}
                            :on-click remove-fn}
            [ic/navigation-close]]]
          [:div {:style {:overflow "hidden"
                         :flex 1}}
           [:div {:class "scroll-view"
                  :ref (fn [el]
                         (reset! scroll-view el))
                  :style {:height "100%"
                          :overflow-y "auto"
                          :overflow-x "hidden"}}
            [:div {:style {:padding 10}}
             (for [item (:data @column)]
               ^{:key (get item "id")}
               [ui/card {:id (str "post-" (get item "id"))
                         :container-style {:margin-bottom 10}}
                [ui/card-header {:title (get item "author")
                                 :avatar (avatar-url (get item "author"))
                                 :subtitle (format-time (get item "created"))}]
                (if-let [image (parseImageUrl item)]
                  [ui/card-media
                   [:img {:src (cached-image image)}]])
                [ui/card-title {:title (get item "title")
                                :title-style {:font-size 18}
                                :subtitle (clojure.string/join
                                            " "
                                            [(count (get item "active_votes"))
                                             "votes,"
                                             (get item "children")
                                             "replies,"
                                             (if (is-post-active item)
                                               (get item "pending_payout_value")
                                               (get item "total_payout_value"))])}]
                [ui/card-actions
                 [:a {:target "_blank"
                      :href (str "https://www.steemit.com" (get item "url"))}
                  [ui/flat-button {:label "Read on Steemit"}]]
                 [:a {:target "_blank"
                      :href (str "https://www.busy.org" (get item "url"))}
                  [ui/flat-button {:label "Read on Busy"}]]]])]]]]))}))

(defn remove-column [column]
  (let [columns (r/cursor app-state [:columns])]
    (swap! columns (fn [old]
                     (filterv #(not (= % column)) old)))))

(defn add-column [text]
  (let [columns (r/cursor app-state [:columns])
        coltype (str (first text))
        text-rest (apply str (rest text))]
    (swap!
      columns
      conj
      (r/atom {:path (if (= coltype "@")
                       "blog"
                       "created")
               :tag (cond
                      (= coltype "@") text-rest
                      (= coltype "#") text-rest
                      :else text)}))))

(defn has-whitespace [text]
  (boolean (re-find #"\s" text)))

(defn column-dialog [show-column-dialog]
  (let [dialog-input (r/atom "")]
    (fn [show-column-dialog]
      [ui/dialog {:title "Add a new column"
                  :open @show-column-dialog
                  :on-request-close (fn []
                                      (reset! dialog-input "")
                                      (reset! show-column-dialog false))
                  :actions
                  [(r/as-element
                     [ui/flat-button
                      {:label "Cancel"
                       :primary true
                       :on-click (fn []
                                   (reset! dialog-input "")
                                   (reset! show-column-dialog
                                     false))}])
                   (r/as-element
                     [ui/flat-button
                      {:label "Add column"
                       :primary true
                       :on-click (fn []
                                   (when (not (has-whitespace @dialog-input))
                                     (reset! show-column-dialog false)
                                     (add-column @dialog-input)
                                     (reset! dialog-input "")))}])]}
       [ui/text-field {:full-width true
                       :auto-focus true
                       :error-text (when (has-whitespace @dialog-input) "No whitespace allowed")
                       :floating-label-text
                         "#hashtag, @username or leave empty"
                       :on-change (fn [e value]
                                    (reset! dialog-input value))
                       :on-key-press (fn [e]
                                       (when (= "Enter" (.-key e))
                                         (when (not (has-whitespace @dialog-input))
                                           (reset! show-column-dialog false)
                                           (add-column @dialog-input)
                                           (reset! dialog-input ""))
                                         (.preventDefault e)))}]])))

(defn content []
  (let [columns (r/cursor app-state [:columns])
        show-column-dialog (r/atom false)]
    (fn []
      [ui/mui-theme-provider {:mui-theme (get-mui-theme)}
       [:div {:style {:display "flex"
                      :flex-direction "column"
                      :flex 1
                      :overflow "hidden"}}
        [ui/app-bar {:title "Columns for Steem"
                     :icon-element-right
                     (r/as-element
                       [ui/flat-button
                        {:label "Add column"
                         :on-click #(reset! show-column-dialog true)}])}]
        [column-dialog show-column-dialog]
        [:div {:id "columns"
               :style {:display "flex"
                       :flex-direction "row"
                       :overflow "hidden"
                       :overflow-x "auto"
                       :flex 1}}
         (for [[index column] (map-indexed vector @columns)]
           ^{:key index}
           [column-component column #(remove-column column)])]]])))

; tells reagent to begin rendering
(r/render-component [content]
  (.querySelector js/document "#app"))

(defonce refresh-interval (atom nil))


(when (nil? @refresh-interval)
  (let [columns (r/cursor app-state [:columns])]
    (reset! refresh-interval
      (js/setInterval
        (fn []
          (js/console.log "Refreshing...")
          (doseq [column @columns]
            (load-column column)))
        60000))))
