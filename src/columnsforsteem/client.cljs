(ns columnsforsteem.client
  (:require [cljsjs.material-ui]
            [cljs-react-material-ui.core :refer [get-mui-theme color]]
            [cljs-react-material-ui.reagent :as ui]
            [cljs-react-material-ui.icons :as ic]
            [reagent.core :as r]))

(defonce column-index (atom 0))

(defonce
  app-state
  (r/atom {:drawer-open false
           :settings {:hide-nsfw true
                      :dark-mode true}
           :columns [(r/atom {:path "created" :tag "technology"})
                     (r/atom {:path "created" :tag "news"})
                     (r/atom {:path "hot"})
                     (r/atom {:path "blog" :tag "crypticwyrm"})]}))

(defn load-settings []
  (let [settings (r/cursor app-state [:settings])
        loaded (js/JSON.parse (.getItem js/localStorage "settingscfs"))]
    (if (nil? loaded)
      nil
      (reset! settings (js->clj loaded :keywordize-keys true)))))

(defn save-settings []
  (let [settings (r/cursor app-state [:settings])]
    (.setItem js/localStorage "settingscfs" (js/JSON.stringify (clj->js @settings)))))

(defn load-columns []
  (let [columns (r/cursor app-state [:columns])
        loaded (js/JSON.parse (.getItem js/localStorage "columns"))]
    (if (nil? loaded)
      nil
      (reset! columns (mapv
                        (fn [col]
                          (r/atom col))
                        (js->clj loaded :keywordize-keys true))))))

(defn save-columns []
  (let [columns (r/cursor app-state [:columns])
        condensed (mapv
                    (fn [column] {:path (:path @column)
                                  :tag (:tag @column)})
                    @columns)]
    (.setItem js/localStorage "columns" (js/JSON.stringify (clj->js condensed)))))

(def image-regex
  #"(?i)https?://((?!http)[^\s])*?\.(jpe?g|png|gif)(\?[A-Za-z0-9!$&'()*+.,;=]*\b)?")
  
(defn parseImageUrl [post]
  (if-let [result
           (if (empty? (get post "json_metadata"))
             (first
               (re-find
                 image-regex
                 (get post "body")))
             (let [parsed (js/JSON.parse (get post "json_metadata"))
                   meta (js->clj parsed)
                   images (get meta "image")]
               (if-not (or (nil? images)
                           (empty? images)
                           (empty? (first images)))
                 (first images)
                 (first
                   (re-find
                     image-regex
                     (get post "body"))))))]
    (clojure.string/replace result "&amp;" "&")
    nil))

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

(defn getDiscussions [path tag limit & {:keys [callback paging]}]
  (.then
    (js/steem.database.getDiscussions
      path
      (if (nil? paging)
        (clj->js {:limit limit
                  :tag tag
                  :truncate_body 5000})
        (clj->js {:limit limit
                  :tag tag
                  :truncate_body 5000
                  :start_author (:start-author paging)
                  :start_permlink (:start-permlink paging)})))
    (fn [result]
      (if callback (callback result)))
    (fn [e]
      (js/console.log "getDiscussions error: " e)
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
                                    (js/clearInterval @scroll-interval))))
                              15))))

(defn all-images [posts]
  (filter #(not (empty? %))
    (map (fn [post]
           (if-let [image (parseImageUrl post)]
             (cached-image image)
             nil))
      posts)))

(defn preload-images [images callback]
  (let [left (atom (count images))
        preloaded (atom [])]
    (if (empty? images)
      (callback preloaded)
      (doseq [image-link images]
        (let [image (.createElement js/document "img")]
          (set! (.-onerror image) (fn []
                                    (swap! left dec)
                                    (if (<= @left 0) (callback preloaded))))
          (set! (.-onload image) (fn []
                                   (swap! left dec)
                                   (if (<= @left 0) (callback preloaded))))
          (set! (.-src image) image-link)
          (swap! preloaded conj image))))))

(defn load-column [column & {:keys [forced second path]}]
  (when (or forced
            second
            (and (not (:loading @column))
                 (not (:loading-bottom @column))
                 (= 0 (.-scrollTop (.querySelector
                                     (.querySelector js/document (str "#" (:element @column)))
                                     ".scroll-view")))))
    (swap! column assoc :last-loaded (js/Date.)
      :loading true)
    (let [loaded-path (if (nil? path)
                        (:path @column)
                        path)]
      (getDiscussions
        (:path @column)
        (if-let [tag (:tag @column)]
          tag
          "")
        (if (or forced
                second
                (empty? (:data @column))
                (not (or (= "created" (:path @column))
                         (= "blog" (:path @column))
                         (= "feed" (:path @column)))))
          25
          1)
        :callback
        (fn [result]
          (when (= loaded-path (:path @column))
            (let [parsed (js->clj result)
                  first-parsed (first parsed)
                  last-top (if (first (:data @column))
                             (get (first (:data @column)) "id")
                             nil)]
              (if (or forced
                      second
                      (empty? (:data @column))
                      (not (or (= "created" (:path @column))
                               (= "blog" (:path @column))
                               (= "feed" (:path @column)))))
                (preload-images
                  (all-images parsed)
                  (fn [preloaded]
                    (when (= loaded-path (:path @column))
                      (swap! column assoc :loading false)
                      (let [column-element (.querySelector
                                             js/document (str "#" (:element @column)))
                            scroll-view (.querySelector column-element ".scroll-view")]
                        (when (or forced
                                  (= 0 (.-scrollTop scroll-view)))
                          (swap! column assoc :images @preloaded
                            :data parsed)
                          (if (and (or (= "created" (:path @column))
                                       (= "blog" (:path @column))
                                       (= "feed" (:path @column)))
                                   last-top
                                   (not (= last-top (get first-parsed "id"))))
                            (js/setTimeout
                              (fn []
                                (.scrollIntoView
                                  (.querySelector column-element (str "#post-" last-top)))
                                (scroll-element scroll-view 500))
                              100)))))))
                (if-not (= last-top (get first-parsed "id"))
                  (do
                    (load-column column :second true
                      :path loaded-path))
                  (swap! column assoc :loading false))))))))))

(defn load-column-bottom [column]
  (if (and (> (count (:data @column)) 0)
           (not (:loading @column))
           (not (:loading-bottom @column))
           (or (nil? (:more @column))
               (:more @column)))
    (do
      (swap! column assoc :loading-bottom true)
      (let [loaded-path (:path @column)]
        (getDiscussions
          (:path @column)
          (if-let [tag (:tag @column)]
            tag
            "")
          25
          :paging {:start-author (get (last (:data @column)) "author")
                   :start-permlink (get (last (:data @column)) "permlink")}
          :callback
          (fn [result]
            (when (= loaded-path (:path @column))
              (let [parsed (js->clj result)]
                (preload-images
                  (all-images parsed)
                  (fn [preloaded]
                    (when (= loaded-path (:path @column))
                      (swap! column update-in [:images] into @preloaded)
                      (swap! column update-in [:data] into (rest parsed))
                      (swap! column assoc :loading-bottom false
                        :more (= (count parsed) 25)))))))))))))

(defn post-card [item]
  (let [settings (r/cursor app-state [:settings])
        metadata (js->clj (js/JSON.parse (get item "json_metadata")))]
    (fn [item]
      (:dark-mode @settings) ; hack to make posts rerender when toggling theme
      [ui/card {:id (str "post-" (get item "id"))
                :container-style {:margin-bottom 10}}
       [ui/card-header {:title (get item "author")
                        :avatar (avatar-url (get item "author"))
                        :subtitle (format-time (get item "created"))}]
       (if-let [image (parseImageUrl item)]
         (when-not (and (:hide-nsfw @settings)
                        (> (count (filter #(= % "nsfw") (get metadata "tags"))) 0))
           [ui/card-media
            [:img {:src (cached-image image)}]]))
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
       [ui/card-actions {:style {:display "flex"
                                 :justify-content "center"}}
        [:a {:target "_blank"
             :href (str "https://www.steemit.com" (get item "url"))}
         [ui/flat-button {:label "Read on Steemit"}]]]])))

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
     (let [settings (r/cursor app-state [:settings])
           scroll-view (atom nil)
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
                 :style {:background (if (:dark-mode @settings)
                                       (color :grey800)
                                       (color :blue500))
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
            (if (or (= "blog" (:path @column))
                    (= "feed" (:path @column)))
              [ui/drop-down-menu {:value (:path @column)
                                  :on-change (fn [e key value]
                                               (when-not (= value (:path @column))
                                                 (swap! column assoc :data []
                                                                     :path value
                                                                     :more nil)
                                                 (load-column column :forced true)
                                                 (save-columns)))
                                  :style {:background (if (:dark-mode @settings)
                                                        (color :grey900)
                                                        (color :blue300))
                                          :height 28}
                                  :underline-style {:display "none"}
                                  :icon-style {:display "none"}
                                  :label-style {:padding-left 24
                                                :padding-right 24
                                                :height 28
                                                :line-height "28px"}}
               [ui/menu-item {:value "blog"
                              :primary-text "Blog"}]
               [ui/menu-item {:value "feed"
                              :primary-text "Feed"}]]
              [ui/drop-down-menu {:value (:path @column)
                                  :on-change (fn [e key value]
                                               (when-not (= value (:path @column))
                                                 (swap! column assoc :data []
                                                                     :path value
                                                                     :more nil)
                                                 (load-column column :forced true)
                                                 (save-columns)))
                                  :style {:background (if (:dark-mode @settings)
                                                        (color :grey900)
                                                        (color :blue300))
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
                        :background-color (if (:dark-mode @settings)
                                            (color :grey700)
                                            (color :blue300))
                        :style {:margin-left 10}}
               (if (or (= "blog" (:path @column))
                       (= "feed" (:path @column)))
                 [ui/avatar {:src (avatar-url (:tag @column))
                             :style {:width 24
                                     :height 24}}]
                 [ui/avatar {:icon (r/as-element [ui/font-icon "#"])
                             :size 24
                             :color (color :blue500)
                             :style {:width 24
                                     :height 24
                                     :line-height "24px"
                                     :background-color (color :blue200)}}])
               (:tag @column)])]
           [ui/icon-button {:tooltip "Close this column"
                            :tooltip-position "bottom-left"
                            :style {:padding 0
                                    :width "auto"
                                    :height "auto"}
                            :on-click remove-fn}
            [ic/navigation-close]]]
          [:div {:style {:overflow "hidden"
                         :flex 1
                         :display "flex"
                         :flex-direction "column"}}
           [:div {:class "scroll-view"
                  :ref (fn [el]
                         (reset! scroll-view el))
                  :on-scroll (fn []
                               (when
                                 (>
                                    (+ (.-scrollTop @scroll-view)
                                       (.-clientHeight @scroll-view)
                                       50)
                                    (.-scrollHeight @scroll-view))
                                 (load-column-bottom column)))
                  :style {:flex 1
                          :overflow-y "auto"
                          :overflow-x "hidden"}}
            [:div {:style {:padding 10}}
             [:div {:style {:position "relative"
                            :width 40
                            :margin-left "auto"
                            :margin-right "auto"}}
              [ui/refresh-indicator {:top 0
                                     :left 0
                                     :status (if (and (:loading @column)
                                                      (empty? (:data @column)))
                                               "loading"
                                               "hide")}]]
             (for [item (:data @column)]
               ^{:key (get item "id")}
               [post-card item])
             [:div {:style {:position "relative"
                            :width 40
                            :margin-left "auto"
                            :margin-right "auto"}}
              [ui/refresh-indicator {:top 0
                                     :left 0
                                     :status (if (or (:loading-bottom @column)
                                                     (:more @column))
                                               "loading"
                                               "hide")}]]]]]]))}))

(defn remove-column [column]
  (let [columns (r/cursor app-state [:columns])]
    (swap! columns (fn [old]
                     (filterv #(not (= % column)) old)))
    (save-columns)))

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
                      :else text)}))
    (save-columns)))

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

(defn drawer-component []
  (let [settings (r/cursor app-state [:settings])]
    (fn []
      [ui/drawer {:open (:drawer-open @app-state)
                  :z-depth 1}
       [ui/list
        [ui/subheader "Settings"]
        [ui/list-item {:primary-text "Hide NSFW images"
                       :right-toggle
                         (r/as-element
                           [ui/toggle
                            {:toggled (:hide-nsfw @settings)
                             :on-toggle (fn [e toggled]
                                          (swap! settings assoc :hide-nsfw toggled)
                                          (save-settings))}])}]
        [ui/list-item {:primary-text "Dark theme"
                       :right-toggle
                         (r/as-element
                           [ui/toggle
                            {:toggled (:dark-mode @settings)
                             :on-toggle (fn [e toggled]
                                          (swap! settings assoc :dark-mode toggled)
                                          (save-settings))}])}]
        [ui/subheader "Info"]
        [:a {:target "_blank"
             :href "https://www.steemit.com"
             :style {:text-decoration "none"}}
         [ui/list-item
          [:b "Columns for Steem"]
          " is a client for the decentralized blockchain based
          social network Steem. The content you see in the application
          is generated by Steem users and is not hosted on this website.
          Click for more information."]]
        [ui/list-item
         "This application is under heavy development, use it on your own
         risk, I disclaim any legal responsibility to the extent possible by
         applicable law."]
        [ui/subheader "Contact"]
        [:a {:target "_blank"
             :href "https://www.steemit.com/@crypticwyrm"
             :style {:text-decoration "none"}}
         [ui/list-item
          "@crypticwyrm on Steem"]]]])))

(defn content []
  (let [columns (r/cursor app-state [:columns])
        settings (r/cursor app-state [:settings])
        show-column-dialog (r/atom false)]
    (fn []
      [ui/mui-theme-provider
       (if (:dark-mode @settings)
         {:mui-theme (get-mui-theme
                       (assoc (js->clj
                                (aget js/MaterialUIStyles "DarkRawTheme")
                                :keywordize-keys true)
                         :palette
                         (merge
                           (:palette (js->clj
                                       (aget js/MaterialUIStyles "DarkRawTheme")
                                       :keywordize-keys true))
                           {})))}
         {:mui-theme (get-mui-theme)})
       [:div {:style {:display "flex"
                      :flex 1
                      :overflow "hidden"}}
        [drawer-component]
        [:div {:id "content-wrapper"
               :style {:display "flex"
                       :flex-direction "column"
                       :flex 1
                       :margin-left (when (:drawer-open @app-state) 256)
                       :overflow "hidden"}}
         [ui/mui-theme-provider
          (if (:dark-mode @settings)
            {:mui-theme (get-mui-theme
                          (assoc (js->clj
                                   (aget js/MaterialUIStyles "DarkRawTheme")
                                   :keywordize-keys true)
                            :palette
                            (merge
                              (:palette (js->clj
                                          (aget js/MaterialUIStyles "DarkRawTheme")
                                          :keywordize-keys true))
                              {:primary1Color "#303030"
                               :alternateTextColor (color :white)})))}
            {:mui-theme (get-mui-theme)})
          [ui/app-bar {:title "Columns for Steem"
                       :on-left-icon-button-touch-tap
                       (fn []
                         (swap! app-state assoc :drawer-open (not (:drawer-open @app-state))))
                       :icon-element-right
                       (r/as-element
                         [ui/flat-button
                          {:label "Add column"
                           :on-click #(reset! show-column-dialog true)}])}]]
         [column-dialog show-column-dialog]
         [ui/paper {:id "columns"
                    :rounded false
                    :style {:display "flex"
                            :flex-direction "row"
                            :overflow "hidden"
                            :overflow-x "auto"
                            :flex 1}}
          (for [[index column] (map-indexed vector @columns)]
            ^{:key index}
            [column-component column #(remove-column column)])]]]])))

(defonce initial-startup (atom false))

(when-not @initial-startup
  (load-settings)
  (load-columns)
  (reset! initial-startup true))

; tells reagent to begin rendering
(r/render-component [content]
  (.querySelector js/document "#app"))

(defonce refresh-interval (atom nil))

(when (nil? @refresh-interval)
  (let [columns (r/cursor app-state [:columns])]
    (reset! refresh-interval
      (js/setInterval
        (fn []
          (doseq [column @columns]
            (let [now (js/Date.)]
              (if (> (- now (:last-loaded @column)) 60000)
                (load-column column)))))
        10000))))
