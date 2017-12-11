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
           :columns [{:id (random-uuid) :path "created" :tag "technology"}
                     {:id (random-uuid) :path "created" :tag "news"}
                     {:id (random-uuid) :path "hot"}
                     {:id (random-uuid) :path "blog" :tag "crypticwyrm"}]}))

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
      (reset! columns
        (mapv #(assoc % :id (random-uuid)) (js->clj loaded :keywordize-keys true))))))

(defn save-columns []
  (let [columns (r/cursor app-state [:columns])
        condensed (mapv
                    (fn [column] {:path (:path column)
                                  :tag (:tag column)})
                    @columns)]
    (.setItem js/localStorage "columns" (js/JSON.stringify (clj->js condensed)))))

(def image-regex
  #"(?i)https?://[^\s<>]*\.(jpe?g|png|gif)(\?[A-Za-z0-9!$&'()*+.,;=]*\b)?")

(def ipfs-regex
  #"(?i)<img src=['\"](https?://((?!http)[^\s])*?(?:(?:\.(?:jpe?g|gif|png)|ipfs/[a-z\d]{40,})))['\"]")

(defn parse-image-url [post]
  (let [metadata (get post "json_metadata")
        body (get post "body")
        result (or
                 ; try parsing json_metadata for image key
                 (when (not (empty? metadata))
                   (let [parsed (js/JSON.parse (get post "json_metadata"))
                         meta (js->clj parsed)
                         images (get meta "image")]
                     (first images)))
                 ; try image regex
                 (first (re-find image-regex body))
                 ; try ipfs regex
                 (nth (re-find ipfs-regex body) 1))]
    (when result
      (clojure.string/replace result "&amp;" "&"))))

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

(defn getAccounts [accounts & {:keys [callback]}]
  (.then
    (js/steem.database.getAccounts
      (clj->js accounts))
    (fn [result]
      (if callback (callback result)))
    (fn [e]
      (js/console.log "getAccounts error: " e)
      (if callback (callback nil)))))

(defn getFollowCount [account & {:keys [callback]}]
  (.then
    (js/steem.call
      "follow_api"
      "get_follow_count"
      (clj->js [account]))
    (fn [result]
      (if callback (callback result)))
    (fn [e]
      (js/console.log "getFollowCount error: " e)
      (if callback (callback nil)))))

(defn parse-accounts []
  (let [columns (r/cursor app-state [:columns])]
    (filterv #(not (nil? %))
      (mapv #(if (or (= "blog" (:path %))
                     (= "feed" (:path %)))
               (:tag %)
               nil)
        @columns))))

(defn update-columns-with-account []
  (let [columns (r/cursor app-state [:columns])
        accounts (parse-accounts)]
    (getAccounts
      accounts
      :callback
      (fn [data]
        (when data
          (js/console.log data)
          (let [account-data (reduce
                               (fn [old account]
                                 (assoc old (get account "name") account))
                               {}
                               (js->clj data))]
            (doseq [idx (range (count @columns))]
              (let [column (r/cursor app-state [:columns idx])]
                (when (or (= "blog" (:path @column))
                          (= "feed" (:path @column)))
                  (getFollowCount
                    (:tag @column)
                    :callback
                    (fn [followsjs]
                      (let [follows (js->clj followsjs)
                            merged (merge (get account-data (:tag @column))
                                          follows)]
                        (swap! column assoc :account merged)
                        (js/console.log (clj->js @column))))))))))))))

(defn scroll-element [el duration]
  (let [scroll-step-temp (/ (- (.-scrollTop el)) (/ duration 15))
        scroll-step (if (< scroll-step-temp -1)
                      scroll-step-temp
                      -1)
        scroll-interval (atom nil)]
    (reset! scroll-interval (js/setInterval
                              (fn []
                                (if (and (.contains js/document el)
                                         (> (.-scrollTop el) 0))
                                  (.scrollBy el 0 scroll-step)
                                  (js/clearInterval @scroll-interval)))
                              15))))

(defn all-images [posts]
  (filter #(not (empty? %))
    (map (fn [post]
           (if-let [image (parse-image-url post)]
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

; number of posts to load for different column sort orders
(def column-load-num
  {"created" 50
   "blog" 25
   "feed" 25
   "trending" 25
   "hot" 25})

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
                        path)
          outdated (> (- (js/Date.) (:last-loaded-full @column)) 300000)]
      (getDiscussions
        (:path @column)
        (if-let [tag (:tag @column)]
          tag
          "")
        (if (or forced
                second
                (empty? (:data @column))
                outdated
                (not (or (= "created" (:path @column))
                         (= "blog" (:path @column))
                         (= "feed" (:path @column)))))
          (column-load-num (:path @column))
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
                      outdated
                      (not (or (= "created" (:path @column))
                               (= "blog" (:path @column))
                               (= "feed" (:path @column)))))
                (preload-images
                  (all-images parsed)
                  (fn [preloaded]
                    (when (= loaded-path (:path @column))
                      (swap! column assoc :loading false
                                          :last-loaded-full (js/Date.))
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
                                (if-let [scroll-to (.querySelector column-element (str "#post-" last-top))]
                                  (.scrollIntoView scroll-to)
                                  (set!
                                    (.-scrollTop scroll-view)
                                    ; scroll to 200 pixels above bottom, so
                                    ; that column isn't bottom loaded
                                    (-
                                      (.-scrollHeight scroll-view)
                                      (.-clientHeight scroll-view)
                                      200)))
                                (scroll-element scroll-view 500))
                              100)))))))
                (if-not (= last-top (get first-parsed "id"))
                  (do
                    (load-column column :second true
                      :path loaded-path))
                  (swap! column assoc :loading false))))))))))

(defn load-column-bottom [column]
  ;(js/console.log "Load bottom...")
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

(defn remove-column [column]
  (let [columns (r/cursor app-state [:columns])]
    (swap! columns (fn [old]
                     (filterv #(not (= (:id %) (:id @column))) old)))
    (save-columns)))

(defn add-column [text]
  (let [columns (r/cursor app-state [:columns])
        coltype (str (first text))
        text-rest (apply str (rest text))]
    (swap!
      columns
      conj
      {:id (random-uuid)
       :path (if (= coltype "@")
               "blog"
               "created")
       :tag (cond
              (= coltype "@") text-rest
              (= coltype "#") text-rest
              :else text)})
    (save-columns)
    (update-columns-with-account)))

(defn card-subtitle [item]
  (let [icon-size 20
        settings (r/cursor app-state [:settings])
        dark-mode (:dark-mode @settings)
        icon-color (if dark-mode (color :grey300) (color :grey600))]
    [:div {:style {:display "flex"
                   :justify-content "space-between"
                   :align-items "center"
                   :padding 5}}
     [ic/hardware-keyboard-arrow-up {:color icon-color
                                     :style {:width icon-size
                                             :height icon-size}}]
     [:div (count (get item "active_votes"))]
     [ic/communication-chat-bubble {:color icon-color
                                    :style {:width icon-size
                                            :height icon-size}}]
     [:div (get item "children")]
     [ic/editor-attach-money {:color icon-color
                              :style {:width icon-size
                                      :height icon-size}}]
     [:div
      (if (is-post-active item)
        (get item "pending_payout_value")
        (get item "total_payout_value"))]]))

(defn post-card [item]
  (let [settings (r/cursor app-state [:settings])
        metadata (js->clj (js/JSON.parse (get item "json_metadata")))]
    (fn [item]
      (:dark-mode @settings) ; hack to make posts rerender when toggling theme
      [ui/card {:id (str "post-" (get item "id"))
                :container-style {:margin-bottom 10}}
       [ui/card-header {:title (get item "author")
                        :avatar (r/as-element
                                  [ui/avatar
                                   {:src (avatar-url (get item "author"))
                                    :title "Show user in new column"
                                    :class "hover-cursor"
                                    :on-click #(add-column (str "@" (get item "author")))}])
                        :subtitle (format-time (get item "created"))}]
       (if-let [image (parse-image-url item)]
         (when-not (and (:hide-nsfw @settings)
                        (> (count (filter #(= % "nsfw") (get metadata "tags"))) 0))
           [ui/card-media
            [:img {:src (cached-image image)}]]))
       [ui/card-title {:title (get item "title")
                       :title-style {:font-size 18
                                     :line-height "24px"}
                       :subtitle (r/as-element [card-subtitle item])}]
       [ui/card-actions {:style {:display "flex"
                                 :justify-content "center"}}
        [:a {:target "_blank"
             :href (str "https://www.steemit.com" (get item "url"))}
         [ui/flat-button {:label "Read on Steemit"}]]]])))

(defn expander [arg1 & arg2]
  (let [props (if arg2 arg1 {})
        state (r/atom {:expanded (:expanded props)})
        settings (r/cursor app-state [:settings])]
    (fn [arg1 & arg2]
      (let [children (if arg2 arg2 arg1)
            child-height (:height @state)
            props (if arg2 arg1 {})]
        [:div {:style (merge
                        {:display "flex"
                         :flex-direction "column"}
                        (:style props))}
         [:div {:style (merge
                         {:background "silver"
                          :max-height (if (:expanded @state)
                                        (* 2 child-height) ; * 2 is hack
                                        0)
                          :overflow "hidden"
                          :transition "max-height 0.5s"}
                         (:style-top props))}

          [:div
           {:style {:display "block"} ; or inline-block
            :ref #(when %
                    (swap! state assoc :height (.-clientHeight %)))}
           (first children)]]
         [:div {:class "expander-bottom"
                :title (if (:expanded @state)
                         "Click to hide"
                         "Click to show")
                :style (merge {:text-align "center"
                               :background "#ddd"}
                         (:style-bottom props))
                :on-click (fn [] (swap! state update :expanded not))}
          (if (:expanded @state) "^" "v")]]))))

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
                    :class "column"
                    :z-depth 2
                    :style {:margin-left 10
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
           [ui/icon-button {:style {:padding 0
                                    :width "auto"
                                    :height "auto"}
                            :on-click remove-fn}
            [ic/navigation-close]]]
          [:div {:style {:overflow "hidden"
                         :flex 1
                         :display "flex"
                         :flex-direction "column"}}
           (if (or (= "blog" (:path @column))
                   (= "feed" (:path @column)))
             [expander {:expanded true
                        :style-top (if (:dark-mode @settings)
                                    {:background (color :grey900)}
                                    {:background (color :grey200)})
                        :style-bottom (if (:dark-mode @settings)
                                        {:background (color :grey800)
                                         :color (color :grey600)
                                         :border-top "1px solid"
                                         :border-color (color :grey900)
                                         :box-shadow "rgba(255, 255, 255, 0.1) 0px 1px 1px 0px inset"}
                                        {:background (color :blue500)
                                         :color (color :blue300)
                                         :border-top "1px solid"
                                         :border-color (color :blue700)
                                         :box-shadow "rgba(255, 255, 255, 0.1) 0px 1px 1px 0px inset"})}
              (when (:account @column)
                [:div {:style {:display "flex"
                               :flex-wrap "wrap"
                               :justify-content "center"
                               :align-items "center"
                               :padding-bottom 2}}
                 (let [metadata (get (:account @column) "json_metadata")
                       parsed (js->clj (if-not (empty? metadata)
                                        (js/JSON.parse metadata)
                                        nil))
                       bio (-> parsed
                             (get "profile")
                             (get "about"))]
                  (when bio
                   [ui/paper {:z-depth 1
                              :style {:margin 5
                                      :padding 5
                                      :text-align "center"}}
                    bio]))
                 [ui/chip {:style {:margin 2}}
                  (get-in @column [:account "balance"])]
                 [ui/chip {:style {:margin 2}}
                  (get-in @column [:account "sbd_balance"])]
                 [ui/chip {:style {:margin 2}}
                  (str (get-in @column [:account "post_count"]) " posts")]
                 [ui/chip {:style {:margin 2}}
                  (str (get-in @column [:account "follower_count"]) " follower")]
                 [ui/chip {:style {:margin 2}}
                  (str (get-in @column [:account "following_count"]) " following")]])])
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
                            :flex 1
                            :padding-bottom 10
                            :padding-top 10}}
          (map-indexed (fn [idx {id :id}]
                         (let [column (r/cursor app-state [:columns idx])]
                           ^{:key id}
                           [column-component column #(remove-column column)]))
                       @columns)]]]])))

(defonce initial-startup (atom false))
(defonce refresh-interval (atom nil))

(defn init []
  (when-not @initial-startup
    (load-settings)
    (load-columns)
    (update-columns-with-account)
    (reset! initial-startup true))

  (r/render-component [content]
    (.getElementById js/document "app"))

  (when (nil? @refresh-interval)
    (let [columns (r/cursor app-state [:columns])]
      (reset! refresh-interval
        (js/setInterval
          (fn []
            (doseq [idx (range (count @columns))]
              (let [column (r/cursor app-state [:columns idx])
                    now (js/Date.)]
                (if (> (- now (:last-loaded @column)) 60000)
                  (load-column column)))))
          10000)))))
