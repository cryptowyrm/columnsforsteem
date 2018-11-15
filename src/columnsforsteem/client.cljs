(ns columnsforsteem.client
  (:require [cljsjs.material-ui]
            [cljs-react-material-ui.core :refer [get-mui-theme color]]
            [cljs-react-material-ui.reagent :as ui]
            [cljs-react-material-ui.icons :as ic]
            [reagent.core :as r]))

(defonce column-index (atom 0))

(defn default-settings []
  {:hide-nsfw true
   :dark-mode true
   :expand-user true
   :show-reblogs true
   :big-pictures false
   :tight-layout true})

(defonce
  app-state
  (r/atom {:drawer-open false
           :settings (default-settings)
           :preview-image nil
           :columns [{:id (random-uuid) :path "created" :tag "technology"}
                     {:id (random-uuid) :path "created" :tag "news"}
                     {:id (random-uuid) :path "hot"}
                     {:id (random-uuid) :path "blog" :tag "crypticwyrm"}]}))

(defn setting-for [setting-key]
  (let [setting (get-in @app-state [:settings setting-key])]
    (if (nil? setting)
      (do
        (swap! app-state assoc-in [:settings setting-key] (setting-key (default-settings)))
        (setting-key (default-settings)))
      setting)))

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

(defn check-version []
  (let [settings (r/cursor app-state [:settings])
        show-update (r/cursor app-state [:show-update])]
    (-> (js/fetch "update.json")
        (.then (fn [result]
                 (.text result)))
        (.then (fn [result]
                 (if-not (= (setting-for :last-updated) result)
                   (reset! show-update result)))))))

(def image-regex
  #"(?i)https?://[^\s<>\[\]]*\.(jpe?g|png|gif)(\?[A-Za-z0-9!$&'()*+.,;=]*\b)?")

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

(defn big-picture [url]
  (str "https://steemitimages.com/0x0/" url))

(defn cached-image [url]
  (if (setting-for :big-pictures)
    (str "https://steemitimages.com/640x480/" url)
    (str "https://steemitimages.com/100x100/" url)))

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

(defn steemPerMvests [total_vesting_fund_steem total_vesting_shares]
  (/ total_vesting_fund_steem total_vesting_shares))

(defn vestsToSteemPower [vests steem_per_mvests]
  (* vests steem_per_mvests))

(defn vests2sp [vests]
  (let [dynamic-global-properties (r/cursor app-state [:dynamic-global-properties])]
    (vestsToSteemPower vests
                       (steemPerMvests
                         (js/parseFloat (get @dynamic-global-properties
                                             "total_vesting_fund_steem"))
                         (js/parseFloat (get @dynamic-global-properties
                                             "total_vesting_shares"))))))

(defn getDynamicGlobalProperties [& {:keys [callback]}]
  (let [dynamic-global-properties (r/cursor app-state [:dynamic-global-properties])]
    (.then
      (js/steem.database.getDynamicGlobalProperties)
      (fn [result]
        (reset! dynamic-global-properties (js->clj result))
        (if callback (callback result)))
      (fn [e]
        (js/console.log "getDynamicGlobalProperties error: " e)
        (if callback (callback e))))))

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
                        (swap! column assoc :account merged)))))))))))))

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
  (set
    (filter #(not (empty? %))
      (map (fn [post]
             (if-let [image (parse-image-url post)]
               (cached-image image)
               nil))
        posts))))

(defn preload-images [images callback]
  ;(js/console.log "preload: " images)
  (let [left (atom (count images))
        preloaded (atom {})]
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
          (swap! preloaded assoc image-link image))))))

(defn sorted-posts [posts]
  (reverse (sort-by #(get % "created") posts)))

(defn refreshed-posts
  "Replaces data of old posts with any new data from new posts, discarding
  any posts that are not included in old posts."
  [new-p old-p]
  (let [mapped (reduce (fn [y x] (assoc y (get x "id") x)) {} new-p)]
    (map (fn [x] (if-let [y (mapped (get x "id"))] y x)) old-p)))

(defn new-posts
  "Takes a sequence of new posts and a sequence of old posts and returns
  a sequence of those that are not included in the old, based on the post id."
  [new-p old-p]
  (let [old-ids (set (map #(get % "id") old-p))]
    (filter #(not (old-ids (get % "id"))) new-p)))

; number of posts to load for different column sort orders
(def column-load-num
  {"created" 50
   "blog" 25
   "feed" 25
   "trending" 25
   "hot" 25})

(defn column-at-top [column]
  (= 0 (.-scrollTop (.querySelector
                      (.querySelector js/document (str "#" (:element @column)))
                      ".scroll-view"))))

(defn should-load-fully
  "Returns true if column should do a full load instead of scrolling
  in new posts through the buffer."
  [column]
  (not (#{"created" "blog" "feed"} (:path @column))))

(defn load-column [column & {:keys [forced]}]
  (when (or forced
            (and (empty? (:buffer @column))
                 (not (:loading @column))
                 (not (:loading-bottom @column))
                 (column-at-top column)))
    (let [loaded-path (:path @column)]
      (swap! column assoc
        :last-loaded (js/Date.)
        :loading true)
      ;(js/console.log "getDiscussions")
      (getDiscussions
        (:path @column)
        (if-let [tag (:tag @column)] tag "")
        25
        :callback
        (fn [result-js]
          (when (= loaded-path (:path @column))
            (let [result (js->clj result-js)
                  n-posts (new-posts result (:data @column))
                  images (all-images n-posts)]
              (preload-images
                images
                (fn [preloaded]
                  (when (= loaded-path (:path @column))
                    (if (column-at-top column)
                      (swap! column assoc
                        :images @preloaded
                        :loading false
                        :buffer (if (or (empty? (:data @column))
                                        forced
                                        (should-load-fully column))
                                  nil
                                  n-posts)
                        :data (if (or (empty? (:data @column))
                                      forced
                                      (should-load-fully column))
                                result
                                (vec (refreshed-posts result (:data @column)))))
                      (swap! column assoc
                        :loading false))))))))))))

(defn load-column-2 [column & {:keys [forced second path]}]
  (when (or forced
            second
            (and (not (:loading @column))
                 (not (:loading-bottom @column))
                 (column-at-top column)))
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
        dark-mode (setting-for :dark-mode)
        icon-color (if dark-mode (color :grey300) (color :grey600))]
    [:div {:style {:display "flex"
                   :justify-content "space-between"
                   :font-size "14px"
                   :color (if (setting-for :dark-mode)
                            "rgba(255, 255, 255, 0.54)"
                            "rgba(0, 0, 0, 0.54)")
                   :align-items "center"
                   :clear (when-not (setting-for :big-pictures) "both")
                   :padding 10}}
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

(defn post-card [item reblog]
  (let [metadata (js->clj (js/JSON.parse (get item "json_metadata")))
        preview-image (r/cursor app-state [:preview-image])
        preview-preloaded (r/cursor app-state [:preview-preloaded])]
    (fn [item]
      (setting-for :dark-mode) ; hack to make posts rerender when toggling theme
      [ui/card {:id (str "post-" (get item "id"))
                :container-style {:background (when (setting-for :dark-mode)
                                                (color :grey900))
                                  :border-top (if (setting-for :dark-mode)
                                                "1px solid #333"
                                                "1px solid #fff")
                                  :border-bottom (if (setting-for :dark-mode)
                                                   "1px solid black"
                                                   "1px solid #ddd")
                                  :display (when (and (not (setting-for :show-reblogs))
                                                      reblog)
                                             "none")}}
       [ui/card-header {:title (get item "author")
                        :avatar (r/as-element
                                  [ui/avatar
                                   {:src (avatar-url (get item "author"))
                                    :title "Show user in new column"
                                    :class "hover-cursor"
                                    :on-click #(add-column (str "@" (get item "author")))}])
                        :subtitle (format-time (get item "created"))}]
       [:div
        {:style {:display (when-not (setting-for :big-pictures) "flex")}}
        [ui/card-title {:title (r/as-element
                                [:a {:target "_blank"
                                     :href (str "https://www.steemit.com" (get item "url"))
                                     :style {:text-decoration "none"
                                             :color (if (setting-for :dark-mode)
                                                      "rgba(255, 255, 255, 0.87)"
                                                      "rgba(0, 0, 0, 0.87)")}}
                                 (get item "title")])
                        :style {:padding-top 0
                                :flex 1}
                        :title-style {:font-size 18
                                      :line-height "24px"}}]
        (if-let [image (parse-image-url item)]
          (when-not (and (setting-for :hide-nsfw)
                         (> (count (filter #(= % "nsfw") (get metadata "tags"))) 0))
            (if (setting-for :big-pictures)
              [ui/card-media
               {:class "hovercursor"
                :on-click (fn []
                           (reset! preview-image image)
                           (preload-images [(big-picture image)]
                                           #(reset! preview-preloaded %))
                           (js/console.log "Preview image:")
                           (js/console.log @preview-image))}
               [:img {:src (cached-image image)}]]
              [ui/card-media
               {:class "hovercursor"
                :style {:width 100
                        :max-height 100
                        :background "black"
                        :display "flex"
                        :align-items "center"
                        :justify-content "center"
                        :border-radius 8
                        :overflow "hidden"
                        :margin-left 5
                        :margin-right 10}
                :on-click (fn []
                            (reset! preview-image image)
                            (preload-images [(big-picture image)]
                                            #(reset! preview-preloaded %))
                            (js/console.log "Preview image:")
                            (js/console.log @preview-image))}
               [:img {:src (cached-image image)}]])))]
       [card-subtitle item]
       [ui/card-actions {:style {:display "none"
                                 :justify-content "center"}}]])))

(defn expander [arg1 & arg2]
  (let [props (if arg2 arg1 {})
        state (r/atom {:expanded (:expanded props)})]
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

(defn column-move-handle [column]
  (let [columns (r/cursor app-state [:columns])
        old-index (.indexOf @columns @column)]
    [:div {:class "move-handle"
           :style {:position "absolute"
                   :top -6
                   :text-align "center"
                   :font-size 16
                   :z-index 10000
                   :display (when (or
                                    (:loading @column)
                                    (:loading-bottom @column))
                              "none")
                   :width "100%"}}
     [:span {:style {:background "#242424"
                     :box-shadow "0px 0px 5px rgb(0, 0, 0, 0.5)"
                     :border-radius 8
                     :padding 5
                     :padding-left 5
                     :padding-right 5}}
      [:span
       {:class "arrow"
        :on-click (fn []
                    (let [new-index (- old-index 1)
                          new-columns (apply conj
                                        (subvec @columns 0 new-index)
                                        @column
                                        (first (subvec @columns new-index))
                                        (subvec @columns (+ old-index 1)))]
                      (reset! columns new-columns)
                      (save-columns)))
        :style {:padding 5
                :border-right "1px solid #3e3e3e"
                :box-shadow "inset -1px 0px 1px #000"
                :display (when (< old-index 1)
                           "none")}}
       "<-"]
      [:span
       {:style {:padding 5}}
       "Move"]
      [:span
       {:class "arrow"
        :on-click (fn []
                    (let [new-index (+ old-index 1)
                          new-columns (apply conj
                                        (subvec @columns 0 old-index)
                                        (fnext (subvec @columns old-index))
                                        @column
                                        (subvec @columns (+ 1 new-index)))]
                      (reset! columns new-columns)
                      (save-columns)))
        :style {:padding 5
                :border-left "1px solid #3e3e3e"
                :box-shadow "inset 1px 0px 1px #000"
                :display (when (= (+ old-index 1) (count @columns))
                           "none")}}
       "->"]]]))

(defn column-header [column remove-fn scroll-view]
  (let [header (atom nil)
        header-wrapper (atom nil)]
    (fn [column remove-fn scrol-view]
      [:div {:class "column-header"
             :ref (fn [el] (reset! header el))
             :style {:background (if (setting-for :dark-mode)
                                   (color :grey800)
                                   (color :blue500))
                     :color "white"
                     :padding 10
                     :position "relative"
                     :display "flex"
                     :align-items "center"}
             :on-click (fn [e]
                         (when @scroll-view
                           (when (or (= (.-target e) @header)
                                     (= (.-target e) @header-wrapper))
                             (scroll-element @scroll-view 500))))}
       [column-move-handle column]
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
                              :style {:background (if (setting-for :dark-mode)
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
                              :style {:background (if (setting-for :dark-mode)
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
                    :background-color (if (setting-for :dark-mode)
                                        (color :grey700)
                                        (color :blue300))
                    :style {:margin-left 10}}
           (if (or (= "blog" (:path @column))
                   (= "feed" (:path @column)))
             [ui/avatar {:src (avatar-url (:tag @column))
                         :style {:width 24
                                 :height 24}}]
             [ui/avatar {:size 24
                         :icon (ic/action-label {:style {:margin 0}})
                         :color (color :grey200)
                         :style {:width 24
                                 :height 24
                                 :line-height "24px"
                                 :font-size "20px"
                                 :background-color (if (setting-for :dark-mode)
                                                     (color :grey500)
                                                     (color :blue200))}}])
           (:tag @column)])]
       [ui/icon-button {:style {:padding 0
                                :width "auto"
                                :height "auto"}
                        :on-click remove-fn}
        [ic/navigation-close]]])))

(defn column-user-expander [column]
  [expander {:expanded (setting-for :expand-user)
             :style-top (if (setting-for :dark-mode)
                         {:background (color :grey900)}
                         {:background (color :grey200)})
             :style-bottom (if (setting-for :dark-mode)
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
      (let [line-height "24px"]
       [:div {:style {:display "flex"
                      :flex-wrap "wrap"
                      :justify-content "center"
                      :align-items "center"}}
        [ui/chip {:style {:margin 2}
                  :label-style {:line-height line-height}
                  :title "Steem Power"}
         (.toFixed (vests2sp (js/parseFloat (get-in @column [:account "vesting_shares"]))) 3) " SP"]
        [ui/chip {:style {:margin 2}
                  :label-style {:line-height line-height}}
         (get-in @column [:account "balance"])]
        [ui/chip {:style {:margin 2}
                  :title "Steem Dollars"
                  :label-style {:line-height line-height}}
         (get-in @column [:account "sbd_balance"])]
        [ui/chip {:style {:margin 2}
                  :label-style {:line-height line-height}}
         (str (get-in @column [:account "post_count"]) " posts")]
        [ui/chip {:style {:margin 2}
                  :label-style {:line-height line-height}}
         (str (get-in @column [:account "follower_count"]) " followers")]
        [ui/chip {:style {:margin 2}
                  :label-style {:line-height line-height}}
         (str (get-in @column [:account "following_count"]) " following")]
        [ui/chip {:style {:margin 2}
                  :title "Voting Power"
                  :label-style {:display "flex"
                                :align-items "center"
                                :line-height line-height}}
         [ui/linear-progress
          {:max 10000
           :mode "determinate"
           :value (get-in @column [:account "voting_power"])
           :color (color :grey500)
           :style {:height 15
                   :min-width 80
                   :margin-right "8px"}}]
         (.toFixed (/ (get-in @column [:account "voting_power"]) 100) 2)
         " % VP"]])])])

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
     (let [scroll-view (atom nil)]
       (fn [column remove-fn]
         [ui/paper {:id (:element @column)
                    :class "column"
                    :z-depth 2
                    :style {:margin-left (if (setting-for :tight-layout)
                                            1
                                            10)
                            :flex 1
                            :display "flex"
                            :flex-direction "column"
                            :min-width 400
                            :max-width 500}}
          [column-header column remove-fn scroll-view]
          [:div {:style {:overflow "hidden"
                         :flex 1
                         :display "flex"
                         :flex-direction "column"}}
           (if (or (= "blog" (:path @column))
                   (= "feed" (:path @column)))
             [column-user-expander column])
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
            [:div
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
             (doall
               (for [item (:data @column)]
                 ^{:key (get item "id")}
                 [post-card
                  item
                  (and (= "blog" (:path @column))
                       (not (= (:tag @column)
                               (get item "author"))))]))
             [:div {:style {:position "relative"
                            :width 40
                            :margin-left "auto"
                            :margin-right "auto"}}
              [ui/refresh-indicator {:top 0
                                     :left 0
                                     :status (if (:loading-bottom @column)
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
        [ui/subheader "App Settings"]
        [ui/list-item {:primary-text "Hide NSFW images"
                       :title "Hides images of posts that are tagged with NSFW, so there could still be NSFW images displayed if the author didn't add the tag"
                       :right-toggle
                         (r/as-element
                           [ui/toggle
                            {:toggled (setting-for :hide-nsfw)
                             :on-toggle (fn [e toggled]
                                          (swap! settings assoc :hide-nsfw toggled)
                                          (save-settings))}])}]
        [ui/list-item {:primary-text "Dark theme"
                       :right-toggle
                         (r/as-element
                           [ui/toggle
                            {:toggled (setting-for :dark-mode)
                             :on-toggle (fn [e toggled]
                                          (swap! settings assoc :dark-mode toggled)
                                          (save-settings))}])}]
        [ui/subheader "Column Settings"]
        [ui/list-item {:primary-text "Show user info by default"
                       :title "If activated, the box with user information at the top of user columns is opened by default"
                       :right-toggle
                         (r/as-element
                           [ui/toggle
                            {:toggled (setting-for :expand-user)
                             :on-toggle (fn [e toggled]
                                          (swap! settings assoc :expand-user toggled)
                                          (save-settings))}])}]
        [ui/list-item {:primary-text "Show reblogs"
                       :title "If activated, user columns will also show posts the user reblogged (resteemed)"
                       :right-toggle
                         (r/as-element
                           [ui/toggle
                            {:toggled (setting-for :show-reblogs)
                             :on-toggle (fn [e toggled]
                                          (swap! settings assoc :show-reblogs toggled)
                                          (save-settings))}])}]
        [ui/list-item {:primary-text "Big pictures"
                       :title "If activated, a large picture will be shown for posts instead of a small thumbnail"
                       :right-toggle
                         (r/as-element
                           [ui/toggle
                            {:toggled (setting-for :big-pictures)
                             :on-toggle (fn [e toggled]
                                          (swap! settings assoc :big-pictures toggled)
                                          (save-settings))}])}]
        [ui/list-item {:primary-text "Tight layout"
                       :title "If activated, there are no margins around columns, saving space"
                       :right-toggle
                         (r/as-element
                           [ui/toggle
                            {:toggled (setting-for :tight-layout)
                             :on-toggle (fn [e toggled]
                                          (swap! settings assoc :tight-layout toggled)
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

(defn preview-dialog []
  (let [preview-image (r/cursor app-state [:preview-image])
        preview-preloaded (r/cursor app-state [:preview-preloaded])
        reset-fn (fn []
                   (reset! preview-image nil)
                   (reset! preview-preloaded nil))]
    [:div {:style {:display (when-not @preview-image "none")}}
     [:div {:on-click reset-fn
            :style {:background "black"
                    :opacity 0.6
                    :position "absolute"
                    :top 0
                    :left 0
                    :bottom 0
                    :right 0
                    :z-index 9999}}]
     [ui/paper {:on-click reset-fn
                :style {:position "absolute"
                        :top 40
                        :left 40
                        :bottom 40
                        :right 40
                        :z-index 10000
                        :z-depth 5
                        :padding 20
                        :display "flex"
                        :flex-direction "column"
                        :overflow "hidden"
                        :white-space "nowrap"}}
      [:h1 {:style {:text-overflow "ellipsis"
                    :overflow "hidden"}}
       (last (clojure.string/split @preview-image #"/"))]
      (if @preview-preloaded
        [:div {:src (big-picture @preview-image)
               :style {:flex 1
                       :background-image (str "url(" (big-picture @preview-image) ")")
                       :background-size "contain"
                       :background-repeat "no-repeat"
                       :background-position "50% 50%"}}]
        [:h1 "Loading..."])]]))

(defn content []
  (let [columns (r/cursor app-state [:columns])
        show-column-dialog (r/atom false)
        show-update (r/cursor app-state [:show-update])
        settings (r/cursor app-state [:settings])]
    (fn []
      [ui/mui-theme-provider
       (if (setting-for :dark-mode)
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
       [:div {:class (if (setting-for :dark-mode)
                       "dark-theme"
                       "light-theme")
              :style {:display "flex"
                      :flex 1
                      :overflow "hidden"}}
        [preview-dialog]
        [drawer-component]
        [:div {:id "content-wrapper"
               :style {:display "flex"
                       :flex-direction "column"
                       :flex 1
                       :margin-left (when (:drawer-open @app-state) 256)
                       :overflow "hidden"}}
         [ui/mui-theme-provider
          (if (setting-for :dark-mode)
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
                       :icon-style-right {:margin-top 0}
                       :icon-element-right
                       (r/as-element
                         [:div {:style {:line-height "64px"}}
                          (if @show-update
                            [:a {:target "_blank"
                                 :href @show-update}
                             [ui/flat-button
                              {:label "Just updated!"
                               :secondary true
                               :on-click (fn []
                                           (swap! settings assoc :last-updated @show-update)
                                           (save-settings)
                                           (reset! show-update nil))}]])
                          [ui/flat-button
                           {:label "Add column"
                            :on-click #(reset! show-column-dialog true)}]])}]]
         [column-dialog show-column-dialog]
         [ui/paper {:id "columns"
                    :class (when-not (setting-for :tight-layout)
                             "margins")
                    :rounded false
                    :style {:display "flex"
                            :flex-direction "row"
                            :overflow "hidden"
                            :overflow-x "auto"
                            :flex 1
                            :background-color (if (setting-for :dark-mode)
                                                (color :grey900)
                                                (when (setting-for :tight-layout)
                                                  (color :grey900)))
                            :padding-bottom (if (setting-for :tight-layout)
                                              1
                                              10)
                            :padding-top (if (setting-for :tight-layout)
                                           1
                                           10)}}
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
    (getDynamicGlobalProperties)
    (js/setInterval
      (fn []
        (getDynamicGlobalProperties))
      (* 5 60000))
    (update-columns-with-account)
    (js/setInterval
      (fn []
        (update-columns-with-account))
      (* 5 60000)) ; update every 5 minutes
    (check-version)
    (reset! initial-startup true))

  (r/render-component [content]
    (.getElementById js/document "app"))

  (when (nil? @refresh-interval)
    (let [columns (r/cursor app-state [:columns])]
      (js/setInterval
        (fn []
          (doseq [idx (range (count @columns))]
            (let [column (r/cursor app-state [:columns idx])]
              (when (and (not (empty? (:buffer @column)))
                         (column-at-top column))
                (let [post (last (:buffer @column))
                      column-element (.querySelector
                                       js/document (str "#" (:element @column)))
                      scroll-view (.querySelector column-element ".scroll-view")
                      target (.querySelector
                               column-element
                               (str "#post-" (get (first (:data @column)) "id")))]
                  (swap! column assoc
                    :buffer (butlast (:buffer @column))
                    :data (into [post] (take 24 (:data @column))))
                  (r/after-render
                    (fn []
                      (when target
                        (let [target-height (.-clientHeight target)]
                          (.scrollTo scroll-view 0 target-height))
                        (scroll-element scroll-view 500)))))))))
        3000)
      (reset! refresh-interval
        (js/setInterval
          (fn []
            (doseq [idx (range (count @columns))]
              (let [column (r/cursor app-state [:columns idx])
                    now (js/Date.)]
                (if (> (- now (:last-loaded @column)) 60000)
                  (load-column column)))))
          10000)))))
