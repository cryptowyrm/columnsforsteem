(ns columnsforsteem.client
  (:require [cljsjs.material-ui]
            [cljs-react-material-ui.core :refer [get-mui-theme color]]
            [cljs-react-material-ui.reagent :as ui]
            [cljs-react-material-ui.icons :as ic]
            [reagent.core :as r]))

(enable-console-print!)

; stores the click count
(defonce
  app-state
  (r/atom {:columns [(r/atom {:path "trending"})
                     (r/atom {:path "hot"})]}))

(defn parseImageUrl [post]
  (if (empty? (get post "json_metadata"))
    ""
    (let [parsed (js/JSON.parse (get post "json_metadata"))
          meta (js->clj parsed)
          images (get meta "image")]
      (if-not (nil? images)
        (first images)
        ""))))

(defn avatar-url [post]
  (str "https://steemitimages.com/u/" (get post "author") "/avatar/small"))

(defn format-time [time]
  (.toLocaleString (js/Date. (str time "Z"))))

(defn getDiscussions [path tag & {:keys [callback]}]
  (.then
    (js/steem.database.getDiscussions
      path
      (clj->js {:limit 10
                :tag tag}))
    (fn [result]
      (js/console.log result)
      (if callback (callback result)))
    (fn [e]
      (js/console.log "getDiscussions error")
      (js/console.log e)
      (if callback (callback nil)))))

(defn load-column [column]
  (getDiscussions
    (:path @column)
    ""
    :callback
    (fn [result]
      (println "Callback called")
      (let [parsed (js->clj result)]
        (swap! column assoc :data parsed)))))


(defn column-component [column]
  [ui/paper {:z-depth 2
             :style {:margin 10
                     :flex 1}}
   [:h3 {:style {:background (color :blue500)
                 :color "white"
                 :margin 0
                 :padding 10}}
    (:path @column)]
   [ui/flat-button {:label "Load data"
                    :on-click (fn []
                                (load-column column))}]
   [:div {:style {:padding 10}}
    (for [item (:data @column)]
      [ui/card {:container-style {:margin-bottom 10}}
       [ui/card-header {:title (get item "author")
                        :avatar (avatar-url item)
                        :subtitle (format-time (get item "created"))}]
       [ui/card-media
        [:img {:src (parseImageUrl item)}]]
       [ui/card-text
        (get item "title")]
       [ui/card-actions
        [ui/flat-button {:label "Read on Steemit"}]
        [ui/flat-button {:label "Read on Busy"}]]])]])

; reagent component to be rendered
(defn content []
  (let [columns (r/cursor app-state [:columns])]
    [ui/mui-theme-provider {:mui-theme (get-mui-theme)}
     [:div {:style {:display "flex"
                    :flex-direction "column"
                    :flex 1}}
      [ui/app-bar {:title "Columns for Steem"}]
      [:div {:style {:display "flex"
                     :flex-direction "row"
                     :flex 1}}
       (for [column @columns]
         [column-component column])]]]))

; tells reagent to begin rendering
(r/render-component [content]
  (.querySelector js/document "#app"))

(when (nil? (:data @(first (:columns @app-state))))
  (println "empty"))
