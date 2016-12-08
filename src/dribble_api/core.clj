(ns dribble-api.core
  (:require [org.httpkit.client :as http]
            [throttler.core :refer [throttle-fn]]
            [cheshire.core :refer [parse-string]]
            [clojure.walk :refer [keywordize-keys]]
            [clojure.string :as str]))

;;(def token "10c9d0a1305005b895b28b0387d5337d82afac907b01e9369252c503f1c78327")
(def token "333bf68f566a1a3da4dc5869749c5c8d7bdff59b9dcb61917fc6db097d77c664")

(defn make-url [path]
  (let [api-user-url "https://api.dribbble.com/v1/users"
        client-access-token token]
    (str api-user-url "/" path "?access_token=" client-access-token)))

(defn with-access-token [url]
  (let [client-access-token token]
    (str url "?access_token=" client-access-token)))

(defn fetch-sync-url [url]
  @(http/get url
             (fn [{:keys [status headers code body]}]
               {:body (keywordize-keys (parse-string body))
                :headers headers})))

(defn find-user [user] (fetch-sync-url (make-url user)))

(defn get-followers-url [user]
  (:followers_url (:body (find-user user))))

(defn with-throttle [function] (throttle-fn function 50 :minute))

(defn fetch-next-page [header-link]
  (some->> header-link
           (re-find #"<(.*)>; rel=\"next\"")
           second))

(defn fetch-key-url [key-url shot]
  (let [request (fetch-sync-url (with-access-token shot))
        body (:body request)]
    (map #(key-url %1) body)))

(defn get-shots [follower-url]
  ((with-throttle
     (fn [acc url]
       (if (= "" url)
         (filter identity (flatten acc))
         (let [request (fetch-sync-url url)
               body (:body request)
               header-link (get-in request [:headers :link])
               current-shots (map #(get-in %1 [:follower :shots_url]) body)
               next-url (fetch-next-page header-link)]

           (recur (conj acc current-shots) (or next-url "")))))) [] follower-url))

(defn fetch-like-urls [shots]
  (filter identity (flatten (map (with-throttle (partial fetch-key-url :likes_url)) shots))))

(defn fetch-likers [likes]
  (filter identity (->> likes
                        (map (with-throttle (partial fetch-sync-url)))
                        (map :body))))

(defn count-likers [likers]
  (->> likers
       (group-by :id)
       (into [])
       (map (fn [[id likers]] (assoc (first likers) :likes (count likers))))
       (sort-by :likes)
       (take 10)))

(defn calc-likes [user]
  (some->> (with-access-token (get-followers-url user))
           (get-shots)
           (fetch-like-urls)
           (fetch-likers)
           (count-likers)))
