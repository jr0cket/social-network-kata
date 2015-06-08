(ns friendface.core
  (:require [clojure.string :as s]))

;; Ideas around data structures 
(def user {:name "anonymous"
           :id "000"})

(def sample-user :alice)

(def event {:type :tweet
            :user :alice
            :message "Hello world!"})

(def event-2
  {:type :tweet,
   :user :bob,
   :message "Hi Alice!"})

(def events [event event-2])


(def timeline {:user :alice :message "My first post"})

(defn show-user-timeline
  "Show the specific users timeline"
  []
  (:message timeline))


(def empty-world
  {:alice {:tweets []
           :dms []
           :follows #{}}
 
   :bob {:tweets []
         :dms []
         :follows #{}}})

(defn get-user-tweets
  [world user]
  (get-in world [user :tweets]))

;; (:tweets (:alice world))

(defn post-tweet-the-long-way
  "Post a tweet to a users timeline"
  [world tweet user]
  (assoc world
    :alice (assoc (:alice world)
             :tweets (conj (get-in world [user :tweets])
                           tweet))))

(post-tweet "Hello world, again!" :alice empty-world)

(defn post-tweet
  "Use update-in function in Clojure core to simplify updating a data structure"
  [world tweet user]
  (update-in world [user :tweets] conj tweet))

{:alice {:tweets ["Hello world, again!"], :dms [], :follows #{}}, :bob {:tweets [], :dms [], :follows #{}}}

(defn parse-mention [mention]
  (-> mention
      (subs 1)
      s/lower-case
      (s/replace #"[^a-z-_]" "")
      keyword))

(parse-mention "@blah_blah-blah!")

(defn find-mentions
  [{:keys [message]}]
  (->> (s/split message #"\s+")
       (filter #(.startsWith % "@"))
       (map parse-mention)
       set))

(find-mentions {:message "@Alice!"})

(defn post-tweet
  "Use update-in function in Clojure core to simplify updating a data structure"
  [world tweet user]
  (let [mentions (find-mentions tweet)
        tweet-with-mentions (assoc tweet
                              :mentions mentions)]
    
    (-> (reduce (fn [acc mentioned-user]
                  (update-in acc [mentioned-user :mentions] conj tweet-with-mentions))
                world
                mentions)
        
        (update-in [user :tweets] conj tweet-with-mentions))))

(-> empty-world
    (post-tweet {:message "Hi @alice!" :time 2} :bob)
    (get-mentions :alice))

(defn start-following
  [world follower followee]
  (update-in world [follower :follows] conj followee))



(def sample-world
  (-> empty-world
      (post-tweet {:message "Hello world" :time 1} :alice)
      (post-tweet {:message "Hi @alice!" :time 2} :bob)
      (post-tweet {:message "Hi its Charlie!" :time 3} :charlie)
      (start-following :bob :alice)
      (start-following :bob :charlie)))

(defn get-user-timeline
  [world user]
  (let [followees (get-in world [user :follows])
        followees-tweets (mapcat #(get-user-tweets world %) followees)
        sorted-tweets (sort-by :time followees-tweets)]
    ;; keywords are functions that look themselves up in a map
    
    (reverse sorted-tweets)))

(defn get-user-timeline
  [world user]
  (->> (get-in world [user :follows])
       (mapcat #(get-user-tweets world %))
       (sort-by :time)
       reverse))

(defn send-dm
  [world from-user to-user message]
  (update-in world [to-user :dms] conj {:from-user from-user
                                        :message message}))

(defn get-dms
  [world user]
  (get-in world [user :dms]))

(defn get-mentions
  [world user]
  (get-in world [user :mentions]))





(get-user-timeline sample-world :bob)
(:a {:a 1, :b 2})


{:charlie {:tweets ({:time 3, :message "Hi its Charlie!"})},
 :alice {:tweets [{:time 1, :message "Hello world"}], :dms [], :follows #{}},
 :bob {:tweets [{:time 2, :message "Hi Alice!"}], :dms [], :follows #{:alice :charlie}}}



;; Use let block to test functions
(let [tweet "Hello world!"
      user :alice
      world empty-world]
  (update-in world [user :tweets] conj tweet))




(update-in empty-world [:alice :tweets] conj "Hello world"){:alice {:tweets ["Hello world"], :dms [], :follows #{}}, :bob {:tweets [], :dms [], :follows #{}}}
