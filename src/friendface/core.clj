(ns friendface.core
  (:require [clojure.string :as s]))

;; The Social Networking kata from the uSwitch coding dojo, June 2015
;; http://monospacedmonologues.com/post/49250842364/the-social-networking-kata

;; This code was developed via REPL driven development, test code was created
;; inline rather than as seperate test framework.

;; A completely stateless approach to solving this challenge was taken, although the data structure used to represent the world could be referenced as an atom.  However, to solve the kata an atom was not needed in this case.

;; The code and documentation is presented relatively linearly, from top to bottom, in the way it was developed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ideas around data structures to model the timeline

;; Initial thought was a map with a name and id for each user.  A map seems good, however a map per user would lead to a great many maps.  An id didnt seem relevant information at this time
(def user {:name "anonymous"
           :id "000"})

;; Users could be represented as a keyword, making them easy to look up
(def sample-user :alice)

;; Or you could have an event, i.e. a post, mention, direct message that contains all the information for that specific event
(def event {:type :tweet
            :user :alice
            :message "Hello world!"})

;; So you would have multiple events created (events would be generated, however we define some for testing)
(def event-2
  {:type :tweet,
   :user :bob,
   :message "Hi Alice!"})

;; Events could be collated into a vector, to build up the total timelines for the whole social network
(def events [event event-2])


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing some behaviour

;; So lets test out some functionality to help us understand which data structure would be good to work with,
;; starting with a user and a message
(def timeline {:user :alice :message "My first post"})

;; As the timeline is a map, we can simply pull the message out using the :message keyword
(defn show-user-timeline
  "Show the specific users timeline"
  []
  (:message timeline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model the whole world in a map
;; Decided to create a map to model the entire world so all the information is in one place
;; Added a basic structure to the world map to experiment with
;; * tweets are an empty set, so we can add more than just a string
;; * same goes for direct messages
;; * a set is used for follows, because we want a unique list of user names
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

;; The above is the same as pulling data out of maps with keywords, for example:
;; (:tweets (:alice world))
;; The get-in function makes the code more human readable and makes it possile to use the argument values passed to the function - in the above case user is replaced by

;; Test the code - although this will just return an empty vector as there are no tweets in the empty-world.  It does however show that we do get a vector as the return data structure.
(get-user-tweets empty-world :alice)

;; Take the existing world and add a new tweet for a specific user.  Find that users tweet map, add the new tweet and add the updated values for that user to the world (well the new world returned as the result)
(defn post-tweet-the-long-way
  "Post a tweet to a users timeline by passing the original world map, the tweet to add and the user to add it to"
  [world tweet user]
  (assoc world
    :alice (assoc (:alice world)
             :tweets (conj (get-in world [user :tweets])
                           tweet))))

;; Simplified version of post-tweet function above
(defn post-tweet
  "Use update-in function in Clojure core to simplify updating a data structure"
  [world tweet user]
  (update-in world [user :tweets] conj tweet))

;; Test the post-tweet function
(post-tweet empty-world "Hello world, again!" :alice)

;; output from the above test code
;; {:alice {:tweets ["Hello world, again!"], :dms [], :follows #{}}, :bob {:tweets [], :dms [], :follows #{}}}

;; Combine the two test functions using the thread-first macro, passing empty-world as the first argument to the post-tweet function call.  Passing the result of the post-tweet function call into the get-user-tweets
(-> empty-world
    (post-tweet "Hello world, again!" :alice)
    (get-user-tweets :alice))

;; Given an @ mention, return just the user name
;; - drop the @ and any non-alphanumeric characters
(defn parse-mention [mention]
  (-> mention
      (subs 1)
      s/lower-case
      (s/replace #"[^a-z-_]" "")
      keyword))

;; testing parse-mention
(parse-mention "@blah_blah-blah!")

;; Parse all the messages and find the messages that include a specific @ mention
(defn find-mentions
  [{:keys [message]}]
  (->> (s/split message #"\s+")
       (filter #(.startsWith % "@"))
       (map parse-mention)
       set))

;; test find-mentions
(find-mentions {:message "@Alice!"})


;; Take the world and find the right user, add the new tweet and return the new version of the world along with any @ mentions.
;; Along the way we create many worlds, so we use reduce to combine all the worlds into one world result
(defn post-tweet
  "Given a world, a tweet and a user, return a world with the tweet added to specific user, including any @ mentions found along the way"
  [world tweet user]
  (let [mentions (find-mentions tweet)
        tweet-with-mentions (assoc tweet
                              :mentions mentions)]

    (-> (reduce (fn [acc mentioned-user]
                  (update-in acc [mentioned-user :mentions] conj tweet-with-mentions))
                world
                mentions)

        (update-in [user :tweets] conj tweet-with-mentions))))

;; Using the thread-first macro to write a simple test case (sans test framework)
;; (-> empty-world
;;    (post-tweet {:message "Hi @alice!" :time 2} :bob)
;;    (get-mentions :alice))


(defn start-following
  "Follow someone in the world"
  [world follower followee]
  (update-in world [follower :follows] conj followee))


;; A more varied test case
(def sample-world
  (-> empty-world
      (post-tweet {:message "Hello world" :time 1} :alice)
      (post-tweet {:message "Hi @alice!" :time 2} :bob)
      (post-tweet {:message "Hi its Charlie!" :time 3} :charlie)
      (start-following :bob :alice)
      (start-following :bob :charlie)))

;; This function was created earlier on in the dojo, its below other functions as it needs to call them to work (if you evaluate the whole file from scratch, Clojure evaluates functions from th top to bottom)
(defn get-user-timeline
  [world user]
  (let [followees (get-in world [user :follows])
        followees-tweets (mapcat #(get-user-tweets world %) followees)
        sorted-tweets (sort-by :time followees-tweets)]
    ;; keywords are functions that look themselves up in a map

    (reverse sorted-tweets)))

;; A reworking of get-user-timeline above, using thread-last to make it much easier to read (reads more like the algorithm you have in your head as you write the function)
(defn get-user-timeline
  [world user]
  (->> (get-in world [user :follows])
       (mapcat #(get-user-tweets world %))
       (sort-by :time)
       reverse))

;; These next few functions were pretty easy to do now we have a world and are using update-in to include changes
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some test code & results


(get-user-timeline sample-world :bob)
;; (:a {:a 1, :b 2})


;; {:charlie {:tweets ({:time 3, :message "Hi its Charlie!"})},
;;  :alice {:tweets [{:time 1, :message "Hello world"}], :dms [], :follows #{}},
;;  :bob {:tweets [{:time 2, :message "Hi Alice!"}], :dms [], :follows #{:alice :charlie}}}


;; Use let block to test functions
(let [tweet "Hello world!"
      user :alice
      world empty-world]
  (update-in world [user :tweets] conj tweet))


(update-in empty-world [:alice :tweets] conj "Hello world")

;; {:alice {:tweets ["Hello world"], :dms [], :follows #{}}, :bob {:tweets [], :dms [], :follows #{}}}
