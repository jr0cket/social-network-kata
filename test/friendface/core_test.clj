(ns friendface.core-test
  (:require [clojure.test :refer :all]
            [friendface.core :refer :all]))

;; (def user {:name "anonymous"
;;            :id "000"})

(def sample-user :alice)

(def event {:type :tweet
            :user :alice
            :message "Hello world!"})

(def events [event event])

;; [{:type :tweet, :user :alice, :message "Hello world!"}
;;  {:type :tweet, :user :bob, :message "Hello world!"}]

(def all-user [
               {}
               ])

{:id}

(deftest post-to-timeline
  (testing "My first test"
    ()))


