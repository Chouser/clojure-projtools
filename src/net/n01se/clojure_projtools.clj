(ns net.n01se.clojure-projtools
  (:import (java.net URL Authenticator PasswordAuthentication))
  (:use [clojure.xml :as xml :only []]
        [clojure.contrib.repl-ln :only [repl]]
        [net.cgrand.enlive-html :as enlive :only []]))

(def *auth*)
(def *project*)
(def *ticket-id*)
(def *ticket*)
(def *dup-to*)

(def milestones
  #{{:id :bugs    :milestone "95732", :branch "1.0",    :dup-to :backlog}
    {:id :backlog :milestone "93751", :branch "master", :dup-to :bugs}
    {:id :next    :milestone "93750", :branch "master", :dup-to :bugs}})

(defn uindex
  "Returns a map of the distinct values of ks in the xrel (as
  a vector) mapped to a map in xrel with the corresponding values of ks."
  [xrel ks]
    (reduce
      (fn [m x]
        (assoc m (vec (map x ks)) x))
      {} xrel))

(Authenticator/setDefault
  (proxy [Authenticator] []
    (getPasswordAuthentication []
      (prn :auth-site (.getRequestingSite this))
      (when-let [[user pw] (*auth* (.getRequestingSite this))]
        (PasswordAuthentication. user pw)))))

(defn parse-assembla [& parts]
  (let [url-str (apply str "http://www.assembla.com/" parts)
        conn (.openConnection (URL. url-str))]
    (.setRequestMethod conn "GET")
    (.setRequestProperty conn "Accept" "application/xml")
    ;(println "connecting...")
    (.connect conn)
    (when (>= (.getResponseCode conn) 400)
      (throw (Exception. (str "HTTP error " (.getResponseCode conn)
                              ": " url-str))))
    ;(println "reading...")
    (with-open [input (.getInputStream conn)]
      (xml/parse input))))

(let [users (atom {})]
  (defn get-user [id]
    (or (@users id)
      (let [user (parse-assembla "user/best_profile/" id)]
        (swap! users assoc id user)
        user))))

(defn parse-ticket [project id]
  (parse-assembla "spaces/" project "/tickets/" id))

(defmacro select-content [elem & query]
  `(apply str (mapcat :content (enlive/select [~elem] [~@query]))))

(defn select-title [ticket]
  (select-content ticket :ticket :> :summary))

(defn select-milestone [ticket]
  (let [id (select-content ticket :ticket :> :milestone-id)]
    ((uindex milestones [:milestone]) [id])))

(defn select-login [user]
  (select-content user :login_name))

(defn select-comments [ticket]
  (for [tcmt (enlive/select [*ticket*] [:ticket-comment])]
    {:user (select-login (get-user (select-content tcmt :user-id)))
     :comment (select-content tcmt :comment)}))

(defn linked-tickets [ticket]
  (for [{:keys [comment] :as tcmt} (select-comments ticket)
        :let [match (re-find #"^(?!Updating tickets \(.*\)).*#\d+.*(?:\n|$)"
                             comment)]
        :when match]
    (assoc tcmt :link-line match)))

(defn ticket
  ([id] (ticket *project* id))
  ([project id]
   (set! *ticket* (parse-ticket project id))
   (set! *ticket-id* id)
   (println (str "\n=== #" id ": " (select-title *ticket*) " ==="))
   (set! *dup-to* (:dup-to (select-milestone *ticket*)))
   (println "Milestone" (:id (select-milestone *ticket*))
            "-- Dup-to" *dup-to*)
   (doseq [{:keys [user link-line]} (linked-tickets *ticket*)]
     (let [comment-len (min (- 76 (count user)) (count link-line))]
       (println (str "<" user "> " (subs link-line 0 comment-len)))))))

(defn go []
  (binding [*auth* {}
            *project* "clojure"
            *ticket-id* nil
            *ticket* nil
            *dup-to* nil]
    (repl)))
