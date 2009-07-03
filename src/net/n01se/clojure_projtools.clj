(ns net.n01se.clojure-projtools
  (:import (java.net URL Authenticator PasswordAuthentication))
  (:use [clojure.xml :as xml :only []]
        [clojure.contrib.repl-ln :only [repl]]
        [clojure.contrib.prxml :only [prxml]]
        [clojure.contrib.shell-out :only [sh with-sh-dir]]
        [clojure.contrib.seq-utils :only [indexed]]
        [net.cgrand.enlive-html :as enlive :only []]))

(def *auth*)
(def *project*)
(def *project-dir*)
(def *ticket-id*)
(def *ticket*)
(def *dup-to*)
(def *patches*)
(def *error-response*)

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
      (when-let [[user pw] *auth*]
        (PasswordAuthentication. user (.toCharArray pw))))))

(defn get-assembla [& parts]
  (let [url-str (apply str "http://www.assembla.com/" parts)
        conn (.openConnection (URL. url-str))]
    (.setRequestMethod conn "GET")
    (.setRequestProperty conn "Accept" "application/xml")
    ;(println "connecting...")
    (with-open [input (.getInputStream conn)]
      (xml/parse input))))

(defn post-assembla! [url-path vxml]
  (set! *error-response* nil)
  ;(let [url-str (str "http://localhost:8888/" url-path)
  (let [url-str (str "http://www.assembla.com/" url-path)
        conn (.openConnection (URL. url-str))]
    (doto conn
      (.setRequestMethod "POST")
      (.setRequestProperty "Content-type" "application/xml")
      (.setRequestProperty "Accept" "application/xml")
      (.setDoOutput true))
    (.connect conn)
    (with-open [output (java.io.OutputStreamWriter.
                         (.getOutputStream conn) "UTF-8")]
      (binding [*out* output]
        (prxml vxml)))
    (try
      (with-open [input (.getInputStream conn)]
        (xml/parse input))
      (catch java.io.IOException e
        (with-open [err (.getErrorStream conn)]
                   (println
                     (apply str
                            (map char (take-while pos?
                                                  (repeatedly #(.read err)))))))
          ;(set! *error-response* (xml/parse err)))
        (throw e)))))

(let [users (atom {})]
  (defn get-user [id]
    (or (@users id)
      (let [user (get-assembla "user/best_profile/" id)]
        (swap! users assoc id user)
        user))))

(defn get-ticket [project id]
  (get-assembla "spaces/" project "/tickets/" id))

(defn create-ticket [attrmap]
  [:ticket
    (when-let [t (:title attrmap)]
      [:summary t])
    (when-let [username (:assigned-to attrmap)]
      [:assigned-to-id
        (throw (Exception. ":assigned-to not working yet"))])
    (when-let [m (:milestone attrmap)]
      [:milestone-id
        (((uindex milestones [:id]) [m]) :milestone)])
    (when-let [p (:priority attrmap)] ; 1 (highest) - 5 (lowest)
      [:priority p])
    (when-let [c (:component attrmap)]
      [:component-id
        (throw (Exception. ":component not working yet"))])
    (when-let [s (:status attrmap)]
      [:status
        (throw (Exception. ":status not working yet"))])
    (when-let [d (:text attrmap)]
      [:description d])])

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

(defmacro git [& args]
  `(with-sh-dir *project-dir*
     (sh "git"
       ~@(mapcat
           #(cond
              (string? %) [%]
              (and (seq? %) (= (first %) `unquote)) [`(str ~(second %))]
              (and (seq? %) (= (first %) `unquote-splicing))
                    `(map str ~(second %))
              :else [(str %)])
           args))))

(defn linked-tickets [ticket]
  (for [{:keys [comment] :as tcmt} (select-comments ticket)
        :let [match (re-find #"^(?!Updating tickets \(.*\)).*#\d+.*(?:\n|$)"
                             comment)]
        :when match]
    (assoc tcmt :link-line match)))

(defn linked-commits [ticket]
  (for [{:keys [comment] :as tcmt} (select-comments ticket)
        [_ sha1] (re-seq #"\[\[(?:r:|[^|]*\|)([0-9a-f]{6,})\]\]" comment)]
    (assoc tcmt
           :sha1 sha1
           :title (git log "--pretty=format:%s" ~sha1 -1))))

(defn ticket
  ([id] (ticket *project* id))
  ([project id]
   (set! *ticket* (get-ticket project id))
   (set! *ticket-id* id)
   (println (str "\n=== #" id ": " (select-title *ticket*) " ==="))
   (set! *dup-to* (:dup-to (select-milestone *ticket*)))
   (println "Milestone" (:id (select-milestone *ticket*))
            "   Dup-to" *dup-to*)
   (set! *patches* (linked-commits *ticket*))
   (doseq [[patch-num patch] (indexed *patches*)]
     (println (str patch-num ": " (:title patch))))
   (doseq [{:keys [user link-line]} (linked-tickets *ticket*)]
     (let [comment-len (min (- 76 (count user)) (count link-line))]
       (println (str "<" user "> " (subs link-line 0 comment-len)))))))

(defn dup-ticket []
  {:title (str (select-title *ticket*) " (from " *ticket-id* ")")
   :milestone *dup-to*
   :priority (select-content *ticket* :ticket :> :priority)
   :text (str "This ticket is for tracking #" *ticket-id*
              " against branch "
              (((uindex milestones [:id]) [*dup-to*]) :branch))})

(defn dup-ticket! []
  ; (let [new-num (select-ticket-number
  (post-assembla!
    (str "spaces/" *project* "/tickets/")
    (create-ticket (dup-ticket)))
  ;(update-ticket! ...)
  ;(str "Created ticket #" new-num "to track this issue against branch 1.0."
  )

(defn go []
  (binding [*auth* nil
            *project* "clojure"
            *project-dir* "/home/chouser/proj/clojure/"
            *ticket-id* nil
            *ticket* nil
            *dup-to* nil
            *patches* nil
            *error-response* nil]
    (repl)))
