(ns net.n01se.clojure-projtools
  (:import
    (java.net URL Authenticator PasswordAuthentication)
    (org.apache.commons.httpclient HttpClient HttpState
                                   UsernamePasswordCredentials)
    (org.apache.commons.httpclient.auth AuthScope)
    (org.apache.commons.httpclient.methods PostMethod)
    (org.apache.commons.httpclient.methods.multipart
      MultipartRequestEntity ByteArrayPartSource FilePart StringPart Part))
  (:use [clojure.xml :as xml :only []]
        [clojure.contrib.repl-ln :only [repl]]
        [clojure.contrib.prxml :only [prxml]]
        [clojure.contrib.shell-out :only [sh with-sh-dir]]
        [clojure.contrib.seq-utils :only [indexed]]
        [clojure.contrib.str-utils2 :as str2 :only []]
        [net.cgrand.enlive-html :as enlive :only []]))

(def *auth*)
(def *project*)
(def *base-dir*)
(def *work-dir*)
(def *ticket-id*)
(def *ticket*)
(def *patches*)
(def *patch*)
(def *target-ticket-id*)
(def *target-ticket*)
(def *dup-to*)
(def *error-response*)

(def milestones
  #{{:id :bugs    :milestone "95732", :branch "heads/1.0", :work-dir "clojure-1.0"
     :dup-to :backlog}
    {:id :backlog :milestone "93751", :branch "master", :work-dir "clojure"
     :dup-to :bugs}
    {:id :next    :milestone "93750", :branch "master", :work-dir "clojure"
     :dup-to :bugs}})

(defn uindex
  "Returns a map of the distinct values of ks in the xrel (as
  a vector) mapped to a map in xrel with the corresponding values of ks."
  [xrel ks]
    (reduce
      (fn [m x]
        (assoc m (vec (map x ks)) x))
      {} xrel))

(defn milestone->
  ([kname k] ((uindex milestones [kname]) [k]))
  ([kname k vname] ((milestone-> kname k) vname)))

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

(defn upload-assembla-doc!
  "Create a new document for assemble project named (ie.
  \"clojure-contrib\").  The title and filename are frequently the
  same -- the filename is only visible as the default name of the file
  when downloaded.  The data must be an array of bytes.  Expects
  *auth* to be a [username password] vector."
  [project title filename #^bytes data]
  (let [[username password] *auth*
        http-state (doto (HttpState.)
                         (.setCredentials (AuthScope. "www.assembla.com" 80)
                                          (UsernamePasswordCredentials.
                                            username password)))
        file-post (PostMethod. (str "http://www.assembla.com/spaces/"
                                    project "/documents/"))
        parts (into-array
                Part
                [(StringPart. "document[name]" title)
                 (FilePart. "document[file]"
                            (ByteArrayPartSource. filename data))])]
    (.setRequestHeader file-post "Accept" "application/xml")
    (.setRequestEntity file-post
                       (MultipartRequestEntity. parts (.getParams file-post)))
    (let [status (.executeMethod (HttpClient.) nil file-post http-state)]
      (with-open [input (.getResponseBodyAsStream file-post)]
        (xml/parse input)))))

(let [users (atom {})]
  (defn get-user [id]
    (or (@users id)
      (let [user (get-assembla "user/best_profile/" id)]
        (swap! users assoc id user)
        user))))

(defn get-ticket [project id]
  (get-assembla "spaces/" project "/tickets/" id))

(defn ticket-vxml [attrmap]
  [:ticket
    (when-let [t (:title attrmap)]
      [:summary t])
    (when-let [username (:assigned-to attrmap)]
      [:assigned-to-id
        (throw (Exception. ":assigned-to not working yet"))])
    (when-let [m (:milestone attrmap)]
      [:milestone-id (milestone-> :id m :milestone)])
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

(defn select-ticket-id [ticket]
  (select-content ticket :ticket :> :number))

(defn select-milestone [ticket]
  (let [id (select-content ticket :ticket :> :milestone-id)]
    (milestone-> :milestone id)))

(defn select-login [user]
  (select-content user :login_name))

(defn select-comments [ticket]
  (for [tcmt (enlive/select [*ticket*] [:ticket-comment])]
    {:user (select-login (get-user (select-content tcmt :user-id)))
     :comment (select-content tcmt :comment)}))

(defn git* [& args]
  (with-sh-dir *work-dir*
    (let [{:keys [exit out err]} (apply sh :return-map true
                                        "git" args)]
      (if (zero? exit)
        out
        (throw (Exception. (str "Git exited with code " exit ": "
                                out " " err)))))))

(defmacro git [& args]
  `(git*
     ~@(mapcat
         #(cond
            (string? %) [%]
            (and (seq? %) (= (first %) `unquote)) [`(str ~(second %))]
            (and (seq? %) (= (first %) `unquote-splicing))
                  `(map str ~(second %))
            :else [(str %)])
         args)))

(defn git-title [sha1]
  (git log "--pretty=format:%s" -1 ~sha1))

(defn git-message [sha1]
  (git log "--pretty=format:%s%n%b" -1 ~sha1))

(defn git-head []
  (git rev-parse --symbolic-full-name HEAD))

(defn linked-tickets [ticket]
  (for [{:keys [comment] :as tcmt} (select-comments ticket)
        :let [match (re-find #"^(?!Updating tickets \(.*\)).*#\d+.*(?:\n|$)"
                             comment)]
        :when match]
    (assoc tcmt :link-line match)))

(defn linked-commits [ticket]
  (for [{:keys [comment] :as tcmt} (select-comments ticket)
        [_ sha1] (re-seq #"\[\[(?:r:|[^|]*\|)([0-9a-f]{6,})\]\]" comment)]
    (assoc tcmt :sha1 sha1 :title (git-title sha1))))

(defn ticket
  ([id] (ticket *project* id))
  ([project id]
   (set! *ticket* (get-ticket project id))
   (set! *ticket-id* id)
   (println (str "\n=== #" id ": " (select-title *ticket*) " ==="))
   (set! *dup-to* (:dup-to (select-milestone *ticket*)))
   (println "Milestone" (:id (select-milestone *ticket*))
            "   Dup-to" *dup-to*)
   (binding [*work-dir* (str *base-dir* (milestone-> :id *dup-to* :work-dir))]
     (set! *patches* (linked-commits *ticket*))
     (doseq [[patch-num patch] (indexed *patches*)]
       (println (str patch-num ": " (:title patch))))
     (doseq [{:keys [user link-line]} (linked-tickets *ticket*)]
       (let [comment-len (min (- 76 (count user)) (count link-line))]
         (println (str "<" user "> " (subs link-line 0 comment-len))))))))

(defn dup-ticket []
  {:title (str (select-title *ticket*) " (from " *ticket-id* ")")
   :milestone *dup-to*
   :priority (select-content *ticket* :ticket :> :priority)
   :text (str "This ticket is for tracking #" *ticket-id*
              " against branch "
              (milestone-> :id *dup-to* :branch))})

(defn dup-ticket! []
  (set! *target-ticket*
    (post-assembla!
      (str "spaces/" *project* "/tickets/")
      (ticket-vxml (dup-ticket))))
  (set! *target-ticket-id* (select-ticket-id *target-ticket*))
  ;(update-ticket! ...)
  ;(str "Created ticket #" new-num "to track this issue against branch 1.0."
  (println (str "Created #" *target-ticket-id* ": "
                (select-title *target-ticket*))))

(defn rebase-msg [old-ticket-id new-ticket-id patch]
  (let [old-msg (git-message (:sha1 patch))]
    (str (str2/replace old-msg (str "#" old-ticket-id) (str "#" new-ticket-id))
         "\n(cherry picked from commit " (:sha1 patch) ")\n")))

; XXX doesn't work yet
(defn attach-file! [ticket-id title filename data tags]
  (let [new-doc (upload-assembla-doc! *project* filename filename data)
        doc-id (select-content new-doc :document :> :id)]
    (post-assembla!
      (str "spaces/" *project* "/tickets/" ticket-id)
      (ticket-vxml {:documents []}))))

(defn attach-patch! []
  (assert *target-ticket-id*)
  (let [msg (rebase-msg *ticket-id* *target-ticket-id* *patch*)]
    (git commit --signoff --template=- ~:in ~msg)
    (let [new-patch (git format-patch "@{1}" --stdout ~:out ~:bytes)
          title (str "the patch for the thing")
          filename (str "ticket-" *target-ticket-id* ".diff")]
      (git reset --hard "@{1}")
      (attach-file! *target-ticket-id* title filename new-patch ["patch"]))))

(defn dup-patch! [& [patch-num]]
  (binding [*work-dir* (str *base-dir* (milestone-> :id *dup-to* :work-dir))]
    (let [patch (nth *patches* (or patch-num 0))]
      (set! *patch* patch)
      (if (:sha1 patch)
        ; This is a commit
        (do
          (try
            (git diff --quiet HEAD)
            (catch Exception e
              (println "Working dir" *work-dir* "not clean?")
              (throw e)))
          (try
            (git diff --quiet ~(milestone-> :id *dup-to* :branch))
            (catch Exception e
              (println "Working dir" *work-dir* "not on branch" (milestone-> :id *dup-to* :branch) "?")
              (throw e)))
          (try
            (git cherry-pick --no-commit ~(:sha1 patch))
            (catch Exception e
              (println "Error during cherry-pick.  Probably needs a merge.")
              (println "Use 'attach-patch!' when ready to proceed.")
              (throw e)))
          (println "Cherry-pick was clean.  Proceeding with attach-patch!")
          (attach-patch!))

        ; Perhaps this is an attached patch file
        (throw (Exception. "Unsupported patch type"))))))

(defn go []
  (binding [*auth* nil
            *project* "clojure"
            *base-dir* "/home/chouser/proj/"
            *ticket-id* nil
            *ticket* nil
            *patches* nil
            *patch* nil
            *target-ticket-id* nil
            *target-ticket* nil
            *dup-to* nil
            *error-response* nil]
    (repl)))
