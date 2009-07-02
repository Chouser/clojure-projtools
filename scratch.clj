; Don't try to load this file -- it's a scratchpad of ideas and notes.
; See src/net/n01se/clojure_projtools.clj if you working code.

(use '[net.cgrand.enlive-html :as enlive :only []]
	 '[net.cgrand.xml :as xml :only []])

(import '(java.net URL))

(def xml
  (let [conn (.openConnection (URL. "http://www.assembla.com/spaces/clojure/tickets/123"))]
    (.setRequestMethod conn "GET")
    (.setRequestProperty conn "Accept" "application/xml")
    (.connect conn)
    (prn :code (.getResponseCode conn))
    (with-open [input (.getInputStream conn)]
      (xml/parse input))))

(prn (-> [xml] (enlive/select [:ticket :> :summary]) first :content first))
;({:tag :id, :attrs nil, :content ["cX-0jCw4Cr3RbzeJe5afGb"]})

;http://www.assembla.com/spaces/clojure/documents/cX-0jCw4Cr3RbzeJe5afGb/download



#{{:id :bugs :milestone "Release bugfixes", :branch "1.0", :dup-to :backlog}
  {:id :backlog :milestone "Backlog", :branch "master", :dup-to :bugs}
  {:id :next :milestone "Next release", :branch "master", :dup-to :bugs}}

;> (ticket 123)
; set *ticket-id* 123
; set *dup-to* "Release bugfixes"
;   prints default dup-to
;   (set! *dup-to* "....") ; specific milestone
;   (set-dup-to "...") ; regex
; patches: [1] att. name [2] att. name [3] commit msg 
;     (set! *ticket* (get-ticket 123))
;     (set! *dup-to* (default-dup-milestone (select-milestone *ticket*)))
;
;     ; these could change, so grab them now.
;     (set! *patch-list*
;       (vec(for [tcmt (select [*ticket*] [:ticket-comment])] {:who (-> [tcmt] (select [:user-id]) first :content first)})

;       (concat
;       (select-attachments t)
;          [#^{:type ::Attachment}
;           {:id "cX-0..."
;            :url ...
;            :name ...}]
;       (for [commit-id (select-commits *ticket*)]
;          #^{:type ::Commit}
;            {:id commit-id, :name (git-title commit-id)}))))
;     (set! *dup-patch* (last *patch-list*))
;     prints list and current selection
;     (set-dup-patch i) ;-> (set! *dup-patch* (nth *patch-list* i))
;> (dup-ticket!)

; (create-ticket (select-title *ticket*)
;                ("From ticket #" *ticket-id*)) ;get the new ticket id
; (rebase-patch)

(if (isa? ::Commit (type *dup-patch*))
  (let [old-head (git-head)]
    (git diff --quiet HEAD) ; throws if there are diffs
    (git checkout ~(:branch *dup-to*))
    (git cherry-pick --no-commit ~(:id *dup-patch*))
    (let [old-msg (git cat-file commit ~(:id *dup-patch*))]
      ; replace #old-id with #new-id
      ; append: (cherry picked from commit b03e19aa341fea01c1279a74f4184f6538d0f72e)
      ; launch editor
      (git commit --signoff --file -) ; < new-msg
      (let [new-patch (git format-patch @{1} --stdout)]
        (git reset --hard @{1}) ; maybe new-patch-id^ instead
        (attach new-ticket new-patch)
        (set-ticket :status :test)

; (git checkout ~old-head)))


(defn git-head []
  (git rev-parse --symbolic-full-name HEAD))
